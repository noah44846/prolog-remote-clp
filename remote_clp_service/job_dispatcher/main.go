package main

import (
	"encoding/json"
	"fmt"
	"log"
	"time"

	jwtware "github.com/gofiber/contrib/jwt"
	"github.com/gofiber/fiber/v2"
	"github.com/gofiber/fiber/v2/middleware/logger"
	"github.com/golang-jwt/jwt/v5"
	"github.com/google/uuid"
)

func getJobResponseCallback(jobResultMap *JobResultMap) func([]byte) {
	inner := func(data []byte) {
		resp := new(JobResponse)
		err := json.Unmarshal(data, resp)
		if err != nil {
			log.Println(err)
			return
		}

		result, ok := jobResultMap.Load(resp.Id)
		if !ok {
			log.Println("Job not found", resp.Id)
			return
		}

		switch resp.Status {
		case JobStatusDone:
			log.Println("Job completed:", resp.Id)

			result.Status = JobStatusDone
			result.Data = resp.Data
			result.Error = resp.Error
		case JobStatusInProgress:
			log.Println("Job processing:", resp.Id)

			result.Status = JobStatusInProgress
		default:
			log.Println("Invalid job state", resp.Id)
		}
	}

	return inner
}

func getAddJobHandler(connection *ConnectionWrapper, jobResultMap *JobResultMap) func(ctx *fiber.Ctx) error {
	inner := func(ctx *fiber.Ctx) error {
		user := ctx.Locals("user").(*jwt.Token)
		claims := user.Claims.(jwt.MapClaims)
		username := claims["username"].(string)

		payload := new(Job)

		if err := ctx.BodyParser(payload); err != nil {
			return err
		}

		connection.PublishJob(ctx.Body())

		jobResponse := JobResponse{
			Id:       payload.Id,
			Status:   JobStatusInProgress,
			Username: username,
		}
		jobResultMap.Store(payload.Id, &jobResponse)

		ctx.Response().Header.Add("Location", "/jobs/results/"+payload.Id.String())

		// return 202 Accepted with location to /jobs/results/:id
		return ctx.Status(fiber.StatusAccepted).JSON(jobResponse)
	}

	return inner
}

func getResultsHandler(jobResultMap *JobResultMap) func(ctx *fiber.Ctx) error {
	inner := func(ctx *fiber.Ctx) error {
		user := ctx.Locals("user").(*jwt.Token)
		claims := user.Claims.(jwt.MapClaims)
		username := claims["username"].(string)

		id, err := uuid.Parse(ctx.Params("id"))
		if err != nil {
			return ctx.Status(fiber.StatusBadRequest).JSON(fiber.Map{
				"error": "Invalid job ID",
			})
		}

		results, ok := jobResultMap.Load(id)
		if !ok {
			return ctx.Status(fiber.StatusNotFound).JSON(fiber.Map{
				"error": "Job not found",
			})
		}

		if results.Username != username {
			return ctx.Status(fiber.StatusForbidden).JSON(fiber.Map{
				"error": "Access denied",
			})
		}

		return ctx.JSON(results)
	}

	return inner
}

func getTokensHandler(config *Config) func(ctx *fiber.Ctx) error {
	inner := func(ctx *fiber.Ctx) error {
		body := new(TokenRequest)

		if err := ctx.BodyParser(body); err != nil {
			return err
		}

		if body.TokenUsername == "" || body.TokenExpiry <= time.Now().Unix() {
			return ctx.Status(fiber.StatusBadRequest).JSON(fiber.Map{
				"error": "Invalid token request",
			})
		}

		if body.AdminPassword != config.adminPassword {
			return ctx.Status(fiber.StatusUnauthorized).JSON(fiber.Map{
				"error": "Invalid password",
			})
		}

		// Create the Claims
		claims := jwt.MapClaims{
			"username": body.TokenUsername,
			"exp":      body.TokenExpiry,
		}

		// Create token
		token := jwt.NewWithClaims(jwt.SigningMethodHS256, claims)

		// Generate encoded token and send it as response.
		t, err := token.SignedString(config.jwtSecret)
		if err != nil {
			return ctx.SendStatus(fiber.StatusInternalServerError)
		}

		return ctx.JSON(fiber.Map{"token": t})
	}

	return inner
}

func StartServer(config *Config, connection *ConnectionWrapper, jobResultMap *JobResultMap) {
	// Create a new Fiber instance
	app := fiber.New()

	// add simple logger
	app.Use(logger.New())

	// Add unauthenticated routes
	app.Post("/tokens", getTokensHandler(config))

	// add JWT middleware
	app.Use(jwtware.New(jwtware.Config{
		SigningKey: jwtware.SigningKey{Key: config.jwtSecret},
	}))

	// Add authenticated routes
	app.Post("/jobs", getAddJobHandler(connection, jobResultMap))

	app.Get("/jobs/results/:id", getResultsHandler(jobResultMap))

	// Start Fiber API server
	log.Fatal(app.Listen(fmt.Sprintf(":%s", config.ApiPort)))
}

func main() {
	// Set up logging
	log.SetFlags(0)
	log.SetOutput(new(logWriter))

	// Load configuration
	config := GetConfig()

	jobResultMap := JobResultMap{}

	connection := Connect(&config, getJobResponseCallback(&jobResultMap))
	defer connection.Close()

	StartServer(&config, &connection, &jobResultMap)
}
