package main

import (
	"encoding/json"
	"fmt"
	"log"

	"github.com/gofiber/fiber/v2"
	"github.com/gofiber/fiber/v2/middleware/logger"
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

		switch resp.Status {
		case JobStatusDone:
			log.Println("Job completed:", resp.Id)
			jobResultMap.Store(resp.Id, resp)
		case JobStatusInProgress:
			log.Println("Job processing:", resp.Id)
			jobResultMap.Store(resp.Id, resp)
		default:
			log.Println("Invalid job state", resp.Id)
		}
	}

	return inner
}

func getAddJobHandler(connection *ConnectionWrapper, jobResultMap *JobResultMap) func(ctx *fiber.Ctx) error {
	inner := func(ctx *fiber.Ctx) error {
		payload := new(Job)

		if err := ctx.BodyParser(payload); err != nil {
			return err
		}

		connection.PublishJob(ctx.Body())

		jobResponse := JobResponse{
			Id:     payload.Id,
			Status: JobStatusInProgress,
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

		return ctx.JSON(results)
	}

	return inner
}

func StartServer(config *Config, connection *ConnectionWrapper, jobResultMap *JobResultMap) {
	// Create a new Fiber instance.
	app := fiber.New()

	// Add middleware.
	app.Use(
		logger.New(), // add simple logger
	)

	// Add route.
	app.Post("/jobs", getAddJobHandler(connection, jobResultMap))

	app.Get("/jobs/results/:id", getResultsHandler(jobResultMap))

	// Start Fiber API server.
	log.Fatal(app.Listen(fmt.Sprintf(":%s", config.ApiPort)))
}

func main() {
	// Load configuration.
	config := GetConfig()

	ExampleClient(&config)

	jobResultMap := JobResultMap{}

	connection := Connect(&config, getJobResponseCallback(&jobResultMap))
	defer connection.Close()

	StartServer(&config, &connection, &jobResultMap)
}
