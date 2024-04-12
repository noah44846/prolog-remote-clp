package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"sync"

	"github.com/gofiber/fiber/v2"
	"github.com/gofiber/fiber/v2/middleware/logger"

	"github.com/streadway/amqp"

	"github.com/google/uuid"
)

type Config struct {
	AmqpServerURL     string
	JobsChannelName   string
	StatusChannelName string
	ApiPort           string
}

type Job struct {
	Id uuid.UUID `json:"id"`
}

type JobResponse struct {
	Id     uuid.UUID        `json:"id"`
	Status JobStatus        `json:"status"`
	Error  string           `json:"error"`
	Data   []map[string]int `json:"data"`
}

type JobStatus int

const (
	JobStatusPending JobStatus = iota
	JobStatusInProgress
	JobStatusDone
)

func (s JobStatus) String() string {
	return [...]string{"pending", "in_progress", "done"}[s]
}

func (s *JobStatus) MarshalJSON() ([]byte, error) {
	return json.Marshal(s.String())
}

func (s *JobStatus) UnmarshalJSON(data []byte) error {
	switch string(data) {
	case `"pending"`:
		*s = JobStatusPending
	case `"in_progress"`:
		*s = JobStatusInProgress
	case `"done"`:
		*s = JobStatusDone
	default:
		return fmt.Errorf("invalid job status: %s", data)
	}
	return nil
}

var jobStatusMap sync.Map
var jobResultMap sync.Map

func getConfig() Config {
	amqpServerHost := os.Getenv("RABBITMQ_HOST")
	if amqpServerHost == "" {
		amqpServerHost = "localhost"
	}
	amqpServerPort := os.Getenv("RABBITMQ_PORT")
	if amqpServerPort == "" {
		amqpServerPort = "5672"
	}
	amqpServerUser := os.Getenv("RABBITMQ_USER")
	if amqpServerUser == "" {
		amqpServerUser = "guest"
	}
	amqpServerPassword := os.Getenv("RABBITMQ_PASSWORD")
	if amqpServerPassword == "" {
		amqpServerPassword = "guest"
	}
	amqpServerURL := fmt.Sprintf("amqp://%s:%s@%s:%s/", amqpServerUser, amqpServerPassword, amqpServerHost, amqpServerPort)
	jobsChannelName := os.Getenv("RABBITMQ_JOBS_CHANNEL_NAME")
	if jobsChannelName == "" {
		jobsChannelName = "rclp-jobs"
	}
	statusChannelName := os.Getenv("RABBITMQ_STATUS_CHANNEL_NAME")
	if statusChannelName == "" {
		statusChannelName = "rclp-status"
	}
	apiPort := os.Getenv("PORT")
	if apiPort == "" {
		apiPort = "3000"
	}

	return Config{
		AmqpServerURL:     amqpServerURL,
		JobsChannelName:   jobsChannelName,
		StatusChannelName: statusChannelName,
		ApiPort:           apiPort,
	}
}

func declareQueue(channel *amqp.Channel, queueName string) {
	_, err := channel.QueueDeclare(
		queueName, // queue name
		true,      // durable
		false,     // auto delete
		false,     // exclusive
		false,     // no wait
		nil,       // arguments
	)
	if err != nil {
		panic(err)
	}
}

func publishJob(channel *amqp.Channel, config Config, job []byte) {
	// Create a message to publish.
	message := amqp.Publishing{
		ContentType: "application/json",
		Body:        job,
	}

	// Attempt to publish a message to the queue.
	if err := channel.Publish(
		"",                     // exchange
		config.JobsChannelName, // queue name
		false,                  // mandatory
		false,                  // immediate
		message,                // message to publish
	); err != nil {
		panic(err)
	}
}

func startConsuming(channel *amqp.Channel, channelName string, callback func([]byte)) {
	messages, err := channel.Consume(
		channelName, // queue name
		"",          // consumer
		false,       // auto-ack
		false,       // exclusive
		false,       // no local
		false,       // no wait
		nil,         // arguments
	)
	if err != nil {
		log.Println(err)
	}

	go func() {
		for message := range messages {
			callback(message.Body)
		}
	}()
}

func jobResponseCallback(data []byte) {
	resp := new(JobResponse)
	err := json.Unmarshal(data, resp)
	if err != nil {
		log.Println(err)
		return
	}

	switch resp.Status {
	case JobStatusDone:
		log.Println("Job completed:", resp.Id)
		jobResultMap.Store(resp.Id, resp.Data)
		jobStatusMap.Store(resp.Id, JobStatusDone)
	case JobStatusInProgress:
		log.Println("Job processing:", resp.Id)
		jobStatusMap.Store(resp.Id, JobStatusInProgress)
	default:
		log.Println("Invalid job state", resp.Id)
	}
}

func main() {
	config := getConfig()

	// Create a new RabbitMQ connection.
	connectRabbitMQ, err := amqp.Dial(config.AmqpServerURL)
	if err != nil {
		panic(err)
	}
	defer connectRabbitMQ.Close()

	// Let's start by opening a channel to our RabbitMQ
	// instance over the connection we have already
	// established.
	channelRabbitMQ, err := connectRabbitMQ.Channel()
	if err != nil {
		panic(err)
	}
	defer channelRabbitMQ.Close()

	// Declare a queue for sending jobs to.
	declareQueue(channelRabbitMQ, config.JobsChannelName)
	// Declare a queue for receiving responses from.
	declareQueue(channelRabbitMQ, config.StatusChannelName)

	// Start consuming messages from the jobs queue.
	startConsuming(channelRabbitMQ, config.StatusChannelName, jobResponseCallback)

	// Create a new Fiber instance.
	app := fiber.New()

	// Add middleware.
	app.Use(
		logger.New(), // add simple logger
	)

	// Add route.
	app.Post("/jobs", func(c *fiber.Ctx) error {
		payload := new(Job)

		if err := c.BodyParser(payload); err != nil {
			return err
		}

		publishJob(channelRabbitMQ, config, c.Body())
		jobStatusMap.Store(payload.Id, JobStatusPending)

		c.Response().Header.Add("Location", "/jobs/status/"+payload.Id.String())

		// return 202 Accepted with location to /jobs/status/:id
		return c.Status(fiber.StatusAccepted).JSON(fiber.Map{
			"status": JobStatusPending.String(),
		})
	})

	app.Get("/jobs/status/:id", func(c *fiber.Ctx) error {
		id, err := uuid.Parse(c.Params("id"))
		if err != nil {
			return c.Status(fiber.StatusBadRequest).JSON(fiber.Map{
				"error": "Invalid job ID",
			})
		}

		status, ok := jobStatusMap.Load(id)
		if !ok {
			return c.Status(fiber.StatusNotFound).JSON(fiber.Map{
				"error": "Job not found",
			})
		}

		jsonResp := fiber.Map{
			"status": status.(JobStatus).String(),
		}

		if status == JobStatusDone {
			c.Response().Header.Add("Location", "/jobs/result/"+id.String())
		}

		return c.JSON(jsonResp)
	})

	app.Get("/jobs/result/:id", func(c *fiber.Ctx) error {
		id, err := uuid.Parse(c.Params("id"))
		if err != nil {
			return c.Status(fiber.StatusBadRequest).JSON(fiber.Map{
				"error": "Invalid job ID",
			})
		}

		result, ok := jobResultMap.Load(id)
		if !ok {
			return c.Status(fiber.StatusNotFound).JSON(fiber.Map{
				"error": "Job not found",
			})
		}

		return c.JSON(fiber.Map{
			"result": result,
		})
	})

	// Start Fiber API server.
	log.Fatal(app.Listen(fmt.Sprintf(":%s", config.ApiPort)))
}
