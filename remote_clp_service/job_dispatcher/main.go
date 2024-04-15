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

const (
	DefaultAmqpServerHost     = "localhost"
	DefaultAmqpServerPort     = "5672"
	DefaultAmqpServerUser     = "guest"
	DefaultAmqpServerPassword = "guest"
	DefaultJobsChannelName    = "rclp-jobs"
	DefaultStatusChannelName  = "rclp-status"
	DefaultApiPort            = "3000"
	AmqpServerHostEnv         = "RABBITMQ_HOST"
	AmqpServerPortEnv         = "RABBITMQ_PORT"
	AmqpServerUserEnv         = "RABBITMQ_USER"
	AmqpServerPasswordEnv     = "RABBITMQ_PASSWORD"
	JobsChannelNameEnv        = "RABBITMQ_JOBS_CHANNEL_NAME"
	StatusChannelNameEnv      = "RABBITMQ_STATUS_CHANNEL_NAME"
	ApiPortEnv                = "PORT"
	AmqpServerUrlFormat       = "amqp://%s:%s@%s:%s/"
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
	Id     uuid.UUID         `json:"id"`
	Status JobStatus         `json:"status"`
	Error  string            `json:"error"`
	Data   *[]map[string]int `json:"data"`
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

type JobResultMap sync.Map

var jobResultMap JobResultMap

func (m *JobResultMap) Load(key uuid.UUID) (*JobResponse, bool) {
	value, ok := (*sync.Map)(m).Load(key)
	if !ok {
		return nil, false
	}
	return value.(*JobResponse), true
}

func (m *JobResultMap) Store(key uuid.UUID, value *JobResponse) {
	(*sync.Map)(m).Store(key, value)
}

func getConfig() Config {
	getEnvOrDefault := func(key, defaultValue string) string {
		value := os.Getenv(key)
		if value == "" {
			return defaultValue
		}
		return value
	}

	amqpServerHost := getEnvOrDefault(AmqpServerHostEnv, DefaultAmqpServerHost)
	amqpServerPort := getEnvOrDefault(AmqpServerPortEnv, DefaultAmqpServerPort)
	amqpServerUser := getEnvOrDefault(AmqpServerUserEnv, DefaultAmqpServerUser)
	amqpServerPassword := getEnvOrDefault(AmqpServerPasswordEnv, DefaultAmqpServerPassword)
	amqpServerURL := fmt.Sprintf(AmqpServerUrlFormat, amqpServerUser, amqpServerPassword, amqpServerHost, amqpServerPort)
	jobsChannelName := getEnvOrDefault(JobsChannelNameEnv, DefaultJobsChannelName)
	statusChannelName := getEnvOrDefault(StatusChannelNameEnv, DefaultStatusChannelName)
	apiPort := getEnvOrDefault(ApiPortEnv, DefaultApiPort)

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

func startConsuming(channel *amqp.Channel, channelName string) {
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
			jobResponseCallback(message.Body)
			message.Ack(false)
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
		jobResultMap.Store(resp.Id, resp)
	case JobStatusInProgress:
		log.Println("Job processing:", resp.Id)
		jobResultMap.Store(resp.Id, resp)
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
	startConsuming(channelRabbitMQ, config.StatusChannelName)

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

		jobResponse := JobResponse{
			Id:     payload.Id,
			Status: JobStatusInProgress,
		}
		jobResultMap.Store(payload.Id, &jobResponse)

		c.Response().Header.Add("Location", "/jobs/results/"+payload.Id.String())

		// return 202 Accepted with location to /jobs/results/:id
		return c.Status(fiber.StatusAccepted).JSON(jobResponse)
	})

	app.Get("/jobs/results/:id", func(c *fiber.Ctx) error {
		id, err := uuid.Parse(c.Params("id"))
		if err != nil {
			return c.Status(fiber.StatusBadRequest).JSON(fiber.Map{
				"error": "Invalid job ID",
			})
		}

		results, ok := jobResultMap.Load(id)
		if !ok {
			return c.Status(fiber.StatusNotFound).JSON(fiber.Map{
				"error": "Job not found",
			})
		}

		return c.JSON(results)
	})

	// Start Fiber API server.
	log.Fatal(app.Listen(fmt.Sprintf(":%s", config.ApiPort)))
}
