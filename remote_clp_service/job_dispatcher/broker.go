package main

import (
	"log"

	"github.com/streadway/amqp"
)

type ConnectionWrapper struct {
	connection *amqp.Connection
	channel    *amqp.Channel
	config     *Config
}

func (c *ConnectionWrapper) declareQueue(queueName string) {
	_, err := c.channel.QueueDeclare(
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

func (c *ConnectionWrapper) PublishJob(job []byte) {
	// Create a message to publish.
	message := amqp.Publishing{
		ContentType: "application/json",
		Body:        job,
	}

	// Attempt to publish a message to the queue.
	if err := c.channel.Publish(
		"",                       // exchange
		c.config.JobsChannelName, // queue name
		false,                    // mandatory
		false,                    // immediate
		message,                  // message to publish
	); err != nil {
		panic(err)
	}
}

func (c *ConnectionWrapper) startConsuming(jobResultCallback func([]byte)) {
	messages, err := c.channel.Consume(
		c.config.StatusChannelName, // queue name
		"",                         // consumer
		false,                      // auto-ack
		false,                      // exclusive
		false,                      // no local
		false,                      // no wait
		nil,                        // arguments
	)
	if err != nil {
		log.Println(err)
	}

	go func() {
		for message := range messages {
			jobResultCallback(message.Body)
			message.Ack(false)
		}
	}()
}

func Connect(config *Config, jobResultCallback func([]byte)) ConnectionWrapper {
	// Create a new RabbitMQ connection.
	connectRabbitMQ, err := amqp.Dial(config.AmqpServerUrl)
	if err != nil {
		panic(err)
	}

	// Let's start by opening a channel to our RabbitMQ
	// instance over the connection we have already
	// established.
	channelRabbitMQ, err := connectRabbitMQ.Channel()
	if err != nil {
		panic(err)
	}

	connectionWrapper := ConnectionWrapper{
		connection: connectRabbitMQ,
		channel:    channelRabbitMQ,
		config:     config,
	}

	// Declare a queue for sending jobs to.
	connectionWrapper.declareQueue(config.JobsChannelName)
	// Declare a queue for receiving responses from.
	connectionWrapper.declareQueue(config.StatusChannelName)

	// Start consuming messages from the jobs queue.
	connectionWrapper.startConsuming(jobResultCallback)

	return connectionWrapper
}

func (c ConnectionWrapper) Close() {
	c.channel.Close()
	c.connection.Close()
}
