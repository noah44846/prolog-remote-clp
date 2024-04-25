package main

import (
	"fmt"
	"os"
)

const (
	DefaultAmqpServerHost     = "localhost"
	DefaultAmqpServerPort     = "5672"
	DefaultAmqpServerUser     = "guest"
	DefaultAmqpServerPassword = "guest"
	DefaultJobsChannelName    = "remote-clp-jobs"
	DefaultStatusChannelName  = "remote-clp-status"
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

func GetConfig() Config {
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
