package main

import (
	"os"
)

const (
	DefaultAmqpServerUrl     = "amqp://guest:guest@localhost:5672/"
	DefaultJobsChannelName   = "remote-clp-jobs"
	DefaultStatusChannelName = "remote-clp-status"
	DefaultRedisUrl          = "localhost:6379"
	DefaultApiPort           = "3000"
	AmqpServerUrlEnv         = "RABBITMQ_URL"
	JobsChannelNameEnv       = "RABBITMQ_JOBS_CHANNEL_NAME"
	StatusChannelNameEnv     = "RABBITMQ_STATUS_CHANNEL_NAME"
	RedisUrlEnv              = "REDIS_URL"
	ApiPortEnv               = "PORT"
)

type Config struct {
	AmqpServerUrl     string
	JobsChannelName   string
	StatusChannelName string
	RedisUrl          string
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

	amqpServerUrl := getEnvOrDefault(AmqpServerUrlEnv, DefaultAmqpServerUrl)
	jobsChannelName := getEnvOrDefault(JobsChannelNameEnv, DefaultJobsChannelName)
	statusChannelName := getEnvOrDefault(StatusChannelNameEnv, DefaultStatusChannelName)
	redisUrl := getEnvOrDefault(RedisUrlEnv, DefaultRedisUrl)
	apiPort := getEnvOrDefault(ApiPortEnv, DefaultApiPort)

	return Config{
		AmqpServerUrl:     amqpServerUrl,
		JobsChannelName:   jobsChannelName,
		StatusChannelName: statusChannelName,
		RedisUrl:          redisUrl,
		ApiPort:           apiPort,
	}
}
