package main

import (
	"os"
)

const (
	DefaultAmqpServerUrl     = "amqp://guest:guest@localhost:5672/"
	DefaultJobsChannelName   = "remote-clp-jobs"
	DefaultStatusChannelName = "remote-clp-status"
	DefaultApiPort           = "3000"
	DefaultJwtSecret         = "secret"
	DefaultAdminPassword     = "admin"
	AmqpServerUrlEnv         = "RABBITMQ_URL"
	JobsChannelNameEnv       = "RABBITMQ_JOBS_CHANNEL_NAME"
	StatusChannelNameEnv     = "RABBITMQ_STATUS_CHANNEL_NAME"
	ApiPortEnv               = "PORT"
	JwtSecretEnv             = "JWT_SECRET"
	AdminPasswordEnv         = "ADMIN_PASSWORD"
)

type Config struct {
	AmqpServerUrl     string
	JobsChannelName   string
	StatusChannelName string
	ApiPort           string
	jwtSecret         []byte
	adminPassword     string
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
	apiPort := getEnvOrDefault(ApiPortEnv, DefaultApiPort)
	jwtSecretStr := getEnvOrDefault(JwtSecretEnv, DefaultJwtSecret)
	adminPassword := getEnvOrDefault(AdminPasswordEnv, DefaultAdminPassword)

	return Config{
		AmqpServerUrl:     amqpServerUrl,
		JobsChannelName:   jobsChannelName,
		StatusChannelName: statusChannelName,
		ApiPort:           apiPort,
		jwtSecret:         []byte(jwtSecretStr),
		adminPassword:     adminPassword,
	}
}
