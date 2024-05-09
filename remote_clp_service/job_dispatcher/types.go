package main

import (
	"encoding/json"
	"fmt"
	"sync"
	"time"

	"github.com/google/uuid"
)

type Job struct {
	Id uuid.UUID `json:"id"`
}

type JobResponse struct {
	Id       uuid.UUID         `json:"id"`
	Status   JobStatus         `json:"status"`
	Error    string            `json:"error"`
	Data     *[]map[string]int `json:"data"`
	Username string            `json:"username"`
}

type TokenRequest struct {
	AdminPassword string `json:"admin_password"`
	TokenUsername string `json:"token_username"`
	TokenExpiry   int64  `json:"token_expiry"`
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

type logWriter struct {
}

func (writer logWriter) Write(bytes []byte) (int, error) {
	return fmt.Print(time.Now().UTC().Format("15:04:05") + " [INFO] " + string(bytes))
}
