FROM golang:1.22-alpine AS builder

# Move to working directory (/build).
WORKDIR /build

# Copy and download dependency using go mod.
COPY go.mod go.sum ./
RUN go mod download

# Copy the code into the container.
COPY . .

# Set necessary environment variables needed 
# for our image and build the sender.
ENV CGO_ENABLED=0 GOOS=linux GOARCH=amd64
RUN go build -ldflags="-s -w" -o job-dispatcher .

FROM bash

# Copy binary and config files from /build 
# to root folder of scratch container.
COPY --from=builder ["/build/job-dispatcher", "/"]

COPY ./wait-for-it.sh /wait-for-it.sh
RUN chmod +x /wait-for-it.sh

# Command to run when starting the container.
ENTRYPOINT ["sh", "-c", "/wait-for-it.sh ${RABBITMQ_HOST}:${RABBITMQ_PORT} -- /job-dispatcher"]
