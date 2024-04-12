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
RUN go build -ldflags="-s -w" -o job_dispatcher .

FROM bash

# Copy binary and config files from /build 
# to root folder of scratch container.
COPY --from=builder ["/build/job_dispatcher", "/"]

RUN wget https://raw.githubusercontent.com/vishnubob/wait-for-it/master/wait-for-it.sh
RUN chmod +x /wait-for-it.sh

# Command to run when starting the container.
ENTRYPOINT ["/job_dispatcher"]
