# 24-ps6-remote-clp

## Start locally

* Install go v1.22.1 and python v3.11.8
* Run `docker compose up rabbitmq -d`
* Run `pip install -r requirements.txt` in `/remote_clp_service/worker`
* Run `go run main.go` in `/remote_clp_service/job_dispatcher`
* Run `swipl` in `/remote_clp_client` to interact with the client
    * `[remote_clp].` to load the client

## Run on docker

* Run `docker compose up -d`
