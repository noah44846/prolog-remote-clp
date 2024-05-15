# Conversion

```sh
mmdc -i diagrams.md -o out/diagram.png -s 3
```

# Diagrams

## Planning

```mermaid
---
config:
  theme: default
  gantt:
    useWidth: 1200
---
gantt
    title 2024 PS6 Prolog Remote Contraint Logic Programming
    dateFormat DD.MM.YYYY
    axisFormat %d.%m
    tickInterval 1week
    weekday monday
    todayMarker off
    excludes weekends

    Documentation submission  : milestone, m1, 16.05.2024, 0h
    Final presentation        : milestone, m2, 22.05.2024, 4h

    section Report
        Specification    : d1, 21.02.2024, 10d
        Write the report :                 51d
    section Tasks
        Analysis of API architecture         : a1,        06.03.2024, 10d
        Design API                           :                        5d
        Design client library                : a2,        20.03.2024, 5d
        Implement basic API                  :                        8d
        Implement client library             :                        7d
        First prototype                      : milestone,             0h
        Deploy the API on kubernetes cluster : a3,        17.04.2024, 3d
        Implement remaining API endpoints    :                        10d
        Implement authentication             :                        5d
        Working application                  : milestone,             0h
        Performance testing                  :                        3d
```

## Sequence diagram

```mermaid
sequenceDiagram
    autonumber
    actor user as User
    participant prolog_client as Prolog client
    participant job_dispatcher as Job dispatcher
    participant worker as Worker
    participant rabbitmq_broker as RabbitMQ broker
    user->>+prolog_client: execute clp program
    prolog_client->>+job_dispatcher: POST /api/jobs: the problem model as JSON
    job_dispatcher-)rabbitmq_broker: push new job
    job_dispatcher-->>-prolog_client: HTTP 202 Location: /api/results/<job_id>
    rabbitmq_broker-)+worker: give job to one of the workers
    worker-)rabbitmq_broker: push job status update from pending to in_progress
    rabbitmq_broker-)job_dispatcher: give job status update
    worker->>worker: solve model
    loop poll job status every second until status is done
        prolog_client->>+job_dispatcher: GET /api/results/<job_id>: poll job status
        job_dispatcher-->>-prolog_client: HTTP 200 job status: in_progress
    end
    worker-)-rabbitmq_broker: push job status update to done + results or error
    rabbitmq_broker-)job_dispatcher: give job status update with results or error
    prolog_client->>+job_dispatcher: GET /api/results/<job_id>: poll job status
    job_dispatcher-->>-prolog_client: HTTP 200 job status: done + data or error message
    prolog_client-->>-user: parsed result of model
```

