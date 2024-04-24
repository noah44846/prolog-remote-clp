import os
import sys
import logging
import json
from typing import NamedTuple, Optional
from enum import Enum
from uuid import UUID

from pika import BlockingConnection, URLParameters

from model_parser import solve_job

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
handler = logging.StreamHandler()
handler.setFormatter(logging.Formatter("%(message)s"))
logger.addHandler(handler)


class BrokerConfiguration(NamedTuple):
    pika_config: URLParameters
    jobs_channel_name: str
    status_channel_name: str


def broker_configuration() -> BrokerConfiguration:
    host = os.environ.get('RABBITMQ_HOST', 'localhost')
    port = os.environ.get('RABBITMQ_PORT', '5672')
    user = os.environ.get('RABBITMQ_USER', 'guest')
    password = os.environ.get('RABBITMQ_PASSWORD', 'guest')
    url = f'amqp://{user}:{password}@{host}:{port}/'
    jobs_channel_name = os.environ.get(
        'RABBITMQ_JOBS_CHANNEL', 'remote-clp-jobs')
    status_channel_name = os.environ.get(
        'RABBITMQ_STATUS_CHANNEL', 'remote-clp-status')
    con = URLParameters(url)
    return BrokerConfiguration(pika_config=con, jobs_channel_name=jobs_channel_name, status_channel_name=status_channel_name)


config = broker_configuration()


class RclpJobStatus(Enum):
    IN_PROGRESS = 'in_progress'
    DONE = 'done'


def publish_status(ch, job_id: UUID, status: RclpJobStatus, result: Optional[list[dict[str, int]]] | None = None, error: Optional[str] = None):
    message = {'id': str(job_id), 'status': status.value,
               'data': result, 'error': error}
    ch.basic_publish(
        exchange='', routing_key=config.status_channel_name, body=json.dumps(message))


def process_job(ch, job: dict):
    job_id = UUID(job['id'])

    logger.debug(f' [X] Processing job {job_id}')
    publish_status(ch, job_id, RclpJobStatus.IN_PROGRESS)

    try:
        result = solve_job(job)
        publish_status(ch, job_id, RclpJobStatus.DONE, result)
        logger.debug(f' [X] Job done {result}')
        logger.debug(f' [X] Job done {job_id}')
    except Exception as e:
        logger.error(f' [X] Job failed {job_id}')
        logger.exception(e)
        publish_status(ch, job_id, RclpJobStatus.DONE, error=str(e))


def on_job_received(ch, method, properties, body: bytes):
    data = body.decode()
    logger.debug(f' [x] Received {data}')
    json_data = json.loads(data)
    logger.debug(f' [x] Received {json_data["id"]}')

    process_job(ch, json_data)

    ch.basic_ack(delivery_tag=method.delivery_tag)


def listen_for_tasks():
    connection = BlockingConnection(config.pika_config)
    channel = connection.channel()

    channel.queue_declare(queue=config.jobs_channel_name, durable=True)
    channel.queue_declare(queue=config.status_channel_name, durable=True)

    channel.basic_qos(prefetch_count=1)
    channel.basic_consume(queue=config.jobs_channel_name,
                          on_message_callback=on_job_received)
    logger.debug(' [*] Waiting for messages. To exit press CTRL+C')

    try:
        channel.start_consuming()
    except KeyboardInterrupt:
        logger.debug('Interrupted')
        try:
            sys.exit(0)
        except SystemExit:
            os._exit(0)


if __name__ == '__main__':
    listen_for_tasks()
