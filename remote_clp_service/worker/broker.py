import os
import sys
import logging
import json
import threading
import time
from typing import NamedTuple, Optional
from enum import Enum
from uuid import UUID

from pika import BlockingConnection, URLParameters

from model_parser import solve_job


RABBITMQ_HEARTBEAT_INTERVAL = 180 # 3 minutes


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
    url = os.environ.get('RABBITMQ_URL', f'amqp://guest:guest@localhost:5672/?heartbeat={RABBITMQ_HEARTBEAT_INTERVAL}')
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
        result, time = solve_job(job)
        publish_status(ch, job_id, RclpJobStatus.DONE, result)
        logger.debug(f' [X] Job done: {result}')
        logger.debug(f' [X] Job done: {len(result)} solutions')
        logger.debug(f' [X] Job done: id {job_id}')
        logger.debug(f' [X] Job done: time {time}')
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


def send_heartbeat(connection):
    while True:
        try:
            connection.add_callback_threadsafe(lambda: connection.process_data_events(time_limit=1))
        except Exception as e:
            logger.warning(f"Error sending heartbeat: {e}")
        time.sleep(RABBITMQ_HEARTBEAT_INTERVAL)


def listen_for_tasks():
    connection = BlockingConnection(config.pika_config)
    channel = connection.channel()

    channel.queue_declare(queue=config.jobs_channel_name, durable=True)
    channel.queue_declare(queue=config.status_channel_name, durable=True)

    channel.basic_qos(prefetch_count=1)
    channel.basic_consume(queue=config.jobs_channel_name,
                          on_message_callback=on_job_received)
    logger.debug(' [*] Waiting for messages. To exit press CTRL+C')

    # Start the heartbeat thread
    heartbeat_thread = threading.Thread(target=send_heartbeat, args=(connection,))
    heartbeat_thread.start()

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
