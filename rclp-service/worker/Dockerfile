FROM python:3.11

WORKDIR /app

COPY . .

COPY ./wait-for-it.sh /wait-for-it.sh
RUN chmod +x /wait-for-it.sh

RUN pip install -r requirements.txt

CMD ["sh", "-c", "/wait-for-it.sh ${RABBITMQ_HOST}:${RABBITMQ_PORT} -- python app.py"]
