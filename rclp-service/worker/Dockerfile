FROM python:3.11

RUN wget https://raw.githubusercontent.com/vishnubob/wait-for-it/master/wait-for-it.sh
RUN chmod +x /wait-for-it.sh

WORKDIR /app

COPY . .

RUN pip install -r requirements.txt

CMD ["sh", "-c", "/wait-for-it.sh ${RABBITMQ_HOST}:${RABBITMQ_PORT} -- python app.py"]
