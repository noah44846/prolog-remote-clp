# 24-ps6-remote-clp

## Start locally

* Install go v1.22.1 and python v3.11.8
* Run `docker compose up rabbitmq -d`
* Configure kubectl to have acces to the cluster (download the kubeconfig file from the rancher and set the KUBECONFIG environment variable to the path of the file or put it in ~/.kube/config)
* Run `kubectl -n remote-clp create secret generic remote-clp-admin-password --from-literal=password=<admin_password>` to create the secret with the admin password for production
* Run `kubectl -n remote-clp create secret generic remote-clp-jwt-secret --from-literal=secret=<jwt_secret>` to create the secret with the jwt secret for production
* Run `pip install -r requirements.txt` in `/remote_clp_service/worker`
* Run `go run main.go` in `/remote_clp_service/job_dispatcher`
* Run `swipl` in `/remote_clp_client` to interact with the client
    * `[remote_clp].` to load the client

## Run on docker

* Run `docker compose up -d`
