# 24-ps6-remote-clp

## Usage

In the SWI-Prolog console, you can use the following predicates to interact with the remote CLP service:

* `api_config/1`: Set the URL (url(...)) and API key (key(...)) for the remote CLP service.
    * The remote CLP service URL is `http://localhost:3000`
    * Example for production deployment: `api_config([url('https://remote-clp.kube.isc.heia-fr.ch/api'), key('<some_jwt>')]).`
* See the examples in `/remote_clp_client/examples` for more information on how to use the client.
* There is a admin interface available at `http://localhost:3000/admin` to create tokens for the users.

## Start locally

1. Install go v1.22.1 and python v3.11.8
2. Run `docker compose up rabbitmq -d`
3. Configure kubectl to have acces to the cluster (download the kubeconfig file from the rancher and set the KUBECONFIG environment variable to the path of the file or put it in ~/.kube/config)
4. Run `kubectl -n remote-clp create secret generic remote-clp-admin-password --from-literal=password=<admin_password>` to create the secret with the admin password for production
5. Run `kubectl -n remote-clp create secret generic remote-clp-jwt-secret --from-literal=secret=<jwt_secret>` to create the secret with the jwt secret for production
6. Run (ideally in a virtual environment but not necessary) `pip install -r requirements.txt` in `/remote_clp_service/worker`
7. Run `go run main.go` in `/remote_clp_service/job_dispatcher`
8. Run `swipl -l ./remote_clp.pl` in `/remote_clp_client` to interact with the client
    * Alternatively, you can use the example programs in `/remote_clp_client/examples` by running `swipl -l ./examples/<example_file>` in `/remote_clp_client`. Don't forget set the correct url and API key in the example files.

## Run on docker

1. Create a `.env` file in the root directory with the following content:
    * `ADMIN_PASSWORD=<admin_password>`
    * `JWT_SECRET=<jwt_secret>`
2. Run `docker compose up -d`
3. Follow the step 8 from the local setup
