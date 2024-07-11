sudo podman run -d \
	--replace \
	--name pgtmp \
	-p 5432:5432 \
	-e POSTGRES_PASSWORD=mysecretpassword \
	-e POSTGRES_HOST_AUTH_METHOD=md5 \
	-e POSTGRES_INITDB_ARGS=--auth-host=md5 \
	postgres:16
