sudo podman run -d \
	--replace \
	--name simplecqrs \
	-p 5432:5432 \
	-e POSTGRES_PASSWORD=simplecqrs \
	-e POSTGRES_HOST_AUTH_METHOD=md5 \
	-e POSTGRES_INITDB_ARGS=--auth-host=md5 \
	postgres:16
