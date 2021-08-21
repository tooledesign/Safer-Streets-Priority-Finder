# Tool info and startup

The report processor is an Rshiny application that lives inside of a Docker container, on the same EC2 instance as the model processor. It is set to always be running, and does not need to be regularly restarted.

Verify that the docker container is not running:

`sudo docker login`

`sudo docker ps`

Assuming that there is no running container with the name `report_processor`, start it:

`sudo docker run -d -v /var/run/docker.sock/var/run/docker.sock --net sp-example-net --name=report_processor --restart=always -p 25250:25250 report_proc
essor`

Verify the report processor is now running:

`sudo docker ps`

# Building the docker container

Make sure you have env_variables.R available locally in root docker directory when building the docker container. The build process will copy it into the container, making it available to the RShiny application.

Run `docker build .` from root docker directory where Dockerfile is available.

Tag image

`docker tag [imageID] tooledesign/report_processor:[version_number]`

Push to hub in Docker desktop

On the EC2 instance, do `sudo docker pull tooledesign/report_processor[version_number]`

Tag image

`sudo docker tag tooledesign/report_processor:[version_number] report_processor`

Start container

`sudo docker run -d -v /var/run/docker.sock/var/run/docker.sock --net sp-example-net --name=report_processor --restart=always -p 25250:25250 report_proc
essor`
