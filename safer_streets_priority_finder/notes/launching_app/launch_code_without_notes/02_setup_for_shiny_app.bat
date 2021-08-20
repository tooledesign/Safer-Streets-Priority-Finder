ECHO EDITED docker.service
sudo nano /lib/systemd/system/docker.service
ECHO Modify the line with ‘ExecStart’.
ECHO ExecStart=/usr/bin/dockerd -H fd:// -D -H tcp://127.0.0.1:2375
ECHO exit nano and save
sudo systemctl daemon-reload
sudo systemctl restart docker
docker network create sp-example-net