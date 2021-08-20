REM this script will build an EC2 instance with Ubuntu 20.04 (x64 bit or ARM) with docker 
REM need to build with sudo privileges 
REM generally the first part of this follows docker's recommendations https://docs.docker.com/engine/install/ubuntu/

REM Step 1.launch in EC2 instance with Ubuntu 20.04 (x64 bit or ARM)
REM Step 2. Connect to Your Amazon EC2 Instance
REM for information on these steps, https://docs.aws.amazon.com/quickstarts/latest/vmlaunch/step-2-connect-to-instance.html

REM after you've connected to your EC2 instance, sudo up
sudo su

REM first we need to update the local system 
sudo apt-get update

REM Update the apt package index and install packages to allow apt to use a repository over HTTPS
sudo apt-get install \
  apt-transport-https \
  ca-certificates \
  curl \
  gnupg \
  lsb-release \
  docker.io

REM If you don't have a Docker ID, head over to https://hub.docker.com to create one
service docker start
docker login 
ECHO username: {username}
ECHO password: {password}

