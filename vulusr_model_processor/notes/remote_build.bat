ECHO enter ec2 instance  
ssh -i "usdot_vul_usr_2021.pem" ubuntu@ec2-3-237-2-114.compute-1.amazonaws.com

ECHO sudo up 
sudo su

ECHO login to docker 
docker login 

ECHO username: {username}
ECHO password: {password}

ECHO stop current running container. Otherwise port will not be avaiable for mapping.  
docker ps 
docker stop {CONTAINER ID} (found on left hand column after docker ps, e.g., 5dbea0ae50c0)

ECHO pull repo
docker pull bikingman/processor

ECHO tag
docker tag bikingman/processor processor

ECHO 
docker run -d -v /var/run/docker.sock:/var/run/docker.sock --net sp-example-net -p 81:3839 processor

ECHO check logs 
docker ps 
docker logs {CONTAINER ID}

ECHO remove unused containers, with -f this does not ask for confirmation 
docker container prune  
docker image prune