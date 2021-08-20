

## Starting a Dockerized Shiny Application

Docker is a great way to contain and ship applications across the internet. In this example, we'll break down the run command for starting a dockerized shiny application.   

Here's an example of how to launch a dockerized Shiny application that will restart if an unexpected crash occurs. 
```
docker run -d \
            -v /var/run/docker.sock:/var/run/docker.sock \
            --net sp-example-net \
            --name=vulusr_beta \
            --restart=always \
            -p 80:3838 \
            vulusr_beta  
```
Here's a breakdown of the commands. 
  1. ```run```, runs a command in a new container
  2. ```-d```, This boolean command instructs the docker container whether to run in detached mode or not. By default, this command is true. In detached mode, you'll return to the terminal. Otherwise, you'll attach to the container where you can see the logs from the application.
  3. ```-v``` creates storage space 
  4. ```--net``` Establishing the direction for the container's network. In this example, we're using a particular network for [ShinyProxy](https://www.openanalytics.eu/tags/shinyproxy/) to communicate with the application.  [Read more on --net here](https://stackoverflow.com/questions/43316376/what-does-net-host-option-in-docker-command-really-do)
  5. ```--name``` name of the container 
  6. ```-p``` port map. In this example, a public call of 80 (http) is mapped to 3838 (default port that Shiny Server listens to). 
  7. ```--restart``` provides direction to container on exit. Default is ```no```, which doesn't automatically restart the container. In this example, we use ```always``` which, "Always restart[s] the container if it stops. If it is manually stopped, it is restarted only when Docker daemon restarts or the container itself is manually restarted." Other options are, 'on-failure' and 'unless-stopped'. [Read more here](https://docs.docker.com/config/containers/start-containers-automatically/)
  
      <br>**Automatic restart testing** <br>
      Get the container ID
      ``` 
      docker ps
      ```

      You can review your containers restart policy with the following
      ```
      docker inspect -f "{{ .HostConfig.RestartPolicy }}" <container_id>
      docker inspect -f "{{ .RestartCount }}" <container_id>
      docker inspect -f "{{ .State.StartedAt }}" <container_id>
      ```
      From there, you can look at the container process and grab the pid.
      ```
      docker exec -it <container_id> ps -aux 
      ```
      Then send a kill signal. 
      ```
      docker exec -it <container_id> kill -13 <pid>
      ```
      And check to ensure the restart count is greater than 0. 
      ```
      docker inspect -f "{{ .RestartCount }}" <container_id>
      ```
      [Source1](https://stackoverflow.com/questions/29680274/how-to-check-if-the-restart-policy-works-of-docker), [Source2](https://stackoverflow.com/questions/54460518/test-restart-policy-how-to-crash-a-container-such-that-it-restarts)
  8. The final line, ```vulusr_beta```, is the name of the container. 



