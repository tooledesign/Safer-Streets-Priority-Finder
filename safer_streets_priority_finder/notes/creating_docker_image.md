## Docker setup 
Notes on how I created local and (to come) remote docker images for lauching app via containers. 
  - Docker images help to manage package version control 
  - Allows for concurrent user problem whereby there will be no interaction between sessions
    - i.e. also addresses one of RShiny’s essential drawbacks: Shiny creates only a single R session per app, meaning that if multiple users access the same app, they all work with the same R session [source](https://www.statworx.com/en/blog/how-to-dockerize-shinyapps/)

## Local setup
Since I'm building this with the golem framework, I **started** with the associated book and section dedicated to deploying an app using Docker to launch a local version. 
  - [Link to book](https://engineering-shiny.org/use-case-building-an-app-from-start-to-finish.html#step-5-deploy-1)

golem::add_dockerfile() is a great command that builds the Docker image file
- golem::add_dockerfile() located on dev/03_deploy.R
  - creates template DOCKER file generated in parent directory 
- once that finished, I updated the file to include other packages (sf, DBI... etc) 
- leaflet depends on the libpng library, added to docker image with RUN apt-get update && apt-get install -y libpng-dev. I also added a bunch of other lower level installs including GDAL PROJ and GEOS
- replaced https://cran.rstudio.com with https://packagemanager.rstudio.com/all/latest in the docker image for faster compilation, since we have the versions identified, it's alright 
- installed node.js and ensured /usr/local/bin is in $PATH 
  - [Noje JS source](ttps://nodejs.org/en/download/)
- built docker image on CL with notes [at the bottom of this book.](https://engineering-shiny.org/use-case-building-an-app-from-start-to-finish.html#step-5-deploy-1)
- once the docker image was built, I launched a container it with, http://localhost:2811/

#### Other sources
   - Source: https://business-science.github.io/shiny-production-with-aws-book/docker-containers.html
   
## AWS EC2 setup
Generally, I followed [this tutorial](https://www.bryanwhiting.com/2019/02/rshiny-on-docker-part1/)

#### Sources 
  - https://www.shafquatarefeen.com/aws-ec2-shiny-windows/
  - https://aws.amazon.com/blogs/big-data/running-r-on-aws/
  
 