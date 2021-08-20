# Safer-Streets-Priority-Finder

The Safer Streets Priority Finder enables you to analyze the risk to bicyclists and pedestrians on your community’s roads. You can use your local road, crash, and study area data or select from nationally available datasets to:

1. Explore descriptive statistics related to your crash data
2. Develop a Sliding Windows Analysis using historical crash data to inform a High Injury Network
3. Develop a Safer Streets Model to estimate risk along your road network, even in areas that haven't had any reported crashes recently

[Checkout the Safer Streets Priority Finder Tool here.](https://www.saferstreetspriorityfinder.com/)

<img width="677" alt="Screen Shot 2021-08-19 at 4 30 40 PM" src="https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/landing_page/images/nola_bike_crash_score_w_study_reduced_social_media.png">


## How to launch the SSPF, what you'll need

### 1. Linux 
You'll need sudo access on a Linux command line. 

### 2. A PostgreSQL database
You'll need to build a PostgreSQL database that follows the schema and table structure provided [here](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/build_psql_db.sql).

### 3. Static tables 
Upload the following table into the 'static' schema on the PostgreSQL datatable. 
1. [Latest 5 years of FARS data](https://www.nhtsa.gov/file-downloads?p=nhtsa/downloads/FARS/)  
2. [USDOT-OST / Pedestrian-Fatality-Risk-Project](https://github.com/USDOT-OST/Pedestrian-Fatality-Risk-Project)
3. [US Census Counties](https://www.census.gov/data.html)
4. [National Open Street Map Roads Dataset, Avaiable from GeoFabrik](https://www.geofabrik.de/data/download.html)

Each dataset listed above needs a state and county [Federal Information Processing Standards (FIPS)](https://www.nist.gov/standardsgov/compliance-faqs-federal-information-processing-standards-fips#:~:text=FIPS%20are%20standards%20and%20guidelines,by%20the%20Secretary%20of%20Commerce) code, where the state FIPS code is always two digits in length, and county codes are three digits, including leading zeros as needed. FIPS codes on each dataset should be stored in TEXT or VARCHAR format. 

### 4. Complete system variables 
Fill out your system variables, so the Docker container can link the user information with the data. You'll need to do this [here](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/safer_streets_priority_finder/env_variables.R) and [here](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/vulusr_model_processor/env_variables.R). Do not relocate this file. 

### 5. Docker Installed 
If you don't already have an Docker installed on your machine, you can get started [here](https://docs.docker.com/get-docker/).

## Launching the application

There are two ways to do this. The easiest way forward will be to hookup the tool to your postgres database and launch a Docker container with the model processor. Once those are ready, launch the tool from your RStudio command line. 

To Launch the model processor ```cd``` to the directory with the model process Dockerfile: ```./Safer-Streets-Priority-Finder/vulusr_model_processor/```. Then build the docker container with, ```docker build -t ssfp .```. After that's complete, you launch the container with, ```docker run -d -v /var/run/docker.sock:/var/run/docker.sock --net sp-example-net --name=ssfp --restart=always -p 9000:3838 vulusr_beta```. The container will be running at port 9000 in this case. To stop the container, use the command, ```docker stop ssfp```. Prune off unwanted containers with, ```docker container prune ssfp ```. 

Once you have your model processor running, open RStudio and open the project file found [here](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/safer_streets_priority_finder/safer_streets_priority_finder.Rproj).

If you have any questions you can reach out to the developers by emailing, saferstreetspriorityfinder@tooledesign.com.
