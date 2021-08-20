# Safer-Streets-Priority-Finder

The Safer Streets Priority Finder enables you to analyze the risk to bicyclists and pedestrians on your community’s roads. You can use your local road, crash, and study area data or select from nationally available datasets to:

1. Explore descriptive statistics related to your crash data
2. Develop a Sliding Windows Analysis using historical crash data to inform a High Injury Network
3. Develop a Safer Streets Model to estimate risk along your road network, even in areas that haven't had any reported crashes recently

[Checkout the Safer Streets Priority Finder Tool here.](https://www.saferstreetspriorityfinder.com/)

<img width="677" alt="Screen Shot 2021-08-19 at 4 30 40 PM" src="https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/landing_page/images/nola_bike_crash_score_w_study_reduced_social_media.png">


## How to launch the SSPF, what you'll need

1. **Linux** - You'll need sudo access on a Linux command line. 
2. **A PostgreSQL Relational Database** - Access to a PostgreSQL database that follows the schema and table structure provided [here](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/build_psql_db.sql).
3. **Static tables** - Upload the following table into the 'static' schema on the PostgreSQL datatable. 
  1. [Fatality Analysis Reporting System (FARS) 2015 - 2019](https://www.nhtsa.gov/file-downloads?p=nhtsa/downloads/FARS/) - as, static.national_fclass_priors
  2. [USDOT-OST / Pedestrian-Fatality-Risk-Project](https://github.com/USDOT-OST/Pedestrian-Fatality-Risk-Project) - as, static.fars_processed
  3. [US Census Counties](https://www.census.gov/data.html) - as, static.us_county_2018
  4. [National Open Street Map Roads Dataset, Available from GeoFabrik](https://www.geofabrik.de/data/download.html) - as, static.osm_centerlines
  5. [Functional Classification Priors](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/national_fclass_priors.csv) - as, static.osm_centerlines
      
      Datasets 1-4 need a state and county [Federal Information Processing Standards (FIPS)](https://www.nist.gov/standardsgov/compliance-faqs-federal-information-processing-standards-fips#:~:text=FIPS%20are%20standards%20and%20guidelines,by%20the%20Secretary%20of%20Commerce) code, where the state FIPS code is always two digits in length, and county codes are three digits, including leading zeros as needed. FIPS codes on each dataset should be stored in TEXT or VARCHAR format.
      
      See [this file](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/build_psql_db.sql) for more infomration on the data structure for each table listed above. 
      
4. **Complete system variables** - Fill out your system variables, so the Docker container can link the user information with the data. You'll need to do this [here](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/safer_streets_priority_finder/env_variables.R) and [here](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/vulusr_model_processor/env_variables.R). Do not relocate this file. 
5. **Docker** - If you don't already have Docker installed on your machine, you can get started [here](https://docs.docker.com/get-docker/).


## How to launch the application

The easiest way to launch the application is setup a PostgreSQL database following the structure provided above. Once that's ready, launch a Docker container with the model processor, and a seperate container with the SSPF Tool.

### Step 1. Clone the Safer Streets Priority Finder repository 

``` sudo up ```

``` git clone https://github.com/tooledesign/Safer-Streets-Priority-Finder.git ```

### Step 2. Update system variables 

Navigate to the first file where system variables are required: 

``` cd Safer-Streets-Priority-Finder/safer_streets_priority_finder/ ```

You can update the system variables on the CL with ```nano``` text editor.

``` nano env_variables.R ```

Update all variables in that file. Here's a breakdown of what each variable refers to: 

1. **SSPF_EMAIL_PASSWORD** - password to an email account that sends a notification to users that a model has started or finished.
2. **SSPF_EMAIL_ADDRESS** - email address to send notification 
3. **SSPF_AMAZON_HOST_ADDRESS***  - host address to PostgreSQL Relational Database 
4. **SSPF_AMAZON_PASSWORD** - password to PostgreSQL Relational Database
5. **SSPF_AMAZON_USERNAME** - username for PostgreSQL Relational Database
6. **SSPF_AMAZON_DATABASE** - the name of the database with schema structure for the tool 

Now do the same for the model processor. 

``` cd .. ``` 

``` cd Safer-Streets-Priority-Finder/vulusr_model_processor/ ```

``` nano env_variables.R ```

### Step 3. Docker setup

Note: by default, Docker runs on root. Either make sure you are logged in as root by using `sudo su`, or prepend all of the following Docker commands with `sudo`.

First, make sure you're logged into your Docker account. 

``` docker login ```

If you haven't already done so, you'll need a network to link systems together. 

``` docker network create sp-example-net ```

### Step 4. Launch model processor 

From the ```Safer-Streets-Priority-Finder/vulusr_model_processor/``` directory, build a local image from the ```Dockerfile```. 

Then build the model processor image with the following: 

```docker build -t sspf_model_processor .```

This will build an image called ```sspf_model_processor```. 

After that's complete, you launch a container with, 

```docker run -d -v /var/run/docker.sock:/var/run/docker.sock --net sp-example-net --name=sspf_model_processor --restart=always -p 9001:3839 sspf_model_processor``` 

At this time, your model processor is running in a detached Docker container listening for a user to request a model. The model will port everything to the PostgreSQL database identified in the env_variables.R file. 

To stop the container, use the command, ```docker stop sspf_model_processor```. 

Prune off unwanted containers with, ```docker container prune sspf_model_processor ```. 

### Step 4. Locally launch the Safer Streets Priority Finder

Navigate to the safer-streets-priority-finder directory. This directory should container a Dockerfile. 

``` cd Safer-Streets-Priority-Finder/safer_streets_priority_finder/ ```

Then build the Safer Streets Priority Finder with the following: 

```docker build -t sspf .```

```docker run -d -v /var/run/docker.sock:/var/run/docker.sock --net sp-example-net --name=sspf --restart=always -p 9000:3838 sspf``` 

Now navigate to http://localhost:9001/, and you should see the Safer Streets Priority Finder.

## Maintaining the model processor manually

The model processor is designed to run **one time only** before needing to have the Docker container refreshed. This process can be done manually via the following steps, or can be automated (details below following the manual process).

### Step 1. Stop the model processor Docker container.

Run the following command to stop the Docker container that is running the model processor.

```docker stop sspf_model_processor```

### Step 2. Remove the old Docker container.

Run the following commmand to remove the old Docker container.

```docker container prune -f```

### Step 3. Reset Docker container status flag in PostgreSQL database.

Run the following SQL query from a SQL editor (psql, DBeaver, PGAdmin, etc.) to update the Docker container status flag.

```UPDATE gen_management.docker_status SET is_fresh = TRUE;```

### Step 4. Start a new docker container to run the SSPF model processor.

Run the following command to spawn a new Docker container running the SSPF model processor.

```docker run -d -v /var/run/docker.sock:/var/run/docker.sock --net sp-example-net --name=sspf_model_processor --restart=always -p 9001:3839 sspf_model_processor```

## Maintaining the model processor automatically

**If you want to automate the process outlined in the above section, to avoid having to manually reset the Docker container each time you run a model, use the ```dockerManager.py``` [script](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/vulusr_model_processor/python/dockerManager.py).**

```dockerManager.py``` is a python tool that automatically refreshes the model processor Docker container after successfully running a model, and is run by a linux cron job. The following instructions detail how to set up and use this tool.

### Step 1. Setup psycopg2

```psycopg2``` is a python module required for communicating with the PostgreSQL database. If you have already installed PostgreSQL locally on the same machine on which you are running the model processor, then this requirement is already satisfied. If you are using a remote PostgreSQL server, separate from the model processor machine, run the following commands to install psycopg2 without having to fully install PostgreSQL.

```sudo apt-get install libpq-dev```

```sudo su```

```pip3 install psycopg2```

```exit```

### Step 2. Set up environmental configuration.

```dockerManager.py``` requires connecting to the PostgreSQL database, and so needs the connection credentials. Store these credentials in a configparser file located at ```./vulusr_model_processor/python/.sspf_config```. Do the following to set up the config file (a template file should already exist there):

```nano ./vulusr_model_processor/python/.sspf_config```

Enter the following text:

    [postgres]
    host=%hostname%
    dbname=%databasename%
    user=%username%
    password=%password%

Substitute in the connection values you used when you set up the PostgreSQL database earlier. When done, exit nano and save the file.

### Step 3. Set up cron job

```dockerManager.py``` relies on a linux cron job to run the python script every X minutes, typically every 1 minute. Do the following to set up the cron job information.

Edit the root-level cron jobs:

```sudo crontab -e```

Add a line for controlling the ```dockerManager.py``` tool.

```* * * * * python3 /home/ubuntu/dockerManager.py```

Save and exit.

By default, ```dockerManager.py``` should be stored at ```~/dockerManager.py```. If you store it elsewhere, be sure to update the filepath accordingly in the previous step when editing the crontab file.

## Cleaning up stale user data automatically

Similar to the above automated process, this repo includes a tool (```userDataCleanup.py```) that automatically removes old user data from the PostgreSQL server from previous model runs, if the user has been inactive for at least 6 months. If you wish to implement this functionality, follow a similar process to set up a cronjob to regularly run the tool.

Follow steps 1 and 2 from the above process for ```dockerManager.py``` if you have not already.

```userDataCleanup.py``` can run less frequently. As with step 3 above, add a line to the root-level crontab file. e.g. to run userDataCleanup.py once weekly on Saturdays at 2am, add the following line to the root-level crontab file:

```0 2 * * 6 python3 /home/ubuntu/userDataCleanup.py```

## Final notes

If you want to run the UI from your local RStudio environment, [here's a list](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/safer_streets_priority_finder/required_libraries.R) of libraries that you'll need. Open [this project](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/safer_streets_priority_finder/safer_streets_priority_finder.Rproj) within RStudio. Then open and run [this file](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/safer_streets_priority_finder/dev/run_dev.R) and that should launch the UI on your local computer.

If you want customized support implementing this tool in the AWS cloud, or need support with Docker containerization, please get in touch with us. You can send questions or support inquiries to the developers by emailing, saferstreetspriorityfinder@tooledesign.com.