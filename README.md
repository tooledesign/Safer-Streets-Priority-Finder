# Safer-Streets-Priority-Finder

The Safer Streets Priority Finder enables you to analyze the risk to bicyclists and pedestrians on your community’s roads. You can use your local road, crash, and study area data or select from nationally available datasets to:

1. Explore descriptive statistics related to your crash data
2. Develop a Sliding Windows Analysis using historical crash data to inform a High Injury Network
3. Develop a Safer Streets Model to estimate risk along your road network, even in areas that haven't had any reported crashes recently

## Getting Started 

You can launch a local instance of the tool as long as you have a Docker Hub account and access to a Linux command line. If you don't have a Docker Hub account you can visit [here](https://hub.docker.com/) to get one. One  tool with the following, 

#### Step 1: One a linux command line and pull the image 

Log in to docker 

```sudo docker login```

Now, pull the two docker files for this tool

```sudo docker pull tooledesign/vulusr_beta```

```sudo docker pull tooledesign/vulusr_beta_processor_v1```
#### Step 2: Namespace the containers 

It's best practice to namespace containers you've downloaded locally. 

```docker tag {YOUR USER NAME}/minifying minifying```

```docker tag {YOUR USER NAME}/vulusr_beta vulusr_beta```

### Step 3: launch the containers 