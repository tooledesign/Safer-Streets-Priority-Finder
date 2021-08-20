These scripts requires psycopg2. Get around the requirement for installing postgreSQL by doing:

`sudo apt-get install libpq-dev`

Then install psycopg2 as root:

`sudo su`

`pip3 install psycopg2`

Add/edit current root-level cronjobs by doing:

`sudo crontab -e`

## dockerManager.py
This script is to live in ~/ on the AWS EC2 instance that runs the model processor. It checks if the Docker container that runs the model processor is stale and idle, and shuts it down and starts a new container if so.

Copy it to the instance using this command:

`scp -i /path/to/identity/pem_file.pem /path/to/identity/dockerManager.py ubuntu@ec2-instance-address.amazonaws.com:/home/ubuntu/`

From there, it is controlled by a root-level cronjob that runs every 1 minute, with the cronjob instruction line:

`* * * * * python3 /home/ubuntu/dockerManager.py`

If the specific Docker image being used for creation of the container changes, the reference will need to be updated on line 32 in dockerManager.py

The docker container is created with a mounted storage volume at /var/lib/docker/volumes/beta_processor_{timestamp}; docker logs are copied and followed in this directory, and this directory can also be used to save .Rdata, .Rhistory, or other files from within the docker container, and will persist after the container exits.

## userDataCleanup.py

This script is to live in ~/ on the AWS EC2 instance that runs the model processor. It checks for user data that is at least 6 months old, for model runs in which the user opted out of storing their data, and deletes the associated user data tables.

Copy it to the instance using this command:

`scp -i /path/to/identity/pem_file.pem /path/to/identity/userDataCleanup.py ubuntu@ec2-instance-address.amazonaws.com:/home/ubuntu/`

From there, it is controlled by a root-level cronjob that runs ever 1 week on Saturdays at 2am, with the cronjob instruction line:

`  0 2  *   *   6     python3 /home/ubuntu/userDataCleanup.py`

