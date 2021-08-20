import psycopg2
import subprocess
from time import sleep, time

#This script checks to see if the docker container is fresh or not, and restarts it if not fresh.

conn = psycopg2.connect(host="usdot-vulusr-t3small.crrjyzlytxji.us-east-1.rds.amazonaws.com", dbname="a0137", user="postgres", password="usdot_vulusr2021")

def checkDockerStatus():
    with conn.cursor() as cur:
        cur.execute("SELECT is_fresh FROM gen_management.docker_status;")
        result = cur.fetchone()[0]
    return result

def setDockerStatus():
    with conn.cursor() as cur:
        cur.execute("UPDATE gen_management.docker_status SET is_fresh = TRUE;")
    conn.commit()

def runCommand(command):
    p = subprocess.Popen(command, shell=True)
    p.communicate()

def process():
    isFresh = checkDockerStatus()
    if isFresh == False:
        #stop docker container
        runCommand('sudo docker stop beta_processor')
        #prune docker
        runCommand('sudo docker container prune -f')
        #get current timestamp
        ts = time()
        #initialize new container with a mounted volume to save any logs/data in
        runCommand('sudo docker run -d -v /var/run/docker.sock:/var/run/docker.sock -v beta_processor_{}:/logs --net sp-example-net --name=beta_processor --restart=always -p 81:3839 vulusr_beta_processor_v1'.format(ts))
        #file the logs according to current timestamp -- use the volume location created by docker
        runCommand('sudo docker logs -f beta_processor &> /var/lib/docker/volumes/beta_processor_{0}/beta_processor_{0}.log &'.format(ts))
        sleep(5)
        setDockerStatus()

if __name__ == "__main__":
    process()