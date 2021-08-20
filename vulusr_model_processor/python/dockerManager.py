import psycopg2
import subprocess
from configparser import ConfigParser
from time import sleep, time

#This script checks to see if the docker container is fresh or not, and restarts it if not fresh.

config = ConfigParser()
config.read('.sspf_config')

conn = psycopg2.connect(host=config.get('postgres','host'), dbname=config.get('postres','dbname'), user=config.get('postgres','user'), password=config.get('postgres','password'))

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
        runCommand('sudo docker stop sspf_model_processor')
        #prune docker
        runCommand('sudo docker container prune -f')
        #get current timestamp
        ts = time()
        #initialize new container with a mounted volume to save any logs/data in
        runCommand('sudo docker run -d -v /var/run/docker.sock:/var/run/docker.sock -v sspf_model_processor_{}:/logs --net sp-example-net --name=sspf_model_processor --restart=always -p 9001:3839 sspf_model_processor'.format(ts))
        #file the logs according to current timestamp -- use the volume location created by docker
        runCommand('sudo docker logs -f sspf_model_processor &> /var/lib/docker/volumes/sspf_model_processor_{0}/sspf_model_processor_{0}.log &'.format(ts))
        sleep(5)
        setDockerStatus()

if __name__ == "__main__":
    process()