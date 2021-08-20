

### Some general notes on setting up a RDB on an AWS EC2 ts.micro instance. 
EC2 security group must include a Postgresql port (i.e. 5432), in addition to the 22 SSH port

### installing PostgreSQL 
After logging into the Ubuntu server through a local SSH tunnel, I followed the tutorial below for installing Postgresql12 on ubuntu 20.04 instance 
https://computingforgeeks.com/install-postgresql-12-on-ubuntu/
  
### setup configuration files   
Updated the configuration file to listen to all queries 
- accessed with sudo nano /etc/postgresql/12/main/postgresql.conf 
- set: listen_addresses = 'localhost'  to listen_addresses = '*' 
- this line was commented out 

Updated pg_hba.conf file to interact with local computer 
- accessed with: sudo nano /etc/postgresql/12/main/pg_hba.conf
- added the following: host    all             all            LOCAL IP        trust
- LOCAL IP was my IP address with a /32 suffix 
- note that this will need to be updated once the Shiny app live
- the following is a great place to better understand the pg_hba.conf domain: https://www.postgresql.org/docs/10/auth-pg-hba-conf.html

After updating the postgresql.conf and postgresql.conf, I had to restart the RDBMS
- with: sudo systemctl restart postgresql

### Granting access to postgresql user
After that I setup the the project user to interact with various privileges 
GRANT CONNECT ON DATABASE usdot_vul_usr TO gis;
GRANT ALL PRIVILEGES ON DATABASE usdot_vul_usr TO gis;
GRANT ALL ON SCHEMA automated TO gis;
GRANT ALL ON SCHEMA generated TO gis;
GRANT ALL ON SCHEMA scratch TO gis;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA automated TO gis;
 
The allowed me to make calls from my local RStudio

### test access to EC2 setup 
drv <- RPostgreSQL::PostgreSQL()
con <- DBI::dbConnect(drv, 
                      dbname   = "usdot_vul_usr", 
                      host     = "ec2-54-90-60-16.compute-1.amazonaws.com", 
                      port     = 5432, 
                      user     = "gis", 
                      password = "gis"
)

DBI::dbGetQuery(con, 'CREATE TABLE automated.first (id INT);')  
DBI::dbGetQuery(con, 'INSERT INTO automated.first (id) VALUES (1);')  
test <- DBI::dbGetQuery(con, 'SELECT * FROM automated.first;') 
print(test)

### Couple of other notes 

##### logging postgresql console
- at one point I misspelled an input in the pg_hba.conf file, which I was able to uncover by tapping in to the log file 
- with: sudo nano /var/log/postgresql/postgresql-12-main.log
- this pointed me towards the problem, which I was able to find and fix 

##### restarting the Ubuntu server 
- another pointer, the Ubuntu system can be restarted 
- with: sudo reboot

##### Text editor on the CL 
- vim isn't great, nano is more intuitive 

##### check postgresql status 
- use command: service postgresql status

##### setup postgresql db to kick start on Ubuntu system bootup 
- use: sudo systemctl enable postgresql

### Installing PostGIS, ensure install postgis for postgresql 12 (version installed) 
- sudo apt install postgis postgresql-12-postgis-3
- sudo apt-get install postgresql-12-postgis-3-scripts
Then log into postgresql databse and create extension 
- CREATE EXTENSION postgis;