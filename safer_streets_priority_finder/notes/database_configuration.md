

## Relational Database (RDS) Configuration

This document was last updated on 4/26/2021, so anything that says 'current' is current as of that date. Also, this document summarizes the beta phase database for the project. 

### usdot-vulusr-t3small
This is the name of the PostgreSQL RDS for the USDOT Vulnerable User Network Risk Analysis Tool (refered to here as, the Tool). This is a cloud based RDS hosted on AWS infrastructure. It's located in us-east-1f (same location as the application and model processor) and its size is: db.t3.small (1 core, 2 vCPU, 2Gb memory). More on AWS RDS instances [here](https://aws.amazon.com/rds/instance-types/).  

*Endpoint*: usdot-vulusr-t3small.crrjyzlytxji.us-east-1.rds.amazonaws.com

### Access
There is only one user account at the moment and that's, *postgres*. This username has all privileges needed for the Tool to operate. The password is *usdot_vulusr2021*. 

### Databases 
a0137 is the name of the database within the RDS instance where all data for the Tool are stored. 

### Schemas
There are currently 7 schemas. These schemas are mainly to house data for the Tool. 
1. *local_user_data* - holds user's crash, roads, and study area data linked to east study with user_id and run_id. 
2. *gen_management* - holds general management tables, such as the table of user accounts. 
3. *model_output* - outputs from model processor that should be kept 
4. *model_output_scratch* - outputs from model processor that aren't needed
5. *static* - Holds static tables such as: 
    - OpenStreetMap tables. These are mainly used by the user if they don't have adequate local roads network. You can read more about OSM data [here](https://en.wikipedia.org/wiki/OpenStreetMap).
    - Fatality Analysis Reporting System data. More information on FARS data can be found  [here](https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars). 
    - counties 
    - PBM tracts 
6. *public* - automatically created by PostgreSQL. Holds objects created without a specified schema. Generally, the Tool and model processor should not create anything in this schema. 
7. *sliding_windows_ouputs* - outputs from the crash density tool that should be kept 
8. *scratch* - outputs from the crash density tool that aren't needed as other scratch tables 

### Extensions 
Current extensions are: 
1. postgis

