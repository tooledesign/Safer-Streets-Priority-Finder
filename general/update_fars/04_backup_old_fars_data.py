import os
import psycopg2
from dotenv import load_dotenv

# get environment variables
load_dotenv("rds_conn_vars.env")

# db connection
HOST = os.environ["HOST"]
DB_NAME = os.environ["DB_NAME"]
USER_NAME = os.environ["USER_NAME"]
PASSWORD = os.environ["PASSWORD"]

connection_string = f"host={HOST} dbname={DB_NAME} user={USER_NAME} password={PASSWORD}"
conn = psycopg2.connect(connection_string)
cur = conn.cursor()

query = f"""
        CREATE TABLE static.fars_processed_2015_2019_backup (LIKE static.fars_processed INCLUDING ALL);

        INSERT INTO static.fars_processed_2015_2019_backup
            SELECT *
            FROM static.fars_processed
        ;
        """

cur.execute(query)
conn.commit()
conn.close()