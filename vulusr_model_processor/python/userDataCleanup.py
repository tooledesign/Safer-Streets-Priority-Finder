import psycopg2
from time import time

#This script checks for 6-month stale user data for specific model runs and deletes it if user opted out of data collection.

conn = psycopg2.connect(host="usdot-vulusr-t3small.crrjyzlytxji.us-east-1.rds.amazonaws.com", dbname="a0137", user="postgres", password="usdot_vulusr2021")

def cleanupStaleRuns(type, threshold=6):
    #checks 3 storage types independently
    query = """SELECT user_id, run_id
                FROM gen_management.accounts a
                WHERE extract(month from age(current_timestamp(0), a.last_login)) > %s
                AND a.{}_storage_opt_out is true;""".format(type)

    with conn.cursor() as cur:
        cur.execute(query, (threshold,))
        runs = cur.fetchall()

    for user_id, run_id in runs:
        deleteStaleRun(user_id, run_id, type)

def deleteStaleRun(user_id, run_id, type):
    with conn.cursor() as cur:
        #cur.execute("DROP TABLE IF EXISTS local_user_data.{}_{}_{};".format('crashes' if type == 'crash' else type, user_id, run_id))
        cur.execute("UPDATE gen_management.accounts SET date_{}_deleted = current_timestamp(0) WHERE user_id = %s AND run_id = %s;".format(type), (user_id, run_id))
        conn.commit()

def process(types = ['crash', 'roads', 'sa']):
    for type in types:
        cleanupStaleRuns(type)

if __name__ == "__main__":
    process()