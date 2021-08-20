import psycopg2
from configparser import ConfigParser
from time import time

#This script checks for 6-month stale user data for specific model runs and deletes it if user opted out of data collection.

config = ConfigParser()
config.read('.sspf_config')

conn = psycopg2.connect(host=config.get('postgres','host'), dbname=config.get('postres','dbname'), user=config.get('postgres','user'), password=config.get('postgres','password'))

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
        cur.execute("DROP TABLE IF EXISTS local_user_data.{}_{}_{};".format('crashes' if type == 'crash' else type, user_id, run_id))
        cur.execute(""" 
        UPDATE gen_management.accounts 
        SET time_since_model_desired = NULL,
                model_status = NULL,
                model_process_time = NULL,
                time_mode_finished = NULL,
                move_windows_long_comp = NULL,
                model_comp = NULL,     
                crash_o_year_col = NULL,
                crash_o_serv_col = NULL,
                crash_o_rep_id_col = NULL,
                crash_o_source = NULL,
                crash_o_mode_col = NULL,
                road_o_name = NULL,
                road_o_source = NULL,
                road_o_id = NULL,
                roads_fun_c_col = NULL,
                o_k_cost = NULL,
                o_a_cost = NULL, 
                o_b_cost = NULL,
                o_c_cost = NULL,
                o_o_cost = NULL,
                discount_rate = NULL,
                sa_bbox_north_4326 = NULL,
                sa_bbox_south_4326 = NULL,
                sa_bbox_east_4326 = NULL, 
                sa_bbox_west_4326 = NULL,
                sa_o_source = NULL,
                roads_o_fun_c_col = NULL 
        WHERE   user_id = %s AND run_id = %s;
        """)
        cur.execute("UPDATE gen_management.accounts SET date_{}_deleted = current_timestamp(0) WHERE user_id = %s AND run_id = %s;".format(type), (user_id, run_id))
        conn.commit()

def process(types = ['crash', 'roads', 'sa']):
    for type in types:
        cleanupStaleRuns(type)

if __name__ == "__main__":
    process()
    
