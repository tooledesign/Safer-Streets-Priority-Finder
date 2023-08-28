import pandas as pd
import os
import psycopg2
from psycopg2 import sql
from dotenv import load_dotenv
from zipfile import ZipFile
from sqlalchemy import create_engine
from io import StringIO

# get environment variables
load_dotenv("rds_conn_vars.env")

# db connection
TDG_HOST = os.environ["TDG_HOST"]
TDG_DB_NAME = os.environ["TDG_DB_NAME"]
TDG_USER_NAME = os.environ["TDG_USER_NAME"]
TDG_PASSWORD = os.environ["TDG_PASSWORD"]

connection_string = f"host={TDG_HOST} dbname={TDG_DB_NAME} user={TDG_USER_NAME} password={TDG_PASSWORD}"
engine = create_engine(f'postgresql://{TDG_USER_NAME}:{TDG_PASSWORD}@{TDG_HOST}:5432/{TDG_DB_NAME}')

conn = psycopg2.connect(connection_string)

# place to save data locally
data_folder = "/mnt/c/Users/tputta/OneDrive - Toole Design/Desktop/SSPF/FARS"

for (start_yr, end_yr) in [(2015, 2019), (2017, 2021)]:
    # store csv as a list of dataframes
    for table_type in ["accident", "person"]:
    # for table_type in ["person"]:
        df_list = []
        for yr in range(start_yr, end_yr+1):
            print(f"Reading {yr} {table_type} data as a dataframe")
            with ZipFile(f"{data_folder}/{yr}/FARS{yr}NationalCSV.zip") as zip_file:
                # print(zip_file.namelist())
                for file_name in list(zip_file.namelist()):
                    if f"{table_type}.csv" in file_name.lower():
                        print(f"    {file_name}")
                        df = pd.read_csv(zip_file.open(file_name), low_memory=False, encoding_errors="ignore")
                        if table_type == "person":
                            df['year'] = yr
                        df_list.append(df)

        # merge all the csvs into one dataframe
        print("Merging all dfs into one")
        df = pd.concat(df_list, ignore_index=True)
        del df_list

        # col names to lower case
        df.columns = map(str.lower, df.columns)

        print("Writing the dataframe to postgres table")
        # df.to_sql(name=f"fars_{table_type}_{start_yr}_{end_yr}", schema="received", con=engine, if_exists="replace", index=False, chunksize=10000, method="multi")

        # using sql copy function instead of pandas to_sql as it is much faster
        df.head(0).to_sql(name=f"fars_{table_type}_{start_yr}_{end_yr}", schema="received", con=engine, if_exists="replace")
        query = sql.SQL(f"copy received.fars_{table_type}_{start_yr}_{end_yr} from stdin with csv delimiter as ','")
        buffer = StringIO()
        df.to_csv(buffer, header=False)
        buffer.seek(0)
        cur = conn.cursor()
        
        cur.copy_expert(sql=query, file=buffer)
        conn.commit()

conn.close()