import pandas as pd
import os
import psycopg2
from dotenv import load_dotenv

# get environment variables
load_dotenv("rds_conn_vars.env")

# db connection
TDG_HOST = os.environ["TDG_HOST"]
TDG_DB_NAME = os.environ["TDG_DB_NAME"]
TDG_USER_NAME = os.environ["TDG_USER_NAME"]
TDG_PASSWORD = os.environ["TDG_PASSWORD"]

connection_string = f"host={TDG_HOST} dbname={TDG_DB_NAME} user={TDG_USER_NAME} password={TDG_PASSWORD}"
conn = psycopg2.connect(connection_string)
cur = conn.cursor()

for (start_yr, end_yr) in [(2015, 2019), (2017, 2021)]:
    print(f"Processing {start_yr}_{end_yr} data")
    query = f"""
            DROP TABLE IF EXISTS automated.fars_processed_{start_yr}_{end_yr};
            CREATE TABLE automated.fars_processed_{start_yr}_{end_yr} AS (
                SELECT
                    a.st_case,
                    -- just go through person types in order of vulnerability
                    CASE
                        WHEN per_typ && ARRAY[5, 8] THEN 'pedestrian'
                        WHEN per_typ && ARRAY[6, 7] THEN 'bicyclist'
                        WHEN per_typ && ARRAY[1, 2, 3] THEN 'motor vehicle'
                        ELSE 'other'
                    END AS crash_mode,
                    -- since its FARS, hard coding in severity
                    'fatality' AS crash_severity,
                    a.year AS crash_year,
                    LPAD(state::TEXT, 2, '0') AS state_fp,
                    LPAD(county::TEXT, 3, '0') AS county_fp,
                    CASE
                        WHEN city != 0 THEN LPAD(city::TEXT, 5, '0')
                        ELSE NULL
                    END AS place_fp,
                    a.func_sys AS functional_class,
                    ST_SetSRID(ST_MakePoint(longitud, latitude), 4326) AS geom
                FROM
                    received.fars_accident_{start_yr}_{end_yr} a
                    LEFT JOIN
                    (
                        SELECT
                            st_case,
                            year,
                            array_agg(per_typ::INT) AS per_typ
                        FROM
                            received.fars_person_{start_yr}_{end_yr}
                        GROUP BY
                            st_case,
                            year
                    ) p
                    ON a.st_case = p.st_case AND a.year = p.year
            )
            ;

            ALTER TABLE automated.fars_processed_{start_yr}_{end_yr} ADD pkey SERIAL PRIMARY KEY;
            CREATE INDEX ON automated.fars_processed_{start_yr}_{end_yr} USING GIST(geom);
            ANALYZE automated.fars_processed_{start_yr}_{end_yr};
            """
    cur.execute(query)
    conn.commit()

conn.close()