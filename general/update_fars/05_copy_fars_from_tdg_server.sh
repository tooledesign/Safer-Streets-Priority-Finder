#!/bin/bash

# Export env vars
export $(grep -v '^#' rds_conn_vars.env | xargs)

ogr2ogr \
    -lco GEOMETRY_NAME=geom \
    -lco precision=NO  \
    -f "PostgreSQL" \
    PG:"host=$HOST dbname=$DB_NAME user=$USER_NAME password=$PASSWORD" \
    PG:"host=$TDG_HOST dbname=$TDG_DB_NAME user=$TDG_USER_NAME password=$TDG_PASSWORD" "automated.fars_processed_2017_2021" \
    -t_srs EPSG:4326 \
    -nln "scratch._tmp_fars_processed_2017_2021" \
    -overwrite \
    -progress \
    --config PG_USE_COPY YES