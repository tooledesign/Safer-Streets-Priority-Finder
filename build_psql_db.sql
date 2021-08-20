

-- create database 
CREATE DATABASE sspf;

-- add postgis 
CREATE EXTENSION postgis;

-- create schema in sspf database
CREATE SCHEMA gen_management;
CREATE SCHEMA local_user_data;
CREATE SCHEMA model_output_scratch;
CREATE SCHEMA model_outputs;
CREATE SCHEMA scratch;
CREATE SCHEMA sliding_windows_outputs;
CREATE SCHEMA static;

-- crete docker check table 
CREATE TABLE gen_management.docker_status
(
    is_fresh BOOLEAN DEFAULT TRUE
);

-- create domain for model_status 
CREATE DOMAIN gen_management.model_status_options AS TEXT
CHECK (
    VALUE IN (
        'no_model_desired',
        'model_needed',
        'model_currently_running',
        'model_estimation_completed')
);

-- create table
CREATE TABLE gen_management.accounts 
(
tdg_id                    BIGSERIAL PRIMARY KEY,
user_id                   INTEGER NOT NULL,
username                  TEXT NOT NULL,
email                     TEXT NOT NULL,
run_id                    TEXT NOT NULL,
crs                       INTEGER NULL,
crash_o_year_col          TEXT NULL, 
crash_o_serv_col          TEXT NULL, 
crash_o_rep_id_col        TEXT NULL,
crash_o_mode_col          TEXT NULL,
roads_o_fun_c_col         TEXT NULL, 
road_o_name               TEXT NULL,
road_o_source             TEXT NULL,
road_o_id                 TEXT NULL,
roads_fun_c_col           TEXT NULL,
crash_o_source            TEXT NULL,
sa_o_source               TEXT NULL,
move_windows_long_comp    BOOLEAN DEFAULT FALSE,	
model_comp                BOOLEAN DEFAULT FALSE,
sa_bbox_north_4326        FLOAT,
sa_bbox_south_4326        FLOAT,
sa_bbox_east_4326         FLOAT,
sa_bbox_west_4326         FLOAT,
time_since_model_desired  TIMESTAMP DEFAULT NULL,
time_mode_finished        TIMESTAMP DEFAULT NULL,
model_process_time        INTERVAL DEFAULT NULL,             
model_status              model_status_options DEFAULT 'no_model_desired',
sa_storage_opt_out        BOOLEAN DEFAULT FALSE,
roads_storage_opt_out     BOOLEAN DEFAULT FALSE,
crash_storage_opt_out     BOOLEAN DEFAULT FALSE,
sa_data_last_added        TIMESTAMP DEFAULT NULL,
roads_data_last_added     TIMESTAMP DEFAULT NULL,
crash_data_last_added     TIMESTAMP DEFAULT NULL,
last_login TIMESTAMP      DEFAULT NOW(),
account_created           TIMESTAMP DEFAULT NOW(),
total_logins              INTEGER DEFAULT 1,
password                  TEXT NULL,
o_username                TEXT NULL,
o_run_id                  TEXT NULL,
date_crash_deleted        TIMESTAMP DEFAULT NULL,
date_roads_deleted        TIMESTAMP DEFAULT NULL, 
date_sa_deleted           TIMESTAMP DEFAULT NULL,
discount_rate             DOUBLE PRECISION DEFAULT 0,
o_k_cost                  DOUBLE PRECISION DEFAULT 0,
o_a_cost                  DOUBLE PRECISION DEFAULT 0,
o_b_cost                  DOUBLE PRECISION DEFAULT 0,
o_c_cost                  DOUBLE PRECISION DEFAULT 0,
o_o_cost                  DOUBLE PRECISION DEFAULT 0,
report_needed             BOOLEAN DEFAULT FALSE,
report_requested_time     TIMESTAMP DEFAULT NULL,
report_finished_time      TIMESTAMP DEFAULT NULL,
);

