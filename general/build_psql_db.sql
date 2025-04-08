-- DROP DATABASE IF EXISTS sspf;
-- the create database command needs to be run from the postgres database
-- the rest of this needs to be run within the sspf database once created
-- create database
-- CREATE DATABASE sspf;
CREATE EXTENSION plpgsql;

-- add postgis
CREATE EXTENSION postgis;

-- add pgcrypto
CREATE EXTENSION pgcrypto;
------------------------------------------------------------------------------------------------------------------------

-- create schema in sspf database
CREATE SCHEMA gen_management;

CREATE SCHEMA local_user_data;

CREATE SCHEMA model_output_scratch;

CREATE SCHEMA model_outputs;

CREATE SCHEMA scratch;

CREATE SCHEMA sliding_windows_outputs;

CREATE SCHEMA static;

------------------------------------------------------------------------------------------------------------------------

-- create docker check table
CREATE TABLE gen_management.docker_status(
    is_fresh boolean DEFAULT TRUE
);

-- create domain for model_status
CREATE DOMAIN gen_management.model_status_options AS TEXT CHECK (VALUE IN ('no_model_desired', 'model_needed', 'model_currently_running', 'model_estimation_completed'));

-- create domain for report_status
CREATE DOMAIN gen_management.report_status_options AS TEXT CHECK (VALUE IN ('no_report_requested', 'report_requested', 'building_report', 'report_ready'));

-- create table
CREATE TABLE gen_management.accounts(
    tdg_id bigserial PRIMARY KEY,
    user_id integer NOT NULL,
    username text NOT NULL,
    email text NOT NULL,
    run_id text NOT NULL,
    crs integer NULL,
    crash_o_year_col text NULL,
    crash_o_serv_col text NULL,
    crash_o_rep_id_col text NULL,
    crash_o_mode_col text NULL,
    roads_o_fun_c_col text NULL,
    road_o_name text NULL,
    road_o_source text NULL,
    road_o_id text NULL,
    roads_fun_c_col text NULL,
    crash_o_source text NULL,
    sa_o_source text NULL,
    move_windows_long_comp boolean DEFAULT FALSE,
    model_comp boolean DEFAULT FALSE,
    sa_bbox_north_4326 float,
    sa_bbox_south_4326 float,
    sa_bbox_east_4326 float,
    sa_bbox_west_4326 float,
    time_since_model_desired timestamp DEFAULT NULL,
    time_mode_finished timestamp DEFAULT NULL,
    model_process_time interval DEFAULT NULL,
    model_status gen_management.model_status_options DEFAULT 'no_model_desired',
    sa_storage_opt_out boolean DEFAULT FALSE,
    roads_storage_opt_out boolean DEFAULT FALSE,
    crash_storage_opt_out boolean DEFAULT FALSE,
    sa_data_last_added timestamp DEFAULT NULL,
    roads_data_last_added timestamp DEFAULT NULL,
    crash_data_last_added timestamp DEFAULT NULL,
    last_login timestamp DEFAULT NOW(),
    account_created timestamp DEFAULT NOW(),
    total_logins integer DEFAULT 1,
    password TEXT NULL,
    o_username text NULL,
    o_run_id text NULL,
    date_crash_deleted timestamp DEFAULT NULL,
    date_roads_deleted timestamp DEFAULT NULL,
    date_sa_deleted timestamp DEFAULT NULL,
    discount_rate double precision DEFAULT 0,
    o_k_cost double precision DEFAULT 0,
    o_a_cost double precision DEFAULT 0,
    o_b_cost double precision DEFAULT 0,
    o_c_cost double precision DEFAULT 0,
    o_o_cost double precision DEFAULT 0,
    report_status gen_management.report_status_options DEFAULT 'no_report_requested',
    report_requested_time timestamp DEFAULT NULL,
    report_finished_time timestamp DEFAULT NULL
);

CREATE TABLE gen_management.salt(
    username text,
    salt text,
    time_created timestamp DEFAULT NOW()
);



------------------------------------------------------------------------------------------------------------------------
-- These queries show the structure of the tables in static, but aren't used to create the tables.


-- fclass prior structure
-- CREATE TABLE static.national_fclass_priors(
--     ogc_fid integer NOT NULL DEFAULT nextval('static.national_fclass_priors_ogc_fid_seq'::regclass),
--     mode character varying COLLATE pg_catalog."default",
--     severity character varying COLLATE pg_catalog."default",
--     road_fclass character varying COLLATE pg_catalog."default",
--     mileage integer,
--     n_crashes double precision,
--     beta_crashes integer,
--     CONSTRAINT national_fclass_priors_pkey PRIMARY KEY (ogc_fid)
-- );

-- far_processed data structure
-- CREATE TABLE static.fars_processed(
--     pkey integer NOT NULL DEFAULT nextval('static.fars_processed_pkey_seq'::regclass),
--     st_case bigint,
--     crash_mode character varying COLLATE pg_catalog."default",
--     crash_severity character varying COLLATE pg_catalog."default",
--     crash_year bigint,
--     state_fp character varying COLLATE pg_catalog."default",
--     county_fp character varying COLLATE pg_catalog."default",
--     place_fp character varying COLLATE pg_catalog."default",
--     functional_class bigint,
--     geom geometry(Geometry, 4326),
--     CONSTRAINT fars_processed_pkey PRIMARY KEY (pkey)
-- );

-- CREATE INDEX fars_processed_geom_geom_idx ON static.fars_processed USING gist(geom) TABLESPACE pg_default;

-- osm_centerlines data structure
-- CREATE TABLE static.osm_centerlines(
--     way_id bigint NOT NULL DEFAULT nextval('static.osm_centerlines_way_id_seq'::regclass),
--     osm_type character varying COLLATE pg_catalog."default" NOT NULL,
--     name character varying COLLATE pg_catalog."default",
--     ref character varying COLLATE pg_catalog."default",
--     oneway smallint,
--     tags json,
--     state_fp character varying COLLATE pg_catalog."default",
--     county_fp character varying COLLATE pg_catalog."default",
--     state_county_fp_array character varying[] COLLATE pg_catalog."default",
--     geom geometry(MultiLineString, 4326),
--     CONSTRAINT osm_centerlines_pkey PRIMARY KEY (way_id)
-- );

-- CREATE INDEX osm_centerlines_geom_geom_idx ON static.osm_centerlines USING gist(geom) TABLESPACE pg_default;

--  static.us_county_2018 data structure
-- CREATE TABLE static.us_county_2018(
--     id integer NOT NULL DEFAULT nextval('static.us_county_2018_id_seq'::regclass),
--     ogc_fid integer,
--     gisjoin character varying COLLATE pg_catalog."default",
--     statefp character varying COLLATE pg_catalog."default",
--     countyfp character varying COLLATE pg_catalog."default",
--     countyns character varying COLLATE pg_catalog."default",
--     geoid character varying COLLATE pg_catalog."default",
--     name character varying COLLATE pg_catalog."default",
--     namelsad character varying COLLATE pg_catalog."default",
--     lsad character varying COLLATE pg_catalog."default",
--     classfp character varying COLLATE pg_catalog."default",
--     mtfcc character varying COLLATE pg_catalog."default",
--     csafp character varying COLLATE pg_catalog."default",
--     cbsafp character varying COLLATE pg_catalog."default",
--     metdivfp character varying COLLATE pg_catalog."default",
--     funcstat character varying COLLATE pg_catalog."default",
--     aland double precision,
--     awater double precision,
--     intptlat character varying COLLATE pg_catalog."default",
--     intptlon character varying COLLATE pg_catalog."default",
--     shape_leng double precision,
--     shape_area double precision,
--     geom geometry(MultiPolygon, 4326),
--     CONSTRAINT us_county_2018_pkey PRIMARY KEY (id)
-- );

-- CREATE INDEX us_county_2018_geom_geom_idx ON static.us_county_2018 USING gist(geom) TABLESPACE pg_default;


