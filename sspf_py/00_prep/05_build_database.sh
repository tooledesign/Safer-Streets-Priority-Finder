#!/bin/bash
set -euo pipefail

# get absolute path to this script's directory
MYPATH="$(cd "$(dirname "$0")" && pwd)"

# update this with the correct env file path if it changes
ENV_LOADED=false
for ENV_FILE in "/etc/sspf/sspf.env" "$MYPATH/../.env"; do
    if [[ -f "$ENV_FILE" ]]; then
        echo "✓ Loading environment from: $ENV_FILE"
        set -a
        source "$ENV_FILE"
        set +a
        ENV_LOADED=true
        break
    fi
done

# if no env file was loaded, exit with an error message since we need the env variables to build the database
if [[ "$ENV_LOADED" == "false" ]]; then
    echo "⚠️  No environment file found. Ensure environment variables are set."
    exit 1
fi

######################################################################################################
# comment/uncomment this line if database needs to be dropped and created fresh
# psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d postgres -c "DROP DATABASE IF EXISTS $DB_NAME;"
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d postgres -c "CREATE DATABASE $DB_NAME WITH TEMPLATE template0;"
######################################################################################################

######################################################################################################
# add extensions
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
    CREATE EXTENSION postgis;
    CREATE EXTENSION pgcrypto;
"
######################################################################################################

######################################################################################################
# add schemas
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
    CREATE SCHEMA gen_management;
    CREATE SCHEMA static;
    CREATE SCHEMA received;
    CREATE SCHEMA inputs;
    CREATE SCHEMA scratch;
    CREATE SCHEMA outputs;
    CREATE SCHEMA tmp_staging;
    CREATE SCHEMA debug;
"
######################################################################################################

######################################################################################################
# add tables
# accounts (one row per account)
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
    CREATE TABLE gen_management.accounts (
        user_id                 BIGSERIAL PRIMARY KEY,
        username                TEXT NOT NULL UNIQUE,
        email                   TEXT NOT NULL,
        account_created         TIMESTAMP DEFAULT NOW(),
        last_login              TIMESTAMP DEFAULT NOW(),
        total_logins            INTEGER DEFAULT 1,
        password_hash           TEXT NOT NULL
    );

    CREATE INDEX ON gen_management.accounts(username);
    ANALYZE gen_management.accounts;
"

# studies (one row per study, foreign key to accounts table)
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
    CREATE TABLE gen_management.studies (
        study_id                BIGSERIAL PRIMARY KEY,
        user_id                 BIGINT NOT NULL REFERENCES gen_management.accounts(user_id),
        username                TEXT NOT NULL REFERENCES gen_management.accounts(username),
        study_name              TEXT NOT NULL,
        study_created           TIMESTAMP DEFAULT NOW(),
        crs                     INTEGER NULL,
        -- study_area related
        study_area_data_source  TEXT NULL,
        study_area_bbox_4326    JSONB,
        study_centroid_4326     GEOMETRY(POINT, 4326) NULL,
        study_state_lookup      TEXT,
        study_area_updated      TIMESTAMP,
        -- roadway related
        roads_data_source       TEXT NULL,
        roads_id_col            TEXT NULL,
        roads_fclass_col        TEXT NULL,
        roads_name_col          TEXT NULL,
        roads_fclass_map        JSONB,
        roads_updated           TIMESTAMP,
        -- crash related
        crash_data_source       TEXT NULL,
        crash_id_col            TEXT NULL,
        crash_year_col          TEXT NULL,
        crash_mode_col          TEXT NULL,
        crash_sev_col           TEXT NULL,
        crash_mode_map          JSONB,
        crash_sev_map           JSONB,
        crash_missing_non_fatal BOOLEAN DEFAULT FALSE,
        crash_updated           TIMESTAMP,
        -- analysis params
        sliding_window_length   FLOAT,
        window_length_ratio     INTEGER,
        short_window_length     FLOAT,
        crash_weights           JSONB,
        crash_costs             JSONB,
        top_streets_threshold   INTEGER,
        crash_join_distance     FLOAT,
        analysis_params_updated TIMESTAMP,
        bayesian_decay          FLOAT,
        bayesian_max_prior_weight FLOAT,
        -- analysis status tracking
        sliding_windows_analysis_status     TEXT DEFAULT 'Not Requested',
        sliding_windows_analysis_updated    TIMESTAMP,
        modeling_status                     TEXT DEFAULT 'Not Requested',
        modeling_updated                    TIMESTAMP
    );

    -- add a unique constraint on (username, study_name)
    ALTER TABLE gen_management.studies ADD CONSTRAINT unique_user_study UNIQUE (username, study_name);

    CREATE INDEX ON gen_management.studies(user_id);
    CREATE INDEX ON gen_management.studies(username);
    CREATE INDEX ON gen_management.studies(study_name);
    CREATE INDEX ON gen_management.studies USING GIST(study_centroid_4326);
    CREATE INDEX ON gen_management.studies(study_state_lookup);
    ANALYZE gen_management.studies;
"
######################################################################################################