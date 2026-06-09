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


# US County
echo "Loading US counties data..."
# Download from https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_county_500k.zip
ogr2ogr -f "PostgreSQL" PG:"host=$DB_HOST port=$DB_PORT dbname=$DB_NAME user=$DB_USER password=$DB_PASSWORD" \
    "/vsizip/vsicurl/https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_county_500k.zip" \
   -nln static.us_counties \
   -lco GEOMETRY_NAME=geom -lco precision=NO -overwrite -progress --config PG_USE_COPY YES -nlt PROMOTE_TO_MULTI -t_srs EPSG:4326

PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -U "$DB_USER" -c "CREATE INDEX ON static.us_counties USING BTREE(state_name);"
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -U "$DB_USER" -c "CREATE INDEX ON static.us_counties USING BTREE(namelsad);"


# US States
echo "Loading US states data..."
# Download from https://www2.census.gov/geo/tiger/TIGER2025/STATE/tl_2025_us_state.zip
ogr2ogr -f "PostgreSQL" PG:"host=$DB_HOST port=$DB_PORT dbname=$DB_NAME user=$DB_USER password=$DB_PASSWORD" \
    "/vsizip/vsicurl/https://www2.census.gov/geo/tiger/TIGER2025/STATE/tl_2025_us_state.zip" \
   -nln static.us_states \
   -lco GEOMETRY_NAME=geom -lco precision=NO -overwrite -progress --config PG_USE_COPY YES -nlt PROMOTE_TO_MULTI -t_srs EPSG:4326

# Create indexes and Primary Key
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -U "$DB_USER" -c "CREATE INDEX ON static.us_states USING BTREE(name);"
# Drop default PK created by ogr2ogr (usually on ogc_fid) if it exists
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -U "$DB_USER" -c "ALTER TABLE static.us_states DROP CONSTRAINT IF EXISTS us_states_pkey;"
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -U "$DB_USER" -c "ALTER TABLE static.us_states ADD PRIMARY KEY (stusps);"

# vacuum analyze to optimize for new data and indexes
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -U "$DB_USER" -c "VACUUM (FULL, ANALYZE) static.us_counties;"
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -U "$DB_USER" -c "VACUUM (FULL, ANALYZE) static.us_states;"
