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

# FARS Crashes (update path if it changes)
ogr2ogr -f "PostgreSQL" PG:"host=$DB_HOST port=$DB_PORT dbname=$DB_NAME user=$DB_USER password=$DB_PASSWORD" \
    "$MYPATH/../staging/fars_crashes.gpkg" \
   -nln static.fars_crashes \
   -lco GEOMETRY_NAME=geom -lco precision=NO -lco OVERWRITE=YES -progress --config PG_USE_COPY YES -t_srs EPSG:4326

# drop rows for crashes older than 5 years from the latest crash year in the data (if any slipped through)
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -U "$DB_USER" -c "
    WITH max_year AS (
        SELECT MAX(year) AS year FROM static.fars_crashes
    )
    DELETE FROM static.fars_crashes
    WHERE year < (SELECT year - 4 FROM max_year);
"

# delete existing pkey and add new surrogate pkey as year_st_case (should be unique for each crash and is a combination of year and case number)
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -U "$DB_USER" -c "
    ALTER TABLE static.fars_crashes
    DROP CONSTRAINT IF EXISTS fars_crashes_pkey;

    ALTER TABLE static.fars_crashes
    ADD COLUMN IF NOT EXISTS year_st_case TEXT;

    UPDATE static.fars_crashes
    SET year_st_case = year::TEXT || '_' || LPAD(st_case::TEXT, 6, '0');

    ALTER TABLE static.fars_crashes
    ADD PRIMARY KEY (year_st_case);
"

# vacuum analyze to optimize for new data and indexes
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -U "$DB_USER" -c "VACUUM (FULL, ANALYZE) static.fars_crashes;"