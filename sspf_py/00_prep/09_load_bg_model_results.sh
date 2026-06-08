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


# Block Group Fatals Model Results (update path if it changes)
echo "Loading block group fatals model results..."
ogr2ogr -f "PostgreSQL" PG:"host=$DB_HOST port=$DB_PORT dbname=$DB_NAME user=$DB_USER password=$DB_PASSWORD" \
    "$MYPATH/../staging/block_group_fatals_model_results.gpkg" \
   -nln static.block_group_fatals \
   -lco GEOMETRY_NAME=geom -lco precision=NO -overwrite -progress --config PG_USE_COPY YES -nlt PROMOTE_TO_MULTI -t_srs EPSG:4326

# add area_sq_m column
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
    ALTER TABLE static.block_group_fatals
    ADD COLUMN IF NOT EXISTS area_sq_m DOUBLE PRECISION;

    UPDATE static.block_group_fatals
    SET area_sq_m = ST_Area(geom::geography);
"

# vacuum analyze to optimize for new data and indexes
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "VACUUM (FULL, ANALYZE) static.block_group_fatals;"