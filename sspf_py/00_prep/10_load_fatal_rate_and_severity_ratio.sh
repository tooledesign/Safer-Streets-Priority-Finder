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

# National Fatal Rates and Severity Ratio by State
echo "Copying static.national_fatal_rates..."
ogr2ogr -f "PostgreSQL" PG:"host=$DB_HOST port=$DB_PORT dbname=$DB_NAME user=$DB_USER password=$DB_PASSWORD" \
   "$MYPATH/default_fatal_rates_by_state.csv" \
   -nln static.national_fatal_rates_by_state \
   -nlt NONE \
   -lco PRECISION=NO \
   -overwrite -progress --config PG_USE_COPY YES

echo "Copying static.severity_ratio_by_state..."
ogr2ogr -f "PostgreSQL" PG:"host=$DB_HOST port=$DB_PORT dbname=$DB_NAME user=$DB_USER password=$DB_PASSWORD" \
    "$MYPATH/state_severity_ratios.csv" \
   -nln static.severity_ratio_by_state \
   -nlt NONE \
   -lco PRECISION=NO \
   -overwrite -progress --config PG_USE_COPY YES

echo "Casting fatal rates columns to NUMERIC..."
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
   ALTER TABLE static.national_fatal_rates_by_state
   ALTER COLUMN ped_fatal_rate TYPE NUMERIC USING ped_fatal_rate::NUMERIC,
   ALTER COLUMN bike_fatal_rate TYPE NUMERIC USING bike_fatal_rate::NUMERIC,
   ALTER COLUMN mv_fatal_rate TYPE NUMERIC USING mv_fatal_rate::NUMERIC;
"

echo "Casting severity ratio columns to NUMERIC..."
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
   ALTER TABLE static.severity_ratio_by_state
   ALTER COLUMN ped_k TYPE NUMERIC USING ped_k::NUMERIC,
   ALTER COLUMN ped_a TYPE NUMERIC USING ped_a::NUMERIC,
   ALTER COLUMN ped_b TYPE NUMERIC USING ped_b::NUMERIC,
   ALTER COLUMN ped_c TYPE NUMERIC USING ped_c::NUMERIC,
   ALTER COLUMN ped_o TYPE NUMERIC USING ped_o::NUMERIC,
   ALTER COLUMN bike_k TYPE NUMERIC USING bike_k::NUMERIC,
   ALTER COLUMN bike_a TYPE NUMERIC USING bike_a::NUMERIC,
   ALTER COLUMN bike_b TYPE NUMERIC USING bike_b::NUMERIC,
   ALTER COLUMN bike_c TYPE NUMERIC USING bike_c::NUMERIC,
   ALTER COLUMN bike_o TYPE NUMERIC USING bike_o::NUMERIC,
   ALTER COLUMN mv_k TYPE NUMERIC USING mv_k::NUMERIC,
   ALTER COLUMN mv_a TYPE NUMERIC USING mv_a::NUMERIC,
   ALTER COLUMN mv_b TYPE NUMERIC USING mv_b::NUMERIC,
   ALTER COLUMN mv_c TYPE NUMERIC USING mv_c::NUMERIC,
   ALTER COLUMN mv_o TYPE NUMERIC USING mv_o::NUMERIC;
"

# vacuum analyze to optimize for new data and indexes
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "VACUUM (FULL, ANALYZE) static.national_fatal_rates_by_state;"
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "VACUUM (FULL, ANALYZE) static.severity_ratio_by_state;"