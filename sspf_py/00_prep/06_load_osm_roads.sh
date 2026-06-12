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

PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "DROP TABLE IF EXISTS static.osm_centerlines;"

# Import only roadway centerlines into static.osm_centerlines
wget https://download.geofabrik.de/north-america/us-latest.osm.pbf -O /tmp/us-latest.osm.pbf

PGOPTIONS='-c work_mem=64MB -c maintenance_work_mem=1GB -c synchronous_commit=off' \
osm2pgsql \
  --slim \
  --drop \
  --flat-nodes="/tmp/flatnodes-us.bin" \
  --cache=5000 \
  --number-processes=1 \
  -c \
  --database "postgresql://$DB_USER:$DB_PASSWORD@$DB_HOST:5432/$DB_NAME" \
  -O flex \
  --style="$MYPATH/roads.lua" \
  --log-progress=true \
  /tmp/us-latest.osm.pbf 2>&1 | tee -a "osm2pgsql_us.log"

# delete the downloaded file to save space
rm /tmp/us-latest.osm.pbf
# delete the flatnodes file to save space
rm /tmp/flatnodes-us.bin

# add index to highway column and add a pkey
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "
    ALTER TABLE static.osm_centerlines ADD PRIMARY KEY (osm_id);
    CREATE INDEX IF NOT EXISTS idx_osm_centerlines_highway ON static.osm_centerlines USING BTREE(highway);
    ANALYZE static.osm_centerlines;
"

# remove private and no access roads
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "
    DELETE FROM static.osm_centerlines WHERE access IN ('private', 'no');
"
# drop some columns we don't need to save space
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "
    ALTER TABLE static.osm_centerlines
    DROP COLUMN IF EXISTS ref,
    DROP COLUMN IF EXISTS oneway,
    DROP COLUMN IF EXISTS maxspeed,
    DROP COLUMN IF EXISTS surface,
    DROP COLUMN IF EXISTS bridge,
    DROP COLUMN IF EXISTS tunnel,
    DROP COLUMN IF EXISTS lanes
;
"

# create a materialized view for quick highway lookups
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "
    DROP MATERIALIZED VIEW IF EXISTS static.osm_highway_values;
    CREATE MATERIALIZED VIEW static.osm_highway_values AS
      SELECT
          DISTINCT highway 
          FROM static.osm_centerlines
      ORDER BY highway;
    ;
    CREATE INDEX idx_osm_highway_values_highway ON static.osm_highway_values USING BTREE(highway);
"

# vacuum analyze to optimize for new data and indexes
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "VACUUM (FULL, ANALYZE) static.osm_centerlines;"
PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "VACUUM (FULL, ANALYZE) static.osm_highway_values;"