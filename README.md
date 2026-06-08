![MIT](https://img.shields.io/github/license/tooledesign/safer-streets-priority-finder)
![Last Commit](https://img.shields.io/github/last-commit/tooledesign/safer-streets-priority-finder)

# Safer-Streets-Priority-Finder

The Safer Streets Priority Finder (SSPF) enables you to analyze the risk to bicyclists and pedestrians on your community’s roads. You can use your local road, crash, and study area data or select from nationally available datasets to:

1. Explore descriptive statistics related to your crash data
2. Develop a Sliding Windows Analysis using historical crash data to inform a High Injury Network
3. Develop a Safer Streets Model to estimate risk along your road network, even in areas that haven't had any reported crashes recently

[Check out the Safer Streets Priority Finder v2.0 here](https://sspf.tooledesign.com/). The version 1.0 URL (https://www.saferstreetspriorityfinder.com/) also redirects to version 2.0


## How to launch your own instance of the SSPF and what you'll need

The active SSPF application is now fully contained in `sspf_py` and runs directly on Linux with Python and PostgreSQL. Docker is no longer required for setup or launch. The below directions are not needed to use the tool that is available on https://sspf.tooledesign.com/, it is only if you wish to launch a local instance of the tool elsewhere.

## Prerequisites

1. Linux machine with sudo access. The scripts were tested and run on Ubuntu 24.04.
2. PostgreSQL server with PostGIS enabled (local or remote). Postgres 17 is the production version being used.
3. Python - the application was developed and deployed using python 3.12 version.
4. `quarto` installed and available on PATH (used for report generation)

## Step 1. Clone the repository

```bash
git clone https://github.com/tooledesign/Safer-Streets-Priority-Finder.git
cd Safer-Streets-Priority-Finder/sspf_py
```

## Step 2. Install system dependencies

Run the Ubuntu utility script. This installs tools used by SSPF and the data-loading scripts (including `psql`, `ogr2ogr`, `osm2pgsql`, and headless Chrome dependencies for Plotly/Kaleido image export).

```bash
bash 00_prep/01_ubuntu_utilities.sh
```

## Step 3. Configure environment variables

If you are using a .env file that is in another location, refactor scripts to point to the correct location. Copy the template and update it with your database credentials and runtime settings:

```bash
cp .env.example .env
nano .env
```

Expected variables:

- `DB_HOST`
- `DB_PORT`
- `DB_NAME`
- `DB_USER`
- `DB_PASSWORD`
- `FLASK_SECRET_KEY`
- `DASH_DEBUG` (`0` or `1`)
- `AUTH_ENABLED` (`0` or `1`)
- `PORT` (default `8050`)
- `HOST` (default `127.0.0.1`)

The default launch and prep scripts load environment variables from `/etc/sspf/sspf.env` first if available, then `.env` in `sspf_py`.

## Step 4. Create Python environment and install packages

```bash
bash set_up_env.sh
```

This script:

1. Creates (or repalces existing) `sspf_venv` virtual environment.
2. Installs Python dependencies from `requirements.txt`
3. Installs a Chrome binary for Plotly/Kaleido
4. Registers the kernel and installs TinyTeX for Quarto-based reporting

## Step 5. Build and load the SSPF database

From `sspf_py`, run the prep scripts in order:

```bash
bash 00_prep/05_build_database.sh
bash 00_prep/06_load_osm_roads.sh
bash 00_prep/07_load_fars.sh
bash 00_prep/08_load_counties_and_state.sh
bash 00_prep/09_load_bg_model_results.sh
bash 00_prep/10_load_fatal_rate_and_severity_ratio.sh
```

Required local staging files (available via download links on the Resources page of the website) before running the loaders:

- `staging/fars_crashes.gpkg`
- `staging/block_group_fatals_model_results.gpkg`

Notes:

- `06_load_osm_roads.sh` downloads OSM data directly from GeoFabrik extracts.
- `08_load_counties_and_state.sh` downloads Census county/state boundaries directly.

## Step 6. Launch the application

```bash
bash launch_app.sh
```

Then open:

- `http://127.0.0.1:8050` (default)

If you changed `HOST`/`PORT` in your environment file, use those values instead.

## Support

For implementation support, contact: saferstreetspriorityfinder@tooledesign.com
