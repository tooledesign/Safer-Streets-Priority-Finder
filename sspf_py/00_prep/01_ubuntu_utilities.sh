#!/bin/bash
set -euo pipefail

# Need to be run on the Ubuntu machine which will host the app
# all the othert subsequent data load scripts can be run from any computer that has access to the app DB and input sources

# install ubuntu utilities
sudo apt update
sudo apt install -y \
    postgresql-client \
    build-essential \
    python3-venv \
    libspatialindex-dev \
    gdal-bin \
    unzip \
    osm2pgsql \
    osmium-tool \
    jq

# Function to install system dependencies for headless Chrome
# This is needed for pdf processor to create images using Plotly/Kaleido
install_chrome_dependencies() {
    echo "Installing system dependencies required for headless Chrome (sudo access may be requested)..."
    sudo apt-get update
    sudo apt-get install -y \
        libnss3 \
        libatk-bridge2.0-0t64 \
        libcups2t64 \
        libxcomposite1 \
        libxdamage1 \
        libxfixes3 \
        libxrandr2 \
        libgbm1 \
        libxkbcommon0 \
        libpango-1.0-0 \
        libcairo2 \
        libasound2t64
}

install_chrome_dependencies