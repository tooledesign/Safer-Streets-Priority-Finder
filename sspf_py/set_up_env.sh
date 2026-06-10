#!/usr/bin/env bash
set -euo pipefail

# inputs
MYPATH="$(dirname "$(realpath "$0")")"    # this assumes that this bash script is located in the app root. Change if that isn't the case
# echo MYPATH is $MYPATH

rm -rf "$MYPATH/sspf_venv"
python3 -m venv "$MYPATH/sspf_venv"

source "$MYPATH/sspf_venv/bin/activate"

# Installs from the full frozen snapshot. To update deps, edit requirements-direct.txt
# then run: pip install -r requirements-direct.txt && pip freeze > requirements.txt
pip install -r "$MYPATH/requirements.txt"

# Install a headless Chrome binary for Kaleido/Plotly image export
if command -v plotly_get_chrome >/dev/null 2>&1; then
	yes | plotly_get_chrome >/dev/null || true
else
	echo "⚠️ plotly_get_chrome was not found in the virtual environment; ensure kaleido is installed.\
    Install the necessary system dependencies for headless Chrome by running the script 00_prep/01_ubuntu_utilities.sh"
fi

# register venv for quarto and install tinytex
python -m ipykernel install --user --name=sspf_venv --display-name "sspf_venv"
quarto install tinytex

# Create local .env from template if it doesn't exist
if [[ ! -f "$MYPATH/.env" ]] && [[ -f "$MYPATH/.env.example" ]]; then
    echo "Creating .env from .env.example template..."
    cp "$MYPATH/.env.example" "$MYPATH/.env"
    echo "⚠️  Please edit .env with your actual credentials before running the app."
fi