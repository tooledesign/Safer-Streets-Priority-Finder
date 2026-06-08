#!/usr/bin/env bash
set -euo pipefail

# inputs
MYPATH="$(cd "$(dirname "$0")" && pwd)"

# Load env file from multiple possible locations (priority order)
ENV_LOADED=false
for ENV_FILE in "/etc/sspf/sspf.env" "$MYPATH/.env"; do
  if [[ -f "$ENV_FILE" ]]; then
    echo "✓ Loading environment from: $ENV_FILE"
    set -a
    source "$ENV_FILE"
    set +a
    ENV_LOADED=true
    break
  fi
done

if [[ "$ENV_LOADED" == "false" ]]; then
  echo "⚠️  No environment file found. Using system environment variables."
fi

# activate venv (if it isn't present, run set_up_env.sh first)
source "$MYPATH/sspf_venv/bin/activate"
cd "$MYPATH"

# port and host the app should listen on (can be overridden by env file)
export PORT=${PORT:-8050}
export HOST=${HOST:-0.0.0.0}

# check if port is available, if not ask user if it is ok to kill the existing process
if lsof -i :$PORT -t >/dev/null 2>&1; then
  read -p "Port $PORT is already in use. Do you want to kill the existing process? (y/n) " choice
  if [[ "$choice" =~ ^[Yy]$ ]]; then
    echo "Killing existing process on port $PORT..."
    # -t returns only PIDs
    lsof -ti :$PORT | xargs kill -9
  else
    echo "Exiting..."
    exit 1
  fi
fi

python3 -m src.app