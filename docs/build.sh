#!/bin/bash

# This script builds the static documentation site
# entirely within the docs folder, keeping Python dependencies isolated

# Check if we need to install python3-venv
NEED_VENV=false
if ! command -v python3 -m venv &> /dev/null; then
  if ! dpkg -l | grep -q python3-venv; then
    NEED_VENV=true
  fi
fi

# Install python3-venv if needed
if [ "$NEED_VENV" = true ]; then
  echo "Python virtual environment package not found. Installing python3-venv..."
  echo "You may be prompted for your password."
  sudo apt-get update && sudo apt-get install -y python3-venv
  if [ $? -ne 0 ]; then
    echo "Failed to install python3-venv. Please install it manually."
    exit 1
  fi
fi

# Clean up any failed previous attempts
if [ -d ".venv" ] && [ ! -f ".venv/bin/activate" ]; then
  echo "Cleaning up incomplete virtual environment..."
  rm -rf .venv
fi

# Create virtual environment if it doesn't exist
if [ ! -d ".venv" ]; then
  echo "Creating virtual environment in docs/.venv..."
  python3 -m venv .venv
  
  # Make sure the virtual environment was created successfully
  if [ ! -f ".venv/bin/activate" ]; then
    echo "Failed to create virtual environment. Trying with --system-site-packages option."
    python3 -m venv --system-site-packages .venv
    
    if [ ! -f ".venv/bin/activate" ]; then
      echo "Virtual environment creation failed. Please check your Python installation."
      exit 1
    fi
  fi
fi

# Activate virtual environment
echo "Activating virtual environment..."
source .venv/bin/activate

# Verify activation was successful
if [ -z "$VIRTUAL_ENV" ]; then
  echo "Failed to activate virtual environment. Please check your setup."
  exit 1
fi

# Install or update dependencies
echo "Installing documentation dependencies..."
pip install -r requirements.txt

# Build the documentation
echo "Building static documentation site..."
python -m mkdocs build

echo "Documentation built successfully in site/ directory"
