# Installing Medi

Medi is currently in pre-alpha development. This guide will walk you through setting up the Medi development environment.

## System Requirements

* **Operating System**: Linux (Ubuntu 20.04+, Debian 11+), macOS (10.15+), or Windows 10/11
* **Memory**: 8GB RAM minimum (16GB recommended)
* **Disk Space**: 2GB for basic installation
* **Optional Requirements**:
  * CUDA-compatible GPU for AI and parallel processing features
  * RISC-V development board for edge device testing

## Installation Methods

### Option 1: Pre-built Binaries (Recommended)

```bash
# Download the Medi installer
curl -sSL https://get.medi-lang.org | bash

# Verify installation
medi --version
```

### Option 2: Building from Source

```bash
# Clone the repository
git clone https://github.com/MediLang/medi.git
cd medi

# Install dependencies
./scripts/install_deps.sh

# Build Medi
make

# Install
sudo make install

# Verify installation
medi --version
```

### Option 3: Docker Container

```bash
# Pull the Medi Docker image
docker pull medilang/medi:latest

# Run a Medi container
docker run -it --rm medilang/medi medi --version
```

## IDE Setup

The Medi IDE (Medi Studio) can be installed separately:

```bash
medi install-ide
```

Alternatively, you can use Medi with VS Code by installing the Medi extension from the marketplace.

## Next Steps

* Learn about [Medi's basic syntax](basic-syntax.md)
* Try [your first Medi program](first-program.md)
* Explore the [standard library](../reference/standard-library.md)
