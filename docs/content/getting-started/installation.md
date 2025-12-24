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

### Building from Source

```bash
# Clone the repository
git clone https://github.com/MediLang/medi.git
cd medi

# Build the compiler CLI
cargo build -p medic

# Run the CLI from the repository
cargo run -p medic -- --help
```

## Next Steps

* Learn about [Medi's basic syntax](basic-syntax.md)
* Try [your first Medi program](first-program.md)
* Explore the [standard library](../reference/standard-library.md)
