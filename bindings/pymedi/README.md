# pymedi

Python bindings prototype for Medi using PyO3 + maturin.

## Build

```bash
python -m venv .venv
source .venv/bin/activate
pip install -U pip maturin
maturin develop
```

## Quick smoke test

```bash
python -c "import pymedi; print(pymedi.mean([1.0,2.0,3.0]))"
```
