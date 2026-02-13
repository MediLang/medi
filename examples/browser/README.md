# Medi Browser (wasm32-unknown-unknown) Example

This example demonstrates compiling a Medi program to a browser-friendly WebAssembly module (no WASI), then loading it from a simple web page.

## Build the compiler (with LLVM backend)

```bash
cargo build -p medic --features llvm-backend
```

## Compile the example to wasm32-unknown-unknown

```bash
# From repo root
./target/debug/medic --emit=wasm32-unknown --out=examples/browser/app.wasm examples/browser/app.tlvx
```

Notes:
- This uses the `wasm32-unknown-unknown` target (no WASI). The top-level statements are lowered into an exported `main(): void`.
- The backend marks `main` with a wasm export attribute so JavaScript can call it.

## Serve the directory and open the page

Modern browsers disallow `fetch()` from `file://`. Serve this directory over HTTP:

```bash
# From repo root (adjust port if you like)
python3 -m http.server --directory examples/browser 8080
# Then open http://localhost:8080/ in your browser.
```

Click "Load & Run". The page fetches `app.wasm`, instantiates it, and calls `exports.main()`.

## Limitations

- No WASI imports in this variant; there is no `print()` intrinsic for the browser target yet. Use JS glue or add host imports to expose I/O.
- For visible output, consider creating a host import that writes to the DOM or console, and lower a Medi intrinsic to call it.
