const out = document.getElementById('out');
const btn = document.getElementById('runBtn');

function logOk(msg) {
  const p = document.createElement('p');
  p.className = 'ok';
  p.textContent = msg;
  out.appendChild(p);
}

function logErr(msg) {
  const p = document.createElement('p');
  p.className = 'err';
  p.textContent = msg;
  out.appendChild(p);
}

btn.addEventListener('click', async () => {
  out.innerHTML = '';
  try {
    // Prepare host imports with a dynamic reference to memory
    // Create linear memory to import into the wasm module
    const initialPages = 16; // 16 * 64KiB = 1 MiB
    const pageBytes = 65536;
    const STACK_SIZE = 64 * 1024; // 64KiB stack reservation
    const linearMemory = new WebAssembly.Memory({ initial: initialPages, maximum: 256 });

    const env = {
      // Expose memory both under the expected import name and our helper alias
      __linear_memory: linearMemory,
      memory: linearMemory,
      // Provide a mutable global stack pointer if the module imports it
      // Initialize SP to end - STACK_SIZE so stack grows downward within bounds
      __stack_pointer: new WebAssembly.Global({ value: 'i32', mutable: true }, (initialPages * pageBytes) - STACK_SIZE),
      // Provide a default heap base (optional, only used if imported)
      __heap_base: new WebAssembly.Global({ value: 'i32', mutable: false }, 64 * 1024),
      // If needed later, uncomment to provide an indirect function table import
      // __indirect_function_table: new WebAssembly.Table({ element: 'anyfunc', initial: 0 }),
      host_log: (ptr, len) => {
        try {
          const mem = env.memory;
          if (!mem) throw new Error('env.memory not initialized');
          const size = mem.buffer.byteLength >>> 0;
          const p = Number(ptr) >>> 0;
          const l = Number(len) >>> 0;
          console.log('[host_log] ptr=', p, 'len=', l, 'memBytes=', size);
          if (!(l >= 0) || !(p >= 0) || p > size) {
            throw new Error(`invalid ptr/len: ptr=${p} len=${l} memBytes=${size}`);
          }
          const end = p + l;
          if (end > size) {
            throw new Error(`out of bounds: ptr+len=${end} > memBytes=${size}`);
          }
          const bytes = new Uint8Array(mem.buffer).subarray(p, end);
          const text = new TextDecoder().decode(bytes);
          console.log(text);
          const pre = document.createElement('pre');
          pre.textContent = text;
          out.appendChild(pre);
        } catch (e) {
          console.error('host_log error:', e);
          logErr(String(e));
        }
      },
      // Accepts i64 as BigInt from WASM
      host_log_i64: (x) => {
        try {
          // x is a BigInt when coming from WebAssembly i64
          const s = (typeof x === 'bigint') ? x.toString() : String(x);
          console.log(s);
          const pre = document.createElement('pre');
          pre.textContent = s;
          out.appendChild(pre);
        } catch (e) {
          console.error('host_log_i64 error:', e);
          logErr(String(e));
        }
      },
      // Accepts f64 as Number from WASM
      host_log_f64: (x) => {
        try {
          // x is a Number from WebAssembly f64
          const s = Number.isFinite(x) ? x.toString() : String(x);
          console.log(s);
          const pre = document.createElement('pre');
          pre.textContent = s;
          out.appendChild(pre);
        } catch (e) {
          console.error('host_log_f64 error:', e);
          logErr(String(e));
        }
      }
    };

    // Fetch and instantiate the wasm module (expects ./app.wasm next to this file)
    const response = await fetch(`./app.wasm?ts=${Date.now()}`);
    if (!response.ok) throw new Error(`Failed to fetch app.wasm: ${response.status}`);
    const bytes = await response.arrayBuffer();
    // Inspect required imports for debugging
    const module = await WebAssembly.compile(bytes);
    const requiredImports = WebAssembly.Module.imports(module);
    console.log('[wasm] required imports:', requiredImports);
    requiredImports.forEach(imp => console.log('[wasm] import:', imp.module, imp.name, imp.kind));

    // Ensure the expected memory import is a WebAssembly.Memory
    const needsLinear = requiredImports.some(imp => imp.kind === 'memory' && imp.module === 'env' && imp.name === '__linear_memory');
    const needsMemory = requiredImports.some(imp => imp.kind === 'memory' && imp.module === 'env' && imp.name === 'memory');
    if (needsLinear && !(env.__linear_memory instanceof WebAssembly.Memory)) {
      throw new Error('env.__linear_memory is not a WebAssembly.Memory at instantiation time');
    }
    if (needsMemory && !(env.memory instanceof WebAssembly.Memory)) {
      throw new Error('env.memory is not a WebAssembly.Memory at instantiation time');
    }

    // Instantiate with the prepared env
    const instance = await WebAssembly.instantiate(module, { env });

    if (typeof instance.exports.main !== 'function') {
      logErr('Export "main" not found. Ensure the module exports main().');
      return;
    }

    instance.exports.main();
    logOk('Called exports.main() successfully.');
  } catch (err) {
    console.error(err);
    logErr(String(err));
  }
});
