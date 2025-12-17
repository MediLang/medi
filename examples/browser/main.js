import { autocompletion } from 'https://esm.sh/@codemirror/autocomplete';
import { StreamLanguage } from 'https://esm.sh/@codemirror/language';
import { python } from 'https://esm.sh/@codemirror/lang-python';
import { javascript } from 'https://esm.sh/@codemirror/lang-javascript';
import { sql } from 'https://esm.sh/@codemirror/lang-sql';
import { Compartment, EditorState } from 'https://esm.sh/@codemirror/state';
import { EditorView } from 'https://esm.sh/@codemirror/view';
import { basicSetup } from 'https://esm.sh/codemirror';

const out = document.getElementById('out');
const btn = document.getElementById('runBtn');
const analyzeBtn = document.getElementById('analyzeBtn');
const diagnosticsEl = document.getElementById('diagnostics');
const privacyEl = document.getElementById('privacy');
const statusEl = document.getElementById('status');
const langSelect = document.getElementById('langSelect');
const fileInput = document.getElementById('fileInput');
const analyticsChart = document.getElementById('analyticsChart');
const analyticsInfo = document.getElementById('analyticsInfo');

const languageCompartment = new Compartment();
const completionCompartment = new Compartment();

const mediKeywords = [
  'let',
  'fn',
  'return',
  'if',
  'else',
  'for',
  'while',
  'match',
  'true',
  'false',
  'null',
  'regulate',
  'scope',
  'federated',
  'query',
  'fhir_query',
  'of',
  'per',
  'as'
];

const mediBuiltins = [
  'print',
  'println',
  'log',
  'debug',
  'trace',
  'export',
  'write_file',
  'writeFile',
  'save',
  'send_http',
  'http_post',
  'http_get'
];

const localMediCompletions = [...mediKeywords, ...mediBuiltins]
  .map((label) => ({ label, type: mediKeywords.includes(label) ? 'keyword' : 'function' }));

const mediLanguage = StreamLanguage.define({
  startState() {
    return { inBlockComment: false };
  },
  token(stream, state) {
    if (state.inBlockComment) {
      if (stream.match('*/')) {
        state.inBlockComment = false;
      } else {
        stream.next();
      }
      return 'comment';
    }

    if (stream.match('//')) {
      stream.skipToEnd();
      return 'comment';
    }
    if (stream.match('/*')) {
      state.inBlockComment = true;
      return 'comment';
    }

    if (stream.eatSpace()) return null;
    if (stream.match(/\d+(?:\.\d+)?/)) return 'number';
    if (stream.match(/"(?:[^\\"]|\\.)*"/)) return 'string';
    if (stream.match(/'(?:[^\\']|\\.)*'/)) return 'string';

    if (stream.match(/[A-Za-z_][A-Za-z0-9_]*/)) {
      const w = stream.current();
      if (mediKeywords.includes(w)) return 'keyword';
      return 'variableName';
    }

    if (stream.match(/==|!=|<=|>=|\?\?|\?:|\.\.|\.\.=|\+|\-|\*|\/|%|=|<|>|!|&|\||\^/)) {
      return 'operator';
    }

    stream.next();
    return null;
  }
});

function setStatus(text) {
  if (!statusEl) return;
  statusEl.textContent = text;
}

function renderDiagnostics(diags) {
  if (!diagnosticsEl) return;
  if (!diags || diags.length === 0) {
    diagnosticsEl.textContent = 'No diagnostics.';
    return;
  }
  const ul = document.createElement('ul');
  ul.className = 'list';
  for (const d of diags) {
    const li = document.createElement('li');
    const where = (typeof d.line === 'number' && typeof d.column === 'number') ? ` (line ${d.line}, col ${d.column})` : '';
    li.textContent = `${d.severity || 'error'}: ${d.message}${where}`;
    if (d.help) {
      const help = document.createElement('div');
      help.className = 'muted';
      help.textContent = d.help;
      li.appendChild(help);
    }
    ul.appendChild(li);
  }
  diagnosticsEl.innerHTML = '';
  diagnosticsEl.appendChild(ul);
}

function renderPrivacy(spans) {
  if (!privacyEl) return;
  if (!spans || spans.length === 0) {
    privacyEl.textContent = 'No privacy spans.';
    return;
  }
  const ul = document.createElement('ul');
  ul.className = 'list';
  for (const p of spans) {
    const li = document.createElement('li');
    li.textContent = `${p.label} [${p.start}, ${p.end}]`;
    ul.appendChild(li);
  }
  privacyEl.innerHTML = '';
  privacyEl.appendChild(ul);
}

function getLanguageExtension(lang) {
  switch (lang) {
    case 'python':
      return python();
    case 'javascript':
      return javascript();
    case 'sql':
      return sql();
    case 'medi':
    default:
      return mediLanguage;
  }
}

function getCompletionExtension(lang) {
  if (lang !== 'medi') {
    return autocompletion({ override: [] });
  }
  return autocompletion({
    override: [async (context) => {
      const before = context.matchBefore(/[A-Za-z_][A-Za-z0-9_]*/);
      if (!before && !context.explicit) return null;
      const from = before ? before.from : context.pos;

      try {
        const source = context.state.doc.toString();
        const resp = await fetch('http://127.0.0.1:8710/complete', {
          method: 'POST',
          headers: { 'content-type': 'application/json' },
          body: JSON.stringify({ source, offset: context.pos })
        });
        const json = await resp.json();
        const options = (json.items || []).map((it) => ({
          label: it.label,
          type: it.kind || 'text'
        }));
        return {
          from: typeof json.from === 'number' ? json.from : from,
          options
        };
      } catch (_e) {
        return {
          from,
          options: localMediCompletions
        };
      }
    }]
  });
}

const initialDoc = `let patients = fhir_query("Patient")
  .filter(p => p.age > 65)
  .limit(10);

println(patients);
`;

const view = new EditorView({
  state: EditorState.create({
    doc: initialDoc,
    extensions: [
      basicSetup,
      languageCompartment.of(getLanguageExtension(langSelect ? langSelect.value : 'medi')),
      completionCompartment.of(getCompletionExtension(langSelect ? langSelect.value : 'medi'))
    ]
  }),
  parent: document.getElementById('editor')
});

let hoverTimer = null;
let lastHoverPos = -1;
async function doHover(pos) {
  try {
    const source = view.state.doc.toString();
    const resp = await fetch('http://127.0.0.1:8710/hover', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ source, offset: pos })
    });
    const json = await resp.json();
    const parts = [];
    if (json.token) parts.push(json.token);
    if (json.type_info) parts.push(String(json.type_info));
    if (json.unit) parts.push(`unit=${json.unit}`);
    if (json.privacy) parts.push(`privacy=${json.privacy}`);
    if (parts.length > 0) {
      setStatus(parts.join(' | '));
    }
  } catch (_e) {
  }
}

view.dom.addEventListener('keyup', () => {
  const pos = view.state.selection.main.head;
  if (pos === lastHoverPos) return;
  lastHoverPos = pos;
  if (hoverTimer) clearTimeout(hoverTimer);
  hoverTimer = setTimeout(() => {
    void doHover(pos);
  }, 250);
});

view.dom.addEventListener('mouseup', () => {
  const pos = view.state.selection.main.head;
  if (pos === lastHoverPos) return;
  lastHoverPos = pos;
  if (hoverTimer) clearTimeout(hoverTimer);
  hoverTimer = setTimeout(() => {
    void doHover(pos);
  }, 150);
});

if (langSelect) {
  langSelect.addEventListener('change', () => {
    const lang = langSelect.value;
    view.dispatch({
      effects: [
        languageCompartment.reconfigure(getLanguageExtension(lang)),
        completionCompartment.reconfigure(getCompletionExtension(lang))
      ]
    });
  });
}

if (fileInput) {
  fileInput.addEventListener('change', async () => {
    const file = fileInput.files && fileInput.files[0];
    if (!file) return;
    const text = await file.text();
    view.dispatch({
      changes: { from: 0, to: view.state.doc.length, insert: text }
    });
  });
}

if (analyzeBtn) {
  analyzeBtn.addEventListener('click', async () => {
    try {
      setStatus('Analyzing...');
      const source = view.state.doc.toString();
      const resp = await fetch('http://127.0.0.1:8710/analyze', {
        method: 'POST',
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ source })
      });
      const json = await resp.json();
      renderDiagnostics(json.diagnostics);
      renderPrivacy(json.privacy);
      renderAnalytics(json.privacy, source);
      setStatus(resp.ok ? 'Analyze complete.' : `Analyze failed (${resp.status}).`);
    } catch (e) {
      setStatus(`Analyze error: ${String(e)}`);
    }
  });
}

function renderAnalytics(privacySpans, source) {
  if (!analyticsChart || !analyticsInfo) return;
  const ctx = analyticsChart.getContext('2d');
  ctx.clearRect(0, 0, analyticsChart.width, analyticsChart.height);

  if (!privacySpans || privacySpans.length === 0) {
    analyticsInfo.textContent = 'No privacy data to visualize.';
    return;
  }

  const counts = {};
  for (const span of privacySpans) {
    const label = span.label || 'Unknown';
    counts[label] = (counts[label] || 0) + 1;
  }

  const labels = Object.keys(counts);
  const values = Object.values(counts);
  const total = values.reduce((a, b) => a + b, 0);
  const colors = {
    'Identified': '#ef4444',
    'Pseudonymized': '#f59e0b',
    'Anonymized': '#22c55e',
    'DeIdentified': '#3b82f6',
    'Unknown': '#9ca3af'
  };

  const barWidth = Math.min(60, (analyticsChart.width - 40) / labels.length - 10);
  const maxVal = Math.max(...values, 1);
  const chartHeight = analyticsChart.height - 40;

  ctx.font = '12px system-ui, sans-serif';
  ctx.textAlign = 'center';

  labels.forEach((label, i) => {
    const x = 30 + i * (barWidth + 10);
    const barHeight = (values[i] / maxVal) * (chartHeight - 20);
    const y = chartHeight - barHeight;

    ctx.fillStyle = colors[label] || '#6b7280';
    ctx.fillRect(x, y, barWidth, barHeight);

    ctx.fillStyle = '#374151';
    ctx.fillText(label.slice(0, 8), x + barWidth / 2, analyticsChart.height - 5);
    ctx.fillText(String(values[i]), x + barWidth / 2, y - 5);
  });

  analyticsInfo.textContent = `Privacy distribution: ${total} span(s) across ${labels.length} category(ies).`;
}

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

// Visual Programming Interface
const vpCanvas = document.getElementById('vpCanvas');
const vpConnections = document.getElementById('vpConnections');
const vpOutput = document.getElementById('vpOutput');
const addQueryNode = document.getElementById('addQueryNode');
const addFilterNode = document.getElementById('addFilterNode');
const addOutputNode = document.getElementById('addOutputNode');
const generateCodeBtn = document.getElementById('generateCode');
const clearCanvasBtn = document.getElementById('clearCanvas');

let vpNodes = [];
let vpEdges = [];
let nodeIdCounter = 0;
let draggingNode = null;
let dragOffset = { x: 0, y: 0 };
let connectingFrom = null;

function createNode(type, x, y) {
  const id = ++nodeIdCounter;
  const node = { id, type, x, y, value: '' };
  vpNodes.push(node);
  renderNodes();
  return node;
}

function renderNodes() {
  const existingNodes = vpCanvas.querySelectorAll('.vp-node');
  existingNodes.forEach(n => n.remove());

  for (const node of vpNodes) {
    const el = document.createElement('div');
    el.className = `vp-node ${node.type}`;
    el.dataset.id = node.id;
    el.style.left = node.x + 'px';
    el.style.top = node.y + 'px';

    let title = '';
    let placeholder = '';
    if (node.type === 'query') {
      title = 'FHIR Query';
      placeholder = 'Resource (e.g. Patient)';
    } else if (node.type === 'filter') {
      title = 'Filter';
      placeholder = 'Condition (e.g. age > 65)';
    } else if (node.type === 'output') {
      title = 'Output';
      placeholder = 'Variable name';
    }

    el.innerHTML = `
      <div class="vp-node-title">${title}</div>
      <input class="vp-node-input" placeholder="${placeholder}" value="${node.value || ''}" />
      ${node.type !== 'query' ? '<div class="vp-port in" data-port="in"></div>' : ''}
      ${node.type !== 'output' ? '<div class="vp-port out" data-port="out"></div>' : ''}
    `;

    const input = el.querySelector('input');
    input.addEventListener('input', (e) => {
      node.value = e.target.value;
    });
    input.addEventListener('mousedown', (e) => e.stopPropagation());

    el.addEventListener('mousedown', (e) => {
      if (e.target.classList.contains('vp-port')) {
        if (e.target.dataset.port === 'out') {
          connectingFrom = node;
        }
        return;
      }
      draggingNode = node;
      const rect = el.getBoundingClientRect();
      dragOffset.x = e.clientX - rect.left;
      dragOffset.y = e.clientY - rect.top;
    });

    const outPort = el.querySelector('.vp-port.out');
    const inPort = el.querySelector('.vp-port.in');

    if (inPort) {
      inPort.addEventListener('mouseup', () => {
        if (connectingFrom && connectingFrom.id !== node.id) {
          const exists = vpEdges.some(e => e.from === connectingFrom.id && e.to === node.id);
          if (!exists) {
            vpEdges.push({ from: connectingFrom.id, to: node.id });
            renderConnections();
          }
        }
        connectingFrom = null;
      });
    }

    vpCanvas.appendChild(el);
  }
}

function renderConnections() {
  vpConnections.innerHTML = '';
  for (const edge of vpEdges) {
    const fromNode = vpNodes.find(n => n.id === edge.from);
    const toNode = vpNodes.find(n => n.id === edge.to);
    if (!fromNode || !toNode) continue;

    const fromEl = vpCanvas.querySelector(`[data-id="${fromNode.id}"]`);
    const toEl = vpCanvas.querySelector(`[data-id="${toNode.id}"]`);
    if (!fromEl || !toEl) continue;

    const fromRect = fromEl.getBoundingClientRect();
    const toRect = toEl.getBoundingClientRect();
    const canvasRect = vpCanvas.getBoundingClientRect();

    const x1 = fromRect.right - canvasRect.left;
    const y1 = fromRect.top + fromRect.height / 2 - canvasRect.top;
    const x2 = toRect.left - canvasRect.left;
    const y2 = toRect.top + toRect.height / 2 - canvasRect.top;

    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const cx = (x1 + x2) / 2;
    path.setAttribute('d', `M ${x1} ${y1} C ${cx} ${y1}, ${cx} ${y2}, ${x2} ${y2}`);
    path.setAttribute('stroke', '#6b7280');
    path.setAttribute('stroke-width', '2');
    path.setAttribute('fill', 'none');
    vpConnections.appendChild(path);
  }
}

if (vpCanvas) {
  vpCanvas.addEventListener('mousemove', (e) => {
    if (!draggingNode) return;
    const rect = vpCanvas.getBoundingClientRect();
    draggingNode.x = Math.max(0, Math.min(rect.width - 130, e.clientX - rect.left - dragOffset.x));
    draggingNode.y = Math.max(0, Math.min(rect.height - 60, e.clientY - rect.top - dragOffset.y));
    renderNodes();
    renderConnections();
  });

  vpCanvas.addEventListener('mouseup', () => {
    draggingNode = null;
    connectingFrom = null;
  });

  vpCanvas.addEventListener('mouseleave', () => {
    draggingNode = null;
    connectingFrom = null;
  });
}

if (addQueryNode) {
  addQueryNode.addEventListener('click', () => {
    createNode('query', 20, 20 + vpNodes.length * 10);
  });
}

if (addFilterNode) {
  addFilterNode.addEventListener('click', () => {
    createNode('filter', 180, 20 + vpNodes.length * 10);
  });
}

if (addOutputNode) {
  addOutputNode.addEventListener('click', () => {
    createNode('output', 340, 20 + vpNodes.length * 10);
  });
}

if (clearCanvasBtn) {
  clearCanvasBtn.addEventListener('click', () => {
    vpNodes = [];
    vpEdges = [];
    nodeIdCounter = 0;
    renderNodes();
    renderConnections();
    if (vpOutput) vpOutput.textContent = 'Canvas cleared.';
  });
}

if (generateCodeBtn) {
  generateCodeBtn.addEventListener('click', () => {
    const lines = [];
    const processed = new Set();

    function processNode(nodeId, varName) {
      if (processed.has(nodeId)) return varName;
      processed.add(nodeId);

      const node = vpNodes.find(n => n.id === nodeId);
      if (!node) return varName;

      const inEdge = vpEdges.find(e => e.to === nodeId);
      let inputVar = inEdge ? processNode(inEdge.from, 'data') : null;

      if (node.type === 'query') {
        const resource = node.value || 'Patient';
        const vn = `${resource.toLowerCase()}s`;
        lines.push(`let ${vn} = fhir_query("${resource}");`);
        return vn;
      } else if (node.type === 'filter') {
        const cond = node.value || 'true';
        const vn = `filtered_${inputVar || 'data'}`;
        lines.push(`let ${vn} = ${inputVar || 'data'}.filter(x => ${cond});`);
        return vn;
      } else if (node.type === 'output') {
        const name = node.value || 'result';
        lines.push(`let ${name} = ${inputVar || 'data'};`);
        lines.push(`println(${name});`);
        return name;
      }
      return varName;
    }

    const outputNodes = vpNodes.filter(n => n.type === 'output');
    if (outputNodes.length === 0) {
      for (const node of vpNodes) {
        processNode(node.id, 'data');
      }
    } else {
      for (const node of outputNodes) {
        processNode(node.id, 'data');
      }
    }

    const code = lines.join('\n') || '// Add nodes and connect them to generate code';
    if (vpOutput) vpOutput.textContent = code;

    view.dispatch({
      changes: { from: view.state.doc.length, insert: '\n\n// Generated from Visual Programming:\n' + code }
    });
  });
}
