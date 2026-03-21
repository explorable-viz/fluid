// Alias require to prevent webpack or browserify from actually requiring.
const req = typeof module === "undefined" ? undefined : module.require;
const util = (function() {
  try {
    return req === undefined ? undefined : req("util");
  } catch(e) {
    return undefined;
  }
})();

export function _trace(x, k) {
  // node only recurses two levels into an object before printing
  // "[object]" for further objects when using console.log()
  if (util !== undefined) {
    console.log(util.inspect(x, { depth: null, colors: true }));
  } else {
    console.log(x);
  }
  return k({});
}

export function _spy(tag, x) {
  if (util !== undefined) {
    console.log(tag + ":", util.inspect(x, { depth: null, colors: true }));
  } else {
    console.log(tag + ":", x);
  }
  return x;
}

export function _debugger(f) {
  debugger;
  return f();

}

const now = (function () {
  var perf;
  if (typeof performance !== "undefined") {
    // In browsers, `performance` is available in the global context
    perf = performance;
  } else if (req) {
    // In Node, `performance` is an export of `perf_hooks`
    try { perf = req("perf_hooks").performance; }
    catch(e) { }
  }

  return (function() { return (perf || Date).now(); });
})();

export function _traceTime(name, f) {
  var start = now();
  var res = f();
  var end = now();
  console.log(name + " took " + (end - start) + "ms");
  return res;
}
