// | This module defines the `Writer` monad.
const writer = x => x;
const runWriter = x => x;
const mapWriter = f => v => f(v);
const execWriter = m => m._2;
export {execWriter, mapWriter, runWriter, writer};
