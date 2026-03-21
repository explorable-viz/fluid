import * as $runtime from "../runtime.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import {new as $$new, _append, _appendBlob, _delete, _fromFormElement, _has, _set, _setBlob} from "./foreign.js";
const FileName = x => x;
const EntryName = x => x;
const setBlob = name => value => filename => fd => {
  const $0 = (() => {
    if (filename.tag === "Nothing") { return Data$dNullable.null; }
    if (filename.tag === "Just") { return Data$dNullable.notNull(filename._1); }
    $runtime.fail();
  })();
  return () => _setBlob(name, value, $0, fd);
};
const $$set = name => value => fd => () => _set(name, value, fd);
const ordFileName = Data$dOrd.ordString;
const ordEntryName = Data$dOrd.ordString;
const newtypeFileName = {Coercible0: () => {}};
const newtypeEntryName = {Coercible0: () => {}};
const has = name => fd => () => _has(name, fd);
const fromFormElement = formElement => () => _fromFormElement(formElement);
const eqFileName = Data$dEq.eqString;
const eqEntryName = Data$dEq.eqString;
const $$delete = name => fd => () => _delete(name, fd);
const appendBlob = name => value => filename => fd => {
  const $0 = (() => {
    if (filename.tag === "Nothing") { return Data$dNullable.null; }
    if (filename.tag === "Just") { return Data$dNullable.notNull(filename._1); }
    $runtime.fail();
  })();
  return () => _appendBlob(name, value, $0, fd);
};
const append = name => value => fd => () => _append(name, value, fd);
export {
  EntryName,
  FileName,
  append,
  appendBlob,
  $$delete as delete,
  eqEntryName,
  eqFileName,
  fromFormElement,
  has,
  newtypeEntryName,
  newtypeFileName,
  ordEntryName,
  ordFileName,
  $$set as set,
  setBlob
};
export * from "./foreign.js";
