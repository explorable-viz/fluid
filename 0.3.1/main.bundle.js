/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "/";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 30);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
function classOf(x) {
    return __nonNull(x).constructor; // weirdly failing on CircleCI without cast
}
exports.classOf = classOf;
function className(o) {
    return classOf(o).name;
}
exports.className = className;
function as(x, C) {
    if (__nonNull(x) instanceof C) {
        return x;
    }
    else {
        return assert(false, "[as] Expected " + C.name + ", got " + className(x));
    }
}
exports.as = as;
function asOpt(x, cls) {
    if (x === null || x === undefined) {
        return x;
    }
    else {
        return as(x, cls);
    }
}
exports.asOpt = asOpt;
function assert(b, msg, ...x̅) {
    if (!b) {
        if (x̅.length > 0) {
            console.warn("Assertion data:\n");
            x̅.forEach(x => console.warn(x));
        }
        throw new Error(msg || "Assertion failure");
    }
}
exports.assert = assert;
function absurd(msg, ...x̅) {
    assert(false, msg, ...x̅);
}
exports.absurd = absurd;
function id(x) {
    return x;
}
exports.id = id;
// User-level error.
function userError(msg, ...x̅) {
    if (x̅.length > 0) {
        console.warn("Error data:\n");
        x̅.forEach(x => console.warn(x));
    }
    throw new Error("User error: " + msg);
}
exports.userError = userError;
function notYetImplemented() {
    throw new Error("Not yet implemented");
}
exports.notYetImplemented = notYetImplemented;
// Useful when a notionally abstract class needs to be concrete.
function abstractMethodError(this_) {
    return assert(false, "Abstract method in " + this_);
}
exports.abstractMethodError = abstractMethodError;
function __nonNull(x, msg) {
    if (x !== null && x !== undefined) {
        return x;
    }
    else {
        return assert(false, `Unexpected null | undefined.${msg === undefined ? "" : `\n${msg}`}`);
    }
}
exports.__nonNull = __nonNull;
function __log(x, msg, transform = (it) => it) {
    const x_ = transform(x);
    if (msg) {
        console.log(msg(x_));
    }
    console.log(x_);
    return x;
}
exports.__log = __log;
function __check(x, predicate) {
    assert(predicate(x));
    return x;
}
exports.__check = __check;


/***/ }),
/* 1 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
// Use to initialise fields for reflection, without requiring constructors.
exports._ = undefined;
// Value in the metalanguage.
class Value {
    __child(k) {
        return this[k];
    }
    // Probably confusingly, "children" isn't a user-level notion; specifically, wrappers
    // like Num and Str have children which are not observable through pattern-matching.
    get __children() {
        return fields(this).map(k => this.__child(k));
    }
}
exports.Value = Value;
// Address or location of persistent object.
class Id extends Value {
    tag(tag) {
        return taggedId(this, tag);
    }
}
exports.Id = Id;
class FunctionId extends Id {
    constructor() {
        super(...arguments);
        this.f = exports._;
    }
}
function functionId(f) {
    return make(FunctionId, f);
}
class ApplicationId extends Id {
    constructor() {
        super(...arguments);
        this.k = exports._;
        this.v = exports._;
    }
}
exports.ApplicationId = ApplicationId;
function applicationId(k, v) {
    return make(ApplicationId, k, v);
}
class TaggedId extends Id {
    constructor() {
        super(...arguments);
        this.k = exports._;
        this.prop = exports._;
    }
}
exports.TaggedId = TaggedId;
function taggedId(k, prop) {
    return make(TaggedId, k, prop);
}
function memoId(f, v̅) {
    const fʹ = functionId(f);
    let k = fʹ;
    for (let v of v̅) {
        k = applicationId(k, v);
    }
    return k;
}
exports.memoId = memoId;
function isPrim(v) {
    return v instanceof Num || v instanceof Str;
}
exports.isPrim = isPrim;
class Num extends Value {
    constructor() {
        super(...arguments);
        this.val = exports._;
    }
    toString() {
        return this.val.toString();
    }
}
exports.Num = Num;
class Str extends Value {
    constructor() {
        super(...arguments);
        this.val = exports._;
    }
    toString() {
        return `"${this.val}"`;
    }
    leq(str) {
        return this.val.localeCompare(str.val) <= 0;
    }
    eq(str) {
        return this.val.localeCompare(str.val) === 0;
    }
    geq(str) {
        return this.val.localeCompare(str.val) >= 0;
    }
}
exports.Str = Str;
// Mergeable state deltas are disjoint.
function mergeInto(tgt, src) {
    Object.keys(src).forEach((prop) => {
        if (!tgt.hasOwnProperty(prop)) {
            tgt[prop] = src[prop];
        }
        else {
            Core_1.absurd(`Incompatible update of field "${prop}" at revision.`, tgt[prop], src[prop]);
        }
    });
}
exports.mergeInto = mergeInto;
// Hash-consed constructors are invariant across worlds.
const __ctrMemo = new Map;
function lookupArg(f, m, v̅, n) {
    // for memoisation purposes, treat f's key as argument -1
    const k = n === -1 ? f.key : v̅[n];
    let v = m.get(k);
    if (v === undefined) {
        if (n === v̅.length - 1) {
            v = f.call(v̅);
            v = v; // TS confused; thinks v can be undefined here
        }
        else {
            v = new Map;
        }
        m.set(k, v);
    }
    return v;
}
class MemoCtr {
    constructor(C) {
        this.C = C;
    }
    get key() {
        return this.C;
    }
    call(v̅) {
        const v = new this.C;
        construct(false, v, v̅);
        Object.freeze(v);
        return v;
    }
}
function memoCall(memo, f, v̅) {
    let v = lookupArg(f, memo, v̅, -1);
    for (let n = 0; n < v̅.length; ++n) {
        // since there are more arguments, the last v was a (possibly nested) map
        v = lookupArg(f, v, v̅, n);
    }
    return v;
}
exports.memoCall = memoCall;
// Experimented with dictionary-based construction pattern; eliminates field order mismatch as a possible
// source of error, but the benefit is very small and doesn't really suit the memoisation pattern.
function make(C, ...v̅) {
    return memoCall(__ctrMemo, new MemoCtr(C), v̅);
}
exports.make = make;
// Depends heavily on (1) getOwnPropertyNames() returning fields in definition-order; and (2)
// constructor functions supplying arguments in the same order.
function construct(compare, tgt, v̅) {
    const f̅ = fields(tgt), ẟ = compare ? {} : null;
    Core_1.assert(f̅.length === v̅.length);
    let n = 0;
    f̅.forEach((prop) => {
        const src = v̅[n++];
        if (compare && tgt.__child(prop) !== src) {
            ẟ[prop] = { before: tgt.__child(prop), after: src };
        }
        tgt[prop] = src;
    });
    return ẟ;
}
exports.construct = construct;
// Exclude metadata according to our convention.
function isField(prop) {
    return !prop.startsWith("__");
}
exports.isField = isField;
function fields(v) {
    return Object.getOwnPropertyNames(v).filter(isField);
}
exports.fields = fields;
function metadataFields(v) {
    return Object.getOwnPropertyNames(v).filter(f => !isField(f) && f !== "__id");
}
exports.metadataFields = metadataFields;


/***/ }),
/* 2 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const Delta_1 = __webpack_require__(13);
const Value_1 = __webpack_require__(1);
function versioned(v) {
    return v.__id !== undefined;
}
exports.versioned = versioned;
function asVersioned(v) {
    if (versioned(v)) {
        return v;
    }
    else {
        return Core_1.assert(false, `${v} is not versioned.`);
    }
}
exports.asVersioned = asVersioned;
const __versioned = new Map();
const __reachable = new Set(); // subset of __versioned reachable at current revision
// The (possibly already extant) versioned object uniquely identified by a memo-key. As an idempotent side-effect,
// record how the object differs from its previous version. External nodes are always created fresh.
function at(C, ...v̅) {
    return (k) => {
        let v = __versioned.get(k);
        if (v === undefined) {
            const v = create(C, ...v̅)(k);
            if (!(k instanceof Extern)) {
                __versioned.set(k, v);
                __reachable.add(v);
            }
            return v;
        }
        else {
            Core_1.assert(!(k instanceof Extern));
            __reachable.add(v);
            reset(v, C, ...v̅);
            return v;
        }
    };
}
exports.at = at;
function create(C, ...v̅) {
    return (k) => {
        const v = new C;
        Object.defineProperty(v, "__id", {
            value: k,
            enumerable: false
        });
        Object.defineProperty(v, "__ẟ", {
            // The delta map is partial; the absence of an entry is equivalent to an empty delta. This allows
            // deltas to be cleared simply by removing all entries from the map.
            get: function () {
                let ẟ = Delta_1.__deltas.ẟ̅.get(this);
                if (ẟ === undefined) {
                    ẟ = new Delta_1.Change({});
                    Delta_1.__deltas.ẟ̅.set(this, ẟ);
                    return ẟ;
                }
                else {
                    return ẟ;
                }
            },
            enumerable: false
        });
        Value_1.construct(false, v, v̅);
        Delta_1.__deltas.created(v);
        return v;
    };
}
exports.create = create;
function reset(v, C, ...v̅) {
    if (v instanceof C) {
        Delta_1.__deltas.changed(v, Value_1.construct(true, v, v̅));
    }
    else {
        reclassify(v, C);
        Value_1.construct(false, v, v̅);
        Delta_1.__deltas.reclassified(v);
    }
}
exports.reset = reset;
// Should emulate the post-state of "new C". Probably need to worry about how this works with inherited properties.
function reclassify(v, ctr) {
    const proto = Object.getPrototypeOf(new ctr);
    Core_1.assert(Object.getPrototypeOf(v) !== proto);
    for (const k of Value_1.fields(v)) {
        Core_1.assert(delete v[k]);
    }
    Object.setPrototypeOf(v, proto);
}
// A memo key which is sourced externally to the system. (The name "External" is already taken.)
class Extern extends Value_1.Id {
    constructor() {
        super(...arguments);
        this.id = Value_1._;
    }
}
exports.Extern = Extern;
function extern(id) {
    return Value_1.make(Extern, id);
}
// Fresh keys represent inputs to the system, e.g. addresses of syntax nodes provided by an external structure editor.
exports.ν = (() => {
    let count = 0;
    return () => {
        return extern(count++);
    };
})();
function num(val) {
    return at(Value_1.Num, val);
}
exports.num = num;
function str(val) {
    return at(Value_1.Str, val);
}
exports.str = str;
const __funMemo = new Map;
// Should clear memo table at each revision, but there are no memo-functions at the moment.
function newRevision() {
    Delta_1.__deltas.clear();
    __versioned.forEach((v, k) => {
        if (!__reachable.has(v)) {
            __versioned.delete(k);
        }
    });
    __reachable.clear();
}
exports.newRevision = newRevision;
class MemoFun {
    constructor(f) {
        this.f = f;
    }
    get key() {
        return this.f;
    }
    call(v̅) {
        return this.f.apply(null, v̅);
        // for an "instance" version where v̅[0] is "this", use:
        // return this.f.apply(v̅[0], v̅.slice(1))
    }
}
// Memoisation.
function memo(f, ...v̅) {
    return Value_1.memoCall(__funMemo, new MemoFun(f), v̅);
}
exports.memo = memo;


/***/ }),
/* 3 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const Value_1 = __webpack_require__(1);
// Value of a datatype constructor; children are always user-level values (i.e. not ES6 primitives).
class DataValue extends Value_1.Value {
    get ctr() {
        return Core_1.className(this);
    }
    __child(k) {
        return super.__child(k);
    }
    get __children() {
        return super.__children;
    }
}
exports.DataValue = DataValue;
// Here to break cyclic dependency.
class ExplValue extends DataValue {
    constructor() {
        super(...arguments);
        this.t = Value_1._;
        this.v = Value_1._;
    }
}
exports.ExplValue = ExplValue;
function explValue(t, v) {
    return Value_1.make(ExplValue, t, v);
}
exports.explValue = explValue;


/***/ }),
/* 4 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const DataType_1 = __webpack_require__(7);
const DataValue_1 = __webpack_require__(3);
const Value_1 = __webpack_require__(1);
const Versioned_1 = __webpack_require__(2);
// See Env for convention regarding instance members on reflected datatypes.
class Bool extends DataValue_1.DataValue {
}
exports.Bool = Bool;
class True extends Bool {
}
exports.True = True;
function true_() {
    return Versioned_1.at(True);
}
exports.true_ = true_;
class False extends Bool {
}
exports.False = False;
function false_() {
    return Versioned_1.at(False);
}
exports.false_ = false_;
class List extends DataValue_1.DataValue {
    static fromArray(x̅) {
        let x̅ʹ = nil();
        for (let n = x̅.length - 1; n >= 0; --n) {
            x̅ʹ = cons(x̅[n], x̅ʹ);
        }
        return x̅ʹ;
    }
    toArray() {
        const x̅ = [];
        this.toArray_(x̅);
        return x̅;
    }
    toArray_(x̅) {
        if (Cons.is(this)) {
            x̅.push(this.head);
            this.tail.toArray_(x̅);
        }
        else if (Nil.is(this)) {
        }
        else {
            return Core_1.absurd();
        }
    }
}
exports.List = List;
class Nil extends List {
    static is(xs) {
        return xs instanceof Nil;
    }
}
exports.Nil = Nil;
function nil() {
    return Value_1.make(Nil);
}
exports.nil = nil;
class Cons extends List {
    constructor() {
        super(...arguments);
        this.head = Value_1._;
        this.tail = Value_1._;
    }
    static is(xs) {
        return xs instanceof Cons;
    }
}
exports.Cons = Cons;
function cons(head, tail) {
    return Value_1.make(Cons, head, tail);
}
exports.cons = cons;
class Pair extends DataValue_1.DataValue {
    constructor() {
        super(...arguments);
        this.fst = Value_1._;
        this.snd = Value_1._;
    }
}
exports.Pair = Pair;
function pair(fst, snd) {
    return Value_1.make(Pair, fst, snd);
}
exports.pair = pair;
class Tree extends DataValue_1.DataValue {
    toArray() {
        const x̅ = [];
        this.toArray_(x̅);
        return x̅;
    }
    toArray_(x̅) {
        if (NonEmpty.is(this)) {
            this.left.toArray_(x̅);
            x̅.push(this.t);
            this.right.toArray_(x̅);
        }
        else if (Empty.is(this)) {
        }
        else {
            return Core_1.absurd();
        }
    }
}
exports.Tree = Tree;
class Empty extends Tree {
    static is(t) {
        return t instanceof Empty;
    }
}
exports.Empty = Empty;
function empty() {
    return Value_1.make(Empty);
}
exports.empty = empty;
class NonEmpty extends Tree {
    constructor() {
        super(...arguments);
        this.left = Value_1._;
        this.t = Value_1._;
        this.right = Value_1._;
    }
    static is(t) {
        return t instanceof NonEmpty;
    }
}
exports.NonEmpty = NonEmpty;
function nonEmpty(left, t, right) {
    return Value_1.make(NonEmpty, left, t, right);
}
exports.nonEmpty = nonEmpty;
class Option extends DataValue_1.DataValue {
}
exports.Option = Option;
class None extends Option {
    static is(o) {
        return o instanceof None;
    }
}
exports.None = None;
class Some extends Option {
    constructor() {
        super(...arguments);
        this.t = Value_1._;
    }
    static is(o) {
        return o instanceof Some;
    }
}
exports.Some = Some;
class Ordering extends DataValue_1.DataValue {
}
exports.Ordering = Ordering;
class LT extends Ordering {
}
exports.LT = LT;
class GT extends Ordering {
}
exports.GT = GT;
class EQ extends Ordering {
}
exports.EQ = EQ;
var BaseTypes;
(function (BaseTypes) {
    function initialise() {
        DataType_1.initDataType(Bool, [True, False]);
        DataType_1.initDataType(List, [Nil, Cons]);
        DataType_1.initDataType(Option, [Some, None]);
        DataType_1.initDataType(Ordering, [LT, GT, EQ]);
        DataType_1.initDataType(Pair, [Pair]);
        DataType_1.initDataType(Tree, [Empty, NonEmpty]);
    }
    BaseTypes.initialise = initialise;
})(BaseTypes = exports.BaseTypes || (exports.BaseTypes = {}));


/***/ }),
/* 5 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const DataValue_1 = __webpack_require__(3);
const Value_1 = __webpack_require__(1);
const Versioned_1 = __webpack_require__(2);
var Expl;
(function (Expl_1) {
    class Expl extends DataValue_1.DataValue {
    }
    Expl_1.Expl = Expl;
    class NonTerminal extends Expl {
    }
    Expl_1.NonTerminal = NonTerminal;
    class Terminal extends Expl {
    }
    Expl_1.Terminal = Terminal;
    class App extends NonTerminal {
        constructor() {
            super(...arguments);
            this.tf = Value_1._;
            this.tu = Value_1._;
            this.δ = Value_1._; // additional recursive functions bound at this step
            this.ξ = Value_1._;
            this.t = Value_1._;
        }
    }
    Expl_1.App = App;
    function app(tf, tu, δ, ξ, t) {
        return Versioned_1.at(App, tf, tu, δ, ξ, t);
    }
    Expl_1.app = app;
    class UnaryApp extends Terminal {
        constructor() {
            super(...arguments);
            this.tf = Value_1._;
            this.tv = Value_1._;
        }
    }
    Expl_1.UnaryApp = UnaryApp;
    function unaryApp(tf, tv) {
        return Versioned_1.at(UnaryApp, tf, tv);
    }
    Expl_1.unaryApp = unaryApp;
    class BinaryApp extends Terminal {
        constructor() {
            super(...arguments);
            this.tv1 = Value_1._;
            this.opName = Value_1._;
            this.tv2 = Value_1._;
        }
    }
    Expl_1.BinaryApp = BinaryApp;
    function binaryApp(tv1, opName, tv2) {
        return Versioned_1.at(BinaryApp, tv1, opName, tv2);
    }
    Expl_1.binaryApp = binaryApp;
    // Has a concrete subclass for each datatype.
    class DataExpl extends Terminal {
        get ctr() {
            return Core_1.className(this);
        }
        get __children() {
            return super.__children;
        }
    }
    Expl_1.DataExpl = DataExpl;
    class Def extends DataValue_1.DataValue {
    }
    Expl_1.Def = Def;
    class Let extends Def {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
            this.tv = Value_1._;
        }
    }
    Expl_1.Let = Let;
    function let_(x, tv) {
        return Versioned_1.at(Let, x, tv);
    }
    Expl_1.let_ = let_;
    class Prim extends Def {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
            this.t_op = Value_1._;
        }
    }
    Expl_1.Prim = Prim;
    function prim(x, t_op) {
        return Versioned_1.at(Prim, x, t_op);
    }
    Expl_1.prim = prim;
    class RecDef extends DataValue_1.DataValue {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
            this.tf = Value_1._;
        }
    }
    Expl_1.RecDef = RecDef;
    function recDef(x, tf) {
        return Versioned_1.at(RecDef, x, tf);
    }
    Expl_1.recDef = recDef;
    class LetRec extends Def {
        constructor() {
            super(...arguments);
            this.δ = Value_1._;
        }
    }
    Expl_1.LetRec = LetRec;
    function letRec(δ) {
        return Versioned_1.at(LetRec, δ);
    }
    Expl_1.letRec = letRec;
    class Defs extends NonTerminal {
        constructor() {
            super(...arguments);
            this.def̅ = Value_1._;
            this.t = Value_1._;
        }
    }
    Expl_1.Defs = Defs;
    function defs(def̅, t) {
        return Versioned_1.at(Defs, def̅, t);
    }
    Expl_1.defs = defs;
    class Const extends Terminal {
    }
    Expl_1.Const = Const;
    function const_() {
        return Versioned_1.at(Const);
    }
    Expl_1.const_ = const_;
    class Fun extends Terminal {
        constructor() {
            super(...arguments);
            this.σ = Value_1._;
        }
    }
    Expl_1.Fun = Fun;
    function fun(σ) {
        return Versioned_1.at(Fun, σ);
    }
    Expl_1.fun = fun;
    class MatchAs extends NonTerminal {
        constructor() {
            super(...arguments);
            this.tu = Value_1._;
            this.ξ = Value_1._;
            this.t = Value_1._;
        }
    }
    Expl_1.MatchAs = MatchAs;
    function matchAs(tu, ξ, t) {
        return Versioned_1.at(MatchAs, tu, ξ, t);
    }
    Expl_1.matchAs = matchAs;
    class Quote extends Terminal {
    }
    Expl_1.Quote = Quote;
    function quote() {
        return Versioned_1.at(Quote);
    }
    Expl_1.quote = quote;
    class Typematch extends NonTerminal {
        constructor() {
            super(...arguments);
            this.tu = Value_1._;
            this.d = Value_1._;
            this.t = Value_1._;
        }
    }
    Expl_1.Typematch = Typematch;
    function typematch(tu, d, t) {
        return Versioned_1.at(Typematch, tu, d, t);
    }
    Expl_1.typematch = typematch;
    class Var extends NonTerminal {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
            this.t = Value_1._;
        }
    }
    Expl_1.Var = Var;
    function var_(x, t) {
        return Versioned_1.at(Var, x, t);
    }
    Expl_1.var_ = var_;
    // Should probably do a better job of restricting k to be a bona fide field name.
    function explChild(t, v, prop) {
        if (t instanceof Terminal) {
            Core_1.assert(t instanceof DataExpl);
            return DataValue_1.explValue(t.__child(prop), v.__child(prop));
        }
        else if (t instanceof NonTerminal) {
            return explChild(t.t, v, prop);
        }
        else {
            // Primitive applications are currently "terminal" forms, which is technically inconsistent with the fact 
            // that they can return structured data. In practice this doesn't matter because they only return values 
            // like True and False, which have no children. Probably primitives should be non-terminal.
            return Core_1.absurd();
        }
    }
    Expl_1.explChild = explChild;
    function explChildren(t, v) {
        return Value_1.fields(v).map(k => explChild(t, v, k));
    }
    Expl_1.explChildren = explChildren;
})(Expl = exports.Expl || (exports.Expl = {}));


/***/ }),
/* 6 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const Set_1 = __webpack_require__(21);
const BaseTypes_1 = __webpack_require__(4);
const DataValue_1 = __webpack_require__(3);
const Match_1 = __webpack_require__(9);
const Value_1 = __webpack_require__(1);
const Versioned_1 = __webpack_require__(2);
// Constants used for parsing, and also for toString() implementations.
exports.strings = {
    arrow: "→",
    as: "as",
    bracketL: "[",
    bracketR: "]",
    comma: ",",
    curlyL: "{",
    curlyR: "}",
    ellipsis: "...",
    equals: "=",
    fun: "fun",
    in_: "in",
    let_: "let",
    letRec: "letrec",
    match: "match",
    primitive: "primitive",
    parenL: "(",
    parenR: ")",
    quotes: '"',
    semicolon: ";",
    typematch: "typematch"
};
var Expr;
(function (Expr_1) {
    class SyntaxNode extends DataValue_1.DataValue {
    }
    Expr_1.SyntaxNode = SyntaxNode;
    class Expr extends SyntaxNode {
    }
    Expr_1.Expr = Expr;
    class App extends Expr {
        constructor() {
            super(...arguments);
            this.f = Value_1._;
            this.e = Value_1._;
        }
    }
    Expr_1.App = App;
    function app(f, e) {
        return Versioned_1.at(App, f, e);
    }
    Expr_1.app = app;
    class BinaryApp extends Expr {
        constructor() {
            super(...arguments);
            this.e1 = Value_1._;
            this.opName = Value_1._;
            this.e2 = Value_1._;
        }
    }
    Expr_1.BinaryApp = BinaryApp;
    function binaryApp(e1, opName, e2) {
        return Versioned_1.at(BinaryApp, e1, opName, e2);
    }
    Expr_1.binaryApp = binaryApp;
    class ConstNum extends Expr {
        constructor() {
            super(...arguments);
            this.val = Value_1._;
        }
    }
    Expr_1.ConstNum = ConstNum;
    function constNum(val) {
        return Versioned_1.at(ConstNum, val);
    }
    Expr_1.constNum = constNum;
    class ConstStr extends Expr {
        constructor() {
            super(...arguments);
            this.val = Value_1._;
        }
    }
    Expr_1.ConstStr = ConstStr;
    function constStr(val) {
        return Versioned_1.at(ConstStr, val);
    }
    Expr_1.constStr = constStr;
    // Has a concrete subclass for each datatype. Should this be parameterised by T extends DataValue?
    class DataExpr extends Expr {
        get ctr() {
            return Core_1.className(this);
        }
        __child(prop) {
            return super.__child(prop);
        }
        get __children() {
            return super.__children;
        }
    }
    Expr_1.DataExpr = DataExpr;
    class Def extends SyntaxNode {
    }
    Expr_1.Def = Def;
    class Let extends Def {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
            this.e = Value_1._;
        }
    }
    Expr_1.Let = Let;
    function let_(x, e) {
        return Versioned_1.at(Let, x, e);
    }
    Expr_1.let_ = let_;
    class Prim extends Def {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
        }
    }
    Expr_1.Prim = Prim;
    function prim(x) {
        return Versioned_1.at(Prim, x);
    }
    Expr_1.prim = prim;
    class RecDef extends SyntaxNode {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
            this.σ = Value_1._;
        }
    }
    Expr_1.RecDef = RecDef;
    function recDef(x, σ) {
        return Versioned_1.at(RecDef, x, σ);
    }
    Expr_1.recDef = recDef;
    class LetRec extends Def {
        constructor() {
            super(...arguments);
            this.δ = Value_1._;
        }
    }
    Expr_1.LetRec = LetRec;
    function letRec(δ) {
        return Versioned_1.at(LetRec, δ);
    }
    Expr_1.letRec = letRec;
    class Defs extends Expr {
        constructor() {
            super(...arguments);
            this.def̅ = Value_1._;
            this.e = Value_1._;
        }
    }
    Expr_1.Defs = Defs;
    function defs(def̅, e) {
        return Versioned_1.at(Defs, def̅, e);
    }
    Expr_1.defs = defs;
    class Fun extends Expr {
        constructor() {
            super(...arguments);
            this.σ = Value_1._;
        }
    }
    Expr_1.Fun = Fun;
    function fun(σ) {
        return Versioned_1.at(Fun, σ);
    }
    Expr_1.fun = fun;
    class MatchAs extends Expr {
        constructor() {
            super(...arguments);
            this.e = Value_1._;
            this.σ = Value_1._;
        }
    }
    Expr_1.MatchAs = MatchAs;
    function matchAs(e, σ) {
        return Versioned_1.at(MatchAs, e, σ);
    }
    Expr_1.matchAs = matchAs;
    class Quote extends Expr {
        constructor() {
            super(...arguments);
            this.e = Value_1._;
        }
    }
    Expr_1.Quote = Quote;
    function quote(e) {
        return Versioned_1.at(Quote, e);
    }
    Expr_1.quote = quote;
    // Bring in line with the current eliminator design, i.e. optimise into an object?
    class Typematch extends Expr {
        constructor() {
            super(...arguments);
            this.e = Value_1._;
            this.cases = Value_1._;
        }
    }
    Expr_1.Typematch = Typematch;
    function typematch(e, cases) {
        return Versioned_1.at(Typematch, e, cases);
    }
    Expr_1.typematch = typematch;
    class Var extends Expr {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
        }
    }
    Expr_1.Var = Var;
    function var_(x) {
        return Versioned_1.at(Var, x);
    }
    Expr_1.var_ = var_;
    // used by Wrattler
    function freeVars(e) {
        if (e instanceof ConstNum) {
            return new Set();
        }
        else if (e instanceof ConstStr) {
            return new Set();
        }
        else if (e instanceof Fun) {
            return freeVarsElim(e.σ);
        }
        else if (e instanceof DataExpr) {
            return Set_1.union(...e.__children.map(freeVars));
        }
        else if (e instanceof Quote) {
            return freeVars(e.e);
        }
        else if (e instanceof Var) {
            return new Set([e.x.val]);
        }
        else if (e instanceof App) {
            return Set_1.union(freeVars(e.f), freeVars(e.e));
        }
        else if (e instanceof BinaryApp) {
            return Set_1.union(freeVars(e.e1), freeVars(e.e2));
        }
        else if (e instanceof Defs) {
            const [bound, free] = freeVarsDefs(e.def̅, new Set());
            return Set_1.union(Set_1.diff(freeVars(e.e), bound), free);
        }
        else if (e instanceof MatchAs) {
            return Set_1.union(freeVars(e.e), freeVarsElim(e.σ));
        }
        else if (e instanceof Typematch) {
            return Set_1.union(freeVars(e.e), ...e.cases.toArray().map(({ snd }) => freeVars(snd)));
        }
        else {
            return Core_1.absurd();
        }
    }
    Expr_1.freeVars = freeVars;
    function freeVarsCont(κ) {
        if (κ instanceof Expr) {
            return freeVars(κ);
        }
        else if (κ instanceof Match_1.Elim) {
            return freeVarsElim(κ);
        }
        else {
            return Core_1.absurd();
        }
    }
    function freeVarsElim(σ) {
        if (Match_1.VarElim.is(σ)) {
            return Set_1.diff(freeVarsCont(σ.κ), new Set([σ.x.val]));
        }
        else if (Match_1.DataElim.is(σ)) {
            return Set_1.union(...σ.__children.map(freeVarsCont));
        }
        else {
            return Core_1.absurd();
        }
    }
    // (bound, free) vars - not necessarily disjoint, because the defs nest
    function freeVarsDefs(def̅, bound) {
        if (BaseTypes_1.Cons.is(def̅)) {
            const def = def̅.head;
            if (def instanceof Prim) {
                const x̅ = new Set([def.x.val]), [boundʹ, free] = freeVarsDefs(def̅.tail, bound);
                return [boundʹ, Set_1.diff(free, x̅)];
            }
            else if (def instanceof Let) {
                const x̅ = new Set([def.x.val]), [boundʹ, free] = freeVarsDefs(def̅.tail, Set_1.union(bound, x̅));
                return [boundʹ, Set_1.union(Set_1.diff(free, x̅), freeVars(def.e))];
            }
            else if (def instanceof LetRec) {
                const f̅ = def.δ.toArray(), x̅ = new Set(f̅.map(f => f.x.val)), [boundʹ, free] = freeVarsDefs(def̅.tail, Set_1.union(bound, x̅));
                return [boundʹ, Set_1.diff(Set_1.union(free, ...f̅.map(f => freeVarsElim(f.σ))), x̅)];
            }
            else {
                return Core_1.absurd();
            }
        }
        else if (BaseTypes_1.Nil.is(def̅)) {
            return [bound, new Set()];
        }
        else {
            return Core_1.absurd();
        }
    }
})(Expr = exports.Expr || (exports.Expr = {}));


/***/ }),
/* 7 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const Expl_1 = __webpack_require__(5);
const Expr_1 = __webpack_require__(6);
const Match_1 = __webpack_require__(9);
const Value_1 = __webpack_require__(1);
const Versioned_1 = __webpack_require__(2);
class PrimType {
    constructor(name, C) {
        this.name = name;
        this.C = C;
    }
}
exports.PrimType = PrimType;
// Neither of these is currently reflective because of non-standard fields.
class DataType {
    constructor(name, elimC, ctrs, exprC̅, explC̅) {
        this.name = name;
        this.elimC = elimC;
        this.ctrs = ctrs;
        this.exprC̅ = exprC̅;
        this.explC̅ = explC̅;
    }
}
exports.DataType = DataType;
// Constructor of a datatype, not to be confused with an instance of such a thing (DataValue) or name of such a thing
// (Lex.Ctr). Fields have a total ordering given by the order of definition in the corresponding class.
class Ctr {
    constructor(C, f̅) {
        this.C = C;
        this.f̅ = f̅;
    }
    get arity() {
        return this.f̅.length;
    }
    get c() {
        return this.C.name;
    }
}
exports.Ctr = Ctr;
function ctrFor(c) {
    return Core_1.__nonNull(exports.ctrToDataType.get(c), `Unknown constructor ${c}.`).ctrs.get(c);
}
exports.ctrFor = ctrFor;
function explClass(C) {
    return Core_1.__nonNull(exports.ctrToDataType.get(C.name)).explC̅.get(C.name);
}
exports.explClass = explClass;
function exprClass(C) {
    return Core_1.__nonNull(exports.ctrToDataType.get(C.name)).exprC̅.get(C.name);
}
exports.exprClass = exprClass;
function valueClass(C) {
    return ctrFor(C.name).C;
}
exports.valueClass = valueClass;
// Populated by initDataTypes(). Constructors are not yet first-class.
exports.types = new Map;
exports.ctrToDataType = new Map;
exports.elimToDataType = new Map;
// See https://stackoverflow.com/questions/33605775 for the dynamic class-naming idiom.
function initDataType(D, C̅) {
    C̅.sort((C, Cʹ) => C.name.localeCompare(Cʹ.name)); // consistent with Str.leq
    const ctrs = C̅.map((C) => [C.name, new Ctr(C, Value_1.fields(new C))]), elimC = {
        [D.name]: class extends Match_1.DataElim {
            constructor() {
                super();
                // lexicographical order hopefully preserved by getOwnPropertyNames()
                C̅.forEach((C) => {
                    this[C.name] = Value_1._;
                });
            }
        }
    }[D.name], exprC̅ = ctrs.map(([c_str, c]) => {
        return [c_str, {
                [c_str]: class extends Expr_1.Expr.DataExpr {
                    constructor() {
                        super();
                        c.f̅.forEach((f) => {
                            this[f] = Value_1._;
                        });
                    }
                }
            }[c_str]];
    }), explC̅ = ctrs.map(([c_str, c]) => {
        return [c_str, {
                [c_str]: class extends Expl_1.Expl.DataExpl {
                    constructor() {
                        super();
                        c.f̅.forEach((f) => {
                            this[f] = Value_1._;
                        });
                    }
                }
            }[c_str]];
    }), d = new DataType(Versioned_1.str(D.name)(Versioned_1.ν()), elimC, new Map(ctrs), new Map(exprC̅), new Map(explC̅));
    C̅.forEach((C) => {
        exports.ctrToDataType.set(C.name, d);
    });
    exports.elimToDataType.set(D.name, d);
    exports.types.set(d.name.val, d);
}
exports.initDataType = initDataType;
exports.types.set(Value_1.Num.name, new PrimType(Versioned_1.str(Value_1.Num.name)(Versioned_1.ν()), Value_1.Num));
exports.types.set(Value_1.Str.name, new PrimType(Versioned_1.str(Value_1.Str.name)(Versioned_1.ν()), Value_1.Str));


/***/ }),
/* 8 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const DataType_1 = __webpack_require__(7);
const DataValue_1 = __webpack_require__(3);
const Value_1 = __webpack_require__(1);
class Point extends DataValue_1.DataValue {
    constructor() {
        super(...arguments);
        this.x = Value_1._;
        this.y = Value_1._;
    }
}
exports.Point = Point;
// Isomorphic to Bool
class Orient extends DataValue_1.DataValue {
}
exports.Orient = Orient;
class Horiz extends Orient {
}
exports.Horiz = Horiz;
class Vert extends Orient {
}
exports.Vert = Vert;
class GraphicsElement extends DataValue_1.DataValue {
}
exports.GraphicsElement = GraphicsElement;
class Circle extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.x = Value_1._;
        this.y = Value_1._;
        this.radius = Value_1._;
        this.fill = Value_1._;
    }
}
exports.Circle = Circle;
class Group extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.gs = Value_1._;
    }
}
exports.Group = Group;
class Line extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.p1 = Value_1._;
        this.p2 = Value_1._;
        this.stroke = Value_1._;
        this.strokeWidth = Value_1._;
    }
}
exports.Line = Line;
class Polyline extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.points = Value_1._;
        this.stroke = Value_1._;
        this.strokeWidth = Value_1._;
    }
}
exports.Polyline = Polyline;
class Polymarkers extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.points = Value_1._;
        this.markers = Value_1._;
    }
}
exports.Polymarkers = Polymarkers;
class Rect extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.x = Value_1._;
        this.y = Value_1._;
        this.width = Value_1._;
        this.height = Value_1._;
        this.fill = Value_1._;
    }
}
exports.Rect = Rect;
class Text extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.x = Value_1._;
        this.y = Value_1._;
        this.str = Value_1._;
        this.anchor = Value_1._; // SVG text-anchor
        this.baseline = Value_1._; // SVG alignment-baseline
    }
}
exports.Text = Text;
class Viewport extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.x = Value_1._;
        this.y = Value_1._;
        this.width = Value_1._;
        this.height = Value_1._;
        this.fill = Value_1._;
        this.margin = Value_1._; // in *parent* reference frame
        this.scale = Value_1._;
        this.translate = Value_1._; // scaling applies to translated coordinates
        this.g = Value_1._;
    }
}
exports.Viewport = Viewport;
class Transform extends DataValue_1.DataValue {
}
exports.Transform = Transform;
class Scale extends Transform {
    constructor() {
        super(...arguments);
        this.x = Value_1._;
        this.y = Value_1._;
    }
}
exports.Scale = Scale;
class Translate extends Transform {
    constructor() {
        super(...arguments);
        this.x = Value_1._;
        this.y = Value_1._;
    }
}
exports.Translate = Translate;
class Marker extends DataValue_1.DataValue {
}
exports.Marker = Marker;
class Arrowhead extends Marker {
}
exports.Arrowhead = Arrowhead;
DataType_1.initDataType(Point, [Point]);
DataType_1.initDataType(Orient, [Horiz, Vert]);
DataType_1.initDataType(GraphicsElement, [Circle, Group, Line, Polyline, Polymarkers, Rect, Text, Viewport]);
DataType_1.initDataType(Transform, [Scale, Translate]);
DataType_1.initDataType(Marker, [Arrowhead]);


/***/ }),
/* 9 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Array_1 = __webpack_require__(11);
const Core_1 = __webpack_require__(0);
const Ord_1 = __webpack_require__(31);
const Lattice_1 = __webpack_require__(12);
const Annotation_1 = __webpack_require__(10);
const BaseTypes_1 = __webpack_require__(4);
const DataValue_1 = __webpack_require__(3);
const DataType_1 = __webpack_require__(7);
const Env_1 = __webpack_require__(14);
const Expl_1 = __webpack_require__(5);
const Expr_1 = __webpack_require__(6);
const Value_1 = __webpack_require__(1);
const Versioned_1 = __webpack_require__(2);
// Unrelated to the annotation lattice. Expr case intentionally only defined for higher-order (function) case.
function join(κ, κʹ) {
    const k = Value_1.memoId(join, arguments);
    if (κ instanceof Elim && κʹ instanceof Elim) {
        return DataElim.join(κ, κʹ);
    }
    else if (κ instanceof Expr_1.Expr.Fun && κʹ instanceof Expr_1.Expr.Fun) {
        return Expr_1.Expr.fun(join(κ.σ, κʹ.σ))(k);
    }
    else {
        return Core_1.absurd("Undefined join.", κ, κʹ);
    }
}
class Match extends DataValue_1.DataValue {
    constructor() {
        super(...arguments);
        this.tv̅ = Value_1._;
        this.κ = Value_1._;
    }
}
exports.Match = Match;
function match(ξ, κ) {
    return Value_1.make(Match, ξ, κ);
}
exports.match = match;
// See GitHub issue #128.
class Elim extends DataValue_1.DataValue {
    // could have called this "match", but conflicts with factory method of same name
    apply(tv) {
        return apply_(this, tv, BaseTypes_1.nil());
    }
}
exports.Elim = Elim;
function apply_(σ, tv, u̅) {
    if (VarElim.is(σ)) {
        const ρ = σ.x.val === "_" ? Env_1.emptyEnv() : Env_1.Env.singleton(σ.x, tv);
        return [ρ, match(u̅, σ.κ)];
    }
    else if (DataElim.is(σ)) {
        const v = tv.v, c = Core_1.className(v);
        if (v instanceof DataValue_1.DataValue) {
            const κ = σ[c];
            if (κ !== undefined) {
                const tv̅ = Expl_1.Expl.explChildren(tv.t, v), [ρ, ξ] = matchArgs(κ, tv̅, u̅);
                return [ρ, match(BaseTypes_1.cons(tv, ξ.tv̅), ξ.κ)];
            }
            else {
                const d = DataType_1.elimToDataType.get(Core_1.className(σ));
                if (d.ctrs.has(c)) {
                    return Core_1.userError(`Pattern mismatch: "${c}" case undefined for ${d.name.val}.`);
                }
                else {
                    return Core_1.userError(`Pattern mismatch: found ${c}, expected ${d.name.val}.`);
                }
            }
        }
        else {
            return Core_1.userError(`Pattern mismatch: ${c} is not a datatype.`, v, σ);
        }
    }
    else {
        return Core_1.absurd();
    }
}
// Parser ensures constructor calls are saturated.
function matchArgs(κ, tv̅, u̅) {
    if (tv̅.length === 0) {
        return [Env_1.emptyEnv(), match(u̅, κ)];
    }
    else {
        const [tv, ...tv̅ʹ] = tv̅;
        if (κ instanceof Elim) {
            const σ = κ, // "unfold" K into Elim<K>
            [ρ, ξ] = apply_(σ, tv, u̅), [ρʹ, ξʹ] = matchArgs(ξ.κ, tv̅ʹ, ξ.tv̅);
            return [ρ.concat(ρʹ), ξʹ];
        }
        else {
            return Core_1.absurd("Too many arguments to constructor.");
        }
    }
}
// Concrete instances have a field per constructor, in *lexicographical* order.
class DataElim extends Elim {
    static is(σ) {
        return σ instanceof DataElim;
    }
    static join(σ, τ) {
        const k = Value_1.memoId(DataElim.join, arguments);
        if (VarElim.is(σ) && VarElim.is(τ) && Ord_1.eq(σ.x, τ.x)) {
            return varElim(σ.x, join(σ.κ, τ.κ))(k);
        }
        else if (DataElim.is(σ) && DataElim.is(τ)) {
            // Both maps (which are non-empty) can (inductively) be assumed to have keys taken from the 
            // same datatype. Ensure that invariant is preserved:
            const c_σ = Value_1.fields(σ)[0], c_τ = Value_1.fields(τ)[0];
            if (DataType_1.ctrToDataType.get(c_σ) !== DataType_1.ctrToDataType.get(c_τ)) {
                Core_1.userError(`${c_σ} and ${c_τ} are constructors of different datatypes.`);
            }
            const cκ̅1 = Array_1.zip(Value_1.fields(σ), σ.__children), cκ̅2 = Array_1.zip(Value_1.fields(τ), τ.__children);
            Core_1.assert(cκ̅1.length === cκ̅2.length);
            const cκ̅ = Array_1.zipWith(([c1, κ1], [c2, κ2]) => {
                Core_1.assert(c1 === c2);
                return [c1, κ1 === undefined ? κ2 : (κ2 === undefined ? κ1 : join(κ1, κ2))];
            })(cκ̅1, cκ̅2);
            return dataElim(...cκ̅)(k);
        }
        else {
            return Core_1.absurd("Undefined join.", σ, τ);
        }
    }
}
exports.DataElim = DataElim;
// cκ̅ non-empty and constructors all of the same datatype.
function dataElim(...cκ̅) {
    const d = Core_1.__nonNull(DataType_1.ctrToDataType.get(cκ̅[0][0])), c̅ = cκ̅.map((([c, _]) => c)), c̅ʹ = [...d.ctrs.keys()], // sorted
    f̅ = [];
    let n = 0;
    for (let nʹ = 0; nʹ < c̅ʹ.length; ++nʹ) {
        if (c̅.includes(c̅ʹ[nʹ])) {
            f̅.push(cκ̅[n++][1]);
        }
        else {
            f̅.push(undefined);
        }
    }
    return Versioned_1.at(d.elimC, ...f̅);
}
exports.dataElim = dataElim;
class VarElim extends Elim {
    constructor() {
        super(...arguments);
        this.x = Value_1._;
        this.κ = Value_1._;
    }
    static is(σ) {
        return σ instanceof VarElim;
    }
}
exports.VarElim = VarElim;
function varElim(x, κ) {
    return Versioned_1.at(VarElim, x, κ);
}
exports.varElim = varElim;
function apply_fwd(ξ) {
    return ξ.tv̅.toArray().reduce((α, tv) => Lattice_1.bool_.meet(α, Annotation_1.isα(tv)), Lattice_1.bool_.top);
}
exports.apply_fwd = apply_fwd;
function apply_bwd(ξ, α) {
    ξ.tv̅.toArray().forEach((tv) => Annotation_1.setjoinα(α, tv));
}
exports.apply_bwd = apply_bwd;


/***/ }),
/* 10 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const Lattice_1 = __webpack_require__(12);
const Set_1 = __webpack_require__(21);
const DataValue_1 = __webpack_require__(3);
const Expl_1 = __webpack_require__(5);
const Expr_1 = __webpack_require__(6);
function annotated(v) {
    return v instanceof Expr_1.Expr.SyntaxNode || v instanceof DataValue_1.ExplValue;
}
exports.annotated = annotated;
function isα(v) {
    return exports.__slice.is(v);
}
exports.isα = isα;
// Currently no deltas are associated with annotations.
function setα(α, v) {
    exports.__slice.set(v, α);
}
exports.setα = setα;
function setjoinα(α, v) {
    setα(Lattice_1.bool_.join(α, isα(v)), v);
}
exports.setjoinα = setjoinα;
function setmeetα(α, v) {
    setα(Lattice_1.bool_.meet(α, isα(v)), v);
}
exports.setmeetα = setmeetα;
var Direction;
(function (Direction) {
    Direction[Direction["Fwd"] = 0] = "Fwd";
    Direction[Direction["Bwd"] = 1] = "Bwd";
})(Direction = exports.Direction || (exports.Direction = {}));
class Annotations {
    constructor() {
        this.ann = new Set(); // unavailable nodes (fwd) or needed nodes (bwd)
        this.direction = Direction.Fwd;
    }
    // Whether v is needed (going backward) or available (going forward).
    is(v) {
        if (this.direction === Direction.Fwd) {
            return Lattice_1.bool_.negate(this.ann.has(v));
        }
        else {
            return this.ann.has(v);
        }
    }
    // Going forward, annotation updates must be decreasing; going backward, increasing. This is because 
    // forward slicing propagates non-availability, whereas backward slicing propagates demand.
    set(v, α) {
        const current = this.is(v);
        if (this.direction === Direction.Fwd && α < current ||
            this.direction === Direction.Bwd && α > current) {
            this.ann.add(v);
        }
        else if (this.direction === Direction.Fwd && α > current ||
            this.direction === Direction.Bwd && α < current) {
            Core_1.absurd(`Incompatible update of annotation from ${current} to ${α}.`, current, α);
        }
        else {
            // idempotent
        }
    }
    reset(direction) {
        this.direction = direction;
        this.ann = new Set();
    }
    restrictTo(tvs) {
        return Set_1.intersection(this.ann, Set_1.union(...tvs.map(tv => explDescendants(tv))));
    }
}
exports.Annotations = Annotations;
function explDescendants(tv) {
    const desc = new Set();
    explDescendants_aux(tv, desc);
    return desc;
}
function explDescendants_aux(tv, desc) {
    desc.add(tv);
    if (tv.v instanceof DataValue_1.DataValue) {
        const { t, v } = tv;
        Expl_1.Expl.explChildren(t, v).forEach((tv) => {
            explDescendants_aux(tv, desc);
        });
    }
}
exports.__slice = new Annotations();


/***/ }),
/* 11 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
function flatten(x̅̅) {
    const x̅ = []; // otherwise TS is confused
    return x̅.concat.apply([], x̅̅);
}
exports.flatten = flatten;
function counts(x̅) {
    const counts = new Map;
    x̅.forEach(x => {
        if (counts.has(x)) {
            counts.set(x, Core_1.__nonNull(counts.get(x)) + 1);
        }
        else {
            counts.set(x, 1);
        }
    });
    return counts;
}
exports.counts = counts;
function zip(x̅, y̅) {
    return zipWith((t, u) => [t, u])(x̅, y̅);
}
exports.zip = zip;
function zipWith(f) {
    return (x̅, y̅) => x̅.map((x, n) => f(x, y̅[n]));
}
exports.zipWith = zipWith;
function includes(x̅, y̅) {
    return y̅.every(y => x̅.includes(y));
}
exports.includes = includes;
function eq(x̅, y̅) {
    let n = x̅.length;
    if (n != y̅.length) {
        return false;
    }
    else {
        while (n--) {
            if (x̅[n] !== y̅[n])
                return false;
        }
        return true;
    }
}
exports.eq = eq;
function nth(x̅, n) {
    Core_1.assert(0 <= n && n < x̅.length);
    return x̅[n];
}
exports.nth = nth;
function last(x̅) {
    Core_1.assert(x̅.length > 0);
    return x̅.slice(-1)[0];
}
exports.last = last;


/***/ }),
/* 12 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
// Actually a boolean lattice...
class LatticeImpl {
    join(...t̅) {
        return t̅.reduce((t1, t2) => this.join2(t1, t2));
    }
    meet(...t̅) {
        return t̅.reduce((t1, t2) => this.meet2(t1, t2));
    }
}
class BoolLattice extends LatticeImpl {
    constructor() {
        super(...arguments);
        this.bot = false;
        this.top = true;
    }
    // Important to assert that arguments are defined since undefined propagates in an unhelpful way.
    join2(b1, b2) {
        return Core_1.__nonNull(b1) || Core_1.__nonNull(b2);
    }
    meet2(b1, b2) {
        return Core_1.__nonNull(b1) && Core_1.__nonNull(b2);
    }
    negate(b) {
        return !b;
    }
}
exports.BoolLattice = BoolLattice;
exports.bool_ = new BoolLattice();


/***/ }),
/* 13 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const Value_1 = __webpack_require__(1);
function leq(s1, s2) {
    return Object.keys(s1).every((prop) => {
        return s2.hasOwnProperty(prop) && s1[prop].before === s2[prop].before && s1[prop].after === s2[prop].after;
    });
}
exports.leq = leq;
function empty(ẟ) {
    return Value_1.fields(ẟ).length === 0;
}
class Deltas {
    constructor() {
        this.ẟ̅ = new Map();
    }
    get size() {
        return this.ẟ̅.size;
    }
    // Change sets must be disjoint at a given revision. Because of sharing within a revision, 
    // a node may first appear new (or reclassified) and then later appear changed, but the 
    // subsequent change sets must be empty.
    changed(v, s_ẟ) {
        let v_ẟ = this.ẟ̅.get(v);
        if (v_ẟ === undefined) {
            this.ẟ̅.set(v, new Change(s_ẟ));
        }
        else if (v_ẟ instanceof Change) {
            Value_1.mergeInto(v_ẟ.changed, s_ẟ);
        }
        else if (v_ẟ instanceof New || v_ẟ instanceof Reclassify) {
            Core_1.assert(empty(s_ẟ));
        }
        else {
            Core_1.absurd();
        }
    }
    // A value cannot be reclassified twice at the same revision.
    reclassified(v) {
        let v_ẟ = this.ẟ̅.get(v);
        if (v_ẟ === undefined) {
            this.ẟ̅.set(v, new Reclassify());
        }
        else {
            Core_1.absurd();
        }
    }
    // A value cannot be created twice at the same revision.
    created(v) {
        let v_ẟ = this.ẟ̅.get(v);
        if (v_ẟ === undefined) {
            this.ẟ̅.set(v, new New());
        }
        else {
            Core_1.absurd();
        }
    }
    clear() {
        this.ẟ̅.clear();
    }
}
exports.Deltas = Deltas;
exports.__deltas = new Deltas();
class Delta {
    eq(ẟ) {
        return this.leq(ẟ) && ẟ.leq(this);
    }
}
exports.Delta = Delta;
class New extends Delta {
    constructor() {
        super();
    }
    leq(ẟ) {
        return ẟ instanceof New;
    }
}
exports.New = New;
class Change extends Delta {
    constructor(changed) {
        super();
        this.changed = changed;
    }
    leq(ẟ) {
        return ẟ instanceof Change && leq(this.changed, ẟ.changed);
    }
    hasChanged(prop) {
        return Value_1.fields(this.changed).includes(prop);
    }
}
exports.Change = Change;
// Constructor has changed, and therefore fields may not align. More sophisticated reclassification
// delta could allow for fields to be shared when an object changes class.
class Reclassify extends Delta {
    constructor() {
        super();
    }
    leq(ẟ) {
        return ẟ instanceof Reclassify;
    }
}
exports.Reclassify = Reclassify;


/***/ }),
/* 14 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const DataValue_1 = __webpack_require__(3);
const Value_1 = __webpack_require__(1);
// Environments are snoc lists; the values are "explained" because usage information is recorded on traces.
class Env extends DataValue_1.DataValue {
    get(k) {
        if (this instanceof EmptyEnv) {
            return undefined;
        }
        else if (this instanceof ExtendEnv) {
            if (this.k.val === k.val) {
                return this.tv;
            }
            else {
                return this.ρ.get(k);
            }
        }
        else {
            return Core_1.absurd();
        }
    }
    has(k) {
        return this.get(k) !== undefined;
    }
    static singleton(k, tv) {
        return extendEnv(emptyEnv(), k, tv);
    }
    concat(ρ) {
        if (ρ instanceof EmptyEnv) {
            return this;
        }
        else if (ρ instanceof ExtendEnv) {
            return extendEnv(this.concat(ρ.ρ), ρ.k, ρ.tv);
        }
        else {
            return Core_1.absurd();
        }
    }
    values() {
        const tvs = [];
        for (let ρ = this; ρ instanceof ExtendEnv; ρ = ρ.ρ) {
            tvs.push(ρ.tv);
        }
        return tvs;
    }
}
exports.Env = Env;
class EmptyEnv extends Env {
}
exports.EmptyEnv = EmptyEnv;
function emptyEnv() {
    return Value_1.make(EmptyEnv);
}
exports.emptyEnv = emptyEnv;
class ExtendEnv extends Env {
    constructor() {
        super(...arguments);
        this.ρ = Value_1._;
        this.k = Value_1._;
        this.tv = Value_1._;
    }
}
exports.ExtendEnv = ExtendEnv;
function extendEnv(ρ, k, tv) {
    return Value_1.make(ExtendEnv, ρ, k, tv);
}
exports.extendEnv = extendEnv;


/***/ }),
/* 15 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const Delta_1 = __webpack_require__(13);
const Expr_1 = __webpack_require__(6);
const Graphics_1 = __webpack_require__(8);
const Value_1 = __webpack_require__(1);
const Versioned_1 = __webpack_require__(2);
const Core_2 = __webpack_require__(26);
__webpack_require__(27);
exports.svg = new Core_2.SVG();
const fontSize = 18;
const classes = "code";
// bizarrely, if I do this later, font metrics are borked:
const lineHeight = exports.svg.textHeight(textElement(fontSize, classes, "m")); // representative character 
// ASCII spaces seem to be trimmed; only Unicode space that seems to render monospaced is this: 
const space_char = "\u00a0";
const shapeRendering = "geometricPrecision";
exports.__dimensions = new Map();
function arrow(ẟ_style) {
    return keyword("arrow", ẟ_style);
}
exports.arrow = arrow;
function border(x, y, width, height, stroke, dashed) {
    const b = rect(x, y, width, height, stroke, "none", border);
    b.setAttribute("stroke-width", "0.5");
    if (dashed) {
        b.setAttribute("stroke-dasharray", "1,1");
    }
    return b;
}
exports.border = border;
function addBorder_changed(g) {
    const { width, height } = Core_1.__nonNull(exports.__dimensions.get(g));
    const b = border(g.x.baseVal.value, g.y.baseVal.value, width, height, "blue", true);
    g.appendChild(b);
    return g;
}
exports.addBorder_changed = addBorder_changed;
function addBorder_focus(g) {
    const { width, height } = Core_1.__nonNull(exports.__dimensions.get(g));
    const b = border(g.x.baseVal.value, g.y.baseVal.value, width, height, "gray", false);
    g.appendChild(b);
    return g;
}
exports.addBorder_focus = addBorder_focus;
function bracket(gs, ẟ_style) {
    return horiz(keyword("bracketL", ẟ_style), ...gs, keyword("bracketR", ẟ_style));
}
exports.bracket = bracket;
function centreDot(ẟ_style) {
    return text("•", ẟ_style);
}
exports.centreDot = centreDot;
function comma(ẟ_style) {
    return keyword("comma", ẟ_style);
}
exports.comma = comma;
// Whether the centre of r1 is to the left of the centre of r2.
function leftOf(r1, r2) {
    return r1.x + r1.width / 2 <= r2.x + r2.width;
}
// TODO: remember what this is for.
function blah(x, length, proportion) {
    return x + proportion * length;
}
// Offset might be better computed as a function of distance between p1 and p2.
function curvedLine(p1, p2, offset) {
    const mp = { x: (p2.x + p1.x) * 0.5, y: (p2.y + p1.y) * 0.5 };
    // angle of perpendicular to line
    const theta = Math.atan2(p2.y - p1.y, p2.x - p1.x) - Math.PI / 2;
    const control = { x: mp.x + offset * Math.cos(theta), y: mp.y + offset * Math.sin(theta) };
    return `M ${p1.x} ${p1.y} Q ${control.x} ${control.y} ${p2.x} ${p2.y}`;
}
// Factor all element creation through this so we can tag with extra metadata.
function createElement(name, createdBy) {
    const e = document.createElementNS(Core_2.SVG.NS, name);
    e.setAttribute("data-created-by", createdBy.name);
    return e;
}
function connector(g1, g2) {
    const g1_ = dims(g1);
    const g2_ = dims(g2);
    const [fromBottom, fromTop] = [0.1, 0.9];
    const connector_ = createElement("path", connector);
    const curveOffset = 5; // somewhat arbitrary
    if (leftOf(g1_, g2_)) {
        connector_.setAttribute("d", curvedLine({ x: g1_.x + g1_.width, y: blah(g1_.y, g1_.height, fromBottom) }, { x: g2_.x, y: blah(g2_.y, g2_.height, fromBottom) }, curveOffset));
    }
    else {
        connector_.setAttribute("d", curvedLine({ x: g1_.x, y: blah(g1_.y, g1_.height, fromTop) }, { x: g2_.x + g2_.width, y: blah(g2_.y, g2_.height, fromTop) }, curveOffset));
    }
    connector_.setAttribute("shape-rendering", shapeRendering);
    connector_.setAttribute("fill", "none");
    connector_.setAttribute("stroke", "blue"); // hardcoded
    connector_.setAttribute("stroke-width", "1");
    connector_.setAttribute("stroke-dasharray", "1,1");
    connector_.setAttribute("marker-end", "url(#Arrowhead-blue)"); // extract to helper function
    return connector_;
}
exports.connector = connector;
// Couldn't get getScreenCTM or getBoundingClientRect to work properly (perhaps because of nested SVGs?) so just use this to compute 
// coordinates of g relative to root SVG.
function coordinates(g) {
    if (g instanceof SVGSVGElement) {
        const { x, y } = g.parentElement instanceof SVGSVGElement ? coordinates(g.parentElement) : { x: 0, y: 0 };
        return { x: x + g.x.baseVal.value, y: y + g.y.baseVal.value };
    }
    else {
        return { x: 0, y: 0 };
    }
}
function delimit(delimiter, ...gs) {
    const gsʹ = [];
    gs.forEach((g, n) => {
        gsʹ.push(g);
        if (n < gs.length - 1) {
            gsʹ.push(delimiter());
        }
    });
    return gsʹ;
}
exports.delimit = delimit;
function dims(g) {
    const { width, height } = Core_1.__nonNull(exports.__dimensions.get(g));
    const { x, y } = coordinates(g);
    return { x, y, width, height };
}
function edge_left(g) {
    const { height } = exports.__dimensions.get(g);
    const edge = line(g.x.baseVal.value, g.y.baseVal.value, g.x.baseVal.value, g.y.baseVal.value + height, "gray", 1);
    edge.setAttribute("stroke-width", "4");
    //   edge.setAttribute("stroke-dasharray", "2,2")
    g.appendChild(edge);
    return g;
}
exports.edge_left = edge_left;
function edge_bottom(g) {
    const { width, height } = exports.__dimensions.get(g);
    const edge = line(g.x.baseVal.value, g.y.baseVal.value + height, g.y.baseVal.value + width, g.y.baseVal.value + height, "gray", 1);
    edge.setAttribute("stroke-width", "2");
    //   edge.setAttribute("stroke-dasharray", "2,2")
    g.appendChild(edge);
    return g;
}
exports.edge_bottom = edge_bottom;
function ellipsis(ẟ_style) {
    return text("…", ẟ_style);
}
exports.ellipsis = ellipsis;
function group() {
    return createElement("g", group);
}
exports.group = group;
function horiz(...gs) {
    const g = createElement("svg", horiz);
    let width_sum = 0, height_max = 0;
    gs.forEach((gʹ) => {
        gʹ.setAttribute("x", `${width_sum}`);
        gʹ.setAttribute("y", `0`);
        const { width, height } = exports.__dimensions.get(gʹ);
        width_sum += width;
        height_max = Math.max(height_max, height);
        g.appendChild(gʹ);
    });
    exports.__dimensions.set(g, { width: width_sum, height: height_max });
    return g;
}
exports.horiz = horiz;
function horizSpace(...gs) {
    return horiz(...delimit(space, ...gs));
}
exports.horizSpace = horizSpace;
function keyword(str, ẟ_style) {
    return text(Expr_1.strings[str], ẟ_style);
}
exports.keyword = keyword;
function line(x1, y1, x2, y2, stroke, strokeWidth) {
    const l = createElement("line", line);
    l.setAttribute("shape-rendering", shapeRendering);
    l.setAttribute("x1", `${round(x1)}`);
    l.setAttribute("y1", `${round(y1)}`);
    l.setAttribute("x2", `${round(x2)}`);
    l.setAttribute("y2", `${round(y2)}`);
    l.setAttribute("stroke", stroke);
    l.setAttribute("stroke-width", `${round(strokeWidth)}`);
    return l;
}
exports.line = line;
function lineRounded(x1, y1, x2, y2, stroke, strokeWidth) {
    const l = line(x1, y1, x2, y2, stroke, strokeWidth);
    l.setAttribute("stroke-linecap", "round");
    return l;
}
exports.lineRounded = lineRounded;
function marker(C, colour) {
    const m = createElement("marker", marker);
    m.setAttribute("id", markerId(C, colour));
    m.setAttribute("orient", "auto");
    m.setAttribute("fill", colour);
    m.setAttribute("stroke", colour);
    return m;
}
exports.marker = marker;
function markerId(C, colour) {
    return `${C.name}-${colour}`;
}
let markerFactory;
{
    markerFactory = new Map();
    markerFactory.set(Graphics_1.Arrowhead.name, marker_arrowhead);
}
// Assume root has a unique defs element called "defs". Return composite marker id.
function markerEnsureDefined(root, C, colour) {
    const id = markerId(C, colour);
    let marker = root.getElementById(id);
    if (marker === null) {
        marker = Core_1.__nonNull(markerFactory.get(C.name))(colour);
        const defs = Core_1.as(root.getElementById("defs"), SVGDefsElement);
        defs.appendChild(marker);
        Core_1.assert(root.getElementById(id) === marker);
    }
    else {
        Core_1.assert(marker instanceof SVGMarkerElement);
    }
    return id;
}
exports.markerEnsureDefined = markerEnsureDefined;
function marker_arrowhead(colour) {
    const m = marker(Graphics_1.Arrowhead, colour);
    const length = 6, width = 4;
    m.setAttribute("refX", `${length}`);
    m.setAttribute("refY", `${width / 2}`);
    m.setAttribute("markerWidth", "16");
    m.setAttribute("markerHeight", "16");
    const path = createElement("path", marker_arrowhead);
    m.appendChild(path);
    path.setAttribute("shape-rendering", shapeRendering);
    path.setAttribute("d", `M ${length} ${width / 2} L 0 ${width} L 0 0 Z`);
    return m;
}
exports.marker_arrowhead = marker_arrowhead;
function circle(x, y, radius, stroke, fill, createdBy) {
    const r = createElement("circle", createdBy);
    r.setAttribute("shape-rendering", shapeRendering);
    r.setAttribute("cx", `${round(x)}`);
    r.setAttribute("cy", `${round(y)}`);
    r.setAttribute("r", `${round(radius)}`);
    r.setAttribute("stroke", stroke);
    r.setAttribute("fill", fill);
    return r;
}
exports.circle = circle;
function parenthesise(g, ẟ_style) {
    return horiz(keyword("parenL", ẟ_style), g, keyword("parenR", ẟ_style));
}
exports.parenthesise = parenthesise;
function parenthesiseIf(parens, g, ẟ_style) {
    return parens ? parenthesise(g, ẟ_style) : g;
}
exports.parenthesiseIf = parenthesiseIf;
// TODO: use Point consistently everywhere?
function pointsToString(p̅) {
    return p̅.map(([x, y]) => `${round(x)},${round(y)}`).join(" ");
}
function polyline(p̅, stroke, strokeWidth) {
    const l = createElement("polyline", polyline);
    l.setAttribute("shape-rendering", shapeRendering);
    l.setAttribute("points", pointsToString(p̅));
    l.setAttribute("stroke", stroke);
    l.setAttribute("stroke-width", `${round(strokeWidth)}`);
    l.setAttribute("stroke-linecap", "round");
    l.setAttribute("fill", "none");
    return l;
}
exports.polyline = polyline;
function rect(x, y, width, height, stroke, fill, createdBy) {
    const r = createElement("rect", createdBy);
    r.setAttribute("shape-rendering", shapeRendering);
    r.setAttribute("x", `${round(x)}`);
    r.setAttribute("y", `${round(y)}`);
    r.setAttribute("width", `${round(width)}`);
    r.setAttribute("height", `${round(height)}`);
    r.setAttribute("stroke", stroke);
    r.setAttribute("fill", fill);
    return r;
}
exports.rect = rect;
// Rounding to pixel boundaries (although often desirable for SVG, e.g. to get sharp lines) doesn't work well 
// for small shapes, but we don't need to maintain the full monstrosity that are floating-point numbers. Round 
// to an appropriate number of decimal places, cast to number to strip trailing zeros, and then cast back to string.
// This seems to be sufficient precision for SVG but is also human-friendly.
function round(n) {
    return (+n.toFixed(3)).toString();
}
exports.round = round;
// Needs to be at the bottom in the z-order, and opaque.
function shading(g, fill) {
    const svg = createElement("svg", shading);
    const { width, height } = exports.__dimensions.get(g);
    const background = rect(g.x.baseVal.value, g.y.baseVal.value, width, height, "none", fill, shading);
    background.setAttribute("pointer-events", "none");
    svg.appendChild(background);
    svg.appendChild(g);
    exports.__dimensions.set(svg, { width, height });
    return svg;
}
exports.shading = shading;
function space() {
    return text(`${space_char}`, DeltaStyle.Unchanged);
}
exports.space = space;
// Content below or to the left is clipped automatically; content to above or to the right is clipped 
// if we set width and height.
function svgElement(overflow, x, y, width, height, defs, createdBy) {
    const svg = createElement("svg", createdBy);
    svg.setAttribute("x", `${round(x)}`);
    svg.setAttribute("y", `${round(y)}`);
    svg.setAttribute("width", `${round(width)}`);
    svg.setAttribute("height", `${round(height)}`);
    // use inline style rather than an attribute, otherwise any active styling will override
    svg.setAttribute("style", `overflow: ${overflow ? "visible" : "hidden"}`);
    if (defs) {
        const d = createElement("defs", createdBy);
        d.setAttribute("id", "defs");
        svg.appendChild(d);
    }
    return svg;
}
exports.svgElement = svgElement;
// Chrome doesn't appear to fully support SVG 2.0 yet; in particular, transform attributes on svg elements are 
// ignored (except at the root). To invert the y-axis, we have to add a nested g element containing the transform.
function svgElement_inverted(w, h) {
    const svg = svgElement(false, 0, 0, w, h, true, svgElement_inverted);
    const g = createElement("g", svgElement_inverted);
    g.setAttribute("transform", `scale(1,-1) translate(0,${-h})`);
    g.setAttribute("width", `${w}`);
    g.setAttribute("height", `${h}`);
    svg.appendChild(g);
    return [svg, g];
}
exports.svgElement_inverted = svgElement_inverted;
// Top-level SVG node with a "defs" element with id "defs".
function svgRootElement(w, h) {
    const svg = svgElement(false, 0, 0, w, h, true, svgRootElement);
    // See https://vecta.io/blog/guide-to-getting-sharp-and-crisp-svg-images
    svg.setAttribute("viewBox", `-0.5 -0.5 ${w.toString()} ${h.toString()}`);
    svg.style.verticalAlign = "top";
    svg.style.display = "inline-block";
    return svg;
}
exports.svgRootElement = svgRootElement;
function text(str, ẟ_style) {
    const text = textElement(fontSize, [classes, ẟ_style].join(" "), str);
    text.setAttribute("transform", `translate(${0},${lineHeight / 2})`);
    text.setAttribute("alignment-baseline", "central");
    const width = exports.svg.textWidth(text);
    exports.__dimensions.set(text, { width, height: lineHeight });
    text.remove();
    return text;
}
exports.text = text;
function textElement(fontSize, class_, str) {
    const text = createElement("text", textElement);
    text.setAttribute("stroke", "none");
    text.setAttribute("font-size", fontSize.toString()); // wasn't able to set this through CSS for some reason
    text.setAttribute("class", class_); // set styling before creating text node, for font metrics to be correct
    text.setAttribute("pointer-events", "none");
    text.appendChild(document.createTextNode(str));
    return text;
}
// Flip text vertically to cancel out the global vertical flip. Don't set x and y but express
// position through a translation so that the scaling doesn't affect the position.
function textElement_graphical(x, y, fontSize, str) {
    const text = textElement(fontSize, "label", str);
    let transform = `translate(${round(x)},${round(y)})`;
    text.setAttribute("transform", transform + " scale(1,-1)");
    return text;
}
exports.textElement_graphical = textElement_graphical;
function unimplemented(v) {
    return horiz(text(`TODO: ${Core_1.className(v)}`, DeltaStyle.Unchanged));
}
exports.unimplemented = unimplemented;
function vert(...gs) {
    const g = createElement("svg", vert);
    let height_sum = 0, width_max = 0;
    gs.forEach((gʹ) => {
        gʹ.setAttribute("y", `${height_sum}`);
        gʹ.setAttribute("x", `0`);
        const { width, height } = exports.__dimensions.get(gʹ);
        height_sum += height;
        width_max = Math.max(width_max, width);
        g.appendChild(gʹ);
    });
    exports.__dimensions.set(g, { width: width_max, height: height_sum });
    return g;
}
exports.vert = vert;
var DeltaStyle;
(function (DeltaStyle) {
    DeltaStyle["New"] = "new";
    DeltaStyle["Changed"] = "changed";
    DeltaStyle["Unchanged"] = "unchanged";
})(DeltaStyle = exports.DeltaStyle || (exports.DeltaStyle = {}));
// Delta-styling for the constructor component of a value (not its child pointers). In particular, primitives appear changed
// iff their value has changed, whereas non-primitives appear changed iff reclassified. Changes to child pointers must be
// visualised separately.
function deltaStyle(v) {
    if (Versioned_1.versioned(v)) {
        if (v.__ẟ instanceof Delta_1.New) {
            return DeltaStyle.New;
        }
        else if (v.__ẟ instanceof Delta_1.Change) {
            if (Object.keys(v.__ẟ.changed).length > 0 && Value_1.isPrim(v)) {
                return DeltaStyle.Changed;
            }
            else {
                return DeltaStyle.Unchanged;
            }
        }
        else if (v.__ẟ instanceof Delta_1.Reclassify) {
            return DeltaStyle.Changed;
        }
        else {
            return Core_1.absurd();
        }
    }
    else {
        return Core_1.absurd();
    }
}
exports.deltaStyle = deltaStyle;


/***/ }),
/* 16 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Array_1 = __webpack_require__(11);
const Core_1 = __webpack_require__(0);
const Lattice_1 = __webpack_require__(12);
const Annotation_1 = __webpack_require__(10);
const BaseTypes_1 = __webpack_require__(4);
const DataType_1 = __webpack_require__(7);
const DataValue_1 = __webpack_require__(3);
const Env_1 = __webpack_require__(14);
const Expl_1 = __webpack_require__(5);
const Expr_1 = __webpack_require__(6);
const FiniteMap_1 = __webpack_require__(23);
const Match_1 = __webpack_require__(9);
const Primitive_1 = __webpack_require__(24);
const Value_1 = __webpack_require__(1);
const Versioned_1 = __webpack_require__(2);
var Eval;
(function (Eval) {
    // ρ plus bindings in δ are closing for f.
    class Closure extends DataValue_1.DataValue {
        constructor() {
            super(...arguments);
            this.ρ = Value_1._;
            this.δ = Value_1._;
            this.f = Value_1._;
        }
    }
    Eval.Closure = Closure;
    function closure(ρ, δ, f) {
        return Versioned_1.at(Closure, ρ, δ, f);
    }
    // Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
    function recDefs(δ_0, ρ, δ) {
        if (BaseTypes_1.Cons.is(δ)) {
            const def = δ.head, [δₜ, ρ_ext] = recDefs(δ_0, ρ, δ.tail), k = Value_1.memoId(recDefs, arguments), tf = DataValue_1.explValue(Expl_1.Expl.fun(def.σ)(k.tag("t")), closure(ρ, δ_0, def.σ)(k.tag("v")));
            return [BaseTypes_1.cons(Expl_1.Expl.recDef(def.x, tf)(k), δₜ), Env_1.extendEnv(ρ_ext, def.x, tf)];
        }
        else if (BaseTypes_1.Nil.is(δ)) {
            return [BaseTypes_1.nil(), Env_1.emptyEnv()];
        }
        else {
            return Core_1.absurd();
        }
    }
    function recDefs_(dir, δ) {
        if (BaseTypes_1.Cons.is(δ)) {
            Array_1.zip(δ.head.tf.v.δ.toArray(), δ.toArray()).map(([def, defₜ]) => {
                Core_1.assert(def.x.eq(defₜ.x));
                if (dir === Annotation_1.Direction.Fwd) {
                    Annotation_1.setα(Annotation_1.isα(def), defₜ.tf);
                }
                else {
                    Annotation_1.setjoinα(Annotation_1.isα(defₜ.tf), def);
                }
            });
        }
        else if (BaseTypes_1.Nil.is(δ)) {
        }
        else {
            return Core_1.absurd();
        }
    }
    // Here we mustn't invert definition order.
    function defs(ρ, def̅, ρ_ext) {
        const k = Value_1.memoId(defs, arguments);
        if (BaseTypes_1.Cons.is(def̅)) {
            const def = def̅.head;
            if (def instanceof Expr_1.Expr.Let) {
                const tv = eval_(ρ.concat(ρ_ext), def.e), [def̅ₜ, ρ_extʹ] = defs(ρ, def̅.tail, Env_1.extendEnv(ρ_ext, def.x, tv));
                return [BaseTypes_1.cons(Expl_1.Expl.let_(def.x, tv)(k), def̅ₜ), ρ_extʹ];
            }
            else if (def instanceof Expr_1.Expr.Prim) {
                // first-class primitives currently happen to be unary
                if (Primitive_1.unaryOps.has(def.x.val)) {
                    const t_op = Primitive_1.unaryOps.get(def.x.val), [def̅ₜ, ρ_extʹ] = defs(ρ, def̅.tail, Env_1.extendEnv(ρ_ext, def.x, t_op));
                    return [BaseTypes_1.cons(Expl_1.Expl.prim(def.x, t_op)(k), def̅ₜ), ρ_extʹ];
                }
                else {
                    return Core_1.userError(`No implementation found for primitive "${def.x.val}".`);
                }
            }
            else if (def instanceof Expr_1.Expr.LetRec) {
                const [δ, ρᵟ] = recDefs(def.δ, ρ.concat(ρ_ext), def.δ), [def̅ₜ, ρ_extʹ] = defs(ρ, def̅.tail, ρ_ext.concat(ρᵟ));
                return [BaseTypes_1.cons(Expl_1.Expl.letRec(δ)(k), def̅ₜ), ρ_extʹ];
            }
            else {
                return Core_1.absurd();
            }
        }
        else if (BaseTypes_1.Nil.is(def̅)) {
            return [BaseTypes_1.nil(), ρ_ext];
        }
        else {
            return Core_1.absurd();
        }
    }
    Eval.defs = defs;
    function defs_fwd(def̅, def̅ₜ) {
        Array_1.zip(def̅.toArray(), def̅ₜ.toArray()).forEach(([def, defₜ]) => {
            if (def instanceof Expr_1.Expr.Let && defₜ instanceof Expl_1.Expl.Let) {
                eval_fwd(def.e, defₜ.tv);
                Annotation_1.setmeetα(Annotation_1.isα(def), defₜ.tv);
            }
            else if (def instanceof Expr_1.Expr.Prim && defₜ instanceof Expl_1.Expl.Prim) {
                Annotation_1.setα(Annotation_1.isα(def), defₜ.t_op);
            }
            else if (def instanceof Expr_1.Expr.LetRec && defₜ instanceof Expl_1.Expl.LetRec) {
                recDefs_(Annotation_1.Direction.Fwd, defₜ.δ);
            }
            else {
                Core_1.absurd();
            }
        });
    }
    function defs_bwd(def̅, def̅ₜ) {
        Array_1.zip(def̅.toArray(), def̅ₜ.toArray()).reverse().forEach(([def, defₜ]) => {
            if (def instanceof Expr_1.Expr.Let && defₜ instanceof Expl_1.Expl.Let) {
                Annotation_1.setjoinα(Annotation_1.isα(defₜ.tv), def);
                eval_bwd(def.e, defₜ.tv);
            }
            else if (def instanceof Expr_1.Expr.Prim && defₜ instanceof Expl_1.Expl.Prim) {
                Annotation_1.setjoinα(Annotation_1.isα(defₜ.t_op), def);
            }
            else if (def instanceof Expr_1.Expr.LetRec && defₜ instanceof Expl_1.Expl.LetRec) {
                recDefs_(Annotation_1.Direction.Bwd, defₜ.δ);
            }
            else {
                Core_1.absurd();
            }
        });
    }
    function eval_(ρ, e) {
        const k = Value_1.memoId(eval_, arguments), [kₜ, kᵥ] = [k.tag("t"), k.tag("v")];
        if (e instanceof Expr_1.Expr.ConstNum) {
            return DataValue_1.explValue(Expl_1.Expl.const_()(kₜ), Versioned_1.num(e.val.val)(kᵥ));
        }
        else if (e instanceof Expr_1.Expr.ConstStr) {
            return DataValue_1.explValue(Expl_1.Expl.const_()(kₜ), Versioned_1.str(e.val.val)(kᵥ));
        }
        else if (e instanceof Expr_1.Expr.Fun) {
            return DataValue_1.explValue(Expl_1.Expl.fun(e.σ)(kₜ), closure(ρ, BaseTypes_1.nil(), e.σ)(kᵥ));
        }
        else if (e instanceof Expr_1.Expr.DataExpr) {
            const tv̅ = e.__children.map((e) => eval_(ρ, e)), C = DataType_1.valueClass(Core_1.classOf(e)), t = Versioned_1.at(DataType_1.explClass(C), ...tv̅.map(({ t }) => t))(kₜ), v = Versioned_1.at(C, ...tv̅.map(({ v }) => v))(kᵥ);
            return DataValue_1.explValue(t, v);
        }
        else if (e instanceof Expr_1.Expr.Quote) {
            return DataValue_1.explValue(Expl_1.Expl.quote()(kₜ), e.e);
        }
        else if (e instanceof Expr_1.Expr.Var) {
            if (ρ.has(e.x)) {
                const { t, v } = ρ.get(e.x);
                return DataValue_1.explValue(Expl_1.Expl.var_(e.x, t)(kₜ), v);
            }
            else {
                return Core_1.userError(`Variable "${e.x.val}" not found.`);
            }
        }
        else if (e instanceof Expr_1.Expr.App) {
            const [tf, tu] = [eval_(ρ, e.f), eval_(ρ, e.e)], [v, u] = [tf.v, tu.v];
            if (v instanceof Closure) {
                const [δ, ρᵟ] = recDefs(v.δ, v.ρ, v.δ), [ρʹ, ξκ] = v.f.apply(tu), { t, v: vʹ } = eval_(v.ρ.concat(ρᵟ.concat(ρʹ)), ξκ.κ);
                return DataValue_1.explValue(Expl_1.Expl.app(tf, tu, δ, ξκ, t)(kₜ), vʹ);
            }
            else if (v instanceof Primitive_1.UnaryOp) {
                if (u instanceof Value_1.Num || u instanceof Value_1.Str) {
                    return DataValue_1.explValue(Expl_1.Expl.unaryApp(tf, tu)(kₜ), v.op(u)(kᵥ));
                }
                else {
                    return Core_1.userError(`Applying "${v.name}" to non-primitive value.`, u);
                }
            }
            else {
                return Core_1.userError(`Cannot apply ${Core_1.className(v)}`);
            }
        }
        else 
        // Binary operators are (currently) "syntax", rather than first-class.
        if (e instanceof Expr_1.Expr.BinaryApp) {
            if (Primitive_1.binaryOps.has(e.opName.val)) {
                const op = Primitive_1.binaryOps.get(e.opName.val).v, [tv1, tv2] = [eval_(ρ, e.e1), eval_(ρ, e.e2)], [v1, v2] = [tv1.v, tv2.v];
                if ((v1 instanceof Value_1.Num || v1 instanceof Value_1.Str) && (v2 instanceof Value_1.Num || v2 instanceof Value_1.Str)) {
                    const k = Value_1.memoId(op.op, [v1, v2]);
                    return DataValue_1.explValue(Expl_1.Expl.binaryApp(tv1, e.opName, tv2)(kₜ), op.op(v1, v2)(k));
                }
                else {
                    return Core_1.userError(`Applying "${e.opName.val}" to non-primitive value.`, v1, v2);
                }
            }
            else {
                return Core_1.userError(`Binary primitive "${e.opName.val}" not found.`);
            }
        }
        else if (e instanceof Expr_1.Expr.Defs) {
            const [def̅ₜ, ρʹ] = defs(ρ, e.def̅, Env_1.emptyEnv()), { t, v } = eval_(ρ.concat(ρʹ), e.e);
            return DataValue_1.explValue(Expl_1.Expl.defs(def̅ₜ, t)(kₜ), v);
        }
        else if (e instanceof Expr_1.Expr.MatchAs) {
            const tu = eval_(ρ, e.e), [ρʹ, ξκ] = e.σ.apply(tu), { t, v } = eval_(ρ.concat(ρʹ), ξκ.κ);
            return DataValue_1.explValue(Expl_1.Expl.matchAs(tu, ξκ, t)(kₜ), v);
        }
        else if (e instanceof Expr_1.Expr.Typematch) {
            const tu = eval_(ρ, e.e), d = DataType_1.ctrToDataType.get(Core_1.className(tu.v)) || DataType_1.types.get(Core_1.className(tu.v)), eʹ = FiniteMap_1.get(e.cases, d.name);
            if (eʹ === undefined) {
                return Core_1.userError(`Typecase mismatch: no clause for ${Core_1.className(tu.v)}.`);
            }
            else {
                const { t, v } = eval_(ρ, eʹ);
                return DataValue_1.explValue(Expl_1.Expl.typematch(tu, d.name, t)(kₜ), v);
            }
        }
        else {
            return Core_1.absurd(`Unimplemented expression form: ${Core_1.className(e)}.`);
        }
    }
    Eval.eval_ = eval_;
    function eval_fwd(e, tv) {
        const { t, v } = tv;
        if (t instanceof Expl_1.Expl.Const && (v instanceof Value_1.Num || v instanceof Value_1.Str)) {
            Annotation_1.setα(Annotation_1.isα(e), tv);
        }
        else if (t instanceof Expl_1.Expl.Fun && v instanceof Closure) {
            Annotation_1.setα(Annotation_1.isα(e), tv);
        }
        else if (t instanceof Expl_1.Expl.Quote) {
            Annotation_1.setα(Annotation_1.isα(e), tv);
        }
        else if (t instanceof Expl_1.Expl.Var) {
            Annotation_1.setα(Lattice_1.bool_.meet(Annotation_1.isα(e), Annotation_1.isα(DataValue_1.explValue(t.t, v))), tv);
        }
        else if (t instanceof Expl_1.Expl.DataExpl) {
            if (v instanceof DataValue_1.DataValue) {
                const eʹ = Core_1.as(e, Expr_1.Expr.DataExpr);
                Array_1.zip(Expl_1.Expl.explChildren(t, v), eʹ.__children).map(([tv, e]) => eval_fwd(e, tv));
                Annotation_1.setα(Annotation_1.isα(e), tv);
            }
            else {
                Core_1.absurd();
            }
        }
        else if (t instanceof Expl_1.Expl.App) {
            const eʹ = Core_1.as(e, Expr_1.Expr.App);
            eval_fwd(eʹ.f, t.tf);
            eval_fwd(eʹ.e, t.tu);
            recDefs_(Annotation_1.Direction.Fwd, t.δ);
            eval_fwd(t.ξ.κ, DataValue_1.explValue(t.t, v));
            Annotation_1.setα(Lattice_1.bool_.meet(Annotation_1.isα(t.tf), Match_1.apply_fwd(t.ξ), Annotation_1.isα(e), Annotation_1.isα(DataValue_1.explValue(t.t, v))), tv);
        }
        else if (t instanceof Expl_1.Expl.UnaryApp) {
            const eʹ = Core_1.as(e, Expr_1.Expr.App);
            eval_fwd(eʹ.f, t.tf);
            eval_fwd(eʹ.e, t.tv);
            Annotation_1.setα(Lattice_1.bool_.meet(Annotation_1.isα(t.tf), Annotation_1.isα(t.tv), Annotation_1.isα(e)), tv);
        }
        else if (t instanceof Expl_1.Expl.BinaryApp) {
            const eʹ = Core_1.as(e, Expr_1.Expr.BinaryApp);
            eval_fwd(eʹ.e1, t.tv1);
            eval_fwd(eʹ.e2, t.tv2);
            Annotation_1.setα(Lattice_1.bool_.meet(Annotation_1.isα(t.tv1), Annotation_1.isα(t.tv2), Annotation_1.isα(e)), tv);
        }
        else if (t instanceof Expl_1.Expl.Defs) {
            const eʹ = Core_1.as(e, Expr_1.Expr.Defs);
            defs_fwd(eʹ.def̅, t.def̅);
            eval_fwd(eʹ.e, DataValue_1.explValue(t.t, v));
            Annotation_1.setα(Lattice_1.bool_.meet(Annotation_1.isα(e), Annotation_1.isα(DataValue_1.explValue(t.t, v))), tv);
        }
        else if (t instanceof Expl_1.Expl.MatchAs) {
            const eʹ = Core_1.as(e, Expr_1.Expr.MatchAs);
            eval_fwd(eʹ.e, t.tu);
            eval_fwd(t.ξ.κ, DataValue_1.explValue(t.t, v));
            Annotation_1.setα(Lattice_1.bool_.meet(Match_1.apply_fwd(t.ξ), Annotation_1.isα(e), Annotation_1.isα(DataValue_1.explValue(t.t, v))), tv);
        }
        else if (t instanceof Expl_1.Expl.Typematch) {
            const eʹ = Core_1.as(e, Expr_1.Expr.Typematch);
            eval_fwd(eʹ.e, t.tu);
            eval_fwd(FiniteMap_1.get(eʹ.cases, t.d), DataValue_1.explValue(t.t, v));
            Annotation_1.setα(Lattice_1.bool_.meet(Annotation_1.isα(e), Annotation_1.isα(DataValue_1.explValue(t.t, v))), tv);
        }
        else {
            Core_1.absurd();
        }
    }
    Eval.eval_fwd = eval_fwd;
    // Avoid excessive joins via a merging implementation; requires all annotations to have been cleared first.
    function eval_bwd(e, tv) {
        const { t, v } = tv;
        if (t instanceof Expl_1.Expl.Const && (v instanceof Value_1.Num || v instanceof Value_1.Str)) {
            Annotation_1.setjoinα(Annotation_1.isα(tv), e);
        }
        else if (t instanceof Expl_1.Expl.Fun && v instanceof Closure) {
            Annotation_1.setjoinα(Annotation_1.isα(tv), e);
        }
        else if (t instanceof Expl_1.Expl.DataExpl) {
            if (v instanceof DataValue_1.DataValue) {
                const eʹ = Core_1.as(e, Expr_1.Expr.DataExpr);
                // reverse order but shouldn't matter in absence of side-effects:
                Array_1.zip(Expl_1.Expl.explChildren(t, v), eʹ.__children).map(([tv, e]) => eval_bwd(e, tv));
                Annotation_1.setjoinα(Annotation_1.isα(tv), e);
            }
            else {
                Core_1.absurd();
            }
        }
        else if (t instanceof Expl_1.Expl.Quote) {
            Annotation_1.setjoinα(Annotation_1.isα(tv), e);
        }
        else if (t instanceof Expl_1.Expl.Var) {
            Annotation_1.setjoinα(Annotation_1.isα(tv), DataValue_1.explValue(t.t, v));
            Annotation_1.setjoinα(Annotation_1.isα(tv), e);
        }
        else if (t instanceof Expl_1.Expl.App) {
            Core_1.assert(t.tf.v instanceof Closure);
            Annotation_1.setjoinα(Annotation_1.isα(tv), DataValue_1.explValue(t.t, v));
            eval_bwd(t.ξ.κ, DataValue_1.explValue(t.t, v));
            Match_1.apply_bwd(t.ξ, Annotation_1.isα(tv));
            recDefs_(Annotation_1.Direction.Bwd, t.δ);
            Annotation_1.setjoinα(Annotation_1.isα(tv), t.tf);
            const eʹ = Core_1.as(e, Expr_1.Expr.App);
            eval_bwd(eʹ.f, t.tf);
            eval_bwd(eʹ.e, t.tu);
            Annotation_1.setjoinα(Annotation_1.isα(tv), e);
        }
        else if (t instanceof Expl_1.Expl.UnaryApp) {
            Annotation_1.setjoinα(Annotation_1.isα(tv), t.tf);
            Annotation_1.setjoinα(Annotation_1.isα(tv), t.tv);
            const eʹ = Core_1.as(e, Expr_1.Expr.App);
            eval_bwd(eʹ.f, t.tf);
            eval_bwd(eʹ.e, t.tv);
            Annotation_1.setjoinα(Annotation_1.isα(tv), e);
        }
        else if (t instanceof Expl_1.Expl.BinaryApp) {
            Core_1.assert(Primitive_1.binaryOps.has(t.opName.val));
            Annotation_1.setjoinα(Annotation_1.isα(tv), t.tv1);
            Annotation_1.setjoinα(Annotation_1.isα(tv), t.tv2);
            const eʹ = Core_1.as(e, Expr_1.Expr.BinaryApp);
            eval_bwd(eʹ.e1, t.tv1);
            eval_bwd(eʹ.e2, t.tv2);
            Annotation_1.setjoinα(Annotation_1.isα(tv), e);
        }
        else if (t instanceof Expl_1.Expl.Defs) {
            Annotation_1.setjoinα(Annotation_1.isα(tv), DataValue_1.explValue(t.t, v));
            const eʹ = Core_1.as(e, Expr_1.Expr.Defs);
            eval_bwd(eʹ.e, DataValue_1.explValue(t.t, v));
            defs_bwd(eʹ.def̅, t.def̅);
            Annotation_1.setjoinα(Annotation_1.isα(tv), e);
        }
        else if (t instanceof Expl_1.Expl.MatchAs) {
            Annotation_1.setjoinα(Annotation_1.isα(tv), DataValue_1.explValue(t.t, v));
            const eʹ = Core_1.as(e, Expr_1.Expr.MatchAs);
            eval_bwd(t.ξ.κ, DataValue_1.explValue(t.t, v));
            Match_1.apply_bwd(t.ξ, Annotation_1.isα(tv));
            eval_bwd(eʹ.e, t.tu);
            Annotation_1.setjoinα(Annotation_1.isα(tv), e);
        }
        else if (t instanceof Expl_1.Expl.Typematch) {
            Annotation_1.setjoinα(Annotation_1.isα(tv), DataValue_1.explValue(t.t, v));
            const eʹ = Core_1.as(e, Expr_1.Expr.Typematch);
            eval_bwd(FiniteMap_1.get(eʹ.cases, t.d), DataValue_1.explValue(t.t, v));
            eval_bwd(eʹ.e, t.tu);
            Annotation_1.setjoinα(Annotation_1.isα(tv), e);
        }
        else {
            Core_1.absurd();
        }
    }
    Eval.eval_bwd = eval_bwd;
})(Eval = exports.Eval || (exports.Eval = {}));
DataType_1.initDataType(Expr_1.Expr.Expr, [Expr_1.Expr.App, Expr_1.Expr.BinaryApp, Expr_1.Expr.ConstNum, Expr_1.Expr.ConstStr, Expr_1.Expr.DataExpr, Expr_1.Expr.Defs, Expr_1.Expr.Fun, Expr_1.Expr.MatchAs, Expr_1.Expr.Quote, Expr_1.Expr.Var]);


/***/ }),
/* 17 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Array_1 = __webpack_require__(11);
const Core_1 = __webpack_require__(0);
const Lattice_1 = __webpack_require__(12);
const Annotation_1 = __webpack_require__(10);
const BaseTypes_1 = __webpack_require__(4);
const DataType_1 = __webpack_require__(7);
const DataValue_1 = __webpack_require__(3);
const Delta_1 = __webpack_require__(13);
const Expl_1 = __webpack_require__(5);
const Expr_1 = __webpack_require__(6);
const Match_1 = __webpack_require__(9);
const Value_1 = __webpack_require__(1);
const Versioned_1 = __webpack_require__(2);
var Let = Expr_1.Expr.Let;
var LetRec = Expr_1.Expr.LetRec;
var Prim = Expr_1.Expr.Prim;
class Cursor {
    notAnnotated() {
        return Core_1.userError("Not an annotated node.", this.on);
    }
    assert(C, pred) {
        return this.at(C, v => Core_1.assert(pred(v)));
    }
    αset() {
        if (Annotation_1.annotated(this.on)) {
            Core_1.assert(Annotation_1.isα(this.on) === Lattice_1.bool_.top);
            return this;
        }
        else {
            return this.notAnnotated();
        }
    }
    αclear() {
        if (Annotation_1.annotated(this.on)) {
            Core_1.assert(Annotation_1.isα(this.on) === Lattice_1.bool_.bot);
            return this;
        }
        else {
            return this.notAnnotated();
        }
    }
    setα() {
        if (Annotation_1.annotated(this.on)) {
            Annotation_1.setα(Lattice_1.bool_.top, this.on);
            return this;
        }
        else {
            return this.notAnnotated();
        }
    }
    clearα() {
        if (Annotation_1.annotated(this.on)) {
            Annotation_1.setα(Lattice_1.bool_.bot, this.on);
            return this;
        }
        else {
            return this.notAnnotated();
        }
    }
    // Helpers specific to certain datatypes.
    treeNodeValue() {
        return this.to(BaseTypes_1.NonEmpty, "t")
            .to(BaseTypes_1.Pair, "snd");
    }
    nth(n) {
        if (n === 0) {
            return this.to(BaseTypes_1.Cons, "head");
        }
        else {
            return this.to(BaseTypes_1.Cons, "tail").nth(n - 1);
        }
    }
}
exports.Cursor = Cursor;
class ExplValueCursor extends Cursor {
    constructor(ancestors, tv) {
        super();
        this.ancestors = ancestors;
        this.tv = tv;
    }
    static descendant(prev, tv) {
        return new ExplValueCursor(prev === null ? [] : [...prev.ancestors, prev.tv], tv);
    }
    static parent(child) {
        Core_1.assert(child.ancestors.length > 0);
        return new ExplValueCursor(child.ancestors.slice(0, child.ancestors.length - 1), Array_1.last(child.ancestors));
    }
    get on() {
        return this.tv;
    }
    to(C, k) {
        return ExplValueCursor.descendant(this, Expl_1.Expl.explChild(this.tv.t, Core_1.as(this.tv.v, C), k));
    }
    toChild(n) {
        if (this.tv.v instanceof DataValue_1.DataValue) {
            const tvs = Expl_1.Expl.explChildren(this.tv.t, this.tv.v);
            if (0 <= n && n < tvs.length) {
                return ExplValueCursor.descendant(this, Array_1.nth(tvs, n));
            }
            else {
                return this;
            }
        }
        else {
            return Core_1.userError("Not a data value");
        }
    }
    toChildOffset(tv, offset) {
        if (this.tv.v instanceof DataValue_1.DataValue) {
            const tvs = Expl_1.Expl.explChildren(this.tv.t, this.tv.v);
            const n = tvs.findIndex(tv_ => tv_ === tv);
            if (n === -1) {
                return Core_1.userError("Not a child");
            }
            else {
                return this.toChild(n + offset);
            }
        }
        else {
            return Core_1.userError("Not a data value");
        }
    }
    nextSibling() {
        if (this.hasParent()) {
            return this.up().toChildOffset(this.tv, 1);
        }
        else {
            return this;
        }
    }
    prevSibling() {
        if (this.hasParent()) {
            return this.up().toChildOffset(this.tv, -1);
        }
        else {
            return this;
        }
    }
    hasParent() {
        return this.ancestors.length > 0;
    }
    up() {
        return ExplValueCursor.parent(this);
    }
    toBinaryArg1(opName) {
        const t = Core_1.as(this.tv.t, Expl_1.Expl.BinaryApp);
        Core_1.assert(t.opName.val === opName);
        return ExplValueCursor.descendant(this, t.tv1);
    }
    toBinaryArg2(opName) {
        const t = Core_1.as(this.tv.t, Expl_1.Expl.BinaryApp);
        Core_1.assert(t.opName.val === opName);
        return ExplValueCursor.descendant(this, t.tv2);
    }
    at(C, f) {
        f(Core_1.as(this.tv.v, C));
        return this;
    }
    isChanged(s_ẟ) {
        Core_1.assert(Versioned_1.asVersioned(this.tv.v).__ẟ.eq(new Delta_1.Change(s_ẟ)));
        return this;
    }
    isUnchanged() {
        Core_1.assert(Versioned_1.asVersioned(this.tv.v).__ẟ.eq(new Delta_1.Change({})));
        return this;
    }
    isNew() {
        Core_1.assert(Versioned_1.asVersioned(this.tv.v).__ẟ instanceof Delta_1.New);
        return this;
    }
    toTerminal() {
        let t = this.tv.t;
        while (t instanceof Expl_1.Expl.NonTerminal) {
            t = t.t;
        }
        return ExplValueCursor.descendant(this, DataValue_1.explValue(t, this.tv.v));
    }
}
exports.ExplValueCursor = ExplValueCursor;
class ExprCursor extends Cursor {
    constructor(v) {
        super();
        this.v = v;
    }
    get on() {
        return this.v;
    }
    // No way to specify only "own" properties statically.
    to(C, prop) {
        const vʹ = Core_1.as(this.v, C)[prop]; // TypeScript nonsense
        return new ExprCursor(vʹ);
    }
    // Allow the data value class to be used to navigate the data expression form.
    constr_to(C, prop) {
        return this.to(DataType_1.exprClass(C), prop);
    }
    toCase(C) {
        const vʹ = Core_1.__nonNull(Core_1.as(this.v, Match_1.DataElim)[C.name]);
        return new ExprCursor(vʹ);
    }
    static defs(defs) {
        const defsʹ = new Map;
        for (; BaseTypes_1.Cons.is(defs); defs = defs.tail) {
            const def = defs.head;
            if (def instanceof Let || def instanceof Prim) {
                defsʹ.set(def.x.val, def);
            }
            else if (def instanceof LetRec) {
                for (let recDefs = def.δ; BaseTypes_1.Cons.is(recDefs); recDefs = recDefs.tail) {
                    const recDef = recDefs.head;
                    defsʹ.set(recDef.x.val, recDef);
                }
            }
            else {
                Core_1.absurd();
            }
        }
        return defsʹ;
    }
    toDef(x) {
        const here = this.to(Expr_1.Expr.Defs, "def̅"), defs = ExprCursor.defs(here.v);
        Core_1.assert(defs.has(x), `No definition of "${x}" found.`);
        return new ExprCursor(defs.get(x));
    }
    at(C, f) {
        f(Core_1.as(this.v, C));
        return this;
    }
    var_(x) {
        this.assert(Match_1.VarElim, σ => σ.x.val === x);
        return this.to(Match_1.VarElim, "κ");
    }
    // Editing API.
    setNum(n) {
        Versioned_1.reset(this.v, Value_1.Num, n);
        return this;
    }
    setStr(str_) {
        Versioned_1.reset(this.v, Value_1.Str, str_);
        return this;
    }
    constr_splice(C, props, makeNode) {
        return this.splice(DataType_1.exprClass(C), props, (e̅) => makeNode(e̅.map(e => Core_1.as(e, Expr_1.Expr.Expr))));
    }
    splice(C, props, makeNode) {
        const v = Core_1.as(this.v, C), v̅ = v.__children, n̅ = props.map(prop => Core_1.__check(Value_1.fields(v).indexOf(prop), n => n != -1)), v̅ʹ = makeNode(n̅.map((n) => v̅[n]));
        n̅.forEach((n, m) => {
            v̅[n] = v̅ʹ[m];
        });
        Versioned_1.reset(v, C, ...v̅);
        return this;
    }
}
exports.ExprCursor = ExprCursor;


/***/ }),
/* 18 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var isOldIE = function isOldIE() {
  var memo;
  return function memorize() {
    if (typeof memo === 'undefined') {
      // Test for IE <= 9 as proposed by Browserhacks
      // @see http://browserhacks.com/#hack-e71d8692f65334173fee715c222cb805
      // Tests for existence of standard globals is to allow style-loader
      // to operate correctly into non-standard environments
      // @see https://github.com/webpack-contrib/style-loader/issues/177
      memo = Boolean(window && document && document.all && !window.atob);
    }

    return memo;
  };
}();

var getTarget = function getTarget() {
  var memo = {};
  return function memorize(target) {
    if (typeof memo[target] === 'undefined') {
      var styleTarget = document.querySelector(target); // Special case to return head of iframe instead of iframe itself

      if (window.HTMLIFrameElement && styleTarget instanceof window.HTMLIFrameElement) {
        try {
          // This will throw an exception if access to iframe is blocked
          // due to cross-origin restrictions
          styleTarget = styleTarget.contentDocument.head;
        } catch (e) {
          // istanbul ignore next
          styleTarget = null;
        }
      }

      memo[target] = styleTarget;
    }

    return memo[target];
  };
}();

var stylesInDom = [];

function getIndexByIdentifier(identifier) {
  var result = -1;

  for (var i = 0; i < stylesInDom.length; i++) {
    if (stylesInDom[i].identifier === identifier) {
      result = i;
      break;
    }
  }

  return result;
}

function modulesToDom(list, options) {
  var idCountMap = {};
  var identifiers = [];

  for (var i = 0; i < list.length; i++) {
    var item = list[i];
    var id = options.base ? item[0] + options.base : item[0];
    var count = idCountMap[id] || 0;
    var identifier = "".concat(id, " ").concat(count);
    idCountMap[id] = count + 1;
    var index = getIndexByIdentifier(identifier);
    var obj = {
      css: item[1],
      media: item[2],
      sourceMap: item[3]
    };

    if (index !== -1) {
      stylesInDom[index].references++;
      stylesInDom[index].updater(obj);
    } else {
      stylesInDom.push({
        identifier: identifier,
        updater: addStyle(obj, options),
        references: 1
      });
    }

    identifiers.push(identifier);
  }

  return identifiers;
}

function insertStyleElement(options) {
  var style = document.createElement('style');
  var attributes = options.attributes || {};

  if (typeof attributes.nonce === 'undefined') {
    var nonce =  true ? __webpack_require__.nc : undefined;

    if (nonce) {
      attributes.nonce = nonce;
    }
  }

  Object.keys(attributes).forEach(function (key) {
    style.setAttribute(key, attributes[key]);
  });

  if (typeof options.insert === 'function') {
    options.insert(style);
  } else {
    var target = getTarget(options.insert || 'head');

    if (!target) {
      throw new Error("Couldn't find a style target. This probably means that the value for the 'insert' parameter is invalid.");
    }

    target.appendChild(style);
  }

  return style;
}

function removeStyleElement(style) {
  // istanbul ignore if
  if (style.parentNode === null) {
    return false;
  }

  style.parentNode.removeChild(style);
}
/* istanbul ignore next  */


var replaceText = function replaceText() {
  var textStore = [];
  return function replace(index, replacement) {
    textStore[index] = replacement;
    return textStore.filter(Boolean).join('\n');
  };
}();

function applyToSingletonTag(style, index, remove, obj) {
  var css = remove ? '' : obj.media ? "@media ".concat(obj.media, " {").concat(obj.css, "}") : obj.css; // For old IE

  /* istanbul ignore if  */

  if (style.styleSheet) {
    style.styleSheet.cssText = replaceText(index, css);
  } else {
    var cssNode = document.createTextNode(css);
    var childNodes = style.childNodes;

    if (childNodes[index]) {
      style.removeChild(childNodes[index]);
    }

    if (childNodes.length) {
      style.insertBefore(cssNode, childNodes[index]);
    } else {
      style.appendChild(cssNode);
    }
  }
}

function applyToTag(style, options, obj) {
  var css = obj.css;
  var media = obj.media;
  var sourceMap = obj.sourceMap;

  if (media) {
    style.setAttribute('media', media);
  } else {
    style.removeAttribute('media');
  }

  if (sourceMap && btoa) {
    css += "\n/*# sourceMappingURL=data:application/json;base64,".concat(btoa(unescape(encodeURIComponent(JSON.stringify(sourceMap)))), " */");
  } // For old IE

  /* istanbul ignore if  */


  if (style.styleSheet) {
    style.styleSheet.cssText = css;
  } else {
    while (style.firstChild) {
      style.removeChild(style.firstChild);
    }

    style.appendChild(document.createTextNode(css));
  }
}

var singleton = null;
var singletonCounter = 0;

function addStyle(obj, options) {
  var style;
  var update;
  var remove;

  if (options.singleton) {
    var styleIndex = singletonCounter++;
    style = singleton || (singleton = insertStyleElement(options));
    update = applyToSingletonTag.bind(null, style, styleIndex, false);
    remove = applyToSingletonTag.bind(null, style, styleIndex, true);
  } else {
    style = insertStyleElement(options);
    update = applyToTag.bind(null, style, options);

    remove = function remove() {
      removeStyleElement(style);
    };
  }

  update(obj);
  return function updateStyle(newObj) {
    if (newObj) {
      if (newObj.css === obj.css && newObj.media === obj.media && newObj.sourceMap === obj.sourceMap) {
        return;
      }

      update(obj = newObj);
    } else {
      remove();
    }
  };
}

module.exports = function (list, options) {
  options = options || {}; // Force single-tag solution on IE6-9, which has a hard limit on the # of <style>
  // tags it will allow on a page

  if (!options.singleton && typeof options.singleton !== 'boolean') {
    options.singleton = isOldIE();
  }

  list = list || [];
  var lastIdentifiers = modulesToDom(list, options);
  return function update(newList) {
    newList = newList || [];

    if (Object.prototype.toString.call(newList) !== '[object Array]') {
      return;
    }

    for (var i = 0; i < lastIdentifiers.length; i++) {
      var identifier = lastIdentifiers[i];
      var index = getIndexByIdentifier(identifier);
      stylesInDom[index].references--;
    }

    var newLastIdentifiers = modulesToDom(newList, options);

    for (var _i = 0; _i < lastIdentifiers.length; _i++) {
      var _identifier = lastIdentifiers[_i];

      var _index = getIndexByIdentifier(_identifier);

      if (stylesInDom[_index].references === 0) {
        stylesInDom[_index].updater();

        stylesInDom.splice(_index, 1);
      }
    }

    lastIdentifiers = newLastIdentifiers;
  };
};

/***/ }),
/* 19 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


/*
  MIT License http://www.opensource.org/licenses/mit-license.php
  Author Tobias Koppers @sokra
*/
// css base code, injected by the css-loader
// eslint-disable-next-line func-names
module.exports = function (useSourceMap) {
  var list = []; // return the list of modules as css string

  list.toString = function toString() {
    return this.map(function (item) {
      var content = cssWithMappingToString(item, useSourceMap);

      if (item[2]) {
        return "@media ".concat(item[2], " {").concat(content, "}");
      }

      return content;
    }).join('');
  }; // import a list of modules into the list
  // eslint-disable-next-line func-names


  list.i = function (modules, mediaQuery, dedupe) {
    if (typeof modules === 'string') {
      // eslint-disable-next-line no-param-reassign
      modules = [[null, modules, '']];
    }

    var alreadyImportedModules = {};

    if (dedupe) {
      for (var i = 0; i < this.length; i++) {
        // eslint-disable-next-line prefer-destructuring
        var id = this[i][0];

        if (id != null) {
          alreadyImportedModules[id] = true;
        }
      }
    }

    for (var _i = 0; _i < modules.length; _i++) {
      var item = [].concat(modules[_i]);

      if (dedupe && alreadyImportedModules[item[0]]) {
        // eslint-disable-next-line no-continue
        continue;
      }

      if (mediaQuery) {
        if (!item[2]) {
          item[2] = mediaQuery;
        } else {
          item[2] = "".concat(mediaQuery, " and ").concat(item[2]);
        }
      }

      list.push(item);
    }
  };

  return list;
};

function cssWithMappingToString(item, useSourceMap) {
  var content = item[1] || ''; // eslint-disable-next-line prefer-destructuring

  var cssMapping = item[3];

  if (!cssMapping) {
    return content;
  }

  if (useSourceMap && typeof btoa === 'function') {
    var sourceMapping = toComment(cssMapping);
    var sourceURLs = cssMapping.sources.map(function (source) {
      return "/*# sourceURL=".concat(cssMapping.sourceRoot || '').concat(source, " */");
    });
    return [content].concat(sourceURLs).concat([sourceMapping]).join('\n');
  }

  return [content].join('\n');
} // Adapted from convert-source-map (MIT)


function toComment(sourceMap) {
  // eslint-disable-next-line no-undef
  var base64 = btoa(unescape(encodeURIComponent(JSON.stringify(sourceMap))));
  var data = "sourceMappingURL=data:application/json;charset=utf-8;base64,".concat(base64);
  return "/*# ".concat(data, " */");
}

/***/ }),
/* 20 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* WEBPACK VAR INJECTION */(function(global) {/**!
 * @fileOverview Kickass library to create and place poppers near their reference elements.
 * @version 1.16.1
 * @license
 * Copyright (c) 2016 Federico Zivolo and contributors
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
var isBrowser = typeof window !== 'undefined' && typeof document !== 'undefined' && typeof navigator !== 'undefined';

var timeoutDuration = function () {
  var longerTimeoutBrowsers = ['Edge', 'Trident', 'Firefox'];
  for (var i = 0; i < longerTimeoutBrowsers.length; i += 1) {
    if (isBrowser && navigator.userAgent.indexOf(longerTimeoutBrowsers[i]) >= 0) {
      return 1;
    }
  }
  return 0;
}();

function microtaskDebounce(fn) {
  var called = false;
  return function () {
    if (called) {
      return;
    }
    called = true;
    window.Promise.resolve().then(function () {
      called = false;
      fn();
    });
  };
}

function taskDebounce(fn) {
  var scheduled = false;
  return function () {
    if (!scheduled) {
      scheduled = true;
      setTimeout(function () {
        scheduled = false;
        fn();
      }, timeoutDuration);
    }
  };
}

var supportsMicroTasks = isBrowser && window.Promise;

/**
* Create a debounced version of a method, that's asynchronously deferred
* but called in the minimum time possible.
*
* @method
* @memberof Popper.Utils
* @argument {Function} fn
* @returns {Function}
*/
var debounce = supportsMicroTasks ? microtaskDebounce : taskDebounce;

/**
 * Check if the given variable is a function
 * @method
 * @memberof Popper.Utils
 * @argument {Any} functionToCheck - variable to check
 * @returns {Boolean} answer to: is a function?
 */
function isFunction(functionToCheck) {
  var getType = {};
  return functionToCheck && getType.toString.call(functionToCheck) === '[object Function]';
}

/**
 * Get CSS computed property of the given element
 * @method
 * @memberof Popper.Utils
 * @argument {Eement} element
 * @argument {String} property
 */
function getStyleComputedProperty(element, property) {
  if (element.nodeType !== 1) {
    return [];
  }
  // NOTE: 1 DOM access here
  var window = element.ownerDocument.defaultView;
  var css = window.getComputedStyle(element, null);
  return property ? css[property] : css;
}

/**
 * Returns the parentNode or the host of the element
 * @method
 * @memberof Popper.Utils
 * @argument {Element} element
 * @returns {Element} parent
 */
function getParentNode(element) {
  if (element.nodeName === 'HTML') {
    return element;
  }
  return element.parentNode || element.host;
}

/**
 * Returns the scrolling parent of the given element
 * @method
 * @memberof Popper.Utils
 * @argument {Element} element
 * @returns {Element} scroll parent
 */
function getScrollParent(element) {
  // Return body, `getScroll` will take care to get the correct `scrollTop` from it
  if (!element) {
    return document.body;
  }

  switch (element.nodeName) {
    case 'HTML':
    case 'BODY':
      return element.ownerDocument.body;
    case '#document':
      return element.body;
  }

  // Firefox want us to check `-x` and `-y` variations as well

  var _getStyleComputedProp = getStyleComputedProperty(element),
      overflow = _getStyleComputedProp.overflow,
      overflowX = _getStyleComputedProp.overflowX,
      overflowY = _getStyleComputedProp.overflowY;

  if (/(auto|scroll|overlay)/.test(overflow + overflowY + overflowX)) {
    return element;
  }

  return getScrollParent(getParentNode(element));
}

/**
 * Returns the reference node of the reference object, or the reference object itself.
 * @method
 * @memberof Popper.Utils
 * @param {Element|Object} reference - the reference element (the popper will be relative to this)
 * @returns {Element} parent
 */
function getReferenceNode(reference) {
  return reference && reference.referenceNode ? reference.referenceNode : reference;
}

var isIE11 = isBrowser && !!(window.MSInputMethodContext && document.documentMode);
var isIE10 = isBrowser && /MSIE 10/.test(navigator.userAgent);

/**
 * Determines if the browser is Internet Explorer
 * @method
 * @memberof Popper.Utils
 * @param {Number} version to check
 * @returns {Boolean} isIE
 */
function isIE(version) {
  if (version === 11) {
    return isIE11;
  }
  if (version === 10) {
    return isIE10;
  }
  return isIE11 || isIE10;
}

/**
 * Returns the offset parent of the given element
 * @method
 * @memberof Popper.Utils
 * @argument {Element} element
 * @returns {Element} offset parent
 */
function getOffsetParent(element) {
  if (!element) {
    return document.documentElement;
  }

  var noOffsetParent = isIE(10) ? document.body : null;

  // NOTE: 1 DOM access here
  var offsetParent = element.offsetParent || null;
  // Skip hidden elements which don't have an offsetParent
  while (offsetParent === noOffsetParent && element.nextElementSibling) {
    offsetParent = (element = element.nextElementSibling).offsetParent;
  }

  var nodeName = offsetParent && offsetParent.nodeName;

  if (!nodeName || nodeName === 'BODY' || nodeName === 'HTML') {
    return element ? element.ownerDocument.documentElement : document.documentElement;
  }

  // .offsetParent will return the closest TH, TD or TABLE in case
  // no offsetParent is present, I hate this job...
  if (['TH', 'TD', 'TABLE'].indexOf(offsetParent.nodeName) !== -1 && getStyleComputedProperty(offsetParent, 'position') === 'static') {
    return getOffsetParent(offsetParent);
  }

  return offsetParent;
}

function isOffsetContainer(element) {
  var nodeName = element.nodeName;

  if (nodeName === 'BODY') {
    return false;
  }
  return nodeName === 'HTML' || getOffsetParent(element.firstElementChild) === element;
}

/**
 * Finds the root node (document, shadowDOM root) of the given element
 * @method
 * @memberof Popper.Utils
 * @argument {Element} node
 * @returns {Element} root node
 */
function getRoot(node) {
  if (node.parentNode !== null) {
    return getRoot(node.parentNode);
  }

  return node;
}

/**
 * Finds the offset parent common to the two provided nodes
 * @method
 * @memberof Popper.Utils
 * @argument {Element} element1
 * @argument {Element} element2
 * @returns {Element} common offset parent
 */
function findCommonOffsetParent(element1, element2) {
  // This check is needed to avoid errors in case one of the elements isn't defined for any reason
  if (!element1 || !element1.nodeType || !element2 || !element2.nodeType) {
    return document.documentElement;
  }

  // Here we make sure to give as "start" the element that comes first in the DOM
  var order = element1.compareDocumentPosition(element2) & Node.DOCUMENT_POSITION_FOLLOWING;
  var start = order ? element1 : element2;
  var end = order ? element2 : element1;

  // Get common ancestor container
  var range = document.createRange();
  range.setStart(start, 0);
  range.setEnd(end, 0);
  var commonAncestorContainer = range.commonAncestorContainer;

  // Both nodes are inside #document

  if (element1 !== commonAncestorContainer && element2 !== commonAncestorContainer || start.contains(end)) {
    if (isOffsetContainer(commonAncestorContainer)) {
      return commonAncestorContainer;
    }

    return getOffsetParent(commonAncestorContainer);
  }

  // one of the nodes is inside shadowDOM, find which one
  var element1root = getRoot(element1);
  if (element1root.host) {
    return findCommonOffsetParent(element1root.host, element2);
  } else {
    return findCommonOffsetParent(element1, getRoot(element2).host);
  }
}

/**
 * Gets the scroll value of the given element in the given side (top and left)
 * @method
 * @memberof Popper.Utils
 * @argument {Element} element
 * @argument {String} side `top` or `left`
 * @returns {number} amount of scrolled pixels
 */
function getScroll(element) {
  var side = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 'top';

  var upperSide = side === 'top' ? 'scrollTop' : 'scrollLeft';
  var nodeName = element.nodeName;

  if (nodeName === 'BODY' || nodeName === 'HTML') {
    var html = element.ownerDocument.documentElement;
    var scrollingElement = element.ownerDocument.scrollingElement || html;
    return scrollingElement[upperSide];
  }

  return element[upperSide];
}

/*
 * Sum or subtract the element scroll values (left and top) from a given rect object
 * @method
 * @memberof Popper.Utils
 * @param {Object} rect - Rect object you want to change
 * @param {HTMLElement} element - The element from the function reads the scroll values
 * @param {Boolean} subtract - set to true if you want to subtract the scroll values
 * @return {Object} rect - The modifier rect object
 */
function includeScroll(rect, element) {
  var subtract = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;

  var scrollTop = getScroll(element, 'top');
  var scrollLeft = getScroll(element, 'left');
  var modifier = subtract ? -1 : 1;
  rect.top += scrollTop * modifier;
  rect.bottom += scrollTop * modifier;
  rect.left += scrollLeft * modifier;
  rect.right += scrollLeft * modifier;
  return rect;
}

/*
 * Helper to detect borders of a given element
 * @method
 * @memberof Popper.Utils
 * @param {CSSStyleDeclaration} styles
 * Result of `getStyleComputedProperty` on the given element
 * @param {String} axis - `x` or `y`
 * @return {number} borders - The borders size of the given axis
 */

function getBordersSize(styles, axis) {
  var sideA = axis === 'x' ? 'Left' : 'Top';
  var sideB = sideA === 'Left' ? 'Right' : 'Bottom';

  return parseFloat(styles['border' + sideA + 'Width']) + parseFloat(styles['border' + sideB + 'Width']);
}

function getSize(axis, body, html, computedStyle) {
  return Math.max(body['offset' + axis], body['scroll' + axis], html['client' + axis], html['offset' + axis], html['scroll' + axis], isIE(10) ? parseInt(html['offset' + axis]) + parseInt(computedStyle['margin' + (axis === 'Height' ? 'Top' : 'Left')]) + parseInt(computedStyle['margin' + (axis === 'Height' ? 'Bottom' : 'Right')]) : 0);
}

function getWindowSizes(document) {
  var body = document.body;
  var html = document.documentElement;
  var computedStyle = isIE(10) && getComputedStyle(html);

  return {
    height: getSize('Height', body, html, computedStyle),
    width: getSize('Width', body, html, computedStyle)
  };
}

var classCallCheck = function (instance, Constructor) {
  if (!(instance instanceof Constructor)) {
    throw new TypeError("Cannot call a class as a function");
  }
};

var createClass = function () {
  function defineProperties(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor) descriptor.writable = true;
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }

  return function (Constructor, protoProps, staticProps) {
    if (protoProps) defineProperties(Constructor.prototype, protoProps);
    if (staticProps) defineProperties(Constructor, staticProps);
    return Constructor;
  };
}();





var defineProperty = function (obj, key, value) {
  if (key in obj) {
    Object.defineProperty(obj, key, {
      value: value,
      enumerable: true,
      configurable: true,
      writable: true
    });
  } else {
    obj[key] = value;
  }

  return obj;
};

var _extends = Object.assign || function (target) {
  for (var i = 1; i < arguments.length; i++) {
    var source = arguments[i];

    for (var key in source) {
      if (Object.prototype.hasOwnProperty.call(source, key)) {
        target[key] = source[key];
      }
    }
  }

  return target;
};

/**
 * Given element offsets, generate an output similar to getBoundingClientRect
 * @method
 * @memberof Popper.Utils
 * @argument {Object} offsets
 * @returns {Object} ClientRect like output
 */
function getClientRect(offsets) {
  return _extends({}, offsets, {
    right: offsets.left + offsets.width,
    bottom: offsets.top + offsets.height
  });
}

/**
 * Get bounding client rect of given element
 * @method
 * @memberof Popper.Utils
 * @param {HTMLElement} element
 * @return {Object} client rect
 */
function getBoundingClientRect(element) {
  var rect = {};

  // IE10 10 FIX: Please, don't ask, the element isn't
  // considered in DOM in some circumstances...
  // This isn't reproducible in IE10 compatibility mode of IE11
  try {
    if (isIE(10)) {
      rect = element.getBoundingClientRect();
      var scrollTop = getScroll(element, 'top');
      var scrollLeft = getScroll(element, 'left');
      rect.top += scrollTop;
      rect.left += scrollLeft;
      rect.bottom += scrollTop;
      rect.right += scrollLeft;
    } else {
      rect = element.getBoundingClientRect();
    }
  } catch (e) {}

  var result = {
    left: rect.left,
    top: rect.top,
    width: rect.right - rect.left,
    height: rect.bottom - rect.top
  };

  // subtract scrollbar size from sizes
  var sizes = element.nodeName === 'HTML' ? getWindowSizes(element.ownerDocument) : {};
  var width = sizes.width || element.clientWidth || result.width;
  var height = sizes.height || element.clientHeight || result.height;

  var horizScrollbar = element.offsetWidth - width;
  var vertScrollbar = element.offsetHeight - height;

  // if an hypothetical scrollbar is detected, we must be sure it's not a `border`
  // we make this check conditional for performance reasons
  if (horizScrollbar || vertScrollbar) {
    var styles = getStyleComputedProperty(element);
    horizScrollbar -= getBordersSize(styles, 'x');
    vertScrollbar -= getBordersSize(styles, 'y');

    result.width -= horizScrollbar;
    result.height -= vertScrollbar;
  }

  return getClientRect(result);
}

function getOffsetRectRelativeToArbitraryNode(children, parent) {
  var fixedPosition = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;

  var isIE10 = isIE(10);
  var isHTML = parent.nodeName === 'HTML';
  var childrenRect = getBoundingClientRect(children);
  var parentRect = getBoundingClientRect(parent);
  var scrollParent = getScrollParent(children);

  var styles = getStyleComputedProperty(parent);
  var borderTopWidth = parseFloat(styles.borderTopWidth);
  var borderLeftWidth = parseFloat(styles.borderLeftWidth);

  // In cases where the parent is fixed, we must ignore negative scroll in offset calc
  if (fixedPosition && isHTML) {
    parentRect.top = Math.max(parentRect.top, 0);
    parentRect.left = Math.max(parentRect.left, 0);
  }
  var offsets = getClientRect({
    top: childrenRect.top - parentRect.top - borderTopWidth,
    left: childrenRect.left - parentRect.left - borderLeftWidth,
    width: childrenRect.width,
    height: childrenRect.height
  });
  offsets.marginTop = 0;
  offsets.marginLeft = 0;

  // Subtract margins of documentElement in case it's being used as parent
  // we do this only on HTML because it's the only element that behaves
  // differently when margins are applied to it. The margins are included in
  // the box of the documentElement, in the other cases not.
  if (!isIE10 && isHTML) {
    var marginTop = parseFloat(styles.marginTop);
    var marginLeft = parseFloat(styles.marginLeft);

    offsets.top -= borderTopWidth - marginTop;
    offsets.bottom -= borderTopWidth - marginTop;
    offsets.left -= borderLeftWidth - marginLeft;
    offsets.right -= borderLeftWidth - marginLeft;

    // Attach marginTop and marginLeft because in some circumstances we may need them
    offsets.marginTop = marginTop;
    offsets.marginLeft = marginLeft;
  }

  if (isIE10 && !fixedPosition ? parent.contains(scrollParent) : parent === scrollParent && scrollParent.nodeName !== 'BODY') {
    offsets = includeScroll(offsets, parent);
  }

  return offsets;
}

function getViewportOffsetRectRelativeToArtbitraryNode(element) {
  var excludeScroll = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

  var html = element.ownerDocument.documentElement;
  var relativeOffset = getOffsetRectRelativeToArbitraryNode(element, html);
  var width = Math.max(html.clientWidth, window.innerWidth || 0);
  var height = Math.max(html.clientHeight, window.innerHeight || 0);

  var scrollTop = !excludeScroll ? getScroll(html) : 0;
  var scrollLeft = !excludeScroll ? getScroll(html, 'left') : 0;

  var offset = {
    top: scrollTop - relativeOffset.top + relativeOffset.marginTop,
    left: scrollLeft - relativeOffset.left + relativeOffset.marginLeft,
    width: width,
    height: height
  };

  return getClientRect(offset);
}

/**
 * Check if the given element is fixed or is inside a fixed parent
 * @method
 * @memberof Popper.Utils
 * @argument {Element} element
 * @argument {Element} customContainer
 * @returns {Boolean} answer to "isFixed?"
 */
function isFixed(element) {
  var nodeName = element.nodeName;
  if (nodeName === 'BODY' || nodeName === 'HTML') {
    return false;
  }
  if (getStyleComputedProperty(element, 'position') === 'fixed') {
    return true;
  }
  var parentNode = getParentNode(element);
  if (!parentNode) {
    return false;
  }
  return isFixed(parentNode);
}

/**
 * Finds the first parent of an element that has a transformed property defined
 * @method
 * @memberof Popper.Utils
 * @argument {Element} element
 * @returns {Element} first transformed parent or documentElement
 */

function getFixedPositionOffsetParent(element) {
  // This check is needed to avoid errors in case one of the elements isn't defined for any reason
  if (!element || !element.parentElement || isIE()) {
    return document.documentElement;
  }
  var el = element.parentElement;
  while (el && getStyleComputedProperty(el, 'transform') === 'none') {
    el = el.parentElement;
  }
  return el || document.documentElement;
}

/**
 * Computed the boundaries limits and return them
 * @method
 * @memberof Popper.Utils
 * @param {HTMLElement} popper
 * @param {HTMLElement} reference
 * @param {number} padding
 * @param {HTMLElement} boundariesElement - Element used to define the boundaries
 * @param {Boolean} fixedPosition - Is in fixed position mode
 * @returns {Object} Coordinates of the boundaries
 */
function getBoundaries(popper, reference, padding, boundariesElement) {
  var fixedPosition = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : false;

  // NOTE: 1 DOM access here

  var boundaries = { top: 0, left: 0 };
  var offsetParent = fixedPosition ? getFixedPositionOffsetParent(popper) : findCommonOffsetParent(popper, getReferenceNode(reference));

  // Handle viewport case
  if (boundariesElement === 'viewport') {
    boundaries = getViewportOffsetRectRelativeToArtbitraryNode(offsetParent, fixedPosition);
  } else {
    // Handle other cases based on DOM element used as boundaries
    var boundariesNode = void 0;
    if (boundariesElement === 'scrollParent') {
      boundariesNode = getScrollParent(getParentNode(reference));
      if (boundariesNode.nodeName === 'BODY') {
        boundariesNode = popper.ownerDocument.documentElement;
      }
    } else if (boundariesElement === 'window') {
      boundariesNode = popper.ownerDocument.documentElement;
    } else {
      boundariesNode = boundariesElement;
    }

    var offsets = getOffsetRectRelativeToArbitraryNode(boundariesNode, offsetParent, fixedPosition);

    // In case of HTML, we need a different computation
    if (boundariesNode.nodeName === 'HTML' && !isFixed(offsetParent)) {
      var _getWindowSizes = getWindowSizes(popper.ownerDocument),
          height = _getWindowSizes.height,
          width = _getWindowSizes.width;

      boundaries.top += offsets.top - offsets.marginTop;
      boundaries.bottom = height + offsets.top;
      boundaries.left += offsets.left - offsets.marginLeft;
      boundaries.right = width + offsets.left;
    } else {
      // for all the other DOM elements, this one is good
      boundaries = offsets;
    }
  }

  // Add paddings
  padding = padding || 0;
  var isPaddingNumber = typeof padding === 'number';
  boundaries.left += isPaddingNumber ? padding : padding.left || 0;
  boundaries.top += isPaddingNumber ? padding : padding.top || 0;
  boundaries.right -= isPaddingNumber ? padding : padding.right || 0;
  boundaries.bottom -= isPaddingNumber ? padding : padding.bottom || 0;

  return boundaries;
}

function getArea(_ref) {
  var width = _ref.width,
      height = _ref.height;

  return width * height;
}

/**
 * Utility used to transform the `auto` placement to the placement with more
 * available space.
 * @method
 * @memberof Popper.Utils
 * @argument {Object} data - The data object generated by update method
 * @argument {Object} options - Modifiers configuration and options
 * @returns {Object} The data object, properly modified
 */
function computeAutoPlacement(placement, refRect, popper, reference, boundariesElement) {
  var padding = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : 0;

  if (placement.indexOf('auto') === -1) {
    return placement;
  }

  var boundaries = getBoundaries(popper, reference, padding, boundariesElement);

  var rects = {
    top: {
      width: boundaries.width,
      height: refRect.top - boundaries.top
    },
    right: {
      width: boundaries.right - refRect.right,
      height: boundaries.height
    },
    bottom: {
      width: boundaries.width,
      height: boundaries.bottom - refRect.bottom
    },
    left: {
      width: refRect.left - boundaries.left,
      height: boundaries.height
    }
  };

  var sortedAreas = Object.keys(rects).map(function (key) {
    return _extends({
      key: key
    }, rects[key], {
      area: getArea(rects[key])
    });
  }).sort(function (a, b) {
    return b.area - a.area;
  });

  var filteredAreas = sortedAreas.filter(function (_ref2) {
    var width = _ref2.width,
        height = _ref2.height;
    return width >= popper.clientWidth && height >= popper.clientHeight;
  });

  var computedPlacement = filteredAreas.length > 0 ? filteredAreas[0].key : sortedAreas[0].key;

  var variation = placement.split('-')[1];

  return computedPlacement + (variation ? '-' + variation : '');
}

/**
 * Get offsets to the reference element
 * @method
 * @memberof Popper.Utils
 * @param {Object} state
 * @param {Element} popper - the popper element
 * @param {Element} reference - the reference element (the popper will be relative to this)
 * @param {Element} fixedPosition - is in fixed position mode
 * @returns {Object} An object containing the offsets which will be applied to the popper
 */
function getReferenceOffsets(state, popper, reference) {
  var fixedPosition = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : null;

  var commonOffsetParent = fixedPosition ? getFixedPositionOffsetParent(popper) : findCommonOffsetParent(popper, getReferenceNode(reference));
  return getOffsetRectRelativeToArbitraryNode(reference, commonOffsetParent, fixedPosition);
}

/**
 * Get the outer sizes of the given element (offset size + margins)
 * @method
 * @memberof Popper.Utils
 * @argument {Element} element
 * @returns {Object} object containing width and height properties
 */
function getOuterSizes(element) {
  var window = element.ownerDocument.defaultView;
  var styles = window.getComputedStyle(element);
  var x = parseFloat(styles.marginTop || 0) + parseFloat(styles.marginBottom || 0);
  var y = parseFloat(styles.marginLeft || 0) + parseFloat(styles.marginRight || 0);
  var result = {
    width: element.offsetWidth + y,
    height: element.offsetHeight + x
  };
  return result;
}

/**
 * Get the opposite placement of the given one
 * @method
 * @memberof Popper.Utils
 * @argument {String} placement
 * @returns {String} flipped placement
 */
function getOppositePlacement(placement) {
  var hash = { left: 'right', right: 'left', bottom: 'top', top: 'bottom' };
  return placement.replace(/left|right|bottom|top/g, function (matched) {
    return hash[matched];
  });
}

/**
 * Get offsets to the popper
 * @method
 * @memberof Popper.Utils
 * @param {Object} position - CSS position the Popper will get applied
 * @param {HTMLElement} popper - the popper element
 * @param {Object} referenceOffsets - the reference offsets (the popper will be relative to this)
 * @param {String} placement - one of the valid placement options
 * @returns {Object} popperOffsets - An object containing the offsets which will be applied to the popper
 */
function getPopperOffsets(popper, referenceOffsets, placement) {
  placement = placement.split('-')[0];

  // Get popper node sizes
  var popperRect = getOuterSizes(popper);

  // Add position, width and height to our offsets object
  var popperOffsets = {
    width: popperRect.width,
    height: popperRect.height
  };

  // depending by the popper placement we have to compute its offsets slightly differently
  var isHoriz = ['right', 'left'].indexOf(placement) !== -1;
  var mainSide = isHoriz ? 'top' : 'left';
  var secondarySide = isHoriz ? 'left' : 'top';
  var measurement = isHoriz ? 'height' : 'width';
  var secondaryMeasurement = !isHoriz ? 'height' : 'width';

  popperOffsets[mainSide] = referenceOffsets[mainSide] + referenceOffsets[measurement] / 2 - popperRect[measurement] / 2;
  if (placement === secondarySide) {
    popperOffsets[secondarySide] = referenceOffsets[secondarySide] - popperRect[secondaryMeasurement];
  } else {
    popperOffsets[secondarySide] = referenceOffsets[getOppositePlacement(secondarySide)];
  }

  return popperOffsets;
}

/**
 * Mimics the `find` method of Array
 * @method
 * @memberof Popper.Utils
 * @argument {Array} arr
 * @argument prop
 * @argument value
 * @returns index or -1
 */
function find(arr, check) {
  // use native find if supported
  if (Array.prototype.find) {
    return arr.find(check);
  }

  // use `filter` to obtain the same behavior of `find`
  return arr.filter(check)[0];
}

/**
 * Return the index of the matching object
 * @method
 * @memberof Popper.Utils
 * @argument {Array} arr
 * @argument prop
 * @argument value
 * @returns index or -1
 */
function findIndex(arr, prop, value) {
  // use native findIndex if supported
  if (Array.prototype.findIndex) {
    return arr.findIndex(function (cur) {
      return cur[prop] === value;
    });
  }

  // use `find` + `indexOf` if `findIndex` isn't supported
  var match = find(arr, function (obj) {
    return obj[prop] === value;
  });
  return arr.indexOf(match);
}

/**
 * Loop trough the list of modifiers and run them in order,
 * each of them will then edit the data object.
 * @method
 * @memberof Popper.Utils
 * @param {dataObject} data
 * @param {Array} modifiers
 * @param {String} ends - Optional modifier name used as stopper
 * @returns {dataObject}
 */
function runModifiers(modifiers, data, ends) {
  var modifiersToRun = ends === undefined ? modifiers : modifiers.slice(0, findIndex(modifiers, 'name', ends));

  modifiersToRun.forEach(function (modifier) {
    if (modifier['function']) {
      // eslint-disable-line dot-notation
      console.warn('`modifier.function` is deprecated, use `modifier.fn`!');
    }
    var fn = modifier['function'] || modifier.fn; // eslint-disable-line dot-notation
    if (modifier.enabled && isFunction(fn)) {
      // Add properties to offsets to make them a complete clientRect object
      // we do this before each modifier to make sure the previous one doesn't
      // mess with these values
      data.offsets.popper = getClientRect(data.offsets.popper);
      data.offsets.reference = getClientRect(data.offsets.reference);

      data = fn(data, modifier);
    }
  });

  return data;
}

/**
 * Updates the position of the popper, computing the new offsets and applying
 * the new style.<br />
 * Prefer `scheduleUpdate` over `update` because of performance reasons.
 * @method
 * @memberof Popper
 */
function update() {
  // if popper is destroyed, don't perform any further update
  if (this.state.isDestroyed) {
    return;
  }

  var data = {
    instance: this,
    styles: {},
    arrowStyles: {},
    attributes: {},
    flipped: false,
    offsets: {}
  };

  // compute reference element offsets
  data.offsets.reference = getReferenceOffsets(this.state, this.popper, this.reference, this.options.positionFixed);

  // compute auto placement, store placement inside the data object,
  // modifiers will be able to edit `placement` if needed
  // and refer to originalPlacement to know the original value
  data.placement = computeAutoPlacement(this.options.placement, data.offsets.reference, this.popper, this.reference, this.options.modifiers.flip.boundariesElement, this.options.modifiers.flip.padding);

  // store the computed placement inside `originalPlacement`
  data.originalPlacement = data.placement;

  data.positionFixed = this.options.positionFixed;

  // compute the popper offsets
  data.offsets.popper = getPopperOffsets(this.popper, data.offsets.reference, data.placement);

  data.offsets.popper.position = this.options.positionFixed ? 'fixed' : 'absolute';

  // run the modifiers
  data = runModifiers(this.modifiers, data);

  // the first `update` will call `onCreate` callback
  // the other ones will call `onUpdate` callback
  if (!this.state.isCreated) {
    this.state.isCreated = true;
    this.options.onCreate(data);
  } else {
    this.options.onUpdate(data);
  }
}

/**
 * Helper used to know if the given modifier is enabled.
 * @method
 * @memberof Popper.Utils
 * @returns {Boolean}
 */
function isModifierEnabled(modifiers, modifierName) {
  return modifiers.some(function (_ref) {
    var name = _ref.name,
        enabled = _ref.enabled;
    return enabled && name === modifierName;
  });
}

/**
 * Get the prefixed supported property name
 * @method
 * @memberof Popper.Utils
 * @argument {String} property (camelCase)
 * @returns {String} prefixed property (camelCase or PascalCase, depending on the vendor prefix)
 */
function getSupportedPropertyName(property) {
  var prefixes = [false, 'ms', 'Webkit', 'Moz', 'O'];
  var upperProp = property.charAt(0).toUpperCase() + property.slice(1);

  for (var i = 0; i < prefixes.length; i++) {
    var prefix = prefixes[i];
    var toCheck = prefix ? '' + prefix + upperProp : property;
    if (typeof document.body.style[toCheck] !== 'undefined') {
      return toCheck;
    }
  }
  return null;
}

/**
 * Destroys the popper.
 * @method
 * @memberof Popper
 */
function destroy() {
  this.state.isDestroyed = true;

  // touch DOM only if `applyStyle` modifier is enabled
  if (isModifierEnabled(this.modifiers, 'applyStyle')) {
    this.popper.removeAttribute('x-placement');
    this.popper.style.position = '';
    this.popper.style.top = '';
    this.popper.style.left = '';
    this.popper.style.right = '';
    this.popper.style.bottom = '';
    this.popper.style.willChange = '';
    this.popper.style[getSupportedPropertyName('transform')] = '';
  }

  this.disableEventListeners();

  // remove the popper if user explicitly asked for the deletion on destroy
  // do not use `remove` because IE11 doesn't support it
  if (this.options.removeOnDestroy) {
    this.popper.parentNode.removeChild(this.popper);
  }
  return this;
}

/**
 * Get the window associated with the element
 * @argument {Element} element
 * @returns {Window}
 */
function getWindow(element) {
  var ownerDocument = element.ownerDocument;
  return ownerDocument ? ownerDocument.defaultView : window;
}

function attachToScrollParents(scrollParent, event, callback, scrollParents) {
  var isBody = scrollParent.nodeName === 'BODY';
  var target = isBody ? scrollParent.ownerDocument.defaultView : scrollParent;
  target.addEventListener(event, callback, { passive: true });

  if (!isBody) {
    attachToScrollParents(getScrollParent(target.parentNode), event, callback, scrollParents);
  }
  scrollParents.push(target);
}

/**
 * Setup needed event listeners used to update the popper position
 * @method
 * @memberof Popper.Utils
 * @private
 */
function setupEventListeners(reference, options, state, updateBound) {
  // Resize event listener on window
  state.updateBound = updateBound;
  getWindow(reference).addEventListener('resize', state.updateBound, { passive: true });

  // Scroll event listener on scroll parents
  var scrollElement = getScrollParent(reference);
  attachToScrollParents(scrollElement, 'scroll', state.updateBound, state.scrollParents);
  state.scrollElement = scrollElement;
  state.eventsEnabled = true;

  return state;
}

/**
 * It will add resize/scroll events and start recalculating
 * position of the popper element when they are triggered.
 * @method
 * @memberof Popper
 */
function enableEventListeners() {
  if (!this.state.eventsEnabled) {
    this.state = setupEventListeners(this.reference, this.options, this.state, this.scheduleUpdate);
  }
}

/**
 * Remove event listeners used to update the popper position
 * @method
 * @memberof Popper.Utils
 * @private
 */
function removeEventListeners(reference, state) {
  // Remove resize event listener on window
  getWindow(reference).removeEventListener('resize', state.updateBound);

  // Remove scroll event listener on scroll parents
  state.scrollParents.forEach(function (target) {
    target.removeEventListener('scroll', state.updateBound);
  });

  // Reset state
  state.updateBound = null;
  state.scrollParents = [];
  state.scrollElement = null;
  state.eventsEnabled = false;
  return state;
}

/**
 * It will remove resize/scroll events and won't recalculate popper position
 * when they are triggered. It also won't trigger `onUpdate` callback anymore,
 * unless you call `update` method manually.
 * @method
 * @memberof Popper
 */
function disableEventListeners() {
  if (this.state.eventsEnabled) {
    cancelAnimationFrame(this.scheduleUpdate);
    this.state = removeEventListeners(this.reference, this.state);
  }
}

/**
 * Tells if a given input is a number
 * @method
 * @memberof Popper.Utils
 * @param {*} input to check
 * @return {Boolean}
 */
function isNumeric(n) {
  return n !== '' && !isNaN(parseFloat(n)) && isFinite(n);
}

/**
 * Set the style to the given popper
 * @method
 * @memberof Popper.Utils
 * @argument {Element} element - Element to apply the style to
 * @argument {Object} styles
 * Object with a list of properties and values which will be applied to the element
 */
function setStyles(element, styles) {
  Object.keys(styles).forEach(function (prop) {
    var unit = '';
    // add unit if the value is numeric and is one of the following
    if (['width', 'height', 'top', 'right', 'bottom', 'left'].indexOf(prop) !== -1 && isNumeric(styles[prop])) {
      unit = 'px';
    }
    element.style[prop] = styles[prop] + unit;
  });
}

/**
 * Set the attributes to the given popper
 * @method
 * @memberof Popper.Utils
 * @argument {Element} element - Element to apply the attributes to
 * @argument {Object} styles
 * Object with a list of properties and values which will be applied to the element
 */
function setAttributes(element, attributes) {
  Object.keys(attributes).forEach(function (prop) {
    var value = attributes[prop];
    if (value !== false) {
      element.setAttribute(prop, attributes[prop]);
    } else {
      element.removeAttribute(prop);
    }
  });
}

/**
 * @function
 * @memberof Modifiers
 * @argument {Object} data - The data object generated by `update` method
 * @argument {Object} data.styles - List of style properties - values to apply to popper element
 * @argument {Object} data.attributes - List of attribute properties - values to apply to popper element
 * @argument {Object} options - Modifiers configuration and options
 * @returns {Object} The same data object
 */
function applyStyle(data) {
  // any property present in `data.styles` will be applied to the popper,
  // in this way we can make the 3rd party modifiers add custom styles to it
  // Be aware, modifiers could override the properties defined in the previous
  // lines of this modifier!
  setStyles(data.instance.popper, data.styles);

  // any property present in `data.attributes` will be applied to the popper,
  // they will be set as HTML attributes of the element
  setAttributes(data.instance.popper, data.attributes);

  // if arrowElement is defined and arrowStyles has some properties
  if (data.arrowElement && Object.keys(data.arrowStyles).length) {
    setStyles(data.arrowElement, data.arrowStyles);
  }

  return data;
}

/**
 * Set the x-placement attribute before everything else because it could be used
 * to add margins to the popper margins needs to be calculated to get the
 * correct popper offsets.
 * @method
 * @memberof Popper.modifiers
 * @param {HTMLElement} reference - The reference element used to position the popper
 * @param {HTMLElement} popper - The HTML element used as popper
 * @param {Object} options - Popper.js options
 */
function applyStyleOnLoad(reference, popper, options, modifierOptions, state) {
  // compute reference element offsets
  var referenceOffsets = getReferenceOffsets(state, popper, reference, options.positionFixed);

  // compute auto placement, store placement inside the data object,
  // modifiers will be able to edit `placement` if needed
  // and refer to originalPlacement to know the original value
  var placement = computeAutoPlacement(options.placement, referenceOffsets, popper, reference, options.modifiers.flip.boundariesElement, options.modifiers.flip.padding);

  popper.setAttribute('x-placement', placement);

  // Apply `position` to popper before anything else because
  // without the position applied we can't guarantee correct computations
  setStyles(popper, { position: options.positionFixed ? 'fixed' : 'absolute' });

  return options;
}

/**
 * @function
 * @memberof Popper.Utils
 * @argument {Object} data - The data object generated by `update` method
 * @argument {Boolean} shouldRound - If the offsets should be rounded at all
 * @returns {Object} The popper's position offsets rounded
 *
 * The tale of pixel-perfect positioning. It's still not 100% perfect, but as
 * good as it can be within reason.
 * Discussion here: https://github.com/FezVrasta/popper.js/pull/715
 *
 * Low DPI screens cause a popper to be blurry if not using full pixels (Safari
 * as well on High DPI screens).
 *
 * Firefox prefers no rounding for positioning and does not have blurriness on
 * high DPI screens.
 *
 * Only horizontal placement and left/right values need to be considered.
 */
function getRoundedOffsets(data, shouldRound) {
  var _data$offsets = data.offsets,
      popper = _data$offsets.popper,
      reference = _data$offsets.reference;
  var round = Math.round,
      floor = Math.floor;

  var noRound = function noRound(v) {
    return v;
  };

  var referenceWidth = round(reference.width);
  var popperWidth = round(popper.width);

  var isVertical = ['left', 'right'].indexOf(data.placement) !== -1;
  var isVariation = data.placement.indexOf('-') !== -1;
  var sameWidthParity = referenceWidth % 2 === popperWidth % 2;
  var bothOddWidth = referenceWidth % 2 === 1 && popperWidth % 2 === 1;

  var horizontalToInteger = !shouldRound ? noRound : isVertical || isVariation || sameWidthParity ? round : floor;
  var verticalToInteger = !shouldRound ? noRound : round;

  return {
    left: horizontalToInteger(bothOddWidth && !isVariation && shouldRound ? popper.left - 1 : popper.left),
    top: verticalToInteger(popper.top),
    bottom: verticalToInteger(popper.bottom),
    right: horizontalToInteger(popper.right)
  };
}

var isFirefox = isBrowser && /Firefox/i.test(navigator.userAgent);

/**
 * @function
 * @memberof Modifiers
 * @argument {Object} data - The data object generated by `update` method
 * @argument {Object} options - Modifiers configuration and options
 * @returns {Object} The data object, properly modified
 */
function computeStyle(data, options) {
  var x = options.x,
      y = options.y;
  var popper = data.offsets.popper;

  // Remove this legacy support in Popper.js v2

  var legacyGpuAccelerationOption = find(data.instance.modifiers, function (modifier) {
    return modifier.name === 'applyStyle';
  }).gpuAcceleration;
  if (legacyGpuAccelerationOption !== undefined) {
    console.warn('WARNING: `gpuAcceleration` option moved to `computeStyle` modifier and will not be supported in future versions of Popper.js!');
  }
  var gpuAcceleration = legacyGpuAccelerationOption !== undefined ? legacyGpuAccelerationOption : options.gpuAcceleration;

  var offsetParent = getOffsetParent(data.instance.popper);
  var offsetParentRect = getBoundingClientRect(offsetParent);

  // Styles
  var styles = {
    position: popper.position
  };

  var offsets = getRoundedOffsets(data, window.devicePixelRatio < 2 || !isFirefox);

  var sideA = x === 'bottom' ? 'top' : 'bottom';
  var sideB = y === 'right' ? 'left' : 'right';

  // if gpuAcceleration is set to `true` and transform is supported,
  //  we use `translate3d` to apply the position to the popper we
  // automatically use the supported prefixed version if needed
  var prefixedProperty = getSupportedPropertyName('transform');

  // now, let's make a step back and look at this code closely (wtf?)
  // If the content of the popper grows once it's been positioned, it
  // may happen that the popper gets misplaced because of the new content
  // overflowing its reference element
  // To avoid this problem, we provide two options (x and y), which allow
  // the consumer to define the offset origin.
  // If we position a popper on top of a reference element, we can set
  // `x` to `top` to make the popper grow towards its top instead of
  // its bottom.
  var left = void 0,
      top = void 0;
  if (sideA === 'bottom') {
    // when offsetParent is <html> the positioning is relative to the bottom of the screen (excluding the scrollbar)
    // and not the bottom of the html element
    if (offsetParent.nodeName === 'HTML') {
      top = -offsetParent.clientHeight + offsets.bottom;
    } else {
      top = -offsetParentRect.height + offsets.bottom;
    }
  } else {
    top = offsets.top;
  }
  if (sideB === 'right') {
    if (offsetParent.nodeName === 'HTML') {
      left = -offsetParent.clientWidth + offsets.right;
    } else {
      left = -offsetParentRect.width + offsets.right;
    }
  } else {
    left = offsets.left;
  }
  if (gpuAcceleration && prefixedProperty) {
    styles[prefixedProperty] = 'translate3d(' + left + 'px, ' + top + 'px, 0)';
    styles[sideA] = 0;
    styles[sideB] = 0;
    styles.willChange = 'transform';
  } else {
    // othwerise, we use the standard `top`, `left`, `bottom` and `right` properties
    var invertTop = sideA === 'bottom' ? -1 : 1;
    var invertLeft = sideB === 'right' ? -1 : 1;
    styles[sideA] = top * invertTop;
    styles[sideB] = left * invertLeft;
    styles.willChange = sideA + ', ' + sideB;
  }

  // Attributes
  var attributes = {
    'x-placement': data.placement
  };

  // Update `data` attributes, styles and arrowStyles
  data.attributes = _extends({}, attributes, data.attributes);
  data.styles = _extends({}, styles, data.styles);
  data.arrowStyles = _extends({}, data.offsets.arrow, data.arrowStyles);

  return data;
}

/**
 * Helper used to know if the given modifier depends from another one.<br />
 * It checks if the needed modifier is listed and enabled.
 * @method
 * @memberof Popper.Utils
 * @param {Array} modifiers - list of modifiers
 * @param {String} requestingName - name of requesting modifier
 * @param {String} requestedName - name of requested modifier
 * @returns {Boolean}
 */
function isModifierRequired(modifiers, requestingName, requestedName) {
  var requesting = find(modifiers, function (_ref) {
    var name = _ref.name;
    return name === requestingName;
  });

  var isRequired = !!requesting && modifiers.some(function (modifier) {
    return modifier.name === requestedName && modifier.enabled && modifier.order < requesting.order;
  });

  if (!isRequired) {
    var _requesting = '`' + requestingName + '`';
    var requested = '`' + requestedName + '`';
    console.warn(requested + ' modifier is required by ' + _requesting + ' modifier in order to work, be sure to include it before ' + _requesting + '!');
  }
  return isRequired;
}

/**
 * @function
 * @memberof Modifiers
 * @argument {Object} data - The data object generated by update method
 * @argument {Object} options - Modifiers configuration and options
 * @returns {Object} The data object, properly modified
 */
function arrow(data, options) {
  var _data$offsets$arrow;

  // arrow depends on keepTogether in order to work
  if (!isModifierRequired(data.instance.modifiers, 'arrow', 'keepTogether')) {
    return data;
  }

  var arrowElement = options.element;

  // if arrowElement is a string, suppose it's a CSS selector
  if (typeof arrowElement === 'string') {
    arrowElement = data.instance.popper.querySelector(arrowElement);

    // if arrowElement is not found, don't run the modifier
    if (!arrowElement) {
      return data;
    }
  } else {
    // if the arrowElement isn't a query selector we must check that the
    // provided DOM node is child of its popper node
    if (!data.instance.popper.contains(arrowElement)) {
      console.warn('WARNING: `arrow.element` must be child of its popper element!');
      return data;
    }
  }

  var placement = data.placement.split('-')[0];
  var _data$offsets = data.offsets,
      popper = _data$offsets.popper,
      reference = _data$offsets.reference;

  var isVertical = ['left', 'right'].indexOf(placement) !== -1;

  var len = isVertical ? 'height' : 'width';
  var sideCapitalized = isVertical ? 'Top' : 'Left';
  var side = sideCapitalized.toLowerCase();
  var altSide = isVertical ? 'left' : 'top';
  var opSide = isVertical ? 'bottom' : 'right';
  var arrowElementSize = getOuterSizes(arrowElement)[len];

  //
  // extends keepTogether behavior making sure the popper and its
  // reference have enough pixels in conjunction
  //

  // top/left side
  if (reference[opSide] - arrowElementSize < popper[side]) {
    data.offsets.popper[side] -= popper[side] - (reference[opSide] - arrowElementSize);
  }
  // bottom/right side
  if (reference[side] + arrowElementSize > popper[opSide]) {
    data.offsets.popper[side] += reference[side] + arrowElementSize - popper[opSide];
  }
  data.offsets.popper = getClientRect(data.offsets.popper);

  // compute center of the popper
  var center = reference[side] + reference[len] / 2 - arrowElementSize / 2;

  // Compute the sideValue using the updated popper offsets
  // take popper margin in account because we don't have this info available
  var css = getStyleComputedProperty(data.instance.popper);
  var popperMarginSide = parseFloat(css['margin' + sideCapitalized]);
  var popperBorderSide = parseFloat(css['border' + sideCapitalized + 'Width']);
  var sideValue = center - data.offsets.popper[side] - popperMarginSide - popperBorderSide;

  // prevent arrowElement from being placed not contiguously to its popper
  sideValue = Math.max(Math.min(popper[len] - arrowElementSize, sideValue), 0);

  data.arrowElement = arrowElement;
  data.offsets.arrow = (_data$offsets$arrow = {}, defineProperty(_data$offsets$arrow, side, Math.round(sideValue)), defineProperty(_data$offsets$arrow, altSide, ''), _data$offsets$arrow);

  return data;
}

/**
 * Get the opposite placement variation of the given one
 * @method
 * @memberof Popper.Utils
 * @argument {String} placement variation
 * @returns {String} flipped placement variation
 */
function getOppositeVariation(variation) {
  if (variation === 'end') {
    return 'start';
  } else if (variation === 'start') {
    return 'end';
  }
  return variation;
}

/**
 * List of accepted placements to use as values of the `placement` option.<br />
 * Valid placements are:
 * - `auto`
 * - `top`
 * - `right`
 * - `bottom`
 * - `left`
 *
 * Each placement can have a variation from this list:
 * - `-start`
 * - `-end`
 *
 * Variations are interpreted easily if you think of them as the left to right
 * written languages. Horizontally (`top` and `bottom`), `start` is left and `end`
 * is right.<br />
 * Vertically (`left` and `right`), `start` is top and `end` is bottom.
 *
 * Some valid examples are:
 * - `top-end` (on top of reference, right aligned)
 * - `right-start` (on right of reference, top aligned)
 * - `bottom` (on bottom, centered)
 * - `auto-end` (on the side with more space available, alignment depends by placement)
 *
 * @static
 * @type {Array}
 * @enum {String}
 * @readonly
 * @method placements
 * @memberof Popper
 */
var placements = ['auto-start', 'auto', 'auto-end', 'top-start', 'top', 'top-end', 'right-start', 'right', 'right-end', 'bottom-end', 'bottom', 'bottom-start', 'left-end', 'left', 'left-start'];

// Get rid of `auto` `auto-start` and `auto-end`
var validPlacements = placements.slice(3);

/**
 * Given an initial placement, returns all the subsequent placements
 * clockwise (or counter-clockwise).
 *
 * @method
 * @memberof Popper.Utils
 * @argument {String} placement - A valid placement (it accepts variations)
 * @argument {Boolean} counter - Set to true to walk the placements counterclockwise
 * @returns {Array} placements including their variations
 */
function clockwise(placement) {
  var counter = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

  var index = validPlacements.indexOf(placement);
  var arr = validPlacements.slice(index + 1).concat(validPlacements.slice(0, index));
  return counter ? arr.reverse() : arr;
}

var BEHAVIORS = {
  FLIP: 'flip',
  CLOCKWISE: 'clockwise',
  COUNTERCLOCKWISE: 'counterclockwise'
};

/**
 * @function
 * @memberof Modifiers
 * @argument {Object} data - The data object generated by update method
 * @argument {Object} options - Modifiers configuration and options
 * @returns {Object} The data object, properly modified
 */
function flip(data, options) {
  // if `inner` modifier is enabled, we can't use the `flip` modifier
  if (isModifierEnabled(data.instance.modifiers, 'inner')) {
    return data;
  }

  if (data.flipped && data.placement === data.originalPlacement) {
    // seems like flip is trying to loop, probably there's not enough space on any of the flippable sides
    return data;
  }

  var boundaries = getBoundaries(data.instance.popper, data.instance.reference, options.padding, options.boundariesElement, data.positionFixed);

  var placement = data.placement.split('-')[0];
  var placementOpposite = getOppositePlacement(placement);
  var variation = data.placement.split('-')[1] || '';

  var flipOrder = [];

  switch (options.behavior) {
    case BEHAVIORS.FLIP:
      flipOrder = [placement, placementOpposite];
      break;
    case BEHAVIORS.CLOCKWISE:
      flipOrder = clockwise(placement);
      break;
    case BEHAVIORS.COUNTERCLOCKWISE:
      flipOrder = clockwise(placement, true);
      break;
    default:
      flipOrder = options.behavior;
  }

  flipOrder.forEach(function (step, index) {
    if (placement !== step || flipOrder.length === index + 1) {
      return data;
    }

    placement = data.placement.split('-')[0];
    placementOpposite = getOppositePlacement(placement);

    var popperOffsets = data.offsets.popper;
    var refOffsets = data.offsets.reference;

    // using floor because the reference offsets may contain decimals we are not going to consider here
    var floor = Math.floor;
    var overlapsRef = placement === 'left' && floor(popperOffsets.right) > floor(refOffsets.left) || placement === 'right' && floor(popperOffsets.left) < floor(refOffsets.right) || placement === 'top' && floor(popperOffsets.bottom) > floor(refOffsets.top) || placement === 'bottom' && floor(popperOffsets.top) < floor(refOffsets.bottom);

    var overflowsLeft = floor(popperOffsets.left) < floor(boundaries.left);
    var overflowsRight = floor(popperOffsets.right) > floor(boundaries.right);
    var overflowsTop = floor(popperOffsets.top) < floor(boundaries.top);
    var overflowsBottom = floor(popperOffsets.bottom) > floor(boundaries.bottom);

    var overflowsBoundaries = placement === 'left' && overflowsLeft || placement === 'right' && overflowsRight || placement === 'top' && overflowsTop || placement === 'bottom' && overflowsBottom;

    // flip the variation if required
    var isVertical = ['top', 'bottom'].indexOf(placement) !== -1;

    // flips variation if reference element overflows boundaries
    var flippedVariationByRef = !!options.flipVariations && (isVertical && variation === 'start' && overflowsLeft || isVertical && variation === 'end' && overflowsRight || !isVertical && variation === 'start' && overflowsTop || !isVertical && variation === 'end' && overflowsBottom);

    // flips variation if popper content overflows boundaries
    var flippedVariationByContent = !!options.flipVariationsByContent && (isVertical && variation === 'start' && overflowsRight || isVertical && variation === 'end' && overflowsLeft || !isVertical && variation === 'start' && overflowsBottom || !isVertical && variation === 'end' && overflowsTop);

    var flippedVariation = flippedVariationByRef || flippedVariationByContent;

    if (overlapsRef || overflowsBoundaries || flippedVariation) {
      // this boolean to detect any flip loop
      data.flipped = true;

      if (overlapsRef || overflowsBoundaries) {
        placement = flipOrder[index + 1];
      }

      if (flippedVariation) {
        variation = getOppositeVariation(variation);
      }

      data.placement = placement + (variation ? '-' + variation : '');

      // this object contains `position`, we want to preserve it along with
      // any additional property we may add in the future
      data.offsets.popper = _extends({}, data.offsets.popper, getPopperOffsets(data.instance.popper, data.offsets.reference, data.placement));

      data = runModifiers(data.instance.modifiers, data, 'flip');
    }
  });
  return data;
}

/**
 * @function
 * @memberof Modifiers
 * @argument {Object} data - The data object generated by update method
 * @argument {Object} options - Modifiers configuration and options
 * @returns {Object} The data object, properly modified
 */
function keepTogether(data) {
  var _data$offsets = data.offsets,
      popper = _data$offsets.popper,
      reference = _data$offsets.reference;

  var placement = data.placement.split('-')[0];
  var floor = Math.floor;
  var isVertical = ['top', 'bottom'].indexOf(placement) !== -1;
  var side = isVertical ? 'right' : 'bottom';
  var opSide = isVertical ? 'left' : 'top';
  var measurement = isVertical ? 'width' : 'height';

  if (popper[side] < floor(reference[opSide])) {
    data.offsets.popper[opSide] = floor(reference[opSide]) - popper[measurement];
  }
  if (popper[opSide] > floor(reference[side])) {
    data.offsets.popper[opSide] = floor(reference[side]);
  }

  return data;
}

/**
 * Converts a string containing value + unit into a px value number
 * @function
 * @memberof {modifiers~offset}
 * @private
 * @argument {String} str - Value + unit string
 * @argument {String} measurement - `height` or `width`
 * @argument {Object} popperOffsets
 * @argument {Object} referenceOffsets
 * @returns {Number|String}
 * Value in pixels, or original string if no values were extracted
 */
function toValue(str, measurement, popperOffsets, referenceOffsets) {
  // separate value from unit
  var split = str.match(/((?:\-|\+)?\d*\.?\d*)(.*)/);
  var value = +split[1];
  var unit = split[2];

  // If it's not a number it's an operator, I guess
  if (!value) {
    return str;
  }

  if (unit.indexOf('%') === 0) {
    var element = void 0;
    switch (unit) {
      case '%p':
        element = popperOffsets;
        break;
      case '%':
      case '%r':
      default:
        element = referenceOffsets;
    }

    var rect = getClientRect(element);
    return rect[measurement] / 100 * value;
  } else if (unit === 'vh' || unit === 'vw') {
    // if is a vh or vw, we calculate the size based on the viewport
    var size = void 0;
    if (unit === 'vh') {
      size = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
    } else {
      size = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
    }
    return size / 100 * value;
  } else {
    // if is an explicit pixel unit, we get rid of the unit and keep the value
    // if is an implicit unit, it's px, and we return just the value
    return value;
  }
}

/**
 * Parse an `offset` string to extrapolate `x` and `y` numeric offsets.
 * @function
 * @memberof {modifiers~offset}
 * @private
 * @argument {String} offset
 * @argument {Object} popperOffsets
 * @argument {Object} referenceOffsets
 * @argument {String} basePlacement
 * @returns {Array} a two cells array with x and y offsets in numbers
 */
function parseOffset(offset, popperOffsets, referenceOffsets, basePlacement) {
  var offsets = [0, 0];

  // Use height if placement is left or right and index is 0 otherwise use width
  // in this way the first offset will use an axis and the second one
  // will use the other one
  var useHeight = ['right', 'left'].indexOf(basePlacement) !== -1;

  // Split the offset string to obtain a list of values and operands
  // The regex addresses values with the plus or minus sign in front (+10, -20, etc)
  var fragments = offset.split(/(\+|\-)/).map(function (frag) {
    return frag.trim();
  });

  // Detect if the offset string contains a pair of values or a single one
  // they could be separated by comma or space
  var divider = fragments.indexOf(find(fragments, function (frag) {
    return frag.search(/,|\s/) !== -1;
  }));

  if (fragments[divider] && fragments[divider].indexOf(',') === -1) {
    console.warn('Offsets separated by white space(s) are deprecated, use a comma (,) instead.');
  }

  // If divider is found, we divide the list of values and operands to divide
  // them by ofset X and Y.
  var splitRegex = /\s*,\s*|\s+/;
  var ops = divider !== -1 ? [fragments.slice(0, divider).concat([fragments[divider].split(splitRegex)[0]]), [fragments[divider].split(splitRegex)[1]].concat(fragments.slice(divider + 1))] : [fragments];

  // Convert the values with units to absolute pixels to allow our computations
  ops = ops.map(function (op, index) {
    // Most of the units rely on the orientation of the popper
    var measurement = (index === 1 ? !useHeight : useHeight) ? 'height' : 'width';
    var mergeWithPrevious = false;
    return op
    // This aggregates any `+` or `-` sign that aren't considered operators
    // e.g.: 10 + +5 => [10, +, +5]
    .reduce(function (a, b) {
      if (a[a.length - 1] === '' && ['+', '-'].indexOf(b) !== -1) {
        a[a.length - 1] = b;
        mergeWithPrevious = true;
        return a;
      } else if (mergeWithPrevious) {
        a[a.length - 1] += b;
        mergeWithPrevious = false;
        return a;
      } else {
        return a.concat(b);
      }
    }, [])
    // Here we convert the string values into number values (in px)
    .map(function (str) {
      return toValue(str, measurement, popperOffsets, referenceOffsets);
    });
  });

  // Loop trough the offsets arrays and execute the operations
  ops.forEach(function (op, index) {
    op.forEach(function (frag, index2) {
      if (isNumeric(frag)) {
        offsets[index] += frag * (op[index2 - 1] === '-' ? -1 : 1);
      }
    });
  });
  return offsets;
}

/**
 * @function
 * @memberof Modifiers
 * @argument {Object} data - The data object generated by update method
 * @argument {Object} options - Modifiers configuration and options
 * @argument {Number|String} options.offset=0
 * The offset value as described in the modifier description
 * @returns {Object} The data object, properly modified
 */
function offset(data, _ref) {
  var offset = _ref.offset;
  var placement = data.placement,
      _data$offsets = data.offsets,
      popper = _data$offsets.popper,
      reference = _data$offsets.reference;

  var basePlacement = placement.split('-')[0];

  var offsets = void 0;
  if (isNumeric(+offset)) {
    offsets = [+offset, 0];
  } else {
    offsets = parseOffset(offset, popper, reference, basePlacement);
  }

  if (basePlacement === 'left') {
    popper.top += offsets[0];
    popper.left -= offsets[1];
  } else if (basePlacement === 'right') {
    popper.top += offsets[0];
    popper.left += offsets[1];
  } else if (basePlacement === 'top') {
    popper.left += offsets[0];
    popper.top -= offsets[1];
  } else if (basePlacement === 'bottom') {
    popper.left += offsets[0];
    popper.top += offsets[1];
  }

  data.popper = popper;
  return data;
}

/**
 * @function
 * @memberof Modifiers
 * @argument {Object} data - The data object generated by `update` method
 * @argument {Object} options - Modifiers configuration and options
 * @returns {Object} The data object, properly modified
 */
function preventOverflow(data, options) {
  var boundariesElement = options.boundariesElement || getOffsetParent(data.instance.popper);

  // If offsetParent is the reference element, we really want to
  // go one step up and use the next offsetParent as reference to
  // avoid to make this modifier completely useless and look like broken
  if (data.instance.reference === boundariesElement) {
    boundariesElement = getOffsetParent(boundariesElement);
  }

  // NOTE: DOM access here
  // resets the popper's position so that the document size can be calculated excluding
  // the size of the popper element itself
  var transformProp = getSupportedPropertyName('transform');
  var popperStyles = data.instance.popper.style; // assignment to help minification
  var top = popperStyles.top,
      left = popperStyles.left,
      transform = popperStyles[transformProp];

  popperStyles.top = '';
  popperStyles.left = '';
  popperStyles[transformProp] = '';

  var boundaries = getBoundaries(data.instance.popper, data.instance.reference, options.padding, boundariesElement, data.positionFixed);

  // NOTE: DOM access here
  // restores the original style properties after the offsets have been computed
  popperStyles.top = top;
  popperStyles.left = left;
  popperStyles[transformProp] = transform;

  options.boundaries = boundaries;

  var order = options.priority;
  var popper = data.offsets.popper;

  var check = {
    primary: function primary(placement) {
      var value = popper[placement];
      if (popper[placement] < boundaries[placement] && !options.escapeWithReference) {
        value = Math.max(popper[placement], boundaries[placement]);
      }
      return defineProperty({}, placement, value);
    },
    secondary: function secondary(placement) {
      var mainSide = placement === 'right' ? 'left' : 'top';
      var value = popper[mainSide];
      if (popper[placement] > boundaries[placement] && !options.escapeWithReference) {
        value = Math.min(popper[mainSide], boundaries[placement] - (placement === 'right' ? popper.width : popper.height));
      }
      return defineProperty({}, mainSide, value);
    }
  };

  order.forEach(function (placement) {
    var side = ['left', 'top'].indexOf(placement) !== -1 ? 'primary' : 'secondary';
    popper = _extends({}, popper, check[side](placement));
  });

  data.offsets.popper = popper;

  return data;
}

/**
 * @function
 * @memberof Modifiers
 * @argument {Object} data - The data object generated by `update` method
 * @argument {Object} options - Modifiers configuration and options
 * @returns {Object} The data object, properly modified
 */
function shift(data) {
  var placement = data.placement;
  var basePlacement = placement.split('-')[0];
  var shiftvariation = placement.split('-')[1];

  // if shift shiftvariation is specified, run the modifier
  if (shiftvariation) {
    var _data$offsets = data.offsets,
        reference = _data$offsets.reference,
        popper = _data$offsets.popper;

    var isVertical = ['bottom', 'top'].indexOf(basePlacement) !== -1;
    var side = isVertical ? 'left' : 'top';
    var measurement = isVertical ? 'width' : 'height';

    var shiftOffsets = {
      start: defineProperty({}, side, reference[side]),
      end: defineProperty({}, side, reference[side] + reference[measurement] - popper[measurement])
    };

    data.offsets.popper = _extends({}, popper, shiftOffsets[shiftvariation]);
  }

  return data;
}

/**
 * @function
 * @memberof Modifiers
 * @argument {Object} data - The data object generated by update method
 * @argument {Object} options - Modifiers configuration and options
 * @returns {Object} The data object, properly modified
 */
function hide(data) {
  if (!isModifierRequired(data.instance.modifiers, 'hide', 'preventOverflow')) {
    return data;
  }

  var refRect = data.offsets.reference;
  var bound = find(data.instance.modifiers, function (modifier) {
    return modifier.name === 'preventOverflow';
  }).boundaries;

  if (refRect.bottom < bound.top || refRect.left > bound.right || refRect.top > bound.bottom || refRect.right < bound.left) {
    // Avoid unnecessary DOM access if visibility hasn't changed
    if (data.hide === true) {
      return data;
    }

    data.hide = true;
    data.attributes['x-out-of-boundaries'] = '';
  } else {
    // Avoid unnecessary DOM access if visibility hasn't changed
    if (data.hide === false) {
      return data;
    }

    data.hide = false;
    data.attributes['x-out-of-boundaries'] = false;
  }

  return data;
}

/**
 * @function
 * @memberof Modifiers
 * @argument {Object} data - The data object generated by `update` method
 * @argument {Object} options - Modifiers configuration and options
 * @returns {Object} The data object, properly modified
 */
function inner(data) {
  var placement = data.placement;
  var basePlacement = placement.split('-')[0];
  var _data$offsets = data.offsets,
      popper = _data$offsets.popper,
      reference = _data$offsets.reference;

  var isHoriz = ['left', 'right'].indexOf(basePlacement) !== -1;

  var subtractLength = ['top', 'left'].indexOf(basePlacement) === -1;

  popper[isHoriz ? 'left' : 'top'] = reference[basePlacement] - (subtractLength ? popper[isHoriz ? 'width' : 'height'] : 0);

  data.placement = getOppositePlacement(placement);
  data.offsets.popper = getClientRect(popper);

  return data;
}

/**
 * Modifier function, each modifier can have a function of this type assigned
 * to its `fn` property.<br />
 * These functions will be called on each update, this means that you must
 * make sure they are performant enough to avoid performance bottlenecks.
 *
 * @function ModifierFn
 * @argument {dataObject} data - The data object generated by `update` method
 * @argument {Object} options - Modifiers configuration and options
 * @returns {dataObject} The data object, properly modified
 */

/**
 * Modifiers are plugins used to alter the behavior of your poppers.<br />
 * Popper.js uses a set of 9 modifiers to provide all the basic functionalities
 * needed by the library.
 *
 * Usually you don't want to override the `order`, `fn` and `onLoad` props.
 * All the other properties are configurations that could be tweaked.
 * @namespace modifiers
 */
var modifiers = {
  /**
   * Modifier used to shift the popper on the start or end of its reference
   * element.<br />
   * It will read the variation of the `placement` property.<br />
   * It can be one either `-end` or `-start`.
   * @memberof modifiers
   * @inner
   */
  shift: {
    /** @prop {number} order=100 - Index used to define the order of execution */
    order: 100,
    /** @prop {Boolean} enabled=true - Whether the modifier is enabled or not */
    enabled: true,
    /** @prop {ModifierFn} */
    fn: shift
  },

  /**
   * The `offset` modifier can shift your popper on both its axis.
   *
   * It accepts the following units:
   * - `px` or unit-less, interpreted as pixels
   * - `%` or `%r`, percentage relative to the length of the reference element
   * - `%p`, percentage relative to the length of the popper element
   * - `vw`, CSS viewport width unit
   * - `vh`, CSS viewport height unit
   *
   * For length is intended the main axis relative to the placement of the popper.<br />
   * This means that if the placement is `top` or `bottom`, the length will be the
   * `width`. In case of `left` or `right`, it will be the `height`.
   *
   * You can provide a single value (as `Number` or `String`), or a pair of values
   * as `String` divided by a comma or one (or more) white spaces.<br />
   * The latter is a deprecated method because it leads to confusion and will be
   * removed in v2.<br />
   * Additionally, it accepts additions and subtractions between different units.
   * Note that multiplications and divisions aren't supported.
   *
   * Valid examples are:
   * ```
   * 10
   * '10%'
   * '10, 10'
   * '10%, 10'
   * '10 + 10%'
   * '10 - 5vh + 3%'
   * '-10px + 5vh, 5px - 6%'
   * ```
   * > **NB**: If you desire to apply offsets to your poppers in a way that may make them overlap
   * > with their reference element, unfortunately, you will have to disable the `flip` modifier.
   * > You can read more on this at this [issue](https://github.com/FezVrasta/popper.js/issues/373).
   *
   * @memberof modifiers
   * @inner
   */
  offset: {
    /** @prop {number} order=200 - Index used to define the order of execution */
    order: 200,
    /** @prop {Boolean} enabled=true - Whether the modifier is enabled or not */
    enabled: true,
    /** @prop {ModifierFn} */
    fn: offset,
    /** @prop {Number|String} offset=0
     * The offset value as described in the modifier description
     */
    offset: 0
  },

  /**
   * Modifier used to prevent the popper from being positioned outside the boundary.
   *
   * A scenario exists where the reference itself is not within the boundaries.<br />
   * We can say it has "escaped the boundaries" — or just "escaped".<br />
   * In this case we need to decide whether the popper should either:
   *
   * - detach from the reference and remain "trapped" in the boundaries, or
   * - if it should ignore the boundary and "escape with its reference"
   *
   * When `escapeWithReference` is set to`true` and reference is completely
   * outside its boundaries, the popper will overflow (or completely leave)
   * the boundaries in order to remain attached to the edge of the reference.
   *
   * @memberof modifiers
   * @inner
   */
  preventOverflow: {
    /** @prop {number} order=300 - Index used to define the order of execution */
    order: 300,
    /** @prop {Boolean} enabled=true - Whether the modifier is enabled or not */
    enabled: true,
    /** @prop {ModifierFn} */
    fn: preventOverflow,
    /**
     * @prop {Array} [priority=['left','right','top','bottom']]
     * Popper will try to prevent overflow following these priorities by default,
     * then, it could overflow on the left and on top of the `boundariesElement`
     */
    priority: ['left', 'right', 'top', 'bottom'],
    /**
     * @prop {number} padding=5
     * Amount of pixel used to define a minimum distance between the boundaries
     * and the popper. This makes sure the popper always has a little padding
     * between the edges of its container
     */
    padding: 5,
    /**
     * @prop {String|HTMLElement} boundariesElement='scrollParent'
     * Boundaries used by the modifier. Can be `scrollParent`, `window`,
     * `viewport` or any DOM element.
     */
    boundariesElement: 'scrollParent'
  },

  /**
   * Modifier used to make sure the reference and its popper stay near each other
   * without leaving any gap between the two. Especially useful when the arrow is
   * enabled and you want to ensure that it points to its reference element.
   * It cares only about the first axis. You can still have poppers with margin
   * between the popper and its reference element.
   * @memberof modifiers
   * @inner
   */
  keepTogether: {
    /** @prop {number} order=400 - Index used to define the order of execution */
    order: 400,
    /** @prop {Boolean} enabled=true - Whether the modifier is enabled or not */
    enabled: true,
    /** @prop {ModifierFn} */
    fn: keepTogether
  },

  /**
   * This modifier is used to move the `arrowElement` of the popper to make
   * sure it is positioned between the reference element and its popper element.
   * It will read the outer size of the `arrowElement` node to detect how many
   * pixels of conjunction are needed.
   *
   * It has no effect if no `arrowElement` is provided.
   * @memberof modifiers
   * @inner
   */
  arrow: {
    /** @prop {number} order=500 - Index used to define the order of execution */
    order: 500,
    /** @prop {Boolean} enabled=true - Whether the modifier is enabled or not */
    enabled: true,
    /** @prop {ModifierFn} */
    fn: arrow,
    /** @prop {String|HTMLElement} element='[x-arrow]' - Selector or node used as arrow */
    element: '[x-arrow]'
  },

  /**
   * Modifier used to flip the popper's placement when it starts to overlap its
   * reference element.
   *
   * Requires the `preventOverflow` modifier before it in order to work.
   *
   * **NOTE:** this modifier will interrupt the current update cycle and will
   * restart it if it detects the need to flip the placement.
   * @memberof modifiers
   * @inner
   */
  flip: {
    /** @prop {number} order=600 - Index used to define the order of execution */
    order: 600,
    /** @prop {Boolean} enabled=true - Whether the modifier is enabled or not */
    enabled: true,
    /** @prop {ModifierFn} */
    fn: flip,
    /**
     * @prop {String|Array} behavior='flip'
     * The behavior used to change the popper's placement. It can be one of
     * `flip`, `clockwise`, `counterclockwise` or an array with a list of valid
     * placements (with optional variations)
     */
    behavior: 'flip',
    /**
     * @prop {number} padding=5
     * The popper will flip if it hits the edges of the `boundariesElement`
     */
    padding: 5,
    /**
     * @prop {String|HTMLElement} boundariesElement='viewport'
     * The element which will define the boundaries of the popper position.
     * The popper will never be placed outside of the defined boundaries
     * (except if `keepTogether` is enabled)
     */
    boundariesElement: 'viewport',
    /**
     * @prop {Boolean} flipVariations=false
     * The popper will switch placement variation between `-start` and `-end` when
     * the reference element overlaps its boundaries.
     *
     * The original placement should have a set variation.
     */
    flipVariations: false,
    /**
     * @prop {Boolean} flipVariationsByContent=false
     * The popper will switch placement variation between `-start` and `-end` when
     * the popper element overlaps its reference boundaries.
     *
     * The original placement should have a set variation.
     */
    flipVariationsByContent: false
  },

  /**
   * Modifier used to make the popper flow toward the inner of the reference element.
   * By default, when this modifier is disabled, the popper will be placed outside
   * the reference element.
   * @memberof modifiers
   * @inner
   */
  inner: {
    /** @prop {number} order=700 - Index used to define the order of execution */
    order: 700,
    /** @prop {Boolean} enabled=false - Whether the modifier is enabled or not */
    enabled: false,
    /** @prop {ModifierFn} */
    fn: inner
  },

  /**
   * Modifier used to hide the popper when its reference element is outside of the
   * popper boundaries. It will set a `x-out-of-boundaries` attribute which can
   * be used to hide with a CSS selector the popper when its reference is
   * out of boundaries.
   *
   * Requires the `preventOverflow` modifier before it in order to work.
   * @memberof modifiers
   * @inner
   */
  hide: {
    /** @prop {number} order=800 - Index used to define the order of execution */
    order: 800,
    /** @prop {Boolean} enabled=true - Whether the modifier is enabled or not */
    enabled: true,
    /** @prop {ModifierFn} */
    fn: hide
  },

  /**
   * Computes the style that will be applied to the popper element to gets
   * properly positioned.
   *
   * Note that this modifier will not touch the DOM, it just prepares the styles
   * so that `applyStyle` modifier can apply it. This separation is useful
   * in case you need to replace `applyStyle` with a custom implementation.
   *
   * This modifier has `850` as `order` value to maintain backward compatibility
   * with previous versions of Popper.js. Expect the modifiers ordering method
   * to change in future major versions of the library.
   *
   * @memberof modifiers
   * @inner
   */
  computeStyle: {
    /** @prop {number} order=850 - Index used to define the order of execution */
    order: 850,
    /** @prop {Boolean} enabled=true - Whether the modifier is enabled or not */
    enabled: true,
    /** @prop {ModifierFn} */
    fn: computeStyle,
    /**
     * @prop {Boolean} gpuAcceleration=true
     * If true, it uses the CSS 3D transformation to position the popper.
     * Otherwise, it will use the `top` and `left` properties
     */
    gpuAcceleration: true,
    /**
     * @prop {string} [x='bottom']
     * Where to anchor the X axis (`bottom` or `top`). AKA X offset origin.
     * Change this if your popper should grow in a direction different from `bottom`
     */
    x: 'bottom',
    /**
     * @prop {string} [x='left']
     * Where to anchor the Y axis (`left` or `right`). AKA Y offset origin.
     * Change this if your popper should grow in a direction different from `right`
     */
    y: 'right'
  },

  /**
   * Applies the computed styles to the popper element.
   *
   * All the DOM manipulations are limited to this modifier. This is useful in case
   * you want to integrate Popper.js inside a framework or view library and you
   * want to delegate all the DOM manipulations to it.
   *
   * Note that if you disable this modifier, you must make sure the popper element
   * has its position set to `absolute` before Popper.js can do its work!
   *
   * Just disable this modifier and define your own to achieve the desired effect.
   *
   * @memberof modifiers
   * @inner
   */
  applyStyle: {
    /** @prop {number} order=900 - Index used to define the order of execution */
    order: 900,
    /** @prop {Boolean} enabled=true - Whether the modifier is enabled or not */
    enabled: true,
    /** @prop {ModifierFn} */
    fn: applyStyle,
    /** @prop {Function} */
    onLoad: applyStyleOnLoad,
    /**
     * @deprecated since version 1.10.0, the property moved to `computeStyle` modifier
     * @prop {Boolean} gpuAcceleration=true
     * If true, it uses the CSS 3D transformation to position the popper.
     * Otherwise, it will use the `top` and `left` properties
     */
    gpuAcceleration: undefined
  }
};

/**
 * The `dataObject` is an object containing all the information used by Popper.js.
 * This object is passed to modifiers and to the `onCreate` and `onUpdate` callbacks.
 * @name dataObject
 * @property {Object} data.instance The Popper.js instance
 * @property {String} data.placement Placement applied to popper
 * @property {String} data.originalPlacement Placement originally defined on init
 * @property {Boolean} data.flipped True if popper has been flipped by flip modifier
 * @property {Boolean} data.hide True if the reference element is out of boundaries, useful to know when to hide the popper
 * @property {HTMLElement} data.arrowElement Node used as arrow by arrow modifier
 * @property {Object} data.styles Any CSS property defined here will be applied to the popper. It expects the JavaScript nomenclature (eg. `marginBottom`)
 * @property {Object} data.arrowStyles Any CSS property defined here will be applied to the popper arrow. It expects the JavaScript nomenclature (eg. `marginBottom`)
 * @property {Object} data.boundaries Offsets of the popper boundaries
 * @property {Object} data.offsets The measurements of popper, reference and arrow elements
 * @property {Object} data.offsets.popper `top`, `left`, `width`, `height` values
 * @property {Object} data.offsets.reference `top`, `left`, `width`, `height` values
 * @property {Object} data.offsets.arrow] `top` and `left` offsets, only one of them will be different from 0
 */

/**
 * Default options provided to Popper.js constructor.<br />
 * These can be overridden using the `options` argument of Popper.js.<br />
 * To override an option, simply pass an object with the same
 * structure of the `options` object, as the 3rd argument. For example:
 * ```
 * new Popper(ref, pop, {
 *   modifiers: {
 *     preventOverflow: { enabled: false }
 *   }
 * })
 * ```
 * @type {Object}
 * @static
 * @memberof Popper
 */
var Defaults = {
  /**
   * Popper's placement.
   * @prop {Popper.placements} placement='bottom'
   */
  placement: 'bottom',

  /**
   * Set this to true if you want popper to position it self in 'fixed' mode
   * @prop {Boolean} positionFixed=false
   */
  positionFixed: false,

  /**
   * Whether events (resize, scroll) are initially enabled.
   * @prop {Boolean} eventsEnabled=true
   */
  eventsEnabled: true,

  /**
   * Set to true if you want to automatically remove the popper when
   * you call the `destroy` method.
   * @prop {Boolean} removeOnDestroy=false
   */
  removeOnDestroy: false,

  /**
   * Callback called when the popper is created.<br />
   * By default, it is set to no-op.<br />
   * Access Popper.js instance with `data.instance`.
   * @prop {onCreate}
   */
  onCreate: function onCreate() {},

  /**
   * Callback called when the popper is updated. This callback is not called
   * on the initialization/creation of the popper, but only on subsequent
   * updates.<br />
   * By default, it is set to no-op.<br />
   * Access Popper.js instance with `data.instance`.
   * @prop {onUpdate}
   */
  onUpdate: function onUpdate() {},

  /**
   * List of modifiers used to modify the offsets before they are applied to the popper.
   * They provide most of the functionalities of Popper.js.
   * @prop {modifiers}
   */
  modifiers: modifiers
};

/**
 * @callback onCreate
 * @param {dataObject} data
 */

/**
 * @callback onUpdate
 * @param {dataObject} data
 */

// Utils
// Methods
var Popper = function () {
  /**
   * Creates a new Popper.js instance.
   * @class Popper
   * @param {Element|referenceObject} reference - The reference element used to position the popper
   * @param {Element} popper - The HTML / XML element used as the popper
   * @param {Object} options - Your custom options to override the ones defined in [Defaults](#defaults)
   * @return {Object} instance - The generated Popper.js instance
   */
  function Popper(reference, popper) {
    var _this = this;

    var options = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {};
    classCallCheck(this, Popper);

    this.scheduleUpdate = function () {
      return requestAnimationFrame(_this.update);
    };

    // make update() debounced, so that it only runs at most once-per-tick
    this.update = debounce(this.update.bind(this));

    // with {} we create a new object with the options inside it
    this.options = _extends({}, Popper.Defaults, options);

    // init state
    this.state = {
      isDestroyed: false,
      isCreated: false,
      scrollParents: []
    };

    // get reference and popper elements (allow jQuery wrappers)
    this.reference = reference && reference.jquery ? reference[0] : reference;
    this.popper = popper && popper.jquery ? popper[0] : popper;

    // Deep merge modifiers options
    this.options.modifiers = {};
    Object.keys(_extends({}, Popper.Defaults.modifiers, options.modifiers)).forEach(function (name) {
      _this.options.modifiers[name] = _extends({}, Popper.Defaults.modifiers[name] || {}, options.modifiers ? options.modifiers[name] : {});
    });

    // Refactoring modifiers' list (Object => Array)
    this.modifiers = Object.keys(this.options.modifiers).map(function (name) {
      return _extends({
        name: name
      }, _this.options.modifiers[name]);
    })
    // sort the modifiers by order
    .sort(function (a, b) {
      return a.order - b.order;
    });

    // modifiers have the ability to execute arbitrary code when Popper.js get inited
    // such code is executed in the same order of its modifier
    // they could add new properties to their options configuration
    // BE AWARE: don't add options to `options.modifiers.name` but to `modifierOptions`!
    this.modifiers.forEach(function (modifierOptions) {
      if (modifierOptions.enabled && isFunction(modifierOptions.onLoad)) {
        modifierOptions.onLoad(_this.reference, _this.popper, _this.options, modifierOptions, _this.state);
      }
    });

    // fire the first update to position the popper in the right place
    this.update();

    var eventsEnabled = this.options.eventsEnabled;
    if (eventsEnabled) {
      // setup event listeners, they will take care of update the position in specific situations
      this.enableEventListeners();
    }

    this.state.eventsEnabled = eventsEnabled;
  }

  // We can't use class properties because they don't get listed in the
  // class prototype and break stuff like Sinon stubs


  createClass(Popper, [{
    key: 'update',
    value: function update$$1() {
      return update.call(this);
    }
  }, {
    key: 'destroy',
    value: function destroy$$1() {
      return destroy.call(this);
    }
  }, {
    key: 'enableEventListeners',
    value: function enableEventListeners$$1() {
      return enableEventListeners.call(this);
    }
  }, {
    key: 'disableEventListeners',
    value: function disableEventListeners$$1() {
      return disableEventListeners.call(this);
    }

    /**
     * Schedules an update. It will run on the next UI update available.
     * @method scheduleUpdate
     * @memberof Popper
     */


    /**
     * Collection of utilities useful when writing custom modifiers.
     * Starting from version 1.7, this method is available only if you
     * include `popper-utils.js` before `popper.js`.
     *
     * **DEPRECATION**: This way to access PopperUtils is deprecated
     * and will be removed in v2! Use the PopperUtils module directly instead.
     * Due to the high instability of the methods contained in Utils, we can't
     * guarantee them to follow semver. Use them at your own risk!
     * @static
     * @private
     * @type {Object}
     * @deprecated since version 1.8
     * @member Utils
     * @memberof Popper
     */

  }]);
  return Popper;
}();

/**
 * The `referenceObject` is an object that provides an interface compatible with Popper.js
 * and lets you use it as replacement of a real DOM node.<br />
 * You can use this method to position a popper relatively to a set of coordinates
 * in case you don't have a DOM node to use as reference.
 *
 * ```
 * new Popper(referenceObject, popperNode);
 * ```
 *
 * NB: This feature isn't supported in Internet Explorer 10.
 * @name referenceObject
 * @property {Function} data.getBoundingClientRect
 * A function that returns a set of coordinates compatible with the native `getBoundingClientRect` method.
 * @property {number} data.clientWidth
 * An ES6 getter that will return the width of the virtual reference element.
 * @property {number} data.clientHeight
 * An ES6 getter that will return the height of the virtual reference element.
 */


Popper.Utils = (typeof window !== 'undefined' ? window : global).PopperUtils;
Popper.placements = placements;
Popper.Defaults = Defaults;

/* harmony default export */ __webpack_exports__["a"] = (Popper);
//# sourceMappingURL=popper.js.map

/* WEBPACK VAR INJECTION */}.call(this, __webpack_require__(36)))

/***/ }),
/* 21 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
function diff(x̅, y̅) {
    return filter(x̅, x => !(y̅.has(x)));
}
exports.diff = diff;
function every(x̅, pred) {
    return Array.from(x̅).every(pred);
}
exports.every = every;
function filter(x̅, pred) {
    return new Set([...x̅].filter(pred));
}
exports.filter = filter;
function map(x̅, f) {
    return new Set([...x̅].map(f));
}
exports.map = map;
function some(x̅, pred) {
    return Array.from(x̅).some(pred);
}
exports.some = some;
function union(...x̅̅) {
    const y̅ = new Set();
    x̅̅.forEach(x̅ => {
        x̅.forEach(x => {
            y̅.add(x);
        });
    });
    return y̅;
}
exports.union = union;
function intersection(x̅̅, y̅) {
    const zs = new Set();
    x̅̅.forEach(x => {
        if (y̅.has(x)) {
            zs.add(x);
        }
    });
    return zs;
}
exports.intersection = intersection;


/***/ }),
/* 22 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const nearley_1 = __webpack_require__(32);
const Core_1 = __webpack_require__(0);
const BaseTypes_1 = __webpack_require__(4);
const DataType_1 = __webpack_require__(7);
const Env_1 = __webpack_require__(14);
const Eval_1 = __webpack_require__(16);
const Expr_1 = __webpack_require__(6);
__webpack_require__(8); // for datatypes
const Parse_1 = __webpack_require__(33);
const Versioned_1 = __webpack_require__(2);
// Kindergarten modules.
var Module;
(function (Module) {
    function initialise(resourceServerUrl) {
        Core_1.assert(Module.resourceServerUrl === undefined);
        Module.resourceServerUrl = Core_1.__nonNull(resourceServerUrl, `resourceServerUrl is ${resourceServerUrl}`);
        BaseTypes_1.BaseTypes.initialise();
        console.log("Initialising base types");
        Module.prelude = loadModule(Env_1.emptyEnv(), "prelude");
        Module.graphics = loadModule(Module.prelude, "graphics");
    }
    Module.initialise = initialise;
})(Module = exports.Module || (exports.Module = {}));
function import_(...modules) {
    if (modules.length === 0) {
        return Env_1.emptyEnv();
    }
    else {
        const [m, ...ms] = modules;
        return m.concat(import_(...ms));
    }
}
function loadFile(folder, file) {
    Core_1.assert(Module.resourceServerUrl !== undefined, "Module system not initialised.");
    let text;
    const xmlhttp = new XMLHttpRequest;
    const url = Module.resourceServerUrl + "/" + folder + "/" + file + ".fld";
    console.log(`Opening ${url}`);
    xmlhttp.open("GET", url, false);
    xmlhttp.send();
    if (xmlhttp.status === 200) {
        text = xmlhttp.responseText;
    }
    if (text === undefined) {
        return Core_1.assert(false, `${url} could not be loaded.`);
    }
    else {
        return text;
    }
}
exports.loadFile = loadFile;
// Not sure if Nearley can parse arbitrary non-terminal, as opposed to root.
function loadModule(ρ, file) {
    const src = loadFile("fluid/lib", file);
    const srcʹ = src + " in 0";
    const e = Core_1.as(successfulParse(srcʹ), Expr_1.Expr.Defs);
    return Eval_1.Eval.defs(ρ, e.def̅, Env_1.emptyEnv())[1];
}
exports.loadModule = loadModule;
function openWithImports(file, ...modules) {
    return parseWithImports(loadFile("fluid/example", file), ...modules);
}
exports.openWithImports = openWithImports;
function openDatasetAs(file, x) {
    const [ρ, e] = parseWithImports(loadFile("fluid/dataset", file));
    return Env_1.Env.singleton(Versioned_1.str(x)(Versioned_1.ν()), Eval_1.Eval.eval_(ρ, e));
}
exports.openDatasetAs = openDatasetAs;
function parseWithImports(src, ...modules) {
    Core_1.assert(Module.resourceServerUrl !== undefined, "Module system not initialised.");
    return [import_(Core_1.__nonNull(Module.prelude), Core_1.__nonNull(Module.graphics), ...modules), successfulParse(src)];
}
exports.parseWithImports = parseWithImports;
// https://github.com/kach/nearley/issues/276#issuecomment-324162234
function successfulParse(str) {
    const { results } = new nearley_1.Parser(nearley_1.Grammar.fromCompiled(Parse_1.default)).feed(str);
    if (results.length > 1) {
        Core_1.userError("Ambiguous parse.");
    }
    else if (results.length === 0) {
        Core_1.userError("Unsuccessful parse.");
    }
    return results[0];
}
exports.successfulParse = successfulParse;
// create an expression and evaluate it, so we have an explained value
function bindDataset(ρ, vs, x) {
    return Env_1.extendEnv(ρ, Versioned_1.str(x)(Versioned_1.ν()), Eval_1.Eval.eval_(ρ, asList(vs.map(asRecord))));
}
exports.bindDataset = bindDataset;
function asRecord(v) {
    return asList(Object.getOwnPropertyNames(v).map(k => asPair(k, v[k])));
}
function asPair(k, v) {
    return Versioned_1.at(DataType_1.exprClass(BaseTypes_1.Pair), asPrimValue(k), asPrimValue(v))(Versioned_1.ν());
}
function asList(e̅) {
    let e̅ʹ = Versioned_1.at(DataType_1.exprClass(BaseTypes_1.Nil))(Versioned_1.ν());
    for (let e of [...e̅].reverse()) {
        e̅ʹ = Versioned_1.at(DataType_1.exprClass(BaseTypes_1.Cons), e, e̅ʹ)(Versioned_1.ν());
    }
    return e̅ʹ;
}
function asPrimValue(v) {
    if (typeof v === "number") {
        return Expr_1.Expr.constNum(Versioned_1.num(v)(Versioned_1.ν()))(Versioned_1.ν());
    }
    else if (typeof v === "string") {
        return Expr_1.Expr.constStr(Versioned_1.str(v)(Versioned_1.ν()))(Versioned_1.ν());
    }
    else {
        return Core_1.userError(`Ill-formed data: expected string or number, found ${typeof v}.`);
    }
}


/***/ }),
/* 23 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const BaseTypes_1 = __webpack_require__(4);
function get(m, k) {
    if (BaseTypes_1.NonEmpty.is(m)) {
        if (k.leq(m.t.fst)) {
            if (m.t.fst.leq(k)) {
                return m.t.snd;
            }
            else {
                return get(m.left, k);
            }
        }
        else {
            return get(m.right, k);
        }
    }
    else if (BaseTypes_1.Empty.is(m)) {
        return undefined;
    }
    else {
        return Core_1.absurd();
    }
}
exports.get = get;
function insert(m, k, v) {
    if (BaseTypes_1.NonEmpty.is(m)) {
        if (k.leq(m.t.fst)) {
            if (m.t.fst.leq(k)) {
                return BaseTypes_1.nonEmpty(m.left, BaseTypes_1.pair(k, v), m.right);
            }
            else {
                return BaseTypes_1.nonEmpty(insert(m.left, k, v), m.t, m.right);
            }
        }
        else {
            return BaseTypes_1.nonEmpty(m.left, m.t, insert(m.right, k, v));
        }
    }
    else if (BaseTypes_1.Empty.is(m)) {
        return BaseTypes_1.nonEmpty(m, BaseTypes_1.pair(k, v), m);
    }
    else {
        return Core_1.absurd();
    }
}
exports.insert = insert;
function singleton(k, v) {
    return insert(BaseTypes_1.empty(), k, v);
}
exports.singleton = singleton;
// Union with a combining function. If keys are equal, right-hand key will be used in the output.
// Avoid primes in signature; seems to be incompatible with version of ts-loader used by Wrattler.
function unionWith(m1, m2, f) {
    if (BaseTypes_1.NonEmpty.is(m2)) {
        const k = m2.t.fst, v = m2.t.snd, vʹ = get(m1, k), u = vʹ === undefined ? v : f(v, vʹ);
        return unionWith(insert(unionWith(m1, m2.left, f), k, u), m2.right, f);
    }
    else if (BaseTypes_1.Empty.is(m2)) {
        return m1;
    }
    else {
        return Core_1.absurd();
    }
}
exports.unionWith = unionWith;


/***/ }),
/* 24 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const BaseTypes_1 = __webpack_require__(4);
const DataValue_1 = __webpack_require__(3);
const Expl_1 = __webpack_require__(5);
const Value_1 = __webpack_require__(1);
const Versioned_1 = __webpack_require__(2);
// In the following two classes, we store the operation without generic type parameters, as fields can't
// have polymorphic type. Then access the operation via a method and reinstate the polymorphism via a cast.
class PrimOp extends Value_1.Value {
    constructor() {
        super(...arguments);
        this.name = Value_1._;
    }
}
exports.PrimOp = PrimOp;
class UnaryOp extends PrimOp {
    constructor() {
        super(...arguments);
        this.op = Value_1._;
    }
}
exports.UnaryOp = UnaryOp;
class BinaryOp extends PrimOp {
    constructor() {
        super(...arguments);
        this.op = Value_1._;
    }
}
exports.BinaryOp = BinaryOp;
const ceiling = x => Versioned_1.num(Math.ceil(x.val));
// Used to take arbitrary value as additional argument, but now primitives have primitive arguments.
const debugLog = v => (k) => {
    console.log(v);
    return v;
};
const floor = x => Versioned_1.num(Math.floor(x.val));
const log = x => Versioned_1.num(Math.log(Core_1.as(x, Value_1.Num).val));
const numToStr = x => Versioned_1.str(x.val.toString());
// No longer support overloaded functions, since the pattern-matching semantics is non-trivial; might require typecase.
// If we want integer division, apparently ~~(x / y) will round in the right direction.
const div = (x, y) => (k) => {
    const n = Versioned_1.num(Core_1.as(x, Value_1.Num).val / Core_1.as(y, Value_1.Num).val)(k);
    if (!isFinite(n.val)) {
        Core_1.userError("Division by zero", x, y);
    }
    return n;
};
const concat = (x, y) => Versioned_1.str(Core_1.as(x, Value_1.Str).val + Core_1.as(y, Value_1.Str).val);
const equal = (x, y) => {
    if (x instanceof Value_1.Num && y instanceof Value_1.Num) {
        return x.val === y.val ? BaseTypes_1.true_() : BaseTypes_1.false_();
    }
    else if (x instanceof Value_1.Str && y instanceof Value_1.Str) {
        return x.val === y.val ? BaseTypes_1.true_() : BaseTypes_1.false_();
    }
    else {
        return Core_1.userError(`Expected ${Value_1.Num.name} or ${Value_1.Str.name}.`);
    }
};
const error = message => Core_1.assert(false, "Fluid error:\n" + message.val);
const greaterEq = (x, y) => {
    if (x instanceof Value_1.Num && y instanceof Value_1.Num) {
        return x.val >= y.val ? BaseTypes_1.true_() : BaseTypes_1.false_();
    }
    else if (x instanceof Value_1.Str && y instanceof Value_1.Str) {
        // string comparison delegates to central implementation for consistency
        return x.geq(y) ? BaseTypes_1.true_() : BaseTypes_1.false_();
    }
    else {
        return Core_1.userError(`Expected ${Value_1.Num.name} or ${Value_1.Str.name}.`);
    }
};
const greater = (x, y) => Core_1.as(x, Value_1.Num).val > Core_1.as(y, Value_1.Num).val ? BaseTypes_1.true_() : BaseTypes_1.false_();
const lessEq = (x, y) => {
    if (x instanceof Value_1.Num && y instanceof Value_1.Num) {
        return Core_1.as(x, Value_1.Num).val <= Core_1.as(y, Value_1.Num).val ? BaseTypes_1.true_() : BaseTypes_1.false_();
    }
    else if (x instanceof Value_1.Str && y instanceof Value_1.Str) {
        return x.leq(y) ? BaseTypes_1.true_() : BaseTypes_1.false_();
    }
    else {
        return Core_1.userError(`Expected ${Value_1.Num.name} or ${Value_1.Str.name}.`);
    }
};
const less = (x, y) => Core_1.as(x, Value_1.Num).val < Core_1.as(y, Value_1.Num).val ? BaseTypes_1.true_() : BaseTypes_1.false_();
const minus = (x, y) => Versioned_1.num(Core_1.as(x, Value_1.Num).val - Core_1.as(y, Value_1.Num).val);
const plus = (x, y) => Versioned_1.num(Core_1.as(x, Value_1.Num).val + Core_1.as(y, Value_1.Num).val);
const pow = (x, y) => Versioned_1.num(Math.pow(Core_1.as(x, Value_1.Num).val, Core_1.as(y, Value_1.Num).val));
const times = (x, y) => Versioned_1.num(Core_1.as(x, Value_1.Num).val * Core_1.as(y, Value_1.Num).val);
// Convenience methods for building the maps. Export to allow other modules to provide operations.
function unary_(op) {
    return DataValue_1.explValue(Expl_1.Expl.const_()(Versioned_1.ν()), Versioned_1.at(UnaryOp, op.name, op)(Versioned_1.ν()));
}
exports.unary_ = unary_;
function binary_(op) {
    return DataValue_1.explValue(Expl_1.Expl.const_()(Versioned_1.ν()), Versioned_1.at(BinaryOp, op.name, op)(Versioned_1.ν()));
}
exports.binary_ = binary_;
// Primitives with identifiers as names are unary and first-class.
exports.unaryOps = new Map([
    [ceiling.name, unary_(ceiling)],
    [debugLog.name, unary_(debugLog)],
    [error.name, unary_(error)],
    [floor.name, unary_(floor)],
    [log.name, unary_(log)],
    [numToStr.name, unary_(numToStr)]
]);
exports.binaryOps = new Map([
    ["-", binary_(minus)],
    ["+", binary_(plus)],
    ["*", binary_(times)],
    ["**", binary_(pow)],
    ["/", binary_(div)],
    ["==", binary_(equal)],
    [">", binary_(greater)],
    [">=", binary_(greaterEq)],
    ["<", binary_(less)],
    ["<=", binary_(lessEq)],
    ["++", binary_(concat)]
]);


/***/ }),
/* 25 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Array_1 = __webpack_require__(11);
const Core_1 = __webpack_require__(0);
const BaseTypes_1 = __webpack_require__(4);
const Graphics_1 = __webpack_require__(8);
const Primitive_1 = __webpack_require__(24);
const Versioned_1 = __webpack_require__(2);
const Core_2 = __webpack_require__(26);
const Cursor_1 = __webpack_require__(17);
const Interactor_1 = __webpack_require__(35);
const Renderer_1 = __webpack_require__(15);
const fontSize = 12;
exports.svg = new Core_2.SVG();
function scale(x_scale, y_scale) {
    return ([x, y]) => {
        return [x * x_scale, y * y_scale];
    };
}
function translate(x_inc, y_inc) {
    return ([x, y]) => {
        return [x + x_inc, y + y_inc];
    };
}
function invertScale(scale) {
    return ([x, y]) => {
        const [x_scale, y_scale] = scale([1, 1]);
        return [x / x_scale, y / y_scale];
    };
}
function transformFun(t) {
    if (t instanceof Graphics_1.Scale) {
        Core_1.assert(t.x.val >= 0 && t.y.val >= 0);
        return scale(t.x.val, t.y.val);
    }
    else if (t instanceof Graphics_1.Translate) {
        Core_1.assert(isFinite(t.x.val) && isFinite(t.y.val));
        return translate(t.x.val, t.y.val);
    }
    else {
        return Core_1.absurd();
    }
}
function postcompose(f1, f2) {
    return ([x, y]) => {
        return f1(f2([x, y]));
    };
}
class GraphicsRenderer {
    // transform attribute isn't supported on SVGElement, so it contains a group element with the inversion transform.
    constructor(editor, root, initialAncestor) {
        this.showInvisible = false;
        this.editor = editor;
        this.root = root;
        this.ancestors = [initialAncestor];
        this.translations = [Core_1.id];
        this.scalings = [Core_1.id];
    }
    get current() {
        return this.ancestors[this.ancestors.length - 1];
    }
    // scaling applies to translated coordinates
    get transform() {
        return postcompose(this.scale, Array_1.last(this.translations));
    }
    get scale() {
        return Array_1.last(this.scalings);
    }
    render(tg, [w, h]) {
        Core_1.assert(this.ancestors.length === 1);
        const root = this.current;
        while (root.firstChild !== null) {
            root.removeChild(root.firstChild);
        }
        const width = parseFloat(Core_1.__nonNull(root.getAttribute("width")));
        const height = parseFloat(Core_1.__nonNull(root.getAttribute("height")));
        this.withLocalFrame(scale(width / w, height / h), Core_1.id, () => {
            this.renderElement(Cursor_1.ExplValueCursor.descendant(null, tg));
        });
    }
    renderElement(tg /*<GraphicsElement>*/) {
        const g = Core_1.as(tg.tv.v, Graphics_1.GraphicsElement);
        if (g instanceof Graphics_1.Circle) {
            return this.circle(tg);
        }
        else if (g instanceof Graphics_1.Group) {
            return this.group(tg);
        }
        else if (g instanceof Graphics_1.Line) {
            return this.line(tg);
        }
        else if (g instanceof Graphics_1.Polyline) {
            return this.polyline(tg);
        }
        else if (g instanceof Graphics_1.Polymarkers) {
            return this.polymarkers(tg);
        }
        else if (g instanceof Graphics_1.Rect) {
            return this.rect(tg);
        }
        else if (g instanceof Graphics_1.Text) {
            return this.text(tg);
        }
        else if (g instanceof Graphics_1.Viewport) {
            return this.viewport(tg);
        }
        else {
            return Core_1.absurd();
        }
    }
    // Scalings accumulate as we go down. Translations don't, because we use nested SVGs.
    withLocalFrame(scale, translate, localRender) {
        let result;
        this.scalings.push(postcompose(Array_1.last(this.scalings), scale));
        this.translations.push(translate);
        result = localRender();
        this.translations.pop();
        this.scalings.pop();
        return result;
    }
    // Scale circle by product of x, y scaling factors to maintain ratio of area to fixed rectangle as an invariant.
    circle(tg /*<Rect>*/) {
        const g = Core_1.as(tg.tv.v, Graphics_1.Circle);
        const [x, y] = this.transform([g.x.val, g.y.val]);
        const [x_scale, y_scale] = this.scale([1, 1]);
        const r = Renderer_1.circle(x, y, g.radius.val * x_scale * y_scale, "none", g.fill.val, this.circle);
        this.current.appendChild(r);
        return r;
    }
    group(tg /*<Group>*/) {
        const g = Renderer_1.group();
        this.current.appendChild(g);
        this.ancestors.push(g);
        for (let tg̅ = tg.to(Graphics_1.Group, "gs"); BaseTypes_1.Cons.is(Core_1.as(tg̅.tv.v, BaseTypes_1.List)); tg̅ = tg̅.to(BaseTypes_1.Cons, "tail")) {
            this.renderElement(tg̅.to(BaseTypes_1.Cons, "head"));
        }
        this.ancestors.pop();
        return g;
    }
    // For line/polyline, each point is considered a "child", and therefore subject to my local scaling.
    line(tg /*<Polyline>*/) {
        const g = Core_1.as(tg.tv.v, Graphics_1.Line);
        const [[x1, y1], [x2, y2]] = [
            this.transform([g.p1.x.val, g.p1.y.val]),
            this.transform([g.p2.x.val, g.p2.y.val])
        ];
        const l = Renderer_1.lineRounded(x1, y1, x2, y2, g.stroke.val, g.strokeWidth.val);
        this.current.appendChild(l);
        return l;
    }
    polyline(tg /*<Polyline>*/) {
        const g = Core_1.as(tg.tv.v, Graphics_1.Polyline);
        const ps = g.points.toArray().map((p) => {
            return this.transform([p.x.val, p.y.val]);
        });
        const l = Renderer_1.polyline(ps, g.stroke.val, g.strokeWidth.val);
        this.current.appendChild(l);
        return l;
    }
    // Polymarkers have coordinates relative to the points, in the *parent* scaling.
    polymarkers(tg /*<Polymarkers>*/) {
        const g = Renderer_1.group();
        this.current.appendChild(g);
        this.ancestors.push(g);
        const invScale = invertScale(this.scale);
        for (let tg̅ = tg.to(Graphics_1.Polymarkers, "markers"), tps = tg.to(Graphics_1.Polymarkers, "points"); BaseTypes_1.Cons.is(Core_1.as(tg̅.tv.v, BaseTypes_1.List)) || BaseTypes_1.Cons.is(Core_1.as(tps.tv.v, BaseTypes_1.List)); tg̅ = tg̅.to(BaseTypes_1.Cons, "tail"), tps = tps.to(BaseTypes_1.Cons, "tail")) {
            if (!BaseTypes_1.Cons.is(Core_1.as(tg̅.tv.v, BaseTypes_1.List)) || !BaseTypes_1.Cons.is(Core_1.as(tps.tv.v, BaseTypes_1.List))) {
                Core_1.userError(`${Graphics_1.Polymarkers.name}: more markers than points.`);
            }
            else {
                const tp = tps.to(BaseTypes_1.Cons, "head");
                const p = Core_1.as(tp.tv.v, Graphics_1.Point);
                const [x, y] = this.transform([p.x.val, p.y.val]);
                const markerViewport = Renderer_1.svgElement(true, x, y, 10, 10, false, this.polymarkers);
                this.current.appendChild(markerViewport);
                this.ancestors.push(markerViewport);
                this.withLocalFrame(invScale, Core_1.id, () => {
                    const marker = this.renderElement(tg̅.to(BaseTypes_1.Cons, "head"));
                    if (marker instanceof SVGCircleElement) {
                        new Interactor_1.PointInteractor(this.editor, tp, marker);
                    }
                });
                this.ancestors.pop();
            }
        }
        this.ancestors.pop();
        return g;
    }
    rect(tg /*<Rect>*/) {
        const g = Core_1.as(tg.tv.v, Graphics_1.Rect);
        const [x, y] = this.transform([g.x.val, g.y.val]);
        const [width, height] = this.scale([g.width.val, g.height.val]);
        Core_1.assert(width >= 0 && height >= 0);
        const r = Renderer_1.rect(x, y, width, height, "none", g.fill.val, this.rect);
        new Interactor_1.RectInteractor(this.editor, tg, r);
        this.current.appendChild(r);
        return r;
    }
    text(tg /*<Text>*/) {
        const g = Core_1.as(tg.tv.v, Graphics_1.Text), [x, y] = this.transform([g.x.val, g.y.val]), t = Renderer_1.textElement_graphical(x, y, fontSize, g.str.val);
        this.current.appendChild(t);
        t.setAttribute("fill", "black");
        t.setAttribute("text-anchor", `${g.anchor.val}`);
        t.setAttribute("alignment-baseline", `${g.baseline.val}`);
        return t;
    }
    viewport(tg /*<Viewport>*/) {
        const g = Core_1.as(tg.tv.v, Graphics_1.Viewport);
        // dimensions are relative to parent coordinate space, so not transformed by g's scaling
        const [x, y] = this.transform([g.x.val, g.y.val]);
        const [width, height] = this.scale([g.width.val, g.height.val]);
        Core_1.assert(width >= 0 && height >= 0);
        const outerSvg = Renderer_1.svgElement(false, x, y, width, height, false, this.viewport);
        if (g.fill.val !== "none") {
            outerSvg.appendChild(Renderer_1.rect(0, 0, width, height, "none", g.fill.val, this.viewport));
        }
        this.current.appendChild(outerSvg);
        if (this.showInvisible) {
            this.current.appendChild(Renderer_1.border(x, y, width, height, "gray", true));
        }
        this.ancestors.push(outerSvg);
        const margin = g.margin.val;
        const [widthʹ, heightʹ] = [Math.max(width - margin * 2), height - margin * 2];
        const innerScale = ([x, y]) => {
            return [x * widthʹ / width, y * heightʹ / height];
        };
        const innerViewport = Renderer_1.svgElement(true, margin, margin, widthʹ, heightʹ, false, this.viewport);
        this.current.appendChild(innerViewport);
        this.ancestors.push(innerViewport);
        this.withLocalFrame(postcompose(innerScale, transformFun(g.scale)), transformFun(g.translate), () => {
            this.renderElement(tg.to(Graphics_1.Viewport, "g"));
        });
        this.ancestors.pop();
        this.ancestors.pop();
        return outerSvg;
    }
    setMarkerMid(el, C, colour) {
        const markerId = Renderer_1.markerEnsureDefined(this.root, C, colour); // revisit cast
        el.setAttribute("marker-mid", `url(#${markerId})`);
    }
}
exports.GraphicsRenderer = GraphicsRenderer;
{
    // Additional primitives that rely on offline rendering to compute text metrics. Combining these would 
    // require more general primitives that can return tuples.
    const textWidth = (str) => {
        return Versioned_1.num(exports.svg.textWidth(Renderer_1.textElement_graphical(0, 0, fontSize, str.val)));
    };
    const textHeight = (str) => {
        return Versioned_1.num(exports.svg.textHeight(Renderer_1.textElement_graphical(0, 0, fontSize, str.val)));
    };
    Primitive_1.unaryOps.set(textWidth.name, Primitive_1.unary_(textWidth));
    Primitive_1.unaryOps.set(textHeight.name, Primitive_1.unary_(textHeight));
}


/***/ }),
/* 26 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
// First-class module.
class SVG {
    constructor() {
        this.hiddenMetricsElement = document.createElementNS(SVG.NS, "svg");
        this.hiddenMetricsElement.setAttribute("width", "0");
        this.hiddenMetricsElement.setAttribute("height", "0");
        this.hiddenMetricsElement.style.visibility = "hidden";
        document.body.appendChild(this.hiddenMetricsElement);
    }
    textWidth(text) {
        this.hiddenMetricsElement.appendChild(text);
        const width = text.getBBox().width;
        text.remove();
        return width;
    }
    textHeight(text) {
        this.hiddenMetricsElement.appendChild(text);
        const height = text.getBBox().height;
        text.remove();
        return height;
    }
}
exports.SVG = SVG;
SVG.NS = "http://www.w3.org/2000/svg";


/***/ }),
/* 27 */
/***/ (function(module, exports, __webpack_require__) {

var api = __webpack_require__(18);
            var content = __webpack_require__(41);

            content = content.__esModule ? content.default : content;

            if (typeof content === 'string') {
              content = [[module.i, content, '']];
            }

var options = {};

options.insert = "head";
options.singleton = false;

var update = api(content, options);

var exported = content.locals ? content.locals : {};



module.exports = exported;

/***/ }),
/* 28 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const Annotation_1 = __webpack_require__(10);
const DataValue_1 = __webpack_require__(3);
const Eval_1 = __webpack_require__(16);
const Expl_1 = __webpack_require__(5);
const Graphics_1 = __webpack_require__(8);
const Versioned_1 = __webpack_require__(2);
const Cursor_1 = __webpack_require__(17);
const Renderer_1 = __webpack_require__(15);
const View_1 = __webpack_require__(29);
__webpack_require__(27);
// Previously Editor, but clashes a bit with the Wrattler class of the same name.
var Pane;
(function (Pane_1) {
    function initialise(resourceServerUrl) {
        View_1.View.initialise(resourceServerUrl);
    }
    Pane_1.initialise = initialise;
    class Pane {
        constructor(listener, appRoot, [width, height], tooltipPlacement, ρ_external, ρ_imports, e) {
            this.slice = new Set();
            this.listener = listener;
            this.rootPane = Renderer_1.svgRootElement(width, height);
            this.tooltips = new Set();
            this.tooltipPlacement = tooltipPlacement;
            Renderer_1.markerEnsureDefined(this.rootPane, Graphics_1.Arrowhead, "blue");
            appRoot.appendChild(this.rootPane);
            this.ρ_external = ρ_external;
            this.ρ_imports = ρ_imports;
            this.e = e;
            // evaluate twice so we can start with an empty delta
            this.tv = Eval_1.Eval.eval_(this.ρ, this.e);
            this.here = Cursor_1.ExplValueCursor.descendant(null, this.tv);
            this.direction = Annotation_1.Direction.Fwd;
            Versioned_1.newRevision();
            Eval_1.Eval.eval_(this.ρ, this.e); // reestablish reachable nodes
        }
        get ρ() {
            return this.ρ_external.concat(this.ρ_imports);
        }
        visibleTooltips() {
            return [...this.tooltips].filter(tooltip => tooltip.state.isVisible);
        }
        initialise() {
            this.render();
            const this_ = this;
            // https://stackoverflow.com/questions/5597060
            document.onkeydown = function (ev) {
                if (ev.shiftKey) {
                    if (ev.keyCode == 37) { // left
                        this_.here = this_.here.prevSibling();
                        this_.render();
                    }
                    else if (ev.keyCode == 38) { // up
                        if (this_.here.hasParent()) {
                            this_.here = this_.here.up();
                            this_.render();
                        }
                    }
                    else if (ev.keyCode == 39) { // right
                        this_.here = this_.here.nextSibling();
                        this_.render();
                    }
                    else if (ev.keyCode == 40) { // down
                        if (this_.here.tv.v instanceof DataValue_1.DataValue) {
                            this_.here = this_.here.toChild(0);
                            this_.render();
                        }
                    }
                }
            };
            document.onkeypress = function (ev) {
                if (ev.shiftKey) {
                    if (ev.key === "V") {
                        View_1.View.existingView(this_.here.tv).toggleValue();
                        this_.render();
                    }
                    else if (ev.key === "E") {
                        View_1.View.existingView(this_.here.tv).toggleExpl();
                        this_.render();
                    }
                }
            };
        }
        bwdSlice(setNeeded) {
            Annotation_1.__slice.reset(Annotation_1.Direction.Bwd);
            setNeeded();
            Eval_1.Eval.eval_bwd(this.e, this.tv);
            Annotation_1.__slice.ann = Annotation_1.__slice.restrictTo(this.ρ_external.values());
            this.listener.onBwdSlice(this, Annotation_1.__slice.ann);
            this.direction = Annotation_1.Direction.Bwd;
            this.slice = Annotation_1.__slice.ann;
        }
        // Forward-slice with respect to supplied slice of ρ_external.
        fwdSlice(externDeps) {
            Annotation_1.__slice.direction = Annotation_1.Direction.Fwd;
            Annotation_1.__slice.ann = externDeps;
            Eval_1.Eval.eval_fwd(this.e, this.tv);
            Annotation_1.__slice.ann = Annotation_1.__slice.restrictTo([this.tv]);
            this.direction = Annotation_1.Direction.Fwd;
            this.slice = Annotation_1.__slice.ann;
        }
        render() {
            // https://stackoverflow.com/questions/48310643
            const children = Array.from(this.rootPane.childNodes);
            children.forEach((child) => {
                if (!(child instanceof SVGDefsElement)) {
                    this.rootPane.removeChild(child);
                }
            });
            this.tooltips.forEach(tooltip => tooltip.destroy());
            this.tooltips.clear();
            View_1.View.render(this);
        }
        onEdit() {
            this.tv = Eval_1.Eval.eval_(this.ρ, this.e);
            this.here = Cursor_1.ExplValueCursor.descendant(null, DataValue_1.explValue(Core_1.as(this.tv.t, Expl_1.Expl.Defs).t, this.tv.v));
            // cursor may no longer be valid, how to deal with that?
            this.render();
        }
        onViewChange() {
            this.render();
        }
    }
    Pane_1.Pane = Pane;
})(Pane = exports.Pane || (exports.Pane = {}));


/***/ }),
/* 29 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const Array_1 = __webpack_require__(11);
const BaseTypes_1 = __webpack_require__(4);
const DataType_1 = __webpack_require__(7);
const DataValue_1 = __webpack_require__(3);
const Delta_1 = __webpack_require__(13);
const Env_1 = __webpack_require__(14);
const Eval_1 = __webpack_require__(16);
const Expl_1 = __webpack_require__(5);
const Expr_1 = __webpack_require__(6);
const Graphics_1 = __webpack_require__(8);
const Match_1 = __webpack_require__(9);
const Module_1 = __webpack_require__(22);
const Value_1 = __webpack_require__(1);
const Versioned_1 = __webpack_require__(2);
const Cursor_1 = __webpack_require__(17);
const GraphicsRenderer_1 = __webpack_require__(25);
const Renderer_1 = __webpack_require__(15);
var Closure = Eval_1.Eval.Closure;
var View;
(function (View_1) {
    View_1.defaultDims = [320, 360];
    function initialise(resourceServerUrl) {
        Module_1.Module.initialise(resourceServerUrl);
        // Shenanigans to call an internal function. Will extract this into a (reverse) FFI.
        const x = "g";
        const [ρ, dimsExpr] = Module_1.parseWithImports(`dimensions ${x}`);
        View_1.dimensions = function (tg) {
            const tv = Eval_1.Eval.eval_(ρ.concat(Env_1.Env.singleton(Versioned_1.str(x)(Versioned_1.ν()), tg)), dimsExpr);
            if (tv.v instanceof Graphics_1.Point) {
                return [tv.v.x.val, tv.v.y.val];
            }
            else {
                return Core_1.absurd();
            }
        };
    }
    View_1.initialise = initialise;
    // Prefer globals to threading parameters everywhere.
    let __currentEditor = null;
    const __links = new Set();
    const __svgs = new Map(); // memoised render within a single update
    function render(editor) {
        __svgs.clear();
        __links.clear();
        Core_1.assert(__currentEditor === null);
        __currentEditor = editor;
        const g = view(editor.tv, true, false).render();
        editor.rootPane.appendChild(g); // need to render main view so links can make use of getBoundingClientRect
        renderLinks(__links).forEach((link) => {
            editor.rootPane.appendChild(link);
        });
        __currentEditor = null;
    }
    View_1.render = render;
    const views = new Map(); // persists across edits
    function existingView(tv) {
        return Core_1.__nonNull(views.get(tv));
    }
    View_1.existingView = existingView;
    function isExprFor(e, C) {
        return Core_1.classOf(e) === DataType_1.exprClass(C);
    }
    // Unpack evaluation memo-key to recover original expression. TODO: make generic
    // and move near to memo code.
    function exprFor(t) {
        if (Versioned_1.versioned(t)) {
            return Core_1.as(Core_1.as(Core_1.as(t.__id, Value_1.TaggedId).k, Value_1.ApplicationId).v, Expr_1.Expr.Expr);
        }
        else {
            return Core_1.absurd();
        }
    }
    function renderLinks(links) {
        return [...links].map(((link) => {
            return Renderer_1.connector(Core_1.__nonNull(__svgs.get(link.from)), Core_1.__nonNull(__svgs.get(link.to)));
        }));
    }
    class View {
        render() {
            const g = this.render_();
            __svgs.set(this, g);
            return g;
        }
    }
    class ExprView extends View {
        constructor(parens, e) {
            super();
            this.parens = parens;
            this.e = e;
        }
        render_() {
            const parens = this.parens;
            const e = this.e;
            if (e instanceof Expr_1.Expr.ConstNum) {
                // ouch: disregard delta-info on expression itself
                return Renderer_1.horiz(num_(e.val, e.val));
            }
            else if (e instanceof Expr_1.Expr.ConstStr) {
                // ouch: disregard delta-info on expression itself
                return Renderer_1.horiz(str_(e.val));
            }
            else if (e instanceof Expr_1.Expr.Fun) {
                const g = Renderer_1.horizSpace(Renderer_1.keyword("fun", Renderer_1.deltaStyle(e)), elim(e.σ));
                return Renderer_1.parenthesiseIf(parens, g, Renderer_1.deltaStyle(e));
            }
            else if (e instanceof Expr_1.Expr.DataExpr) {
                if (isExprFor(e, BaseTypes_1.Pair)) {
                    return pair_expr(e);
                }
                else if (isExprFor(e, BaseTypes_1.Nil) || isExprFor(e, BaseTypes_1.Cons)) {
                    return list_expr(parens, e);
                }
                else {
                    return dataConstr_expr(parens, e);
                }
            }
            else if (e instanceof Expr_1.Expr.Quote) {
                return Renderer_1.unimplemented(e);
            }
            else if (e instanceof Expr_1.Expr.Var) {
                // ouch: disregard delta-info on Var.x
                return Renderer_1.horiz(Renderer_1.text(e.x.val, Renderer_1.deltaStyle(e)));
            }
            else if (e instanceof Expr_1.Expr.App) {
                return Renderer_1.parenthesiseIf(parens, Renderer_1.horizSpace(expr(!(e.f instanceof Expr_1.Expr.App), e.f), expr(true, e.e)), Renderer_1.deltaStyle(e));
            }
            else if (e instanceof Expr_1.Expr.BinaryApp) {
                // ignore operator precedence, but allow function application to take priority over any binary operation
                return Renderer_1.parenthesiseIf(parens, Renderer_1.horizSpace(expr(!(e.e1 instanceof Expr_1.Expr.App), e.e1), Renderer_1.text(e.opName.val, Renderer_1.deltaStyle(e)), // what about changes associated with e.opName
                expr(!(e.e2 instanceof Expr_1.Expr.App), e.e2)), Renderer_1.deltaStyle(e));
            }
            else if (e instanceof Expr_1.Expr.Defs) {
                return Renderer_1.parenthesiseIf(parens, Renderer_1.vert(Renderer_1.vert(...e.def̅.toArray().map(def_ => def(def_))), expr(false, e.e)), Renderer_1.deltaStyle(e));
            }
            else if (e instanceof Expr_1.Expr.MatchAs) {
                return Renderer_1.vert(Renderer_1.horizSpace(Renderer_1.keyword("match", Renderer_1.deltaStyle(e)), expr(false, e.e), Renderer_1.keyword("as", Renderer_1.deltaStyle(e))), elim(e.σ));
            }
            else if (e instanceof Expr_1.Expr.Typematch) {
                return Renderer_1.vert(Renderer_1.horizSpace(Renderer_1.keyword("typematch", Renderer_1.deltaStyle(e)), expr(false, e.e), Renderer_1.keyword("as", Renderer_1.deltaStyle(e))), ...e.cases.toArray().map(({ fst: x, snd: e }) => Renderer_1.horizSpace(Renderer_1.text(x.val, Renderer_1.deltaStyle(x)), Renderer_1.arrow(Renderer_1.deltaStyle(e)), expr(false, e))));
            }
            else {
                return Core_1.absurd(`Unimplemented expression form: ${Core_1.className(e)}.`);
            }
        }
    }
    class ExplValueView extends View {
        constructor(tv, show_v, show_ts) {
            super();
            this.tv = tv;
            this.show_v = show_v;
            this.show_ts = show_ts;
            this.initialise();
        }
        assertValid() {
            Core_1.assert(this.show_v || this.show_ts);
        }
        initialise() {
            const ts = splitExpls(this.tv.t);
            if (ts.length === 0 || !this.show_ts) {
                this.t_visible = false;
                this.v_visible = true;
            }
            else {
                this.t_visible = true;
                this.v_visible = this.show_v;
            }
            return [ts, splitValue(this.tv)];
        }
        render_() {
            this.assertValid();
            const [ts, tv] = this.initialise();
            let g;
            if (!this.v_visible) {
                g = expls(ts);
            }
            else if (!this.t_visible) {
                g = valueView(tv).render();
            }
            else {
                g = Renderer_1.vert(expls(ts), Renderer_1.horizSpace(Renderer_1.text("▸", Renderer_1.deltaStyle(Array_1.nth(ts, ts.length - 1))), valueView(tv).render()));
            }
            if (this.tv === __currentEditor.here.tv) {
                return Renderer_1.addBorder_focus(!this.t_visible && ts.length > 0 ? Renderer_1.edge_left(g) : g);
            }
            else {
                return g;
            }
        }
        toggleValue() {
            if (!this.show_v) {
                this.show_v = true;
            }
            else if (this.show_ts) {
                this.show_v = false;
            }
        }
        toggleExpl() {
            if (!this.show_ts) {
                this.show_ts = true;
            }
            else if (this.show_v) {
                this.show_ts = false;
            }
        }
    }
    class ExplView extends View {
        constructor(t) {
            super();
            this.t = t;
            this.bodyVisible = false;
        }
        render_() {
            let g;
            if (this.t instanceof Expl_1.Expl.Var) {
                g = Renderer_1.horiz(Renderer_1.text(this.t.x.val, Renderer_1.deltaStyle(this.t)));
            }
            else if (this.t instanceof Expl_1.Expl.UnaryApp) {
                g = view(this.t.tf, false, true).render();
            }
            else if (this.t instanceof Expl_1.Expl.BinaryApp) {
                g = Renderer_1.horizSpace(view(this.t.tv1, false, true).render(), Renderer_1.text(this.t.opName.val, Renderer_1.deltaStyle(this.t)), // what about changes associated with t.opName?
                view(this.t.tv2, false, true).render());
            }
            else if (this.t instanceof Expl_1.Expl.App) {
                g = Renderer_1.vert(Renderer_1.horizSpace(view(this.t.tf, false, true).render(), view(this.t.tu, false, true).render()), this.appBody());
            }
            else if (this.t instanceof Expl_1.Expl.Defs) {
                g = Renderer_1.vert(...this.t.def̅.toArray().map(defₜ));
            }
            else if (this.t instanceof Expl_1.Expl.MatchAs) {
                g = Renderer_1.vert(Renderer_1.horizSpace(Renderer_1.keyword("match", Renderer_1.deltaStyle(this.t)), view(this.t.tu, false, true).render(), Renderer_1.keyword("as", Renderer_1.deltaStyle(this.t))), elimMatch(this.t.ξ));
            }
            else if (this.t instanceof Expl_1.Expl.Typematch) {
                return Renderer_1.unimplemented(this.t);
            }
            else {
                return Core_1.absurd("Unknown explanation form", this.t);
            }
            return Renderer_1.shading(g, "white");
        }
        appBody() {
            const app = Core_1.as(this.t, Expl_1.Expl.App);
            const ts = splitExpls(app.t);
            if (ts.length === 0 || this.bodyVisible) {
                return expls(ts);
            }
            else {
                const g = Renderer_1.ellipsis(Renderer_1.deltaStyle(app.t));
                g.addEventListener("click", (ev) => {
                    ev.stopPropagation();
                    this.bodyVisible = true;
                    __currentEditor.onViewChange();
                });
                return g;
            }
        }
    }
    View_1.ExplView = ExplView;
    class ValueView extends View {
        constructor(tv) {
            super();
            this.tv = tv;
        }
        render_() {
            let g;
            if (this.tv.v instanceof Value_1.Num) {
                const e = exprFor(this.tv.t);
                g = Renderer_1.horiz(num_(this.tv.v, e instanceof Expr_1.Expr.ConstNum ? e.val : undefined));
            }
            else if (this.tv.v instanceof Value_1.Str) {
                g = Renderer_1.horiz(str_(this.tv.v));
            }
            else if (this.tv.v instanceof Closure) {
                // treat closures as their function literals, for now
                g = Renderer_1.horizSpace(Renderer_1.keyword("fun", Renderer_1.deltaStyle(this.tv.v)), elim(this.tv.v.f));
            }
            else if (this.tv.v instanceof DataValue_1.DataValue) {
                if (this.tv.v instanceof Graphics_1.GraphicsElement) {
                    const tg = this.tv;
                    const dim = { width: View_1.defaultDims[0], height: View_1.defaultDims[1] };
                    let g1;
                    [g, g1] = Renderer_1.svgElement_inverted(dim.width, dim.height);
                    new GraphicsRenderer_1.GraphicsRenderer(__currentEditor, g, g1).render(tg, Core_1.__nonNull(View_1.dimensions)(tg));
                    Renderer_1.__dimensions.set(g, dim);
                }
                else if (this.tv.v instanceof BaseTypes_1.Pair) {
                    g = pair(this.tv);
                }
                else if (this.tv.v instanceof BaseTypes_1.List) {
                    g = list(this.tv);
                }
                else {
                    g = dataConstr(false, this.tv);
                }
            }
            else {
                g = Renderer_1.unimplemented(this.tv.v);
            }
            return Renderer_1.shading(g, "lavender");
        }
    }
    View_1.ValueView = ValueView;
    // Values are treated slightly differently because the "key" of a value view is the value (to distinguish
    // it from the view of the ExplValue), but the Expl is also required to render the value.
    function valueView(tv) {
        let w = views.get(Core_1.__nonNull(tv).v);
        if (w === undefined) {
            w = new ValueView(tv);
            views.set(tv.v, w);
            return w;
        }
        else {
            return w;
        }
    }
    View_1.valueView = valueView;
    function view(tv, show_v, show_ts) {
        let w = views.get(tv);
        if (w === undefined) {
            w = new ExplValueView(tv, show_v, show_ts);
            views.set(tv, w);
            return w;
        }
        else {
            return w;
        }
    }
    View_1.view = view;
    function view_child(C, tv, prop_, show_v, show_ts) {
        if (Versioned_1.versioned(tv.v) && Versioned_1.versioned(tv.t)) {
            const prop = prop_;
            const w = view(Expl_1.Expl.explChild(tv.t, tv.v, prop_), show_v, show_ts);
            const g = w.render();
            if (tv.v.__ẟ instanceof Delta_1.Change && tv.v.__ẟ.hasChanged(prop)) {
                // All a bit hacky, need to rethink:
                const t_prev = tv.t.__ẟ instanceof Delta_1.Change && tv.t.__ẟ.hasChanged(prop) ?
                    Core_1.as(tv.t.__ẟ.changed[prop].before, Expl_1.Expl.Expl) :
                    tv.t;
                const w_existing = views.get(DataValue_1.explValue(t_prev, Core_1.as(tv.v.__ẟ.changed[prop].before, Value_1.Value)));
                if (w_existing) {
                    __links.add({ from: w, to: w_existing });
                }
                return Renderer_1.addBorder_changed(g);
            }
            else {
                return g;
            }
        }
        else {
            return Core_1.absurd();
        }
    }
    function explView(t) {
        let w = views.get(t);
        if (w === undefined) {
            w = new ExplView(t);
            views.set(t, w);
            return w;
        }
        else {
            return w;
        }
    }
    View_1.explView = explView;
    function splitExpls(t) {
        if (t instanceof Expl_1.Expl.Const) {
            return [];
        }
        else if (t instanceof Expl_1.Expl.Fun) {
            return [];
        }
        else if (t instanceof Expl_1.Expl.DataExpl) {
            return [];
        }
        else if (t instanceof Expl_1.Expl.Var) {
            // values of variables themselves have explanations, but ignore those for now
            return [t];
        }
        else 
        // don't recurse into App as it has its own expansion state
        if (t instanceof Expl_1.Expl.UnaryApp || t instanceof Expl_1.Expl.BinaryApp) {
            return [t];
        }
        else if (t instanceof Expl_1.Expl.App) {
            return [t];
        }
        else if (t instanceof Expl_1.Expl.NonTerminal) {
            return [t, ...splitExpls(t.t)];
        }
        else {
            return Core_1.absurd("Unknown explanation form", t);
        }
    }
    View_1.splitExpls = splitExpls;
    // The value part must be an ExplValue, because in the data value case we need the explanation as well to
    // render the value.
    function splitValue(tv) {
        const { t, v } = tv;
        if (t instanceof Expl_1.Expl.Const) {
            return tv;
        }
        else if (t instanceof Expl_1.Expl.Fun) {
            return tv;
        }
        else if (t instanceof Expl_1.Expl.DataExpl) {
            return tv;
        }
        else if (t instanceof Expl_1.Expl.Var) {
            // values of variables themselves have explanations, but ignore those for now
            return splitValue(DataValue_1.explValue(t.t, v));
        }
        else 
        // don't recurse into App as it has its own expansion state
        if (t instanceof Expl_1.Expl.UnaryApp || t instanceof Expl_1.Expl.BinaryApp) {
            return tv;
        }
        else if (t instanceof Expl_1.Expl.App) {
            return tv;
        }
        else if (t instanceof Expl_1.Expl.NonTerminal) {
            return splitValue(DataValue_1.explValue(t.t, v));
        }
        else {
            return Core_1.absurd();
        }
    }
    View_1.splitValue = splitValue;
    function expls(ts) {
        return Renderer_1.vert(...ts.map(t => explView(t).render()));
    }
    function compareCtr(c1, c2) {
        const n = DataType_1.ctrFor(c1).arity - DataType_1.ctrFor(c2).arity;
        return n === 0 ? c1.localeCompare(c2) : n;
    }
    function cont(κ) {
        if (κ instanceof Expr_1.Expr.Expr) {
            return [[[], κ]];
        }
        else if (κ instanceof Match_1.Elim) {
            return clauses(κ);
        }
        else {
            return Core_1.absurd();
        }
    }
    function clauses(σ) {
        if (Match_1.VarElim.is(σ)) {
            const cs = cont(σ.κ);
            // disregard any delta information on x :-/
            return cs.map(([cxs, e]) => [[[σ.x, Renderer_1.deltaStyle(σ)], ...cxs], e]);
        }
        else if (Match_1.DataElim.is(σ)) {
            const cκs = Array_1.zip(Value_1.fields(σ), σ.__children).sort(([c1,], [c2,]) => compareCtr(c1, c2));
            return Array_1.flatten(cκs.filter(([c, κ]) => κ !== undefined).map(([c, κ]) => cont(Core_1.__nonNull(κ)).map(([cxs, e]) => [[[DataType_1.ctrFor(c), Renderer_1.deltaStyle(σ)], ...cxs], e])));
        }
        else {
            return Core_1.absurd();
        }
    }
    function consComma(ẟ_style, src) {
        const g = Renderer_1.comma(ẟ_style);
        g.addEventListener("click", (ev) => {
            ev.stopPropagation();
            if (src !== undefined) {
                Versioned_1.newRevision();
                if (ev.metaKey) {
                    if (ev.altKey) {
                        // if my tail is another cons, swap the two head elements
                        const e = Core_1.as(new Cursor_1.ExprCursor(src).constr_to(BaseTypes_1.Cons, "tail").v, Expr_1.Expr.Expr);
                        if (isExprFor(e, BaseTypes_1.Cons)) {
                            const e1 = Core_1.as(new Cursor_1.ExprCursor(src).constr_to(BaseTypes_1.Cons, "head").v, Expr_1.Expr.Expr);
                            const e2 = Core_1.as(new Cursor_1.ExprCursor(e).constr_to(BaseTypes_1.Cons, "head").v, Expr_1.Expr.Expr);
                            // constr_splice on src, replacing head with head of src.tail
                            // constr_splice on src.tail, replacing head with head of src
                            new Cursor_1.ExprCursor(src).constr_splice(BaseTypes_1.Cons, ["head"], ([e]) => {
                                return [e2];
                            });
                            new Cursor_1.ExprCursor(e).constr_splice(BaseTypes_1.Cons, ["head"], ([e]) => {
                                return [e1];
                            });
                        }
                    }
                    else {
                        new Cursor_1.ExprCursor(src).constr_splice(BaseTypes_1.Cons, ["head"], ([e]) => {
                            const eʹ = Expr_1.Expr.app(Expr_1.Expr.var_(Versioned_1.str("sq")(Versioned_1.ν()))(Versioned_1.ν()), Expr_1.Expr.var_(Versioned_1.str("x")(Versioned_1.ν()))(Versioned_1.ν()))(Versioned_1.ν());
                            return [Versioned_1.at(DataType_1.exprClass(BaseTypes_1.Pair), e, eʹ)(Versioned_1.ν())];
                        });
                    }
                }
                else {
                    new Cursor_1.ExprCursor(src).constr_splice(BaseTypes_1.Cons, ["tail"], ([e]) => {
                        const eʹ = Expr_1.Expr.constNum(Versioned_1.num(0)(Versioned_1.ν()))(Versioned_1.ν());
                        return [Versioned_1.at(DataType_1.exprClass(BaseTypes_1.Cons), eʹ, e)(Versioned_1.ν())];
                    });
                }
                __currentEditor.onEdit();
            }
        });
        return g;
    }
    function dataConstr(parens, { t, v }) {
        const tvs = Expl_1.Expl.explChildren(t, v);
        // a constructor expression makes its value, so their root delta highlighting must agree
        const gs = tvs.map(tvʹ => view(tvʹ, true, false).render());
        const g = Renderer_1.horizSpace(Renderer_1.text(v.ctr, Renderer_1.deltaStyle(v)), ...(tvs.length > 2 ? [Renderer_1.vert(...gs)] : gs));
        return Renderer_1.parenthesiseIf(tvs.length > 0 && parens, g, Renderer_1.deltaStyle(t));
    }
    function dataConstr_expr(parens, e) {
        const es = e.__children;
        const gs = es.map(eʹ => expr(true, eʹ));
        const g = Renderer_1.horizSpace(Renderer_1.text(e.ctr, Renderer_1.deltaStyle(e)), ...(es.length > 2 ? [Renderer_1.vert(...gs)] : gs));
        return Renderer_1.parenthesiseIf(es.length > 0 && parens, g, Renderer_1.deltaStyle(e));
    }
    function def(def) {
        if (def instanceof Expr_1.Expr.Prim) {
            return Renderer_1.horizSpace(Renderer_1.keyword("primitive", Renderer_1.deltaStyle(def)), patternVar(def.x));
        }
        else if (def instanceof Expr_1.Expr.Let) {
            if (def.e instanceof Expr_1.Expr.Fun) {
                return Renderer_1.horizSpace(Renderer_1.keyword("let_", Renderer_1.deltaStyle(def)), patternVar(def.x), elim(def.e.σ));
            }
            else {
                return Renderer_1.horizSpace(Renderer_1.keyword("let_", Renderer_1.deltaStyle(def)), patternVar(def.x), Renderer_1.keyword("equals", Renderer_1.deltaStyle(def)), expr(false, def.e));
            }
        }
        else if (def instanceof Expr_1.Expr.LetRec) {
            return Renderer_1.horizSpace(Renderer_1.keyword("letRec", Renderer_1.deltaStyle(def)), Renderer_1.vert(...def.δ.toArray().map(def => recDef(def))));
        }
        else {
            return Core_1.absurd();
        }
    }
    function defₜ(def) {
        if (def instanceof Expl_1.Expl.Prim) {
            return Renderer_1.horizSpace(Renderer_1.keyword("primitive", Renderer_1.deltaStyle(def)), patternVar(def.x));
        }
        else if (def instanceof Expl_1.Expl.Let) {
            if (def.tv.t instanceof Expl_1.Expl.Fun && def.tv.v instanceof Closure) {
                return Renderer_1.horizSpace(Renderer_1.keyword("let_", Renderer_1.deltaStyle(def)), patternVar(def.x), elim(def.tv.v.f));
            }
            else {
                return Renderer_1.horizSpace(Renderer_1.keyword("let_", Renderer_1.deltaStyle(def)), patternVar(def.x), Renderer_1.keyword("equals", Renderer_1.deltaStyle(def)), view(def.tv, false, true).render());
            }
        }
        else if (def instanceof Expl_1.Expl.LetRec) {
            return Renderer_1.horizSpace(Renderer_1.keyword("letRec", Renderer_1.deltaStyle(def)), Renderer_1.vert(...def.δ.toArray().map(def => recDefₜ(def))));
        }
        else {
            return Core_1.absurd();
        }
    }
    function elim(σ) {
        return Renderer_1.vert(...clauses(σ).map(([cxs, e]) => {
            const [[g], cxsʹ] = patterns(false, 1, cxs);
            Core_1.assert(cxsʹ.length === 0);
            const gʹ = e instanceof Expr_1.Expr.Fun ?
                elim(e.σ) : // curried function resugaring
                Renderer_1.horizSpace(Renderer_1.arrow(Renderer_1.deltaStyle(e)), expr(false, e));
            return Renderer_1.horizSpace(g, gʹ);
        }));
    }
    // Hack just to support Bool, Ordering, etc.
    function elimMatch(ξ) {
        const tv = Array_1.nth(ξ.tv̅.toArray(), 0);
        // don't think the contination is needed; already stored in the trace
        return Renderer_1.horizSpace(Renderer_1.text(tv.v.ctr, Renderer_1.deltaStyle(tv.v)), Renderer_1.arrow(Renderer_1.deltaStyle(tv.v)));
    }
    function expr_(parens, e) {
        let w = views.get(e);
        if (w === undefined) {
            w = new ExprView(parens, e);
            views.set(e, w);
            return w;
        }
        else {
            return w;
        }
    }
    function expr(parens, e) {
        return expr_(parens, e).render();
    }
    // Really want some kind of view typeclass, so this isn't specific to expression. Also: consolidate with ExprCursor.
    function expr_child(C, parens, e, prop) {
        if (Versioned_1.versioned(e)) {
            const w = expr_(parens, e.__child(prop));
            const g = w.render();
            if (e.__ẟ instanceof Delta_1.Change && e.__ẟ.hasChanged(prop)) {
                const w_existing = views.get(Core_1.as(e.__ẟ.changed[prop].before, Expr_1.Expr.Expr));
                if (w_existing) {
                    __links.add({ from: w, to: w_existing });
                }
                return Renderer_1.addBorder_changed(g);
            }
            else {
                return g;
            }
        }
        else {
            return Core_1.absurd();
        }
    }
    function list({ t, v }) {
        if (BaseTypes_1.Cons.is(v)) {
            const vʹ = v;
            const e = exprFor(t);
            return Renderer_1.horiz(view_child(BaseTypes_1.Cons, DataValue_1.explValue(t, vʹ), "head", true, false), consComma(Renderer_1.deltaStyle(v), isExprFor(e, BaseTypes_1.Cons) ? e : undefined), Renderer_1.space(), view(Expl_1.Expl.explChild(t, vʹ, "tail"), true, false).render());
        }
        else if (BaseTypes_1.Nil.is(v)) {
            return Renderer_1.horiz(Renderer_1.centreDot(Renderer_1.deltaStyle(v)));
        }
        else {
            return Core_1.absurd();
        }
    }
    function list_expr(parens, e) {
        if (isExprFor(e, BaseTypes_1.Cons)) {
            return Renderer_1.parenthesiseIf(parens, Renderer_1.horiz(expr_child(BaseTypes_1.Cons, false, e, "head"), consComma(Renderer_1.deltaStyle(e), e), Renderer_1.space(), list_expr(false, e.__child("tail"))), Renderer_1.deltaStyle(e));
        }
        else if (isExprFor(e, BaseTypes_1.Nil)) {
            return Renderer_1.horiz(Renderer_1.centreDot(Renderer_1.deltaStyle(e)));
        }
        else {
            return Renderer_1.horiz(expr(false, e)); // promote to nested SVG; need to rethink
        }
    }
    function num_(n, src) {
        const g = Renderer_1.text(n.toString(), Renderer_1.deltaStyle(n));
        if (src && Number.isInteger(src.val)) {
            g.addEventListener("click", (ev) => {
                Versioned_1.newRevision();
                new Cursor_1.ExprCursor(src).setNum(ev.metaKey ? src.val - 1 : src.val + 1);
                ev.stopPropagation();
                __currentEditor.onEdit();
            });
        }
        return g;
    }
    function pair(tv) {
        return Renderer_1.parenthesise(Renderer_1.horiz(view_child(BaseTypes_1.Pair, tv, "fst", true, false), pairComma(Renderer_1.deltaStyle(tv.t), exprFor(tv.t)), Renderer_1.space(), view_child(BaseTypes_1.Pair, tv, "snd", true, false)), Renderer_1.deltaStyle(tv.t));
    }
    function pairComma(ẟ_style, src) {
        const g = Renderer_1.comma(ẟ_style);
        g.addEventListener("click", (ev) => {
            ev.stopPropagation();
            if (src !== undefined) {
                Versioned_1.newRevision();
                if (ev.metaKey) {
                    new Cursor_1.ExprCursor(src).constr_splice(BaseTypes_1.Pair, ["fst", "snd"], ([e1, e2]) => {
                        return [e2, e1];
                    });
                }
                __currentEditor.onEdit();
            }
        });
        return g;
    }
    function pair_expr(e) {
        return Renderer_1.parenthesise(Renderer_1.horiz(expr_child(BaseTypes_1.Pair, false, e, "fst"), pairComma(Renderer_1.deltaStyle(e), e), Renderer_1.space(), expr_child(BaseTypes_1.Pair, false, e, "snd")), Renderer_1.deltaStyle(e));
    }
    function patterns(parens, n, cxs) {
        if (n === 0) {
            return [[], cxs];
        }
        else {
            const [ctr_x, ẟ_style] = cxs[0];
            if (ctr_x instanceof DataType_1.Ctr) {
                if (ctr_x.C === BaseTypes_1.Pair) {
                    const [[g1, g2], cxsʹ] = patterns(false, 2, cxs.slice(1));
                    const [gsʹ, cxsʹʹ] = patterns(parens, n - 1, cxsʹ);
                    return [[Renderer_1.parenthesise(Renderer_1.horiz(g1, Renderer_1.comma(ẟ_style), Renderer_1.space(), g2), ẟ_style), ...gsʹ], cxsʹʹ];
                }
                else if (ctr_x.C === BaseTypes_1.Nil) {
                    const [g, cxsʹ] = [Renderer_1.centreDot(ẟ_style), cxs.slice(1)];
                    const [gs, cxsʹʹ] = patterns(parens, n - 1, cxsʹ);
                    return [[g, ...gs], cxsʹʹ];
                }
                else if (ctr_x.C === BaseTypes_1.Cons) {
                    const [[g_head, g_tail], cxsʹ] = patterns(false, ctr_x.arity, cxs.slice(1));
                    const g = Renderer_1.horiz(g_head, Renderer_1.comma(ẟ_style), Renderer_1.space(), g_tail);
                    const [gsʹ, cxsʹʹ] = patterns(parens, n - 1, cxsʹ);
                    return [[Renderer_1.parenthesiseIf(ctr_x.arity > 0 && parens, g, ẟ_style), ...gsʹ], cxsʹʹ];
                }
                else {
                    const [gs, cxsʹ] = patterns(true, ctr_x.arity, cxs.slice(1));
                    const g = Renderer_1.horizSpace(Renderer_1.text(ctr_x.c, ẟ_style), ...gs);
                    const [gsʹ, cxsʹʹ] = patterns(parens, n - 1, cxsʹ);
                    return [[Renderer_1.parenthesiseIf(ctr_x.arity > 0 && parens, g, ẟ_style), ...gsʹ], cxsʹʹ];
                }
            }
            else if (ctr_x instanceof Value_1.Str) {
                const [gs, cxsʹ] = patterns(parens, n - 1, cxs.slice(1));
                // ouch, ignore ẟ_style coming from trie and use variable instead :-/
                return [[patternVar(ctr_x), ...gs], cxsʹ];
            }
            else {
                return Core_1.absurd();
            }
        }
    }
    function patternVar(x) {
        return Renderer_1.text(x.val, Renderer_1.deltaStyle(x));
    }
    function recDef(def) {
        return Renderer_1.horizSpace(patternVar(def.x), elim(def.σ));
    }
    function recDefₜ(def) {
        return Renderer_1.horizSpace(patternVar(def.x), elim(def.tf.v.f));
    }
    function str_(str) {
        return Renderer_1.text(str.toString(), Renderer_1.deltaStyle(str));
    }
})(View = exports.View || (exports.View = {}));


/***/ }),
/* 30 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
__webpack_require__(4); // need these early because of a Webpack dependency problem
__webpack_require__(8);
const Module_1 = __webpack_require__(22);
__webpack_require__(25);
const Pane_1 = __webpack_require__(28);
const PaneCoordinator_1 = __webpack_require__(46);
function initialise() {
    Pane_1.Pane.initialise(".");
    // TODO: eliminate redundancy with "renewables" test
    const appRoot = Core_1.__nonNull(document.getElementById("app-root"));
    const coordinator = new PaneCoordinator_1.PaneCoordinator(appRoot, Module_1.openDatasetAs("renewables-restricted", "data"));
    // const [ρ1, e1]: [Env, Expr] = openWithImports("graphics/grouped-bar-chart")
    const [ρ2, e2] = Module_1.openWithImports("graphics/stacked-bar-chart");
    const [ρ3, e3] = Module_1.openWithImports("graphics/line-chart");
    // Wait for fonts to load before rendering, otherwise metrics will be wrong.
    window.onload = (ev) => {
        //    coordinator.addPane(ρ1, e1)
        coordinator.addPane(ρ2, e2);
        coordinator.addPane(ρ3, e3);
    };
}
exports.initialise = initialise;
initialise();


/***/ }),
/* 31 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
function eq(a, b) {
    return a.leq(b) && b.leq(a);
}
exports.eq = eq;


/***/ }),
/* 32 */
/***/ (function(module, exports, __webpack_require__) {

(function(root, factory) {
    if ( true && module.exports) {
        module.exports = factory();
    } else {
        root.nearley = factory();
    }
}(this, function() {

    function Rule(name, symbols, postprocess) {
        this.id = ++Rule.highestId;
        this.name = name;
        this.symbols = symbols;        // a list of literal | regex class | nonterminal
        this.postprocess = postprocess;
        return this;
    }
    Rule.highestId = 0;

    Rule.prototype.toString = function(withCursorAt) {
        function stringifySymbolSequence (e) {
            return e.literal ? JSON.stringify(e.literal) :
                   e.type ? '%' + e.type : e.toString();
        }
        var symbolSequence = (typeof withCursorAt === "undefined")
                             ? this.symbols.map(stringifySymbolSequence).join(' ')
                             : (   this.symbols.slice(0, withCursorAt).map(stringifySymbolSequence).join(' ')
                                 + " ● "
                                 + this.symbols.slice(withCursorAt).map(stringifySymbolSequence).join(' ')     );
        return this.name + " → " + symbolSequence;
    }


    // a State is a rule at a position from a given starting point in the input stream (reference)
    function State(rule, dot, reference, wantedBy) {
        this.rule = rule;
        this.dot = dot;
        this.reference = reference;
        this.data = [];
        this.wantedBy = wantedBy;
        this.isComplete = this.dot === rule.symbols.length;
    }

    State.prototype.toString = function() {
        return "{" + this.rule.toString(this.dot) + "}, from: " + (this.reference || 0);
    };

    State.prototype.nextState = function(child) {
        var state = new State(this.rule, this.dot + 1, this.reference, this.wantedBy);
        state.left = this;
        state.right = child;
        if (state.isComplete) {
            state.data = state.build();
        }
        return state;
    };

    State.prototype.build = function() {
        var children = [];
        var node = this;
        do {
            children.push(node.right.data);
            node = node.left;
        } while (node.left);
        children.reverse();
        return children;
    };

    State.prototype.finish = function() {
        if (this.rule.postprocess) {
            this.data = this.rule.postprocess(this.data, this.reference, Parser.fail);
        }
    };


    function Column(grammar, index) {
        this.grammar = grammar;
        this.index = index;
        this.states = [];
        this.wants = {}; // states indexed by the non-terminal they expect
        this.scannable = []; // list of states that expect a token
        this.completed = {}; // states that are nullable
    }


    Column.prototype.process = function(nextColumn) {
        var states = this.states;
        var wants = this.wants;
        var completed = this.completed;

        for (var w = 0; w < states.length; w++) { // nb. we push() during iteration
            var state = states[w];

            if (state.isComplete) {
                state.finish();
                if (state.data !== Parser.fail) {
                    // complete
                    var wantedBy = state.wantedBy;
                    for (var i = wantedBy.length; i--; ) { // this line is hot
                        var left = wantedBy[i];
                        this.complete(left, state);
                    }

                    // special-case nullables
                    if (state.reference === this.index) {
                        // make sure future predictors of this rule get completed.
                        var exp = state.rule.name;
                        (this.completed[exp] = this.completed[exp] || []).push(state);
                    }
                }

            } else {
                // queue scannable states
                var exp = state.rule.symbols[state.dot];
                if (typeof exp !== 'string') {
                    this.scannable.push(state);
                    continue;
                }

                // predict
                if (wants[exp]) {
                    wants[exp].push(state);

                    if (completed.hasOwnProperty(exp)) {
                        var nulls = completed[exp];
                        for (var i = 0; i < nulls.length; i++) {
                            var right = nulls[i];
                            this.complete(state, right);
                        }
                    }
                } else {
                    wants[exp] = [state];
                    this.predict(exp);
                }
            }
        }
    }

    Column.prototype.predict = function(exp) {
        var rules = this.grammar.byName[exp] || [];

        for (var i = 0; i < rules.length; i++) {
            var r = rules[i];
            var wantedBy = this.wants[exp];
            var s = new State(r, 0, this.index, wantedBy);
            this.states.push(s);
        }
    }

    Column.prototype.complete = function(left, right) {
        var copy = left.nextState(right);
        this.states.push(copy);
    }


    function Grammar(rules, start) {
        this.rules = rules;
        this.start = start || this.rules[0].name;
        var byName = this.byName = {};
        this.rules.forEach(function(rule) {
            if (!byName.hasOwnProperty(rule.name)) {
                byName[rule.name] = [];
            }
            byName[rule.name].push(rule);
        });
    }

    // So we can allow passing (rules, start) directly to Parser for backwards compatibility
    Grammar.fromCompiled = function(rules, start) {
        var lexer = rules.Lexer;
        if (rules.ParserStart) {
          start = rules.ParserStart;
          rules = rules.ParserRules;
        }
        var rules = rules.map(function (r) { return (new Rule(r.name, r.symbols, r.postprocess)); });
        var g = new Grammar(rules, start);
        g.lexer = lexer; // nb. storing lexer on Grammar is iffy, but unavoidable
        return g;
    }


    function StreamLexer() {
      this.reset("");
    }

    StreamLexer.prototype.reset = function(data, state) {
        this.buffer = data;
        this.index = 0;
        this.line = state ? state.line : 1;
        this.lastLineBreak = state ? -state.col : 0;
    }

    StreamLexer.prototype.next = function() {
        if (this.index < this.buffer.length) {
            var ch = this.buffer[this.index++];
            if (ch === '\n') {
              this.line += 1;
              this.lastLineBreak = this.index;
            }
            return {value: ch};
        }
    }

    StreamLexer.prototype.save = function() {
      return {
        line: this.line,
        col: this.index - this.lastLineBreak,
      }
    }

    StreamLexer.prototype.formatError = function(token, message) {
        // nb. this gets called after consuming the offending token,
        // so the culprit is index-1
        var buffer = this.buffer;
        if (typeof buffer === 'string') {
            var nextLineBreak = buffer.indexOf('\n', this.index);
            if (nextLineBreak === -1) nextLineBreak = buffer.length;
            var line = buffer.substring(this.lastLineBreak, nextLineBreak)
            var col = this.index - this.lastLineBreak;
            message += " at line " + this.line + " col " + col + ":\n\n";
            message += "  " + line + "\n"
            message += "  " + Array(col).join(" ") + "^"
            return message;
        } else {
            return message + " at index " + (this.index - 1);
        }
    }


    function Parser(rules, start, options) {
        if (rules instanceof Grammar) {
            var grammar = rules;
            var options = start;
        } else {
            var grammar = Grammar.fromCompiled(rules, start);
        }
        this.grammar = grammar;

        // Read options
        this.options = {
            keepHistory: false,
            lexer: grammar.lexer || new StreamLexer,
        };
        for (var key in (options || {})) {
            this.options[key] = options[key];
        }

        // Setup lexer
        this.lexer = this.options.lexer;
        this.lexerState = undefined;

        // Setup a table
        var column = new Column(grammar, 0);
        var table = this.table = [column];

        // I could be expecting anything.
        column.wants[grammar.start] = [];
        column.predict(grammar.start);
        // TODO what if start rule is nullable?
        column.process();
        this.current = 0; // token index
    }

    // create a reserved token for indicating a parse fail
    Parser.fail = {};

    Parser.prototype.feed = function(chunk) {
        var lexer = this.lexer;
        lexer.reset(chunk, this.lexerState);

        var token;
        while (token = lexer.next()) {
            // We add new states to table[current+1]
            var column = this.table[this.current];

            // GC unused states
            if (!this.options.keepHistory) {
                delete this.table[this.current - 1];
            }

            var n = this.current + 1;
            var nextColumn = new Column(this.grammar, n);
            this.table.push(nextColumn);

            // Advance all tokens that expect the symbol
            var literal = token.text !== undefined ? token.text : token.value;
            var value = lexer.constructor === StreamLexer ? token.value : token;
            var scannable = column.scannable;
            for (var w = scannable.length; w--; ) {
                var state = scannable[w];
                var expect = state.rule.symbols[state.dot];
                // Try to consume the token
                // either regex or literal
                if (expect.test ? expect.test(value) :
                    expect.type ? expect.type === token.type
                                : expect.literal === literal) {
                    // Add it
                    var next = state.nextState({data: value, token: token, isToken: true, reference: n - 1});
                    nextColumn.states.push(next);
                }
            }

            // Next, for each of the rules, we either
            // (a) complete it, and try to see if the reference row expected that
            //     rule
            // (b) predict the next nonterminal it expects by adding that
            //     nonterminal's start state
            // To prevent duplication, we also keep track of rules we have already
            // added

            nextColumn.process();

            // If needed, throw an error:
            if (nextColumn.states.length === 0) {
                // No states at all! This is not good.
                var err = new Error(this.reportError(token));
                err.offset = this.current;
                err.token = token;
                throw err;
            }

            // maybe save lexer state
            if (this.options.keepHistory) {
              column.lexerState = lexer.save()
            }

            this.current++;
        }
        if (column) {
          this.lexerState = lexer.save()
        }

        // Incrementally keep track of results
        this.results = this.finish();

        // Allow chaining, for whatever it's worth
        return this;
    };

    Parser.prototype.reportError = function(token) {
        var lines = [];
        var tokenDisplay = (token.type ? token.type + " token: " : "") + JSON.stringify(token.value !== undefined ? token.value : token);
        lines.push(this.lexer.formatError(token, "Syntax error"));
        lines.push('Unexpected ' + tokenDisplay + '. Instead, I was expecting to see one of the following:\n');
        var lastColumnIndex = this.table.length - 2;
        var lastColumn = this.table[lastColumnIndex];
        var expectantStates = lastColumn.states
            .filter(function(state) {
                var nextSymbol = state.rule.symbols[state.dot];
                return nextSymbol && typeof nextSymbol !== "string";
            });

        // Display a "state stack" for each expectant state
        // - which shows you how this state came to be, step by step.
        // If there is more than one derivation, we only display the first one.
        var stateStacks = expectantStates
            .map(function(state) {
                return this.buildFirstStateStack(state, []);
            }, this);
        // Display each state that is expecting a terminal symbol next.
        stateStacks.forEach(function(stateStack) {
            var state = stateStack[0];
            var nextSymbol = state.rule.symbols[state.dot];
            var symbolDisplay = this.getSymbolDisplay(nextSymbol);
            lines.push('A ' + symbolDisplay + ' based on:');
            this.displayStateStack(stateStack, lines);
        }, this);

        lines.push("");
        return lines.join("\n");
    };

    Parser.prototype.displayStateStack = function(stateStack, lines) {
        var lastDisplay;
        var sameDisplayCount = 0;
        for (var j = 0; j < stateStack.length; j++) {
            var state = stateStack[j];
            var display = state.rule.toString(state.dot);
            if (display === lastDisplay) {
                sameDisplayCount++;
            } else {
                if (sameDisplayCount > 0) {
                    lines.push('    ⬆ ︎' + sameDisplayCount + ' more lines identical to this');
                }
                sameDisplayCount = 0;
                lines.push('    ' + display);
            }
            lastDisplay = display;
        }
    };

    Parser.prototype.getSymbolDisplay = function(symbol) {
        var type = typeof symbol;
        if (type === "string") {
            return symbol;
        } else if (type === "object" && symbol.literal) {
            return JSON.stringify(symbol.literal);
        } else if (type === "object" && symbol instanceof RegExp) {
            return 'character matching ' + symbol;
        } else if (type === "object" && symbol.type) {
            return symbol.type + ' token';
        } else {
            throw new Error('Unknown symbol type: ' + symbol);
        }
    };

    /*
    Builds a the first state stack. You can think of a state stack as the call stack
    of the recursive-descent parser which the Nearley parse algorithm simulates.
    A state stack is represented as an array of state objects. Within a
    state stack, the first item of the array will be the starting
    state, with each successive item in the array going further back into history.

    This function needs to be given a starting state and an empty array representing
    the visited states, and it returns an single state stack.

    */
    Parser.prototype.buildFirstStateStack = function(state, visited) {
        if (visited.indexOf(state) !== -1) {
            // Found cycle, return null
            // to eliminate this path from the results, because
            // we don't know how to display it meaningfully
            return null;
        }
        if (state.wantedBy.length === 0) {
            return [state];
        }
        var prevState = state.wantedBy[0];
        var childVisited = [state].concat(visited);
        var childResult = this.buildFirstStateStack(prevState, childVisited);
        if (childResult === null) {
            return null;
        }
        return [state].concat(childResult);
    };

    Parser.prototype.save = function() {
        var column = this.table[this.current];
        column.lexerState = this.lexerState;
        return column;
    };

    Parser.prototype.restore = function(column) {
        var index = column.index;
        this.current = index;
        this.table[index] = column;
        this.table.splice(index + 1);
        this.lexerState = column.lexerState;

        // Incrementally keep track of results
        this.results = this.finish();
    };

    // nb. deprecated: use save/restore instead!
    Parser.prototype.rewind = function(index) {
        if (!this.options.keepHistory) {
            throw new Error('set option `keepHistory` to enable rewinding')
        }
        // nb. recall column (table) indicies fall between token indicies.
        //        col 0   --   token 0   --   col 1
        this.restore(this.table[index]);
    };

    Parser.prototype.finish = function() {
        // Return the possible parsings
        var considerations = [];
        var start = this.grammar.start;
        var column = this.table[this.table.length - 1]
        column.states.forEach(function (t) {
            if (t.rule.name === start
                    && t.dot === t.rule.symbols.length
                    && t.reference === 0
                    && t.data !== Parser.fail) {
                considerations.push(t);
            }
        });
        return considerations.map(function(c) {return c.data; });
    };

    return {
        Parser: Parser,
        Grammar: Grammar,
        Rule: Rule,
    };

}));


/***/ }),
/* 33 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
// Generated automatically by nearley, version 2.19.1
// http://github.com/Hardmath123/nearley
// Bypasses TS6133. Allow declared but unused functions.
// @ts-ignore
function id(d) { return d[0]; }
const moo = __webpack_require__(34);
const lexer = moo.compile({
    ident: {
        match: /[a-zA-Z_][0-9a-zA-Z_]*'*/,
        type: moo.keywords({
            keyword: ["_", "as", "match", "fun", "in", "let", "letrec", "primitive", "typematch"],
        })
    },
    whitespace: {
        match: /[ \f\t\r\n]+/,
        lineBreaks: true
    },
    singleLineComment: /\/\/.*$/,
    // JSON grammar for numbers, https://tools.ietf.org/html/rfc7159.html#section-6.
    // Seems Moo requires us to use non-capturing groups (?:)
    number: /\-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?(?:[e|E][-|+]?[0-9]+)?/,
    string: /"(?:\\["\\]|[^\n"\\])*"/,
    // not quite sure why I can't use literals here:
    sumOp: /\-|\+\+|\+/,
    exponentOp: /\*\*/,
    productOp: /\*|\//,
    compareOp: /==|<=|<|>=|>/,
    symbol: ["(", ")", "=", "→", ";", "{", "}", ",", "[", "]", "..."],
});
const Core_1 = __webpack_require__(0);
const BaseTypes_1 = __webpack_require__(4);
const DataType_1 = __webpack_require__(7);
const Expr_1 = __webpack_require__(6);
const FiniteMap_1 = __webpack_require__(23);
const Match_1 = __webpack_require__(9);
const Value_1 = __webpack_require__(1);
const Versioned_1 = __webpack_require__(2);
// Constructors must start with an uppercase letter, a la Haskell. Will fix this as part of issue #49.
function isCtr(str) {
    const ch = str.charAt(0);
    return ch === ch.toUpperCase() && ch !== ch.toLowerCase();
}
function compose(mk_κ1, mk_κ2) {
    return (κ) => mk_κ1(mk_κ2(κ));
}
;
;
;
;
const grammar = {
    Lexer: lexer,
    ParserRules: [
        { "name": "rootExpr", "symbols": ["_", "expr"], "postprocess": ([, e]) => e },
        { "name": "rootExpr", "symbols": ["expr"], "postprocess": id },
        { "name": "_$ebnf$1$subexpression$1", "symbols": [(lexer.has("whitespace") ? { type: "whitespace" } : whitespace)] },
        { "name": "_$ebnf$1$subexpression$1", "symbols": [(lexer.has("singleLineComment") ? { type: "singleLineComment" } : singleLineComment)] },
        { "name": "_$ebnf$1", "symbols": ["_$ebnf$1$subexpression$1"] },
        { "name": "_$ebnf$1$subexpression$2", "symbols": [(lexer.has("whitespace") ? { type: "whitespace" } : whitespace)] },
        { "name": "_$ebnf$1$subexpression$2", "symbols": [(lexer.has("singleLineComment") ? { type: "singleLineComment" } : singleLineComment)] },
        { "name": "_$ebnf$1", "symbols": ["_$ebnf$1", "_$ebnf$1$subexpression$2"], "postprocess": (d) => d[0].concat([d[1]]) },
        { "name": "_", "symbols": ["_$ebnf$1"] },
        { "name": "expr", "symbols": ["compareExpr"], "postprocess": id },
        { "name": "expr", "symbols": ["defs1"], "postprocess": id },
        { "name": "expr", "symbols": ["fun"], "postprocess": id },
        { "name": "expr", "symbols": ["matchAs"], "postprocess": id },
        { "name": "expr", "symbols": ["typematch"], "postprocess": id },
        { "name": "defs1$macrocall$2", "symbols": [{ "literal": "in" }] },
        { "name": "defs1$macrocall$1$macrocall$2", "symbols": ["defs1$macrocall$2"] },
        { "name": "defs1$macrocall$1$macrocall$1", "symbols": ["defs1$macrocall$1$macrocall$2"], "postprocess": id },
        { "name": "defs1$macrocall$1$macrocall$1", "symbols": ["defs1$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "defs1$macrocall$1", "symbols": ["defs1$macrocall$1$macrocall$1"] },
        { "name": "defs1", "symbols": ["defList", "defs1$macrocall$1", "expr"], "postprocess": ([defs, , e]) => Expr_1.Expr.defs(defs, e)(Versioned_1.ν()) },
        { "name": "compareExpr", "symbols": ["compareExpr", "compareOp", "sumExpr"], "postprocess": ([e1, op, e2]) => Expr_1.Expr.binaryApp(e1, Versioned_1.str(op)(Versioned_1.ν()), e2)(Versioned_1.ν()) },
        { "name": "compareExpr", "symbols": ["sumExpr"], "postprocess": id },
        { "name": "sumExpr", "symbols": ["sumExpr", "sumOp", "productExpr"], "postprocess": ([e1, op, e2]) => Expr_1.Expr.binaryApp(e1, Versioned_1.str(op)(Versioned_1.ν()), e2)(Versioned_1.ν()) },
        { "name": "sumExpr", "symbols": ["productExpr"], "postprocess": id },
        { "name": "productExpr", "symbols": ["productExpr", "productOp", "exponentExpr"], "postprocess": ([e1, op, e2]) => Expr_1.Expr.binaryApp(e1, Versioned_1.str(op)(Versioned_1.ν()), e2)(Versioned_1.ν()) },
        { "name": "productExpr", "symbols": ["exponentExpr"], "postprocess": id },
        { "name": "exponentExpr", "symbols": ["exponentExpr", "exponentOp", "appChain"], "postprocess": ([e1, op, e2]) => Expr_1.Expr.binaryApp(e1, Versioned_1.str(op)(Versioned_1.ν()), e2)(Versioned_1.ν()) },
        { "name": "exponentExpr", "symbols": ["appChain"], "postprocess": id },
        { "name": "appChain", "symbols": ["simpleExpr"], "postprocess": id },
        { "name": "appChain", "symbols": ["appChain", "simpleExpr"], "postprocess": ([e1, e2]) => Expr_1.Expr.app(e1, e2)(Versioned_1.ν()) },
        { "name": "simpleExpr", "symbols": ["variable"], "postprocess": id },
        { "name": "simpleExpr", "symbols": ["string"], "postprocess": id },
        { "name": "simpleExpr", "symbols": ["number"], "postprocess": id },
        { "name": "simpleExpr", "symbols": ["parenthExpr"], "postprocess": id },
        { "name": "simpleExpr", "symbols": ["pair"], "postprocess": id },
        { "name": "simpleExpr", "symbols": ["list"], "postprocess": id },
        { "name": "simpleExpr", "symbols": ["constr"], "postprocess": id },
        { "name": "variable", "symbols": ["var"], "postprocess": ([x]) => Expr_1.Expr.var_(x)(Versioned_1.ν()) },
        { "name": "var$macrocall$2", "symbols": [(lexer.has("ident") ? { type: "ident" } : ident)] },
        { "name": "var$macrocall$1", "symbols": ["var$macrocall$2"], "postprocess": id },
        { "name": "var$macrocall$1", "symbols": ["var$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "var", "symbols": ["var$macrocall$1"], "postprocess": ([[x]], _, reject) => {
                if (isCtr(x.value)) {
                    return reject;
                }
                return Versioned_1.str(x.value)(Versioned_1.ν());
            } },
        { "name": "string$macrocall$2", "symbols": [(lexer.has("string") ? { type: "string" } : string)] },
        { "name": "string$macrocall$1", "symbols": ["string$macrocall$2"], "postprocess": id },
        { "name": "string$macrocall$1", "symbols": ["string$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "string", "symbols": ["string$macrocall$1"], "postprocess": ([[lit]]) => Expr_1.Expr.constStr(Versioned_1.str(lit.value.slice(1, -1))(Versioned_1.ν()))(Versioned_1.ν()) },
        { "name": "number$macrocall$2", "symbols": [(lexer.has("number") ? { type: "number" } : number)] },
        { "name": "number$macrocall$1", "symbols": ["number$macrocall$2"], "postprocess": id },
        { "name": "number$macrocall$1", "symbols": ["number$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "number", "symbols": ["number$macrocall$1"], "postprocess": ([[lit]]) => Expr_1.Expr.constNum(Versioned_1.num(new Number(lit.value).valueOf())(Versioned_1.ν()))(Versioned_1.ν()) },
        { "name": "parenthExpr$macrocall$2", "symbols": [{ "literal": "(" }] },
        { "name": "parenthExpr$macrocall$1", "symbols": ["parenthExpr$macrocall$2"], "postprocess": id },
        { "name": "parenthExpr$macrocall$1", "symbols": ["parenthExpr$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "parenthExpr$macrocall$4", "symbols": [{ "literal": ")" }] },
        { "name": "parenthExpr$macrocall$3", "symbols": ["parenthExpr$macrocall$4"], "postprocess": id },
        { "name": "parenthExpr$macrocall$3", "symbols": ["parenthExpr$macrocall$4", "_"], "postprocess": ([x,]) => x },
        { "name": "parenthExpr", "symbols": ["parenthExpr$macrocall$1", "expr", "parenthExpr$macrocall$3"], "postprocess": ([, e,]) => e },
        { "name": "pair$macrocall$2", "symbols": [{ "literal": "(" }] },
        { "name": "pair$macrocall$1", "symbols": ["pair$macrocall$2"], "postprocess": id },
        { "name": "pair$macrocall$1", "symbols": ["pair$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "pair$macrocall$4", "symbols": [{ "literal": "," }] },
        { "name": "pair$macrocall$3", "symbols": ["pair$macrocall$4"], "postprocess": id },
        { "name": "pair$macrocall$3", "symbols": ["pair$macrocall$4", "_"], "postprocess": ([x,]) => x },
        { "name": "pair$macrocall$6", "symbols": [{ "literal": ")" }] },
        { "name": "pair$macrocall$5", "symbols": ["pair$macrocall$6"], "postprocess": id },
        { "name": "pair$macrocall$5", "symbols": ["pair$macrocall$6", "_"], "postprocess": ([x,]) => x },
        { "name": "pair", "symbols": ["pair$macrocall$1", "expr", "pair$macrocall$3", "expr", "pair$macrocall$5"], "postprocess": ([, e1, , e2,]) => Versioned_1.at(DataType_1.exprClass(BaseTypes_1.Pair), e1, e2)(Versioned_1.ν()) },
        { "name": "list$macrocall$2", "symbols": [{ "literal": "[" }] },
        { "name": "list$macrocall$1", "symbols": ["list$macrocall$2"], "postprocess": id },
        { "name": "list$macrocall$1", "symbols": ["list$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "list$macrocall$4", "symbols": [{ "literal": "]" }] },
        { "name": "list$macrocall$3", "symbols": ["list$macrocall$4"], "postprocess": id },
        { "name": "list$macrocall$3", "symbols": ["list$macrocall$4", "_"], "postprocess": ([x,]) => x },
        { "name": "list", "symbols": ["list$macrocall$1", "listOpt", "list$macrocall$3"], "postprocess": ([, e,]) => e },
        { "name": "constr", "symbols": ["ctr", "args"], "postprocess": ([c, e̅], _, reject) => {
                Core_1.assert(c instanceof Value_1.Str);
                const ctr = DataType_1.ctrFor(c.val);
                if (ctr.arity !== e̅.length) {
                    return reject;
                }
                return Versioned_1.at(DataType_1.exprClass(ctr.C), ...e̅)(Versioned_1.ν());
            } },
        { "name": "ctr$macrocall$2", "symbols": [(lexer.has("ident") ? { type: "ident" } : ident)] },
        { "name": "ctr$macrocall$1", "symbols": ["ctr$macrocall$2"], "postprocess": id },
        { "name": "ctr$macrocall$1", "symbols": ["ctr$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "ctr", "symbols": ["ctr$macrocall$1"], "postprocess": ([[x]], _, reject) => {
                if (!isCtr(x.value)) {
                    return reject;
                }
                return Versioned_1.str(x.value)(Versioned_1.ν());
            } },
        { "name": "args", "symbols": [], "postprocess": () => [] },
        { "name": "args$macrocall$2", "symbols": [{ "literal": "(" }] },
        { "name": "args$macrocall$1", "symbols": ["args$macrocall$2"], "postprocess": id },
        { "name": "args$macrocall$1", "symbols": ["args$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "args$ebnf$1", "symbols": [] },
        { "name": "args$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": "," }] },
        { "name": "args$ebnf$1$subexpression$1$macrocall$1", "symbols": ["args$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
        { "name": "args$ebnf$1$subexpression$1$macrocall$1", "symbols": ["args$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "args$ebnf$1$subexpression$1", "symbols": ["args$ebnf$1$subexpression$1$macrocall$1", "expr"], "postprocess": ([, e]) => e },
        { "name": "args$ebnf$1", "symbols": ["args$ebnf$1", "args$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
        { "name": "args$macrocall$4", "symbols": [{ "literal": ")" }] },
        { "name": "args$macrocall$3", "symbols": ["args$macrocall$4"], "postprocess": id },
        { "name": "args$macrocall$3", "symbols": ["args$macrocall$4", "_"], "postprocess": ([x,]) => x },
        { "name": "args", "symbols": ["args$macrocall$1", "expr", "args$ebnf$1", "args$macrocall$3"], "postprocess": ([, e, es,]) => [e, ...es] },
        { "name": "typematch$macrocall$2", "symbols": [{ "literal": "typematch" }] },
        { "name": "typematch$macrocall$1$macrocall$2", "symbols": ["typematch$macrocall$2"] },
        { "name": "typematch$macrocall$1$macrocall$1", "symbols": ["typematch$macrocall$1$macrocall$2"], "postprocess": id },
        { "name": "typematch$macrocall$1$macrocall$1", "symbols": ["typematch$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "typematch$macrocall$1", "symbols": ["typematch$macrocall$1$macrocall$1"] },
        { "name": "typematch$macrocall$4", "symbols": [{ "literal": "as" }] },
        { "name": "typematch$macrocall$3$macrocall$2", "symbols": ["typematch$macrocall$4"] },
        { "name": "typematch$macrocall$3$macrocall$1", "symbols": ["typematch$macrocall$3$macrocall$2"], "postprocess": id },
        { "name": "typematch$macrocall$3$macrocall$1", "symbols": ["typematch$macrocall$3$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "typematch$macrocall$3", "symbols": ["typematch$macrocall$3$macrocall$1"] },
        { "name": "typematch", "symbols": ["typematch$macrocall$1", "expr", "typematch$macrocall$3", "typeMatches"], "postprocess": ([, e, , m]) => Expr_1.Expr.typematch(e, m)(Versioned_1.ν()) },
        { "name": "defList$ebnf$1", "symbols": [] },
        { "name": "defList$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": ";" }] },
        { "name": "defList$ebnf$1$subexpression$1$macrocall$1", "symbols": ["defList$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
        { "name": "defList$ebnf$1$subexpression$1$macrocall$1", "symbols": ["defList$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "defList$ebnf$1$subexpression$1", "symbols": ["defList$ebnf$1$subexpression$1$macrocall$1", "def"], "postprocess": ([, def]) => def },
        { "name": "defList$ebnf$1", "symbols": ["defList$ebnf$1", "defList$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
        { "name": "defList", "symbols": ["def", "defList$ebnf$1"], "postprocess": ([def, defs]) => BaseTypes_1.List.fromArray([def, ...defs]) },
        { "name": "def", "symbols": ["let"], "postprocess": id },
        { "name": "def", "symbols": ["letrec"], "postprocess": id },
        { "name": "def", "symbols": ["prim"], "postprocess": id },
        { "name": "let$macrocall$2", "symbols": [{ "literal": "let" }] },
        { "name": "let$macrocall$1$macrocall$2", "symbols": ["let$macrocall$2"] },
        { "name": "let$macrocall$1$macrocall$1", "symbols": ["let$macrocall$1$macrocall$2"], "postprocess": id },
        { "name": "let$macrocall$1$macrocall$1", "symbols": ["let$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "let$macrocall$1", "symbols": ["let$macrocall$1$macrocall$1"] },
        { "name": "let$macrocall$4", "symbols": [{ "literal": "=" }] },
        { "name": "let$macrocall$3", "symbols": ["let$macrocall$4"], "postprocess": id },
        { "name": "let$macrocall$3", "symbols": ["let$macrocall$4", "_"], "postprocess": ([x,]) => x },
        { "name": "let", "symbols": ["let$macrocall$1", "var", "let$macrocall$3", "expr"], "postprocess": ([, x, , e]) => Expr_1.Expr.let_(x, e)(Versioned_1.ν()) },
        { "name": "letrec$macrocall$2", "symbols": [{ "literal": "letrec" }] },
        { "name": "letrec$macrocall$1$macrocall$2", "symbols": ["letrec$macrocall$2"] },
        { "name": "letrec$macrocall$1$macrocall$1", "symbols": ["letrec$macrocall$1$macrocall$2"], "postprocess": id },
        { "name": "letrec$macrocall$1$macrocall$1", "symbols": ["letrec$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "letrec$macrocall$1", "symbols": ["letrec$macrocall$1$macrocall$1"] },
        { "name": "letrec$ebnf$1", "symbols": [] },
        { "name": "letrec$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": ";" }] },
        { "name": "letrec$ebnf$1$subexpression$1$macrocall$1", "symbols": ["letrec$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
        { "name": "letrec$ebnf$1$subexpression$1$macrocall$1", "symbols": ["letrec$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "letrec$ebnf$1$subexpression$1", "symbols": ["letrec$ebnf$1$subexpression$1$macrocall$1", "recDef"], "postprocess": ([, recDef]) => recDef },
        { "name": "letrec$ebnf$1", "symbols": ["letrec$ebnf$1", "letrec$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
        { "name": "letrec", "symbols": ["letrec$macrocall$1", "recDef", "letrec$ebnf$1"], "postprocess": ([, recDef, δ]) => Expr_1.Expr.letRec(BaseTypes_1.List.fromArray([recDef, ...δ]))(Versioned_1.ν()) },
        { "name": "prim$macrocall$2", "symbols": [{ "literal": "primitive" }] },
        { "name": "prim$macrocall$1$macrocall$2", "symbols": ["prim$macrocall$2"] },
        { "name": "prim$macrocall$1$macrocall$1", "symbols": ["prim$macrocall$1$macrocall$2"], "postprocess": id },
        { "name": "prim$macrocall$1$macrocall$1", "symbols": ["prim$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "prim$macrocall$1", "symbols": ["prim$macrocall$1$macrocall$1"] },
        { "name": "prim", "symbols": ["prim$macrocall$1", "var"], "postprocess": ([, x]) => Expr_1.Expr.prim(x)(Versioned_1.ν()) },
        { "name": "recDef$macrocall$2", "symbols": [{ "literal": "fun" }] },
        { "name": "recDef$macrocall$1$macrocall$2", "symbols": ["recDef$macrocall$2"] },
        { "name": "recDef$macrocall$1$macrocall$1", "symbols": ["recDef$macrocall$1$macrocall$2"], "postprocess": id },
        { "name": "recDef$macrocall$1$macrocall$1", "symbols": ["recDef$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "recDef$macrocall$1", "symbols": ["recDef$macrocall$1$macrocall$1"] },
        { "name": "recDef", "symbols": ["recDef$macrocall$1", "var", "matches"], "postprocess": ([, f, σ]) => Expr_1.Expr.recDef(f, σ)(Versioned_1.ν()) },
        { "name": "fun$macrocall$2", "symbols": [{ "literal": "fun" }] },
        { "name": "fun$macrocall$1$macrocall$2", "symbols": ["fun$macrocall$2"] },
        { "name": "fun$macrocall$1$macrocall$1", "symbols": ["fun$macrocall$1$macrocall$2"], "postprocess": id },
        { "name": "fun$macrocall$1$macrocall$1", "symbols": ["fun$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "fun$macrocall$1", "symbols": ["fun$macrocall$1$macrocall$1"] },
        { "name": "fun", "symbols": ["fun$macrocall$1", "matches"], "postprocess": ([, σ]) => Expr_1.Expr.fun(σ)(Versioned_1.ν()) },
        { "name": "matchAs$macrocall$2", "symbols": [{ "literal": "match" }] },
        { "name": "matchAs$macrocall$1$macrocall$2", "symbols": ["matchAs$macrocall$2"] },
        { "name": "matchAs$macrocall$1$macrocall$1", "symbols": ["matchAs$macrocall$1$macrocall$2"], "postprocess": id },
        { "name": "matchAs$macrocall$1$macrocall$1", "symbols": ["matchAs$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "matchAs$macrocall$1", "symbols": ["matchAs$macrocall$1$macrocall$1"] },
        { "name": "matchAs$macrocall$4", "symbols": [{ "literal": "as" }] },
        { "name": "matchAs$macrocall$3$macrocall$2", "symbols": ["matchAs$macrocall$4"] },
        { "name": "matchAs$macrocall$3$macrocall$1", "symbols": ["matchAs$macrocall$3$macrocall$2"], "postprocess": id },
        { "name": "matchAs$macrocall$3$macrocall$1", "symbols": ["matchAs$macrocall$3$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "matchAs$macrocall$3", "symbols": ["matchAs$macrocall$3$macrocall$1"] },
        { "name": "matchAs", "symbols": ["matchAs$macrocall$1", "expr", "matchAs$macrocall$3", "matches"], "postprocess": ([, e, , σ]) => Expr_1.Expr.matchAs(e, σ)(Versioned_1.ν()) },
        { "name": "matches", "symbols": ["match"], "postprocess": id },
        { "name": "matches$macrocall$2", "symbols": [{ "literal": "{" }] },
        { "name": "matches$macrocall$1", "symbols": ["matches$macrocall$2"], "postprocess": id },
        { "name": "matches$macrocall$1", "symbols": ["matches$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "matches$ebnf$1", "symbols": [] },
        { "name": "matches$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": ";" }] },
        { "name": "matches$ebnf$1$subexpression$1$macrocall$1", "symbols": ["matches$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
        { "name": "matches$ebnf$1$subexpression$1$macrocall$1", "symbols": ["matches$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "matches$ebnf$1$subexpression$1", "symbols": ["matches$ebnf$1$subexpression$1$macrocall$1", "match"], "postprocess": ([, m]) => m },
        { "name": "matches$ebnf$1", "symbols": ["matches$ebnf$1", "matches$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
        { "name": "matches$macrocall$4", "symbols": [{ "literal": "}" }] },
        { "name": "matches$macrocall$3", "symbols": ["matches$macrocall$4"], "postprocess": id },
        { "name": "matches$macrocall$3", "symbols": ["matches$macrocall$4", "_"], "postprocess": ([x,]) => x },
        { "name": "matches", "symbols": ["matches$macrocall$1", "match", "matches$ebnf$1", "matches$macrocall$3"], "postprocess": ([, m, ms,]) => [m, ...ms].reduce(Match_1.DataElim.join) },
        { "name": "match$macrocall$2", "symbols": [{ "literal": "→" }] },
        { "name": "match$macrocall$1", "symbols": ["match$macrocall$2"], "postprocess": id },
        { "name": "match$macrocall$1", "symbols": ["match$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "match", "symbols": ["pattern", "match$macrocall$1", "expr"], "postprocess": ([mk_κ, , e]) => mk_κ(e) },
        { "name": "match", "symbols": ["pattern", "matches"], "postprocess": ([mk_κ1, σ]) => mk_κ1(Expr_1.Expr.fun(σ)(Versioned_1.ν())) },
        { "name": "typeMatches", "symbols": ["typeMatch"], "postprocess": id },
        { "name": "typeMatches$macrocall$2", "symbols": [{ "literal": "{" }] },
        { "name": "typeMatches$macrocall$1", "symbols": ["typeMatches$macrocall$2"], "postprocess": id },
        { "name": "typeMatches$macrocall$1", "symbols": ["typeMatches$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "typeMatches$ebnf$1", "symbols": [] },
        { "name": "typeMatches$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": ";" }] },
        { "name": "typeMatches$ebnf$1$subexpression$1$macrocall$1", "symbols": ["typeMatches$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
        { "name": "typeMatches$ebnf$1$subexpression$1$macrocall$1", "symbols": ["typeMatches$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "typeMatches$ebnf$1$subexpression$1", "symbols": ["typeMatches$ebnf$1$subexpression$1$macrocall$1", "typeMatch"], "postprocess": ([, m]) => m },
        { "name": "typeMatches$ebnf$1", "symbols": ["typeMatches$ebnf$1", "typeMatches$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
        { "name": "typeMatches$macrocall$4", "symbols": [{ "literal": "}" }] },
        { "name": "typeMatches$macrocall$3", "symbols": ["typeMatches$macrocall$4"], "postprocess": id },
        { "name": "typeMatches$macrocall$3", "symbols": ["typeMatches$macrocall$4", "_"], "postprocess": ([x,]) => x },
        { "name": "typeMatches", "symbols": ["typeMatches$macrocall$1", "typeMatch", "typeMatches$ebnf$1", "typeMatches$macrocall$3"], "postprocess": ([, m, ms,]) => [m, ...ms].reduce((m1, m2) => FiniteMap_1.unionWith(m1, m2, (e, eʹ) => Core_1.userError("Overlapping typecase branches."))) },
        { "name": "typeMatch$macrocall$2", "symbols": [{ "literal": "→" }] },
        { "name": "typeMatch$macrocall$1", "symbols": ["typeMatch$macrocall$2"], "postprocess": id },
        { "name": "typeMatch$macrocall$1", "symbols": ["typeMatch$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "typeMatch", "symbols": ["typename", "typeMatch$macrocall$1", "expr"], "postprocess": ([x, , e]) => {
                Core_1.assert(x instanceof Value_1.Str);
                if (!DataType_1.types.has(x.val)) {
                    Core_1.userError(`Type name ${x.val} not found.`);
                }
                return FiniteMap_1.singleton(x, e);
            } },
        { "name": "typename$macrocall$2", "symbols": [(lexer.has("ident") ? { type: "ident" } : ident)] },
        { "name": "typename$macrocall$1", "symbols": ["typename$macrocall$2"], "postprocess": id },
        { "name": "typename$macrocall$1", "symbols": ["typename$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "typename", "symbols": ["typename$macrocall$1"], "postprocess": ([[x]]) => Versioned_1.str(x.value)(Versioned_1.ν()) },
        { "name": "listOpt", "symbols": [], "postprocess": () => Versioned_1.at(DataType_1.exprClass(BaseTypes_1.Nil))(Versioned_1.ν()) },
        { "name": "listOpt$ebnf$1", "symbols": [] },
        { "name": "listOpt$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": "," }] },
        { "name": "listOpt$ebnf$1$subexpression$1$macrocall$1", "symbols": ["listOpt$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
        { "name": "listOpt$ebnf$1$subexpression$1$macrocall$1", "symbols": ["listOpt$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "listOpt$ebnf$1$subexpression$1", "symbols": ["listOpt$ebnf$1$subexpression$1$macrocall$1", "expr"], "postprocess": ([, e]) => e },
        { "name": "listOpt$ebnf$1", "symbols": ["listOpt$ebnf$1", "listOpt$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
        { "name": "listOpt", "symbols": ["expr", "listOpt$ebnf$1", "listRestOpt"], "postprocess": ([e, es, eʹ]) => [e, ...es, eʹ].reverse().reduce((e̅, e) => Versioned_1.at(DataType_1.exprClass(BaseTypes_1.Cons), e, e̅)(Versioned_1.ν())) },
        { "name": "listRestOpt", "symbols": [], "postprocess": () => Versioned_1.at(DataType_1.exprClass(BaseTypes_1.Nil))(Versioned_1.ν()) },
        { "name": "listRestOpt$macrocall$2", "symbols": [{ "literal": "," }] },
        { "name": "listRestOpt$macrocall$1", "symbols": ["listRestOpt$macrocall$2"], "postprocess": id },
        { "name": "listRestOpt$macrocall$1", "symbols": ["listRestOpt$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "listRestOpt$macrocall$4", "symbols": [{ "literal": "..." }] },
        { "name": "listRestOpt$macrocall$3", "symbols": ["listRestOpt$macrocall$4"], "postprocess": id },
        { "name": "listRestOpt$macrocall$3", "symbols": ["listRestOpt$macrocall$4", "_"], "postprocess": ([x,]) => x },
        { "name": "listRestOpt", "symbols": ["listRestOpt$macrocall$1", "listRestOpt$macrocall$3", "expr"], "postprocess": ([, , e]) => e },
        { "name": "pattern", "symbols": ["variable_pattern"], "postprocess": id },
        { "name": "pattern", "symbols": ["pair_pattern"], "postprocess": id },
        { "name": "pattern", "symbols": ["list_pattern"], "postprocess": id },
        { "name": "pattern", "symbols": ["constr_pattern"], "postprocess": id },
        { "name": "variable_pattern$macrocall$2", "symbols": [{ "literal": "_" }] },
        { "name": "variable_pattern$macrocall$1$macrocall$2", "symbols": ["variable_pattern$macrocall$2"] },
        { "name": "variable_pattern$macrocall$1$macrocall$1", "symbols": ["variable_pattern$macrocall$1$macrocall$2"], "postprocess": id },
        { "name": "variable_pattern$macrocall$1$macrocall$1", "symbols": ["variable_pattern$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "variable_pattern$macrocall$1", "symbols": ["variable_pattern$macrocall$1$macrocall$1"] },
        { "name": "variable_pattern", "symbols": ["variable_pattern$macrocall$1"], "postprocess": () => (κ) => Match_1.varElim(Versioned_1.str("_")(Versioned_1.ν()), κ)(Versioned_1.ν()) },
        { "name": "variable_pattern", "symbols": ["var"], "postprocess": ([x]) => (κ) => Match_1.varElim(x, κ)(Versioned_1.ν()) },
        { "name": "pair_pattern$macrocall$2", "symbols": [{ "literal": "(" }] },
        { "name": "pair_pattern$macrocall$1", "symbols": ["pair_pattern$macrocall$2"], "postprocess": id },
        { "name": "pair_pattern$macrocall$1", "symbols": ["pair_pattern$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "pair_pattern$macrocall$4", "symbols": [{ "literal": "," }] },
        { "name": "pair_pattern$macrocall$3", "symbols": ["pair_pattern$macrocall$4"], "postprocess": id },
        { "name": "pair_pattern$macrocall$3", "symbols": ["pair_pattern$macrocall$4", "_"], "postprocess": ([x,]) => x },
        { "name": "pair_pattern$macrocall$6", "symbols": [{ "literal": ")" }] },
        { "name": "pair_pattern$macrocall$5", "symbols": ["pair_pattern$macrocall$6"], "postprocess": id },
        { "name": "pair_pattern$macrocall$5", "symbols": ["pair_pattern$macrocall$6", "_"], "postprocess": ([x,]) => x },
        { "name": "pair_pattern", "symbols": ["pair_pattern$macrocall$1", "pattern", "pair_pattern$macrocall$3", "pattern", "pair_pattern$macrocall$5"], "postprocess": ([, mk_κ1, , mk_κ2, ,]) => (κ) => Match_1.dataElim([BaseTypes_1.Pair.name, compose(mk_κ1, mk_κ2)(κ)])(Versioned_1.ν()) },
        { "name": "list_pattern$macrocall$2", "symbols": [{ "literal": "[" }] },
        { "name": "list_pattern$macrocall$1", "symbols": ["list_pattern$macrocall$2"], "postprocess": id },
        { "name": "list_pattern$macrocall$1", "symbols": ["list_pattern$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "list_pattern$macrocall$4", "symbols": [{ "literal": "]" }] },
        { "name": "list_pattern$macrocall$3", "symbols": ["list_pattern$macrocall$4"], "postprocess": id },
        { "name": "list_pattern$macrocall$3", "symbols": ["list_pattern$macrocall$4", "_"], "postprocess": ([x,]) => x },
        { "name": "list_pattern", "symbols": ["list_pattern$macrocall$1", "listOpt_pattern", "list_pattern$macrocall$3"], "postprocess": ([, mk_κ,]) => mk_κ },
        { "name": "listOpt_pattern", "symbols": [], "postprocess": () => (κ) => Match_1.dataElim([BaseTypes_1.Nil.name, κ])(Versioned_1.ν()) },
        { "name": "listOpt_pattern", "symbols": ["list1_pattern"], "postprocess": id },
        { "name": "list1_pattern", "symbols": ["pattern", "listRestOpt_pattern"], "postprocess": ([mk_κ1, mk_κ2]) => (κ) => Match_1.dataElim([BaseTypes_1.Cons.name, compose(mk_κ1, mk_κ2)(κ)])(Versioned_1.ν()) },
        { "name": "listRestOpt_pattern", "symbols": [], "postprocess": () => (κ) => Match_1.dataElim([BaseTypes_1.Nil.name, κ])(Versioned_1.ν()) },
        { "name": "listRestOpt_pattern$macrocall$2", "symbols": [{ "literal": "," }] },
        { "name": "listRestOpt_pattern$macrocall$1", "symbols": ["listRestOpt_pattern$macrocall$2"], "postprocess": id },
        { "name": "listRestOpt_pattern$macrocall$1", "symbols": ["listRestOpt_pattern$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "listRestOpt_pattern$macrocall$4", "symbols": [{ "literal": "..." }] },
        { "name": "listRestOpt_pattern$macrocall$3", "symbols": ["listRestOpt_pattern$macrocall$4"], "postprocess": id },
        { "name": "listRestOpt_pattern$macrocall$3", "symbols": ["listRestOpt_pattern$macrocall$4", "_"], "postprocess": ([x,]) => x },
        { "name": "listRestOpt_pattern", "symbols": ["listRestOpt_pattern$macrocall$1", "listRestOpt_pattern$macrocall$3", "pattern"], "postprocess": ([, , mk_κ]) => mk_κ },
        { "name": "listRestOpt_pattern$macrocall$6", "symbols": [{ "literal": "," }] },
        { "name": "listRestOpt_pattern$macrocall$5", "symbols": ["listRestOpt_pattern$macrocall$6"], "postprocess": id },
        { "name": "listRestOpt_pattern$macrocall$5", "symbols": ["listRestOpt_pattern$macrocall$6", "_"], "postprocess": ([x,]) => x },
        { "name": "listRestOpt_pattern", "symbols": ["listRestOpt_pattern$macrocall$5", "list1_pattern"], "postprocess": ([, mk_κ]) => mk_κ },
        { "name": "constr_pattern", "symbols": ["ctr", "args_pattern"], "postprocess": ([c, mk_κs], _, reject) => {
                Core_1.assert(c instanceof Value_1.Str);
                if (DataType_1.ctrFor(c.val).arity !== mk_κs.length) {
                    return reject;
                }
                return (κ) => Match_1.dataElim([c.val, mk_κs.reduce(compose, (κ) => κ)(κ)])(Versioned_1.ν());
            } },
        { "name": "args_pattern", "symbols": [], "postprocess": () => [] },
        { "name": "args_pattern$macrocall$2", "symbols": [{ "literal": "(" }] },
        { "name": "args_pattern$macrocall$1", "symbols": ["args_pattern$macrocall$2"], "postprocess": id },
        { "name": "args_pattern$macrocall$1", "symbols": ["args_pattern$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "args_pattern$ebnf$1", "symbols": [] },
        { "name": "args_pattern$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": "," }] },
        { "name": "args_pattern$ebnf$1$subexpression$1$macrocall$1", "symbols": ["args_pattern$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
        { "name": "args_pattern$ebnf$1$subexpression$1$macrocall$1", "symbols": ["args_pattern$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "args_pattern$ebnf$1$subexpression$1", "symbols": ["args_pattern$ebnf$1$subexpression$1$macrocall$1", "pattern"], "postprocess": ([, mk_κ]) => mk_κ },
        { "name": "args_pattern$ebnf$1", "symbols": ["args_pattern$ebnf$1", "args_pattern$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
        { "name": "args_pattern$macrocall$4", "symbols": [{ "literal": ")" }] },
        { "name": "args_pattern$macrocall$3", "symbols": ["args_pattern$macrocall$4"], "postprocess": id },
        { "name": "args_pattern$macrocall$3", "symbols": ["args_pattern$macrocall$4", "_"], "postprocess": ([x,]) => x },
        { "name": "args_pattern", "symbols": ["args_pattern$macrocall$1", "pattern", "args_pattern$ebnf$1", "args_pattern$macrocall$3"], "postprocess": ([, mk_κ, mk_κs,]) => [mk_κ, ...mk_κs] },
        { "name": "compareOp$macrocall$2", "symbols": [(lexer.has("compareOp") ? { type: "compareOp" } : compareOp)] },
        { "name": "compareOp$macrocall$1", "symbols": ["compareOp$macrocall$2"], "postprocess": id },
        { "name": "compareOp$macrocall$1", "symbols": ["compareOp$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "compareOp", "symbols": ["compareOp$macrocall$1"], "postprocess": ([[x]]) => x.value },
        { "name": "exponentOp$macrocall$2", "symbols": [(lexer.has("exponentOp") ? { type: "exponentOp" } : exponentOp)] },
        { "name": "exponentOp$macrocall$1", "symbols": ["exponentOp$macrocall$2"], "postprocess": id },
        { "name": "exponentOp$macrocall$1", "symbols": ["exponentOp$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "exponentOp", "symbols": ["exponentOp$macrocall$1"], "postprocess": ([[x]]) => x.value },
        { "name": "productOp$macrocall$2", "symbols": [(lexer.has("productOp") ? { type: "productOp" } : productOp)] },
        { "name": "productOp$macrocall$1", "symbols": ["productOp$macrocall$2"], "postprocess": id },
        { "name": "productOp$macrocall$1", "symbols": ["productOp$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "productOp", "symbols": ["productOp$macrocall$1"], "postprocess": ([[x]]) => x.value },
        { "name": "sumOp$macrocall$2", "symbols": [(lexer.has("sumOp") ? { type: "sumOp" } : sumOp)] },
        { "name": "sumOp$macrocall$1", "symbols": ["sumOp$macrocall$2"], "postprocess": id },
        { "name": "sumOp$macrocall$1", "symbols": ["sumOp$macrocall$2", "_"], "postprocess": ([x,]) => x },
        { "name": "sumOp", "symbols": ["sumOp$macrocall$1"], "postprocess": ([[x]]) => x.value }
    ],
    ParserStart: "rootExpr",
};
exports.default = grammar;


/***/ }),
/* 34 */
/***/ (function(module, exports, __webpack_require__) {

var __WEBPACK_AMD_DEFINE_FACTORY__, __WEBPACK_AMD_DEFINE_ARRAY__, __WEBPACK_AMD_DEFINE_RESULT__;(function(root, factory) {
  if (true) {
    !(__WEBPACK_AMD_DEFINE_ARRAY__ = [], __WEBPACK_AMD_DEFINE_FACTORY__ = (factory),
				__WEBPACK_AMD_DEFINE_RESULT__ = (typeof __WEBPACK_AMD_DEFINE_FACTORY__ === 'function' ?
				(__WEBPACK_AMD_DEFINE_FACTORY__.apply(exports, __WEBPACK_AMD_DEFINE_ARRAY__)) : __WEBPACK_AMD_DEFINE_FACTORY__),
				__WEBPACK_AMD_DEFINE_RESULT__ !== undefined && (module.exports = __WEBPACK_AMD_DEFINE_RESULT__)) /* global define */
  } else {}
}(this, function() {
  'use strict';

  var hasOwnProperty = Object.prototype.hasOwnProperty
  var toString = Object.prototype.toString
  var hasSticky = typeof new RegExp().sticky === 'boolean'

  /***************************************************************************/

  function isRegExp(o) { return o && toString.call(o) === '[object RegExp]' }
  function isObject(o) { return o && typeof o === 'object' && !isRegExp(o) && !Array.isArray(o) }

  function reEscape(s) {
    return s.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&')
  }
  function reGroups(s) {
    var re = new RegExp('|' + s)
    return re.exec('').length - 1
  }
  function reCapture(s) {
    return '(' + s + ')'
  }
  function reUnion(regexps) {
    if (!regexps.length) return '(?!)'
    var source =  regexps.map(function(s) {
      return "(?:" + s + ")"
    }).join('|')
    return "(?:" + source + ")"
  }

  function regexpOrLiteral(obj) {
    if (typeof obj === 'string') {
      return '(?:' + reEscape(obj) + ')'

    } else if (isRegExp(obj)) {
      // TODO: consider /u support
      if (obj.ignoreCase) throw new Error('RegExp /i flag not allowed')
      if (obj.global) throw new Error('RegExp /g flag is implied')
      if (obj.sticky) throw new Error('RegExp /y flag is implied')
      if (obj.multiline) throw new Error('RegExp /m flag is implied')
      return obj.source

    } else {
      throw new Error('Not a pattern: ' + obj)
    }
  }

  function objectToRules(object) {
    var keys = Object.getOwnPropertyNames(object)
    var result = []
    for (var i = 0; i < keys.length; i++) {
      var key = keys[i]
      var thing = object[key]
      var rules = [].concat(thing)
      if (key === 'include') {
        for (var j = 0; j < rules.length; j++) {
          result.push({include: rules[j]})
        }
        continue
      }
      var match = []
      rules.forEach(function(rule) {
        if (isObject(rule)) {
          if (match.length) result.push(ruleOptions(key, match))
          result.push(ruleOptions(key, rule))
          match = []
        } else {
          match.push(rule)
        }
      })
      if (match.length) result.push(ruleOptions(key, match))
    }
    return result
  }

  function arrayToRules(array) {
    var result = []
    for (var i = 0; i < array.length; i++) {
      var obj = array[i]
      if (obj.include) {
        var include = [].concat(obj.include)
        for (var j = 0; j < include.length; j++) {
          result.push({include: include[j]})
        }
        continue
      }
      if (!obj.type) {
        throw new Error('Rule has no type: ' + JSON.stringify(obj))
      }
      result.push(ruleOptions(obj.type, obj))
    }
    return result
  }

  function ruleOptions(type, obj) {
    if (!isObject(obj)) {
      obj = { match: obj }
    }
    if (obj.include) {
      throw new Error('Matching rules cannot also include states')
    }

    // nb. error and fallback imply lineBreaks
    var options = {
      defaultType: type,
      lineBreaks: !!obj.error || !!obj.fallback,
      pop: false,
      next: null,
      push: null,
      error: false,
      fallback: false,
      value: null,
      type: null,
      shouldThrow: false,
    }

    // Avoid Object.assign(), so we support IE9+
    for (var key in obj) {
      if (hasOwnProperty.call(obj, key)) {
        options[key] = obj[key]
      }
    }

    // type transform cannot be a string
    if (typeof options.type === 'string' && type !== options.type) {
      throw new Error("Type transform cannot be a string (type '" + options.type + "' for token '" + type + "')")
    }

    // convert to array
    var match = options.match
    options.match = Array.isArray(match) ? match : match ? [match] : []
    options.match.sort(function(a, b) {
      return isRegExp(a) && isRegExp(b) ? 0
           : isRegExp(b) ? -1 : isRegExp(a) ? +1 : b.length - a.length
    })
    return options
  }

  function toRules(spec) {
    return Array.isArray(spec) ? arrayToRules(spec) : objectToRules(spec)
  }

  var defaultErrorRule = ruleOptions('error', {lineBreaks: true, shouldThrow: true})
  function compileRules(rules, hasStates) {
    var errorRule = null
    var fast = Object.create(null)
    var fastAllowed = true
    var unicodeFlag = null
    var groups = []
    var parts = []

    // If there is a fallback rule, then disable fast matching
    for (var i = 0; i < rules.length; i++) {
      if (rules[i].fallback) {
        fastAllowed = false
      }
    }

    for (var i = 0; i < rules.length; i++) {
      var options = rules[i]

      if (options.include) {
        // all valid inclusions are removed by states() preprocessor
        throw new Error('Inheritance is not allowed in stateless lexers')
      }

      if (options.error || options.fallback) {
        // errorRule can only be set once
        if (errorRule) {
          if (!options.fallback === !errorRule.fallback) {
            throw new Error("Multiple " + (options.fallback ? "fallback" : "error") + " rules not allowed (for token '" + options.defaultType + "')")
          } else {
            throw new Error("fallback and error are mutually exclusive (for token '" + options.defaultType + "')")
          }
        }
        errorRule = options
      }

      var match = options.match.slice()
      if (fastAllowed) {
        while (match.length && typeof match[0] === 'string' && match[0].length === 1) {
          var word = match.shift()
          fast[word.charCodeAt(0)] = options
        }
      }

      // Warn about inappropriate state-switching options
      if (options.pop || options.push || options.next) {
        if (!hasStates) {
          throw new Error("State-switching options are not allowed in stateless lexers (for token '" + options.defaultType + "')")
        }
        if (options.fallback) {
          throw new Error("State-switching options are not allowed on fallback tokens (for token '" + options.defaultType + "')")
        }
      }

      // Only rules with a .match are included in the RegExp
      if (match.length === 0) {
        continue
      }
      fastAllowed = false

      groups.push(options)

      // Check unicode flag is used everywhere or nowhere
      for (var j = 0; j < match.length; j++) {
        var obj = match[j]
        if (!isRegExp(obj)) {
          continue
        }

        if (unicodeFlag === null) {
          unicodeFlag = obj.unicode
        } else if (unicodeFlag !== obj.unicode && options.fallback === false) {
          throw new Error('If one rule is /u then all must be')
        }
      }

      // convert to RegExp
      var pat = reUnion(match.map(regexpOrLiteral))

      // validate
      var regexp = new RegExp(pat)
      if (regexp.test("")) {
        throw new Error("RegExp matches empty string: " + regexp)
      }
      var groupCount = reGroups(pat)
      if (groupCount > 0) {
        throw new Error("RegExp has capture groups: " + regexp + "\nUse (?: … ) instead")
      }

      // try and detect rules matching newlines
      if (!options.lineBreaks && regexp.test('\n')) {
        throw new Error('Rule should declare lineBreaks: ' + regexp)
      }

      // store regex
      parts.push(reCapture(pat))
    }


    // If there's no fallback rule, use the sticky flag so we only look for
    // matches at the current index.
    //
    // If we don't support the sticky flag, then fake it using an irrefutable
    // match (i.e. an empty pattern).
    var fallbackRule = errorRule && errorRule.fallback
    var flags = hasSticky && !fallbackRule ? 'ym' : 'gm'
    var suffix = hasSticky || fallbackRule ? '' : '|'

    if (unicodeFlag === true) flags += "u"
    var combined = new RegExp(reUnion(parts) + suffix, flags)
    return {regexp: combined, groups: groups, fast: fast, error: errorRule || defaultErrorRule}
  }

  function compile(rules) {
    var result = compileRules(toRules(rules))
    return new Lexer({start: result}, 'start')
  }

  function checkStateGroup(g, name, map) {
    var state = g && (g.push || g.next)
    if (state && !map[state]) {
      throw new Error("Missing state '" + state + "' (in token '" + g.defaultType + "' of state '" + name + "')")
    }
    if (g && g.pop && +g.pop !== 1) {
      throw new Error("pop must be 1 (in token '" + g.defaultType + "' of state '" + name + "')")
    }
  }
  function compileStates(states, start) {
    var all = states.$all ? toRules(states.$all) : []
    delete states.$all

    var keys = Object.getOwnPropertyNames(states)
    if (!start) start = keys[0]

    var ruleMap = Object.create(null)
    for (var i = 0; i < keys.length; i++) {
      var key = keys[i]
      ruleMap[key] = toRules(states[key]).concat(all)
    }
    for (var i = 0; i < keys.length; i++) {
      var key = keys[i]
      var rules = ruleMap[key]
      var included = Object.create(null)
      for (var j = 0; j < rules.length; j++) {
        var rule = rules[j]
        if (!rule.include) continue
        var splice = [j, 1]
        if (rule.include !== key && !included[rule.include]) {
          included[rule.include] = true
          var newRules = ruleMap[rule.include]
          if (!newRules) {
            throw new Error("Cannot include nonexistent state '" + rule.include + "' (in state '" + key + "')")
          }
          for (var k = 0; k < newRules.length; k++) {
            var newRule = newRules[k]
            if (rules.indexOf(newRule) !== -1) continue
            splice.push(newRule)
          }
        }
        rules.splice.apply(rules, splice)
        j--
      }
    }

    var map = Object.create(null)
    for (var i = 0; i < keys.length; i++) {
      var key = keys[i]
      map[key] = compileRules(ruleMap[key], true)
    }

    for (var i = 0; i < keys.length; i++) {
      var name = keys[i]
      var state = map[name]
      var groups = state.groups
      for (var j = 0; j < groups.length; j++) {
        checkStateGroup(groups[j], name, map)
      }
      var fastKeys = Object.getOwnPropertyNames(state.fast)
      for (var j = 0; j < fastKeys.length; j++) {
        checkStateGroup(state.fast[fastKeys[j]], name, map)
      }
    }

    return new Lexer(map, start)
  }

  function keywordTransform(map) {
    var reverseMap = Object.create(null)
    var byLength = Object.create(null)
    var types = Object.getOwnPropertyNames(map)
    for (var i = 0; i < types.length; i++) {
      var tokenType = types[i]
      var item = map[tokenType]
      var keywordList = Array.isArray(item) ? item : [item]
      keywordList.forEach(function(keyword) {
        (byLength[keyword.length] = byLength[keyword.length] || []).push(keyword)
        if (typeof keyword !== 'string') {
          throw new Error("keyword must be string (in keyword '" + tokenType + "')")
        }
        reverseMap[keyword] = tokenType
      })
    }

    // fast string lookup
    // https://jsperf.com/string-lookups
    function str(x) { return JSON.stringify(x) }
    var source = ''
    source += 'switch (value.length) {\n'
    for (var length in byLength) {
      var keywords = byLength[length]
      source += 'case ' + length + ':\n'
      source += 'switch (value) {\n'
      keywords.forEach(function(keyword) {
        var tokenType = reverseMap[keyword]
        source += 'case ' + str(keyword) + ': return ' + str(tokenType) + '\n'
      })
      source += '}\n'
    }
    source += '}\n'
    return Function('value', source) // type
  }

  /***************************************************************************/

  var Lexer = function(states, state) {
    this.startState = state
    this.states = states
    this.buffer = ''
    this.stack = []
    this.reset()
  }

  Lexer.prototype.reset = function(data, info) {
    this.buffer = data || ''
    this.index = 0
    this.line = info ? info.line : 1
    this.col = info ? info.col : 1
    this.queuedToken = info ? info.queuedToken : null
    this.queuedThrow = info ? info.queuedThrow : null
    this.setState(info ? info.state : this.startState)
    this.stack = info && info.stack ? info.stack.slice() : []
    return this
  }

  Lexer.prototype.save = function() {
    return {
      line: this.line,
      col: this.col,
      state: this.state,
      stack: this.stack.slice(),
      queuedToken: this.queuedToken,
      queuedThrow: this.queuedThrow,
    }
  }

  Lexer.prototype.setState = function(state) {
    if (!state || this.state === state) return
    this.state = state
    var info = this.states[state]
    this.groups = info.groups
    this.error = info.error
    this.re = info.regexp
    this.fast = info.fast
  }

  Lexer.prototype.popState = function() {
    this.setState(this.stack.pop())
  }

  Lexer.prototype.pushState = function(state) {
    this.stack.push(this.state)
    this.setState(state)
  }

  var eat = hasSticky ? function(re, buffer) { // assume re is /y
    return re.exec(buffer)
  } : function(re, buffer) { // assume re is /g
    var match = re.exec(buffer)
    // will always match, since we used the |(?:) trick
    if (match[0].length === 0) {
      return null
    }
    return match
  }

  Lexer.prototype._getGroup = function(match) {
    var groupCount = this.groups.length
    for (var i = 0; i < groupCount; i++) {
      if (match[i + 1] !== undefined) {
        return this.groups[i]
      }
    }
    throw new Error('Cannot find token type for matched text')
  }

  function tokenToString() {
    return this.value
  }

  Lexer.prototype.next = function() {
    var index = this.index

    // If a fallback token matched, we don't need to re-run the RegExp
    if (this.queuedGroup) {
      var token = this._token(this.queuedGroup, this.queuedText, index)
      this.queuedGroup = null
      this.queuedText = ""
      return token
    }

    var buffer = this.buffer
    if (index === buffer.length) {
      return // EOF
    }

    // Fast matching for single characters
    var group = this.fast[buffer.charCodeAt(index)]
    if (group) {
      return this._token(group, buffer.charAt(index), index)
    }

    // Execute RegExp
    var re = this.re
    re.lastIndex = index
    var match = eat(re, buffer)

    // Error tokens match the remaining buffer
    var error = this.error
    if (match == null) {
      return this._token(error, buffer.slice(index, buffer.length), index)
    }

    var group = this._getGroup(match)
    var text = match[0]

    if (error.fallback && match.index !== index) {
      this.queuedGroup = group
      this.queuedText = text

      // Fallback tokens contain the unmatched portion of the buffer
      return this._token(error, buffer.slice(index, match.index), index)
    }

    return this._token(group, text, index)
  }

  Lexer.prototype._token = function(group, text, offset) {
    // count line breaks
    var lineBreaks = 0
    if (group.lineBreaks) {
      var matchNL = /\n/g
      var nl = 1
      if (text === '\n') {
        lineBreaks = 1
      } else {
        while (matchNL.exec(text)) { lineBreaks++; nl = matchNL.lastIndex }
      }
    }

    var token = {
      type: (typeof group.type === 'function' && group.type(text)) || group.defaultType,
      value: typeof group.value === 'function' ? group.value(text) : text,
      text: text,
      toString: tokenToString,
      offset: offset,
      lineBreaks: lineBreaks,
      line: this.line,
      col: this.col,
    }
    // nb. adding more props to token object will make V8 sad!

    var size = text.length
    this.index += size
    this.line += lineBreaks
    if (lineBreaks !== 0) {
      this.col = size - nl + 1
    } else {
      this.col += size
    }

    // throw, if no rule with {error: true}
    if (group.shouldThrow) {
      throw new Error(this.formatError(token, "invalid syntax"))
    }

    if (group.pop) this.popState()
    else if (group.push) this.pushState(group.push)
    else if (group.next) this.setState(group.next)

    return token
  }

  if (typeof Symbol !== 'undefined' && Symbol.iterator) {
    var LexerIterator = function(lexer) {
      this.lexer = lexer
    }

    LexerIterator.prototype.next = function() {
      var token = this.lexer.next()
      return {value: token, done: !token}
    }

    LexerIterator.prototype[Symbol.iterator] = function() {
      return this
    }

    Lexer.prototype[Symbol.iterator] = function() {
      return new LexerIterator(this)
    }
  }

  Lexer.prototype.formatError = function(token, message) {
    if (token == null) {
      // An undefined token indicates EOF
      var text = this.buffer.slice(this.index)
      var token = {
        text: text,
        offset: this.index,
        lineBreaks: text.indexOf('\n') === -1 ? 0 : 1,
        line: this.line,
        col: this.col,
      }
    }
    var start = Math.max(0, token.offset - token.col + 1)
    var eol = token.lineBreaks ? token.text.indexOf('\n') : token.text.length
    var firstLine = this.buffer.substring(start, token.offset + eol)
    message += " at line " + token.line + " col " + token.col + ":\n\n"
    message += "  " + firstLine + "\n"
    message += "  " + Array(token.col).join(" ") + "^"
    return message
  }

  Lexer.prototype.clone = function() {
    return new Lexer(this.states, this.state)
  }

  Lexer.prototype.has = function(tokenType) {
    return true
  }


  return {
    compile: compile,
    states: compileStates,
    error: Object.freeze({error: true}),
    fallback: Object.freeze({fallback: true}),
    keywords: keywordTransform,
  }

}));


/***/ }),
/* 35 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const tippy_js_1 = __webpack_require__(47);
__webpack_require__(37);
__webpack_require__(39);
const Core_1 = __webpack_require__(0);
const Lattice_1 = __webpack_require__(12);
const Annotation_1 = __webpack_require__(10);
const Expl_1 = __webpack_require__(5);
const Graphics_1 = __webpack_require__(8);
const Value_1 = __webpack_require__(1);
const Renderer_1 = __webpack_require__(15);
function createTooltip(element, placement) {
    return tippy_js_1.default(element, { theme: "light-border", placement });
}
class Interactor {
    constructor(editor, C, cursor /*<Pair<Num, Num>>*/, element) {
        this.propFocus = null;
        this.editor = editor;
        this.C = C;
        this.tooltip = createTooltip(element, editor.tooltipPlacement);
        this.editor.tooltips.add(this.tooltip);
        this.cursor = cursor;
        this.element = element;
        const p = Core_1.as(cursor.tv.v, C);
        const propsFocus = this.focusedProps(cursor.tv);
        if (propsFocus.length > 0) {
            this.tooltip.setContent(this.propValues(p, propsFocus));
            this.tooltip.show();
            element.classList.add("focus");
        }
        element.addEventListener("mousemove", (e) => {
            e.stopPropagation();
            this.onMouseMove(e);
        });
        element.addEventListener("mouseout", (e) => {
            e.stopPropagation();
            this.onMouseOut(e);
        });
    }
    // Non-primitive dependencies render as a bullet.
    propValues(g, props) {
        const lines = props.map((prop) => {
            const propVal = g.__child(prop);
            const propStr = propVal instanceof Value_1.Num ?
                Renderer_1.round(propVal.val) :
                propVal instanceof Value_1.Str ?
                    propVal.val : "•";
            return `${prop}: ${propStr}`;
        });
        return lines.join("</br>");
    }
    focusedProps(tv) {
        return Value_1.fields(tv.v).filter((prop) => {
            const tv_ = Expl_1.Expl.explChild(tv.t, tv.v, prop);
            return Core_1.__nonNull(this.editor.direction) === Annotation_1.Direction.Fwd ? Lattice_1.bool_.negate(Annotation_1.isα(tv_)) : Annotation_1.isα(tv_);
        });
    }
    onMouseMove(e) {
        const v = Core_1.as(this.cursor.tv.v, this.C);
        const rect = this.element.getBoundingClientRect();
        // invert sign on y axis because of global inversion for SVG graphics
        const x_prop = Math.max(e.clientX - rect.left, 0) / rect.width;
        const y_prop = Math.min(rect.bottom - e.clientY, rect.height) / rect.height;
        const propFocus = this.propFor(x_prop, y_prop);
        if (this.propFocus !== propFocus) {
            this.propFocus = propFocus;
            this.editor.bwdSlice(() => {
                Annotation_1.setα(Lattice_1.bool_.top, this.cursor.to(this.C, propFocus).tv);
            });
            this.tooltip.setContent(this.propValues(v, [propFocus]));
            this.element.classList.add("focus");
        }
    }
    onMouseOut(e) {
        this.propFocus = null;
        this.editor.bwdSlice(() => { });
        this.element.classList.remove("focus");
    }
}
class PointInteractor extends Interactor {
    constructor(editor, tp /*<Point>*/, marker) {
        super(editor, Graphics_1.Point, tp, marker);
    }
    propFor(x_prop, y_prop) {
        return "y";
    }
}
exports.PointInteractor = PointInteractor;
class RectInteractor extends Interactor {
    constructor(editor, tg /*<Rect>*/, r) {
        super(editor, Graphics_1.Rect, tg, r);
    }
    // Determine which "diagonal quadrant" of the unit square [1, 1] contains [x, y], and
    // then map to the corresponding attribute of the rectangle.
    propFor(x, y) {
        const corner = y > x ? ["x", "height"] : ["y", "width"];
        return y < 1 - x ? corner[0] : corner[1];
    }
}
exports.RectInteractor = RectInteractor;


/***/ }),
/* 36 */
/***/ (function(module, exports) {

var g;

// This works in non-strict mode
g = (function() {
	return this;
})();

try {
	// This works if eval is allowed (see CSP)
	g = g || new Function("return this")();
} catch (e) {
	// This works if the window reference is available
	if (typeof window === "object") g = window;
}

// g can still be undefined, but nothing to do about it...
// We return undefined, instead of nothing here, so it's
// easier to handle this case. if(!global) { ...}

module.exports = g;


/***/ }),
/* 37 */
/***/ (function(module, exports, __webpack_require__) {

var api = __webpack_require__(18);
            var content = __webpack_require__(38);

            content = content.__esModule ? content.default : content;

            if (typeof content === 'string') {
              content = [[module.i, content, '']];
            }

var options = {};

options.insert = "head";
options.singleton = false;

var update = api(content, options);

var exported = content.locals ? content.locals : {};



module.exports = exported;

/***/ }),
/* 38 */
/***/ (function(module, exports, __webpack_require__) {

// Imports
var ___CSS_LOADER_API_IMPORT___ = __webpack_require__(19);
exports = ___CSS_LOADER_API_IMPORT___(false);
// Module
exports.push([module.i, ".tippy-tooltip[data-animation=fade][data-state=hidden]{opacity:0}.tippy-iOS{cursor:pointer!important;-webkit-tap-highlight-color:transparent}.tippy-popper{pointer-events:none;max-width:calc(100vw - 10px);transition-timing-function:cubic-bezier(.165,.84,.44,1);transition-property:transform}.tippy-tooltip{position:relative;color:#fff;border-radius:4px;font-size:14px;line-height:1.4;background-color:#333;transition-property:visibility,opacity,transform;outline:0}.tippy-tooltip[data-placement^=top]>.tippy-arrow{border-width:8px 8px 0;border-top-color:#333;margin:0 3px;transform-origin:50% 0;bottom:-7px}.tippy-tooltip[data-placement^=bottom]>.tippy-arrow{border-width:0 8px 8px;border-bottom-color:#333;margin:0 3px;transform-origin:50% 7px;top:-7px}.tippy-tooltip[data-placement^=left]>.tippy-arrow{border-width:8px 0 8px 8px;border-left-color:#333;margin:3px 0;transform-origin:0 50%;right:-7px}.tippy-tooltip[data-placement^=right]>.tippy-arrow{border-width:8px 8px 8px 0;border-right-color:#333;margin:3px 0;transform-origin:7px 50%;left:-7px}.tippy-tooltip[data-interactive][data-state=visible]{pointer-events:auto}.tippy-tooltip[data-inertia][data-state=visible]{transition-timing-function:cubic-bezier(.54,1.5,.38,1.11)}.tippy-arrow{position:absolute;border-color:transparent;border-style:solid}.tippy-content{padding:5px 9px}", ""]);
// Exports
module.exports = exports;


/***/ }),
/* 39 */
/***/ (function(module, exports, __webpack_require__) {

var api = __webpack_require__(18);
            var content = __webpack_require__(40);

            content = content.__esModule ? content.default : content;

            if (typeof content === 'string') {
              content = [[module.i, content, '']];
            }

var options = {};

options.insert = "head";
options.singleton = false;

var update = api(content, options);

var exported = content.locals ? content.locals : {};



module.exports = exported;

/***/ }),
/* 40 */
/***/ (function(module, exports, __webpack_require__) {

// Imports
var ___CSS_LOADER_API_IMPORT___ = __webpack_require__(19);
exports = ___CSS_LOADER_API_IMPORT___(false);
// Module
exports.push([module.i, ".tippy-tooltip.light-border-theme{background-color:#fff;background-clip:padding-box;border:1px solid rgba(0,8,16,.15);color:#26323d;box-shadow:0 4px 14px -2px rgba(0,8,16,.08)}.tippy-tooltip.light-border-theme>.tippy-backdrop{background-color:#fff}.tippy-tooltip.light-border-theme>.tippy-arrow:after,.tippy-tooltip.light-border-theme>.tippy-arrow:before,.tippy-tooltip.light-border-theme>.tippy-svg-arrow:after,.tippy-tooltip.light-border-theme>.tippy-svg-arrow:before{content:\"\";position:absolute;z-index:-1}.tippy-tooltip.light-border-theme>.tippy-svg-arrow{fill:#fff}.tippy-tooltip.light-border-theme>.tippy-svg-arrow:after{background-image:url(data:image/svg+xml;base64,PHN2ZyBjbGlwLXJ1bGU9ImV2ZW5vZGQiIGZpbGwtcnVsZT0iZXZlbm9kZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgc3Ryb2tlLW1pdGVybGltaXQ9IjEuNDE0IiB2aWV3Qm94PSIwIDAgMTggNyIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj48cGF0aCBkPSJNMCA3czIuMDIxLS4wMTUgNS4yNTMtNC4yMThDNi41ODQgMS4wNTEgNy43OTcuMDA3IDkgMGMxLjIwMy0uMDA3IDIuNDE2IDEuMDM1IDMuNzYxIDIuNzgyQzE2LjAxMiA3LjAwNSAxOCA3IDE4IDd6IiBmaWxsPSIjMzMzIiBmaWxsLW9wYWNpdHk9Ii4yMzUiIGZpbGwtcnVsZT0ibm9uemVybyIvPjwvc3ZnPg==);background-size:18px 7px;width:18px;height:7px;left:0;top:0;fill:rgba(0,8,16,.15)}.tippy-tooltip.light-border-theme[data-placement^=top]>.tippy-svg-arrow:after{top:1px;transform:rotate(180deg)}.tippy-tooltip.light-border-theme[data-placement^=top]>.tippy-arrow{border-top-color:#fff}.tippy-tooltip.light-border-theme[data-placement^=top]>.tippy-arrow:after{border-top:7px solid #fff;top:-7px}.tippy-tooltip.light-border-theme[data-placement^=top]>.tippy-arrow:before{border-top:7px solid rgba(0,8,16,.2);bottom:-1px}.tippy-tooltip.light-border-theme[data-placement^=bottom]>.tippy-svg-arrow:after{top:-1px}.tippy-tooltip.light-border-theme[data-placement^=bottom]>.tippy-arrow{border-bottom-color:#fff}.tippy-tooltip.light-border-theme[data-placement^=bottom]>.tippy-arrow:after{border-bottom:7px solid #fff;bottom:-7px}.tippy-tooltip.light-border-theme[data-placement^=bottom]>.tippy-arrow:before{border-bottom:7px solid rgba(0,8,16,.2);bottom:-6px}.tippy-tooltip.light-border-theme[data-placement^=left]>.tippy-svg-arrow:after{left:1px;top:0;transform:rotate(90deg)}.tippy-tooltip.light-border-theme[data-placement^=left]>.tippy-arrow{border-left-color:#fff}.tippy-tooltip.light-border-theme[data-placement^=left]>.tippy-arrow:after{border-left:7px solid #fff;left:-7px}.tippy-tooltip.light-border-theme[data-placement^=left]>.tippy-arrow:before{border-left:7px solid rgba(0,8,16,.2);left:-6px}.tippy-tooltip.light-border-theme[data-placement^=right]>.tippy-svg-arrow:after{left:-1px;top:0;transform:rotate(-90deg)}.tippy-tooltip.light-border-theme[data-placement^=right]>.tippy-arrow{border-right-color:#fff}.tippy-tooltip.light-border-theme[data-placement^=right]>.tippy-arrow:after{border-right:7px solid #fff;right:-7px}.tippy-tooltip.light-border-theme[data-placement^=right]>.tippy-arrow:before{border-right:7px solid rgba(0,8,16,.2);right:-6px}.tippy-tooltip.light-border-theme[data-placement^=bottom]>.tippy-arrow,.tippy-tooltip.light-border-theme[data-placement^=bottom]>.tippy-svg-arrow,.tippy-tooltip.light-border-theme[data-placement^=top]>.tippy-arrow,.tippy-tooltip.light-border-theme[data-placement^=top]>.tippy-svg-arrow{transform:translateX(-1px)}.tippy-tooltip.light-border-theme[data-placement^=bottom]>.tippy-arrow:after,.tippy-tooltip.light-border-theme[data-placement^=bottom]>.tippy-arrow:before,.tippy-tooltip.light-border-theme[data-placement^=top]>.tippy-arrow:after,.tippy-tooltip.light-border-theme[data-placement^=top]>.tippy-arrow:before{left:-7px;border-left:7px solid transparent;border-right:7px solid transparent}.tippy-tooltip.light-border-theme[data-placement^=left]>.tippy-arrow,.tippy-tooltip.light-border-theme[data-placement^=left]>.tippy-svg-arrow,.tippy-tooltip.light-border-theme[data-placement^=right]>.tippy-arrow,.tippy-tooltip.light-border-theme[data-placement^=right]>.tippy-svg-arrow{transform:translateY(-1px)}.tippy-tooltip.light-border-theme[data-placement^=left]>.tippy-arrow:after,.tippy-tooltip.light-border-theme[data-placement^=left]>.tippy-arrow:before,.tippy-tooltip.light-border-theme[data-placement^=right]>.tippy-arrow:after,.tippy-tooltip.light-border-theme[data-placement^=right]>.tippy-arrow:before{top:-7px;border-top:7px solid transparent;border-bottom:7px solid transparent}", ""]);
// Exports
module.exports = exports;


/***/ }),
/* 41 */
/***/ (function(module, exports, __webpack_require__) {

// Imports
var ___CSS_LOADER_API_IMPORT___ = __webpack_require__(19);
var ___CSS_LOADER_GET_URL_IMPORT___ = __webpack_require__(42);
var ___CSS_LOADER_URL_IMPORT_0___ = __webpack_require__(43);
var ___CSS_LOADER_URL_IMPORT_1___ = __webpack_require__(44);
var ___CSS_LOADER_URL_IMPORT_2___ = __webpack_require__(45);
exports = ___CSS_LOADER_API_IMPORT___(false);
var ___CSS_LOADER_URL_REPLACEMENT_0___ = ___CSS_LOADER_GET_URL_IMPORT___(___CSS_LOADER_URL_IMPORT_0___);
var ___CSS_LOADER_URL_REPLACEMENT_1___ = ___CSS_LOADER_GET_URL_IMPORT___(___CSS_LOADER_URL_IMPORT_1___);
var ___CSS_LOADER_URL_REPLACEMENT_2___ = ___CSS_LOADER_GET_URL_IMPORT___(___CSS_LOADER_URL_IMPORT_2___);
// Module
exports.push([module.i, "@font-face {\n   font-family: \"inconsolata\";\n   font-style: normal;\n   font-weight: 400;\n   src: url(" + ___CSS_LOADER_URL_REPLACEMENT_0___ + ") format('woff');\n}\n\n@font-face {\n   font-family: \"Slabo 13px\";\n   font-style: normal;\n   font-weight: 400;\n   src: url(" + ___CSS_LOADER_URL_REPLACEMENT_1___ + ") format('woff');\n}\n\n@font-face {\n   font-family: \"AvenirLTStd-Book\";\n   font-style: normal;\n   font-weight: 400;\n   src: url(" + ___CSS_LOADER_URL_REPLACEMENT_2___ + ") format('woff');\n}\n\n.code {\n   font-family: \"inconsolata\";\n   stroke: none;\n}\n\n.label {\n   font-family: \"AvenirLTStd-Book\";\n   stroke: none;\n}\n\n.unchanged {\n   fill: black;\n}\n\n.changed {\n   fill: blue;\n}\n\n.new {\n   fill: limegreen;\n}\n\n.tippy-tooltip {\n   font-family: \"inconsolata\";\n}\n\n.tippy-tooltip.light-border-theme {\n   font-size: 9pt;\n}\n\n.focus {\n   stroke: black;\n   stroke-opacity: 0.1;\n   fill-opacity: 0.7;\n}\n", ""]);
// Exports
module.exports = exports;


/***/ }),
/* 42 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


module.exports = function (url, options) {
  if (!options) {
    // eslint-disable-next-line no-param-reassign
    options = {};
  } // eslint-disable-next-line no-underscore-dangle, no-param-reassign


  url = url && url.__esModule ? url.default : url;

  if (typeof url !== 'string') {
    return url;
  } // If url is already wrapped in quotes, remove them


  if (/^['"].*['"]$/.test(url)) {
    // eslint-disable-next-line no-param-reassign
    url = url.slice(1, -1);
  }

  if (options.hash) {
    // eslint-disable-next-line no-param-reassign
    url += options.hash;
  } // Should url be wrapped?
  // See https://drafts.csswg.org/css-values-3/#urls


  if (/["'() \t\n]/.test(url) || options.needQuotes) {
    return "\"".concat(url.replace(/"/g, '\\"').replace(/\n/g, '\\n'), "\"");
  }

  return url;
};

/***/ }),
/* 43 */
/***/ (function(module, exports) {

module.exports = "data:font/woff;base64,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"

/***/ }),
/* 44 */
/***/ (function(module, exports) {

module.exports = "data:font/woff;base64,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"

/***/ }),
/* 45 */
/***/ (function(module, exports) {

module.exports = "data:font/woff;base64,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"

/***/ }),
/* 46 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(0);
const Annotation_1 = __webpack_require__(10);
const Pane_1 = __webpack_require__(28);
const View_1 = __webpack_require__(29);
class PaneCoordinator {
    constructor(appRoot, ρ_external) {
        this.panes = new Set();
        this.appRoot = appRoot;
        this.ρ_external = ρ_external;
    }
    addPane(ρ, e, tooltipPlacement = "top") {
        const pane = new Pane_1.Pane.Pane(this, this.appRoot, View_1.View.defaultDims, tooltipPlacement, this.ρ_external, ρ, e);
        pane.initialise();
        this.panes.add(pane);
        return pane;
    }
    removePane(pane) {
        Core_1.assert(this.panes.has(pane));
        this.panes.delete(pane);
    }
    onBwdSlice(editor, externDeps) {
        // consider availability of ρ_external only; treat ρ and e as unlimited resources
        [...this.panes]
            .filter(editor_ => editor_ !== editor)
            .forEach((editor_) => {
            editor_.fwdSlice(externDeps);
            editor_.render(); // TODO: just redo selection rendering
        });
        Annotation_1.__slice.reset(Annotation_1.Direction.Fwd);
    }
}
exports.PaneCoordinator = PaneCoordinator;


/***/ }),
/* 47 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
// ESM COMPAT FLAG
__webpack_require__.r(__webpack_exports__);

// EXPORTS
__webpack_require__.d(__webpack_exports__, "createTippyWithPlugins", function() { return /* reexport */ createTippyWithPlugins; });
__webpack_require__.d(__webpack_exports__, "default", function() { return /* reexport */ tippy; });
__webpack_require__.d(__webpack_exports__, "hideAll", function() { return /* reexport */ hideAll; });
__webpack_require__.d(__webpack_exports__, "roundArrow", function() { return /* reexport */ ROUND_ARROW; });
__webpack_require__.d(__webpack_exports__, "animateFill", function() { return /* binding */ animateFill; });
__webpack_require__.d(__webpack_exports__, "createSingleton", function() { return /* binding */ tippy_esm_createSingleton; });
__webpack_require__.d(__webpack_exports__, "delegate", function() { return /* binding */ delegate; });
__webpack_require__.d(__webpack_exports__, "followCursor", function() { return /* binding */ tippy_esm_followCursor; });
__webpack_require__.d(__webpack_exports__, "inlinePositioning", function() { return /* binding */ inlinePositioning; });
__webpack_require__.d(__webpack_exports__, "sticky", function() { return /* binding */ sticky; });

// EXTERNAL MODULE: ./node_modules/popper.js/dist/esm/popper.js
var esm_popper = __webpack_require__(20);

// CONCATENATED MODULE: ./node_modules/tippy.js/dist/tippy.chunk.esm.js
/**!
* tippy.js v5.2.1
* (c) 2017-2020 atomiks
* MIT License
*/


function _extends() {
  _extends = Object.assign || function (target) {
    for (var i = 1; i < arguments.length; i++) {
      var source = arguments[i];

      for (var key in source) {
        if (Object.prototype.hasOwnProperty.call(source, key)) {
          target[key] = source[key];
        }
      }
    }

    return target;
  };

  return _extends.apply(this, arguments);
}

var version = "5.2.1";

/**
 * Triggers reflow
 */
function reflow(element) {
  void element.offsetHeight;
}
/**
 * Sets the innerHTML of an element
 */

function setInnerHTML(element, html) {
  element[innerHTML()] = html;
}
/**
 * Determines if the value is a reference element
 */

function isReferenceElement(value) {
  return !!(value && value._tippy && value._tippy.reference === value);
}
/**
 * Safe .hasOwnProperty check, for prototype-less objects
 */

function tippy_chunk_esm_hasOwnProperty(obj, key) {
  return {}.hasOwnProperty.call(obj, key);
}
/**
 * Returns an array of elements based on the value
 */

function getArrayOfElements(value) {
  if (isElement(value)) {
    return [value];
  }

  if (isNodeList(value)) {
    return arrayFrom(value);
  }

  if (Array.isArray(value)) {
    return value;
  }

  return arrayFrom(document.querySelectorAll(value));
}
/**
 * Returns a value at a given index depending on if it's an array or number
 */

function getValueAtIndexOrReturn(value, index, defaultValue) {
  if (Array.isArray(value)) {
    var v = value[index];
    return v == null ? Array.isArray(defaultValue) ? defaultValue[index] : defaultValue : v;
  }

  return value;
}
/**
 * Prevents errors from being thrown while accessing nested modifier objects
 * in `popperOptions`
 */

function getModifier(obj, key) {
  return obj && obj.modifiers && obj.modifiers[key];
}
/**
 * Determines if the value is of type
 */

function isType(value, type) {
  var str = {}.toString.call(value);
  return str.indexOf('[object') === 0 && str.indexOf(type + "]") > -1;
}
/**
 * Determines if the value is of type Element
 */

function isElement(value) {
  return isType(value, 'Element');
}
/**
 * Determines if the value is of type NodeList
 */

function isNodeList(value) {
  return isType(value, 'NodeList');
}
/**
 * Determines if the value is of type MouseEvent
 */

function isMouseEvent(value) {
  return isType(value, 'MouseEvent');
}
/**
 * Firefox extensions don't allow setting .innerHTML directly, this will trick
 * it
 */

function innerHTML() {
  return 'innerHTML';
}
/**
 * Evaluates a function if one, or returns the value
 */

function invokeWithArgsOrReturn(value, args) {
  return typeof value === 'function' ? value.apply(void 0, args) : value;
}
/**
 * Sets a popperInstance modifier's property to a value
 */

function setModifierValue(modifiers, name, property, value) {
  modifiers.filter(function (m) {
    return m.name === name;
  })[0][property] = value;
}
/**
 * Returns a new `div` element
 */

function div() {
  return document.createElement('div');
}
/**
 * Applies a transition duration to a list of elements
 */

function setTransitionDuration(els, value) {
  els.forEach(function (el) {
    if (el) {
      el.style.transitionDuration = value + "ms";
    }
  });
}
/**
 * Sets the visibility state to elements so they can begin to transition
 */

function setVisibilityState(els, state) {
  els.forEach(function (el) {
    if (el) {
      el.setAttribute('data-state', state);
    }
  });
}
/**
 * Debounce utility. To avoid bloating bundle size, we're only passing 1
 * argument here, a more generic function would pass all arguments. Only
 * `onMouseMove` uses this which takes the event object for now.
 */

function debounce(fn, ms) {
  // Avoid wrapping in `setTimeout` if ms is 0 anyway
  if (ms === 0) {
    return fn;
  }

  var timeout;
  return function (arg) {
    clearTimeout(timeout);
    timeout = setTimeout(function () {
      fn(arg);
    }, ms);
  };
}
/**
 * Preserves the original function invocation when another function replaces it
 */

function preserveInvocation(originalFn, currentFn, args) {
  if (originalFn && originalFn !== currentFn) {
    originalFn.apply(void 0, args);
  }
}
/**
 * Deletes properties from an object (pure)
 */

function removeProperties(obj, keys) {
  var clone = _extends({}, obj);

  keys.forEach(function (key) {
    delete clone[key];
  });
  return clone;
}
/**
 * Ponyfill for Array.from - converts iterable values to an array
 */

function arrayFrom(value) {
  return [].slice.call(value);
}
/**
 * Works like Element.prototype.closest, but uses a callback instead
 */

function closestCallback(element, callback) {
  while (element) {
    if (callback(element)) {
      return element;
    }

    element = element.parentElement;
  }

  return null;
}
/**
 * Determines if an array or string includes a string
 */

function includes(a, b) {
  return a.indexOf(b) > -1;
}
/**
 * Creates an array from string of values separated by whitespace
 */

function splitBySpaces(value) {
  return value.split(/\s+/).filter(Boolean);
}
/**
 * Returns the `nextValue` if `nextValue` is not `undefined`, otherwise returns
 * `currentValue`
 */

function useIfDefined(nextValue, currentValue) {
  return nextValue !== undefined ? nextValue : currentValue;
}
/**
 * Converts a value that's an array or single value to an array
 */

function normalizeToArray(value) {
  return [].concat(value);
}
/**
 * Returns the ownerDocument of the first available element, otherwise global
 * document
 */

function getOwnerDocument(elementOrElements) {
  var _normalizeToArray = normalizeToArray(elementOrElements),
      element = _normalizeToArray[0];

  return element ? element.ownerDocument || document : document;
}
/**
 * Adds item to array if array does not contain it
 */

function pushIfUnique(arr, value) {
  if (arr.indexOf(value) === -1) {
    arr.push(value);
  }
}
/**
 * Adds `px` if value is a number, or returns it directly
 */

function appendPxIfNumber(value) {
  return typeof value === 'number' ? value + "px" : value;
}
/**
 * Filters out duplicate elements in an array
 */

function unique(arr) {
  return arr.filter(function (item, index) {
    return arr.indexOf(item) === index;
  });
}
/**
 * Returns number from number or CSS units string
 */

function getNumber(value) {
  return typeof value === 'number' ? value : parseFloat(value);
}
/**
 * Gets number or CSS string units in pixels (e.g. `1rem` -> 16)
 */

function getUnitsInPx(doc, value) {
  var isRem = typeof value === 'string' && includes(value, 'rem');
  var html = doc.documentElement;
  var rootFontSize = 16;

  if (html && isRem) {
    return parseFloat(getComputedStyle(html).fontSize || String(rootFontSize)) * getNumber(value);
  }

  return getNumber(value);
}
/**
 * Adds the `distancePx` value to the placement of a Popper.Padding object
 */

function getComputedPadding(basePlacement, padding, distancePx) {
  if (padding === void 0) {
    padding = 5;
  }

  var freshPaddingObject = {
    top: 0,
    right: 0,
    bottom: 0,
    left: 0
  };
  var keys = Object.keys(freshPaddingObject);
  return keys.reduce(function (obj, key) {
    obj[key] = typeof padding === 'number' ? padding : padding[key];

    if (basePlacement === key) {
      obj[key] = typeof padding === 'number' ? padding + distancePx : padding[basePlacement] + distancePx;
    }

    return obj;
  }, freshPaddingObject);
}

function createMemoryLeakWarning(method) {
  var txt = method === 'destroy' ? 'n already-' : ' ';
  return "\n    " + method + "() was called on a" + txt + "destroyed instance. This is a no-op but\n    indicates a potential memory leak.\n  ";
}
function clean(value) {
  var spacesAndTabs = /[ \t]{2,}/g;
  var lineStartWithSpaces = /^[ \t]*/gm;
  return value.replace(spacesAndTabs, ' ').replace(lineStartWithSpaces, '').trim();
}

function getDevMessage(message) {
  return clean("\n  %ctippy.js\n\n  %c" + clean(message) + "\n\n  %c\uD83D\uDC77\u200D This is a development-only message. It will be removed in production.\n  ");
}

function getFormattedMessage(message) {
  return [getDevMessage(message), // title
  'color: #00C584; font-size: 1.3em; font-weight: bold;', // message
  'line-height: 1.5', // footer
  'color: #a6a095;'];
}
/**
 * Helpful wrapper around `console.warn()`.
 * TODO: Should we use a cache so it only warns a single time and not spam the
 * console? (Need to consider hot reloading and invalidation though). Chrome
 * already batches warnings as well.
 */

function warnWhen(condition, message) {
  if (condition) {
    var _console;

    (_console = console).warn.apply(_console, getFormattedMessage(message));
  }
}
/**
 * Helpful wrapper around `console.error()`
 */

function errorWhen(condition, message) {
  if (condition) {
    var _console2;

    (_console2 = console).error.apply(_console2, getFormattedMessage(message));
  }
}
/**
 * Validates the `targets` value passed to `tippy()`
 */

function validateTargets(targets) {
  var didPassFalsyValue = !targets;
  var didPassPlainObject = Object.prototype.toString.call(targets) === '[object Object]' && !targets.addEventListener;
  errorWhen(didPassFalsyValue, ['tippy() was passed', '`' + String(targets) + '`', 'as its targets (first) argument. Valid types are: String, Element, Element[],', 'or NodeList.'].join(' '));
  errorWhen(didPassPlainObject, ['tippy() was passed a plain object which is no longer supported as an argument.', 'See: https://atomiks.github.io/tippyjs/misc/#custom-position'].join(' '));
}

var pluginProps = {
  animateFill: false,
  followCursor: false,
  inlinePositioning: false,
  sticky: false
};
var defaultProps = _extends({
  allowHTML: true,
  animation: 'fade',
  appendTo: function appendTo() {
    return document.body;
  },
  aria: 'describedby',
  arrow: true,
  boundary: 'scrollParent',
  content: '',
  delay: 0,
  distance: 10,
  duration: [300, 250],
  flip: true,
  flipBehavior: 'flip',
  flipOnUpdate: false,
  hideOnClick: true,
  ignoreAttributes: false,
  inertia: false,
  interactive: false,
  interactiveBorder: 2,
  interactiveDebounce: 0,
  lazy: true,
  maxWidth: 350,
  multiple: false,
  offset: 0,
  onAfterUpdate: function onAfterUpdate() {},
  onBeforeUpdate: function onBeforeUpdate() {},
  onCreate: function onCreate() {},
  onDestroy: function onDestroy() {},
  onHidden: function onHidden() {},
  onHide: function onHide() {},
  onMount: function onMount() {},
  onShow: function onShow() {},
  onShown: function onShown() {},
  onTrigger: function onTrigger() {},
  onUntrigger: function onUntrigger() {},
  placement: 'top',
  plugins: [],
  popperOptions: {},
  role: 'tooltip',
  showOnCreate: false,
  theme: '',
  touch: true,
  trigger: 'mouseenter focus',
  triggerTarget: null,
  updateDuration: 0,
  zIndex: 9999
}, pluginProps);
var defaultKeys = Object.keys(defaultProps);
/**
 * If the setProps() method encounters one of these, the popperInstance must be
 * recreated
 */

var POPPER_INSTANCE_DEPENDENCIES = ['arrow', 'boundary', 'distance', 'flip', 'flipBehavior', 'flipOnUpdate', 'offset', 'placement', 'popperOptions'];
/**
 * Mutates the defaultProps object by setting the props specified
 */

var setDefaultProps = function setDefaultProps(partialProps) {
  if (false) {}

  var keys = Object.keys(partialProps);
  keys.forEach(function (key) {
    defaultProps[key] = partialProps[key];
  });
};
/**
 * Returns an extended props object including plugin props
 */

function getExtendedPassedProps(passedProps) {
  var plugins = passedProps.plugins || [];
  var pluginProps = plugins.reduce(function (acc, plugin) {
    var name = plugin.name,
        defaultValue = plugin.defaultValue;

    if (name) {
      acc[name] = passedProps[name] !== undefined ? passedProps[name] : defaultValue;
    }

    return acc;
  }, {});
  return _extends({}, passedProps, {}, pluginProps);
}
/**
 * Returns an object of optional props from data-tippy-* attributes
 */

function getDataAttributeProps(reference, plugins) {
  var propKeys = plugins ? Object.keys(getExtendedPassedProps(_extends({}, defaultProps, {
    plugins: plugins
  }))) : defaultKeys;
  var props = propKeys.reduce(function (acc, key) {
    var valueAsString = (reference.getAttribute("data-tippy-" + key) || '').trim();

    if (!valueAsString) {
      return acc;
    }

    if (key === 'content') {
      acc[key] = valueAsString;
    } else {
      try {
        acc[key] = JSON.parse(valueAsString);
      } catch (e) {
        acc[key] = valueAsString;
      }
    }

    return acc;
  }, {});
  return props;
}
/**
 * Evaluates the props object by merging data attributes and disabling
 * conflicting props where necessary
 */

function evaluateProps(reference, props) {
  var out = _extends({}, props, {
    content: invokeWithArgsOrReturn(props.content, [reference])
  }, props.ignoreAttributes ? {} : getDataAttributeProps(reference, props.plugins));

  if (out.interactive) {
    out.aria = null;
  }

  return out;
}
/**
 * Validates props with the valid `defaultProps` object
 */

function validateProps(partialProps, plugins) {
  if (partialProps === void 0) {
    partialProps = {};
  }

  if (plugins === void 0) {
    plugins = [];
  }

  var keys = Object.keys(partialProps);
  keys.forEach(function (prop) {
    var value = partialProps[prop];
    var didSpecifyPlacementInPopperOptions = prop === 'popperOptions' && value !== null && typeof value === 'object' && tippy_chunk_esm_hasOwnProperty(value, 'placement');
    var nonPluginProps = removeProperties(defaultProps, ['animateFill', 'followCursor', 'inlinePositioning', 'sticky']); // These props have custom warnings

    var customWarningProps = ['a11y', 'arrowType', 'showOnInit', 'size', 'target', 'touchHold'];
    var didPassUnknownProp = !tippy_chunk_esm_hasOwnProperty(nonPluginProps, prop) && !includes(customWarningProps, prop); // Check if the prop exists in `plugins`

    if (didPassUnknownProp) {
      didPassUnknownProp = plugins.filter(function (plugin) {
        return plugin.name === prop;
      }).length === 0;
    }

    warnWhen(prop === 'target', ['The `target` prop was removed in v5 and replaced with the delegate() addon', 'in order to conserve bundle size.', 'See: https://atomiks.github.io/tippyjs/addons/#event-delegation'].join(' '));
    warnWhen(prop === 'a11y', ['The `a11y` prop was removed in v5. Make sure the element you are giving a', 'tippy to is natively focusable, such as <button> or <input>, not <div>', 'or <span>.'].join(' '));
    warnWhen(prop === 'showOnInit', 'The `showOnInit` prop was renamed to `showOnCreate` in v5.');
    warnWhen(prop === 'arrowType', ['The `arrowType` prop was removed in v5 in favor of overloading the `arrow`', 'prop.', '\n\n', '"round" string was replaced with importing the string from the package.', '\n\n', "* import {roundArrow} from 'tippy.js'; (ESM version)\n", '* const {roundArrow} = tippy; (IIFE CDN version)', '\n\n', 'Before: {arrow: true, arrowType: "round"}\n', 'After: {arrow: roundArrow}`'].join(' '));
    warnWhen(prop === 'touchHold', ['The `touchHold` prop was removed in v5 in favor of overloading the `touch`', 'prop.', '\n\n', 'Before: {touchHold: true}\n', 'After: {touch: "hold"}'].join(' '));
    warnWhen(prop === 'size', ['The `size` prop was removed in v5. Instead, use a theme that specifies', 'CSS padding and font-size properties.'].join(' '));
    warnWhen(prop === 'theme' && value === 'google', 'The included theme "google" was renamed to "material" in v5.');
    warnWhen(didSpecifyPlacementInPopperOptions, ['Specifying placement in `popperOptions` is not supported. Use the base-level', '`placement` prop instead.', '\n\n', 'Before: {popperOptions: {placement: "bottom"}}\n', 'After: {placement: "bottom"}'].join(' '));
    warnWhen(didPassUnknownProp, ["`" + prop + "`", "is not a valid prop. You may have spelled it incorrectly, or if it's a", 'plugin, forgot to pass it in an array as props.plugins.', '\n\n', 'In v5, the following props were turned into plugins:', '\n\n', '* animateFill\n', '* followCursor\n', '* sticky', '\n\n', 'All props: https://atomiks.github.io/tippyjs/all-props/\n', 'Plugins: https://atomiks.github.io/tippyjs/plugins/'].join(' '));
  });
}

var PASSIVE = {
  passive: true
};
var ROUND_ARROW = '<svg viewBox="0 0 18 7" xmlns="http://www.w3.org/2000/svg"><path d="M0 7s2.021-.015 5.253-4.218C6.584 1.051 7.797.007 9 0c1.203-.007 2.416 1.035 3.761 2.782C16.012 7.005 18 7 18 7H0z"/></svg>';
var IOS_CLASS = "tippy-iOS";
var POPPER_CLASS = "tippy-popper";
var TOOLTIP_CLASS = "tippy-tooltip";
var CONTENT_CLASS = "tippy-content";
var BACKDROP_CLASS = "tippy-backdrop";
var ARROW_CLASS = "tippy-arrow";
var SVG_ARROW_CLASS = "tippy-svg-arrow";
var POPPER_SELECTOR = "." + POPPER_CLASS;
var TOOLTIP_SELECTOR = "." + TOOLTIP_CLASS;
var CONTENT_SELECTOR = "." + CONTENT_CLASS;
var ARROW_SELECTOR = "." + ARROW_CLASS;
var SVG_ARROW_SELECTOR = "." + SVG_ARROW_CLASS;

var currentInput = {
  isTouch: false
};
var lastMouseMoveTime = 0;
/**
 * When a `touchstart` event is fired, it's assumed the user is using touch
 * input. We'll bind a `mousemove` event listener to listen for mouse input in
 * the future. This way, the `isTouch` property is fully dynamic and will handle
 * hybrid devices that use a mix of touch + mouse input.
 */

function onDocumentTouchStart() {
  if (currentInput.isTouch) {
    return;
  }

  currentInput.isTouch = true;

  if (window.performance) {
    document.addEventListener('mousemove', onDocumentMouseMove);
  }
}
/**
 * When two `mousemove` event are fired consecutively within 20ms, it's assumed
 * the user is using mouse input again. `mousemove` can fire on touch devices as
 * well, but very rarely that quickly.
 */

function onDocumentMouseMove() {
  var now = performance.now();

  if (now - lastMouseMoveTime < 20) {
    currentInput.isTouch = false;
    document.removeEventListener('mousemove', onDocumentMouseMove);
  }

  lastMouseMoveTime = now;
}
/**
 * When an element is in focus and has a tippy, leaving the tab/window and
 * returning causes it to show again. For mouse users this is unexpected, but
 * for keyboard use it makes sense.
 * TODO: find a better technique to solve this problem
 */

function onWindowBlur() {
  var activeElement = document.activeElement;

  if (isReferenceElement(activeElement)) {
    var instance = activeElement._tippy;

    if (activeElement.blur && !instance.state.isVisible) {
      activeElement.blur();
    }
  }
}
/**
 * Adds the needed global event listeners
 */

function bindGlobalEventListeners() {
  document.addEventListener('touchstart', onDocumentTouchStart, _extends({}, PASSIVE, {
    capture: true
  }));
  window.addEventListener('blur', onWindowBlur);
}

var isBrowser = typeof window !== 'undefined' && typeof document !== 'undefined';
var ua = isBrowser ? navigator.userAgent : '';
var isIE = /MSIE |Trident\//.test(ua);
var isIOS = isBrowser && /iPhone|iPad|iPod/.test(navigator.platform);
function updateIOSClass(isAdd) {
  var shouldAdd = isAdd && isIOS && currentInput.isTouch;
  document.body.classList[shouldAdd ? 'add' : 'remove'](IOS_CLASS);
}

/**
 * Returns the popper's placement, ignoring shifting (top-start, etc)
 */

function getBasePlacement(placement) {
  return placement.split('-')[0];
}
/**
 * Adds `data-inertia` attribute
 */

function addInertia(tooltip) {
  tooltip.setAttribute('data-inertia', '');
}
/**
 * Removes `data-inertia` attribute
 */

function removeInertia(tooltip) {
  tooltip.removeAttribute('data-inertia');
}
/**
 * Adds interactive-related attributes
 */

function addInteractive(tooltip) {
  tooltip.setAttribute('data-interactive', '');
}
/**
 * Removes interactive-related attributes
 */

function removeInteractive(tooltip) {
  tooltip.removeAttribute('data-interactive');
}
/**
 * Sets the content of a tooltip
 */

function tippy_chunk_esm_setContent(contentEl, props) {
  if (isElement(props.content)) {
    setInnerHTML(contentEl, '');
    contentEl.appendChild(props.content);
  } else if (typeof props.content !== 'function') {
    var key = props.allowHTML ? 'innerHTML' : 'textContent';
    contentEl[key] = props.content;
  }
}
/**
 * Returns the child elements of a popper element
 */

function getChildren(popper) {
  return {
    tooltip: popper.querySelector(TOOLTIP_SELECTOR),
    content: popper.querySelector(CONTENT_SELECTOR),
    arrow: popper.querySelector(ARROW_SELECTOR) || popper.querySelector(SVG_ARROW_SELECTOR)
  };
}
/**
 * Creates an arrow element and returns it
 */

function createArrowElement(arrow) {
  var arrowElement = div();

  if (arrow === true) {
    arrowElement.className = ARROW_CLASS;
  } else {
    arrowElement.className = SVG_ARROW_CLASS;

    if (isElement(arrow)) {
      arrowElement.appendChild(arrow);
    } else {
      setInnerHTML(arrowElement, arrow);
    }
  }

  return arrowElement;
}
/**
 * Constructs the popper element and returns it
 */

function createPopperElement(id, props) {
  var popper = div();
  popper.className = POPPER_CLASS;
  popper.style.position = 'absolute';
  popper.style.top = '0';
  popper.style.left = '0';
  var tooltip = div();
  tooltip.className = TOOLTIP_CLASS;
  tooltip.id = "tippy-" + id;
  tooltip.setAttribute('data-state', 'hidden');
  tooltip.setAttribute('tabindex', '-1');
  updateTheme(tooltip, 'add', props.theme);
  var content = div();
  content.className = CONTENT_CLASS;
  content.setAttribute('data-state', 'hidden');

  if (props.interactive) {
    addInteractive(tooltip);
  }

  if (props.arrow) {
    tooltip.setAttribute('data-arrow', '');
    tooltip.appendChild(createArrowElement(props.arrow));
  }

  if (props.inertia) {
    addInertia(tooltip);
  }

  tippy_chunk_esm_setContent(content, props);
  tooltip.appendChild(content);
  popper.appendChild(tooltip);
  updatePopperElement(popper, props, props);
  return popper;
}
/**
 * Updates the popper element based on the new props
 */

function updatePopperElement(popper, prevProps, nextProps) {
  var _getChildren = getChildren(popper),
      tooltip = _getChildren.tooltip,
      content = _getChildren.content,
      arrow = _getChildren.arrow;

  popper.style.zIndex = '' + nextProps.zIndex;
  tooltip.setAttribute('data-animation', nextProps.animation);
  tooltip.style.maxWidth = appendPxIfNumber(nextProps.maxWidth);

  if (nextProps.role) {
    tooltip.setAttribute('role', nextProps.role);
  } else {
    tooltip.removeAttribute('role');
  }

  if (prevProps.content !== nextProps.content) {
    tippy_chunk_esm_setContent(content, nextProps);
  } // arrow


  if (!prevProps.arrow && nextProps.arrow) {
    // false to true
    tooltip.appendChild(createArrowElement(nextProps.arrow));
    tooltip.setAttribute('data-arrow', '');
  } else if (prevProps.arrow && !nextProps.arrow) {
    // true to false
    tooltip.removeChild(arrow);
    tooltip.removeAttribute('data-arrow');
  } else if (prevProps.arrow !== nextProps.arrow) {
    // true to 'round' or vice-versa
    tooltip.removeChild(arrow);
    tooltip.appendChild(createArrowElement(nextProps.arrow));
  } // interactive


  if (!prevProps.interactive && nextProps.interactive) {
    addInteractive(tooltip);
  } else if (prevProps.interactive && !nextProps.interactive) {
    removeInteractive(tooltip);
  } // inertia


  if (!prevProps.inertia && nextProps.inertia) {
    addInertia(tooltip);
  } else if (prevProps.inertia && !nextProps.inertia) {
    removeInertia(tooltip);
  } // theme


  if (prevProps.theme !== nextProps.theme) {
    updateTheme(tooltip, 'remove', prevProps.theme);
    updateTheme(tooltip, 'add', nextProps.theme);
  }
}
/**
 * Add/remove transitionend listener from tooltip
 */

function updateTransitionEndListener(tooltip, action, listener) {
  ['transitionend', 'webkitTransitionEnd'].forEach(function (event) {
    tooltip[action + 'EventListener'](event, listener);
  });
}
/**
 * Adds/removes theme from tooltip's classList
 */

function updateTheme(tooltip, action, theme) {
  splitBySpaces(theme).forEach(function (name) {
    tooltip.classList[action](name + "-theme");
  });
}
/**
 * Determines if the mouse cursor is outside of the popper's interactive border
 * region
 */

function isCursorOutsideInteractiveBorder(popperTreeData, event) {
  var clientX = event.clientX,
      clientY = event.clientY;
  return popperTreeData.every(function (_ref) {
    var popperRect = _ref.popperRect,
        tooltipRect = _ref.tooltipRect,
        interactiveBorder = _ref.interactiveBorder;
    // Get min/max bounds of both the popper and tooltip rects due to
    // `distance` offset
    var mergedRect = {
      top: Math.min(popperRect.top, tooltipRect.top),
      right: Math.max(popperRect.right, tooltipRect.right),
      bottom: Math.max(popperRect.bottom, tooltipRect.bottom),
      left: Math.min(popperRect.left, tooltipRect.left)
    };
    var exceedsTop = mergedRect.top - clientY > interactiveBorder;
    var exceedsBottom = clientY - mergedRect.bottom > interactiveBorder;
    var exceedsLeft = mergedRect.left - clientX > interactiveBorder;
    var exceedsRight = clientX - mergedRect.right > interactiveBorder;
    return exceedsTop || exceedsBottom || exceedsLeft || exceedsRight;
  });
}

var idCounter = 1;
var mouseMoveListeners = [];
/**
 * Used by `hideAll()`
 */

var mountedInstances = [];
/**
 * Creates and returns a Tippy object. We're using a closure pattern instead of
 * a class so that the exposed object API is clean without private members
 * prefixed with `_`.
 */

function createTippy(reference, passedProps) {
  var props = evaluateProps(reference, _extends({}, defaultProps, {}, getExtendedPassedProps(passedProps))); // If the reference shouldn't have multiple tippys, return null early

  if (!props.multiple && reference._tippy) {
    return null;
  }
  /* ======================= 🔒 Private members 🔒 ======================= */


  var showTimeout;
  var hideTimeout;
  var scheduleHideAnimationFrame;
  var isBeingDestroyed = false;
  var isVisibleFromClick = false;
  var didHideDueToDocumentMouseDown = false;
  var popperUpdates = 0;
  var lastTriggerEvent;
  var currentMountCallback;
  var currentTransitionEndListener;
  var listeners = [];
  var debouncedOnMouseMove = debounce(onMouseMove, props.interactiveDebounce);
  var currentTarget; // Support iframe contexts
  // Static check that assumes any of the `triggerTarget` or `reference`
  // nodes will never change documents, even when they are updated

  var doc = getOwnerDocument(props.triggerTarget || reference);
  /* ======================= 🔑 Public members 🔑 ======================= */

  var id = idCounter++;
  var popper = createPopperElement(id, props);
  var popperChildren = getChildren(popper);
  var popperInstance = null;
  var plugins = unique(props.plugins); // These two elements are static

  var tooltip = popperChildren.tooltip,
      content = popperChildren.content;
  var transitionableElements = [tooltip, content];
  var state = {
    // The current real placement (`data-placement` attribute)
    currentPlacement: null,
    // Is the instance currently enabled?
    isEnabled: true,
    // Is the tippy currently showing and not transitioning out?
    isVisible: false,
    // Has the instance been destroyed?
    isDestroyed: false,
    // Is the tippy currently mounted to the DOM?
    isMounted: false,
    // Has the tippy finished transitioning in?
    isShown: false
  };
  var instance = {
    // properties
    id: id,
    reference: reference,
    popper: popper,
    popperChildren: popperChildren,
    popperInstance: popperInstance,
    props: props,
    state: state,
    plugins: plugins,
    // methods
    clearDelayTimeouts: clearDelayTimeouts,
    setProps: setProps,
    setContent: setContent,
    show: show,
    hide: hide,
    enable: enable,
    disable: disable,
    destroy: destroy
  };
  /* ==================== Initial instance mutations =================== */

  reference._tippy = instance;
  popper._tippy = instance;
  var pluginsHooks = plugins.map(function (plugin) {
    return plugin.fn(instance);
  });
  var hadAriaExpandedAttributeOnCreate = reference.hasAttribute('aria-expanded');
  addListenersToTriggerTarget();
  handleAriaExpandedAttribute();

  if (!props.lazy) {
    createPopperInstance();
  }

  invokeHook('onCreate', [instance]);

  if (props.showOnCreate) {
    scheduleShow();
  } // Prevent a tippy with a delay from hiding if the cursor left then returned
  // before it started hiding


  popper.addEventListener('mouseenter', function () {
    if (instance.props.interactive && instance.state.isVisible) {
      instance.clearDelayTimeouts();
    }
  });
  popper.addEventListener('mouseleave', function (event) {
    if (instance.props.interactive && includes(instance.props.trigger, 'mouseenter')) {
      debouncedOnMouseMove(event);
      doc.addEventListener('mousemove', debouncedOnMouseMove);
    }
  });
  return instance;
  /* ======================= 🔒 Private methods 🔒 ======================= */

  function getNormalizedTouchSettings() {
    var touch = instance.props.touch;
    return Array.isArray(touch) ? touch : [touch, 0];
  }

  function getIsCustomTouchBehavior() {
    return getNormalizedTouchSettings()[0] === 'hold';
  }

  function getCurrentTarget() {
    return currentTarget || reference;
  }

  function getDelay(isShow) {
    // For touch or keyboard input, force `0` delay for UX reasons
    // Also if the instance is mounted but not visible (transitioning out),
    // ignore delay
    if (instance.state.isMounted && !instance.state.isVisible || currentInput.isTouch || lastTriggerEvent && lastTriggerEvent.type === 'focus') {
      return 0;
    }

    return getValueAtIndexOrReturn(instance.props.delay, isShow ? 0 : 1, defaultProps.delay);
  }

  function invokeHook(hook, args, shouldInvokePropsHook) {
    if (shouldInvokePropsHook === void 0) {
      shouldInvokePropsHook = true;
    }

    pluginsHooks.forEach(function (pluginHooks) {
      if (tippy_chunk_esm_hasOwnProperty(pluginHooks, hook)) {
        // @ts-ignore
        pluginHooks[hook].apply(pluginHooks, args);
      }
    });

    if (shouldInvokePropsHook) {
      var _instance$props;

      // @ts-ignore
      (_instance$props = instance.props)[hook].apply(_instance$props, args);
    }
  }

  function handleAriaDescribedByAttribute() {
    var aria = instance.props.aria;

    if (!aria) {
      return;
    }

    var attr = "aria-" + aria;
    var id = tooltip.id;
    var nodes = normalizeToArray(instance.props.triggerTarget || reference);
    nodes.forEach(function (node) {
      var currentValue = node.getAttribute(attr);

      if (instance.state.isVisible) {
        node.setAttribute(attr, currentValue ? currentValue + " " + id : id);
      } else {
        var nextValue = currentValue && currentValue.replace(id, '').trim();

        if (nextValue) {
          node.setAttribute(attr, nextValue);
        } else {
          node.removeAttribute(attr);
        }
      }
    });
  }

  function handleAriaExpandedAttribute() {
    // If the user has specified `aria-expanded` on their reference when the
    // instance was created, we have to assume they're controlling it externally
    // themselves
    if (hadAriaExpandedAttributeOnCreate) {
      return;
    }

    var nodes = normalizeToArray(instance.props.triggerTarget || reference);
    nodes.forEach(function (node) {
      if (instance.props.interactive) {
        node.setAttribute('aria-expanded', instance.state.isVisible && node === getCurrentTarget() ? 'true' : 'false');
      } else {
        node.removeAttribute('aria-expanded');
      }
    });
  }

  function cleanupInteractiveMouseListeners() {
    doc.body.removeEventListener('mouseleave', scheduleHide);
    doc.removeEventListener('mousemove', debouncedOnMouseMove);
    mouseMoveListeners = mouseMoveListeners.filter(function (listener) {
      return listener !== debouncedOnMouseMove;
    });
  }

  function onDocumentMouseDown(event) {
    // Clicked on interactive popper
    if (instance.props.interactive && popper.contains(event.target)) {
      return;
    } // Clicked on the event listeners target


    if (getCurrentTarget().contains(event.target)) {
      if (currentInput.isTouch) {
        return;
      }

      if (instance.state.isVisible && includes(instance.props.trigger, 'click')) {
        return;
      }
    }

    if (instance.props.hideOnClick === true) {
      isVisibleFromClick = false;
      instance.clearDelayTimeouts();
      instance.hide(); // `mousedown` event is fired right before `focus` if pressing the
      // currentTarget. This lets a tippy with `focus` trigger know that it
      // should not show

      didHideDueToDocumentMouseDown = true;
      setTimeout(function () {
        didHideDueToDocumentMouseDown = false;
      }); // The listener gets added in `scheduleShow()`, but this may be hiding it
      // before it shows, and hide()'s early bail-out behavior can prevent it
      // from being cleaned up

      if (!instance.state.isMounted) {
        removeDocumentMouseDownListener();
      }
    }
  }

  function addDocumentMouseDownListener() {
    doc.addEventListener('mousedown', onDocumentMouseDown, true);
  }

  function removeDocumentMouseDownListener() {
    doc.removeEventListener('mousedown', onDocumentMouseDown, true);
  }

  function onTransitionedOut(duration, callback) {
    onTransitionEnd(duration, function () {
      if (!instance.state.isVisible && popper.parentNode && popper.parentNode.contains(popper)) {
        callback();
      }
    });
  }

  function onTransitionedIn(duration, callback) {
    onTransitionEnd(duration, callback);
  }

  function onTransitionEnd(duration, callback) {
    function listener(event) {
      if (event.target === tooltip) {
        updateTransitionEndListener(tooltip, 'remove', listener);
        callback();
      }
    } // Make callback synchronous if duration is 0
    // `transitionend` won't fire otherwise


    if (duration === 0) {
      return callback();
    }

    updateTransitionEndListener(tooltip, 'remove', currentTransitionEndListener);
    updateTransitionEndListener(tooltip, 'add', listener);
    currentTransitionEndListener = listener;
  }

  function on(eventType, handler, options) {
    if (options === void 0) {
      options = false;
    }

    var nodes = normalizeToArray(instance.props.triggerTarget || reference);
    nodes.forEach(function (node) {
      node.addEventListener(eventType, handler, options);
      listeners.push({
        node: node,
        eventType: eventType,
        handler: handler,
        options: options
      });
    });
  }

  function addListenersToTriggerTarget() {
    if (getIsCustomTouchBehavior()) {
      on('touchstart', onTrigger, PASSIVE);
      on('touchend', onMouseLeave, PASSIVE);
    }

    splitBySpaces(instance.props.trigger).forEach(function (eventType) {
      if (eventType === 'manual') {
        return;
      }

      on(eventType, onTrigger);

      switch (eventType) {
        case 'mouseenter':
          on('mouseleave', onMouseLeave);
          break;

        case 'focus':
          on(isIE ? 'focusout' : 'blur', onBlurOrFocusOut);
          break;

        case 'focusin':
          on('focusout', onBlurOrFocusOut);
          break;
      }
    });
  }

  function removeListenersFromTriggerTarget() {
    listeners.forEach(function (_ref) {
      var node = _ref.node,
          eventType = _ref.eventType,
          handler = _ref.handler,
          options = _ref.options;
      node.removeEventListener(eventType, handler, options);
    });
    listeners = [];
  }

  function onTrigger(event) {
    var shouldScheduleClickHide = false;

    if (!instance.state.isEnabled || isEventListenerStopped(event) || didHideDueToDocumentMouseDown) {
      return;
    }

    lastTriggerEvent = event;
    currentTarget = event.currentTarget;
    handleAriaExpandedAttribute();

    if (!instance.state.isVisible && isMouseEvent(event)) {
      // If scrolling, `mouseenter` events can be fired if the cursor lands
      // over a new target, but `mousemove` events don't get fired. This
      // causes interactive tooltips to get stuck open until the cursor is
      // moved
      mouseMoveListeners.forEach(function (listener) {
        return listener(event);
      });
    } // Toggle show/hide when clicking click-triggered tooltips


    if (event.type === 'click' && (!includes(instance.props.trigger, 'mouseenter') || isVisibleFromClick) && instance.props.hideOnClick !== false && instance.state.isVisible) {
      shouldScheduleClickHide = true;
    } else {
      var _getNormalizedTouchSe = getNormalizedTouchSettings(),
          value = _getNormalizedTouchSe[0],
          duration = _getNormalizedTouchSe[1];

      if (currentInput.isTouch && value === 'hold' && duration) {
        // We can hijack the show timeout here, it will be cleared by
        // `scheduleHide()` when necessary
        showTimeout = setTimeout(function () {
          scheduleShow(event);
        }, duration);
      } else {
        scheduleShow(event);
      }
    }

    if (event.type === 'click') {
      isVisibleFromClick = !shouldScheduleClickHide;
    }

    if (shouldScheduleClickHide) {
      scheduleHide(event);
    }
  }

  function onMouseMove(event) {
    var isCursorOverReferenceOrPopper = closestCallback(event.target, function (el) {
      return el === reference || el === popper;
    });

    if (event.type === 'mousemove' && isCursorOverReferenceOrPopper) {
      return;
    }

    var popperTreeData = arrayFrom(popper.querySelectorAll(POPPER_SELECTOR)).concat(popper).map(function (popper) {
      var instance = popper._tippy;
      var tooltip = instance.popperChildren.tooltip;
      var interactiveBorder = instance.props.interactiveBorder;
      return {
        popperRect: popper.getBoundingClientRect(),
        tooltipRect: tooltip.getBoundingClientRect(),
        interactiveBorder: interactiveBorder
      };
    });

    if (isCursorOutsideInteractiveBorder(popperTreeData, event)) {
      cleanupInteractiveMouseListeners();
      scheduleHide(event);
    }
  }

  function onMouseLeave(event) {
    if (isEventListenerStopped(event)) {
      return;
    }

    if (includes(instance.props.trigger, 'click') && isVisibleFromClick) {
      return;
    }

    if (instance.props.interactive) {
      doc.body.addEventListener('mouseleave', scheduleHide);
      doc.addEventListener('mousemove', debouncedOnMouseMove);
      pushIfUnique(mouseMoveListeners, debouncedOnMouseMove);
      debouncedOnMouseMove(event);
      return;
    }

    scheduleHide(event);
  }

  function onBlurOrFocusOut(event) {
    if (!includes(instance.props.trigger, 'focusin') && event.target !== getCurrentTarget()) {
      return;
    } // If focus was moved to within the popper


    if (instance.props.interactive && event.relatedTarget && popper.contains(event.relatedTarget)) {
      return;
    }

    scheduleHide(event);
  }

  function isEventListenerStopped(event) {
    var supportsTouch = 'ontouchstart' in window;
    var isTouchEvent = includes(event.type, 'touch');
    var isCustomTouch = getIsCustomTouchBehavior();
    return supportsTouch && currentInput.isTouch && isCustomTouch && !isTouchEvent || currentInput.isTouch && !isCustomTouch && isTouchEvent;
  }

  function createPopperInstance() {
    var popperOptions = instance.props.popperOptions;
    var arrow = instance.popperChildren.arrow;
    var flipModifier = getModifier(popperOptions, 'flip');
    var preventOverflowModifier = getModifier(popperOptions, 'preventOverflow');
    var distancePx;

    function applyMutations(data) {
      var prevPlacement = instance.state.currentPlacement;
      instance.state.currentPlacement = data.placement;

      if (instance.props.flip && !instance.props.flipOnUpdate) {
        if (data.flipped) {
          instance.popperInstance.options.placement = data.placement;
        }

        setModifierValue(instance.popperInstance.modifiers, 'flip', 'enabled', false);
      }

      tooltip.setAttribute('data-placement', data.placement);

      if (data.attributes['x-out-of-boundaries'] !== false) {
        tooltip.setAttribute('data-out-of-boundaries', '');
      } else {
        tooltip.removeAttribute('data-out-of-boundaries');
      }

      var basePlacement = getBasePlacement(data.placement);
      var isVerticalPlacement = includes(['top', 'bottom'], basePlacement);
      var isSecondaryPlacement = includes(['bottom', 'right'], basePlacement); // Apply `distance` prop

      tooltip.style.top = '0';
      tooltip.style.left = '0';
      tooltip.style[isVerticalPlacement ? 'top' : 'left'] = (isSecondaryPlacement ? 1 : -1) * distancePx + 'px'; // Careful not to cause an infinite loop here
      // Fixes https://github.com/FezVrasta/popper.js/issues/784

      if (prevPlacement && prevPlacement !== data.placement) {
        instance.popperInstance.update();
      }
    }

    var config = _extends({
      eventsEnabled: false,
      placement: instance.props.placement
    }, popperOptions, {
      modifiers: _extends({}, popperOptions && popperOptions.modifiers, {
        // We can't use `padding` on the popper el because of these bugs when
        // flipping from a vertical to horizontal placement or vice-versa,
        // there is severe flickering.
        // https://github.com/FezVrasta/popper.js/issues/720
        // This workaround increases bundle size by 250B minzip unfortunately,
        // due to need to custom compute the distance (since Popper rect does
        // not get affected by the inner tooltip's distance offset)
        tippyDistance: {
          enabled: true,
          order: 0,
          fn: function fn(data) {
            // `html` fontSize may change while `popperInstance` is alive
            // e.g. on resize in media queries
            distancePx = getUnitsInPx(doc, instance.props.distance);
            var basePlacement = getBasePlacement(data.placement);
            var computedPreventOverflowPadding = getComputedPadding(basePlacement, preventOverflowModifier && preventOverflowModifier.padding, distancePx);
            var computedFlipPadding = getComputedPadding(basePlacement, flipModifier && flipModifier.padding, distancePx);
            var instanceModifiers = instance.popperInstance.modifiers;
            setModifierValue(instanceModifiers, 'preventOverflow', 'padding', computedPreventOverflowPadding);
            setModifierValue(instanceModifiers, 'flip', 'padding', computedFlipPadding);
            return data;
          }
        },
        preventOverflow: _extends({
          boundariesElement: instance.props.boundary
        }, preventOverflowModifier),
        flip: _extends({
          enabled: instance.props.flip,
          behavior: instance.props.flipBehavior
        }, flipModifier),
        arrow: _extends({
          element: arrow,
          enabled: !!arrow
        }, getModifier(popperOptions, 'arrow')),
        offset: _extends({
          offset: instance.props.offset
        }, getModifier(popperOptions, 'offset'))
      }),
      onCreate: function onCreate(data) {
        applyMutations(data);
        preserveInvocation(popperOptions && popperOptions.onCreate, config.onCreate, [data]);
        runMountCallback();
      },
      onUpdate: function onUpdate(data) {
        applyMutations(data);
        preserveInvocation(popperOptions && popperOptions.onUpdate, config.onUpdate, [data]);
        runMountCallback();
      }
    });

    instance.popperInstance = new esm_popper["a" /* default */](reference, popper, config);
  }

  function runMountCallback() {
    // Only invoke currentMountCallback after 2 updates
    // This fixes some bugs in Popper.js (TODO: aim for only 1 update)
    if (popperUpdates === 0) {
      popperUpdates++; // 1

      instance.popperInstance.update();
    } else if (currentMountCallback && popperUpdates === 1) {
      popperUpdates++; // 2

      reflow(popper);
      currentMountCallback();
    }
  }

  function mount() {
    // The mounting callback (`currentMountCallback`) is only run due to a
    // popperInstance update/create
    popperUpdates = 0;
    var appendTo = instance.props.appendTo;
    var parentNode; // By default, we'll append the popper to the triggerTargets's parentNode so
    // it's directly after the reference element so the elements inside the
    // tippy can be tabbed to
    // If there are clipping issues, the user can specify a different appendTo
    // and ensure focus management is handled correctly manually

    var node = getCurrentTarget();

    if (instance.props.interactive && appendTo === defaultProps.appendTo || appendTo === 'parent') {
      parentNode = node.parentNode;
    } else {
      parentNode = invokeWithArgsOrReturn(appendTo, [node]);
    } // The popper element needs to exist on the DOM before its position can be
    // updated as Popper.js needs to read its dimensions


    if (!parentNode.contains(popper)) {
      parentNode.appendChild(popper);
    }

    if (false) {}

    setModifierValue(instance.popperInstance.modifiers, 'flip', 'enabled', instance.props.flip);
    instance.popperInstance.enableEventListeners(); // Mounting callback invoked in `onUpdate`

    instance.popperInstance.update();
  }

  function scheduleShow(event) {
    instance.clearDelayTimeouts();

    if (!instance.popperInstance) {
      createPopperInstance();
    }

    if (event) {
      invokeHook('onTrigger', [instance, event]);
    }

    addDocumentMouseDownListener();
    var delay = getDelay(true);

    if (delay) {
      showTimeout = setTimeout(function () {
        instance.show();
      }, delay);
    } else {
      instance.show();
    }
  }

  function scheduleHide(event) {
    instance.clearDelayTimeouts();
    invokeHook('onUntrigger', [instance, event]);

    if (!instance.state.isVisible) {
      removeDocumentMouseDownListener();
      return;
    } // For interactive tippies, scheduleHide is added to a document.body handler
    // from onMouseLeave so must intercept scheduled hides from mousemove/leave
    // events when trigger contains mouseenter and click, and the tip is
    // currently shown as a result of a click.


    if (includes(instance.props.trigger, 'mouseenter') && includes(instance.props.trigger, 'click') && includes(['mouseleave', 'mousemove'], event.type) && isVisibleFromClick) {
      return;
    }

    var delay = getDelay(false);

    if (delay) {
      hideTimeout = setTimeout(function () {
        if (instance.state.isVisible) {
          instance.hide();
        }
      }, delay);
    } else {
      // Fixes a `transitionend` problem when it fires 1 frame too
      // late sometimes, we don't want hide() to be called.
      scheduleHideAnimationFrame = requestAnimationFrame(function () {
        instance.hide();
      });
    }
  }
  /* ======================= 🔑 Public methods 🔑 ======================= */


  function enable() {
    instance.state.isEnabled = true;
  }

  function disable() {
    // Disabling the instance should also hide it
    // https://github.com/atomiks/tippy.js-react/issues/106
    instance.hide();
    instance.state.isEnabled = false;
  }

  function clearDelayTimeouts() {
    clearTimeout(showTimeout);
    clearTimeout(hideTimeout);
    cancelAnimationFrame(scheduleHideAnimationFrame);
  }

  function setProps(partialProps) {
    if (false) {}

    if (instance.state.isDestroyed) {
      return;
    }

    if (false) {}

    invokeHook('onBeforeUpdate', [instance, partialProps]);
    removeListenersFromTriggerTarget();
    var prevProps = instance.props;
    var nextProps = evaluateProps(reference, _extends({}, instance.props, {}, partialProps, {
      ignoreAttributes: true
    }));
    nextProps.ignoreAttributes = useIfDefined(partialProps.ignoreAttributes, prevProps.ignoreAttributes);
    instance.props = nextProps;
    addListenersToTriggerTarget();

    if (prevProps.interactiveDebounce !== nextProps.interactiveDebounce) {
      cleanupInteractiveMouseListeners();
      debouncedOnMouseMove = debounce(onMouseMove, nextProps.interactiveDebounce);
    }

    updatePopperElement(popper, prevProps, nextProps);
    instance.popperChildren = getChildren(popper); // Ensure stale aria-expanded attributes are removed

    if (prevProps.triggerTarget && !nextProps.triggerTarget) {
      normalizeToArray(prevProps.triggerTarget).forEach(function (node) {
        node.removeAttribute('aria-expanded');
      });
    } else if (nextProps.triggerTarget) {
      reference.removeAttribute('aria-expanded');
    }

    handleAriaExpandedAttribute();

    if (instance.popperInstance) {
      if (POPPER_INSTANCE_DEPENDENCIES.some(function (prop) {
        return tippy_chunk_esm_hasOwnProperty(partialProps, prop) && partialProps[prop] !== prevProps[prop];
      })) {
        var currentReference = instance.popperInstance.reference;
        instance.popperInstance.destroy();
        createPopperInstance();
        instance.popperInstance.reference = currentReference;

        if (instance.state.isVisible) {
          instance.popperInstance.enableEventListeners();
        }
      } else {
        instance.popperInstance.update();
      }
    }

    invokeHook('onAfterUpdate', [instance, partialProps]);
  }

  function setContent(content) {
    instance.setProps({
      content: content
    });
  }

  function show(duration) {
    if (duration === void 0) {
      duration = getValueAtIndexOrReturn(instance.props.duration, 0, defaultProps.duration);
    }

    if (false) {} // Early bail-out


    var isAlreadyVisible = instance.state.isVisible;
    var isDestroyed = instance.state.isDestroyed;
    var isDisabled = !instance.state.isEnabled;
    var isTouchAndTouchDisabled = currentInput.isTouch && !instance.props.touch;

    if (isAlreadyVisible || isDestroyed || isDisabled || isTouchAndTouchDisabled) {
      return;
    } // Normalize `disabled` behavior across browsers.
    // Firefox allows events on disabled elements, but Chrome doesn't.
    // Using a wrapper element (i.e. <span>) is recommended.


    if (getCurrentTarget().hasAttribute('disabled')) {
      return;
    }

    if (!instance.popperInstance) {
      createPopperInstance();
    }

    invokeHook('onShow', [instance], false);

    if (instance.props.onShow(instance) === false) {
      return;
    }

    addDocumentMouseDownListener();
    popper.style.visibility = 'visible';
    instance.state.isVisible = true; // Prevent a transition of the popper from its previous position and of the
    // elements at a different placement
    // Check if the tippy was fully unmounted before `show()` was called, to
    // allow for smooth transition for `createSingleton()`

    if (!instance.state.isMounted) {
      setTransitionDuration(transitionableElements.concat(popper), 0);
    }

    currentMountCallback = function currentMountCallback() {
      if (!instance.state.isVisible) {
        return;
      }

      setTransitionDuration([popper], instance.props.updateDuration);
      setTransitionDuration(transitionableElements, duration);
      setVisibilityState(transitionableElements, 'visible');
      handleAriaDescribedByAttribute();
      handleAriaExpandedAttribute();
      pushIfUnique(mountedInstances, instance);
      updateIOSClass(true);
      instance.state.isMounted = true;
      invokeHook('onMount', [instance]);
      onTransitionedIn(duration, function () {
        instance.state.isShown = true;
        invokeHook('onShown', [instance]);
      });
    };

    mount();
  }

  function hide(duration) {
    if (duration === void 0) {
      duration = getValueAtIndexOrReturn(instance.props.duration, 1, defaultProps.duration);
    }

    if (false) {} // Early bail-out


    var isAlreadyHidden = !instance.state.isVisible && !isBeingDestroyed;
    var isDestroyed = instance.state.isDestroyed;
    var isDisabled = !instance.state.isEnabled && !isBeingDestroyed;

    if (isAlreadyHidden || isDestroyed || isDisabled) {
      return;
    }

    invokeHook('onHide', [instance], false);

    if (instance.props.onHide(instance) === false && !isBeingDestroyed) {
      return;
    }

    removeDocumentMouseDownListener();
    popper.style.visibility = 'hidden';
    instance.state.isVisible = false;
    instance.state.isShown = false;
    setTransitionDuration(transitionableElements, duration);
    setVisibilityState(transitionableElements, 'hidden');
    handleAriaDescribedByAttribute();
    handleAriaExpandedAttribute();
    onTransitionedOut(duration, function () {
      instance.popperInstance.disableEventListeners();
      instance.popperInstance.options.placement = instance.props.placement;
      popper.parentNode.removeChild(popper);
      mountedInstances = mountedInstances.filter(function (i) {
        return i !== instance;
      });

      if (mountedInstances.length === 0) {
        updateIOSClass(false);
      }

      instance.state.isMounted = false;
      invokeHook('onHidden', [instance]);
    });
  }

  function destroy() {
    if (false) {}

    if (instance.state.isDestroyed) {
      return;
    }

    isBeingDestroyed = true;
    instance.clearDelayTimeouts();
    instance.hide(0);
    removeListenersFromTriggerTarget();
    delete reference._tippy;

    if (instance.popperInstance) {
      instance.popperInstance.destroy();
    }

    isBeingDestroyed = false;
    instance.state.isDestroyed = true;
    invokeHook('onDestroy', [instance]);
  }
}

function tippy(targets, optionalProps,
/** @deprecated use Props.plugins */
plugins) {
  if (optionalProps === void 0) {
    optionalProps = {};
  }

  if (plugins === void 0) {
    plugins = [];
  }

  plugins = defaultProps.plugins.concat(optionalProps.plugins || plugins);

  if (false) {}

  bindGlobalEventListeners();

  var passedProps = _extends({}, optionalProps, {
    plugins: plugins
  });

  var elements = getArrayOfElements(targets);

  if (false) { var isMoreThanOneReferenceElement, isSingleContentElement; }

  var instances = elements.reduce(function (acc, reference) {
    var instance = reference && createTippy(reference, passedProps);

    if (instance) {
      acc.push(instance);
    }

    return acc;
  }, []);
  return isElement(targets) ? instances[0] : instances;
}

tippy.version = version;
tippy.defaultProps = defaultProps;
tippy.setDefaultProps = setDefaultProps;
tippy.currentInput = currentInput;
/**
 * Hides all visible poppers on the document
 */

var hideAll = function hideAll(_temp) {
  var _ref = _temp === void 0 ? {} : _temp,
      excludedReferenceOrInstance = _ref.exclude,
      duration = _ref.duration;

  mountedInstances.forEach(function (instance) {
    var isExcluded = false;

    if (excludedReferenceOrInstance) {
      isExcluded = isReferenceElement(excludedReferenceOrInstance) ? instance.reference === excludedReferenceOrInstance : instance.popper === excludedReferenceOrInstance.popper;
    }

    if (!isExcluded) {
      instance.hide(duration);
    }
  });
};
/**
 * Returns a proxy wrapper function that passes the plugins
 * @deprecated use tippy.setDefaultProps({plugins: [...]});
 */

function createTippyWithPlugins(outerPlugins) {
  if (false) {}

  var tippyPluginsWrapper = function tippyPluginsWrapper(targets, optionalProps, innerPlugins) {
    if (optionalProps === void 0) {
      optionalProps = {};
    }

    if (innerPlugins === void 0) {
      innerPlugins = [];
    }

    innerPlugins = optionalProps.plugins || innerPlugins;
    return tippy(targets, _extends({}, optionalProps, {
      plugins: [].concat(outerPlugins, innerPlugins)
    }));
  };

  tippyPluginsWrapper.version = version;
  tippyPluginsWrapper.defaultProps = defaultProps;
  tippyPluginsWrapper.setDefaultProps = setDefaultProps;
  tippyPluginsWrapper.currentInput = currentInput; // @ts-ignore

  return tippyPluginsWrapper;
}


//# sourceMappingURL=tippy.chunk.esm.js.map

// CONCATENATED MODULE: ./node_modules/tippy.js/dist/tippy.esm.js
/**!
* tippy.js v5.2.1
* (c) 2017-2020 atomiks
* MIT License
*/




/**
 * Re-uses a single tippy element for many different tippy instances.
 * Replaces v4's `tippy.group()`.
 */

var tippy_esm_createSingleton = function createSingleton(tippyInstances, optionalProps,
/** @deprecated use Props.plugins */
plugins) {
  if (optionalProps === void 0) {
    optionalProps = {};
  }

  if (plugins === void 0) {
    plugins = [];
  }

  if (false) {}

  plugins = optionalProps.plugins || plugins;
  tippyInstances.forEach(function (instance) {
    instance.disable();
  });

  var userAria = _extends({}, defaultProps, {}, optionalProps).aria;

  var currentAria;
  var currentTarget;
  var shouldSkipUpdate = false;
  var references = tippyInstances.map(function (instance) {
    return instance.reference;
  });
  var singleton = {
    fn: function fn(instance) {
      function handleAriaDescribedByAttribute(isShow) {
        if (!currentAria) {
          return;
        }

        var attr = "aria-" + currentAria;

        if (isShow && !instance.props.interactive) {
          currentTarget.setAttribute(attr, instance.popperChildren.tooltip.id);
        } else {
          currentTarget.removeAttribute(attr);
        }
      }

      return {
        onAfterUpdate: function onAfterUpdate(_, _ref) {
          var aria = _ref.aria;

          // Ensure `aria` for the singleton instance stays `null`, while
          // changing the `userAria` value
          if (aria !== undefined && aria !== userAria) {
            if (!shouldSkipUpdate) {
              userAria = aria;
            } else {
              shouldSkipUpdate = true;
              instance.setProps({
                aria: null
              });
              shouldSkipUpdate = false;
            }
          }
        },
        onDestroy: function onDestroy() {
          tippyInstances.forEach(function (instance) {
            instance.enable();
          });
        },
        onMount: function onMount() {
          handleAriaDescribedByAttribute(true);
        },
        onUntrigger: function onUntrigger() {
          handleAriaDescribedByAttribute(false);
        },
        onTrigger: function onTrigger(_, event) {
          var target = event.currentTarget;
          var index = references.indexOf(target); // bail-out

          if (target === currentTarget) {
            return;
          }

          currentTarget = target;
          currentAria = userAria;

          if (instance.state.isVisible) {
            handleAriaDescribedByAttribute(true);
          }

          instance.popperInstance.reference = target;
          instance.setContent(tippyInstances[index].props.content);
        }
      };
    }
  };
  return tippy(div(), _extends({}, optionalProps, {
    plugins: [singleton].concat(plugins),
    aria: null,
    triggerTarget: references
  }));
};

var BUBBLING_EVENTS_MAP = {
  mouseover: 'mouseenter',
  focusin: 'focus',
  click: 'click'
};
/**
 * Creates a delegate instance that controls the creation of tippy instances
 * for child elements (`target` CSS selector).
 */

function delegate(targets, props,
/** @deprecated use Props.plugins */
plugins) {
  if (plugins === void 0) {
    plugins = [];
  }

  if (false) {}

  plugins = props.plugins || plugins;
  var listeners = [];
  var childTippyInstances = [];
  var target = props.target;
  var nativeProps = removeProperties(props, ['target']);

  var parentProps = _extends({}, nativeProps, {
    plugins: plugins,
    trigger: 'manual'
  });

  var childProps = _extends({}, nativeProps, {
    plugins: plugins,
    showOnCreate: true
  });

  var returnValue = tippy(targets, parentProps);
  var normalizedReturnValue = normalizeToArray(returnValue);

  function onTrigger(event) {
    if (!event.target) {
      return;
    }

    var targetNode = event.target.closest(target);

    if (!targetNode) {
      return;
    } // Get relevant trigger with fallbacks:
    // 1. Check `data-tippy-trigger` attribute on target node
    // 2. Fallback to `trigger` passed to `delegate()`
    // 3. Fallback to `defaultProps.trigger`


    var trigger = targetNode.getAttribute('data-tippy-trigger') || props.trigger || defaultProps.trigger; // Only create the instance if the bubbling event matches the trigger type

    if (!includes(trigger, BUBBLING_EVENTS_MAP[event.type])) {
      return;
    }

    var instance = tippy(targetNode, childProps);

    if (instance) {
      childTippyInstances = childTippyInstances.concat(instance);
    }
  }

  function on(node, eventType, handler, options) {
    if (options === void 0) {
      options = false;
    }

    node.addEventListener(eventType, handler, options);
    listeners.push({
      node: node,
      eventType: eventType,
      handler: handler,
      options: options
    });
  }

  function addEventListeners(instance) {
    var reference = instance.reference;
    on(reference, 'mouseover', onTrigger);
    on(reference, 'focusin', onTrigger);
    on(reference, 'click', onTrigger);
  }

  function removeEventListeners() {
    listeners.forEach(function (_ref) {
      var node = _ref.node,
          eventType = _ref.eventType,
          handler = _ref.handler,
          options = _ref.options;
      node.removeEventListener(eventType, handler, options);
    });
    listeners = [];
  }

  function applyMutations(instance) {
    var originalDestroy = instance.destroy;

    instance.destroy = function (shouldDestroyChildInstances) {
      if (shouldDestroyChildInstances === void 0) {
        shouldDestroyChildInstances = true;
      }

      if (shouldDestroyChildInstances) {
        childTippyInstances.forEach(function (instance) {
          instance.destroy();
        });
      }

      childTippyInstances = [];
      removeEventListeners();
      originalDestroy();
    };

    addEventListeners(instance);
  }

  normalizedReturnValue.forEach(applyMutations);
  return returnValue;
}

var animateFill = {
  name: 'animateFill',
  defaultValue: false,
  fn: function fn(instance) {
    var _instance$popperChild = instance.popperChildren,
        tooltip = _instance$popperChild.tooltip,
        content = _instance$popperChild.content;
    var backdrop = instance.props.animateFill ? createBackdropElement() : null;

    function addBackdropToPopperChildren() {
      instance.popperChildren.backdrop = backdrop;
    }

    return {
      onCreate: function onCreate() {
        if (backdrop) {
          addBackdropToPopperChildren();
          tooltip.insertBefore(backdrop, tooltip.firstElementChild);
          tooltip.setAttribute('data-animatefill', '');
          tooltip.style.overflow = 'hidden';
          instance.setProps({
            animation: 'shift-away',
            arrow: false
          });
        }
      },
      onMount: function onMount() {
        if (backdrop) {
          var transitionDuration = tooltip.style.transitionDuration;
          var duration = Number(transitionDuration.replace('ms', '')); // The content should fade in after the backdrop has mostly filled the
          // tooltip element. `clip-path` is the other alternative but is not
          // well-supported and is buggy on some devices.

          content.style.transitionDelay = Math.round(duration / 10) + "ms";
          backdrop.style.transitionDuration = transitionDuration;
          setVisibilityState([backdrop], 'visible'); // Warn if the stylesheets are not loaded

          if (false) {}
        }
      },
      onShow: function onShow() {
        if (backdrop) {
          backdrop.style.transitionDuration = '0ms';
        }
      },
      onHide: function onHide() {
        if (backdrop) {
          setVisibilityState([backdrop], 'hidden');
        }
      },
      onAfterUpdate: function onAfterUpdate() {
        // With this type of prop, it's highly unlikely it will be changed
        // dynamically. We'll leave out the diff/update logic it to save bytes.
        // `popperChildren` is assigned a new object onAfterUpdate
        addBackdropToPopperChildren();
      }
    };
  }
};

function createBackdropElement() {
  var backdrop = div();
  backdrop.className = BACKDROP_CLASS;
  setVisibilityState([backdrop], 'hidden');
  return backdrop;
}

var tippy_esm_followCursor = {
  name: 'followCursor',
  defaultValue: false,
  fn: function fn(instance) {
    var reference = instance.reference,
        popper = instance.popper;
    var originalReference = null; // Support iframe contexts
    // Static check that assumes any of the `triggerTarget` or `reference`
    // nodes will never change documents, even when they are updated

    var doc = getOwnerDocument(instance.props.triggerTarget || reference); // Internal state

    var lastMouseMoveEvent;
    var mouseCoords = null;
    var isInternallySettingControlledProp = false; // These are controlled by this plugin, so we need to store the user's
    // original prop value

    var userProps = instance.props;

    function setUserProps(props) {
      var keys = Object.keys(props);
      keys.forEach(function (prop) {
        userProps[prop] = useIfDefined(props[prop], userProps[prop]);
      });
    }

    function getIsManual() {
      return instance.props.trigger.trim() === 'manual';
    }

    function getIsEnabled() {
      // #597
      var isValidMouseEvent = getIsManual() ? true : // Check if a keyboard "click"
      mouseCoords !== null && !(mouseCoords.clientX === 0 && mouseCoords.clientY === 0);
      return instance.props.followCursor && isValidMouseEvent;
    }

    function getIsInitialBehavior() {
      return currentInput.isTouch || instance.props.followCursor === 'initial' && instance.state.isVisible;
    }

    function resetReference() {
      if (instance.popperInstance && originalReference) {
        instance.popperInstance.reference = originalReference;
      }
    }

    function handlePlacement() {
      // Due to `getVirtualOffsets()`, we need to reverse the placement if it's
      // shifted (start -> end, and vice-versa)
      // Early bail-out
      if (!getIsEnabled() && instance.props.placement === userProps.placement) {
        return;
      }

      var placement = userProps.placement;
      var shift = placement.split('-')[1];
      isInternallySettingControlledProp = true;
      instance.setProps({
        placement: getIsEnabled() && shift ? placement.replace(shift, shift === 'start' ? 'end' : 'start') : placement
      });
      isInternallySettingControlledProp = false;
    }

    function handlePopperListeners() {
      if (!instance.popperInstance) {
        return;
      } // Popper's scroll listeners make sense for `true` only. TODO: work out
      // how to only listen horizontal scroll for "horizontal" and vertical
      // scroll for "vertical"


      if (getIsEnabled() && getIsInitialBehavior()) {
        instance.popperInstance.disableEventListeners();
      }
    }

    function handleMouseMoveListener() {
      if (getIsEnabled()) {
        addListener();
      } else {
        resetReference();
      }
    }

    function triggerLastMouseMove() {
      if (getIsEnabled()) {
        onMouseMove(lastMouseMoveEvent);
      }
    }

    function addListener() {
      doc.addEventListener('mousemove', onMouseMove);
    }

    function removeListener() {
      doc.removeEventListener('mousemove', onMouseMove);
    }

    function onMouseMove(event) {
      var _lastMouseMoveEvent = lastMouseMoveEvent = event,
          clientX = _lastMouseMoveEvent.clientX,
          clientY = _lastMouseMoveEvent.clientY;

      if (!instance.popperInstance || !instance.state.currentPlacement) {
        return;
      } // If the instance is interactive, avoid updating the position unless it's
      // over the reference element


      var isCursorOverReference = closestCallback(event.target, function (el) {
        return el === reference;
      });
      var followCursor = instance.props.followCursor;
      var isHorizontal = followCursor === 'horizontal';
      var isVertical = followCursor === 'vertical';
      var isVerticalPlacement = includes(['top', 'bottom'], getBasePlacement(instance.state.currentPlacement)); // The virtual reference needs some size to prevent itself from overflowing

      var _getVirtualOffsets = getVirtualOffsets(popper, isVerticalPlacement),
          size = _getVirtualOffsets.size,
          x = _getVirtualOffsets.x,
          y = _getVirtualOffsets.y;

      if (isCursorOverReference || !instance.props.interactive) {
        // Preserve custom position ReferenceObjects, which may not be the
        // original targets reference passed as an argument
        if (originalReference === null) {
          originalReference = instance.popperInstance.reference;
        }

        instance.popperInstance.reference = {
          referenceNode: reference,
          // These `client` values don't get used by Popper.js if they are 0
          clientWidth: 0,
          clientHeight: 0,
          getBoundingClientRect: function getBoundingClientRect() {
            var rect = reference.getBoundingClientRect();
            return {
              width: isVerticalPlacement ? size : 0,
              height: isVerticalPlacement ? 0 : size,
              top: (isHorizontal ? rect.top : clientY) - y,
              bottom: (isHorizontal ? rect.bottom : clientY) + y,
              left: (isVertical ? rect.left : clientX) - x,
              right: (isVertical ? rect.right : clientX) + x
            };
          }
        };
        instance.popperInstance.update();
      }

      if (getIsInitialBehavior()) {
        removeListener();
      }
    }

    return {
      onAfterUpdate: function onAfterUpdate(_, partialProps) {
        if (!isInternallySettingControlledProp) {
          setUserProps(partialProps);

          if (partialProps.placement) {
            handlePlacement();
          }
        } // A new placement causes the popperInstance to be recreated


        if (partialProps.placement) {
          handlePopperListeners();
        } // Wait for `.update()` to set `instance.state.currentPlacement` to
        // the new placement


        requestAnimationFrame(triggerLastMouseMove);
      },
      onMount: function onMount() {
        triggerLastMouseMove();
        handlePopperListeners();
      },
      onShow: function onShow() {
        if (getIsManual()) {
          // Since there's no trigger event to use, we have to use these as
          // baseline coords
          mouseCoords = {
            clientX: 0,
            clientY: 0
          }; // Ensure `lastMouseMoveEvent` doesn't access any other properties
          // of a MouseEvent here

          lastMouseMoveEvent = mouseCoords;
          handlePlacement();
          handleMouseMoveListener();
        }
      },
      onTrigger: function onTrigger(_, event) {
        // Tapping on touch devices can trigger `mouseenter` then `focus`
        if (mouseCoords) {
          return;
        }

        if (isMouseEvent(event)) {
          mouseCoords = {
            clientX: event.clientX,
            clientY: event.clientY
          };
          lastMouseMoveEvent = event;
        }

        handlePlacement();
        handleMouseMoveListener();
      },
      onUntrigger: function onUntrigger() {
        // If untriggered before showing (`onHidden` will never be invoked)
        if (!instance.state.isVisible) {
          removeListener();
          mouseCoords = null;
        }
      },
      onHidden: function onHidden() {
        removeListener();
        resetReference();
        mouseCoords = null;
      }
    };
  }
};
function getVirtualOffsets(popper, isVerticalPlacement) {
  var size = isVerticalPlacement ? popper.offsetWidth : popper.offsetHeight;
  return {
    size: size,
    x: isVerticalPlacement ? size : 0,
    y: isVerticalPlacement ? 0 : size
  };
}

// position. This will require the `followCursor` plugin's fixes for overflow
// due to using event.clientX/Y values. (normalizedPlacement, getVirtualOffsets)

var inlinePositioning = {
  name: 'inlinePositioning',
  defaultValue: false,
  fn: function fn(instance) {
    var reference = instance.reference;

    function getIsEnabled() {
      return !!instance.props.inlinePositioning;
    }

    return {
      onHidden: function onHidden() {
        if (getIsEnabled()) {
          instance.popperInstance.reference = reference;
        }
      },
      onShow: function onShow() {
        if (!getIsEnabled()) {
          return;
        }

        instance.popperInstance.reference = {
          referenceNode: reference,
          // These `client` values don't get used by Popper.js if they are 0
          clientWidth: 0,
          clientHeight: 0,
          getBoundingClientRect: function getBoundingClientRect() {
            return getInlineBoundingClientRect(instance.state.currentPlacement && getBasePlacement(instance.state.currentPlacement), reference.getBoundingClientRect(), arrayFrom(reference.getClientRects()));
          }
        };
      }
    };
  }
};
function getInlineBoundingClientRect(currentBasePlacement, boundingRect, clientRects) {
  // Not an inline element, or placement is not yet known
  if (clientRects.length < 2 || currentBasePlacement === null) {
    return boundingRect;
  }

  switch (currentBasePlacement) {
    case 'top':
    case 'bottom':
      {
        var firstRect = clientRects[0];
        var lastRect = clientRects[clientRects.length - 1];
        var isTop = currentBasePlacement === 'top';
        var top = firstRect.top;
        var bottom = lastRect.bottom;
        var left = isTop ? firstRect.left : lastRect.left;
        var right = isTop ? firstRect.right : lastRect.right;
        var width = right - left;
        var height = bottom - top;
        return {
          top: top,
          bottom: bottom,
          left: left,
          right: right,
          width: width,
          height: height
        };
      }

    case 'left':
    case 'right':
      {
        var minLeft = Math.min.apply(Math, clientRects.map(function (rects) {
          return rects.left;
        }));
        var maxRight = Math.max.apply(Math, clientRects.map(function (rects) {
          return rects.right;
        }));
        var measureRects = clientRects.filter(function (rect) {
          return currentBasePlacement === 'left' ? rect.left === minLeft : rect.right === maxRight;
        });
        var _top = measureRects[0].top;
        var _bottom = measureRects[measureRects.length - 1].bottom;
        var _left = minLeft;
        var _right = maxRight;

        var _width = _right - _left;

        var _height = _bottom - _top;

        return {
          top: _top,
          bottom: _bottom,
          left: _left,
          right: _right,
          width: _width,
          height: _height
        };
      }

    default:
      {
        return boundingRect;
      }
  }
}

var sticky = {
  name: 'sticky',
  defaultValue: false,
  fn: function fn(instance) {
    var reference = instance.reference,
        popper = instance.popper;

    function getReference() {
      return instance.popperInstance ? instance.popperInstance.reference : reference;
    }

    function shouldCheck(value) {
      return instance.props.sticky === true || instance.props.sticky === value;
    }

    var prevRefRect = null;
    var prevPopRect = null;

    function updatePosition() {
      var currentRefRect = shouldCheck('reference') ? getReference().getBoundingClientRect() : null;
      var currentPopRect = shouldCheck('popper') ? popper.getBoundingClientRect() : null;

      if (currentRefRect && areRectsDifferent(prevRefRect, currentRefRect) || currentPopRect && areRectsDifferent(prevPopRect, currentPopRect)) {
        instance.popperInstance.update();
      }

      prevRefRect = currentRefRect;
      prevPopRect = currentPopRect;

      if (instance.state.isMounted) {
        requestAnimationFrame(updatePosition);
      }
    }

    return {
      onMount: function onMount() {
        if (instance.props.sticky) {
          updatePosition();
        }
      }
    };
  }
};

function areRectsDifferent(rectA, rectB) {
  if (rectA && rectB) {
    return rectA.top !== rectB.top || rectA.right !== rectB.right || rectA.bottom !== rectB.bottom || rectA.left !== rectB.left;
  }

  return true;
}


//# sourceMappingURL=tippy.esm.js.map


/***/ })
/******/ ]);