import { Int, None, Prim, Some, Str, __toArray, __toList } from "./BaseTypes"
import { Ctr, ctrToDataType, fields } from "./DataType"
import { empty, insert } from "./FiniteMap"
import { __def, key, keyP } from "./Memo"
import { UnaryOp, BinaryOp, intToString } from "./Primitive"
import * as P from "./Primitive"
import {
   BinaryPrimOp, ConstInt, Constr, ConstStr, EmptyTrace, Env, Proj, Trace, UnaryPrimOp,
   Value, str
} from "./Syntax"
import { __nonNull, assert, assertMessage, className, typeCheck } from "./Util"

// Chief subtlety is retracting the reification of a raw (untraced) value; see 0.6.3 release notes.
__def(reflect)
export function reflect (v: Value): Object | null {
   if (v === null) {
      return null
   }
   return v.__visit({
      is_ConstInt: (v: ConstInt): Object => // TS bug disallows Int here
         v.val,
      is_ConstStr: (v: ConstStr): Str =>
         v.val,
      is_Constr: function (v: Constr): Object {
         const α: Addr = key(reflect, arguments),
               ctr: string = v.ctr.val,
               tvs: ITraced[] = __toArray(v.args)
         assertMessage(constructors.has(ctr), 'No such constructor.', ctr)
         const traced0: boolean = // whether v is the image of a Traced0 in 'reify'
            ctr === className(Traced) &&
            !tvs.find(ch => !(ch instanceof Traced0 && ch.name instanceof None && ch.trace instanceof EmptyTrace))
         if (traced0) {
            return Traced0.at(α, <Trace>tvs[0].val, <Prim.Option<Str>>tvs[1].val, tvs[2].val)
         } else {
            const ctrFun: Ctr<Object> = __nonNull(constructors.get(ctr))
            return (<any>ctrFun).at(α, ...tvs) // cast to access static factory method
         }
      }
   })
}

// See 0.2.4 and 0.6.3 release notes for design discussion. A child is a "key" of an object, i.e. #
// specifically not an inherited property. (This imposes some constraints on JavaScript types that we
// want to reify into the object language, namely that they correspond in a naive way to the disjuncts
// of algebraic data types.)
// The rules are as follows:
//    (1) every field f of the datatype is assumed to have a Traced implementation slot called "_" + f
//    (2) the Traced0 constiant is treated specially: it has the constructor "Traced"; and its (untraced)
//        fields are are promoted into Traced0 nodes with empty traces.
__def(reify)
export function reify (v: Object): Value | null {
   const α: Addr = key(reify, arguments)
   if (v === null) {
      return null
   } else
   if (v instanceof Int) {
      return ConstInt.at_(α, v)
   } else
   if (v instanceof Str) {
      return ConstStr.at_(α, v)
   } else {
      const β: Addr = keyP(α, 'args', 'v'),
            traced0: boolean = v instanceof Traced0,
            ctr: Str = Str.at(keyP(α, 'ctr', 'v'), className(traced0 ? Traced : v.constructor)),
            tvs: ITraced[] = fields(v).map((f: string): ITraced => {
         const child: any = (<any>v)['_' + f] // hardcoded naming convention; cast for untyped field access
         return traced0 ? __val(keyP(β, f), child) : child
      })
      assert(nonObject.indexOf(ctr.val) === -1)
      return Constr.at_(α, ctr, __toList(tvs))
   }
}

// We don't treat Number or String as bona fide objects, to avoid picking up unintended implicit coercions.
const nonObject: string[] = [className(Number), className(String)]

// At a given version (there is only one, currently) enforce "single assignment" semantics.
Object.prototype.__version = function () {
   if (this.__history.length === 0) {
      this.__history.push(__shallowCopy(this))
   } else {
      assert(__shallowEq(this, this.__history[0]))
   }
}

Object.defineProperty(Object.prototype, '__version', {
   enumerable: false
})

// Previously used Object.assign, but that goes via getters/setters.
function __shallowCopy (src: Object): Object {
   const tgt: Object = new (src.constructor as { new(): Object } ) // lacks a construct signature
   for (let x of Object.keys(src)) {
      (<any>tgt)[x] = (<any>src)[x]
   }
   return tgt
}

function __shallowEq (o1: Object, o2: Object): boolean {
   assert(o1.constructor === o2.constructor)
   for (let x of Object.keys(o1)) {
      if ((<any>o1)[x] !== (<any>o2)[x]) {
         return false
      }
   }
   return true
}

export function __getField (x: any, f: string): Object {
   assertMessage(f in x, 'Doesn\'t have field \'' + f + '\'.', x)
   return <Object>x[f]
}

// Populated by initDataTypes(). Note that constructors are not (yet) first-class.
export const constructors: Map<string, Ctr<Object>> = new Map
export const projections: Map<string, UnaryOp> = new Map

// Populated by initPrimitives().
export var unaryOps: Map<string, UnaryOp> = new Map
export var binaryOps: Map<string, BinaryOp> = new Map

// Map primitives to their underlying JS operation.
function initPrimitives (): void {
   unaryOps.set("intToString", intToString)
   unaryOps.set("reflect", reflect)
   unaryOps.set("reify", reify)

   binaryOps.set(str.concat, P.concat)
   binaryOps.set(str.div, P.div)
   binaryOps.set(str.equal, P.equalOp)
   binaryOps.set(str.greaterT, P.greaterT)
   binaryOps.set(str.lessT, P.lessT)
   binaryOps.set(str.minus, P.minus)
   binaryOps.set(str.plus, P.plus)
   binaryOps.set(str.times, P.times)
   binaryOps.set("error", P.error)
}

// Only primitives at the moment; eventually other library code. Fake "syntax" for primitives.
export function prelude (): Env {
   initPrimitives()

   var ρ: Env = empty<Str, Object>()
   ρ = insert(ρ, Str.at(ν(), 'ctrToDataType'), ctrToDataType)
   for (let f in projections.keys()) {
      const f_: Str = Str.at(ν(), f)
      ρ = insert(ρ, f_, Proj.at(ν(), __val(ν(), f_)))
   }
   for (let op in unaryOps.keys()) {
      const op_: Str = Str.at(ν(), op)
      ρ = insert(ρ, op_, UnaryPrimOp.at(ν(), __val(ν(), op_)))
   }
   for (let op in binaryOps.keys()) {
      const op_: Str = Str.at(ν(), op)
      ρ = insert(ρ, op_, BinaryPrimOp.at(ν(), __val(ν(), op_)))
   }
   return ρ
}

// Permits two varieties of traced values, those whose trace and value components are themselves traced,
// and those whose trace and value components are untraced; see 0.6.3 release notes.
// The 'name' field appears here to allow the syntactic unification of patterns with values.
export class ITraced<T = Object> {
   trace: Trace
   name: Prim.Option<Str>
   val: T
}

// This is the "reified" form of a traced value.
export class Traced<T> extends ITraced<T> {
   _trace: ITraced<Trace>
   _name: ITraced<Prim.Option<Str>>
   _val: ITraced<T>

   static at <T> (α: Addr, trace: ITraced<Trace>, name: ITraced<Prim.Option<Str>>, val: ITraced<T>): Traced<T> {
      const this_: Traced<T> = create(α, Traced)
      this_._trace = typeCheck_(trace, Trace)
      this_._name = typeCheck_(name, Prim.Option)
      this_._val = val
      this_.__version()
      return this_
   }

   get trace (): Trace {
      return this._trace.val
   }

   get name (): Prim.Option<Str> {
      return this._name.val
   }

   get val (): T {
      return this._val.val
   }
}

// When reified, this will appear as a Traced whose children have empty traces.
export class Traced0<T> extends ITraced<T> {
   _trace: Trace
   _name: Prim.Option<Str>
   _val: T

   static at <T> (α: Addr, trace: Trace, name: Prim.Option<Str>, val: T): Traced0<T> {
      const this_: Traced0<T> = create(α, Traced0)
      this_._trace = typeCheck(trace, Trace)
      this_._name = typeCheck(name, Prim.Option)
      this_._val = val
      this_.__version()
      return this_
   }

   get trace (): Trace {
      return this._trace
   }

   get name (): Prim.Option<Str> {
      return this._name
   }

   get val (): T {
      return this._val
   }
}

// Helpers.
export function __val <T> (α: Addr, v: T): ITraced<T> {
   return __tracedK(α, EmptyTrace.at(keyP(α, 't')), __nonNull(v))
}

export function __tracedK <T> (α: Addr, t: Trace, v: T): ITraced<T> {
   return Traced0.at(α, t, None.at<Str>(keyP(α, 'name')), v)
}

export function __traced_var <T> (α: Addr, t: Trace, name: Str, v: T): ITraced<T> {
   return Traced0.at(α, t, Some.at_(keyP(α, 'name'), name), v)
}

export function typeCheck_ <T> (v: ITraced<T>, ctr: Ctr<T>): ITraced<T> {
   if (v !== undefined) { // idiom for reifying datatypes means fields can be uninitialised
      typeCheck(v, ITraced)
      typeCheck(v.val, ctr)
   }
   return v
}

const __instances: Map<Addr, Object> = new Map()

// Allocate a blank object uniquely identified by a memo-key. Needs to be initialised afterwards.
export function create <T> (α: Addr, ctr: Ctr<T>): T {
   var o: Object | undefined = __instances.get(α)
   if (o === undefined) {
      o = new ctr
      // This may massively suck, performance-wise.
      Object.defineProperty(o, '__addr', {
         value: α,
         enumerable: false
      })
      Object.defineProperty(o, '__history', {
         value: [],
         enumerable: false
      })
      __instances.set(α, o)
   } else {
      assertMessage(o.constructor === ctr, 'Address collision.', α, className(o.constructor), className(ctr))
   }
   return <T>o
}

// Fresh keys represent inputs to the system.
export const ν: () => Addr =
   (() => {
      var count: number = 0
      return () => {
         return (count++).toString()
      }
   })()
