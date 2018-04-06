import { assert, funName, make } from "./util/Core"
import { Env, EnvEntry, ExtendEnv } from "./Env"
import { ν, PersistentObject } from "./Runtime"
import { Expr, Lex, Trie, Value } from "./Syntax"

export type PrimResult<T> = [Value.Value | null, T] // v, σv
export type PrimBody<T> = (v: Value.Value | null, σ: Trie.Trie<T>) => PrimResult<T>
type TrieCtr<T> = (α: Object, body: PrimBody<T>) => Trie.Prim<PrimBody<T>>

function match<T> (v: Value.Value, σ: Trie.Trie<T>): PrimResult<T> {
   if (v instanceof Value.PrimOp && Trie.Fun.is(σ)) {
      return [v, σ.body]
   }  else
   if (v instanceof Value.ConstInt && Trie.ConstInt.is(σ)) {
      return [v, σ.body]
   } else 
   if (v instanceof Value.ConstStr && Trie.ConstStr.is(σ)) {
      return [v, σ.body]
   } else 
   if (v instanceof Value.Constr && Trie.Constr.is(σ) && σ.cases.has(v.ctr.str)) {
      return [v, σ.cases.get(v.ctr.str)!]
   } else {
      return assert(false, "Demand mismatch.")
   }
}

class PrimId extends PersistentObject {
   __PrimId (): void {
      // discriminator
   }
}

class ExternalPrimId extends PrimId {
   op: string

   static make (op: string): PrimId {
      const this_: ExternalPrimId = make(ExternalPrimId, op)
      this_.op = op
      return this_
   }
}

class PrimArgDemandId extends PersistentObject {
   k: PrimId

   static make (k: PrimId): PrimArgDemandId {
      const this_: PrimArgDemandId = make(PrimArgDemandId, k)
      this_.k = k
      return this_
   }
}

class PartialAppId extends PrimId {
   k: PrimId
   v: Value.Value

   static make (k: PrimId, v: Value.Value): PartialAppId {
      const this_: PartialAppId = make(PartialAppId, k, v)
      this_.k = k
      this_.v = v
      return this_
   }
}

function makePrim<T extends Value.Value, V extends Value.Value> (
   k: PrimId, 
   name: string, 
   op: (x: T) => V,
   at1: (α: PersistentObject, body: PrimBody<V>) => Trie.Prim<PrimBody<V>>
): Value.PrimOp {
   const primBody: PrimBody<V> = (x: T, σ: Trie.Trie<V>): PrimResult<V> => match(op(x), σ)
   return Value.PrimOp.at(k, name,  at1(PrimArgDemandId.make(k), primBody))
}

function unary<T extends Value.Value, V extends Value.Value> (
   op: (x: T) => V,
   at1: TrieCtr<V>,
): Value.PrimOp {
   return makePrim(ExternalPrimId.make(funName(op)), op.name, op, at1)
}

function binary<T extends Value.Value, U extends Value.Value, V extends Value.Value> (
   op: (x: T, y: U) => V,
   at1: TrieCtr<Value.PrimOp>,
   at2: TrieCtr<V>
): Value.PrimOp {
   const k: PrimId = ExternalPrimId.make(funName(op)),
         partiallyApply: (x: T) => Value.PrimOp =
            (x: T) => makePrim(PartialAppId.make(k, x), op.name + " " + x, (y: U) => op(x, y), at2)
   return makePrim(k, op.name, partiallyApply, at1)
}

class UnaryPrimResultId extends PersistentObject {
   op: string
   v: Value.Value

   static make (op: string, v: Value.Value): UnaryPrimResultId {
      const this_: UnaryPrimResultId = make(UnaryPrimResultId, v)
      this_.v = v
      return this_
   }
}

class BinaryPrimResultId extends PersistentObject {
   op: string
   v1: Value.Value
   v2: Value.Value

   static make (op: string, v1: Value.Value, v2: Value.Value): BinaryPrimResultId {
      const this_: BinaryPrimResultId = make(BinaryPrimResultId, v1, v2)
      this_.v1 = v1
      this_.v2 = v2
      return this_
   }
}

function __true (α: PersistentObject): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("True"), [])
}

function __false (α: PersistentObject): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("False"), [])
}

// See 0.2.4 release notes re. primitive ops with identifiers as names.
// Used to take an arbitrary value as an additional argument but now primitives must have
// primitive arguments.
export function error (message: Value.ConstStr): Value.Value {
   return assert(false, "LambdaCalc error:\n" + message.val)
}

export function intToString (x: Value.ConstInt): Value.ConstStr {
   const k: PersistentObject = UnaryPrimResultId.make(funName(intToString), x)
   return Value.ConstStr.at(k, x.toString())
}

// No longer support overloaded functions, since the demand-indexed semantics is non-trivial.
export function equalInt (x: Value.ConstInt, y: Value.ConstInt): Value.Constr {
   const k: PersistentObject = BinaryPrimResultId.make(funName(equalInt), x, y)
   return x.val === y.val ? __true(k) : __false(k)
}

export function equalStr (x: Value.ConstStr, y: Value.ConstStr): Value.Constr {
   const k: PersistentObject = BinaryPrimResultId.make(funName(equalStr), x, y)
   return x.val === y.val ? __true(k) : __false(k)
}

export function greaterInt (x: Value.ConstInt, y: Value.ConstInt): Value.Constr {
   const k: PersistentObject = BinaryPrimResultId.make(funName(greaterInt), x, y)
   return x.val > y.val ? __true(k) : __false(k)
}

export function greaterStr (x: Value.ConstStr, y: Value.ConstStr): Value.Constr {
   const k: PersistentObject = BinaryPrimResultId.make(funName(greaterStr), x, y)
   return x.val > y.val ? __true(k) : __false(k)
}

export function lessInt (x: Value.ConstInt, y: Value.ConstInt): Value.Constr {
   const k: PersistentObject = BinaryPrimResultId.make(funName(lessInt), x, y)
   return x.val > y.val ? __true(k) : __false(k)
}

export function lessStr (x: Value.ConstStr, y: Value.ConstStr): Value.Constr {
   const k: PersistentObject = BinaryPrimResultId.make(funName(lessStr), x, y)
   return x.val > y.val ? __true(k) : __false(k)
}

export function minus (x: Value.ConstInt, y: Value.ConstInt): Value.ConstInt {
   const k: PersistentObject = BinaryPrimResultId.make(funName(minus), x, y)
   return Value.ConstInt.at(k, x.val - y.val)
}

export function plus (x: Value.ConstInt, y: Value.ConstInt): Value.ConstInt {
   const k: PersistentObject = BinaryPrimResultId.make(funName(plus), x, y)
   return Value.ConstInt.at(k, x.val + y.val)
}

export function times (x: Value.ConstInt, y: Value.ConstInt): Value.ConstInt {
   const k: PersistentObject = BinaryPrimResultId.make(funName(times), x, y)
   return Value.ConstInt.at(k, x.val * y.val)
}

export function div (x: Value.ConstInt, y: Value.ConstInt): Value.ConstInt {
   // Apparently this will round in the right direction.
   const k: PersistentObject = BinaryPrimResultId.make(funName(div), x, y)
   return Value.ConstInt.at(k, ~~(x.val / y.val))
}

export function concat (x: Value.ConstStr, y: Value.ConstStr): Value.ConstStr {
   const k: PersistentObject = BinaryPrimResultId.make(funName(concat), x, y)
   return Value.ConstStr.at(k, x.val + y.val)
}

// Must come after the definitions above.
const ops: [string, Value.PrimOp][] = [
   ["error", unary(error, Trie.ConstStr.at)],
   ["intToString", unary(intToString, Trie.ConstInt.at)],
   ["-", binary(minus, Trie.ConstInt.at, Trie.ConstInt.at)],
   ["+", binary(plus, Trie.ConstInt.at, Trie.ConstInt.at)],
   ["*", binary(times, Trie.ConstInt.at, Trie.ConstInt.at)],
   ["/", binary(div, Trie.ConstInt.at, Trie.ConstInt.at)],
   ["==", binary(equalInt, Trie.ConstInt.at, Trie.ConstInt.at)],
   ["===", binary(equalStr, Trie.ConstStr.at, Trie.ConstStr.at)],
   [">", binary(greaterInt, Trie.ConstInt.at, Trie.ConstInt.at)],
   [">>", binary(greaterStr, Trie.ConstStr.at, Trie.ConstStr.at)],
   ["<", binary(lessInt, Trie.ConstInt.at, Trie.ConstInt.at)],
   ["<<", binary(lessStr, Trie.ConstStr.at, Trie.ConstStr.at)],
   ["++", binary(concat, Trie.ConstStr.at, Trie.ConstStr.at)],
]

// Fake "syntax" for primitives.
export function prelude (): Env {
   let ρ: Env = Env.empty()
   ops.forEach(([x, op]: [string, Value.PrimOp]): void => {
      const e: Expr.PrimOp = Expr.PrimOp.at(ν(), op)
      ρ = ExtendEnv.make(ρ, x, EnvEntry.make(Env.empty(), Expr.EmptyRecDefs.make(), e))
   })
   return ρ
}
