import { make } from "./util/Core"
import { Env } from "./Env"
import { PersistentObject } from "./Runtime"
import { Trie, Value } from "./Syntax"

export type PrimResult<K> = [Value | null, K]
type TrieCtr = (body: null) => Trie.Prim<null>
type Binary<T, U, V> = (x: T, y: U) => (α: PersistentObject) => V

export class PrimBody extends PersistentObject {
   // fields can't have polymorphic types

   // Access prim body via a method to reinstate the polymorphism via a cast.
   invoke<K extends PersistentObject | null> (v1: Value | null, v2: Value | null, σ: Trie<K>): (α: PersistentObject) => PrimResult<K> {
      return null as any
   }
} 

export class BinOp extends PersistentObject {
   name: string
   σ1: Trie.Prim<null>
   σ2: Trie.Prim<null>
   b: PrimBody

   static make (name: string, σ1: Trie.Prim<null>, σ2: Trie.Prim<null>, b: PrimBody): BinOp {
      const this_: BinOp = make(BinOp, σ1, σ2, b)
      this_.name = name
      this_.σ1 = σ1
      this_.σ2 = σ2
      this_.b = b
      return this_
   }
}

function makeBinary<T extends Value, U extends Value, V extends Value> (op: Binary<T, U, V>, trie1: TrieCtr, trie2: TrieCtr): BinOp {
   return BinOp.make(op.name, trie1(null), trie2(null), null)
}

export const ops: Map<string, BinOp> = new Map([
   ["-", makeBinary(minus, Trie.ConstInt.make, Trie.ConstInt.make)],
])

export function minus (x: Value.ConstInt, y: Value.ConstInt): (α: PersistentObject) => Value.ConstInt {
   return α => Value.ConstInt.at(α, x.val - y.val)
}

// Not adding primitive names to the prelude at the moment, because they aren't bona fide expressions.
export function prelude (): Env {
   return Env.empty()
}
