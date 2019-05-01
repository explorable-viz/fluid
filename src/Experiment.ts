import { Ord } from "./util/Ord"
import { Class, __nonNull, absurd, funName } from "./util/Core"
import { Env } from "./Env"

namespace Expr {
   export class Expr {
   }

   export class Constr {
      ctr: string
      args: List<Expr>
   }

   export type Trie<K extends Kont<K>> = Trie.Trie<K>

   export interface Kont<K> {
   }

   export type Args<K extends Kont<K>> = Args.Args<K>

   export namespace Args {
      export abstract class Args<K extends Kont<K>> implements Kont<Args<K>> {
      }

      export class End<K extends Kont<K>> extends Args<K> {
         κ: K
      }

      export class Next<K extends Kont<K>> extends Args<K> {
         σ: Trie<Args<K>>
      }
   }

   namespace Trie {
      export abstract class Trie<K extends Kont<K>> implements Kont<Trie<K>> {
      }

      export class Constr<K extends Kont<K>> extends Trie<K> {
         cases: FiniteMap<string, Args<K>>
      }
   }
}

type Expr = Expr.Expr

class Expl {
}

class Empty extends Expl {
}

export function empty (): Empty {
   return make(Empty, {})
}

type State<T> = CoreProps<T>

// Dynamic version of State?
export interface Stateʹ {
   [prop: string]: Value
}

type ExplState<T> = { 
   [prop in keyof CoreProps<T>]: Expl 
}

// Gather the metadata properties associated with T.
interface Metadata<T> {
   expl?: ExplState<T>
   match (σ: T): void
}  

type CoreProps<T> = Pick<T, Exclude<keyof T, keyof Metadata<T>>>

// Value in the metalanguage.
abstract class Value {
}

abstract class Explainable<T> extends Value implements Metadata<T> {
   expl?: ExplState<T>
   abstract match<U> (σ: Fun<U>): U
}

// Not easy to put this into Explainable and have it be specifically typed enough.
function construct<T> (tgt: T, state: State<T>): T {
   return constructʹ(tgt, state) as T
}

// Dynamic version of construct.
function constructʹ (tgt: Value, state: Stateʹ): Value {
   // TODO: copy state to fields of tgt
   return tgt
}

function make<T> (ctr: Class<T>, state: State<T>): T {
   return construct(new ctr, state)
}

interface Fun<U> {
}

abstract class List<T> extends Explainable<List<T>> {
   abstract match<U> (σ: ListFun<T, U>): U
}

export class Nil<T> extends List<T> {
   match<U> (σ: ListFun<T, U>): U {
      return σ.Nil()
   }
}

export class Cons<T> extends List<T> {
   head: T
   tail: List<T>

   match<U> (σ: ListFun<T, U>): U {
      return σ.Cons(this.head, this.tail)
   }
}

interface ListFun<T, U> extends Fun<U> {
   Nil (): U
   Cons (x: T, xs: List<T>): U
}

export function cons<T> (head: T, tail: List<T>): List<T> {
   return make(Cons, { head, tail })
}

export class Pair<T, U> extends Explainable<Pair<T, U>> {
   fst: T
   snd: U

   match<V> (σ: PairFun<T, U, V>): V {
      return σ.isPair(this)
   }
}

interface PairFun<T, U, V> extends Fun<V> {
   isPair (xs: Pair<T, U>): V}

export function pair<T, U> (fst: T, snd: U): Pair<T, U> {
   return make<Pair<T, U>>(Pair, { fst, snd })
}

export type FiniteMap<K extends Ord<K>, V> = Tree<Pair<K, V>>

type ExplVal = [Expl, Value]

type Datatype = {
   cls: Class<Value>, 
   fields: string[]
} 

// TODO: exclude metadata in a way that's consistent with Metadata<T>
export function isField (prop: string): boolean {
   throw new Error
}

// Utterly dependent on fields being provided in declaration order, although not part of spec :-/
export function fields (cls: Class<Value>): string[] {
   const proto: Object = Object.getPrototypeOf(new cls)
   return Object.getOwnPropertyNames(proto).filter(isField)
}

// Guess this would be populated at compile-time or by a type provider. Is there a reflective way to access the classes of a module?
const datatypeFor_: Class<Value>[] = 
   [Cons,
    Empty,
    Nil],
   datatypeFor: Map<string, Datatype> = new Map(
      datatypeFor_.map((cls): [string, Datatype] => [funName(__nonNull(cls)), { cls, fields: fields(cls) }])
   )

export function eval_ (ρ: Env, e: Expr): ExplVal {
   if (e instanceof Expr.Constr) {
      const d: Datatype = __nonNull(datatypeFor.get(e.ctr)),
            state: Stateʹ = {}
      let e̅: List<Expr> = e.args
      for (const f of d.fields) {
         e̅.match({
            Nil(): void {
               absurd()
            },
            Cons(e: Expr, e̅ʹ: List<Expr>): void {
               state[f] = eval_(ρ, e)
               e̅ = e̅ʹ
            }
         })
      }
      return [empty(), construct(new d.cls, state)]
   } else {
      return absurd()
   }
}
