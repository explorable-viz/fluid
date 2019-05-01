import { Ord } from "./util/Ord"
import { Class, __nonNull, absurd, funName } from "./util/Core"
import { Env } from "./Env"

// Value in the metalanguage.
abstract class Value {
}

abstract class Explainable<T> extends Value implements Metadata<T> {
   expl?: ExplState<T>
   abstract match<U> (σ: Fun<U>): U
}

namespace Expr {
   export abstract class Expr extends Explainable<Expr> {
      abstract match<U> (σ: ExprFun<U>): U
   }

   interface ExprFun<U> {
      Constr (ctr: string, args: List<Expr>): U
      Fun (σ: Trie<Expr>): U
      MatchAs (e: Expr, σ: Trie<Expr>): U
   }

   export class Constr extends Expr {
      ctr: string
      args: List<Expr>

      match<U> (σ: ExprFun<U>): U {
         return σ.Constr(this.ctr, this.args)
      }
   }

   export class Fun extends Expr {
      σ: Trie<Expr>

      match<U> (σ: ExprFun<U>): U {
         return σ.Fun(this.σ)
      }
   }

   export class MatchAs extends Expr {
      e: Expr
      σ: Trie<Expr>

      match<U> (σ: ExprFun<U>): U {
         return σ.MatchAs(this.e, this.match)
      }
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

namespace Expl {
   export class Expl {
   }

   class Empty extends Expl {
   }

   export function empty (): Empty {
      return make(Empty, {})
   }
}

type Expl = Expl.Expl

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

class Fun<U> extends Value {
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
      return σ.Pair(this.fst, this.snd)
   }
}

interface PairFun<T, U, V> extends Fun<V> {
   Pair (fst: T, snd: U): V
}

export abstract class Tree<T> extends Explainable<Tree<T>> {
}

export class Empty<T> extends Tree<T> {
   match<U> (σ: TreeFun<T, U>): U {
      return σ.Empty()
   }
}

export class NonEmpty<T> extends Tree<T> {
   left: Tree<T>
   t: T
   right: Tree<T>

   match<U> (σ: TreeFun<T, U>): U {
      return σ.NonEmpty(this.left, this.t, this.right)
   }
}

interface TreeFun<T, U> extends Fun<U> {
   Empty (): U
   NonEmpty (left: Tree<T>, t: T, right: Tree<T>): U
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
   return e.match({
      Constr(ctr, args): ExplVal {
         const d: Datatype = __nonNull(datatypeFor.get(ctr)),
               state: Stateʹ = {}
         let e̅: List<Expr> = args
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
         return [Expl.empty(), construct(new d.cls, state)]
      },
      Fun(σ): ExplVal {
         throw new Error
      },
      MatchAs(e, σ): ExplVal {
         throw new Error
      }
   })
}
