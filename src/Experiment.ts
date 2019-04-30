import { Class, __nonNull, absurd, funName } from "./util/Core"
import { Env } from "./Env"

namespace Expr {
   export class Expr {
   }

   export class Constr {
      ctr: string
      args: List<Expr>
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

type ExplState<T> = { [prop in keyof CoreProps<T>]: Expl }

// Gather the metadata properties associated with T.
interface Metadata<T> {
   expl?: ExplState<T>
   classify (σ: T): void
}  

type CoreProps<T> = Pick<T, Exclude<keyof T, keyof Metadata<T>>>

// Value in the metalanguage.
abstract class Value {
}

abstract class Explainable<T> extends Value implements Metadata<T> {
   expl?: ExplState<T>
   abstract classify<U> (σ: Trie<U>): U
}

// Not easy to put this into Explainable and have it be specifically typed enough.
function construct<T> (tgt: T, state: State<T>): T {
   return constructʹ(tgt, state) as T
}

// Dynamic version of construct.
function constructʹ (tgt: Value, state: Stateʹ): Value {
   // copy state to fields of tgt
   return tgt
}

abstract class List<T> extends Explainable<List<T>> {
   abstract classify<U> (σ: ListTrie<T, U>): U
}

interface Trie<U> {
}

interface ListTrie<T, U> extends Trie<U> {
   isNil (xs: Nil<T>): U
   isCons (xs: Cons<T>): U
}

export class Nil<T> extends List<T> {
   classify<U> (σ: ListTrie<T, U>): U {
      return σ.isNil(this)
   }
}

export class Cons<T> extends List<T> {
   head: T
   tail: List<T>

   classify<U> (σ: ListTrie<T, U>): U {
      return σ.isCons(this)
   }
}

export function cons<T> (head: T, tail: List<T>): List<T> {
   return make(Cons, { head: head, tail: tail })
}

function make<T> (ctr: Class<T>, state: State<T>): T {
   return construct(new ctr, state)
}

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
         e̅.classify({
            isNil(e̅ʹ: Nil<Expr>): void {
               absurd()
            },
            isCons(e̅ʹ: Cons<Expr>): void {
               state[f] = eval_(ρ, e̅ʹ.head)
               e̅ = e̅ʹ.tail
            }
         })
      }
      return [empty(), construct(new d.cls, state)]
   } else {
      return absurd()
   }
}
