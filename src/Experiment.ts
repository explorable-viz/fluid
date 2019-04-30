import { __nonNull } from "./util/Core"

class Expl {
}

type State<T> = {
   readonly [prop in keyof T]: T[prop]
}

abstract class Explainable<T> {
   expl (prop: keyof T): Expl {
      return __nonNull(__nonNull(this.__expl).get(prop))
   }

   __expl?: Map<keyof T, Expl>
}

// Not easy to put this into Explainable and have it be specifically typed enough.
function construct<T> (tgt: T, state: State<T>): T {
   // copy state to fields of tgt
   return tgt
}

abstract class List<T> extends Explainable<List<T>> {
   abstract classify (σ: ListTrie<T>): void
}

interface ListTrie<T> {
   isNil (xs: Nil<T>): void
   isCons (xs: Cons<T>): void
}

export class Nil<T> extends List<T> {
   classify (σ: ListTrie<T>): void {
      σ.isNil(this)
   }
}

export class Cons<T> extends List<T> {
   head: T
   tail: List<T>

   classify (σ: ListTrie<T>): void {
      σ.isCons(this)
   }
}

export function cons<T> (head: T, tail: List<T>): List<T> {
   return construct(new Cons, { head: "", tail: null })
}
