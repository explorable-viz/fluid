import { __nonNull } from "./util/Core"

class Expl {
}

type State<T> = CoreProps<T>
type ExplState<T> = { [prop in keyof CoreProps<T>]: Expl }

// Gather the metadata properties associated with T.
interface Metadata<T> {
   __expl?: ExplState<T>
   expl (prop: keyof CoreProps<T>): Expl
   classify (σ: T): void
}  

type CoreProps<T> = Pick<T, Exclude<keyof T, keyof Metadata<T>>>

abstract class Explainable<T> implements Metadata<T> {
   expl (prop: keyof CoreProps<T>): Expl {
      return __nonNull(this.__expl)[prop]
   }

   __expl?: ExplState<T>
   abstract classify (σ: Trie): void
}

// Not easy to put this into Explainable and have it be specifically typed enough.
function construct<T> (tgt: T, state: State<T>): T {
   // copy state to fields of tgt
   return tgt
}

abstract class List<T> extends Explainable<List<T>> {
   abstract classify (σ: ListTrie<T>): void
}

interface Trie {
}

interface ListTrie<T> extends Trie {
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
   return construct(new Cons, { head: head, tail: tail })
}
