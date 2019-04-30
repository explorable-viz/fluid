import { __nonNull } from "./util/Core"

class Expl {
}

type State<T> = CoreProps<T>

// Gather the metadata properties associated with T.
interface Metadata<T> {
   __expl?: Map<CoreProps<T>, Expl>
   expl (prop: CoreProps<T>): Expl
   classify (σ: T): void
}

type CoreProps<T> = Pick<T, Exclude<keyof T, keyof Metadata<T>>>

abstract class Explainable<T> implements Metadata<T> {
   expl (prop: CoreProps<T>): Expl {
      return __nonNull(__nonNull(this.__expl).get(prop))
   }

   __expl?: Map<CoreProps<T>, Expl> // Todo: switch to object
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
