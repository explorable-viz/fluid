import { Constr, Persistent, _, make } from "./Value2"

export abstract class Bool extends Constr<Bool> {
}

export class True extends Bool {
}

export function true_ (): Bool {
   return make(True)
}

export class False extends Bool {
}

export function false_ (): Bool {
   return make(False)
}

export abstract class List<T> extends Constr<List<T>> {
   abstract map<U extends Persistent> (f: (t: T) => U): List<U>

   toArray (): T[] {
      const x̅: T[] = []
      let y̅: List<T> = this
      for (; Cons.is(y̅); y̅ = y̅.tail) {
         x̅.push(y̅.head)
      }
      return x̅
   }
}

export class Nil<T> extends List<T> {
   static is<T> (xs: List<T>): xs is Nil<T> {
      return xs instanceof Nil
   }

   map<U extends Persistent> (f: (t: T) => U): Nil<U> {
      return nil()
   }
}

export function nil<T> (): List<T> {
   return make<Nil<T>>(Nil)
}

export class Cons<T> extends List<T> {
   head: T = _
   tail: List<T> = _

   static is<T> (xs: List<T>): xs is Cons<T> {
      return xs instanceof Cons
   }

   map<U extends Persistent> (f: (t: T) => U): Cons<U> {
      return cons(f(this.head), this.tail.map(f))
   }
}

export function cons<T extends Persistent> (head: T, tail: List<T>): Cons<T> {
   return make<Cons<T>>(Cons, head, tail)
}

export class Pair<T, U> extends Constr<Pair<T, U>> {
   fst: T = _
   snd: U = _
}

export abstract class Tree<T> extends Constr<Tree<T>> {
}

export class Empty<T> extends Tree<T> {
}

export class NonEmpty<T> extends Tree<T> {
   left: Tree<T> = _
   t: T = _
   right: Tree<T> = _
}

export function map<T, U> (t: Tree<T>, f: (t: T) => U): Tree<U> {
   throw new Error
}
