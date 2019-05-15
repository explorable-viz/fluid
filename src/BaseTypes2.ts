import { Constr, Id, Persistent, _, make } from "./Value2"
import { at } from "./Versioned2"

export abstract class Bool extends Constr<"Bool"> {
}

export class True extends Bool {
}

export function true_ (): Bool {
   return make(True)
}

export function trueʹ (k: Id): Bool {
   return at(k, True)
}

export class False extends Bool {
}

export function false_ (): Bool {
   return make(False)
}

export function falseʹ (k: Id): Bool {
   return at(k, False)
}

export abstract class List<T> extends Constr<"List"> {
   abstract map<U extends Persistent> (f: (t: T) => U): List<U>

   static fromArray<T extends Persistent> (x̅: T[]): List<T> {
      let x̅ʹ: List<T> = nil()
      for (let n: number = x̅.length - 1; n >= 0; --n) {
         x̅ʹ = cons(x̅[n], x̅ʹ)
      }
      return x̅ʹ
   }

   toArray (): T[] {
      const x̅: T[] = []
      this.toArray_(x̅)
      return x̅
   }

   abstract toArray_ (x̅: T[]): void
}

export class Nil<T> extends List<T> {
   static is<T> (xs: List<T>): xs is Nil<T> {
      return xs instanceof Nil
   }

   map<U extends Persistent> (f: (t: T) => U): Nil<U> {
      return nil()
   }

   toArray_ (x̅: T[]): void {
   }
}

export function nil<T> (): List<T> {
   return make(Nil) as Nil<T>
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

   toArray_ (x̅: T[]): void {
      x̅.push(this.head)
      this.tail.toArray_(x̅)
   }
}

export function cons<T extends Persistent> (head: T, tail: List<T>): Cons<T> {
   return make(Cons, head, tail) as Cons<T>
}

export class Pair<T, U> extends Constr<"Pair"> {
   fst: T = _
   snd: U = _
}

export function pair<T extends Persistent, U extends Persistent> (fst: T, snd: U): Pair<T, U> {
   return make(Pair, fst, snd) as Pair<T, U>
}

export abstract class Tree<T> extends Constr<"Tree"> {
   abstract map<U extends Persistent> (f: (t: T) => U): Tree<U>

   toArray (): T[] {
      const x̅: T[] = []
      this.toArray_(x̅)
      return x̅
   }

   abstract toArray_ (x̅: T[]): void
}

export class Empty<T> extends Tree<T> {
   static is<T extends Persistent> (t: Tree<T>): t is Empty<T> {
      return t instanceof Empty
   }

   map<U extends Persistent> (f: (t: T) => U): Empty<U> {
      return empty()
   }

   toArray_ (x̅: T[]): void {
   }
}

export function empty<T extends Persistent> (): Empty<T> {
   return make(Empty) as Empty<T>
}

export class NonEmpty<T> extends Tree<T> {
   left: Tree<T> = _
   t: T = _
   right: Tree<T> = _

   static is<T extends Persistent> (t: Tree<T>): t is NonEmpty<T> {
      return t instanceof NonEmpty
   }

   map<U extends Persistent> (f: (t: T) => U): NonEmpty<U> {
      return nonEmpty(this.left.map(f), f(this.t), this.right.map(f))
   }

   toArray_ (x̅: T[]): void {
      this.left.toArray_(x̅)
      x̅.push(this.t)
      this.right.toArray_(x̅)
   }
}

export function nonEmpty <T extends Persistent> (left: Tree<T>, t: T, right: Tree<T>): NonEmpty<T> {
   return make(NonEmpty, left, t, right) as NonEmpty<T>
}

export abstract class Option<T extends Persistent> extends Constr<"Option"> {
}

export class None<T extends Persistent> extends Option<T> {
}

export class Some<T extends Persistent> extends Option<T> {
   t: T = _
}

export abstract class Ordering extends Constr<"Ordering"> {
}

export class LT extends Ordering {
}

export class GT extends Ordering {
}

export class EQ extends Ordering {
}
