import { make } from "./util/Core"
import { Persistent, PersistentObject } from "./Runtime"

// Basic datatypes for interned structures.

export abstract class List<T extends Persistent> extends PersistentObject {
   __List (): void {
      // discriminator
   }

   static fromArray<T extends Persistent> (xs: T[]): List<T> {
      let xs_: List<T> = Nil.make()
      for (let n: number = xs.length - 1; n >= 0; --n) {
         xs_ = Cons.make(xs[n], xs_)
      }
      return xs_
   }

   abstract length: number
   abstract map<U extends Persistent> (f: (t: T) => U): List<U>
}

export class Nil<T extends Persistent> extends List<T> { 
   static is<T extends Persistent> (xs: List<T>): xs is Nil<T> {
      return xs instanceof Nil
   }

   static make<T extends Persistent> (): Nil<T> {
      return make<Nil<T>>(Nil)
   }

   get length (): number {
      return 0
   }

   map<U extends Persistent> (f: (t: T) => U): Nil<U> {
      return Nil.make()
   }
}

export class Cons<T extends Persistent> extends List<T> {
   head: T
   tail: List<T>

   static is<T extends Persistent> (xs: List<T>): xs is Cons<T> {
      return xs instanceof Cons
   }

   static make<T extends Persistent> (head: T, tail: List<T>): Cons<T> {
      const this_: Cons<T> = make<Cons<T>>(Cons, head, tail)
      this_.head = head
      this_.tail = tail
      return this_
   }

   get length (): number {
      return 1 + this.tail.length
   }

   map<U extends Persistent> (f: (t: T) => U): Nil<U> {
      return Cons.make(f(this.head), this.tail.map(f))
   }
}

export class Pair<T extends Persistent, U extends Persistent> extends PersistentObject {
   fst: T
   snd: U

   static make<T extends Persistent, U extends Persistent> (fst: T, snd: U): Pair<T, U> {
      const this_: Pair<T, U> = make<Pair<T, U>>(Pair, fst, snd)
      this_.fst = fst
      this_.snd = snd
      return this_
   }
}

export class Tree<T extends Persistent> extends PersistentObject {
   __Tree (): void {
      // discriminator
   }
}

export class Empty<T extends Persistent> extends Tree<T> {
   static is<T extends Persistent> (xs: Tree<T>): xs is Empty<T> {
      return xs instanceof Empty
   }

   static make<T extends Persistent> (): Empty<T> {
      return make(Empty)
   }
}

export class NonEmpty<T extends Persistent> extends Tree<T> {
   left: Tree<T>
   t: T
   right: Tree<T>

   static is<T extends Persistent> (xs: Tree<T>): xs is NonEmpty<T> {
      return xs instanceof NonEmpty
   }

   static make<T extends Persistent> (left: Tree<T>, t: T, right: Tree<T>): NonEmpty<T> {
      const this_: NonEmpty<T> = make<NonEmpty<T>>(NonEmpty, left, t, right)
      this_.left = left
      this_.t = t
      this_.right = right
      return this_
   }
}
