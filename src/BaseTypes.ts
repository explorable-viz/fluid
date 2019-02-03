import { make } from "./util/Core"
import { InternedObject, Persistent } from "./Runtime"

// Basic datatypes for interned structures.

export abstract class List<T extends Persistent> extends InternedObject {
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
   constructor (
      public head: T,
      public tail: List<T>
   ) {
      super()
   }

   static is<T extends Persistent> (xs: List<T>): xs is Cons<T> {
      return xs instanceof Cons
   }

   static make<T extends Persistent> (head: T, tail: List<T>): Cons<T> {
      return make<Cons<T>>(Cons, head, tail)
   }

   get length (): number {
      return 1 + this.tail.length
   }

   map<U extends Persistent> (f: (t: T) => U): Cons<U> {
      return Cons.make(f(this.head), this.tail.map(f))
   }
}

export class Pair<T extends Persistent, U extends Persistent> extends InternedObject {
   constructor (
      public fst: T,
      public snd: U
   ) {
      super()
   }

   static make<T extends Persistent, U extends Persistent> (fst: T, snd: U): Pair<T, U> {
      return make<Pair<T, U>>(Pair, fst, snd)
   }
}

export abstract class Tree<T extends Persistent> extends InternedObject {
   abstract map<U extends Persistent> (f: (t: T) => U): Tree<U>
}

export class Empty<T extends Persistent> extends Tree<T> {
   static is<T extends Persistent> (xs: Tree<T>): xs is Empty<T> {
      return xs instanceof Empty
   }

   static make<T extends Persistent> (): Empty<T> {
      return make<Empty<T>>(Empty)
   }

   map<U extends Persistent> (f: (t: T) => U): Empty<U> {
      return Empty.make()
   }
}

export class NonEmpty<T extends Persistent> extends Tree<T> {
   constructor (
      public left: Tree<T>,
      public t: T,
      public right: Tree<T>
   ) {
      super()
   }

   static is<T extends Persistent> (xs: Tree<T>): xs is NonEmpty<T> {
      return xs instanceof NonEmpty
   }

   static make<T extends Persistent> (left: Tree<T>, t: T, right: Tree<T>): NonEmpty<T> {
      return make<NonEmpty<T>>(NonEmpty, left, t, right)
   }

   map<U extends Persistent> (f: (t: T) => U): NonEmpty<U> {
      return NonEmpty.make(this.left.map(f), f(this.t), this.right.map(f))
   }
}
