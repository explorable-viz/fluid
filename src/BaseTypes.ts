import { Persistent, PersistentObject, make } from "./util/Persistent"

// Basic datatypes for interned structures.

export type List̊<T extends Persistent> = List<T> | null

export abstract class List<T extends Persistent> implements PersistentObject {
   static fromArray<T extends Persistent> (xs: T[]): List<T> {
      let xs_: List<T> = Nil.make()
      for (let n: number = xs.length - 1; n >= 0; --n) {
         xs_ = Cons.make(xs[n], xs_)
      }
      return xs_
   }

   abstract length: number
   abstract map<U extends Persistent> (f: (t: T) => U): List<U>
   abstract constructor_ (...args: Persistent[]): void // TS requires duplicate def
}

export class Nil<T extends Persistent> extends List<T> {
   constructor_ () {
   }

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
   
   constructor_ (
      head: T,
      tail: List<T>
   ) {
      this.head = head
      this.tail = tail
   }

   static is<T extends Persistent> (xs: List<T>): xs is Cons<T> {
      return xs instanceof Cons
   }

   static make<T extends Persistent> (head: T, tail: List<T>): Cons<T> {
      return make(Cons, head, tail) as Cons<T>
   }

   get length (): number {
      return 1 + this.tail.length
   }

   map<U extends Persistent> (f: (t: T) => U): Cons<U> {
      return Cons.make(f(this.head), this.tail.map(f))
   }
}

export class Pair<T extends Persistent, U extends Persistent> implements PersistentObject {
   fst: T
   snd: U

   constructor_ (
      fst: T,
      snd: U
   ) {
      this.fst = fst
      this.snd = snd
   }

   static make<T extends Persistent, U extends Persistent> (fst: T, snd: U): Pair<T, U> {
      return make(Pair, fst, snd) as Pair<T, U>
   }
}

export abstract class Tree<T extends Persistent> implements PersistentObject {
   abstract map<U extends Persistent> (f: (t: T) => U): Tree<U>
   abstract constructor_ (...args: Persistent[]): void // TS requires duplicate def
}

export class Empty<T extends Persistent> extends Tree<T> {
   constructor_ () {      
   }

   static is<T extends Persistent> (xs: Tree<T>): xs is Empty<T> {
      return xs instanceof Empty
   }

   static make<T extends Persistent> (): Empty<T> {
      return make(Empty) as Empty<T>
   }

   map<U extends Persistent> (f: (t: T) => U): Empty<U> {
      return Empty.make()
   }
}

export class NonEmpty<T extends Persistent> extends Tree<T> {
   left: Tree<T>
   t: T
   right: Tree<T>

   constructor_ (
      left: Tree<T>,
      t: T,
      right: Tree<T>
   ) {
      this.left = left
      this.t = t
      this.right = right
   }

   static is<T extends Persistent> (xs: Tree<T>): xs is NonEmpty<T> {
      return xs instanceof NonEmpty
   }

   static make<T extends Persistent> (left: Tree<T>, t: T, right: Tree<T>): NonEmpty<T> {
      return make(NonEmpty, left, t, right) as NonEmpty<T>
   }

   map<U extends Persistent> (f: (t: T) => U): NonEmpty<U> {
      return NonEmpty.make(this.left.map(f), f(this.t), this.right.map(f))
   }
}
