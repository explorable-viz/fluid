import { Persistent, PersistentObject, make } from "./util/Persistent"

// Basic datatypes for interned structures.

export abstract class List<T extends Persistent> implements PersistentObject {
   static fromArray<T extends Persistent> (x̅: T[]): List<T> {
      let x̅ʹ: List<T> = nil()
      for (let n: number = x̅.length - 1; n >= 0; --n) {
         x̅ʹ = cons(x̅[n], x̅ʹ)
      }
      return x̅ʹ
   }

   abstract length: number
   abstract map<U extends Persistent> (f: (t: T) => U): List<U>
   abstract constructor_ (...v̅: Persistent[]): void // TS requires duplicate def
}

export class Nil<T extends Persistent> extends List<T> {
   constructor_ () {
   }

   static is<T extends Persistent> (x̅: List<T>): x̅ is Nil<T> {
      return x̅ instanceof Nil
   }

   get length (): number {
      return 0
   }

   map<U extends Persistent> (f: (t: T) => U): Nil<U> {
      return nil()
   }
}

export function nil<T extends Persistent> (): Nil<T> {
   return make<Nil<T>>(Nil)
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

   static is<T extends Persistent> (x̅: List<T>): x̅ is Cons<T> {
      return x̅ instanceof Cons
   }

   get length (): number {
      return 1 + this.tail.length
   }

   map<U extends Persistent> (f: (t: T) => U): Cons<U> {
      return cons(f(this.head), this.tail.map(f))
   }
}

export function cons<T extends Persistent> (head: T, tail: List<T>): Cons<T> {
   return make(Cons, head, tail) as Cons<T>
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
}

export function pair<T extends Persistent, U extends Persistent> (fst: T, snd: U): Pair<T, U> {
   return make(Pair, fst, snd) as Pair<T, U>
}

export abstract class Tree<T extends Persistent> implements PersistentObject {
   abstract map<U extends Persistent> (f: (t: T) => U): Tree<U>
   abstract constructor_ (...v̅: Persistent[]): void 
}

export class Empty<T extends Persistent> extends Tree<T> {
   constructor_ () {      
   }

   static is<T extends Persistent> (t: Tree<T>): t is Empty<T> {
      return t instanceof Empty
   }

   map<U extends Persistent> (f: (t: T) => U): Empty<U> {
      return empty()
   }
}

export function empty<T extends Persistent> (): Empty<T> {
   return make(Empty) as Empty<T>
}

export class NonEmpty<T extends Persistent> extends Tree<T> {
   left: Tree<T>
   t: T
   right: Tree<T>

   constructor_ (
      left: Tree<T>,
      t: T,
      right: Tree<T>
   ): void {
      this.left = left
      this.t = t
      this.right = right
   }

   static is<T extends Persistent> (t: Tree<T>): t is NonEmpty<T> {
      return t instanceof NonEmpty
   }

   map<U extends Persistent> (f: (t: T) => U): NonEmpty<U> {
      return nonEmpty(this.left.map(f), f(this.t), this.right.map(f))
   }
}

export function nonEmpty <T extends Persistent> (left: Tree<T>, t: T, right: Tree<T>): NonEmpty<T> {
   return make(NonEmpty, left, t, right) as NonEmpty<T>
}

export abstract class Option<T extends Persistent> implements PersistentObject {
   __tag: "Option"
   abstract constructor_ (...v̅: Persistent[]): void 
}

export class None<T extends Persistent> extends Option<T> {
   constructor_ (): void {
   }

   static is<T extends Persistent> (x: Option<T>): x is None<T> {
      return x instanceof None
   }
}

export function none<T extends Persistent> (): None<T> {
   return make(None)
}

export class Some<T extends Persistent> extends Option<T> {
   t: T

   constructor_ (t: T): void {
      this.t = t
   }

   static is<T extends Persistent> (x: Option<T>): x is Some<T> {
      return x instanceof Some
   }
}

export function some <T extends Persistent> (t: T): Some<T> {
   return make(Some, t) as Some<T>
}
