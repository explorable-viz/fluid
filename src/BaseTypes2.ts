import { Constr, Value, make } from "./ExplVal2"

export abstract class List<T> extends Constr<List<T>> {
}

export class Nil<T> extends List<T> {
   static is<T> (xs: List<T>): xs is Nil<T> {
      return xs instanceof Nil
   }
}

export function nil<T> (): List<T> {
   return make(Nil, {})
}

export class Cons<T extends Value> extends List<T> {
   head: T
   tail: List<T>

   static is<T> (xs: List<T>): xs is Cons<T> {
      return xs instanceof Cons
   }
}

export function cons<T extends Value> (head: T, tail: List<T>): List<T> {
   return make(Cons, { head, tail })
}

export class Pair<T, U> extends Constr<Pair<T, U>> {
   fst: T
   snd: U
}

export abstract class Tree<T> extends Constr<Tree<T>> {
}

export class Empty<T> extends Tree<T> {
}

export class NonEmpty<T> extends Tree<T> {
   left: Tree<T>
   t: T
   right: Tree<T>
}

export function map<T, U> (t: Tree<T>, f: (t: T) => U): Tree<U> {
   throw new Error
}
