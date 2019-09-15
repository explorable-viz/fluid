import { absurd } from "./util/Core"
import { initDataType } from "./DataType"
import { DataValue } from "./DataValue"
import { dataValue } from "./Eval"
import { Persistent, _, make } from "./Value"

// See Env for convention regarding instance members on reflected datatypes.

export abstract class Bool extends DataValue<"Bool"> {
}

export class True extends Bool {
}

export function true_ (): Bool {
   return dataValue(True.name, []) as Bool
}

export class False extends Bool {
}

export function false_ (): Bool {
   return dataValue(False.name, []) as Bool
}

export abstract class List<T> extends DataValue<"List"> {
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

   toArray_ (x̅: T[]): void {
      if (Cons.is(this)) {
         x̅.push(this.head)
         this.tail.toArray_(x̅)
      } else
      if (Nil.is(this)) {
      } else {
         return absurd()
      }
   }
}

export class Nil<T> extends List<T> {
   static is<T> (xs: List<T>): xs is Nil<T> {
      return xs instanceof Nil
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
}

export function cons<T extends Persistent> (head: T, tail: List<T>): Cons<T> {
   return make(Cons, head, tail) as Cons<T>
}

export class Pair<T, U> extends DataValue<"Pair"> {
   fst: T = _
   snd: U = _
}

export function pair<T extends Persistent, U extends Persistent> (fst: T, snd: U): Pair<T, U> {
   return make(Pair, fst, snd) as Pair<T, U>
}

export abstract class Tree<T extends Persistent> extends DataValue<"Tree"> {
   toArray (): T[] {
      const x̅: T[] = []
      this.toArray_(x̅)
      return x̅
   }

   toArray_ (x̅: T[]): void {
      if (NonEmpty.is(this)) {
         this.left.toArray_(x̅)
         x̅.push(this.t)
         this.right.toArray_(x̅)
      } else 
      if (Empty.is(this)) {
      } else {
         return absurd()
      }
   }
}

export class Empty<T extends Persistent> extends Tree<T> {
   static is<T extends Persistent> (t: Tree<T>): t is Empty<T> {
      return t instanceof Empty
   }
}

export function empty<T extends Persistent> (): Empty<T> {
   return make(Empty) as Empty<T>
}

export class NonEmpty<T extends Persistent> extends Tree<T> {
   left: Tree<T> = _
   t: T = _
   right: Tree<T> = _

   static is<T extends Persistent> (t: Tree<T>): t is NonEmpty<T> {
      return t instanceof NonEmpty
   }
}

export function nonEmpty <T extends Persistent> (left: Tree<T>, t: T, right: Tree<T>): NonEmpty<T> {
   return make(NonEmpty, left, t, right) as NonEmpty<T>
}

export abstract class Option<T extends Persistent> extends DataValue<"Option"> {
}

export class None<T extends Persistent> extends Option<T> {
}

export class Some<T extends Persistent> extends Option<T> {
   t: T = _
}

export abstract class Ordering extends DataValue<"Ordering"> {
}

export class LT extends Ordering {
}

export class GT extends Ordering {
}

export class EQ extends Ordering {
}

initDataType(Bool, [True, False])
initDataType(List, [Nil, Cons])
initDataType(Option, [Some, None])
initDataType(Ordering, [LT, GT, EQ])
initDataType(Pair, [Pair])
initDataType(Tree, [Empty, NonEmpty])
