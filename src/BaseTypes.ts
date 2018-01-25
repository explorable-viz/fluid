import { __def, key, keyP } from "./Memo"
import { Ord } from "./Ord"
import { ITraced, __val, create, typeCheck_ } from "./Runtime"
import { assert, typeCheck } from "./Util"

export class Int {
   val: number

   static at (α: Addr, val: number): Int {
      const this1: Int = create(α, Int)
      this1.val = val
      this1.__version()
      return this1
   }

   toString (): string {
      return this.val.toString()
   }
}

export class Str implements Ord<Str> {
   val: string

   static at (α: Addr, val: string): Str {
      const this1: Str = create(α, Str)
      this1.val = val
      this1.__version()
      return this1
   }

   toString (): string {
      return this.val
   }

   leq (that: Str): boolean {
      return this.val <= that.val
   }
}

export class Unit {
   static at (α: Addr): Unit {
      const this1: Unit = create(α, Unit)
      this1.__version()
      return this1
   }
}

export class Box<T> {
   _unbox: ITraced<T>;

   static at <T> (α: Addr, unbox: ITraced<T>): Box<T> {
      const this1: Box<T> = create(α, Box)
      this1._unbox = typeCheck(unbox, ITraced)
      this1.__version()
      return this1
   }

   get unbox (): T {
      return this._unbox.val
   }
}

export class Bool {
}

export class True extends Bool {
   static at (α: Addr): True {
      const this1: True = create(α, True)
      this1.__version()
      return this1
   }
}

export class False extends Bool {
   static at (α: Addr): False {
      const this1: False = create(α, False)
      this1.__version()
      return this1
   }
}

export module Prim {
   export class Option<T> {
      __visit<U> (v: OptionVisitor<T, U>): U {
         return assert(false)
      }
   }

}

export interface OptionVisitor<T, U> {
   is_None (x: None<T>): U
   is_Some (x: Some<T>): U
}

export class None<T> extends Prim.Option<T> {
   static at <T> (α: Addr): None<T> {
      const this1: None<T> = create(α, None)
      this1.__version()
      return this1
   }

   __visit <U> (v: OptionVisitor<T, U>): U {
      return v.is_None(this)
   }
}

export class Some<T> extends Prim.Option<T> {
   _valOf: ITraced<T>;

   static at <T> (α: Addr, valOf: ITraced<T>): Some<T> {
      const this1: Some<T> = create(α, Some)
      this1._valOf = typeCheck(valOf, ITraced)
      this1.__version()
      return this1
   }

   static at_ <T> (α: Addr, valOf: T): Some<T> {
      return Some.at(α, __val(keyP(α, 'valOf'), valOf))
   }

   __visit <U> (v: OptionVisitor<T, U>): U {
      return v.is_Some(this)
   }

   get valOf (): T {
      return this._valOf.val
   }
}

export class Pair<T, U> {
   _fst: ITraced<T>;
   _snd: ITraced<U>;

   static at <T, U> (α: Addr, fst: ITraced<T>, snd: ITraced<U>): Pair<T, U> {
      const this1: Pair<T, U> = create(α, Pair)
      this1._fst = typeCheck(fst, ITraced)
      this1._snd = typeCheck(snd, ITraced)
      this1.__version()
      return this1
   }

   static at_ <T, U> (α: Addr, fst: T, snd: U): Pair<T, U> {
      return Pair.at(α, __val(keyP(α, 'fst'), fst), __val(keyP(α, 'snd'), snd))
   }

   get fst (): T {
      return this._fst.val
   }

   get snd (): U {
      return this._snd.val
   }
}

export class Ordering {
}

export class LT extends Ordering {
   static at (α: Addr): LT {
      const this1: LT = create(α, LT)
      this1.__version()
      return this1
   }
}

export class GT extends Ordering {
   static at (α: Addr): GT {
      const this1: GT = create(α, GT)
      this1.__version()
      return this1
   }
}

export class EQ extends Ordering {
   static at (α: Addr): EQ {
      const this1: LT = create(α, EQ)
      this1.__version()
      return this1
   }
}

export class List<T> {
   __visit<U> (v: ListVisitor<T, U>): U {
      return assert(false)
   }
}

export interface ListVisitor<T, U> {
   is_Nil (xs: Nil<T>): U
   is_Cons (xs: Cons<T>): U
}

export class Nil<T> extends List<T> {
   static at <T> (α: Addr): Nil<T> {
      const this1: Nil<T> = create(α, Nil)
      this1.__version()
      return this1
   }

   __visit <U> (v: ListVisitor<T, U>): U {
      return v.is_Nil(this)
   }
}

export class Cons<T> extends List<T> {
   _head: ITraced<T>;
   _tail: ITraced<List<T>>;

   static at <T> (α: Addr, head: ITraced<T>, tail: ITraced<List<T>>): Cons<T> {
      const this1: Cons<T> = create(α, Cons)
      this1._head = typeCheck(head, ITraced)
      this1._tail = typeCheck_(tail, List)
      this1.__version()
      return this1
   }

   static at_ <T> (α: Addr, head: T, tail: List<T>): Cons<T> {
      return Cons.at(α, __val(keyP(α, 'head'), head), __val(keyP(α, 'tail'), tail))
   }

   __visit <U> (v: ListVisitor<T, U>): U {
      return v.is_Cons(this)
   }

   get head (): T {
      return this._head.val
   }

   get tail (): List<T> {
      return this._tail.val
   }
}

export class Tree<T> {
   __visit<U> (v: TreeVisitor<T, U>): U {
      return assert(false)
   }
}

export interface TreeVisitor<T, U> {
   is_Empty (t: Empty<T>): U
   is_NonEmpty (t: NonEmpty<T>): U
}

export class Empty<T> extends Tree<T> {
   static at <T> (α: Addr): Empty<T> {
      const this1: Empty<T> = create(α, Empty)
      this1.__version()
      return this1
   }

   __visit <U> (v: TreeVisitor<T, U>): U {
      return v.is_Empty(this)
   }
}

export class NonEmpty<T> extends Tree<T> {
   _left: ITraced<Tree<T>>;
   _t: ITraced<T>;
   _right: ITraced<Tree<T>>;

   static at <T> (α: Addr, left: ITraced<Tree<T>>, t: ITraced<T>, right: ITraced<Tree<T>>): NonEmpty<T> {
      const this1: NonEmpty<T> = create(α, NonEmpty)
      this1._left = typeCheck_(left, Tree)
      this1._t = typeCheck(t, ITraced)
      this1._right = typeCheck_(right, Tree)
      this1.__version()
      return this1
   }

   static at_ <T> (α: Addr, left: Tree<T>, t: T, right: Tree<T>): NonEmpty<T> {
      return NonEmpty.at(
         α,
         __val(keyP(α, 'left'), left),
         __val(keyP(α, 't'), t),
         __val(keyP(α, 'right'), right)
      )
   }

   __visit <U> (v: TreeVisitor<T, U>): U {
      return v.is_NonEmpty(this)
   }

   get left (): Tree<T> {
      return this._left.val
   }

   get t (): T {
      return this._t.val
   }

   get right (): Tree<T> {
      return this._right.val
   }
}

__def(map)
export function map <T, U> (xs: List<T>, f: (x: T) => U): List<U> {
   const α: Addr = key(map, arguments)
   return xs.__visit({
      is_Nil: (xs: Nil<T>) =>
         Nil.at(α),
      is_Cons: (xs: Cons<T>) =>
         Cons.at_(α, f(xs.head), map(xs.tail, f))
   })
}

__def(nil)
export function nil <T> (): List<T> {
   return Nil.at(key(nil, arguments))
}

__def(cons)
export function cons <T> (x: T, xs: List<T>) {
   return Cons.at_(key(cons, arguments), x, xs)
}

__def(zip)
export function zip <T, U> (xs1: List<T>, xs2: List<U>): List<Pair<T, U>> {
   const α: Addr = key(zip, arguments)
   return xs1.__visit({
      is_Nil: (xs1: Nil<T>) =>
         Nil.at(α),
      is_Cons: (xs1: Cons<T>) =>
         xs2.__visit({
            is_Nil: (xs2: Nil<U>) =>
               Nil.at(α),
            is_Cons: (xs2: Cons<U>) =>
               Cons.at_(α,
                  Pair.at_(keyP(α, 'head', 'v'), xs1.head, xs2.head),
                  zip(xs1.tail, xs2.tail)
               )
         })
   })
}

export function __toArray <T> (xs: List<T>): T[] {
   return xs.__visit({
      is_Nil: (xs: Nil<T>): T[] =>
         [],
      is_Cons: (xs: Cons<T>): T[] => {
         const xs_: T[] = __toArray(xs.tail)
         xs_.unshift(xs.head)
         return xs_
      }
   })
}

export function __toList <T> (xs: T[]): List<T> {
   var ys: List<T> = nil<T>()
   for (var i: number = xs.length - 1; i >= 0; --i)
      ys = cons(xs[i], ys)
   return ys
}

export function __forEach <T> (xs: List<T>, f: (t: T) => void): void {
   return xs.__visit({
      is_Nil (xs: Nil<T>): void {
      },
      is_Cons (xs: Cons<T>): void {
         f(xs.head)
         __forEach(xs.tail, f)
      }
   })
}
