import { ann } from "../../src/util/Annotated"
import { AClass, Class, absurd, as, assert } from "../../src/util/Core"
import { Persistent, PersistentObject } from "../../src/util/Persistent"
import { AnnotatedVersioned } from "../../src/util/Versioned"
import { Cons, NonEmpty, Pair } from "../../src/BaseTypes"
import { ExplVal, Value } from "../../src/ExplVal"
import { Expr } from "../../src/Expr"

import Args = Expr.Args

export class Cursor {
   prev: PersistentObject[] = []
   o: PersistentObject

   constructor (o: PersistentObject) {
      this.goto(o)
   }

   goto (o: PersistentObject): Cursor {
      this.o = o
      return this
   }

   skipPrelude (): Cursor {
      return this.to(Expr.LetRec, "e")
   }

   to<T extends PersistentObject> (cls: Class<T>, prop: keyof T): Cursor {
      const oʹ: T[keyof T] = as<Persistent, T>(this.o, cls)[prop] // TypeScript nonsense
      this.o = oʹ as any as PersistentObject
      return this
   }

   at<T extends PersistentObject> (cls: AClass<T>, f: (o: T) => void): Cursor {
      f(as<PersistentObject, T>(this.o, cls))
      return this
   }

   assert<T extends PersistentObject> (cls: AClass<T>, pred: (o: T) => boolean): Cursor {
      return this.at(cls, o => assert(pred(o)))
   }

   needed (): Cursor {
      return this.assert(AnnotatedVersioned, o => o.α === ann.top)
   }

   notNeeded (): Cursor {
      return this.assert(AnnotatedVersioned, o => o.α === ann.bot)
   }

   need (): Cursor {
      return this.at(AnnotatedVersioned, o => o.setα(ann.top))
   }

   notNeed (): Cursor {
      return this.at(AnnotatedVersioned, o => o.setα(ann.bot))
   }

   push (): Cursor {
      this.prev.push(this.o)
      return this
   }

   pop (): Cursor {
      const o: PersistentObject | undefined = this.prev.pop()
      if (o === undefined) {
         return absurd()
      } else {
         this.o = o
      }
      return this
   }

   // Helpers specific to certain data types.

   toElem (n: number): Cursor {
      if (n === 0) {
         return this.to(Cons, "head")
      } else {
         this.to(Cons, "tail")
         return this.toElem(n - 1)
      }
   }

   constrArg<T extends PersistentObject> (ctr: string, n: number): Cursor {
      return this.at(Expr.Constr, e => assert(e.ctr.str === ctr, `${e.ctr.str} !== ${ctr}`))
                 .to(Expr.Constr, "args")
                 .toElem(n)
   }

   val_constrArg<T extends PersistentObject> (ctr: string, n: number): Cursor {
      return this.at(Value.Constr, e => assert(e.ctr.str === ctr, `${e.ctr.str} !== ${ctr}`))
                 .to(Value.Constr, "args")
                 .toElem(n)
   }

   value (): Cursor {
      return this.to(ExplVal, "v")
   }

   nodeValue (): Cursor {
      return this.to(NonEmpty, "t")
                 .to(Pair, "snd")
   }

   arg<T extends PersistentObject> (cls: Class<T>, prop: keyof T): Cursor {
      return this.to(Args.Next, "σ")
                 .to(cls, prop)
   }

   end (): Cursor {
      return this.to(Args.End, "κ")
   }
}
