import { AClass, Class, absurd, as, assert } from "../../src/util/Core"
import { ann } from "../../src/util/Lattice"
import { annotated, setα } from "../../src/Annotated"
import { Cons, List, NonEmpty, Pair } from "../../src/BaseTypes"
import { DataValue, ExplValue } from "../../src/DataValue"
import { Expl } from "../../src/Expl"
import { Expr } from "../../src/Expr"
import { Persistent, Value } from "../../src/Value"

import Def = Expr.Def
import Let = Expr.Let
import LetRec = Expr.LetRec
import Prim = Expr.Prim
import RecDef = Expr.RecDef
import Trie = Expr.Trie

export class ExplCursor {
   readonly tv: ExplValue

   constructor (tv: ExplValue) {
      this.tv = tv
   }

   to<T extends DataValue> (C: Class<T>, k: keyof T): ExplCursor {
      return new ExplCursor(Expl.explChild(this.tv.t, as(this.tv.v, DataValue), k))
   }

   at<T extends Value> (C: AClass<T>, f: (o: T) => void): this {
      f(as<Value, T>(this.tv.v, C))
      return this
   }

   assert<T extends Value> (C: AClass<T>, pred: (v: T) => boolean): this {
      return this.at(C, v => assert(pred(v)))
   }

   αset (): this {
      assert(annotated(this.tv.t) && this.tv.t.__α === ann.top)
      return this
   }

   αclear (): this {
      assert(annotated(this.tv.t) && this.tv.t.__α === ann.bot)
      return this
   }

   setα (): this {
      if (annotated(this.tv.t)) {
         setα(ann.top, this.tv.t)
      } else {
         assert(false)
      }
      return this
   }

   clearα (): this {
      if (annotated(this.tv.t)) {
         setα(ann.bot, this.tv.t)
      } else {
         assert(false)
      }
      return this
   }
}

export class Cursor {
   readonly v: Value

   constructor (v: Value) {
      this.v = v
   }

   skipImport (): Cursor {
      return this.to(Expr.Defs, "e") // all "modules" have this form
   }

   skipImports (): Cursor {
      return this.skipImport() // prelude
   }

   // No way to specify only "own" properties statically.
   to<T extends Value> (C: Class<T>, prop: keyof T): Cursor {
      const vʹ: T[keyof T] = as<Persistent, T>(this.v, C)[prop] // TypeScript nonsense
      return new Cursor(vʹ as any)
   }

   static defs (defs: List<Def>): Map<string, Let | Prim | RecDef> {
      const defsʹ: Map<string, Let | Prim | RecDef> = new Map
      for (; Cons.is(defs); defs = defs.tail) {
         const def: Def = defs.head
         if (def instanceof Let || def instanceof Prim) {
            defsʹ.set(def.x.val, def)
         } else
         if (def instanceof LetRec) {
            for (let recDefs: List<RecDef> = def.δ; Cons.is(recDefs); recDefs = recDefs.tail) {
               const recDef: RecDef = recDefs.head
               defsʹ.set(recDef.x.val, recDef)
            }
         } else {
            absurd()
         }
      }
      return defsʹ
   }

   toDef (x: string): Cursor {
      const here: Cursor = this.to(Expr.Defs, "def̅"),
            defs: Map<string, Let | Prim | RecDef> = Cursor.defs(here.v as List<Def>)
      assert(defs.has(x), `No definition of "${x}" found.`)
      return new Cursor(defs.get(x)!)
   }

   at<T extends Value> (C: AClass<T>, f: (o: T) => void): Cursor {
      f(as<Value, T>(this.v, C))
      return this
   }

   assert<T extends Value> (C: AClass<T>, pred: (v: T) => boolean): Cursor {
      return this.at(C, v => assert(pred(v)))
   }

   αset (): Cursor {
      assert(annotated(this.v) && this.v.__α === ann.top)
      return this
   }

   αclear (): Cursor {
      assert(annotated(this.v) && this.v.__α === ann.bot)
      return this
   }

   setα (): Cursor {
      if (annotated(this.v)) {
         setα(ann.top, this.v)
      } else {
         assert(false)
      }
      return this
   }

   clearα (): Cursor {
      if (annotated(this.v)) {
         setα(ann.bot, this.v)
      } else {
         assert(false)
      }
      return this
   }

   // Helpers specific to certain datatypes.

   toElem (n: number): Cursor {
      if (n === 0) {
         return this.to(Cons, "head")
      } else {
         this.to(Cons, "tail")
         return this.toElem(n - 1)
      }
   }

   // Not sure what the T parameters are for here...
   constrArg<T extends Value> (ctr: string, n: number): Cursor {
      return this.at(Expr.Constr, e => assert(e.ctr.val === ctr, `${e.ctr.val} !== ${ctr}`))
                 .to(Expr.Constr, "args")
                 .toElem(n)
   }

   nodeValue (): Cursor {
      return this.to(NonEmpty, "t")
                 .to(Pair, "snd")
   }

   var_ (x: string): Cursor {
      return this.assert(Trie.Var, σ => σ.x.val === x)
                 .to(Trie.Var, "κ")      
   }
}
