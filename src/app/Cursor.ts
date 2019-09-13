import { AClass, Class, absurd, as, assert } from "../../src/util/Core"
import { ann } from "../../src/util/Lattice"
import { annotated } from "../../src/Annotated"
import { Cons, List, NonEmpty, Pair } from "../../src/BaseTypes"
import { DataValue, Expl_ } from "../../src/DataValue"
import { Expl } from "../../src/Expl"
import { Expr } from "../../src/Expr"
import { Persistent, Value } from "../../src/Value"

import Def = Expr.Def
import Let = Expr.Let
import LetRec = Expr.LetRec
import Prim = Expr.Prim
import RecDef = Expr.RecDef
import Trie = Expr.Trie

// TODO: common base class for syntactic forms?
type Annotated = Expr.Expr | Expr.Def | Expr.RecDef | Expl.Expl

function isAnnotated (v: Value): v is Annotated {
   return v instanceof Expr.Expr || v instanceof Expr.Def || v instanceof Expr.RecDef || v instanceof Expl.Expl
}

export class ExplCursor {
   prev: Expl_[] = []
   tv: Expl_

   constructor (tv: Expl_) {
      this.goto(tv)
   }

   goto (tv: Expl_): ExplCursor {
      this.tv = tv
      return this
   }

   to<T extends DataValue> (C: Class<T>, k: keyof T): ExplCursor {
      this.tv = Expl.explChild(this.tv.t, as(this.tv.v, DataValue), k)
      return this
   }

   at<T extends Value> (C: AClass<T>, f: (o: T) => void): this {
      f(as<Value, T>(this.tv.v, C))
      return this
   }

   assert<T extends Value> (C: AClass<T>, pred: (v: T) => boolean): this {
      return this.at(C, v => assert(pred(v)))
   }

   needed (): this {
      assert(annotated(this.tv.t) && this.tv.t.__α === ann.top)
      return this
   }

   notNeeded(): this {
      assert(isAnnotated(this.tv.t) && this.tv.t.__α === ann.bot)
      return this
   }

   need (): this {
      if (isAnnotated(this.tv.t)) {
         this.tv.t.__α = ann.top
      } else {
         assert(false)
      }
      return this
   }

   notNeed(): this {
      if (isAnnotated(this.tv.t)) {
         this.tv.t.__α = ann.top
      } else {
         assert(false)
      }
      return this
   }

   push (): this {
      this.prev.push(this.tv)
      return this
   }

   pop (): this {
      const tv: Expl_ | undefined = this.prev.pop()
      if (tv === undefined) {
         return absurd()
      } else {
         this.tv = tv
      }
      return this
   }
}

export class Cursor {
   prev: Value[] = []
   v: Value

   constructor (v: Value) {
      this.goto(v)
   }

   goto (v: Value): Cursor {
      this.v = v
      return this
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
      this.v = vʹ as any as Value
      return this
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
      this.to(Expr.Defs, "def̅")
      const defs: Map<string, Let | Prim | RecDef> = Cursor.defs(this.v as List<Def>)
      assert(defs.has(x), `No definition of "${x}" found.`)
      return this.goto(defs.get(x)!)
   }

   at<T extends Value> (C: AClass<T>, f: (o: T) => void): Cursor {
      f(as<Value, T>(this.v, C))
      return this
   }

   assert<T extends Value> (C: AClass<T>, pred: (v: T) => boolean): Cursor {
      return this.at(C, v => assert(pred(v)))
   }

   needed (): Cursor {
      assert(annotated(this.v) && this.v.__α === ann.top)
      return this
   }

   notNeeded (): Cursor {
      assert(isAnnotated(this.v) && this.v.__α === ann.bot)
      return this
   }

   need (): Cursor {
      if (isAnnotated(this.v)) {
         this.v.__α = ann.top
      } else {
         assert(false)
      }
      return this
   }

   notNeed (): Cursor {
      if (isAnnotated(this.v)) {
         this.v.__α = ann.bot
      } else {
         assert(false)
      }
      return this
   }

   push (): Cursor {
      this.prev.push(this.v)
      return this
   }

   pop (): Cursor {
      const v: Value | undefined = this.prev.pop()
      if (v === undefined) {
         return absurd()
      } else {
         this.v = v
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
