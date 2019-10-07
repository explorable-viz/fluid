import { AClass, Class, absurd, as, assert, className, error } from "../../src/util/Core"
import { ann } from "../../src/util/Lattice"
import { Annotated, annotated, setα } from "../../src/Annotated"
import { Cons, List, NonEmpty, Pair } from "../../src/BaseTypes"
import { DataValue, ExplValue, explValue } from "../../src/DataValue"
import { Change, New } from "../../src/Delta"
import { Expl } from "../../src/Expl"
import { Expr } from "../../src/Expr"
import { Num, Persistent, State, Str, Value } from "../../src/Value"

import Def = Expr.Def
import Let = Expr.Let
import LetRec = Expr.LetRec
import Prim = Expr.Prim
import RecDef = Expr.RecDef
import Trie = Expr.Trie

export abstract class Cursor {
   abstract annotated: Annotated & Value
   abstract to<T extends DataValue> (C: Class<T>, k: keyof T): Cursor
   abstract at<T extends Value> (C: AClass<T>, f: (o: T) => void): Cursor

   assert<T extends Value> (C: AClass<T>, pred: (v: T) => boolean): Cursor {
      return this.at(C, v => assert(pred(v)))
   }

   αset (): this {
      assert(this.annotated.__α === ann.top)
      return this
   }

   αclear (): this {
      assert(this.annotated.__α === ann.bot)
      return this
   }

   setα (): this {
      setα(ann.top, this.annotated)
      return this
   }

   clearα (): this {
      setα(ann.bot, this.annotated)
      return this
   }
}

export class ExplValueCursor extends Cursor {
   readonly tv: ExplValue

   constructor (tv: ExplValue) {
      super()
      this.tv = tv
   }

   get annotated (): Annotated & Value {
      return this.tv.t
   }

   to<T extends DataValue> (C: Class<T>, k: keyof T): ExplValueCursor {
      return new ExplValueCursor(Expl.explChild(this.tv.t, as(this.tv.v, C), k))
   }

   toBinaryArg1 (): ExplValueCursor {
      return new ExplValueCursor(as(this.tv.t, Expl.BinaryApp).tv1)
   }

   toBinaryArg2 (): ExplValueCursor {
      return new ExplValueCursor(as(this.tv.t, Expl.BinaryApp).tv2)
   }

   at<T extends Value> (C: AClass<T>, f: (o: T) => void): this {
      f(as<Value, T>(this.tv.v, C))
      return this
   }

   isChanged (s: State): ExplValueCursor {
      assert(this.tv.v.__ẟ.eq(new Change(s)))
      return this
   }

   isUnchanged (): ExplValueCursor {
      assert(this.tv.v.__ẟ.eq(new Change({})))
      return this
   }

   isNew (): ExplValueCursor {
      assert(this.tv.v.__ẟ instanceof New)
      return this
   }

   toTerminal (): ExplValueCursor {
      let t: Expl = this.tv.t
      while (t instanceof Expl.NonTerminal) {
         t = t.t
      }
      return new ExplValueCursor(explValue(t, this.tv.v))      
   }   
}

export class ExprCursor extends Cursor {
   readonly v: Value // would prefer SyntaxNode, but we also traverse "adminstrative" nodes like cons cells.

   constructor (v: Value) {
      super()
      this.v = v
   }

   get annotated (): Annotated & Value {
      if (annotated(this.v)) {
         return this.v
      } else {
         return error(className(this.v) + " is not an annotated value.")
      }
   }

   skipImport (): ExprCursor {
      return this.to(Expr.Defs, "e") // all "modules" have this form
   }

   skipImports (): ExprCursor {
      return this.skipImport() // prelude
   }

   // No way to specify only "own" properties statically.
   to<T extends Value> (C: Class<T>, prop: keyof T): ExprCursor {
      const vʹ: T[keyof T] = as<Persistent, T>(this.v, C)[prop] // TypeScript nonsense
      return new ExprCursor(vʹ as any)
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

   toDef (x: string): ExprCursor {
      const here: ExprCursor = this.to(Expr.Defs, "def̅"),
            defs: Map<string, Let | Prim | RecDef> = ExprCursor.defs(here.v as List<Def>)
      assert(defs.has(x), `No definition of "${x}" found.`)
      return new ExprCursor(defs.get(x)!)
   }

   at<T extends Value> (C: AClass<T>, f: (o: T) => void): ExprCursor {
      f(as<Value, T>(this.v, C))
      return this
   }

   // Helpers specific to certain datatypes.

   toElem (n: number): ExprCursor {
      if (n === 0) {
         return this.to(Cons, "head")
      } else {
         return this.to(Cons, "tail").toElem(n - 1)
      }
   }

   // Not sure what the T parameters are for here...
   constrArg<T extends Value> (ctr: string, n: number): ExprCursor {
      return this.at(Expr.Constr, e => assert(e.ctr.val === ctr, `${e.ctr.val} !== ${ctr}`))
                 .to(Expr.Constr, "args")
                 .toElem(n)
   }

   treeNodeValue (): ExprCursor {
      return this.to(NonEmpty, "t")
                 .to(Pair, "snd")
   }

   var_ (x: string): ExprCursor {
      this.assert(Trie.Var, σ => σ.x.val === x)
      return this.to(Trie.Var, "κ")      
   }

   // Editing API

   setNum (n: number): ExprCursor {
      as(this.v, Num).val = n
      return this
   }

   setStr (str: string): ExprCursor {
      as(this.v, Str).val = str
      return this
   }
}
