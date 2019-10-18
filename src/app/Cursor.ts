import { AClass, Class, __check, __nonNull, absurd, as, assert, className, error } from "../../src/util/Core"
import { ann } from "../../src/util/Lattice"
import { Annotated, annotated, setα } from "../../src/Annotated"
import { Cons, List, NonEmpty, Pair } from "../../src/BaseTypes"
import { exprClass } from "../../src/DataType"
import { DataValue, ExplValue, explValue } from "../../src/DataValue"
import { Change, New } from "../../src/Delta"
import { Expl } from "../../src/Expl"
import { Expr } from "../../src/Expr"
import { DataElim, VarElim } from "../../src/Match"
import { Num, Persistent, State, Str, Value, fields } from "../../src/Value"
import { asVersioned, reset } from "../../src/Versioned"

import DataExpr = Expr.DataExpr
import Def = Expr.Def
import Let = Expr.Let
import LetRec = Expr.LetRec
import Prim = Expr.Prim
import RecDef = Expr.RecDef

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
   ancestors: ExplValue[]
   readonly tv: ExplValue

   constructor (prev: ExplValueCursor | null, tv: ExplValue) {
      super()
      this.ancestors = prev === null ? [] : [...prev.ancestors, prev.tv]
      this.tv = tv
   }

   get annotated (): Annotated & Value {
      return this.tv.t
   }

   to<T extends DataValue> (C: Class<T>, k: keyof T): ExplValueCursor {
      return new ExplValueCursor(this, Expl.explChild(this.tv.t, as(this.tv.v, C), k))
   }

   toBinaryArg1 (opName: string): ExplValueCursor {
      const t: Expl.BinaryApp = as(this.tv.t, Expl.BinaryApp)
      assert(t.opName.val === opName)
      return this.to(Expl.BinaryApp, "tv1")
   }

   toBinaryArg2 (opName: string): ExplValueCursor {
      const t: Expl.BinaryApp = as(this.tv.t, Expl.BinaryApp)
      assert(t.opName.val === opName)
      return this.to(Expl.BinaryApp, "tv2")
   }

   at<T extends Value> (C: AClass<T>, f: (o: T) => void): this {
      f(as<Value, T>(this.tv.v, C))
      return this
   }

   isChanged (s: State): ExplValueCursor {
      assert(asVersioned(this.tv.v).__ẟ.eq(new Change(s)))
      return this
   }

   isUnchanged (): ExplValueCursor {
      assert(asVersioned(this.tv.v).__ẟ.eq(new Change({})))
      return this
   }

   isNew (): ExplValueCursor {
      assert(asVersioned(this.tv.v).__ẟ instanceof New)
      return this
   }

   toTerminal (): ExplValueCursor {
      let t: Expl = this.tv.t
      while (t instanceof Expl.NonTerminal) {
         t = t.t
      }
      return new ExplValueCursor(this, explValue(t, this.tv.v))      
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

   // Allow the data value class to be used to navigate the data expression form.
   constr_to<T extends DataValue> (C: Class<T>, prop: keyof T): ExprCursor {
      return this.to<DataExpr>(exprClass(C), prop as keyof DataExpr)
   }

   toCase<T extends DataValue> (C: Class<T>): ExprCursor {
      const vʹ: Value = __nonNull((as(this.v, DataElim) as any)[C.name])
      return new ExprCursor(vʹ)
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

   treeNodeValue (): ExprCursor {
      return this.to(NonEmpty, "t")
                 .to(Pair, "snd")
   }

   var_ (x: string): ExprCursor {
      this.assert(VarElim, σ => σ.x.val === x)
      return this.to(VarElim, "κ")      
   }

   // Editing API.

   setNum (n: number): ExprCursor {
      reset(this.v, Num, n)
      return this
   }

   setStr (str_: string): ExprCursor {
      reset(this.v, Str, str_)
      return this
   }

   constr_splice<T extends DataValue> (C: Class<T>, props: (keyof T)[], makeNode: (e̅: Expr[]) => Expr[]): ExprCursor {
      return this.splice<DataValue>(
         exprClass(C), 
         props as (keyof DataValue)[], 
         (e̅: Persistent[]): Expr[] => makeNode(e̅.map(e => as(e, Expr.Expr)))
      )
   } 

   splice<T extends Value> (C: Class<T>, props: (keyof T)[], makeNode: (v̅: Persistent[]) => Persistent[]): ExprCursor {
      const v: T = as<Persistent, T>(this.v, C), 
            v̅: Persistent[] = v.__children,
            n̅: number[] = props.map(prop => __check(fields(v).indexOf(prop as string), n => n != -1)),
            v̅ʹ: Persistent[] = makeNode(n̅.map((n: number): Persistent => v̅[n]))
      n̅.forEach((n: number, m: number): void => {
         v̅[n] = v̅ʹ[m]
      })
      reset(v, C, ...v̅)
      return this
   } 
}
