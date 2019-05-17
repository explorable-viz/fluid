import { ann } from "../../src/util/Annotated2"
import { AClass, Class, absurd, as, assert } from "../../src/util/Core"
import { Persistent, Value } from "../../src/Value2"
import { Cons, NonEmpty, Pair } from "../../src/BaseTypes2"
import { Expr } from "../../src/Expr2"
import { getα, setα } from "../../src/Versioned2"

import Args = Expr.Args
import Trie = Expr.Trie

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
      return this
         .skipImport() // prelude
         .skipImport() // graphics
   }

   // No way to specify only "own" properties statically.
   to<T extends Value> (C: Class<T>, prop: keyof T): Cursor {
      const vʹ: T[keyof T] = as<Persistent, T>(this.v, C)[prop] // TypeScript nonsense
      this.v = vʹ as any as Value
      return this
   }

   toRecDef (fun: string): Cursor {
      this.to(Expr.LetRec, "δ")
      while (true) {
         this.push().toElem(0)
         if (as(this.v, Expr.RecDef).x.val === fun) {
            break
         } else {
            this.pop().to(Cons, "tail")
         }
      }
      return this.pop().toElem(0) // clear stack
   }

   at<T extends Value> (C: AClass<T>, f: (o: T) => void): Cursor {
      f(as<Value, T>(this.v, C))
      return this
   }

   assert<T extends Value> (C: AClass<T>, pred: (v: T) => boolean): Cursor {
      return this.at(C, v => assert(pred(v)))
   }

   needed (): Cursor {
      assert(getα(this.v) === ann.top)
      return this
   }

   notNeeded (): Cursor {
      assert(getα(this.v) === ann.bot)
      return this
   }

   need (): Cursor {
      setα(ann.top, this.v)
      return this
   }

   notNeed (): Cursor {
      setα(ann.bot, this.v)
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

   // Helpers specific to certain data types.

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

   arg<T extends Value> (C: Class<T>, prop: keyof T): Cursor {
      return this.to(Args.Next, "σ")
                 .to(C, prop)
   }

   arg_var<T extends Value> (x: string): Cursor {
      return this.to(Args.Next, "σ")
                 .var_(x)
   }

   end (): Cursor {
      return this.to(Args.End, "κ")
   }

   var_ (x: string): Cursor {
      return this.assert(Trie.Var, σ => σ.x.val === x)
                 .to(Trie.Var, "κ")      
   }
}
