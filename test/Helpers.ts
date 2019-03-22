import { AClass, Class, __nonNull, absurd, as, assert } from "../src/util/Core"
import { Persistent, PersistentObject, World } from "../src/util/Persistent"
import { parse } from "../src/util/parse/Core"
import { Annotated, ann, setall } from "../src/Annotated"
import { Cons, NonEmpty, Pair } from "../src/BaseTypes"
import { initDataTypes } from "../src/DataType"
import { Env } from "../src/Env"
import { Eval } from "../src/Eval"
import { ExplVal, Value } from "../src/ExplVal"
import { Expr, Kont, Lex } from "../src/Expr"
import { singleton, unionWith } from "../src/FiniteMap"
import { instantiate } from "../src/Instantiate"
import { Parse } from "../src/Parse"
import { prelude } from "../src/Primitive"

import Args = Expr.Args
import Trie = Expr.Trie

export function initialise (): void {
   // Fix the toString impl on String to behave sensibly.
   String.prototype.toString = function (this: String): string {
      return "'" + this + "'"
   }
   initDataTypes()
}

export class Cursor {
   prev: PersistentObject[] = []
   o: PersistentObject

   constructor (o: PersistentObject) {
      this.o = o
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
      return this.assert(Annotated, o => o.α === ann.top)
   }

   notNeeded (): Cursor {
      return this.assert(Annotated, o => o.α === ann.bot)
   }

   need (): Cursor {
      return this.at(Annotated, o => o.setα(ann.top))
   }

   notNeed (): Cursor {
      return this.at(Annotated, o => o.setα(ann.bot))
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
      return this.at(Expr.Constr, e => assert(e.ctr.str === ctr))
                 .to(Expr.Constr, "args")
                 .toElem(n)
   }

   val_constrArg<T extends PersistentObject> (ctr: string, n: number): Cursor {
      return this.at(Value.Constr, e => assert(e.ctr.str === ctr))
                 .to(Value.Constr, "args")
                 .toElem(n)
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

export abstract class FwdSlice {
   constructor (e: Expr) {
      World.newRevision()
      this.setup(new Cursor(e))
      this.expect(new Cursor(Eval.eval_(ρ, e).v))
   }

   abstract setup (expr: Cursor): void
   abstract expect (val: Cursor): void
}

export enum Profile {
   Parse,
   Run,
   Visualise
}

export namespace τ {
   export function arg<K extends Kont<K>> (σ: Trie<Args<K>>): Args.Next<K> {
      return Args.Next.make(σ)
   }

   export function endArgs<K extends Kont<K>> (κ: K): Args.End<K> {
      return Args.End.make(κ)
   }

   export function var_<K extends Kont<K>> (κ: K): Trie.Var<K> {
      return Trie.Var.make(Lex.Var.make("q"), κ)
   }

   export function cons<K extends Kont<K>> (Π: Args<K>): Trie.Constr<K> {
      return Trie.Constr.make(singleton("Cons", Π))
   }

   export function nil<K extends Kont<K>> (Π: Args<K>): Trie.Constr<K> {
      return Trie.Constr.make(singleton("Nil", Π))
   }

   export function pair<K extends Kont<K>> (Π: Args<K>): Trie.Constr<K> {
      return Trie.Constr.make(singleton("Pair", Π))
   }

   export function point<K extends Kont<K>> (Π: Args<K>): Trie.Constr<K> {
      return Trie.Constr.make(singleton("Point", Π))
   }

   export function some<K extends Kont<K>> (Π: Args<K>): Trie.Constr<K> {
      return Trie.Constr.make(singleton("Some", Π))
   }
}

// Could have used join, but only defined for syntactic tries.
export function merge<K extends Kont<K>> (σ1: Trie.Constr<K>, σ2: Trie.Constr<K>): Trie.Constr<K> {
   return Trie.Constr.make(unionWith(σ1.cases, σ2.cases, (v: Args<K>, vʹ: Args<K>) => assert(false)))
}

export function parseExample (src: string | null): Expr {
   const e: Expr = __nonNull(parse(Parse.expr, __nonNull(src))).ast
   return instantiate(ρ, e)
}

export function runExample (e: Expr): void {
   const tv: ExplVal = Eval.eval_(ρ, e)
   console.log(tv)
   World.newRevision()
   setall(tv, ann.bot)
   World.newRevision()
   const here: Cursor = new Cursor(tv)
   here.to(ExplVal, "v")
       .at(Value.Value, v => v.setα(ann.top))
   let eʹ: Expr = Eval.uneval(tv)
   assert(e === eʹ)
}

export let ρ: Env = prelude()

// An asychronously loading test file; when loading completes text will be non-null.
export class TestFile {
   text: string | null

   constructor() {
      this.text = null
   }
}

// Maybe there's a way to use ES6 promises instead.
export function loadTestFile (folder: string, file: string): TestFile {
   let testFile: TestFile = new TestFile
   const xmlhttp = new XMLHttpRequest
   xmlhttp.open("GET", folder + "/" + file + ".lcalc", false)
   xmlhttp.send()
   if (xmlhttp.status === 200) {
     testFile.text = xmlhttp.responseText
   }
   return testFile
}

export function loadExample (file: string): TestFile {
	return loadTestFile("example", file)
}
