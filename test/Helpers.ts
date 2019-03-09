import { AClass, Class, __nonNull, absurd, as, assert } from "../src/util/Core"
import { Persistent, PersistentObject } from "../src/util/Persistent"
import { parse } from "../src/util/parse/Core"
import { Cons } from "../src/BaseTypes"
import { initDataTypes } from "../src/DataType"
import { Env } from "../src/Env"
import { Eval } from "../src/Eval"
import { Expr, Lex } from "../src/Expr"
import { singleton, unionWith } from "../src/FiniteMap"
import { instantiate } from "../src/Instantiate"
import { Parse } from "../src/Parse"
import { prelude } from "../src/Primitive"
import { Traced } from "../src/Traced"

import Args = Expr.Args
import Kont = Expr.Kont
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

   toElem (n: number): Cursor {
      if (n === 0) {
         this.to(Cons, "head")
         return this
      } else {
         this.to(Cons, "tail")
         return this.toElem(n - 1)
      }
   }
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
   return __nonNull(parse(Parse.expr, __nonNull(src))).ast
}

export function runExample (e: Expr): Traced {
   const tv: Traced = Eval.eval_(ρ, instantiate(ρ, e))
   console.log(tv)
   return tv
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
