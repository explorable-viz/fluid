import { __nonNull, assert } from "../src/util/Core"
import { parse } from "../src/util/parse/Core"
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

   export function int<K extends Kont<K>> (κ: K): Trie.Prim<K> {
      return Trie.ConstInt.make(κ)
   }

   export function str<K extends Kont<K>> (κ: K): Trie.Prim<K> {
      return Trie.ConstStr.make(κ)
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

   export function top<K extends Kont<K>> (κ: K): Trie.Top<K> {
      return Trie.Top.make(κ)
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
