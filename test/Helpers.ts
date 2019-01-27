import { __nonNull, assert } from "../src/util/Core"
import { parse } from "../src/util/parse/Core"
import { initDataTypes } from "../src/DataType"
import { Env } from "../src/Env"
import { Eval } from "../src/Eval"
import { Expr, Lex } from "../src/Expr"
import { singleton, unionWith } from "../src/FiniteMap"
import { instantiate } from "../src/Instantiate"
import { match } from "../src/Match"
import { Parse } from "../src/Parse"
import { prelude } from "../src/Primitive"
import { Traced } from "../src/Traced"

import Args = Traced.Args
import Trie = Traced.Trie

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
   Match,
   Visualise
}

export namespace τ {
   export function arg<K> (σ: Trie<Args<K>>): Args.Next<K> {
      return Args.Next.make(σ)
   }

   export function endArgs<K> (κ: K): Args.End<K> {
      return Args.End.make(κ)
   }

   export function var_<K> (κ: K): Trie.Var<K> {
      return Trie.Var.make(new Lex.Var("q"), κ)
   }

   export function int<K> (κ: K): Trie.Prim<K> {
      return Trie.ConstInt.make(κ)
   }

   export function str<K> (κ: K): Trie.Prim<K> {
      return Trie.ConstStr.make(κ)
   }

   export function cons<K> (Π: Args<K>): Trie.Constr<K> {
      return Trie.Constr.make(singleton("Cons", Π))
   }

   export function nil<K> (Π: Args<K>): Trie.Constr<K> {
      return Trie.Constr.make(singleton("Nil", Π))
   }

   export function pair<K> (Π: Args<K>): Trie.Constr<K> {
      return Trie.Constr.make(singleton("Pair", Π))
   }

   export function point<K> (Π: Args<K>): Trie.Constr<K> {
      return Trie.Constr.make(singleton("Point", Π))
   }

   export function some<K> (Π: Args<K>): Trie.Constr<K> {
      return Trie.Constr.make(singleton("Some", Π))
   }

   export function top(): Trie.Top {
      return Trie.Top.make()
   }
}

// Could have used join, but only defined for syntactic tries.
export function merge<K> (σ1: Trie.Constr<K>, σ2: Trie.Constr<K>): Trie.Constr<K> {
   return Trie.Constr.make(unionWith(σ1.cases, σ2.cases, (v: Args<K>, vʹ: Args<K>) => assert(false)))
}

export function runExample (p: Profile, src: string, σ: Trie<null>): Traced | null {
   const e: Expr = __nonNull(parse(Parse.expr, __nonNull(src))).ast
   console.log(Profile[p])
   if (p >= Profile.Run) {
      const [tv, , ]: Eval.Result<null> = Eval.eval_(ρ, instantiate(ρ)(e), σ)
      console.log(tv)
      if (p >= Profile.Match) {
         console.log(match(σ, tv.v))
      }
      return tv
   }
   return null
}

export let ρ: Env = prelude()

export function runTest (prog: string, profile: Profile, σ: Trie<null> = τ.var_(null)): Traced | null {
   return runExample(profile, prog, σ)
}

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
