import * as $ from "jquery"
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
import { Kont, Trie } from "../src/Traced"

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
   export function arg (σ: Trie): Trie.Next {
      return Trie.Next.make(σ)
   }

   export function endArgs (κ: Kont): Trie.End {
      return Trie.End.make(κ)
   }

   export function var_ (κ: Kont): Trie.Var {
      return Trie.Var.make(new Lex.Var("q"), κ)
   }

   export function int (κ: Kont): Trie.Prim {
      return Trie.ConstInt.make(κ)
   }

   export function str (κ: Kont): Trie.Prim {
      return Trie.ConstStr.make(κ)
   }

   export function cons (Π: Trie.Args): Trie.Constr {
      return Trie.Constr.make(singleton("Cons", Π))
   }

   export function nil (Π: Trie.Args): Trie.Constr {
      return Trie.Constr.make(singleton("Nil", Π))
   }

   export function pair (Π: Trie.Args): Trie.Constr {
      return Trie.Constr.make(singleton("Pair", Π))
   }

   export function some (Π: Trie.Args): Trie.Constr {
      return Trie.Constr.make(singleton("Some", Π))
   }
}

// Could have used join, but only defined for syntactic tries.
export function merge (σ1: Trie.Constr, σ2: Trie.Constr): Trie.Constr {
   return Trie.Constr.make(unionWith(σ1.cases, σ2.cases, (v: Trie.Args, vʹ: Trie.Args) => assert(false)))
}

export function runExample (p: Profile, src: string, σ: Trie): void {
   const e: Expr = __nonNull(parse(Parse.expr, __nonNull(src))).ast
   console.log(Profile[p])
   if (p >= Profile.Run) {
      const [tv, , ]: Eval.Result = Eval.eval_(ρ, instantiate(ρ)(e), σ)
      console.log(tv)
      if (p >= Profile.Match) {
         console.log(match(σ, tv.v))
      }
   }
}

export let ρ: Env = prelude()

export function runTest (prog: string, profile: Profile, σ: Trie = τ.var_(null)): void {
   runExample(profile, prog, σ)
}

// An asychronously loading test file; when loading completes text will be non-null.
export class TestFile {
   text: string | null

   constructor() {
      this.text = null
   }
}

// Maybe there's a way to use ES6 promises instead.
export function loadTestFile(folder: string, file: string): TestFile {
   let testFile: TestFile = new TestFile
   before((done: MochaDone) => {
      const filename: string = folder + "/" + file + ".lcalc"
      $.get(filename, text => {
         testFile.text = text
         console.log("Loaded " + filename)
         done()
      })
   })
   return testFile
}

// For now just see if all the examples run without an exception.
export function testAll (): void {
   initialise()
}
