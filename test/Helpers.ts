import * as $ from "jquery"
import { initDataTypes } from "../src/DataType"
import { Env } from "../src/Env"
import { Eval } from "../src/Eval"
import { Expr, Lex } from "../src/Expr"
import { singleton } from "../src/FiniteMap"
import { instantiate } from "../src/Instantiate"
import { Parse } from "../src/Parse"
import { prelude } from "../src/Primitive"
import { Kont, Trie } from "../src/Traced"
import { parse } from "../src/util/parse/Core"
import { __nonNull } from "../src/util/Core"

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
   export function arg (σ: Trie): Trie.Cons {
      return Trie.Cons.make(σ)
   }

   export function endArgs (κ: Kont): Trie.Nil {
      return Trie.Nil.make(κ)
   }

   export function var_ (κ: Kont): Trie {
      return Trie.Var.make(new Lex.Var("q"), κ)
   }

   export function int (κ: Kont): Trie {
      return Trie.ConstInt.make(κ)
   }

   export function str (κ: Kont): Trie {
      return Trie.ConstStr.make(κ)
   }

   export function cons (Π: Trie.Args) {
      return Trie.Constr.make(singleton("Cons", Π))
   }

   export function pair (Π: Trie.Args): Trie {
      return Trie.Constr.make(singleton("Pair", Π))
   }

   export function some (Π: Trie.Args): Trie {
      return Trie.Constr.make(singleton("Some", Π))
   }
}

export function runExample (p: Profile, src: string, σ: Trie): void {
   const e: Expr = __nonNull(parse(Parse.expr, __nonNull(src))).ast
   console.log(Profile[p])
   if (p >= Profile.Run) {
      const [tv, , ]: Eval.Result = Eval.eval_(ρ, instantiate(ρ)(e), σ)
      console.log(tv)
      console.log(Eval.match(σ, tv.v))
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
