import * as $ from "jquery"
import { initDataTypes } from "../src/DataType"
import { Env } from "../src/Env"
import { Eval } from "../src/Eval"
import { singleton } from "../src/FiniteMap"
import { Parse } from "../src/Parse"
import { prelude } from "../src/Primitive"
import { Persistent, ν } from "../src/Runtime"
import { Expr, Lex, Trie } from "../src/Syntax"
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

const defaultProfile = Profile.Parse

export namespace τ {
   export function var_<T> (t: T): Trie<T> {
      return Trie.Var.make(new Lex.Var("x"), t)
   }

   export function int<T> (t: T): Trie<T> {
      return Trie.ConstInt.make(t)
   }

   export function str<T> (t: T): Trie<T> {
      return Trie.ConstStr.make(t)
   }

   export function cons<T extends Persistent> (t: T): Trie<T> {
      return Trie.Constr.at(ν(), singleton("Cons", t))
   }

   export function pair<T extends Persistent> (t: T): Trie<T> {
      return Trie.Constr.at(ν(), singleton("Pair", t))
   }

   export function some<T extends Persistent> (t: T): Trie<T> {
      return Trie.Constr.at(ν(), singleton("Some", t))
   }
}

export function runExample (p: Profile, src: string, σ: Trie<null>): void {
   const e: Expr = __nonNull(parse(Parse.expr, __nonNull(src))).ast
   if (p >= Profile.Run) {
      const [tv, , ]: Eval.EvalResult<null> = Eval.eval_(ρ, e, σ)
      console.log(tv)
   }
}

export let ρ: Env = prelude()

export function runTest (prog: string, profile: Profile = defaultProfile, σ: Trie<null> = τ.var_(null)): void {
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
   console.log("Default test profile: " + Profile[defaultProfile] + ".")
   initialise()
}
