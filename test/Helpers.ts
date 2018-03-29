/// <reference path="../src/Object.ts" />

import * as $ from "jquery"
import { initDataTypes } from "../src/DataType"
import { Eval } from "../src/Eval"
import { Parse } from "../src/Parse"
import { prelude } from "../src/Primitive"
import { ν } from "../src/Runtime"
import { Env, Expr, Lex, Trie } from "../src/Syntax"
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
const σ: Trie.Trie<null> = Trie.Var.at(ν(), new Lex.Var("x"), null)

export function runExample (p: Profile, src: string): void {
   const e: Expr.Expr = __nonNull(parse(Parse.expr, __nonNull(src))).ast
   if (p >= Profile.Run) {
      const [tv, , ]: Eval.EvalResult<null> = Eval.eval_(baseEnv, σ, e)
      console.log(tv)
   }
}

export let baseEnv: Env = prelude()

export function runTest (prog: string, profile: Profile = defaultProfile): void {
   runExample(profile, prog)
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
