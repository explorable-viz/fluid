/// <reference path="../src/Object.ts" />

import * as $ from "jquery"
import { Str } from "../src/BaseTypes"
import { initDataTypes } from "../src/DataType"
import { Eval } from "../src/Eval"
import { insert } from "../src/FiniteMap"
import { __def, key, keyP } from "../src/Memo"
import { Parse } from "../src/Parse"
import { parse } from "../src/Parsing"
import { Render } from "../src/Render"
import { ITraced, prelude } from "../src/Runtime"
import { Env } from "../src/Syntax"
import { __nonNull } from "../src/Util"
import { View } from "../src/View"

export function initialise (): void {
   // Fix the toString impl on String to behave sensibly.
   String.prototype.toString = function (this: String): string {
      return "'" + this + "'"
   }
   initDataTypes()
   baseEnv = prelude()
}

export enum Profile {
   Parse,
   Run,
   Visualise
}

const profile = Profile.Parse

export function runExample (p: Profile, src: string): void {
   const e: ITraced = __nonNull(parse(Parse.expr, __nonNull(src))).ast
   if (p >= Profile.Run) {
      const v: ITraced = Eval.eval_(baseEnv)(null)(e).expr
      console.log(v)
      if (p >= Profile.Visualise) {
         visualise(v)
      }
   }
}

__def(visualise)
function visualise (tv: ITraced): ITraced<View> {
   // TODO: supply some demand on the view.
   const α: Addr = key(visualise, arguments),
         ρ: Env = insert(baseEnv, Str.at(keyP(α, '1'), 'tv'), __nonNull(tv)),
         viz_: ITraced<View> = Eval.eval_(ρ)(null)(viz).expr as ITraced<View>
   console.log(viz_.val)
   Render.renderInDoc(viz_.val)
   return viz_
}

let viz: ITraced<View> = null
export let baseEnv: Env = null

export function runTest (prog: string): void {
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
   console.log('Test profile: ' + Profile[profile] + '.')

   // Set the viz code.
   function loadVizCode (): void {
      const readReq = new XMLHttpRequest()
      readReq.open('GET', 'visualise.lcalc', true)
      readReq.onload = () => {
         if (profile >= Profile.Parse) {
            viz = __nonNull(parse(Parse.expr, readReq.responseText)).ast as ITraced<View>
            console.log("Loaded visualisation code.")
         }
         runTest("arithmetic")
      }
      readReq.send(null)
   }

   initialise()
   loadVizCode()
}
