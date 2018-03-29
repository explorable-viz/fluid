/// <reference path="../src/Object.ts" />

import * as $ from "jquery"
import { initDataTypes } from "../src/DataType"
import { Parse } from "../src/Parse"
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

export function runExample (p: Profile, src: string): void {
   __nonNull(parse(Parse.expr, __nonNull(src))).ast
}

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
   console.log("Test profile: " + Profile[profile] + ".")

   // Set the viz code.
   function loadVizCode (): void {
      const readReq = new XMLHttpRequest()
      readReq.open("GET", "visualise.lcalc", true)
      readReq.onload = () => {
         runTest("arithmetic")
      }
      readReq.send(null)
   }

   initialise()
   loadVizCode()
}
