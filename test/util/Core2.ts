import { __nonNull } from "../../src/util/Core"
import { successfulParse } from "../../src/util/parse/Core2"
import { initDataTypes } from "../../src/DataType2"
import { Expr } from "../../src/Expr2"
import { Env } from "../../src/Func2"
import { Parse } from "../../src/Parse2"
import { createPrelude } from "../../src/Primitive2"

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

// Kindergarten modules: load another file as though it were a letrec block, with body e.
export function prependModule (src: string, e: Expr): Expr.LetRec {
   return Expr.letRec(
      successfulParse(Parse.recDefs1, src), e)
}

export function parse (src: string): Expr {
   return prependModule(loadLib("prelude"), 
          prependModule(loadLib("graphics"), 
          successfulParse(Parse.expr, src)))
}

export let prelude: Env = createPrelude()

// An asychronously loading test file; when loading completes text will be non-null.
export class TestFile {
   text: string | null

   constructor () {
      this.text = null
   }
}

// Maybe there's a way to use ES6 promises instead.
export function loadTestFile (folder: string, file: string): string {
   let testFile: TestFile = new TestFile
   const xmlhttp: XMLHttpRequest = new XMLHttpRequest
   xmlhttp.open("GET", folder + "/" + file + ".lcalc", false)
   xmlhttp.send()
   if (xmlhttp.status === 200) {
      testFile.text = xmlhttp.responseText
   }
   return __nonNull(testFile.text)
}

export function load (file: string): string {
	return loadTestFile("example", file)
}

export function loadLib (file: string): string {
	return loadTestFile("example/lib", file)
}
