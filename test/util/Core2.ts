import { __nonNull } from "../../src/util/Core"
import { successfulParse } from "../../src/util/parse/Core2"
import { ann } from "../../src/util/Annotated2"
import { emptyEnv } from "../../src/Env2"
import { Eval } from "../../src/Eval2"
import { Expr } from "../../src/Expr2"
import "../../src/Graphics2"
import { Parse } from "../../src/Parse2"
import { ν, setallα } from "../../src/Versioned2"
import { Cursor } from "./Cursor2"

export function initialise (): void {
   // Fix the toString impl on String to behave sensibly.
   String.prototype.toString = function (this: String): string {
      return "'" + this + "'"
   }
}

export class FwdSlice {
   expr: Cursor
   val: Cursor

   constructor (e: Expr) {
      // World.newRevision()
      setallα(e, ann.top)
      this.expr = new Cursor(e)
      this.setup()
      this.val = new Cursor(Eval.eval_(emptyEnv(), e))
      this.expect()
   }

   setup (): void {      
   }

   expect (): void {
   }

   get e (): Expr {
      return this.expr.v as Expr
   }
}


export enum Profile {
   Parse,
   Run,
   Visualise
}

// Kindergarten modules: load another file as though it were a letrec block, with body e.
export function prependModule (src: string, e: Expr): Expr.Defs {
   return Expr.defs(ν(), successfulParse(Parse.defList, src), e)
}

export function parse (src: string): Expr {
   const e: Expr = prependModule(loadLib("prelude"), 
                   prependModule(loadLib("graphics"), 
                   successfulParse(Parse.expr, src)))
   return setallα(e, ann.top)
}

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
