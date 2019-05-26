import { __nonNull, as } from "../../src/util/Core"
import { successfulParse } from "../../src/util/parse/Core2"
import { ann } from "../../src/util/Annotated2"
import { emptyEnv } from "../../src/Env2"
import { Eval } from "../../src/Eval2"
import { ExplValue } from "../../src/ExplValue2"
import { Expr } from "../../src/Expr2"
import "../../src/Graphics2"
import { Parse } from "../../src/Parse2"
import { ν, setallα } from "../../src/Versioned2"
import { Cursor } from "./Cursor2"

export class FwdSlice {
   expr: Cursor
   val: Cursor

   constructor (e: Expr) {
      setallα(e, ann.top)
      this.expr = new Cursor(e)
      this.setup()
      const tv: ExplValue = Eval.eval_(emptyEnv(), e)
      if (flags.get(Flags.Fwd)) {
         Eval.eval_fwd(tv)
         this.val = new Cursor(tv.v)
         this.expect()
      }
      console.log(e)
      console.log(tv.v)
   }

   setup (): void {
   }

   expect (): void {
   }

   get e (): Expr {
      return as(this.expr.v, Expr.Expr)
   }
}

export class BwdSlice {
   val: Cursor
   expr: Cursor

   constructor (e: Expr) {
      if (flags.get(Flags.Bwd)) {
         setallα(e, ann.bot)
         const tv: ExplValue = Eval.eval_(emptyEnv(), e) // just to obtain tv
         Eval.eval_fwd(tv) // clear annotations on all values
         this.val = new Cursor(tv.v)
         this.setup()
         this.expr = new Cursor(Eval.eval_bwd(tv))
         this.expect()
      }
   }

   setup (): void {
   }

   expect (): void {      
   }
}

enum Flags { Bwd, Fwd }
const flags: Map<Flags, boolean> = new Map([
   [Flags.Fwd, false],
   [Flags.Bwd, false]
])

// Kindergarten modules: load another file as though it were a defs block, with body e.
export function prependModule (src: string, e: Expr): Expr.Defs {
   return Expr.defs(ν(), successfulParse(Parse.defList, src), e)
}

export function parse (src: string): Expr {
   return prependModule(loadLib("prelude"), 
          prependModule(loadLib("graphics"), 
          successfulParse(Parse.expr, src)))
}

// An asychronously loading test file; when loading completes text will be non-null.
export class TestFile {
   text: string | null

   constructor () {
      this.text = null
   }
}

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
