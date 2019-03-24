import { __nonNull, assert } from "../src/util/Core"
import { World } from "../src/util/Versioned"
import { parse } from "../src/util/parse/Core"
import { ann, setall } from "../src/Annotated"
import { initDataTypes } from "../src/DataType"
import { Env } from "../src/Env"
import { Eval } from "../src/Eval"
import { ExplVal, Value } from "../src/ExplVal"
import { Expr, Kont } from "../src/Expr"
import { unionWith } from "../src/FiniteMap"
import { instantiate } from "../src/Instantiate"
import { Parse } from "../src/Parse"
import { prelude } from "../src/Primitive"
import { Cursor } from "./Cursor"

import Args = Expr.Args
import Trie = Expr.Trie

export function initialise (): void {
   // Fix the toString impl on String to behave sensibly.
   String.prototype.toString = function (this: String): string {
      return "'" + this + "'"
   }
   initDataTypes()
}

export abstract class FwdSlice {
   expr: Cursor
   val: Cursor

   constructor (e: Expr) {
      World.newRevision()
      setall(e, ann.top) // parser should no longer need to do this
      this.expr = new Cursor(e)
      this.setup()
      this.val = new Cursor(Eval.eval_(ρ, e).v)
      this.expect()
   }

   abstract setup (): void
   abstract expect (): void

   get e (): Expr {
      return this.expr.o as Expr
   }
}

// Precondition: must be safe to reexecute e in the current revision, to obtain a trace.
export abstract class BwdSlice {
   val: Cursor
   expr: Cursor

   constructor (e: Expr) {
      World.newRevision()
      setall(e, ann.bot)
      const tv: ExplVal = Eval.eval_(ρ, e) // just to obtain tv
      setall(tv, ann.bot) // necessary given what I've just done?
      World.newRevision()
      this.val = new Cursor(tv.v)
      this.setup()
      this.expr = new Cursor(Eval.uneval(tv))
      this.expect()
   }

   abstract setup (): void
   abstract expect (): void
}

export enum Profile {
   Parse,
   Run,
   Visualise
}

// Could have used join, but only defined for syntactic tries.
export function merge<K extends Kont<K>> (σ1: Trie.Constr<K>, σ2: Trie.Constr<K>): Trie.Constr<K> {
   return Trie.constr(unionWith(σ1.cases, σ2.cases, (v: Args<K>, vʹ: Args<K>) => assert(false)))
}

export function parseExample (src: string | null): Expr {
   const e: Expr = __nonNull(parse(Parse.expr, __nonNull(src))).ast
   return instantiate(ρ, e)
}

export function runExample (e: Expr): void {
   const tv: ExplVal = Eval.eval_(ρ, e)
   console.log(tv)
   World.newRevision()
   setall(tv, ann.bot)
   World.newRevision()
   const here: Cursor = new Cursor(tv)
   here.to(ExplVal, "v")
       .at(Value.Value, v => v.setα(ann.top))
   let eʹ: Expr = Eval.uneval(tv)
   assert(e === eʹ)
}

export let ρ: Env = prelude()

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
