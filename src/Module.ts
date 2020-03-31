import { Grammar, Parser } from "nearley"
import { __nonNull, as, assert, userError } from "./util/Core"
import { BaseTypes, Cons, List, Nil, Pair } from "./BaseTypes"
import { exprClass } from "./DataType"
import { Env, ExtendEnv, emptyEnv, extendEnv } from "./Env"
import { Eval } from "./Eval"
import { Expr } from "./Expr"
import "./Graphics" // for datatypes
import grammar from "./Parse"
import { PrimValue, Str } from "./Value"
import { ν, at, num, str } from "./Versioned"

// Kindergarten modules.

export namespace Module {
   export let resourceServerUrl: string // should not end in "/", use "." for local run

   // Define as constants to enforce sharing; could use memoisation.
   export let prelude: Env
   export let graphics: Env

   export function initialise (resourceServerUrl: string): void {
      assert(Module.resourceServerUrl === undefined)
      Module.resourceServerUrl = __nonNull(resourceServerUrl, `resourceServerUrl is ${resourceServerUrl}`)
      BaseTypes.initialise()
      prelude = loadModule(emptyEnv(), "prelude")
      graphics = loadModule(prelude, "graphics")
   }
}

function import_ (...modules: Env[]): Env {
   if (modules.length === 0) {
      return emptyEnv()
   } else {
      const [m, ...ms] = modules
      return m.concat(import_(...ms))
   }
}

export function loadFile (folder: string, file: string): string {
   assert(Module.resourceServerUrl !== undefined, "Module system not initialised.")
   let text: string
   const xmlhttp: XMLHttpRequest = new XMLHttpRequest
   const url: string = Module.resourceServerUrl + "/" + folder + "/" + file + ".fld"
   console.log(`Opening ${url}`)
   xmlhttp.open("GET", url, false)
   xmlhttp.send()
   if (xmlhttp.status === 200) {
      text = xmlhttp.responseText
   }
   if (text! === undefined) {
      return assert(false, `${url} could not be loaded.`)
   } else  {
      return text!
   }
}

// Not sure if Nearley can parse arbitrary non-terminal, as opposed to root.
export function loadModule (ρ: Env, file: string): Env {
   const src: string = loadFile("fluid/lib", file)
   const srcʹ: string = src + " in 0"
   const e: Expr.Defs = as(successfulParse(srcʹ), Expr.Defs)
   return Eval.defs(ρ, e.def̅, emptyEnv())[1]
}

export function openWithImports (file: string, ...modules: Env[]): [Env, Expr] {
   return parseWithImports(loadFile("fluid/example", file), ...modules)
}

export function openDatasetAs (file: string, x: string): ExtendEnv {
   const [ρ, e]: [Env, Expr] = parseWithImports(loadFile("fluid/dataset", file))
   return Env.singleton(str(x)(ν()), Eval.eval_(ρ, e))
}

export function parseWithImports (src: string, ...modules: Env[]): [Env, Expr] {
   assert(Module.resourceServerUrl !== undefined, "Module system not initialised.")
   return [import_(__nonNull(Module.prelude), __nonNull(Module.graphics), ...modules), successfulParse(src)]
}

// https://github.com/kach/nearley/issues/276#issuecomment-324162234
export function successfulParse (str: string): Expr {
   const { results }: Parser = new Parser(Grammar.fromCompiled(grammar)).feed(str)
   if (results.length > 1) {
      userError("Ambiguous parse.")
   } else
   if (results.length === 0) {
      userError("Unsuccessful parse.")
   }
   return results[0]
}

export type Record = List<Pair<Str, PrimValue>> // entry in dataset

// create an expression and evaluate it, so we have an explained value
export function bindDataset (ρ: Env, vs: Object[], x: string): ExtendEnv {
   return extendEnv(ρ, str(x)(ν()), Eval.eval_(ρ, asList(vs.map(asRecord))))
}

function asRecord (v: Object): Expr {
   return asList(Object.getOwnPropertyNames(v).map(k => asPair(k, (v as any)[k])))
}

function asPair (k: string, v: unknown): Expr {
   return at(exprClass(Pair), asPrimValue(k), asPrimValue(v))(ν())
}

function asList (e̅: Expr[]): Expr {
   let e̅ʹ: Expr = at(exprClass(Nil))(ν())
   for (let e of [...e̅].reverse()) {
      e̅ʹ = at(exprClass(Cons), e, e̅ʹ)(ν())
   }
   return e̅ʹ
}

function asPrimValue (v: unknown): Expr {
   if (typeof v === "number") {
      return Expr.constNum(num(v)(ν()))(ν())
   } else
   if (typeof v === "string") {
      return Expr.constStr(str(v)(ν()))(ν())
   } else {
      return userError(`Ill-formed data: expected string or number, found ${typeof v}.`)
   }
}
