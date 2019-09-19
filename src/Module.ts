import { Grammar, Parser } from "nearley"
import { __nonNull, as, error } from "./util/Core"
import { List, Pair, pair } from "./BaseTypes"
import { ExplValue } from "./DataValue"
import { Env, ExtendEnv, emptyEnv } from "./Env"
import { Eval } from "./Eval"
import { Expr } from "./Expr"
import "./Graphics" // for datatypes
import grammar from "./Parse"
import { PrimValue, Str } from "./Value"
import { ν, num, str } from "./Versioned"

// Kindergarten modules.
type Module = List<Expr.Def>

// Define as constants to enforce sharing; could use memoisation.
export const module_prelude: Module = loadModule("prelude"),
             module_graphics: Module = loadModule("graphics"),
             module_renderData: Module = loadModule("renderData")

function import_ (modules: Module[], e: Expr): Expr {
   if (modules.length === 0) {
      return e
   } else {
      return Expr.defs(ν(), modules[0], import_(modules.slice(1), e))
   }
}

export function loadTestFile (folder: string, file: string): string {
   let text: string
   const xmlhttp: XMLHttpRequest = new XMLHttpRequest
   xmlhttp.open("GET", "./" + folder + "/" + file + ".lcalc", false)
   xmlhttp.send()
   if (xmlhttp.status === 200) {
      text = xmlhttp.responseText
   }
   return __nonNull(text!)
}

// Not sure if Nearley can parse arbitrary non-terminal, as opposed to root.
export function loadModule (file: string): Module {
   const fileʹ: string = loadTestFile("lcalc/lib", file) + " in 0",
         e: Expr.Defs = as(successfulParse(fileʹ), Expr.Defs)
   return e.def̅
}

export function open (file: string): Expr {
   return openWithImports(file, [])
}

export function openWithImports (file: string, modules: Module[]): Expr {
   return parseWithImports(loadTestFile("lcalc/example", file), modules)
}

export function openDatasetAs (file: string, x: string): ExtendEnv {
   return Env.singleton(str(x), Eval.eval_(emptyEnv(), parseWithImports(loadTestFile("lcalc/dataset", file), [])))
}

export function parseWithImports (src: string, modules: Module[]): Expr {
   return import_([module_prelude], import_(modules, successfulParse(src)))
}

// https://github.com/kach/nearley/issues/276#issuecomment-324162234
export function successfulParse (str: string): Expr {
   const { results }: Parser = new Parser(Grammar.fromCompiled(grammar)).feed(str)
   if (results.length > 1) {
      error("Ambiguous parse.")
   }
   return results[0]
}

export type Record = List<Pair<Str, PrimValue>>

export function createDatasetAs (vs: Object[], x: string): ExtendEnv {
   // This will totally fail, just want something that compiles :-/
   return Env.singleton(str(x), as(List.fromArray(vs.map(asRecord)) as any, ExplValue))
}

function asRecord (v: Object): Record {
   return List.fromArray(Object.getOwnPropertyNames(v).map(k => pair(str(k), asPrimValue((v as any)[k]))))
}

function asPrimValue (v: Object): PrimValue {
   if (typeof v === "number") {
      return num(v)
   } else
   if (typeof v === "string") {
      return str(v)
   } else {
      return error(`Ill-formed data: expected string or number, found ${typeof v}.`)
   }
}
