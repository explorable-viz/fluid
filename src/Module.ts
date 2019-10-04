import { Grammar, Parser } from "nearley"
import { __nonNull, as, error } from "./util/Core"
import { Cons, List, Nil, Pair } from "./BaseTypes"
import { Env, ExtendEnv, emptyEnv, extendEnv } from "./Env"
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
   } else
   if (results.length === 0) {
      error("Unsuccessful parse.")
   }
   return results[0]
}

export type Record = List<Pair<Str, PrimValue>> // entry in dataset

// create an expression and evaluate it, so we have an explained value
export function bindDataset (ρ: Env, vs: Object[], x: string): ExtendEnv {
   return extendEnv(ρ, str(x), Eval.eval_(ρ, asList(vs.map(asRecord))))
}

function asRecord (v: Object): Expr {
   return asList(Object.getOwnPropertyNames(v).map(k => asPair(k, (v as any)[k])))
}

function asPair (k: string, v: any): Expr {
   return Expr.constr(ν(), str(Pair.name), List.fromArray([asPrimValue(k), asPrimValue(v)]))
}

function asList (e̅: Expr[]): Expr {
   let e̅ʹ: Expr = Expr.constr(ν(), str(Nil.name), List.fromArray([]))
   for (let e of [...e̅].reverse()) {
      e̅ʹ = Expr.constr(ν(), str(Cons.name), List.fromArray([e, e̅ʹ]))
   }
   return e̅ʹ
}

function asPrimValue (v: any): Expr {
   if (typeof v === "number") {
      return Expr.constNum(ν(), num(v)(ν()))
   } else
   if (typeof v === "string") {
      return Expr.constStr(ν(), str(v))
   } else {
      return error(`Ill-formed data: expected string or number, found ${typeof v}.`)
   }
}
