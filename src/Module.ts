import { __nonNull } from "./util/Core"
import { successfulParse } from "./util/parse/Core"
import { List } from "./BaseTypes"
import { Env, ExtendEnv, emptyEnv } from "./Env"
import { Eval } from "./Eval"
import { Expr } from "./Expr"
import { Parse } from "./Parse"
import { ν, str } from "./Versioned"

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

function importDefaults (e: Expr): Expr {
   return import_([module_prelude], e)
}

export function loadTestFile (folder: string, file: string): string {
   let text: string
   const xmlhttp: XMLHttpRequest = new XMLHttpRequest
   xmlhttp.open("GET", folder + "/" + file + ".lcalc", false)
   xmlhttp.send()
   if (xmlhttp.status === 200) {
      text = xmlhttp.responseText
   }
   return __nonNull(text!)
}

export function loadModule (file: string): Module {
   return successfulParse(Parse.defList, loadTestFile("lcalc/lib", file))
}

export function open (file: string): Expr {
   return openWithImports(file, [])
}

export function openWithImports (file: string, modules: Module[]): Expr {
   return parseWithImports(loadTestFile("lcalc/example", file), modules)
}

export function openDatasetAs (file: string, x: string): ExtendEnv {
   const e: Expr = parse(loadTestFile("lcalc/dataset", file))
   return Env.singleton(str(ν(), x), Eval.eval_(emptyEnv(), e).v)
}

export function parse (src: string): Expr {
   return importDefaults(successfulParse(Parse.expr, src))
}

export function parseWithImports (src: string, modules: Module[]): Expr {
   return importDefaults(import_(modules, successfulParse(Parse.expr, src)))
}
