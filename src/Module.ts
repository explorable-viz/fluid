import { Grammar, Parser } from "nearley"
import { __nonNull, error } from "./util/Core"
import { SyntaxNode, successfulParse } from "./util/parse/Core"
import { List } from "./BaseTypes"
import { Env, ExtendEnv, emptyEnv } from "./Env"
import { Eval } from "./Eval"
import { Expr } from "./Expr"
import { Parse } from "./Parse"
import * as grammar from "./Parse2"
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

export function importDefaults (e: Expr): Expr {
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
   const e: Expr = parseWithImports(loadTestFile("lcalc/dataset", file), [])
   return Env.singleton(str(ν(), x), Eval.eval_(emptyEnv(), e).v)
}

export function parseWithImports (src: string, modules: Module[]): Expr {
   // return importDefaults(import_(modules, successfulParse(Parse.expr, src)))
   return successfulParse2(src)
}

// https://github.com/kach/nearley/issues/276#issuecomment-324162234
export function successfulParse2<T extends SyntaxNode> (str: string): T {
   const results: any[] = new Parser(Grammar.fromCompiled(grammar)).feed(str).results
   console.log(results)
   if (results.length > 1) {
      error("Ambiguous parse.")
   }
   return results[0]
}
