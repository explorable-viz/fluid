import { __nonNull } from "./util/Core"
import { successfulParse } from "./util/parse/Core"
import { List } from "./BaseTypes"
import { Expr } from "./Expr"
import { Parse } from "./Parse"
import { ν } from "./Versioned"

// Kindergarten modules.
type Module = List<Expr.Def>

const module_prelude: Module = loadModule("prelude"),
      module_graphics: Module = loadModule("graphics"),
      module_renderData: Module = loadModule("renderData")

function import_ (module: Module, e: Expr): Expr.Defs {
   return Expr.defs(ν(), module, e)
}

function importDefaults (e: Expr): Expr {
   return import_(module_prelude, 
          import_(module_graphics,
          import_(module_renderData, e)))
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

export function load (file: string): string {
	return loadTestFile("lcalc/example", file)
}

export function loadData (file: string): string {
	return loadTestFile("lcalc/dataset", file)
}

export function loadModule (file: string): Module {
   return successfulParse(Parse.defList, loadTestFile("lcalc/lib", file))
}

export function parse (src: string): Expr {
   return importDefaults(successfulParse(Parse.expr, src))
}
