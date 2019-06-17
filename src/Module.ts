import { __nonNull } from "./util/Core"
import { successfulParse } from "./util/parse/Core"
import { List } from "./BaseTypes"
import { Expr } from "./Expr"
import { Parse } from "./Parse"
import { ν } from "./Versioned"

// Kindergarten modules.
type Module = List<Expr.Def>

const module_prelude: Module = successfulParse(Parse.defList, loadLib("prelude")),
      module_graphics: Module = successfulParse(Parse.defList, loadLib("graphics")),
      module_renderData: Module = successfulParse(Parse.defList, loadLib("renderData"))

export function prependModule (module: Module, e: Expr): Expr.Defs {
   return Expr.defs(ν(), module, e)
}

export function importDefaults (e: Expr): Expr {
   return prependModule(module_prelude, 
          prependModule(module_graphics,
          prependModule(module_renderData,e)))
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

export function loadLib (file: string): string {
	return loadTestFile("lcalc/lib", file)
}

export function parse (src: string): Expr {
   return importDefaults(successfulParse(Parse.expr, src))
}
