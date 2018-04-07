import { __getField, __nonNull, className } from "./util/Core"
import { makeUnary } from "./Primitive"
import { Ctr, PersistentObject } from "./Runtime"
import { Trie, Value } from "./Syntax"

// The fields of a constructor have a total ordering independent of their lexicographical ordering.
// This is the order in which they are defined in the class definition (TODO: check). Not interned.
class DataType {
   name: string
   ctrs: Map<string, string[]>  // fields of my constructors

   constructor (name: string, ctrs: Map<string, string[]>) {
      this.name = name
      this.ctrs = ctrs
   }
}

function isField (f: string): boolean {
   return f !== "constructor" && f !== "init" && f !== "toString" && f.substring(0, 2) !== "__"
}

export function fields (v: Object): string[] {
   const proto: Object = __nonNull(Object.getPrototypeOf(v))
   return Object.getOwnPropertyNames(proto).filter(isField)
}

// Populated by initDataTypes(). Constructors are not yet first-class.
export const projections: [string, Value.PrimOp][] = []

// Populated by initDataTypes().
export let ctrToDataType: Map<string, DataType> | null = null

// Create a "blank" object for each constructor of the datatype, then interrogate its prototype
// for fields. As a side-effect, populate ctrToDataType and projections map.
// Emulate "source code" by creating a fresh locations for constructors and fields.
function initDataType <T> (typeName: string, ctrs: Ctr<T>[]): void {
   let ctrDefs: Map<string, string[]> = new Map
   ctrs.map((ctr: Ctr<T>): void => {
      const fs: string[] = fields(new ctr)
      let fs_: string[] = []
      for (let i: number = fs.length - 1; i >= 0; --i) {
         const f: string = fs[i]
         fs_.push(f)
         const proj: (x: Value.Constr) => (α: PersistentObject) => Value.Value =
            (x: Value.Constr) => (α: PersistentObject) => __getField(x, f)
         projections.push([f, makeUnary(proj, Trie.Constr.at])
      }
      ctrDefs.set(className(ctr), fs_)
   })
}
   
export function initDataTypes (): void {
}
