import { Cons, List, Nil, Str, __forEach } from "./BaseTypes"
import * as T from "./BaseTypes"
import { FiniteMap, empty, insert, keys } from "./FiniteMap"
import { keyP } from "./Memo"
import { ITraced, Traced, __getField, __val, constructors, create, projections, ν }  from "./Runtime"
import * as AST from "./Syntax"
import { className, funName } from "./Util"
import * as V from "./View"

// The fields of a constructor have a total ordering independent of their lexicographical ordering.
// This is the order in which they are defined in the class definition (TODO: check).
class DataType {
   _name: ITraced<Str>
   _ctrs: ITraced<FiniteMap<Str, List<Str>>>  // fields of my constructors

   static at (α: Addr, name: ITraced<Str>, ctrs: ITraced<FiniteMap<Str, List<Str>>>): DataType {
      const this_: DataType = create(α, DataType)
      this_._name = name
      this_._ctrs = ctrs
      this_.__version()
      return this_
   }

   static at_ (α: Addr, name: Str, ctrs: FiniteMap<Str, List<Str>>): DataType {
      return DataType.at(α, __val(keyP(α, "name"), name), __val(keyP(α, "ctrs"), ctrs))
   }

   get name (): Str {
      return this._name.val
   }

   get ctrs (): FiniteMap<Str, List<Str>> {
      return this._ctrs.val
   }
}

function isField (f: string): boolean {
   return f !== "constructor" && f !== "init" && f !== "toString" && f.substring(0, 2) !== "__"
}

// Tolerate a null prototype so that we can reify Object.prototype.
export function fields (v: Object): string[] {
   const proto: Object = Object.getPrototypeOf(v)
   if (proto === null) {
      return []
   } else {
      return Object.getOwnPropertyNames(proto).filter(isField)
   }
}

export interface Ctr<T> {
   new (): T
}

// Populated by initDataTypes().
export let ctrToDataType: FiniteMap<Str, DataType> = null

// Create a "blank" object for each constructor of the datatype, then interrogate its prototype
// for fields. As a side-effect, populate ctrToDataType and projections map.
// Emulate "source code" by creating a fresh locations for constructors and fields.
function initDataType <T> (typeName: Str, ctrs: Ctr<T>[]): void {
   let ctrDefs: FiniteMap<Str, List<Str>> = empty<Str, List<Str>>()
   ctrs.map((ctr: Ctr<T>): void => {
      const fs: string[] = fields(new ctr)
      let fs_: List<Str> = Nil.at<Str>(ν())
      for (let i: number = fs.length - 1; i >= 0; --i) {
         const f: string = fs[i]
         fs_ = Cons.at_(ν(), Str.at(ν(), f), fs_)
         projections.set(f, function (x: any): Object {
            // Static field access probably faster, but requires code-generation.
            return __getField(x, f)
         })
      }
      ctrDefs = insert(ctrDefs, Str.at(ν(), className(ctr)), fs_)
   })
   const d: DataType = DataType.at_(ν(), typeName, ctrDefs)
   __forEach(keys(d.ctrs), (ctr: Str): void => {
      ctrToDataType = insert(ctrToDataType, ctr, d)
   })
}

// Run once to reify data types.
export function initDataTypes () {
   ctrToDataType = empty<Str, DataType>()

   function init <T> (d: Ctr<T>, ctrs: Ctr<T>[]): void {
      for (let ctr of ctrs) {
         constructors.set(funName(ctr), ctr)
      }
      initDataType(Str.at(ν(), className(d)), ctrs)
   }

   init(AST.AppBody, [AST.EmptyBody, AST.FunBody, AST.PrimBody])
   init(T.Bool, [T.False, T.True])
   init(T.Box, [T.Box])
   init(AST.Closure, [AST.Closure])
   init(AST.Match, [AST.Match])
   init(DataType, [DataType])
   init(T.List, [T.Cons, T.Nil])
   init(T.Ordering, [T.LT, T.GT, T.EQ])
   init(T.Pair, [T.Pair])
   init(T.Prim.Option, [T.None, T.Some])
   init(AST.PrimOp, [AST.BinaryPrimOp, AST.Proj, AST.UnaryPartialPrimOp, AST.UnaryPrimOp])
   init(AST.RecBinding, [AST.RecBinding])
   init(AST.RecDefinition, [AST.RecDefinition])
   init(AST.Trace, [AST.App, AST.EmptyTrace, AST.Fun, AST.Let, AST.LetRec, AST.MatchAs, AST.OpName, AST.Var])
   init(Traced, [Traced])
   init(T.Tree, [T.Empty, T.NonEmpty])
   init(T.Unit, [T.Unit])
   init(AST.Value, [AST.Constr, AST.ConstInt, AST.ConstStr])
   init(V.View, [V.Background, V.EmptyView, V.Word, V.Horiz, V.RoundedCell, V.Space, V.Vert])
   console.log("Data types initialised.")
}
