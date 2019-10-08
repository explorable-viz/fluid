/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { Edit } from "./util/Core"
import { Cons, Pair, NonEmpty } from "../src/BaseTypes"
import { Expr } from "../src/Expr"
import { open } from "../src/Module"
import { ν, str } from "../src/Versioned"
import { ExplValueCursor, ExprCursor } from "..//src/app/Cursor"

import Trie = Expr.Trie
import app = Expr.app
import dataExpr = Expr.dataExpr
import var_ = Expr.var_

before((done: MochaDone) => {
   done()
})

describe("edit", () => {
   describe("arithmetic", () => {
      it("ok", () => {
         const e: Expr = open("arithmetic")
         new (class extends Edit {
            setup (here: ExprCursor) {
               here.skipImports()
                   .to(Expr.BinaryApp, "e1")
                   .to(Expr.BinaryApp, "e2")
                   .to(Expr.ConstNum, "val")
                   .setNum(6)
            }

            expect (here: ExplValueCursor) {
               here.isChanged({ val: 49 })
                   .toTerminal()
                   .toBinaryArg1()
                   .isChanged({ val: 7 })
                   .toTerminal()
                   .toBinaryArg2()
                   .isChanged({ val: 6 })
            }
         })(e)
      })
   })

   describe("filter", () => {
      it("ok", () => {
         const e: Expr = open("filter")
         new (class extends Edit {
            setup (here: ExprCursor) {
               here.skipImports()
                   .to(Expr.App, "f")
                   .to(Expr.App, "e")
                   .to(Expr.Fun, "σ")
                   .to(Trie.Var, "κ")
                   .to(Expr.BinaryApp, "e1")
                   .to(Expr.ConstNum, "val")
                   .setNum(3)
            }

            expect (here: ExplValueCursor) {
               here.isNew()
                   .to(Cons, "head")
                   .isUnchanged()
               here.to(Cons, "tail")
                   .isUnchanged()
                   .to(Cons, "tail")
                   .isUnchanged()
            }
         })(e)
      })
   })

   describe("foldr_sumSquares", () => {
      it("ok", () => {
         const e: Expr = open("foldr_sumSquares")
         new (class extends Edit {
            setup (here: ExprCursor) {
               here.skipImports()
                   .to(Expr.App, "f")
                   .to(Expr.App, "f")
                   .to(Expr.App, "e")
                   .to(Expr.Fun, "σ")
                   .to(Trie.Constr, "cases")
                   .treeNodeValue()
                   .var_("x")
                   .var_("y") // body of clause 
                   .to(Expr.BinaryApp, "opName")
                   .setStr("/")
                   // TODO: finish...
            }

            expect (here: ExplValueCursor) {
            }
         })(e)
      })
   })

   describe("ic2019", () => {
      it("ok", () => {
         const e: Expr = open("ic2019")
         new (class extends Edit {
            setup (here: ExprCursor) {
               here.skipImports()
                   .toDef("f")
                   .to(Expr.RecDef, "σ")
                   .to(Trie.Constr, "cases")
                   .to(NonEmpty, "left") // Cons
                   .treeNodeValue()
                   .var_("x").var_("xs")
                   .spliceConstrArg(Cons, 0, (e: Expr): Expr => {
                      const eʹ: Expr = app(var_(str("sq")(ν()))(ν()), var_(str("x")(ν()))(ν()))(ν())
                      return dataExpr(Pair.name, [e, eʹ])(ν())
                   })
            }

            expect (here: ExplValueCursor) {
               here.to(Cons, "head").isNew().to(Pair, "fst").isUnchanged()
               here.to(Cons, "head").to(Pair, "snd").isNew()
               here = here.to(Cons, "tail")
               here.to(Cons, "head").isNew().to(Pair, "fst").isUnchanged()
               here.to(Cons, "head").to(Pair, "snd").isNew()
            }
         })(e)
      })
   })   
})
