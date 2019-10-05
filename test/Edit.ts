/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { BwdSlice } from "./util/Core"
import { Cons, NonEmpty } from "../src/BaseTypes"
import { Expr } from "../src/Expr"
import { open } from "../src/Module"
import { ExprCursor, ExplValueCursor } from "..//src/app/Cursor"

import Trie = Expr.Trie

before((done: MochaDone) => {
   done()
})

// Putting test name in a variable interacts poorly with asynchronous execution.
describe("edit", () => {
   describe("arithmetic-edit", () => {
      it("ok", () => {
         const e: Expr = open("arithmetic"),
               here: ExprCursor = new ExprCursor(e)
         here.skipImports()
             .to(Expr.BinaryApp, "e1")
             .to(Expr.BinaryApp, "e2")
             .to(Expr.ConstNum, "val")
         })
   })

   describe("zipW", () => {
      it("ok", () => {
         const e: Expr = open("zipW")
         // needing constructor of first element requires constructor at head of supplied op, plus application of op in zipW
         new (class extends BwdSlice {
            setup (here: ExplValueCursor): void {
               here.to(Cons, "head").setα()
            }
            expect (): void {
               const here: ExprCursor = this.expr
               let hereʹ: ExprCursor = here
                  .toDef("zipW")
                  .to(Expr.RecDef, "σ")
                  .var_("op")
                  .to(Expr.Fun, "σ")
                  .to(Trie.Constr, "cases")
               hereʹ.treeNodeValue().αclear() // body of outer Nil clause
               hereʹ = hereʹ
                  .to(NonEmpty, "left")
                  .treeNodeValue()          
                  .var_("x").var_("xs").αclear()
                  .to(Expr.Fun, "σ")
                  .to(Trie.Constr, "cases")
            }
         })(e)
      })
   })
})
