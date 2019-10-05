/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import "./util/Core" // otherwise things mysteriously go wrong
import { Expr } from "../src/Expr"
import { open } from "../src/Module"
import { ExprCursor } from "..//src/app/Cursor"

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
             .setNum(5)
         })
   })
})
