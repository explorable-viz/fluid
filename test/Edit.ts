/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { Edit } from "./util/Core"
import { Expr } from "../src/Expr"
import { open } from "../src/Module"
import { Delta } from "../src/Value"
import { ExprCursor } from "..//src/app/Cursor"

before((done: MochaDone) => {
   done()
})

describe("edit", () => {
   describe("arithmetic-edit", () => {
      it("ok", () => {
         const e: Expr = open("arithmetic")
         new (class extends Edit {
            setup (here: ExprCursor) {
               here.skipImports()
                   .to(Expr.BinaryApp, "e1")
                   .to(Expr.BinaryApp, "e2")
                   .to(Expr.ConstNum, "val")
                   .setNum(5)
            }

            expect (delta: Delta) {
            }
         })(e)
      })
   })
})
