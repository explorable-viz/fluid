/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { Edit } from "./util/Core"
import { assert } from "../src/util/Core"
import { Change, Delta } from "../src/Delta"
import { Expr } from "../src/Expr"
import { open } from "../src/Module"
import { ExprCursor } from "..//src/app/Cursor"

import Trie = Expr.Trie

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

            expect (ẟ̅ : Delta[]) {
               assert(ẟ̅.length === 3)
               assert(ẟ̅[0].eq(new Change( { val: 6 })))
               assert(ẟ̅[1].eq(new Change( { val: 7 })))
               assert(ẟ̅[2].eq(new Change( { val: 49 })))
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

            expect (ẟ̅ : Delta[]) {
               assert(ẟ̅.length === 0)
            }
         })(e)
      })
   })
})
