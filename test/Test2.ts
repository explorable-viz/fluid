/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { nil } from "../src/BaseTypes2"
import { eval_ } from "../src/Eval2"
import { Expr } from "../src/Expr2"

before((done: MochaDone) => {
	done()
})

describe("example", () => {
	describe("test", () => {
		it("ok", () => {
         const e: Expr = Expr.constr("Nil", nil())
         eval_(e)
      })
	})
})
