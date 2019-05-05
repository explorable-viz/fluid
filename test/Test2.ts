/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { assert } from "../src/util/Core"
import { nil } from "../src/BaseTypes2"
import { fields } from "../src/DataType2"
import { Expr, Lex } from "../src/Expr2"

before((done: MochaDone) => {
	done()
})

describe("example", () => {
	describe("test", () => {
		it("ok", () => {
         Expr.constr(Lex.ctr("Nil"), nil())
         assert(fields(Expr.Constr).toString() === ["ctr", "args"].toString())
//         eval_(e)
      })
	})
})
