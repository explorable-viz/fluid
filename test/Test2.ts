/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { FwdSlice, load, parse } from "./util/Core2"
import { emptyEnv } from "../src/Env2"
import { Eval } from "../src/Eval2"
import { Expr } from "../src/Expr2"

before((done: MochaDone) => {
	done()
})

// putting test name in a variable interacts poorly with asynchronous execution
describe("example", () => {
	describe("arithmetic", () => {
		it("ok", () => {
         const e: Expr = parse(load("arithmetic"))
			new (class extends FwdSlice {
				setup (): void {
					this.expr
						.skipImports()
						.to(Expr.BinaryApp, "e1").notNeed()
				}
				expect (): void {
					this.val.notNeeded()
				} 
			})(e)
		})
   })

   describe("bar-chart", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("bar-chart"))))
		})
   })

   describe("compose", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("compose"))))
		})
	})

   describe("factorial", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("factorial"))))
		})
	})

   describe("filter", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("filter"))))
		})
	})

   describe("foldr_sumSquares", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("foldr_sumSquares"))))
		})
	})

   describe("length", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("length"))))
		})
	})

	describe("lexicalScoping", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("lexicalScoping"))))
		})
	})

   describe("lookup", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("lookup"))))
		})
	})

   describe("map", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("map"))))
		})
	})

   describe("mergeSort", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("mergeSort"))))
		})
	})

	describe("normalise", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("normalise"))))
		})
   })

	describe("pattern-match", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("pattern-match"))))
		})
   })

	describe("reverse", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("reverse"))))
		})
   })

	describe("zipW", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("zipW"))))
		})
   })
})
