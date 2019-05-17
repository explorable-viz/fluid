/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { initialise, load, parse } from "./util/Core2"
import { emptyEnv } from "../src/Env2"
import { Eval } from "../src/Eval2"

before((done: MochaDone) => {
	initialise()
	done()
})

// putting test name in a variable interacts poorly with asynchronous execution
describe("example", () => {
	describe("arithmetic", () => {
		it("ok", () => {
			console.log(Eval.eval_(emptyEnv(), parse(load("arithmetic"))))
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
