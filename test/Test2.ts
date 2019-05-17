/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { initialise, load, parse, prelude } from "./util/Core2"
import { Eval } from "../src/Eval2"

before((done: MochaDone) => {
	initialise()
	done()
})

// putting test name in a variable interacts poorly with asynchronous execution
describe("example", () => {
	describe("arithmetic", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("arithmetic"))))
		})
   })

/*
   describe("bar-chart", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("bar-chart"))))
		})
	})

   describe("compose", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("compose"))))
		})
	})

   describe("factorial", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("factorial"))))
		})
	})

   describe("filter", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("filter"))))
		})
	})

   describe("foldr_sumSquares", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("foldr_sumSquares"))))
		})
	})

   describe("length", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("length"))))
		})
	})

	describe("lexicalScoping", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("lexicalScoping"))))
		})
	})

   describe("lookup", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("lookup"))))
		})
	})

   describe("map", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("map"))))
		})
	})

   describe("mergeSort", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("mergeSort"))))
		})
	})

	describe("normalise", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("normalise"))))
		})
   })

	describe("pattern-match", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("pattern-match"))))
		})
   })

	describe("reverse", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("reverse"))))
		})
   })

	describe("zipW", () => {
		it("ok", () => {
			console.log(Eval.eval_(prelude, parse(load("zipW"))))
		})
   })
*/
})
