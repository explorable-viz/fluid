/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { initialise, load, parse, prelude } from "./util/Core2"
import { Eval } from "../src/Eval2"

before((done: MochaDone) => {
	initialise()
	done()
})

// putting test name in a variable interacts poorly with asynchronous execution
describe("example", () => {
	xdescribe("arithmetic", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("arithmetic")))(prelude))
		})
   })
   
   describe("bar-chart", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("bar-chart")))(prelude))
		})
	})

   xdescribe("compose", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("compose")))(prelude))
		})
	})

   describe("factorial", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("factorial")))(prelude))
		})
	})

   describe("filter", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("filter")))(prelude))
		})
	})

   describe("foldr_sumSquares", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("foldr_sumSquares")))(prelude))
		})
	})

   describe("length", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("length")))(prelude))
		})
	})

	xdescribe("lexicalScoping", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("lexicalScoping")))(prelude))
		})
	})

   describe("map", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("map")))(prelude))
		})
	})

   describe("mergeSort", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("mergeSort")))(prelude))
		})
	})

	xdescribe("normalise", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("normalise")))(prelude))
		})
   })

	describe("pattern-match", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("pattern-match")))(prelude))
		})
   })

	describe("reverse", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("reverse")))(prelude))
		})
   })

	describe("zipW", () => {
		it("ok", () => {
			console.log(Eval.interpret(parse(load("zipW")))(prelude))
		})
   })
})
