/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { assert } from "../src/util/Core"
import { World } from "../src/util/Persistent"
import { Expr } from "../src/Expr"
import { Traced} from "../src/Traced"
import { TestFile, τ, initialise, loadExample, parseExample, runExample } from "./Helpers"

import VoidKont = Traced.VoidKont

before((done: MochaDone) => {
	initialise()
	done()
})

describe("example", () => {
	describe("arithmetic", () => {
		const file: TestFile = loadExample("arithmetic")
		it("ok", () => {
			runExample(parseExample(file.text), τ.top(VoidKont.make()))
		})
	})

	describe("bar-chart", () => {
		const file: TestFile = loadExample("bar-chart")
		it("ok", () => {
			runExample(parseExample(file.text), τ.var_(VoidKont.make()))
		})
	})

	describe("compose", () => {
		const file: TestFile = loadExample("compose")
		it("ok", () => {
			runExample(parseExample(file.text), τ.top(VoidKont.make()))
		})
	})

	describe("factorial", () => {
		const file: TestFile = loadExample("factorial")
		it("ok", () => {
			runExample(parseExample(file.text), τ.top(VoidKont.make()))
		})
	})

	describe("filter", () => {
		const file: TestFile = loadExample("filter")
		it("ok", () => {
			const e: Expr = parseExample(file.text)
			const tv: Traced = runExample(e, τ.cons(τ.arg(τ.int(τ.arg(τ.var_(τ.endArgs(VoidKont.make())))))))
			World.newRevision()
			const tvʹ: Traced = runExample(e, τ.cons(τ.arg(τ.var_(τ.arg(τ.var_(τ.endArgs(VoidKont.make())))))))
			assert(tv === tvʹ)
		})
	})

	describe("foldr_sumSquares", () => {
		const file: TestFile = loadExample("foldr_sumSquares")
		it("ok", () => {
			runExample(parseExample(file.text))
		})
	})

	describe("length", () => {
		const file: TestFile = loadExample("length")
		it("ok", () => {
			runExample(parseExample(file.text))
		})
	})

	describe("lexicalScoping", () => {
		const file: TestFile = loadExample("lexicalScoping")
		it("ok", () => {
			runExample(parseExample(file.text))
		})
	})

	describe("lookup", () => {
		const file: TestFile = loadExample("lookup")
		it("ok", () => {
			runExample(parseExample(file.text))
		})
	})

	describe("map", () => {
		const file: TestFile = loadExample("map")
		it("ok", () => {
			runExample(parseExample(file.text))
		})
	})

	describe("mergeSort", () => {
		const file: TestFile = loadExample("mergeSort")
		it("ok", () => {
			runExample(parseExample(file.text), τ.cons(τ.arg(τ.int(τ.arg(τ.var_(τ.endArgs(VoidKont.make())))))))
		})
	})

	describe("normalise", () => {
		const file: TestFile = loadExample("normalise")
		it("ok", () => {
			runExample(parseExample(file.text))
		})
	})

	describe("reverse", () => {
		const file: TestFile = loadExample("reverse")
		it("ok", () => {
			runExample(parseExample(file.text), τ.cons(τ.arg(τ.int(τ.arg(τ.var_(τ.endArgs(VoidKont.make())))))))
		})
	})

	describe("zipW", () => {
		const file: TestFile = loadExample("zipW")
		it("ok", () => {
			runExample(parseExample(file.text), τ.cons(τ.arg(τ.point(τ.arg(τ.int(τ.arg(τ.int(τ.endArgs(τ.arg(τ.var_(τ.endArgs(VoidKont.make()))))))))))))
		})
	})
})
