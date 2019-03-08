/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { TestFile, from, initialise, loadExample, parseExample, runExample } from "./Helpers"
import { Persistent, PersistentObject, World } from "../src/util/Persistent"
import { ann } from "../src/Annotated"
import { Expr } from "../src/Expr"

before((done: MochaDone) => {
	initialise()
	done()
})

describe("example", () => {
	describe("arithmetic", () => {
		const file: TestFile = loadExample("arithmetic")
		it("ok", () => {
			const e: Expr = parseExample(file.text)
			runExample(e)
			World.newRevision()
			let here: Persistent = e
			here = from(here as PersistentObject, Expr.BinaryApp, "e1")
			const hereʹ: Expr = here as Expr
			hereʹ.setα(ann.bot)
			runExample(e)
		})
	})

	describe("bar-chart", () => {
		const file: TestFile = loadExample("bar-chart")
		it("ok", () => {
			runExample(parseExample(file.text))
		})
	})

	describe("compose", () => {
		const file: TestFile = loadExample("compose")
		it("ok", () => {
			runExample(parseExample(file.text))
		})
	})

	describe("factorial", () => {
		const file: TestFile = loadExample("factorial")
		it("ok", () => {
			runExample(parseExample(file.text))
		})
	})

	describe("filter", () => {
		const file: TestFile = loadExample("filter")
		it("ok", () => {
			runExample(parseExample(file.text))
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
			runExample(parseExample(file.text))
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
			runExample(parseExample(file.text))
		})
	})

	describe("zipW", () => {
		const file: TestFile = loadExample("zipW")
		it("ok", () => {
			runExample(parseExample(file.text))
		})
	})
})
