/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { TestFile, initialise, loadTestFile, runTest } from "./Helpers"
import { __nonNull } from "../src/Util"

function loadExample(file: string): TestFile {
	return loadTestFile("example", file)
}

before((done: MochaDone) => {
	initialise()
	done()
})

describe("example", () => {
	describe("arithmetic", () => {
		const file: TestFile = loadExample("arithmetic")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("compose", () => {
		const file: TestFile = loadExample("compose")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("factorial", () => {
		const file: TestFile = loadExample("factorial")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("filter", () => {
		const file: TestFile = loadExample("filter")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("foldr_sumSquares", () => {
		const file: TestFile = loadExample("foldr_sumSquares")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("length", () => {
		const file: TestFile = loadExample("length")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("listView", () => {
		const file: TestFile = loadExample("listView")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("lookup", () => {
		const file: TestFile = loadExample("lookup")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("map", () => {
		const file: TestFile = loadExample("map")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("mergeSort", () => {
		const file: TestFile = loadExample("mergeSort")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("normalise", () => {
		const file: TestFile = loadExample("normalise")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("reverse", () => {
		const file: TestFile = loadExample("reverse")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("zipW", () => {
		const file: TestFile = loadExample("zipW")
		it("runs ok", () => {
			runTest(__nonNull(file.text))
		})
	})
})
