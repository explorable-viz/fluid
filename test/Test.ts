/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { Profile, TestFile, σ_int, initialise, loadTestFile, runTest } from "./Helpers"
import { __nonNull } from "../src/util/Core"

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
			runTest(__nonNull(file.text), Profile.Run, σ_int)
		})
	})

	describe("compose", () => {
		const file: TestFile = loadExample("compose")
		it("parses ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("factorial", () => {
		const file: TestFile = loadExample("factorial")
		it("parses ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("filter", () => {
		const file: TestFile = loadExample("filter")
		it("parses ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("foldr_sumSquares", () => {
		const file: TestFile = loadExample("foldr_sumSquares")
		it("parses ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("length", () => {
		const file: TestFile = loadExample("length")
		it("parses ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("lexicalScoping", () => {
		const file: TestFile = loadExample("lexicalScoping")
		it("parses ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("lookup", () => {
		const file: TestFile = loadExample("lookup")
		it("parses ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("map", () => {
		const file: TestFile = loadExample("map")
		it("parses ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("mergeSort", () => {
		const file: TestFile = loadExample("mergeSort")
		it("parses ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("normalise", () => {
		const file: TestFile = loadExample("normalise")
		it("parses ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("reverse", () => {
		const file: TestFile = loadExample("reverse")
		it("parses ok", () => {
			runTest(__nonNull(file.text))
		})
	})

	describe("zipW", () => {
		const file: TestFile = loadExample("zipW")
		it("parses ok", () => {
			runTest(__nonNull(file.text))
		})
	})
})
