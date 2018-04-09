/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { Profile, TestFile, τ, initialise, loadTestFile, runTest } from "./Helpers"
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
			runTest(__nonNull(file.text), Profile.Run, τ.int(null))
		})
	})

	describe("compose", () => {
		const file: TestFile = loadExample("compose")
		it("runs ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.int(null))
		})
	})

	describe("factorial", () => {
		const file: TestFile = loadExample("factorial")
		it("runs ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.int(null))
		})
	})

	describe("filter", () => {
		const file: TestFile = loadExample("filter")
		it("runs ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.cons(τ.int(τ.var_(null))))
		})
	})

	describe("foldr_sumSquares", () => {
		const file: TestFile = loadExample("foldr_sumSquares")
		it("runs ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.int(null))
		})
	})

	describe("length", () => {
		const file: TestFile = loadExample("length")
		it("runs ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.int(null))
		})
	})

	describe("lexicalScoping", () => {
		const file: TestFile = loadExample("lexicalScoping")
		it("runs ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.int(null))
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
		it("runs ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.cons(τ.int(τ.var_(null))))
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
		it("runs ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.pair(τ.int(τ.int(null))))
		})
	})

	describe("reverse", () => {
		const file: TestFile = loadExample("reverse")
		it("runs ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.cons(τ.int(τ.var_(null))))
		})
	})

	describe("zipW", () => {
		const file: TestFile = loadExample("zipW")
		it("runs ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.cons(τ.pair(τ.int(τ.int(τ.var_(null))))))
		})
	})
})
