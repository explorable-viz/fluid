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
		it("ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.int(null))
		})
	})

	describe("compose", () => {
		const file: TestFile = loadExample("compose")
		it("ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.int(null))
		})
	})

	describe("factorial", () => {
		const file: TestFile = loadExample("factorial")
		it("ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.int(null))
		})
	})

	describe("filter", () => {
		const file: TestFile = loadExample("filter")
		it("ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.cons(τ.arg(τ.int(τ.arg(τ.var_(τ.endArgs(null)))))))
		})
	})

	describe("foldr_sumSquares", () => {
		const file: TestFile = loadExample("foldr_sumSquares")
		it("ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.int(null))
		})
	})

	describe("length", () => {
		const file: TestFile = loadExample("length")
		it("ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.int(null))
		})
	})

	describe("lexicalScoping", () => {
		const file: TestFile = loadExample("lexicalScoping")
		it("ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.str(null))
		})
	})

	describe("lookup", () => {
		const file: TestFile = loadExample("lookup")
		it("parses ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.some(τ.arg(τ.str(τ.endArgs(null)))))
		})
	})

	describe("map", () => {
		const file: TestFile = loadExample("map")
		it("ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.cons(τ.arg(τ.int(τ.arg(τ.var_(τ.endArgs(null)))))))
		})
	})

	describe("mergeSort", () => {
		const file: TestFile = loadExample("mergeSort")
		it("ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.cons(τ.arg(τ.int(τ.arg(τ.var_(τ.endArgs(null)))))))
		})
	})

	describe("normalise", () => {
		const file: TestFile = loadExample("normalise")
		it("ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.pair(τ.arg(τ.int(τ.arg(τ.int(τ.endArgs(null)))))))
		})
	})

	describe("reverse", () => {
		const file: TestFile = loadExample("reverse")
		it("ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.cons(τ.arg(τ.int(τ.arg(τ.var_(τ.endArgs(null)))))))
		})
	})

	describe("zipW", () => {
		const file: TestFile = loadExample("zipW")
		it("ok", () => {
			runTest(__nonNull(file.text), Profile.Run, τ.cons(τ.arg(τ.pair(τ.arg(τ.int(τ.arg(τ.int(τ.endArgs(τ.arg(τ.var_(τ.endArgs(null))))))))))))
		})
	})
})
