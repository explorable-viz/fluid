/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { initialise, load, parse, prelude } from "./util/Core2"
import { Eval } from "../src/Eval2"

before((done: MochaDone) => {
	initialise()
	done()
})

describe("example", () => {
	describe("arithmetic", () => {
		it("ok", () => {
			Eval.interpret(parse(load("arithmetic")))(prelude)
		})
	})
})
