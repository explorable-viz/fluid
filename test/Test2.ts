/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { initialise, load, parse } from "./util/Core2"

before((done: MochaDone) => {
	initialise()
	done()
})

describe("example", () => {
	describe("arithmetic", () => {
		it("ok", () => {
			parse(load("arithmetic"))
		})
	})
})
