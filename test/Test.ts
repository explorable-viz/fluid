/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { Cursor, TestFile, initialise, loadExample, parseExample, runExample } from "./Helpers"
import { NonEmpty } from "../src/BaseTypes"
import { assert } from "../src/util/Core"
import { World } from "../src/util/Persistent"
import { ann } from "../src/Annotated"
import { Expr } from "../src/Expr"
import { Traced, Value } from "../src/Traced"

import Trie = Expr.Trie

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
			const here: Cursor = new Cursor(e)
			here.to(Expr.BinaryApp, "e1")
				 .at(Expr.Expr, e => e.setα(ann.bot))
			const v: Value = runExample(e).v
			assert(v.α === ann.bot)
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
			const e: Expr = parseExample(file.text)
			runExample(e)
			World.newRevision()
			let here: Cursor = new Cursor(e)
			here.to(Expr.LetRec, "δ")
				 .toElem(0)
				 .to(Expr.RecDef, "f")
				 .to(Expr.Fun, "σ")
				 .to(Trie.Var, "κ")
				 .to(Expr.Fun, "σ")
				 .to(Trie.Constr, "cases")
				 .to(NonEmpty, "left")
				 .nodeValue()
				 .arg(Trie.Var, "κ")
				 .arg(Trie.Var, "κ")
				 .end()
				 .to(Expr.MatchAs, "σ")
				 .to(Trie.Constr, "cases")
				 .nodeValue().end()
				 .constrArg("Cons", 0)
				 .at(Expr.Var, e => e.setα(ann.bot))
			const v: Value = runExample(e).v
			assert(v.α !== ann.bot)
			here = new Cursor(v)
			here.push()
				 .val_constrArg("Cons", 0)
				 .to(Traced, "v")
				 .assert(Value.ConstInt, v => v.α === ann.bot)
				 .pop()
 				 .val_constrArg("Cons", 1)
				 .to(Traced, "v")
				 .assert(Value.Constr, v => v.ctr.str === "Nil")
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
			const e: Expr = parseExample(file.text)
			runExample(e)
			World.newRevision()
			const here: Cursor = new Cursor(e)
			here.to(Expr.LetRec, "e")
				 .to(Expr.App, "arg")
				 .push()
				 .constrArg("Cons", 0)
	  			 .at(Expr.Expr, e => e.setα(ann.bot))
				 .pop()
				 .constrArg("Cons", 1)
				 .push()
				 .constrArg("Cons", 0)
				 .at(Expr.Expr, e => e.setα(ann.bot))
			let v: Value = runExample(e).v
			assert(v.α !== ann.bot)
			World.newRevision()
			here.pop()
				 .constrArg("Cons", 1)
				 .at(Expr.Constr, e => e.setα(ann.bot))
			v = runExample(e).v
			assert(v.α === ann.bot)
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
			const e: Expr = parseExample(file.text)
			runExample(e)
			World.newRevision()
			const here: Cursor = new Cursor(e)
			here.to(Expr.Let, "σ")
				 .to(Trie.Var, "κ")
				 .to(Expr.LetRec, "e")
				 .to(Expr.App, "arg")
				 .push()
				 .constrArg("NonEmpty", 0)
				 .constrArg("NonEmpty", 1)
				 .constrArg("Pair", 0)
				 .at(Expr.ConstInt, e => e.setα(ann.bot))
			let v = runExample(e).v
			assert(v.α !== ann.bot)
			here.pop()
				 .constrArg("NonEmpty", 1)
				 .constrArg("Pair", 0)
				 .at(Expr.ConstInt, e => e.setα(ann.bot))
			v = runExample(e).v
			assert(v.α === ann.bot)
		})
	})

	describe("map", () => {
		const file: TestFile = loadExample("map")
		it("ok", () => {
			const e: Expr = parseExample(file.text)
			runExample(e)
			World.newRevision()
			let here: Cursor = new Cursor(e)
			here.to(Expr.LetRec, "e")
				 .to(Expr.Let, "σ")
				 .to(Trie.Var, "κ")
				 .to(Expr.App, "arg")
				 .constrArg("Cons", 0)
				 .at(Expr.Expr, e => e.setα(ann.bot))
			let v: Value = runExample(e).v
			assert(v.α !== ann.bot)
			here = new Cursor(v)
			here.push()
				 .val_constrArg("Cons", 0)
				 .assert(Traced, tv => tv.v.α === ann.bot)
				 .pop()
				 .val_constrArg("Cons", 1)
				 .assert(Traced, tv => tv.v.α !== ann.bot)
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
			const e: Expr = parseExample(file.text)
			runExample(e)
			World.newRevision()
			let here: Cursor = new Cursor(e)
			here.to(Expr.LetRec, "e")
				 .to(Expr.App, "arg")
				 .constrArg("Cons", 1)
				 .constrArg("Cons", 1)
				 .at(Expr.Expr, e => e.setα(ann.bot))
			let v: Value = runExample(e).v
			here = new Cursor(v)
			here.assert(Value.Constr, v => v.α === ann.bot)
				 .push()
				 .val_constrArg("Cons", 0)
				 .to(Traced, "v")
				 .assert(Value.ConstInt, v => v.α !== ann.bot)
				 .pop()
				 .val_constrArg("Cons", 1)
				 .to(Traced, "v")
				 .assert(Value.Constr, v => v.α !== ann.bot)
		})
	})

	describe("zipW", () => {
		const file: TestFile = loadExample("zipW")
		it("ok", () => {
			runExample(parseExample(file.text))
		})
	})
})
