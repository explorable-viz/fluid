/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { Cursor, TestFile, ρ, initialise, loadExample, parseExample, runExample } from "./Helpers"
import { NonEmpty } from "../src/BaseTypes"
import { assert } from "../src/util/Core"
import { World } from "../src/util/Persistent"
import { ann, setall } from "../src/Annotated"
import { Eval } from "../src/Eval"
import { Expr } from "../src/Expr"
import { ExplVal, Value } from "../src/ExplVal"

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
			const v: Value = Eval.eval_(ρ, e).v
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
			setall(e, ann.top)
			let here: Cursor = new Cursor(e)
			here.to(Expr.LetRec, "δ")
				 .toElem(0)
				 .to(Expr.RecDef, "σ")
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
			const v: Value = Eval.eval_(ρ, e).v
			assert(v.α !== ann.bot)
			here = new Cursor(v)
			here.push()
				 .val_constrArg("Cons", 0)
				 .to(ExplVal, "v")
				 .assert(Value.ConstInt, v => v.α === ann.bot)
				 .pop()
 				 .val_constrArg("Cons", 1)
				 .to(ExplVal, "v")
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
			// erasing the elements doesn't affect the count:
			World.newRevision()
			setall(e, ann.top)
			let here: Cursor = new Cursor(e)
			here.to(Expr.LetRec, "e")
				 .to(Expr.App, "arg")
				 .push()
				 .constrArg("Cons", 0)
	  			 .at(Expr.Expr, e => e.setα(ann.bot))
				 .pop()

				 .push()
				 .constrArg("Cons", 0)
				 .at(Expr.Expr, e => e.setα(ann.bot))
			let tv: ExplVal = Eval.eval_(ρ, e)
			assert(tv.v.α !== ann.bot)
			// deleting the tail of the tail means length can't be computed:
			World.newRevision()
			here.pop()
				 .constrArg("Cons", 1)
				 .at(Expr.Constr, e => e.setα(ann.bot))
			tv = Eval.eval_(ρ, e)
			assert(tv.v.α === ann.bot)
			// needing the result only needs the cons cells:
			World.newRevision()
			setall(e, ann.bot)
			setall(tv, ann.bot)
			World.newRevision()
			here = new Cursor(tv.v)
			here.at(Value.ConstInt, v => v.setα(ann.top))
			Eval.uneval(tv)
			here = new Cursor(e)
			here.to(Expr.LetRec, "e")
				 .to(Expr.App, "arg")
				 .assert(Expr.Constr, e => e.α === ann.top)
				 .push()
					.constrArg("Cons", 0)
					.assert(Expr.ConstInt, e => e.α === ann.bot)
					.pop()
				 .constrArg("Cons", 1)
				 .assert(Expr.Constr, e => e.α === ann.top)
				 .push()
					.constrArg("Cons", 0)
					.assert(Expr.ConstInt, e => e.α === ann.bot)
					.pop()
				 .constrArg("Cons", 1)
				 .assert(Expr.Constr, e => e.α === ann.top)
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
					.pop()
			let v = Eval.eval_(ρ, e).v
			assert(v.α !== ann.bot)
			World.newRevision()
			here.constrArg("NonEmpty", 1)
				 .constrArg("Pair", 0)
				 .at(Expr.ConstInt, e => e.setα(ann.bot))
			v = Eval.eval_(ρ, e).v
			assert(v.α === ann.bot)
		})
	})

	describe("map", () => {
		const file: TestFile = loadExample("map")
		it("ok", () => {
			const e: Expr = parseExample(file.text)
			runExample(e)
			World.newRevision()
			setall(e, ann.top)
			let here: Cursor = new Cursor(e)
			here.to(Expr.LetRec, "e")
				 .to(Expr.Let, "σ")
				 .to(Trie.Var, "κ")
				 .to(Expr.App, "arg")
				 .constrArg("Cons", 0)
				 .at(Expr.Expr, e => e.setα(ann.bot))
			let v: Value = Eval.eval_(ρ, e).v
			assert(v.α !== ann.bot)
			here = new Cursor(v)
			here.push()
					.val_constrArg("Cons", 0)
					.assert(ExplVal, tv => tv.v.α === ann.bot)
					.pop()
				 .val_constrArg("Cons", 1)
				 .assert(ExplVal, tv => tv.v.α !== ann.bot)
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
			const e: Expr = parseExample(file.text),
					tv: ExplVal = Eval.eval_(ρ, e)
			World.newRevision()
			setall(e, ann.bot)
			setall(tv, ann.bot)
			// retaining only pair constructor discards both subcomputations:
			World.newRevision()
			let here: Cursor = new Cursor(tv.v)
			here.at(Value.Constr, v => v.setα(ann.top))
			Eval.uneval(tv)
			here = new Cursor(e)
			here.push()
					.to(Expr.Let, "e")
					.assert(Expr.ConstInt, e => e.α === ann.bot)
					.pop()
				 .to(Expr.Let, "σ")
				 .to(Trie.Var, "κ")
				 .to(Expr.Let, "e")
				 .assert(Expr.ConstInt, e => e.α === ann.bot)
			// retaining either component of pair retains both subcomputations:
			World.newRevision()
			here = new Cursor(tv.v)
			here.val_constrArg("Pair", 0)
				 .to(ExplVal, "v")
				 .at(Value.ConstInt, v => v.setα(ann.top))
			Eval.uneval(tv)
			here = new Cursor(e)
			here.push()
					.to(Expr.Let, "e")
					.assert(Expr.ConstInt, e => e.α === ann.top)
					.pop()
				 .to(Expr.Let, "σ")
				 .to(Trie.Var, "κ")
				 .to(Expr.Let, "e")
				 .assert(Expr.ConstInt, e => e.α === ann.top)
		})
	})

	describe("reverse", () => {
		const file: TestFile = loadExample("reverse")
		it("ok", () => {
			const e: Expr = parseExample(file.text)
			runExample(e)
			World.newRevision()
			setall(e, ann.top)
			let here: Cursor = new Cursor(e)
			here.to(Expr.LetRec, "e")
				 .to(Expr.App, "arg")
				 .constrArg("Cons", 1)
				 .constrArg("Cons", 1)
				 .at(Expr.Expr, e => e.setα(ann.bot))
			let v: Value = Eval.eval_(ρ, e).v
			here = new Cursor(v)
			here.assert(Value.Constr, v => v.α === ann.bot)
				 .push()
				 .val_constrArg("Cons", 0)
				 .to(ExplVal, "v").needed()
				 .pop()
				 .val_constrArg("Cons", 1)
				 .to(ExplVal, "v").needed()
		})
	})

	describe("zipW", () => {
		const file: TestFile = loadExample("zipW")
		it("ok", () => {
			const e: Expr = parseExample(file.text),
				   tv: ExplVal = Eval.eval_(ρ, e)
			World.newRevision()
			setall(e, ann.bot)
			setall(tv, ann.bot)
			// needing first cons cell of output needs same amount of input lists
			World.newRevision()
			const val: Cursor = new Cursor(tv.v)
			val.at(Value.Constr, v => v.setα(ann.top))
			Eval.uneval(tv)
			const list2: Cursor = new Cursor(e)
			list2.to(Expr.LetRec, "e")
				  .to(Expr.App, "arg").needed()
			const list1: Cursor = new Cursor(e)
			list1.to(Expr.LetRec, "e")
				  .to(Expr.App, "func")
				  .to(Expr.App, "arg").needed()
			const zipW: Cursor = new Cursor(e)
			zipW.to(Expr.LetRec, "δ")
				.toElem(0).needed()
				.to(Expr.RecDef, "σ")
				.to(Trie.Var, "κ").needed()
				// needing constructor of first element requires constructor at head of supplied op, plus application of op in zipW
			World.newRevision()
			val.push()
					.val_constrArg("Cons", 0)
					.to(ExplVal, "v")
					.at(Value.Constr, v => v.setα(ann.top))
			Eval.uneval(tv)
			const fun: Cursor = new Cursor(e)
			fun.to(Expr.LetRec, "e")
				.to(Expr.App, "func")
				.to(Expr.App, "func")
				.to(Expr.App, "arg")
				.to(Expr.Fun, "σ")
				.to(Trie.Constr, "cases")
				.nodeValue()
				.arg(Trie.Var, "κ")
				.arg(Trie.Var, "κ")
				.end().needed()
		zipW.to(Expr.Fun, "σ")
			 .to(Trie.Constr, "cases")
			 .to(NonEmpty, "left")
			 .nodeValue()			 
			 .arg(Trie.Var, "κ")
			 .arg(Trie.Var, "κ")
			 .end()
			 .assert(Expr.Fun, e => e.α === ann.top)
			 .to(Expr.Fun, "σ")
			 .to(Trie.Constr, "cases")
			 .to(NonEmpty, "left")
			 .nodeValue()			 
			 .arg(Trie.Var, "κ")
			 .arg(Trie.Var, "κ")
			 .end().needed()
			 .constrArg("Cons", 0).needed()
			 .to(Expr.App, "arg").needed()
			 .push().constrArg("Pair", 0).notNeeded().pop()
			 .push().constrArg("Pair", 1).notNeeded()
	})
	})
})
