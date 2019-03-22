/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { BwdSlice, Cursor, FwdSlice, TestFile, ρ, initialise, loadExample, parseExample, runExample } from "./Helpers"
import { NonEmpty } from "../src/BaseTypes"
import { World } from "../src/util/Persistent"
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
			new (class extends FwdSlice {
				setup (expr: Cursor): void {
					expr.to(Expr.BinaryApp, "e1").notNeed()
				}
				expect (val: Cursor): void {
					val.notNeeded()
				} 
			})(e)
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
			new (class extends FwdSlice {
				setup (expr: Cursor): void {
					expr.to(Expr.LetRec, "δ")
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
						 .constrArg("Cons", 0).notNeed()
				}
				expect (val: Cursor): void {
					val.need()
						.push().val_constrArg("Cons", 0).to(ExplVal, "v").notNeeded().pop()
						.val_constrArg("Cons", 1)
						.to(ExplVal, "v")
						.assert(Value.Constr, v => v.ctr.str === "Nil")
				}
			})(e)
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
			let test = new (class extends FwdSlice {
				setup (expr: Cursor): void {
					expr.to(Expr.LetRec, "e")
						 .to(Expr.App, "arg")
						 .push().constrArg("Cons", 0).notNeed().pop()
						 .push().constrArg("Cons", 0).notNeed().pop()
				}
				expect (val: Cursor): void {
					val.needed()
				}
			})(e)
			// deleting the tail of the tail means length can't be computed:
			new (class extends FwdSlice {
				setup (expr: Cursor): void {
					expr.goto(test.e)
						 .constrArg("Cons", 1).notNeed()
				}
				expect (val: Cursor): void {
					val.notNeeded()
				}
			})(e)
			// needing the result only needs the cons cells:
			new (class extends BwdSlice {
				setup (val: Cursor): void {
					val.need()
				}
				expect (expr: Cursor): void {
					expr.to(Expr.LetRec, "e")
						 .to(Expr.App, "arg").needed()
						 .push().constrArg("Cons", 0).notNeeded().pop()
						 .constrArg("Cons", 1).needed()
						 .push().constrArg("Cons", 0).notNeeded().pop()
						 .constrArg("Cons", 1).needed()
				}
			})(e)
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
			const last = new (class extends FwdSlice {
				setup (expr: Cursor): void {
					expr.to(Expr.Let, "σ")
						 .to(Trie.Var, "κ")
						 .to(Expr.LetRec, "e")
						 .to(Expr.App, "arg")
						 .push()
							.constrArg("NonEmpty", 0)
							.constrArg("NonEmpty", 1)
							.constrArg("Pair", 0).notNeed().pop()
				}
				expect (val: Cursor): void {
					val.needed()
				}
			})(e)
			new (class extends FwdSlice {
				setup (expr: Cursor): void {
					expr.goto(last.e)
						 .constrArg("NonEmpty", 1)
						 .constrArg("Pair", 0).notNeed()
				}
				expect (val: Cursor): void {
					val.notNeeded()
				}
			})(e)
		})
	})

	describe("map", () => {
		const file: TestFile = loadExample("map")
		it("ok", () => {
			const e: Expr = parseExample(file.text)
			runExample(e)
			new (class extends FwdSlice {
				setup (expr: Cursor): void {
					expr.to(Expr.LetRec, "e")
 						 .to(Expr.Let, "σ")
 						 .to(Trie.Var, "κ")
					 	 .to(Expr.App, "arg")
						 .constrArg("Cons", 0).notNeed()
				  }
				expect (val: Cursor): void {
					val.push().val_constrArg("Cons", 0).to(ExplVal, "v").notNeeded().pop()
						.val_constrArg("Cons", 1)
						.to(ExplVal, "v").needed()
				}
			})(e)
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
			const e: Expr = parseExample(file.text)
			// retaining only pair constructor discards both subcomputations:
			new (class extends BwdSlice {
				setup (val: Cursor): void {
					val.need()
				}
				expect (expr: Cursor): void {
					expr.push().to(Expr.Let, "e").notNeeded().pop()
						 .to(Expr.Let, "σ")
						 .to(Trie.Var, "κ")
						 .to(Expr.Let, "e").notNeeded()
				}
			})(e)
			// retaining either component of pair retains both subcomputations:
			new (class extends BwdSlice {
				setup (val: Cursor): void {
					val.val_constrArg("Pair", 0)
						.to(ExplVal, "v").need()
				}
				expect (expr: Cursor): void {
					expr.push().to(Expr.Let, "e").needed().pop()
						 .to(Expr.Let, "σ")
						 .to(Trie.Var, "κ")
						 .to(Expr.Let, "e").needed()
				}
			})(e)
		})
	})

	describe("reverse", () => {
		const file: TestFile = loadExample("reverse")
		it("ok", () => {
			const e: Expr = parseExample(file.text)
			runExample(e)
			new (class extends FwdSlice {
				setup (expr: Cursor): void {
					expr.to(Expr.LetRec, "e")
 						 .to(Expr.App, "arg")
 						 .constrArg("Cons", 1)
 						 .constrArg("Cons", 1).notNeed()
				}
				expect (val: Cursor): void {
					val.notNeeded()
						.push().val_constrArg("Cons", 0).to(ExplVal, "v").needed()
						.pop()
						.val_constrArg("Cons", 1)
						.to(ExplVal, "v").needed()
				}
			})(e)
		})
	})

	describe("zipW", () => {
		const file: TestFile = loadExample("zipW")
		it("ok", () => {
			const e: Expr = parseExample(file.text)
			// needing first cons cell of output needs same amount of input lists
			const last = new (class extends BwdSlice {
				setup (val: Cursor): void {
					val.need()
				}
				expect (expr: Cursor): void {
					expr.push()
						 	.to(Expr.LetRec, "e")
						  	.to(Expr.App, "arg").needed().pop()
					expr.push()
							.to(Expr.LetRec, "e")
						  	.to(Expr.App, "func")
						  	.to(Expr.App, "arg").needed().pop()
					expr.to(Expr.LetRec, "δ")
						 .toElem(0).needed()
						 .to(Expr.RecDef, "σ")
						 .to(Trie.Var, "κ").needed()
				}
			})(e)
			// needing constructor of first element requires constructor at head of supplied op, plus application of op in zipW
			World.newRevision()
			const tv: ExplVal = Eval.eval_(ρ, e)
			const val: Cursor = new Cursor(tv.v)
			val.push()
					.val_constrArg("Cons", 0)
					.to(ExplVal, "v").need()
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
			new Cursor(last.expr.o).to(Expr.Fun, "σ")
				.to(Trie.Constr, "cases")
				.to(NonEmpty, "left")
				.nodeValue()			 
				.arg(Trie.Var, "κ")
				.arg(Trie.Var, "κ")
				.end().needed()
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
