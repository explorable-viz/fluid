/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { NonEmpty } from "../src/BaseTypes"
import { Expr } from "../src/Expr"
import { Value } from "../src/ExplVal"
import { BwdSlice, FwdSlice, initialise, load, parse } from "./util/Core"

import Trie = Expr.Trie

before((done: MochaDone) => {
	initialise()
	done()
})

describe("example", () => {
	describe("arithmetic", () => {
		it("ok", () => {
			const e: Expr = parse(load("arithmetic"))
			new (class extends FwdSlice {
				setup (): void {
					this.expr
						.skipImports()
						.to(Expr.BinaryApp, "e1").notNeed()
				}
				expect (): void {
					this.val.notNeeded()
				} 
			})(e)
		})
	})

	describe("bar-chart", () => {
		it("ok", () => {
			const e: Expr = parse(load("bar-chart"))
			new (class extends BwdSlice {
				setup (): void {
					this.val.need()
				}
				expect (): void {
					this.expr.needed()
				}
			})(e)
		})
	})

	describe("compose", () => {
		it("ok", () => {
			const e: Expr = parse(load("compose"))
			new FwdSlice(e)
			new BwdSlice(e)
		})
	})

	describe("factorial", () => {
		it("ok", () => {
			const e: Expr = parse(load("factorial"))
			new FwdSlice(e)
			new BwdSlice(e)
		})
	})

	describe("filter", () => {
		it("ok", () => {
			const e: Expr = parse(load("filter"))
			new (class extends FwdSlice {
				setup (): void {
					this.expr
						.toRecDef("filter")
						.to(Expr.RecDef, "σ")
						.var_("p")
						.to(Expr.Fun, "σ")
						.to(Trie.Constr, "cases")
						.to(NonEmpty, "left")
						.nodeValue()
						.arg_var("x").arg_var("xs")
						.end()
						.to(Expr.MatchAs, "σ")
						.to(Trie.Constr, "cases")
						.nodeValue().end()
						.constrArg("Cons", 0).notNeed()
				}
				expect (): void {
					this.val
						.need()
						.push().val_constrArg("Cons", 0).value().notNeeded().pop()
						.val_constrArg("Cons", 1).value()
						.assert(Value.Constr, v => v.ctr.str === "Nil")
				}
			})(e)
		})
	})

	describe("foldr_sumSquares", () => {
		it("ok", () => {
			const e: Expr = parse(load("foldr_sumSquares"))
			new FwdSlice(e)
			new BwdSlice(e)
		})
	})

	describe("length", () => {
		it("ok", () => {
			const e: Expr = parse(load("length"))
			// erasing the elements doesn't affect the count:
			let test = new (class extends FwdSlice {
				setup (): void {
					this.expr
						.skipImports()
						.to(Expr.App, "arg")
						.push().constrArg("Cons", 0).notNeed().pop()
						.push().constrArg("Cons", 0).notNeed().pop()
				}
				expect (): void {
					this.val.needed()
				}
			})(e)
			// deleting the tail of the tail means length can't be computed:
			new (class extends FwdSlice {
				setup (): void {
					this.expr
						.goto(test.e)
						.constrArg("Cons", 1).notNeed()
				}
				expect (): void {
					this.val.notNeeded()
				}
			})(e)
			// needing the result only needs the cons cells:
			new (class extends BwdSlice {
				setup (): void {
					this.val.need()
				}
				expect (): void {
					this.expr
						.skipImports()
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
		it("ok", () => {
			const e: Expr = parse(load("lexicalScoping"))
			new FwdSlice(e)
			new BwdSlice(e)
		})
	})

	describe("lookup", () => {
		it("ok", () => {
			const e: Expr = parse(load("lookup"))
			const last = new (class extends FwdSlice {
				setup (): void {
					this.expr
						.skipImports()
						.to(Expr.Let, "σ")
						.var_("compare")
						.to(Expr.LetRec, "e")
						.to(Expr.App, "arg")
						.push()
							.constrArg("NonEmpty", 0)
							.constrArg("NonEmpty", 1)
							.constrArg("Pair", 0).notNeed().pop()
				}
				expect (): void {
					this.val.needed()
				}
			})(e)
			new (class extends FwdSlice {
				setup (): void {
					this.expr
						.goto(last.e)
						.constrArg("NonEmpty", 1)
						.constrArg("Pair", 0).notNeed()
				}
				expect (): void {
					this.val.notNeeded()
				}
			})(e)
		})
	})

	describe("map", () => {
		it("ok", () => {
			const e: Expr = parse(load("map"))
			new (class extends FwdSlice {
				setup (): void {
					this.expr
						.skipImports()
						.to(Expr.Let, "σ")
 						.var_("incr")
					 	.to(Expr.App, "arg")
						.constrArg("Cons", 0).notNeed()
				  }
				expect (): void {
					this.val
						.push().val_constrArg("Cons", 0).value().notNeeded().pop()
						.val_constrArg("Cons", 1).value().needed()
				}
			})(e)
		})
	})

	describe("mergeSort", () => {
		it("ok", () => {
			const e: Expr = parse(load("mergeSort"))
			new FwdSlice(e)
			new BwdSlice(e)
		})
	})

	describe("normalise", () => {
		it("ok", () => {
			const e: Expr = parse(load("normalise"))
			// retaining only pair constructor discards both subcomputations:
			new (class extends BwdSlice {
				setup (): void {
					this.val.need()
				}
				expect (): void {
					this.expr
						.skipImports()
						.push().to(Expr.Let, "e").notNeeded().pop()
						.to(Expr.Let, "σ")
						.var_("x")
						.to(Expr.Let, "e").notNeeded()
				}
			})(e)
			// retaining either component of pair retains both subcomputations:
			new (class extends BwdSlice {
				setup (): void {
					this.val
						.val_constrArg("Pair", 0).value().need()
				}
				expect (): void {
					this.expr
						.skipImports()
						.push().to(Expr.Let, "e").needed().pop()
						.to(Expr.Let, "σ")
						.var_("x")
						.to(Expr.Let, "e").needed()
				}
			})(e)
		})
	})

	describe("pattern-match", () => {
		it("ok", () => {
			const e: Expr = parse(load("pattern-match"))
			new FwdSlice(e)
			new BwdSlice(e)
		})
	})

	describe("reverse", () => {
		it("ok", () => {
			const e: Expr = parse(load("reverse"))
			new (class extends FwdSlice {
				setup (): void {
					this.expr
						.skipImports()
 						.to(Expr.App, "arg")
 						.constrArg("Cons", 1)
 						.constrArg("Cons", 1).notNeed()
				}
				expect (): void {
					this.val
						.notNeeded()
						.push().val_constrArg("Cons", 0).value().needed()
						.pop()
						.val_constrArg("Cons", 1).value().needed()
				}
			})(e)
		})
	})

	describe("zipW", () => {
		it("ok", () => {
			const e: Expr = parse(load("zipW"))
			// needing first cons cell of output needs same amount of input lists
			new (class extends BwdSlice {
				setup (): void {
					this.val.need()
				}
				expect (): void {
					this.expr
						.push()
							.toRecDef("zipW").needed()
							.to(Expr.RecDef, "σ")
							.var_("op").needed()
							.pop()
						.skipImports()
						.push()
							.to(Expr.App, "arg").needed().pop()
						.push()
							.to(Expr.App, "func")
						  	.to(Expr.App, "arg").needed().pop()
				}
			})(e)
			// needing constructor of first element requires constructor at head of supplied op, plus application of op in zipW
			new (class extends BwdSlice {
				setup (): void {
					this.val
						.val_constrArg("Cons", 0).value().need()
				}
				expect (): void {
					this.expr
						.push()
							.toRecDef("zipW")
							.to(Expr.RecDef, "σ")
							.var_("op")
							.to(Expr.Fun, "σ")
							.to(Trie.Constr, "cases")
							.push().nodeValue().end().notNeeded().pop() // body of outer Nil clause
							.to(NonEmpty, "left")
							.nodeValue()			 
							.arg_var("x").arg_var("xs")
							.end().notNeeded()
							.to(Expr.Fun, "σ")
							.to(Trie.Constr, "cases")
							.to(NonEmpty, "left")
							.nodeValue()			 
							.arg_var("y").arg_var("ys")
							.end().notNeeded()				 // cons constructor
							.constrArg("Cons", 0).needed() // application of op
							.to(Expr.App, "arg").needed()  // pair constructor
							.push().constrArg("Pair", 0).notNeeded().pop()
							.push().constrArg("Pair", 1).notNeeded().pop()
							.pop()
						.skipImports()
						.to(Expr.App, "func")
						.to(Expr.App, "func")
						.to(Expr.App, "arg")
						.to(Expr.Fun, "σ")
						.to(Trie.Constr, "cases")
						.nodeValue()
						.arg_var("x")
						.arg_var("y")
						.end().needed()
				}
			})(e)
		})
	})
})
