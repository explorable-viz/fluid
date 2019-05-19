/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { FwdSlice, load, parse } from "./util/Core2"
import { Cons, List, Nil, NonEmpty } from "../src/BaseTypes2"
import { Expr } from "../src/Expr2"

import Trie = Expr.Trie

before((done: MochaDone) => {
	done()
})

// Putting test name in a variable interacts poorly with asynchronous execution.
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
			new FwdSlice(e)
		})
   })

   describe("compose", () => {
		it("ok", () => {
			const e: Expr = parse(load("compose"))
			new FwdSlice(e)
		})
	})

   describe("factorial", () => {
		it("ok", () => {
         const e: Expr = parse(load("factorial"))
         new FwdSlice(e)
		})
	})

   describe("filter", () => {
		it("ok", () => {
			const e: Expr = parse(load("filter"))
			new (class extends FwdSlice {
				setup (): void {
					this.expr
						.toDef("filter")
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
						.push().to(Cons, "head").notNeeded().pop()
						.to(Cons, "tail")
						.assert(List, v => Nil.is(v))
				}
			})(e)
		})
	})

   describe("foldr_sumSquares", () => {
		it("ok", () => {
			const e: Expr = parse(load("foldr_sumSquares"))
         new FwdSlice(e)
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
		})
	})

	describe("lexicalScoping", () => {
		it("ok", () => {
			const e: Expr = parse(load("lexicalScoping"))
         new FwdSlice(e)
		})
	})

   describe("lookup", () => {
		it("ok", () => {
			const e: Expr = parse(load("lookup"))
			const last = new (class extends FwdSlice {
				setup (): void {
					this.expr
						.skipImports()
						.to(Expr.Defs, "e")
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
						.to(Expr.Defs, "e")
					 	.to(Expr.App, "arg")
						.constrArg("Cons", 0).notNeed()
				  }
				expect (): void {
					this.val
						.push().to(Cons, "head").notNeeded().pop()
						.to(Cons, "tail").needed()
				}
			})(e)
		})
	})

   describe("mergeSort", () => {
		it("ok", () => {
			const e: Expr = parse(load("mergeSort"))
         new FwdSlice(e)
		})
	})

	describe("normalise", () => {
		it("ok", () => {
			const e: Expr = parse(load("normalise"))
         new FwdSlice(e)
		})
   })

	describe("pattern-match", () => {
		it("ok", () => {
			const e: Expr = parse(load("pattern-match"))
         new FwdSlice(e)
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
						.push().to(Cons, "head").needed().pop()
						.to(Cons, "tail").needed()
				}
			})(e)
		})
   })

	describe("zipW", () => {
		it("ok", () => {
			const e: Expr = parse(load("zipW"))
         new FwdSlice(e)
		})
   })
})
