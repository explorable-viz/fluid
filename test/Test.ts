/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { BwdSlice, FwdSlice } from "./util/Core"
import { Cons, List, Nil, NonEmpty, Pair, Some } from "../src/BaseTypes"
import { Env, ExtendEnv } from "../src/Env"
import { Expr } from "../src/Expr"
import { Graphic, Polygon, Point, Translate } from "../src/Graphics"
import { module_graphics, open, openDatasetAs, openWithImports } from "../src/Module"
import { Str } from "../src/Value"
import { Cursor } from "./util/Cursor"

import Trie = Expr.Trie

before((done: MochaDone) => {
	done()
})

// Putting test name in a variable interacts poorly with asynchronous execution.
describe("example", () => {
	describe("arithmetic", () => {
		it("ok", () => {
         const e: Expr = open("arithmetic")
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
			new BwdSlice(e)
		})
   })

   describe("bar-chart", () => {
		it("ok", () => {
         const ρ: Env = openDatasetAs("renewables", "data"),
			      e: Expr = openWithImports("bar-chart", [module_graphics])
			new (class extends FwdSlice {
            setup (): void {
               const data: Cursor = new Cursor(ρ)
               data.to(ExtendEnv, "v")
                   .to(Cons, "head")
                   .to(Pair, "snd")
                   .to(Cons, "head")
                   .to(Pair, "snd")
                   .to(Cons, "head")
                   .to(Pair, "snd").notNeed()
            }
            expect (): void {
               this.val
                  .to(Graphic, "gs")
                  .to(Cons, "head")
                  .to(Graphic, "gs")
                  .to(Cons, "tail")
                  .to(Cons, "head")
                  .to(Translate, "g")
                  .to(Graphic, "gs")
                  .to(Cons, "head")
                  .to(Translate, "g")
                  .to(Translate, "g")
                  .to(Graphic, "gs")
                  .to(Cons, "head")
                  .to(Translate, "g")
                  .to(Graphic, "gs")
                  .to(Cons, "head")
                  .to(Translate, "g")
                  .to(Polygon, "points")
                  .to(Cons, "head")
                  .push().to(Point, "x").notNeeded().pop()
                  .push().to(Point, "y").notNeeded().pop()
            }
         })(e, ρ)
			new (class extends BwdSlice {
				setup (): void {
					this.val.need()
				}
				expect (): void {
					this.expr.needed()
				}
			})(e, ρ)
		})
   })

   describe("compose", () => {
		it("ok", () => {
			const e: Expr = open("compose")
			new FwdSlice(e)
			new BwdSlice(e)
		})
	})

   describe("factorial", () => {
		it("ok", () => {
         const e: Expr = open("factorial")
         new FwdSlice(e)
			new BwdSlice(e)
		})
	})

   describe("filter", () => {
		it("ok", () => {
			const e: Expr = open("filter")
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
						.var_("x").var_("xs")
						.to(Expr.MatchAs, "σ")
						.to(Trie.Constr, "cases")
						.nodeValue()
						.constrArg("Cons", 0).notNeed()
				}
				expect (): void {
					this.val
						.needed()
						.push().to(Cons, "head").notNeeded().pop()
						.to(Cons, "tail")
						.assert(List, v => Nil.is(v))
				}
			})(e)
			new BwdSlice(e)
		})
	})

   describe("foldr_sumSquares", () => {
		it("ok", () => {
			const e: Expr = open("foldr_sumSquares")
         new FwdSlice(e)
			new BwdSlice(e)
		})
	})

   describe("length", () => {
		it("ok", () => {
			const e: Expr = open("length")
			// erasing the elements doesn't affect the count:
			let test = new (class extends FwdSlice {
				setup (): void {
					this.expr
						.skipImports()
						.to(Expr.App, "e")
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
						.to(Expr.App, "e").needed()
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
			const e: Expr = open("lexicalScoping")
         new FwdSlice(e)
			new BwdSlice(e)
		})
	})

   describe("lookup", () => {
		it("ok", () => {
         const e: Expr = open("lookup")
			const last = new (class extends FwdSlice {
				setup (): void {
					this.expr
						.skipImports()
						.to(Expr.Defs, "e")
						.to(Expr.App, "e")
						.push()
							.constrArg("NonEmpty", 0)
							.constrArg("NonEmpty", 1)
							.constrArg("Pair", 0).notNeed().pop()
				}
				expect (): void {
               this.val
                  .push()
                     .to(Some, "t")
                     .assert(Str, str => str.toString() === `"sarah"`).pop()
                  .needed()
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
			new BwdSlice(e)
		})
	})

   describe("map", () => {
		it("ok", () => {
			const e: Expr = open("map")
			new (class extends FwdSlice {
				setup (): void {
					this.expr
						.skipImports()
						.to(Expr.Defs, "e")
					 	.to(Expr.App, "e")
						.constrArg("Cons", 0).notNeed()
				  }
				expect (): void {
					this.val
						.push().to(Cons, "head").notNeeded().pop()
						.to(Cons, "tail").needed()
				}
			})(e)
			new BwdSlice(e)
		})
	})

   describe("mergeSort", () => {
		it("ok", () => {
			const e: Expr = open("mergeSort")
         new FwdSlice(e)
			new BwdSlice(e)
		})
	})

	describe("normalise", () => {
		it("ok", () => {
			const e: Expr = open("normalise")
         new FwdSlice(e)
			// retaining either component of pair retains both subcomputations:
			new (class extends BwdSlice {
				setup (): void {
					this.val
						.to(Pair, "fst").need()
				}
				expect (): void {
					this.expr
                  .skipImports()
                  .push().toDef("x").to(Expr.Let, "e").needed().pop()
						.toDef("y").to(Expr.Let, "e").needed()
				}
			})(e)
		})
   })

	describe("pattern-match", () => {
		it("ok", () => {
			const e: Expr = open("pattern-match")
			new BwdSlice(e)
         new FwdSlice(e)
		})
   })

	describe("reverse", () => {
		it("ok", () => {
			const e: Expr = open("reverse")
			new (class extends FwdSlice {
				setup (): void {
					this.expr
						.skipImports()
 						.to(Expr.App, "e")
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
			new BwdSlice(e)
		})
   })

   describe("typematch", () => {
		it("ok", () => {
			const e: Expr = open("typematch")
			new FwdSlice(e)
			new BwdSlice(e)
		})
	})

	describe("zipW", () => {
		it("ok", () => {
			const e: Expr = open("zipW")
         new FwdSlice(e)
			// needing first cons cell of output needs same amount of input lists
			new (class extends BwdSlice {
				setup (): void {
					this.val.need()
				}
				expect (): void {
					this.expr
						.push()
                     .toDef("zipW")
                     .push().to(Expr.RecDef, "x").needed().pop()
							.to(Expr.RecDef, "σ")
							.var_("op").needed()
							.pop()
						.skipImports()
						.push()
							.to(Expr.App, "e").needed().pop()
						.push()
							.to(Expr.App, "f")
						  	.to(Expr.App, "e").needed().pop()
				}
			})(e)
			// needing constructor of first element requires constructor at head of supplied op, plus application of op in zipW
			new (class extends BwdSlice {
				setup (): void {
					this.val
						.to(Cons, "head").need()
				}
				expect (): void {
					this.expr
						.push()
							.toDef("zipW")
							.to(Expr.RecDef, "σ")
							.var_("op")
							.to(Expr.Fun, "σ")
							.to(Trie.Constr, "cases")
							.push().nodeValue().notNeeded().pop() // body of outer Nil clause
							.to(NonEmpty, "left")
							.nodeValue()			 
							.var_("x").var_("xs").notNeeded()
							.to(Expr.Fun, "σ")
							.to(Trie.Constr, "cases")
							.to(NonEmpty, "left")
							.nodeValue()			 
							.var_("y").var_("ys").notNeeded() // cons constructor
							.constrArg("Cons", 0).needed() // application of op
							.to(Expr.App, "e").needed()  // pair constructor
							.push().constrArg("Pair", 0).notNeeded().pop()
							.push().constrArg("Pair", 1).notNeeded().pop()
							.pop()
						.skipImports()
						.to(Expr.App, "f")
						.to(Expr.App, "f")
						.to(Expr.App, "e")
						.to(Expr.Fun, "σ")
						.to(Trie.Constr, "cases")
						.nodeValue()
						.var_("x").var_("y").needed()
				}
			})(e)
		})
   })
})
