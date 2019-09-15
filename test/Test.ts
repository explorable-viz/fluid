/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { BwdSlice, FwdSlice } from "./util/Core"
import { Cons, List, Nil, NonEmpty, Pair, Some } from "../src/BaseTypes"
import { ExtendEnv } from "../src/Env"
import { Expr } from "../src/Expr"
import { Graphic, Polygon, Point, Translate } from "../src/Graphics"
import { module_graphics, open, openDatasetAs, openWithImports } from "../src/Module"
import { Str } from "../src/Value"
import { Cursor, ExplCursor } from "..//src/app/Cursor"

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
                  .to(Expr.BinaryApp, "e1").clearα()
            }
            expect (): void {
               this.tv.αclear()
            } 
         })(e)
         new BwdSlice(e)
      })
   })

   describe("bar-chart", () => {
      it("ok", () => {
         const ρ: ExtendEnv = openDatasetAs("renewables", "data"),
               e: Expr = openWithImports("bar-chart", [module_graphics])
         new (class extends FwdSlice {
            setup (): void {
               const here: ExplCursor = new ExplCursor(ρ.tv)
               here.to(Cons, "head")
                   .to(Pair, "snd")
                   .to(Cons, "head")
                   .to(Pair, "snd")
                   .to(Cons, "head")
                   .to(Pair, "snd").clearα()
            }
            expect (): void {
               const here: ExplCursor = this.tv
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
                  .to(Cons, "tail")
                  .to(Cons, "tail")
               here.to(Cons, "head").to(Point, "y").αclear()
               here.to(Cons, "tail").to(Cons, "head").to(Point, "y").αclear()
            }
         })(e, ρ)
         new (class extends BwdSlice {
            setup (): void {
               this.tv.setα()
            }
            expect (): void {
               this.expr.αset()
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
                  .constrArg("Cons", 0).clearα()
            }
            expect (): void {
               this.tv.αset()
               this.tv.to(Cons, "head").αclear()
               this.tv.to(Cons, "tail").assert(List, v => Nil.is(v))
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
         const e: Expr = open("length"),
               here: Cursor = new Cursor(e).skipImports().to(Expr.App, "e")
         // erasing the elements doesn't affect the count:
         new (class extends FwdSlice {
            setup (): void {
               here.constrArg("Cons", 0).clearα()
               here.constrArg("Cons", 1).constrArg("Cons", 0).clearα()
            }
            expect (): void {
               this.tv.αset()
            }
         })(e)
         // deleting the tail of the tail means length can't be computed:
         new (class extends FwdSlice {
            setup (): void {
               here.constrArg("Cons", 1).clearα()
            }
            expect (): void {
               this.tv.αclear()
            }
         })(e)
         // needing the result only needs the cons cells:
         new (class extends BwdSlice {
            setup (): void {
               this.tv.setα()
            }
            expect (): void {
               here.αset()
               here.constrArg("Cons", 0).αclear()
               let hereʹ = here.constrArg("Cons", 1).αset()
               hereʹ.constrArg("Cons", 0).αclear()
               hereʹ.constrArg("Cons", 1).αset()
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
         const e: Expr = open("lookup"),
               here: Cursor = new Cursor(e)
            .skipImports()
            .to(Expr.Defs, "e")
            .to(Expr.App, "e")
            .constrArg("NonEmpty", 0)
            .constrArg("NonEmpty", 1)
            .constrArg("Pair", 0)
      new (class extends FwdSlice {
            setup (): void {
               here.clearα()
            }
            expect (): void {
               this.tv.to(Some, "t").assert(Str, str => str.toString() === `"sarah"`)
               this.tv.αset()
            }
         })(e)
         new (class extends FwdSlice {
            setup (): void {
               here
                  .constrArg("NonEmpty", 1)
                  .constrArg("Pair", 0).clearα()
            }
            expect (): void {
               this.tv.αclear()
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
                  .constrArg("Cons", 0).clearα()
              }
            expect (): void {
               this.tv.to(Cons, "head").αclear()
               this.tv.to(Cons, "tail").αset()
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
               this.tv.to(Pair, "fst").setα()
            }
            expect (): void {
               const here: Cursor = this.expr.skipImports()
               here.toDef("x").to(Expr.Let, "e").αset()
               here.toDef("y").to(Expr.Let, "e").αset()
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
                   .constrArg("Cons", 1).clearα()
            }
            expect (): void {
               this.tv.αclear()
               this.tv.to(Cons, "head").αset()
               this.tv.to(Cons, "tail").αset()
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
               this.tv.setα()
            }
            expect (): void {
               let here: Cursor = this.expr
               here.toDef("zipW").αset().to(Expr.RecDef, "σ").var_("op").αset()
               here = here.skipImports()
               here.to(Expr.App, "e").αset()
               here.to(Expr.App, "f").to(Expr.App, "e").αset()
            }
         })(e)
         // needing constructor of first element requires constructor at head of supplied op, plus application of op in zipW
         new (class extends BwdSlice {
            setup (): void {
               this.tv.to(Cons, "head").setα()
            }
            expect (): void {
               const here: Cursor = this.expr
               let hereʹ: Cursor = here
                  .toDef("zipW")
                  .to(Expr.RecDef, "σ")
                  .var_("op")
                  .to(Expr.Fun, "σ")
                  .to(Trie.Constr, "cases")
               hereʹ.nodeValue().αclear() // body of outer Nil clause
               hereʹ = hereʹ
                  .to(NonEmpty, "left")
                  .nodeValue()          
                  .var_("x").var_("xs").αclear()
                  .to(Expr.Fun, "σ")
                  .to(Trie.Constr, "cases")
                  .to(NonEmpty, "left")
                  .nodeValue()          
                  .var_("y").var_("ys").αclear() // cons constructor
                  .constrArg("Cons", 0).αset() // application of op
                  .to(Expr.App, "e").αset()  // pair constructor
               hereʹ.constrArg("Pair", 0).αclear()
               hereʹ.constrArg("Pair", 1).αclear()
               here
                  .skipImports()
                  .to(Expr.App, "f")
                  .to(Expr.App, "f")
                  .to(Expr.App, "e")
                  .to(Expr.Fun, "σ")
                  .to(Trie.Constr, "cases")
                  .nodeValue()
                  .var_("x").var_("y").αset()
            }
         })(e)
      })
   })
})
