/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { __nonNull } from "../src/util/Core"
import { Cons, List, Nil, NonEmpty, Pair, Some, True } from "../src/BaseTypes"
import { Env, ExtendEnv, emptyEnv } from "../src/Env"
import { Expr } from "../src/Expr"
import { Elim } from "../src/Match"
import { Module, bindDataset, openDatasetAs, openWithImports2 } from "../src/Module"
import { Str } from "../src/Value"
import { ExprCursor, ExplValueCursor } from "..//src/app/Cursor"
import { BwdSlice, FwdSlice, funDef } from "./util/Core"

before((done: MochaDone) => {
   Module.initialise()
   done()
})

// Putting test name in a variable interacts poorly with asynchronous execution.
describe("slice", () => {
   describe("arithmetic", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("arithmetic")
         new (class extends FwdSlice {
            setup (here: ExprCursor): void {
               here
                  .to(Expr.BinaryApp, "e1").clearα()
            }
            expect (here: ExplValueCursor): void {
               here.αclear()
            } 
         })(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("compose", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("compose")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   // merge with bar-chart test case once we implement loading from JSON or similar
   describe("create-dataset", () => {
      it("ok", () => {
         const data: Object[] = [
            // some subset of the renewables dataset
            { year: 2015, country: "China", energyType: "Bio", value: 10.3 },
            { year: 2015, country: "China", energyType: "Geothermal", value: 0 },
            { year: 2015, country: "China", energyType: "Hydro", value: 296 }
         ]
         const ρ: ExtendEnv = bindDataset(emptyEnv(), data, "data")
         const [ρʹ, e]: [Env, Expr] = openWithImports2("create-dataset")
         new FwdSlice(ρ.concat(ρʹ), e)
      })
   })

   describe("factorial", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("factorial")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("filter", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("filter")
         new (class extends FwdSlice {
            setup (_: ExprCursor): void {
               const σ: Elim<Expr> = funDef(ρ, "filter")
               const here: ExprCursor = new ExprCursor(σ)
               here
                  .var_("p")
                  .to(Expr.Fun, "σ")
                  .toCase(Cons)
                  .var_("x").var_("xs")
                  .to(Expr.Defs, "e")
                  .to(Expr.MatchAs, "σ")
                  .toCase(True)
                  .constr_to(Cons, "head").clearα()
            }
            expect (here: ExplValueCursor): void {
               here.αset()
               here.to(Cons, "head").αclear()
               here.to(Cons, "tail").to(Cons, "tail").assert(List, v => Nil.is(v))
            }
         })(ρ,e )
         new BwdSlice(ρ, e)
      })
   })

   describe("foldr_sumSquares", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("foldr_sumSquares")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("length", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("length"),
               here: ExprCursor = new ExprCursor(e).to(Expr.App, "e")
         // erasing the elements doesn't affect the count:
         new (class extends FwdSlice {
            setup (_: ExprCursor): void {
               here.constr_to(Cons, "head").clearα()
               here.constr_to(Cons, "tail").constr_to(Cons, "head").clearα()
            }
            expect (here: ExplValueCursor): void {
               here.αset()
            }
         })(ρ, e)
         // deleting the tail of the tail means length can't be computed:
         new (class extends FwdSlice {
            setup (_: ExprCursor): void {
               here.constr_to(Cons, "tail").clearα()
            }
            expect (here: ExplValueCursor): void {
               here.αclear()
            }
         })(ρ, e)
         // needing the result only needs the cons cells:
         new (class extends BwdSlice {
            setup (here: ExplValueCursor): void {
               here.setα()
            }
            expect (): void {
               here.αset()
               here.constr_to(Cons, "head").αclear()
               let hereʹ = here.constr_to(Cons, "tail").αset()
               hereʹ.constr_to(Cons, "head").αclear()
               hereʹ.constr_to(Cons, "tail").αset()
            }
         })(ρ, e)
      })
   })

   describe("lexicalScoping", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("lexicalScoping")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("lookup", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("lookup"),
               here: ExprCursor = new ExprCursor(e)
            .to(Expr.Defs, "e")
            .to(Expr.App, "e")
	      new (class extends FwdSlice {
            setup (_: ExprCursor): void {
					here
						.constr_to(NonEmpty, "left")
						.constr_to(NonEmpty, "t")
						.constr_to(Pair, "fst").clearα()
            }
            expect (here: ExplValueCursor): void {
               here.to(Some, "t").assert(Str, str => str.toString() === `"sarah"`)
               here.αset()
            }
         })(ρ, e)
         new (class extends FwdSlice {
            setup (_: ExprCursor): void {
               here
                  .constr_to(NonEmpty, "t")
                  .constr_to(Pair, "fst").clearα()
            }
            expect (here: ExplValueCursor): void {
               here.αclear()
            }
         })(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("map", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("map")
         new (class extends FwdSlice {
            setup (here: ExprCursor): void {
               here
                  .to(Expr.Defs, "e")
                  .to(Expr.App, "e")
                  .constr_to(Cons, "head").clearα()
              }
            expect (here: ExplValueCursor): void {
               here.to(Cons, "head").αclear()
               here.to(Cons, "tail").αset()
            }
         })(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("mergeSort", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("mergeSort")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("graphics/background", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("graphics/background")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("graphics/grouped-bar-chart", () => {
      it("ok", () => {
         const ρ: ExtendEnv = openDatasetAs("renewables", "data")
         const [ρʹ, e]: [Env, Expr] = openWithImports2("graphics/grouped-bar-chart")
         new FwdSlice(ρ.concat(ρʹ), e)
         new BwdSlice(ρ.concat(ρʹ), e)
      })
   })

   describe("graphics/line-chart", () => {
      it("ok", () => {
         const ρ: ExtendEnv = openDatasetAs("renewables", "data")
         const [ρʹ, e]: [Env, Expr] = openWithImports2("graphics/line-chart")
         new FwdSlice(ρ.concat(ρʹ), e)
         new BwdSlice(ρ.concat(ρʹ), e)
      })
   })

   describe("graphics/stacked-bar-chart", () => {
      it("ok", () => {
         const ρ: ExtendEnv = openDatasetAs("renewables", "data")
         const [ρʹ, e]: [Env, Expr] = openWithImports2("graphics/stacked-bar-chart")
         new FwdSlice(ρ.concat(ρʹ), e)
         new BwdSlice(ρ.concat(ρʹ), e)
      })
   })

   describe("normalise", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("normalise")
         new FwdSlice(ρ, e)
         // retaining either component of pair retains both subcomputations:
         new (class extends BwdSlice {
            setup (here: ExplValueCursor): void {
               here.to(Pair, "fst").setα()
            }
            expect (here: ExprCursor): void {
               here.toDef("x").to(Expr.Let, "e").αset()
               here.toDef("y").to(Expr.Let, "e").αset()
            }
         })(ρ, e)
      })
   })

   describe("pattern-match", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("pattern-match")
         new BwdSlice(ρ, e)
         new FwdSlice(ρ, e)
      })
   })

   describe("reverse", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("reverse")
         new (class extends FwdSlice {
            setup (here: ExprCursor): void {
               here
                  .to(Expr.App, "e")
                  .constr_to(Cons, "tail")
                  .constr_to(Cons, "tail").clearα()
            }
            expect (here: ExplValueCursor): void {
               here.αclear()
               here.to(Cons, "head").αset()
               here.to(Cons, "tail").αset()
            }
         })(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("typematch", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("typematch")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("zipWith", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports2("zipWith")
         new FwdSlice(ρ, e)
         // needing first cons cell of output needs same amount of input lists
         new (class extends BwdSlice {
            setup (here: ExplValueCursor): void {
               here.setα()
            }
            expect (here: ExprCursor): void {
               new ExprCursor(funDef(ρ, "zipWith")).var_("op").αset()
               here.to(Expr.App, "e").αset()
               here.to(Expr.App, "f").to(Expr.App, "e").αset()
            }
         })(ρ, e)
         // needing constructor of first element requires constructor at head of supplied op, plus application of op in zipW
         new (class extends BwdSlice {
            setup (here: ExplValueCursor): void {
               here.to(Cons, "head").setα()
            }
            expect (here: ExprCursor): void {
               let hereʹ: ExprCursor = new ExprCursor(funDef(ρ, "zipWith"))
                  .var_("op")
                  .to(Expr.Fun, "σ")
               hereʹ.toCase(Nil).αclear() // body of outer Nil clause
               hereʹ = hereʹ
                  .toCase(Cons)
                  .var_("x").var_("xs").αclear()
                  .to(Expr.Fun, "σ")
                  .toCase(Cons)
                  .var_("y").var_("ys").αclear() // cons constructor
                  .constr_to(Cons, "head").αset() // application of op
                  .to(Expr.App, "e").αset()  // pair constructor
               hereʹ.constr_to(Pair, "fst").αclear()
               hereʹ.constr_to(Pair, "snd").αclear()
               here
                  .to(Expr.App, "f")
                  .to(Expr.App, "f")
                  .to(Expr.App, "e")
                  .to(Expr.Fun, "σ")
                  .toCase(Pair)
                  .var_("x").var_("y").αset()
            }
         })(ρ, e)
      })
   })
})
