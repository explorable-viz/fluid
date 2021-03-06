/// <reference path="../node_modules/@types/mocha/index.d.ts" />

import { __nonNull, assert } from "../src/util/Core"
import { Cons, List, Nil, NonEmpty, Pair, Some, True } from "../src/BaseTypes"
import { Env, ExtendEnv, emptyEnv } from "../src/Env"
import { Expr } from "../src/Expr"
import { __slice } from "../src/Annotation" // Webpack confused by dependencies; put after Expr
import { Group, Point, Polymarkers, Rect,  Viewport } from "../src/Graphics"
import { Elim } from "../src/Match"
import { bindDataset, openDatasetAs, openWithImports } from "../src/Module"
import { Num, Str } from "../src/Value"
import { ExprCursor, ExplValueCursor } from "../src/app/Cursor"
import { Pane } from "../src/app/Pane"
import { PaneCoordinator } from "../src/app/PaneCoordinator"
import { BwdSlice, FwdSlice, funDef, tooltipsEqual } from "./util/Core"

before((done: MochaDone) => {
   Pane.initialise(".")
   done()
})

// Putting test name in a variable interacts poorly with asynchronous execution.
describe("slice", () => {
   describe("arithmetic", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports("arithmetic")
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
         const [ρ, e]: [Env, Expr] = openWithImports("compose")
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
         const [ρʹ, e]: [Env, Expr] = openWithImports("create-dataset")
         new FwdSlice(ρ.concat(ρʹ), e)
      })
   })

   describe("factorial", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports("factorial")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("filter", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports("filter")
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
               here.nth(0).αclear()
               here.to(Cons, "tail").to(Cons, "tail").assert(List, v => Nil.is(v))
            }
         })(ρ,e )
         new BwdSlice(ρ, e)
      })
   })

   describe("foldr_sumSquares", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports("foldr_sumSquares")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("graphics/background", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports("graphics/background")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("graphics/renewables", () => {
      it("ok", () => {
         const coordinator: PaneCoordinator = new PaneCoordinator(document.body, openDatasetAs("renewables-restricted", "data"))
         const [ρ1, e1]: [Env, Expr] = openWithImports("graphics/grouped-bar-chart")
         const [ρ2, e2]: [Env, Expr] = openWithImports("graphics/stacked-bar-chart")
         const [ρ3, e3]: [Env, Expr] = openWithImports("graphics/line-chart")
         const groupedBar: Pane.Pane = coordinator.addPane(ρ1, e1)
         const stackedBar: Pane.Pane = coordinator.addPane(ρ2, e2)
         const line: Pane.Pane = coordinator.addPane(ρ3, e3)
         line.bwdSlice((): void => {
            const here: ExplValueCursor = ExplValueCursor.descendant(null, line.tv)
            here.to(Viewport, "g")
                .to(Group, "gs")
                .nth(1) // skip caption
                .to(Viewport, "g")
                .to(Group, "gs")
                .nth(3) // first two elements are axes; third element is total
                .to(Group, "gs")
                .nth(1) // first element is polyline; second is polymarkers
                .to(Polymarkers, "points")
                .nth(2) // third point is 2015
                .to(Point, "y").assert(Num, n => n.val === 10.3)
                .setα()
         })
         assert(tooltipsEqual(groupedBar.visibleTooltips(), ["height: 10.3"]))
         assert(tooltipsEqual(stackedBar.visibleTooltips(), ["height: 10.3", "y: 10.3", "y: 106.3", "y: 150.3"]))
         stackedBar.bwdSlice((): void => {
            const here: ExplValueCursor = ExplValueCursor.descendant(null, stackedBar.tv)
            here.to(Viewport, "g")
                .to(Group, "gs")
                .nth(1) // skip caption
                .to(Viewport, "g")
                .to(Group, "gs")
                .nth(0) // first stacked bar
                .to(Viewport, "g")
                .to(Group, "gs")
                .nth(3) // 4th component of stacked bar
                .to(Rect, "y").assert(Num, n => n.val === 150.3)
                .setα()
         })
         assert(tooltipsEqual(groupedBar.visibleTooltips(), ["height: 10.3", "height: 96", "height: 44"]))
         assert(tooltipsEqual(line.visibleTooltips(), ["y: 295.3", "y: 10.3", "y: 96", "y: 44"]))
         groupedBar.bwdSlice((): void => {
            const here: ExplValueCursor = ExplValueCursor.descendant(null, groupedBar.tv)
            here.to(Viewport, "g")
                .to(Group, "gs")
                .nth(1) // skip caption
                .to(Viewport, "g")
                .to(Group, "gs")
                .nth(0) // first group of bars
                .to(Viewport, "g")
                .to(Group, "gs")
                .nth(3) // 4th bar in group
                .to(Rect, "height").assert(Num, n => n.val === 145)
                .setα()
         })
         assert(tooltipsEqual(stackedBar.visibleTooltips(), ["height: 145"]))
         assert(tooltipsEqual(line.visibleTooltips(), ["y: 295.3", "y: 145"]))
      })
   })

   describe("length", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports("length"),
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
         const [ρ, e]: [Env, Expr] = openWithImports("lexicalScoping")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("lookup", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports("lookup"),
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
         const [ρ, e]: [Env, Expr] = openWithImports("map")
         new (class extends FwdSlice {
            setup (here: ExprCursor): void {
               here
                  .to(Expr.Defs, "e")
                  .to(Expr.App, "e")
                  .constr_to(Cons, "head").clearα()
              }
            expect (here: ExplValueCursor): void {
               here.nth(0).αclear()
               here.to(Cons, "tail").αset()
            }
         })(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("mergeSort", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports("mergeSort")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("normalise", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports("normalise")
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
         const [ρ, e]: [Env, Expr] = openWithImports("pattern-match")
         new BwdSlice(ρ, e)
         new FwdSlice(ρ, e)
      })
   })

   describe("reverse", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports("reverse")
         new (class extends FwdSlice {
            setup (here: ExprCursor): void {
               here
                  .to(Expr.App, "e")
                  .constr_to(Cons, "tail")
                  .constr_to(Cons, "tail").clearα()
            }
            expect (here: ExplValueCursor): void {
               here.αclear()
               here.nth(0).αset()
               here.to(Cons, "tail").αset()
            }
         })(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("typematch", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports("typematch")
         new FwdSlice(ρ, e)
         new BwdSlice(ρ, e)
      })
   })

   describe("zipWith", () => {
      it("ok", () => {
         const [ρ, e]: [Env, Expr] = openWithImports("zipWith")
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
               here.nth(0).setα()
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
