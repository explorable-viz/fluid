import { __nonNull, absurd } from "./util/Core"
import { List } from "./BaseTypes2"
import { DataType, datatypeFor } from "./DataType2"
import { Expr } from "./Expr2"
import { Expl, ExplVal, Stateʹ, construct } from "./ExplVal2"

export function eval_ (e: Expr): ExplVal {
   return e.match({
      Constr(ctr, args): ExplVal {
         const d: DataType = __nonNull(datatypeFor.get(ctr)),
               state: Stateʹ = {}
         let e̅: List<Expr> = args
         for (const f of d.fields) {
            e̅.match({
               Nil(): void {
                  absurd()
               },
               Cons(e: Expr, e̅ʹ: List<Expr>): void {
                  state[f] = eval_(e)
                  e̅ = e̅ʹ
               }
            })
         }
         return [Expl.empty(), construct(new d.cls, state)]
      },
      Fun(σ): ExplVal {
         return σ.match({

         })
      },
      MatchAs(e, σ): ExplVal {
         throw new Error
      }
   })
}
