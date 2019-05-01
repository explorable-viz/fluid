import { __nonNull, absurd } from "./util/Core"
import { List } from "./BaseTypes2"
import { DataType, datatypeFor } from "./DataType2"
import { Expr } from "./Expr2"
import { Expl, ExplVal, State_Dyn, construct } from "./ExplVal2"

export function eval_ (e: Expr): ExplVal {
   return e.__match({
      Var(): ExplVal {
         throw new Error
      },
      Constr(ctr, args): ExplVal {
         const d: DataType = __nonNull(datatypeFor.get(ctr)),
               state: State_Dyn = {}
         let e̅: List<Expr> = args
         for (const f of d.fields) {
            e̅.__match({
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
         return σ.__match({
            Var(): ExplVal {
               throw new Error
            },
            Constr(cases): ExplVal {
               // create a "trie object" o such that
               // for every ctr key of cases
                  // o has a field with that name, whose value is a function from arguments to EvalVal obtained by 
               throw new Error
            }
         })
      },
      MatchAs(e, σ): ExplVal {
         throw new Error
      }
   })
}
