import { __nonNull, absurd } from "./util/Core"
import { List } from "./BaseTypes2"
import { DataType, datatypeFor } from "./DataType2"
import { Expr } from "./Expr2"
import { State_Dyn, Value, construct } from "./ExplVal2"

type Env = never // for now

// Repeatedly reinterprets subexpressions, so probably as slow as the previous implementation.
// Should be able to significantly speed up by memoisation.
export function interpret (e: Expr): (ρ: Env) => Value {
   return e.__match({
      Var(x): (ρ: Env) => Value {
         return (ρ: Env) => __nonNull(ρ[x])
      },
      Constr(ctr, args): (ρ: Env) => Value {
         return (ρ: Env): Value => {
            const d: DataType = __nonNull(datatypeFor.get(ctr)),
                  state: State_Dyn = {}
            let e̅: List<Expr> = args
            for (const f of d.fields) {
               e̅.__match<void>({
                  Nil(): void {
                     absurd()
                  },
                  Cons(e: Expr, e̅ʹ: List<Expr>): void {
                     state[f] = interpret(e)(ρ)
                     e̅ = e̅ʹ
                  }
               })
            }
            return construct(new d.cls, state)
         }
      },
      Fun(σ): (ρ: Env) => Value {
         return σ.__match({
            Var(x, κ): (ρ: Env) => Value {
               throw new Error
            },
            Constr(cases): (ρ: Env) => Value {
               // create a "trie object" o such that
               // for every ctr key of cases
                  // o has a field with that name, whose value is a function from arguments to Value obtained by 
               throw new Error
            }
         })
      },
      MatchAs(e, σ): (ρ: Env) => Value {
         return (ρ: Env): Value => {
            return interpret(interpret(e)(ρ).__match<Expr>(σ))(ρ)
         }
      }
   })
}
