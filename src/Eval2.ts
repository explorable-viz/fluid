import { __nonNull, absurd } from "./util/Core"
import { List, ListFunc } from "./BaseTypes2"
import { DataType, datatypeFor } from "./DataType2"
import { Env, concat } from "./Env2"
import { Expr } from "./Expr2"
import { State_Dyn, Value, construct } from "./ExplVal2"
import { interpretTrie } from "./Match2"

import ExprFunc = Expr.ExprFunc
import Trie = Expr.Trie

type InterpretExpr = (ρ: Env) => Value

// Repeatedly reinterprets subexpressions, so probably as slow as the previous implementation.
// Should be able to significantly speed up by memoisation.
export function interpret (e: Expr): InterpretExpr {
   return e.__match(new (class extends ExprFunc<InterpretExpr> {
      Var (x: string): InterpretExpr {
         return (ρ: Env) => __nonNull(ρ[x])
      }
      Constr (ctr: string, args: List<Expr>): InterpretExpr {
         return (ρ: Env): Value => {
            const d: DataType = __nonNull(datatypeFor.get(ctr)),
                  state: State_Dyn = {}
            let e̅: List<Expr> = args
            for (const f of d.fields) {
               e̅.__match(new (class extends ListFunc<Expr, void> {
                  Nil (): void {
                     absurd()
                  }
                  Cons (e: Expr, e̅ʹ: List<Expr>): void {
                     state[f] = interpret(e)(ρ)
                     e̅ = e̅ʹ
                  }
               }))
            }
            return construct(new d.cls, state)
         }
      }
      Fun (σ: Trie<Expr>): InterpretExpr {
         return (ρ: Env) => interpretTrie(σ)
      }
      MatchAs (e: Expr, σ: Trie<Expr>): InterpretExpr {
         return (ρ: Env): Value => {
            const [ρʹ, eʹ] = interpretTrie(σ).__apply(interpret(e)(ρ))
            return interpret(eʹ)(concat(ρ, ρʹ))
         }
      }
   }))
}
