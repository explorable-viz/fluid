import { Class, __nonNull, absurd } from "./util/Core"
import { List, ListFunc, map } from "./BaseTypes2"
import { DataType, datatypeFor } from "./DataType2"
import { Expr } from "./Expr2"
import { State_Dyn, Func, Value, construct } from "./ExplVal2"
import { FiniteMap } from "./FiniteMap2"

import Args = Expr.Args
import ExprFunc = Expr.ExprFunc
import Trie = Expr.Trie
import TrieFunc = Trie.TrieFunc

type Env = never // for now
type InterpretExpr = (ρ: Env) => Value

// Repeatedly reinterprets subexpressions, so probably as slow as the previous implementation.
// Should be able to significantly speed up by memoisation.
export function interpret (e: Expr): InterpretExpr {
   return e.__match(new (class extends ExprFunc<InterpretExpr> {
      Var(x: string): InterpretExpr {
         return (ρ: Env) => __nonNull(ρ[x])
      }
      Constr(ctr: string, args: List<Expr>): InterpretExpr {
         return (ρ: Env): Value => {
            const d: DataType = __nonNull(datatypeFor.get(ctr)),
                  state: State_Dyn = {}
            let e̅: List<Expr> = args
            for (const f of d.fields) {
               e̅.__match(new (class extends ListFunc<Expr, void> {
                  Nil(): void {
                     absurd()
                  }
                  Cons(e: Expr, e̅ʹ: List<Expr>): void {
                     state[f] = interpret(e)(ρ)
                     e̅ = e̅ʹ
                  }
               }))
            }
            return construct(new d.cls, state)
         }
      }
      Fun(σ: Trie<Expr>): InterpretExpr {
         return σ.__match(new (class extends TrieFunc<Expr, InterpretExpr> {
            Var(x: string, κ: Expr): (ρ: Env) => Value {
               throw new Error
            }
            Constr(cases: FiniteMap<string, Args<Expr>>): InterpretExpr {
               const handlers: State_Dyn = {} // TODO: fix type
               // create a "fun object" o such that
               map(cases, ({ fst: ctr, snd: Π }): void => {
                  handlers[ctr] = null // whose value is a function from arguments to Value obtained by 
               })
               throw new Error
            }
         }))
      }
      MatchAs(e: Expr, σ: Trie<Expr>): InterpretExpr {
         return (ρ: Env): Value => {
            return interpret(interpret(e)(ρ).__match(interpretTrie(σ)))(ρ)
         }
      }
   }))
}

function interpretTrie<T> (σ: Trie<T>): Func<T> {

}
