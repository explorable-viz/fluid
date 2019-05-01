import { Class, __nonNull, absurd, funName } from "./util/Core"
import { List } from "./BaseTypes2"
import { Env } from "./Env"

type ExplVal = [Expl, Value]

// TODO: exclude metadata in a way that's consistent with Metadata<T>
export function isField (prop: string): boolean {
   throw new Error
}

// Utterly dependent on fields being provided in declaration order, although not part of spec :-/
export function fields (cls: Class<Value>): string[] {
   const proto: Object = Object.getPrototypeOf(new cls)
   return Object.getOwnPropertyNames(proto).filter(isField)
}

// Guess this would be populated at compile-time or by a type provider. Is there a reflective way to access the classes of a module?
const datatypeFor_: Class<Value>[] = 
   [Cons,
    Empty,
    Nil],
   datatypeFor: Map<string, Datatype> = new Map(
      datatypeFor_.map((cls): [string, Datatype] => [funName(__nonNull(cls)), { cls, fields: fields(cls) }])
   )

export function eval_ (ρ: Env, e: Expr): ExplVal {
   return e.match({
      Constr(ctr, args): ExplVal {
         const d: Datatype = __nonNull(datatypeFor.get(ctr)),
               state: Stateʹ = {}
         let e̅: List<Expr> = args
         for (const f of d.fields) {
            e̅.match({
               Nil(): void {
                  absurd()
               },
               Cons(e: Expr, e̅ʹ: List<Expr>): void {
                  state[f] = eval_(ρ, e)
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
