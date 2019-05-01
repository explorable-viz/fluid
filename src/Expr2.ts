import { List } from "./BaseTypes2"
import { Explainable, make } from "./ExplVal2"
import { FiniteMap } from "./FiniteMap2"

export namespace Expr {
   export abstract class Expr extends Explainable<Expr> {
      abstract __match<U> (σ: ExprFun<U>): U
   }

   interface ExprFun<U> {
      Constr (ctr: string, args: List<Expr>): U
      Fun (σ: Trie<Expr>): U
      MatchAs (e: Expr, σ: Trie<Expr>): U
   }

   export class Constr extends Expr {
      ctr: string
      args: List<Expr>

      constructor (ctr: string, args: List<Expr>) {
         super()
         this.ctr = ctr
         this.args = args
      }

      __match<U> (σ: ExprFun<U>): U {
         return σ.Constr(this.ctr, this.args)
      }
   }

   export function constr (ctr: string, args: List<Expr>): Constr {
      return make(Constr, { ctr, args })
   }

   export class Fun extends Expr {
      σ: Trie<Expr>

      __match<U> (σ: ExprFun<U>): U {
         return σ.Fun(this.σ)
      }
   }

   export class MatchAs extends Expr {
      e: Expr
      σ: Trie<Expr>

      __match<U> (σ: ExprFun<U>): U {
         return σ.MatchAs(this.e, this.σ)
      }
   }

   export type Trie<K extends Kont<K>> = Trie.Trie<K>

   export interface Kont<K> {
   }

   export type Args<K extends Kont<K>> = Args.Args<K>

   export namespace Args {
      export abstract class Args<K extends Kont<K>> implements Kont<Args<K>> {
      }

      export class End<K extends Kont<K>> extends Args<K> {
         κ: K
      }

      export class Next<K extends Kont<K>> extends Args<K> {
         σ: Trie<Args<K>>
      }
   }

   namespace Trie {
      export abstract class Trie<K extends Kont<K>> extends Explainable<Trie<K>> implements Kont<Trie<K>> {
      }

      interface TrieFun<K, U> {
         Constr (cases: FiniteMap<string, Args<K>>): U
         Var (x: string, κ: K): U
      }
      
      export class Constr<K extends Kont<K>> extends Trie<K> {
         cases: FiniteMap<string, Args<K>>

         __match<U> (σ: TrieFun<K, U>): U {
            return σ.Constr(this.cases)
         }
      }

      export class Var<K extends Kont<K>> extends Trie<K> {
         x: string
         κ: K

         __match<U> (σ: TrieFun<K, U>): U {
            return σ.Var(this.x, this.κ)
         }
      }
   }
}

export type Expr = Expr.Expr
