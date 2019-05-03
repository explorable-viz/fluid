import { List } from "./BaseTypes2"
import { Constr as Constrʹ, Func, make } from "./ExplVal2"
import { FiniteMap } from "./FiniteMap2"

// use to initialise fields for reflection, without requiring constructors
const _: any = undefined 

export namespace Expr {
   export abstract class Expr extends Constrʹ<Expr> {
      abstract __match<U> (σ: ExprFunc<U>): U
   }

   export abstract class ExprFunc<U> extends Func<U> {
      abstract Var (x: string): U
      abstract Constr (ctr: string, args: List<Expr>): U
      abstract Fun (σ: Trie<Expr>): U
      abstract MatchAs (e: Expr, σ: Trie<Expr>): U
   }

   export class Var extends Expr {
      x: string

      __match <U> (σ: ExprFunc<U>): U {
         return σ.Var(this.x)
      }
   }

   export class Constr extends Expr {
      ctr: string = _
      args: List<Expr> = _

      __match<U> (σ: ExprFunc<U>): U {
         return σ.Constr(this.ctr, this.args)
      }
   }

   export function constr (ctr: string, args: List<Expr>): Constr {
      return make(Constr, { ctr, args })
   }

   export class Fun extends Expr {
      σ: Trie<Expr> = _

      __match<U> (σ: ExprFunc<U>): U {
         return σ.Fun(this.σ)
      }
   }

   export class MatchAs extends Expr {
      e: Expr = _
      σ: Trie<Expr> = _

      __match<U> (σ: ExprFunc<U>): U {
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

   export namespace Trie {
      export abstract class Trie<K extends Kont<K>> extends Constrʹ<Trie<K>> implements Kont<Trie<K>> {
         abstract __match<U> (σ: TrieFunc<K, U>): U
      }

      export abstract class TrieFunc<K, U> extends Func<U> {
         abstract Constr (cases: FiniteMap<string, Args<K>>): U
         abstract Var (x: string, κ: K): U
      }
      
      export class Constr<K extends Kont<K>> extends Trie<K> {
         cases: FiniteMap<string, Args<K>> = _

         __match<U> (σ: TrieFunc<K, U>): U {
            return σ.Constr(this.cases)
         }
      }

      export class Var<K extends Kont<K>> extends Trie<K> {
         x: string = _
         κ: K = _

         __match<U> (σ: TrieFunc<K, U>): U {
            return σ.Var(this.x, this.κ)
         }
      }
   }
}

export type Expr = Expr.Expr
