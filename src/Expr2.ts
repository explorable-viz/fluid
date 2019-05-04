import { List } from "./BaseTypes2"
import { Constr as Constrʹ, make } from "./ExplVal2"
import { FiniteMap } from "./FiniteMap2"

// use to initialise fields for reflection, without requiring constructors
const _: any = undefined 

export namespace Expr {
   export abstract class Expr extends Constrʹ<Expr> {
   }

   export class App extends Expr {
      func: Expr = _
      arg: Expr = _
   }

   export class ConstNum extends Expr {
      val: number
   }
   
   export class ConstStr extends Expr {
      val: string
   }

   export class Var extends Expr {
      x: string = _
   }

   export class Constr extends Expr {
      ctr: string = _
      args: List<Expr> = _
   }

   export function constr (ctr: string, args: List<Expr>): Constr {
      return make(Constr, { ctr, args })
   }

   export class Fun extends Expr {
      σ: Trie<Expr> = _
   }

   // A let is simply a match where the trie is a variable trie.
   export class Let extends Expr {
      e: Expr = _
      σ: Trie.Var<Expr> = _
   }

   export class MatchAs extends Expr {
      e: Expr = _
      σ: Trie<Expr> = _
   }

   export type Trie<K extends Kont<K>> = Trie.Trie<K>

   export interface Kont<K> {
   }

   export type Args<K extends Kont<K>> = Args.Args<K>

   export namespace Args {
      export abstract class Args<K extends Kont<K>> extends Constrʹ<Args<K>> implements Kont<Args<K>> {
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
      }

      export class Constr<K extends Kont<K>> extends Trie<K> {
         cases: FiniteMap<string, Args<K>> = _
      }

      export class Var<K extends Kont<K>> extends Trie<K> {
         x: string = _
         κ: K = _
      }
   }
}

export type Expr = Expr.Expr
