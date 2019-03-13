import { __nonNull } from "./util/Core"
import { Persistent, PersistentObject, at, make } from "./util/Persistent"
import { Annotated, Annotation } from "./Annotated"
import { List, Pair } from "./BaseTypes"
import { Env } from "./Env"
import { FiniteMap } from "./FiniteMap"
import { Expr, Kont, Lex } from "./Expr"
import { ExplId, ValId } from "./Eval"
import { UnaryOp } from "./Primitive"

import Trie = Expr.Trie

export type Expr = Expr.Expr
export type Value = Value.Value

export namespace Value {
   export abstract class Value extends Annotated implements PersistentObject {
      __tag: "Value.Value"
      abstract constructor_ (...args: Persistent[]): void
   }

   export class Closure extends Value {
      ρ: Env // ρ is _not_ closing for σ; need to extend with the bindings in δ
      δ: List<Expr.RecDef>
      σ: Trie<Expr>
   
      constructor_ (α: Annotation, ρ: Env, δ: List<Expr.RecDef>, σ: Trie<Expr>): void {
         this.α = α
         this.ρ = ρ
         this.δ = δ
         this.σ = σ
      }

      static at (k: ValId, α: Annotation, ρ: Env, δ: List<Expr.RecDef>, σ: Trie<Expr>): Closure {
         return at(k, Closure, α, ρ, δ, σ)
      }
   }

   export abstract class Prim extends Value {
      __subsubtag: "Value.Prim"
   }
   
   export class ConstInt extends Prim {
      val: number

      constructor_ (α: Annotation, val: number): void {
         this.α = α
         this.val = val
      }
   
      static at (k: ValId, α: Annotation, val: number): ConstInt {
         return at(k, ConstInt, α, val)
      }

      toString (): string {
         return `${this.val}`
      }
   }
   
   export class ConstStr extends Prim {
      val: string

      constructor_ (α: Annotation, val: string): void {
         this.α = α
         this.val = val
      }
   
      static at (k: ValId, α: Annotation, val: string): ConstStr {
         return at(k, ConstStr, α, val)
      }

      toString (): string {
         return `"${this.val}"`
      }
   }
   
   export class Constr extends Value {
      ctr: Lex.Ctr
      args: List<ExplVal>

      constructor_ (α: Annotation, ctr: Lex.Ctr, args: List<ExplVal>): void {
         this.α = α
         this.ctr = ctr
         this.args = args
      }
   
      static at (k: ValId, α: Annotation, ctr: Lex.Ctr, args: List<ExplVal>): Constr {
         return at(k, Constr, α, ctr, args)
      }
   }

   export class PrimOp extends Value {
      op: UnaryOp

      constructor_ (α: Annotation, op: UnaryOp): void {
         this.α = α
         this.op = op
      }
   
      static at (k: ValId, α: Annotation, op: UnaryOp): PrimOp {
         return at(k, PrimOp, α, op)
      }
   }
}

export class ExplVal implements PersistentObject, Kont<ExplVal> {
   __tag: "ExplVal"
   ρ: Env // needed for uneval
   t: Expl
   v: Value

   constructor_ (
      ρ: Env,
      t: Expl,
      v: Value
   ) {
      this.ρ = ρ
      this.t = t
      this.v = v
   }

   static make (ρ: Env, t: Expl, v: Value): ExplVal {
      return make(ExplVal, ρ, t, v)
   }
}

export type Match<K> = Match.Match<K>

// A trie which has been matched to a depth of at least one.
export namespace Match {
   export type Args<K extends Kont<K>> = Args.Args<K>

   export namespace Args {
      export abstract class Args<K extends Kont<K>> implements Kont<Args<K>> {
         __tag: "Match.Args"
         abstract κ: K
         abstract setκ<Kʹ extends Kont<Kʹ>> (κ: Kʹ): Args<Kʹ> // _not_ setting a property!
         abstract constructor_ (...args: Persistent[]): void
      }

      export class End<K extends Kont<K>> extends Args<K> {
         κ: K

         constructor_ (κ: K) {
            this.κ = κ
         }

         setκ<Kʹ extends Kont<Kʹ>> (κ: Kʹ): End<Kʹ> {
            return End.make(κ)
         }

         static is<K extends Kont<K>> (Ψ: Args<K>): Ψ is End<K> {
            return Ψ instanceof End
         }

         static make<K extends Kont<K>> (κ: K): End<K> {
            return make(End, κ) as End<K>
         }
      }

      export class Next<K extends Kont<K>> extends Args<K> {
         tξ: ExplMatch<K>

         constructor_ (tξ: ExplMatch<K>) {
            this.tξ = tξ
         }

         get κ (): K {
            return this.tξ.κ
         }

         setκ<Kʹ extends Kont<Kʹ>> (κ: Kʹ): Next<Kʹ> {
            return Next.make(this.tξ.setκ(κ))
         }

         static is<K extends Kont<K>> (Ψ: Args<K>): Ψ is Next<K> {
            return Ψ instanceof Next
         }

         static make<K extends Kont<K>> (tξ: ExplMatch<K>): Next<K> {
            return make(Next, tξ) as Next<K>
         }
      }
   }

   export abstract class Match<K> implements PersistentObject {
      __tag: "Match.Match"
      abstract κ: K
      abstract setκ<Kʹ extends Kont<Kʹ>> (κ: Kʹ): Match<Kʹ> // _not_ setting a property!
      abstract constructor_ (...args: Persistent[]): void
   }

   // Exactly one branch will be live (i.e. an instanceof Match.Args rather than Trie.Args).
   export class Constr<K extends Kont<K>> extends Match<K> {
      cases: FiniteMap<string, Expr.Args<K> | Args<K>> 

      constructor_ (cases: FiniteMap<string, Expr.Args<K> | Args<K>>) {
         this.cases = cases
      }

      get κ (): K {
         let κ: K // TypeScript flow analysis confused by K | null
         this.cases.map(({snd: args}): null => {
            if (args instanceof Args.Args) {
               κ = args.κ
            }
            return null
         })
         return __nonNull(κ!) // workaround
      }

      // This is borked: TypeScript allows Args<K> to convert to Args<Kʹ> even though Kʹ and K are unrelated.
      // Ironically I do actually want K to be a subtype of Kʹ. Maybe need to reinstate mapMatch :-o
      setκ<Kʹ extends Kont<Kʹ>> (κ: Kʹ): Constr<Kʹ> {
         return Constr.make(
            this.cases.map(({fst: ctr, snd: args}): Pair<string, Expr.Args<Kʹ> | Args<Kʹ>> => {
               if (args instanceof Args.Args) {
                  return Pair.make(ctr, args.setκ(κ))
               }
               return Pair.make(ctr, args) // broken but safe coercion?
            })
         )
      }

      static is<K extends Kont<K>> (ξ: Match<K>): ξ is Constr<K> {
         return ξ instanceof Constr
      }

      static make<K extends Kont<K>> (cases: FiniteMap<string, Expr.Args<K> | Args<K>>): Constr<K> {
         return make(Constr, cases) as Constr<K>
      }
   }

   export class Var<K extends Persistent> extends Match<K> {
      x: Lex.Var
      κ: K

      constructor_ (x: Lex.Var, κ: K) {
         this.x = x
         this.κ = κ
      }

      setκ<Kʹ extends Kont<Kʹ>> (κ: Kʹ): Var<Kʹ> {
         return Var.make(this.x, κ)
      }

      static is<K extends Persistent> (ξ: Match<K>): ξ is Var<K> {
         return ξ instanceof Var
      }

      static make<K extends Persistent> (x: Lex.Var, κ: K): Var<K> {
         return make(Var, x, κ) as Var<K>
      }
   }
}

export class ExplMatch<K extends Kont<K>> implements PersistentObject {
   t: Expl̊ // null iff ξ represents a dead branch
   ξ: Match.Match<K>

   constructor_ (t: Expl̊, ξ: Match<K>) {
      this.t = t
      this.ξ = ξ
   }

   get κ (): K {
      return this.ξ.κ
   }

   setκ<Kʹ extends Kont<Kʹ>> (κ: Kʹ): ExplMatch<Kʹ> {
      return ExplMatch.make(this.t, this.ξ.setκ(κ))
   }

   static make<K extends Kont<K>> (t: Expl̊, ξ: Match<K>): ExplMatch<K> {
      return make(ExplMatch, t, ξ) as ExplMatch<K>
   }
}

export type Expl = ExplVal.Expl
export type Expl̊ = Expl | null

export namespace ExplVal {

   export abstract class Expl implements PersistentObject {
      __tag: "Expl.Expl"
      abstract constructor_ (...args: Persistent[]): void
   }

   export class App extends Expl {
      func: ExplVal
      arg: ExplVal
      ρ_defs: Env             // from closeDefs, for uneval
      ρ_match: Env            // from matching argument, for uneval
      body: Match<ExplVal>    // technically Expl would suffice, but for uneval we want environment

      constructor_ (func: ExplVal, arg: ExplVal, ρ_defs: Env, ρ_match: Env, body: Match<ExplVal>): void {
         this.func = func
         this.arg = arg
         this.ρ_defs = ρ_defs
         this.ρ_match = ρ_match
         this.body = body
      }

      static at (k: ExplId, func: ExplVal, arg: ExplVal, ρ_defs: Env, ρ_match: Env, body: Match<ExplVal>): App {
         return at(k, App, func, arg, ρ_defs, ρ_match, body)
      }
   }

   export class UnaryApp extends Expl {
      func: ExplVal
      arg: ExplVal

      constructor_ (func: ExplVal, arg: ExplVal): void {
         this.func = func
         this.arg = arg
      }

      static at (k: ExplId, func: ExplVal, arg: ExplVal): App {
         return at(k, App, func, arg)
      }
   }

   export class Empty extends Expl {
      constructor_ (): void {
      }

      static at (k: ExplId): Empty {
         return at(k, Empty)
      }
   }

   export class Let extends Expl {
      tu: ExplVal
      ξ: Match.Var<ExplVal> // technically Expl would suffice, but for uneval we want environment

      constructor_ (tu: ExplVal, ξ: Match.Var<ExplVal>): void {
         this.tu = tu
         this.ξ = ξ
      }

      static at (k: ExplId, tu: ExplVal, ξ: Match.Var<ExplVal>): Let {
         return at(k, Let, tu, ξ)
      }
   }

   export class LetRec extends Expl {
      δ: List<Expr.RecDef>
      ρ_defs: Env      // from closeDefs, for uneval
      tv: ExplVal
   
      constructor_ (δ: List<Expr.RecDef>, ρ_defs: Env, tv: ExplVal): void {
         this.δ = δ
         this.ρ_defs = ρ_defs
         this.tv = tv
      }

      static at (k: ExplId, δ: List<Expr.RecDef>, ρ_defs: Env, tv: ExplVal): LetRec {
         return at(k, LetRec, δ, ρ_defs, tv)
      }
   }
   
   export class MatchAs extends Expl {
      tu: ExplVal
      ρ_match: Env        // from matching argument, for uneval
      ξ: Match<ExplVal>   // technically Expl would suffice, but for uneval we want environment

      constructor_ (tu: ExplVal, ρ_match: Env, ξ: Match<ExplVal>): void {
         this.tu = tu
         this.ρ_match = ρ_match
         this.ξ = ξ
      }

      static at (k: ExplId, tu: ExplVal, ρ_match: Env, ξ: Match<ExplVal>): MatchAs {
         return at(k, MatchAs, tu, ρ_match, ξ)
      }
   }

   export class BinaryApp extends Expl {
      tv1: ExplVal
      opName: Lex.OpName
      tv2: ExplVal

      constructor_ (tv1: ExplVal, opName: Lex.OpName, tv2: ExplVal): void {
         this.tv1 = tv1
         this.opName = opName
         this.tv2 = tv2
      }

      static at (k: ExplId, tv1: ExplVal, opName: Lex.OpName, tv2: ExplVal): BinaryApp {
         return at(k, BinaryApp, tv1, opName, tv2)
      }
   }

   export class Var extends Expl {
      x: Lex.Var

      constructor_ (x: Lex.Var): void {
         this.x = x
      }

      static at (k: ExplId, x: Lex.Var): Var {
         return at(k, Var, x)
      }
   }
}
