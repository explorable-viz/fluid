import { absurd, as } from "./util/Core"
import { List, Pair, pair } from "./BaseTypes2"
import { Env } from "./Env2"
import { Expr } from "./Expr2"
import { Id, Str, Value, _, make } from "./Value2"
import { Versioned, joinα, setα, strʹ } from "./Versioned2"

import Args = Expr.Args
import Def = Expr.Def
import Kont = Expr.Kont
import RecDef = Expr.RecDef
import Trie = Expr.Trie

// The "runtime identity" of an expression. In the formalism we use a "flat" representation so that e always has an external id;
// here it is more convenient to use an isomorphic nested format.
export class ExprId extends Id {
   j: List<Value> = _
   e: Expr | Versioned<Str> = _ // str for binding occurrences of variables
}

export function exprId (j: List<Value>, e: Expr | Versioned<Str>): ExprId {
   return make(ExprId, j, e)
}

// F-bounded polymorphism doesn't work well here. I've used it for the smaller helper functions 
// (but with horrendous casts), but not for the two main top-level functions.
export function instantiate<T extends Expr> (ρ: Env, e: T): Expr {
   const k: ExprId = exprId(ρ.entries(), e)
   if (e instanceof Expr.ConstNum) {
      return Expr.constNum(k, e.val)
   } else
   if (e instanceof Expr.ConstStr) {
      return Expr.constStr(k, e.val)
   } else
   if (e instanceof Expr.Constr) {
      return Expr.constr(k, e.ctr, e.args.map(e => instantiate(ρ, e)))
   } else
   if (e instanceof Expr.Fun) {
      return Expr.fun(k, instantiateTrie(ρ, e.σ))
   } else
   if (e instanceof Expr.Var) {
      return Expr.var_(k, e.x)
   } else
   if (e instanceof Expr.Defs) {
      return Expr.defs(k, e.def̅.map(def => instantiateDef(ρ, def)), instantiate(ρ, e.e))
   } else
   if (e instanceof Expr.MatchAs) {
      return Expr.matchAs(k, instantiate(ρ, e.e), instantiateTrie(ρ, e.σ))
   } else
   if (e instanceof Expr.App) {
      return Expr.app(k, instantiate(ρ, e.f), instantiate(ρ, e.e))
   } else
   if (e instanceof Expr.BinaryApp) {
      return Expr.binaryApp(k, instantiate(ρ, e.e1), e.opName, instantiate(ρ, e.e2))
   } else {
      return absurd()
   }
}

enum Direction { Fwd, Bwd }

export function instantiate_fwd (e: Expr): void {
   return instantiate_(Direction.Fwd, e)
}

export function instantiate_bwd (e: Expr): void {
   return instantiate_(Direction.Bwd, e)
}

function instantiate_ (dir: Direction, e: Expr): void {
   const eʹ: Expr = as((e.__id as ExprId).e, Expr.Expr)
   if (dir === Direction.Fwd) {
      setα(eʹ.__α, e)
   } else {
      joinα(e.__α, eʹ)
   }
   if (e instanceof Expr.ConstNum || e instanceof Expr.ConstStr || e instanceof Expr.Var) {
      // nothing else to do
   } else
   if (e instanceof Expr.Constr) {
      e.args.toArray().map(e => instantiate_(dir,e))
   } else
   if (e instanceof Expr.Fun) {
      instantiateTrie_(dir, e.σ)
   } else
   if (e instanceof Expr.Defs) {
      e.def̅.toArray().map(def => instantiateDef_(dir, def))
      instantiate_(dir, e.e)
   } else
   if (e instanceof Expr.MatchAs) {
      instantiate_(dir, e.e)
      instantiateTrie_(dir, e.σ)
   } else
   if (e instanceof Expr.App) {
      instantiate_(dir, e.f)
      instantiate_(dir, e.e)
   } else
   if (e instanceof Expr.BinaryApp) {
      instantiate_(dir, e.e1)
      instantiate_(dir, e.e2)
   } else {
      absurd()
   }
}

function instantiateVar (ρ: Env, x: Versioned<Str>): Versioned<Str> {
   const k: ExprId = exprId(ρ.entries(), x)
   return strʹ(k, x.val)
}

function instantiateVar_ (dir: Direction, x: Versioned<Str>): void {
   const xʹ: Versioned<Str> = (x.__id as ExprId).e as Versioned<Str>
   if (dir === Direction.Fwd) {
      setα(xʹ.__α, x)
   } else {
      joinα(x.__α, xʹ)
   }
}

function instantiateDef (ρ: Env, def: Def): Def {
   if (def instanceof Expr.Let) {
      return Expr.let_(instantiateVar(ρ, def.x), instantiate(ρ, def.e))
   } else
   if (def instanceof Expr.Prim) {
      return Expr.prim(instantiateVar(ρ, def.x))
   } else
   if (def instanceof Expr.LetRec) {
      const δ: List<RecDef> = def.δ.map((def: RecDef) => {
         return Expr.recDef(instantiateVar(ρ, def.x), instantiateTrie(ρ, def.σ))
      })
      return Expr.letRec(δ)
   } else {
      return absurd()
   }
}

function instantiateDef_ (dir: Direction, def: Def): void {
   if (def instanceof Expr.Let) {
      instantiateVar_(dir, def.x)
      instantiate_(dir, def.e)
   } else 
   if (def instanceof Expr.Prim) {
      instantiateVar_(dir, def.x)
   } else
   if (def instanceof Expr.LetRec) {
      def.δ.toArray().map(def => {
         instantiateVar_(dir, def.x)
         instantiateTrie_(dir, def.σ)
      })
   } else {
      absurd()
   }
}

function instantiateTrie<K extends Kont<K>, T extends Trie<K>> (ρ: Env, σ: T): T {
   if (Trie.Var.is(σ)) {
      return Trie.var_(σ.x, instantiateKont(ρ, σ.κ) as K) as Trie<K> as T
   } else
   if (Trie.Constr.is(σ)) {
      return Trie.constr<K>(σ.cases.map(
         ({ fst: c, snd: Π }: Pair<Str, Args<K>>): Pair<Str, Args<K>> => {
            return pair(c, instantiateArgs(ρ, Π))
         })
      ) as Trie<K> as T
   } else {
      return absurd()
   }
}

function instantiateTrie_<K extends Kont<K>, T extends Trie<K>> (dir: Direction, σ: T): void {
   if (Trie.Var.is(σ)) {
      instantiateKont_(dir, σ.κ)
   } else
   if (Trie.Constr.is(σ)) {
      σ.cases.toArray().map(
         ({ fst: c, snd: Π }: Pair<Str, Args<K>>): void => instantiateArgs_(dir, Π)
      )
   } else {
      absurd()
   }
}

// See issue #33.
function instantiateKont<K extends Kont<K>> (ρ: Env, κ: K): K {
   if (κ instanceof Trie.Trie) {
      return instantiateTrie<K, Trie<K>>(ρ, κ) as K 
   } else
   if (κ instanceof Expr.Expr) {
      return instantiate(ρ, κ) as Kont<K> as K
   } else
   if (κ instanceof Args.Args) {
      return instantiateArgs(ρ, κ) as K
   } else {
      return absurd()
   }
}

function instantiateKont_<K extends Kont<K>> (dir: Direction, κ: K): void {
   if (κ instanceof Trie.Trie) {
      instantiateTrie_<K, Trie<K>>(dir, κ)
   } else
   if (κ instanceof Expr.Expr) {
      instantiate_(dir, κ)
   } else
   if (κ instanceof Args.Args) {
      instantiateArgs_(dir, κ)
   } else {
      absurd()
   }
}

function instantiateArgs<K extends Kont<K>> (ρ: Env, Π: Args<K>): Args<K> {
   if (Args.End.is(Π)) {
      return Args.end(instantiateKont(ρ, Π.κ))
   } else
   if (Args.Next.is(Π)) {
      return Args.next(instantiateTrie(ρ, Π.σ))
   } else {
      return absurd()
   }
}

function instantiateArgs_<K extends Kont<K>> (dir: Direction, Π: Args<K>): void {
   if (Args.End.is(Π)) {
      instantiateKont_(dir, Π.κ)
   } else
   if (Args.Next.is(Π)) {
      instantiateTrie_(dir, Π.σ)
   } else {
      absurd()
   }
}
