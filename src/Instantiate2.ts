import { Annotation, ann } from "./util/Annotated2"
import { absurd, as } from "./util/Core"
import { List, Pair, pair } from "./BaseTypes2"
import { Env } from "./Env2"
import { Expr } from "./Expr2"
import { Id, Str, Value, _, make } from "./Value2"
import { Versioned, setα, strʹ } from "./Versioned2"

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
      return setα(e.__α, Expr.constNum(k, e.val))
   } else
   if (e instanceof Expr.ConstStr) {
      return setα(e.__α, Expr.constStr(k, e.val))
   } else
   if (e instanceof Expr.Constr) {
      return setα(e.__α, Expr.constr(k, e.ctr, e.args.map(e => instantiate(ρ, e))))
   } else
   if (e instanceof Expr.Fun) {
      return setα(e.__α, Expr.fun(k, instantiateTrie(ρ, e.σ)))
   } else
   if (e instanceof Expr.Var) {
      return setα(e.__α, Expr.var_(k, e.x))
   } else
   if (e instanceof Expr.Defs) {
      return setα(e.__α, Expr.defs(k, e.def̅.map(def => instantiateDef(ρ, def)), instantiate(ρ, e.e)))
   } else
   if (e instanceof Expr.MatchAs) {
      return setα(e.__α, Expr.matchAs(k, instantiate(ρ, e.e), instantiateTrie(ρ, e.σ)))
   } else
   if (e instanceof Expr.App) {
      return setα(e.__α, Expr.app(k, instantiate(ρ, e.f), instantiate(ρ, e.e)))
   } else
   if (e instanceof Expr.BinaryApp) {
      return setα(e.__α, Expr.binaryApp(k, instantiate(ρ, e.e1), e.opName, instantiate(ρ, e.e2)))
   } else {
      return absurd()
   }
}

export function uninstantiate (e: Expr): void {
   const eʹ: Expr = as((e.__id as ExprId).e, Expr.Expr),
         α: Annotation = ann.join(eʹ.__α, e.__α) // merge annotations into source
   if (e instanceof Expr.ConstNum) {
      setα(α, eʹ)
   } else
   if (e instanceof Expr.ConstStr) {
      setα(α, eʹ)
   } else
   if (e instanceof Expr.Constr) {
      e.args.toArray().map(e => uninstantiate(e))
      setα(α, eʹ)
   } else
   if (e instanceof Expr.Fun) {
      uninstantiateTrie(e.σ)
      setα(α, eʹ)
   } else
   if (e instanceof Expr.Var) {
      setα(α, eʹ)
   } else
   if (e instanceof Expr.Defs) {
      e.def̅.toArray().map(uninstantiateDef)
      uninstantiate(e.e)
      setα(α, eʹ)
   } else
   if (e instanceof Expr.MatchAs) {
      uninstantiate(e.e)
      uninstantiateTrie(e.σ)
      setα(α, eʹ)
   } else
   if (e instanceof Expr.App) {
      uninstantiate(e.f)
      uninstantiate(e.e)
      setα(α, eʹ)
   } else
   if (e instanceof Expr.BinaryApp) {
      uninstantiate(e.e1)
      uninstantiate(e.e2)
      setα(α, eʹ)
   } else {
      absurd()
   }
}

function instantiateVar (ρ: Env, x: Versioned<Str>): Versioned<Str> {
   const k: ExprId = exprId(ρ.entries(), x)
   return setα(x.__α, strʹ(k, x.val))
}

function uninstantiateVar (x: Versioned<Str>): void {
   const xʹ: Versioned<Str> = (x.__id as ExprId).e as Versioned<Str>
   setα(ann.join(xʹ.__α, x.__α), xʹ)
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

function uninstantiateDef (def: Def): void {
   if (def instanceof Expr.Let) {
      uninstantiateVar(def.x)
      uninstantiate(def.e)
   } else 
   if (def instanceof Expr.Prim) {
      uninstantiateVar(def.x)
   } else
   if (def instanceof Expr.LetRec) {
      def.δ.toArray().map(def => {
         uninstantiateVar(def.x)
         uninstantiateTrie(def.σ)
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

function uninstantiateTrie<K extends Kont<K>, T extends Trie<K>> (σ: T): void {
   if (Trie.Var.is(σ)) {
      uninstantiateKont(σ.κ)
   } else
   if (Trie.Constr.is(σ)) {
      σ.cases.toArray().map(
         ({ fst: c, snd: Π }: Pair<Str, Args<K>>): void => uninstantiateArgs(Π)
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

function uninstantiateKont<K extends Kont<K>> (κ: K): void {
   if (κ instanceof Trie.Trie) {
      uninstantiateTrie<K, Trie<K>>(κ)
   } else
   if (κ instanceof Expr.Expr) {
      uninstantiate(κ)
   } else
   if (κ instanceof Args.Args) {
      uninstantiateArgs(κ)
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

function uninstantiateArgs<K extends Kont<K>> (Π: Args<K>): void {
   if (Args.End.is(Π)) {
      uninstantiateKont(Π.κ)
   } else
   if (Args.Next.is(Π)) {
      uninstantiateTrie(Π.σ)
   } else {
      absurd()
   }
}
