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
   e: Expr | Versioned<Str> = _ // str case is for binding occurrences of variables
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
      return setα(e.__α, Expr.app(k, instantiate(ρ, e.func), instantiate(ρ, e.arg)))
   } else
   if (e instanceof Expr.BinaryApp) {
      return setα(e.__α, Expr.binaryApp(k, instantiate(ρ, e.e1), e.opName, instantiate(ρ, e.e2)))
   } else {
      return absurd()
   }
}

// It's enough just to return original expression; reconstructing environment would require rethinking. 
export function uninstantiate (e: Expr): Expr {
   const eʹ: Expr = as((e.__id as ExprId).e, Expr.Expr),
         k: Id = eʹ.__id,
         α: Annotation = ann.join(eʹ.__α, e.__α) // merge annotations into source
   if (e instanceof Expr.ConstNum) {
      return setα(α, Expr.constNum(k, e.val))
   } else
   if (e instanceof Expr.ConstStr) {
      return setα(α, Expr.constStr(k, e.val))
   } else
   if (e instanceof Expr.Constr) {
      return setα(α, Expr.constr(k, e.ctr, e.args.map(e => uninstantiate(e))))
   } else
   if (e instanceof Expr.Fun) {
      return setα(α, Expr.fun(k, uninstantiateTrie(e.σ)))
   } else
   if (e instanceof Expr.Var) {
      return setα(α, Expr.var_(k, e.x))
   } else
   if (e instanceof Expr.Defs) {
      return setα(α, Expr.defs(k, e.def̅.map(uninstantiateDef), uninstantiate(e.e)))
   } else
   if (e instanceof Expr.MatchAs) {
      return setα(α, Expr.matchAs(k, uninstantiate(e.e), uninstantiateTrie(e.σ)))
   } else
   if (e instanceof Expr.App) {
      return setα(α, Expr.app(k, uninstantiate(e.func), uninstantiate(e.arg)))
   } else
   if (e instanceof Expr.BinaryApp) {
      return setα(α, Expr.binaryApp(k, uninstantiate(e.e1), e.opName, uninstantiate(e.e2)))
   } else {
      return absurd()
   }
}

function instantiateVar (ρ: Env, x: Versioned<Str>): Versioned<Str> {
   const k: ExprId = exprId(ρ.entries(), x)
   return setα(x.__α, strʹ(k, x.val))
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

function uninstantiateVar (x: Versioned<Str>): Versioned<Str> {
   const xʹ: Versioned<Str> = (x.__id as ExprId).e as Versioned<Str>
   return setα(ann.join(xʹ.__α, x.__α), xʹ)
}

function uninstantiateDef (def: Def): Def {
   if (def instanceof Expr.Let) {
      return Expr.let_(uninstantiateVar(def.x), uninstantiate(def.e))
   } else 
   if (def instanceof Expr.Prim) {
      return Expr.prim(uninstantiateVar(def.x))
   }
   if (def instanceof Expr.LetRec) {
      const δ: List<RecDef> = def.δ.map(def => {
         return Expr.recDef(uninstantiateVar(def.x), uninstantiateTrie(def.σ))
      })
      return Expr.letRec(δ)
   } else {
      return absurd()
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

function uninstantiateTrie<K extends Kont<K>, T extends Trie<K>> (σ: T): T {
   if (Trie.Var.is(σ)) {
      return Trie.var_(σ.x, uninstantiateKont(σ.κ)) as Trie<K> as T
   } else
   if (Trie.Constr.is(σ)) {
      return Trie.constr(σ.cases.map(
         ({ fst: c, snd: Π }: Pair<Str, Args<K>>): Pair<Str, Args<K>> => {
            return pair(c, uninstantiateArgs(Π))
         })
      ) as Trie<K> as T
   } else {
      return absurd()
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

function uninstantiateKont<K extends Kont<K>> (κ: K): K {
   if (κ instanceof Trie.Trie) {
      return uninstantiateTrie<K, Trie<K>>(κ) as K
   } else
   if (κ instanceof Expr.Expr) {
      return uninstantiate(κ) as any as K
   } else
   if (κ instanceof Args.Args) {
      return uninstantiateArgs(κ) as K
   } else {
      return absurd()
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

function uninstantiateArgs<K extends Kont<K>> (Π: Args<K>): Args<K> {
   if (Args.End.is(Π)) {
      return Args.end(uninstantiateKont(Π.κ))
   } else
   if (Args.Next.is(Π)) {
      return Args.next(uninstantiateTrie(Π.σ))
   } else {
      return absurd()
   }
}
