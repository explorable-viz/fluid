import { List, Int, Prim, Str, Tree } from "./BaseTypes"
import { FiniteMap, unionWith } from "./FiniteMap"
import { __def, key, keyP } from "./Memo"
import { partiallyApply } from "./Primitive"
import { eq } from "./Ord"
import { ITraced, __val, unaryOps, binaryOps, create, projections, typeCheck_ } from "./Runtime"
import { __nonNull, assert, assertMessage, failWithMessage, typeCheck } from "./Util"

export namespace str {
   // Primitive ops.
   export const concat: string = '++'
   export const div: string = '/'
   export const equal: string = '=='
   export const greaterT: string = '>'
   export const lessT: string = '<'
   export const minus: string = '-'
   export const plus: string = '+'
   export const times: string = '*'

   // Export constants used for parsing, but also for toString() implementations.
   export const as: string = 'as'
   export const match: string = 'match'
   export const matchBodySep: string = '→'
   export const fun: string = 'fun'
   export const in_: string = 'in'
   export const let_: string = 'let'
   export const letInitSep: string = '='
   export const letRec: string = 'letrec'
   export const parenL: string = '('
   export const parenR: string = ')'
}

export type Env = FiniteMap<Str, Object>

// Primitive ops; see 0.4.4 release notes.
export class PrimOp {
   __apply (v: Object): Object {
      return failWithMessage("Would like this to be abstract.")
   }
}

// Assume all dynamic type-checking is performed inside the underlying JS operation, although
// currently there mostly isn't any.
export class UnaryPrimOp extends PrimOp {
   _name: ITraced<Str>

   static at (α: Addr, name: ITraced<Str>): UnaryPrimOp {
      const this_: UnaryPrimOp = create(α, UnaryPrimOp)
      this_._name = typeCheck(name, ITraced)
      this_.__version()
      return this_
   }

   __apply (v: Object): Object {
      return __nonNull(unaryOps.get(this.name.val))(v)
   }

   get name (): Str {
      return this._name.val
   }

   toString (): string {
      return this.name.val
   }
}

export class BinaryPrimOp extends PrimOp {
   _name: ITraced<Str>

   static at (α: Addr, name: ITraced<Str>): BinaryPrimOp {
      const this_: BinaryPrimOp = create(α, BinaryPrimOp)
      this_._name = typeCheck(name, ITraced)
      this_.__version()
      return this_
   }

   __apply (v1: Object): PrimOp {
      return partiallyApply(this, v1)
   }

   get name (): Str {
      return this._name.val
   }

   toString (): string {
      return this.name.val
   }
}

// Binary op that has been applied to a single operand.
export class UnaryPartialPrimOp extends PrimOp {
   _name: ITraced<Str>
   _binOp: ITraced<BinaryPrimOp>
   _v1: ITraced

   static at (α: Addr, name: ITraced<Str>, binOp: ITraced<BinaryPrimOp>, v1: ITraced): UnaryPartialPrimOp {
      const this_: UnaryPartialPrimOp = create(α, UnaryPartialPrimOp)
      this_._name = typeCheck(name, ITraced)
      this_._binOp = typeCheck_(binOp, BinaryPrimOp)
      this_._v1 = v1
      this_.__version()
      return this_
   }

   static at_ (α: Addr, name: Str, binOp: BinaryPrimOp, v1: Object): UnaryPartialPrimOp {
      return UnaryPartialPrimOp.at(
         α,
         __val(keyP(α, 'name'), name),
         __val(keyP(α, 'binOp'), binOp),
         __val(keyP(α, 'v1'), v1)
      )
   }

   __apply (v2: Object): Object {
      return __nonNull(binaryOps.get(this.binOp.name.val))(this.v1, v2)
   }

   get name (): Str {
      return this._name.val
   }

   get binOp (): BinaryPrimOp {
      return this._binOp.val
   }

   get v1 (): Object {
      return this._v1.val
   }

   toString (): string {
      return this.name.val
   }
}

// Syntactically distinguish projection functions from other unary ops, previously because we generated an
// implementation; may no longer be necessary.
export class Proj extends PrimOp {
   _name: ITraced<Str>

   static at (α: Addr, name: ITraced<Str>): Proj {
      const this_: Proj = create(α, Proj)
      this_._name = typeCheck(name, ITraced)
      this_.__version()
      return this_
   }

   __apply (v: Object): Object {
      return __nonNull(projections.get(this.name.val))(v)
   }

   get name (): Str {
      return this._name.val
   }

   toString (): string {
      return this.name.val
   }
}

export class Closure {
   _ρ: ITraced<Env>
   _defs: ITraced<List<RecDefinition>>
   _func: ITraced<Fun>

   static at (α: Addr, ρ: ITraced<Env>, defs: ITraced<List<RecDefinition>>, func: ITraced<Fun>): Closure {
      const this_: Closure = create(α, Closure)
      this_._ρ = typeCheck_(ρ, Tree)
      this_._defs = typeCheck_(defs, List)
      this_._func = typeCheck_(func, Fun)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, ρ: Env, defs: List<RecDefinition>, func: Fun): Closure {
      return Closure.at(
         α,
         __val(keyP(α, 'ρ'), ρ),
         __val(keyP(α, 'defs'), defs),
         __val(keyP(α, 'func'), func)
      )
   }

   get ρ (): Env {
      return this._ρ.val
   }

   get defs (): List<RecDefinition> {
      return this._defs.val
   }

   get func (): Fun {
      return this._func.val
   }
}

// A reified value.
export class Value {
   __visit <a> (v: ValueVisitor<a>): a {
      return assert(false)
   }
}

export interface ValueVisitor<a> {
   is_Constr (v: Constr): a
   is_ConstInt (v: ConstInt): a
   is_ConstStr (v: ConstStr): a
}

export class ConstInt extends Value {
   _val: ITraced<Int>

   static at (α: Addr, val: ITraced<Int>): ConstInt {
      const this_: ConstInt = create(α, ConstInt)
      this_._val = typeCheck_(val, Int)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, val: Int): ConstInt {
      return ConstInt.at(α, __val(keyP(α, 'val'), val))
   }

   __visit <a> (v: ValueVisitor<a>): a {
      return v.is_ConstInt(this)
   }

   get val (): Int {
      return this._val.val
   }
}

export class ConstStr extends Value {
   _val: ITraced<Str>

   static at (α: Addr, val: ITraced<Str>): ConstStr {
      const this_: ConstStr = create(α, ConstStr)
      this_._val = typeCheck_(val, Str)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, val: Str): ConstStr {
      return ConstStr.at(α, __val(keyP(α, 'val'), val))
   }

   __visit <a> (v: ValueVisitor<a>): a {
      return v.is_ConstStr(this)
   }

   get val (): Str {
      return this._val.val
   }
}

// Reifies a value of a datatype, but only "one level down": although the _explanations_ of my children
// are observable, the children themselves remain as native values, rather than also being reified.
export class Constr extends Value {
   _ctr: ITraced<Str>
   _args: ITraced<List<ITraced>>

   static at (α: Addr, ctr: ITraced<Str>, args: ITraced<List<ITraced>>): Constr {
      const this_: Constr = create(α, Constr)
      this_._ctr = typeCheck(ctr, ITraced)
      this_._args = typeCheck_(args, List)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, ctr: Str, args: List<ITraced>): Constr {
      return Constr.at(α, __val(keyP(α, 'ctr'), ctr), __val(keyP(α, 'args'), args))
   }

   __visit <a> (v: ValueVisitor<a>): a {
      return v.is_Constr(this)
   }

   get ctr (): Str {
      return this._ctr.val
   }

   get args (): List<ITraced> {
      return this._args.val
   }
}

export class Trace {
   __visit<T> (v: TraceVisitor<T>): T {
      return assert(false)
   }
}

export interface TraceVisitor<T> {
   is_App (t: App): T
   is_EmptyTrace (t: EmptyTrace): T
   is_Fun (t: Fun): T
   is_Let (t: Let): T
   is_LetRec (t: LetRec): T
   is_MatchAs (t: MatchAs): T
   is_OpName (t: OpName): T
   is_Var (t: Var): T
}

// I don't think this is the same as ⊥; it represents the "end" of an explanation.
export class EmptyTrace extends Trace {
   static at (α: Addr): EmptyTrace {
      const this_: Trace = create(α, EmptyTrace)
      this_.__version()
      return this_
   }

   __visit <T> (v: TraceVisitor<T>): T {
      return v.is_EmptyTrace(this)
   }
}

// The name of a primitive operation, such as * or +, where that name is /not/ a standard identifier.
// Other uses of primitive operations are treated as variables.
export class OpName extends Trace {
   _name: ITraced<Str>

   static at (α: Addr, name: ITraced<Str>): OpName {
      const this_: OpName = create(α, OpName)
      this_._name = typeCheck(name, ITraced)
      this_.__version()
      return this_
   }

   __visit <T> (v: TraceVisitor<T>): T {
      return v.is_OpName(this)
   }

   get name (): Str {
      return this._name.val
   }
}

// A _use_ of a variable, not a defining occurrence.
export class Var extends Trace {
   _name: ITraced<Str>

   static at (α: Addr, name: ITraced<Str>): Var {
      const this_: Var = create(α, Var)
      this_._name = typeCheck(name, ITraced)
      this_.__version()
      return this_
   }

   __visit <T> (v: TraceVisitor<T>): T {
      return v.is_Var(this)
   }

   get name (): Str {
      return this._name.val
   }
}

// Expression form only. TODO: don't I need to unify this now with Closure?
export class Fun extends Trace {
   _σ: ITraced<Trie<ITraced>>

   static at (α: Addr, σ: ITraced<Trie<ITraced>>): Fun {
      const this_: Fun = create(α, Fun)
      this_._σ = typeCheck(σ, ITraced)
      this_.__version()
      return this_
   }

   __visit <T> (v: TraceVisitor<T>): T {
      return v.is_Fun(this)
   }

   get σ (): Trie<ITraced> {
      return this._σ.val
   }
}

// Body of a lambda abstraction or primitive.
export class AppBody {
}

// An application expression has an empty body.
export class EmptyBody extends AppBody {
   static at (α: Addr): EmptyBody {
      return create(α, EmptyBody)
   }
}

// For primitives there is no trace part, but we will still show how the argument is consumed.
// TODO: unify with matches?
export class PrimBody extends AppBody {
   _param: ITraced<Str>

   static at (α: Addr, param: ITraced<Str>): PrimBody {
      const this_: PrimBody = create(α, PrimBody)
      this_._param = typeCheck_(param, Str)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, param: Str): PrimBody {
      return PrimBody.at(α, __val(keyP(α, 'param'), param))
   }

   get param (): Str {
      return this._param.val
   }
}

export class FunBody extends AppBody {
   _σ: ITraced<Trie<ITraced>>

   static at (α: Addr, σ: ITraced<Trie<ITraced>>): FunBody {
      const this_: FunBody = create(α, FunBody)
      this_._σ = typeCheck(σ, ITraced)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, σ: Trie<ITraced>): FunBody {
      return FunBody.at(α, __val(keyP(α, 'σ'), σ))
   }

   get σ (): Trie<ITraced> {
      return this._σ.val
   }
}

export class App extends Trace {
   _func: ITraced<ITraced>
   _arg: ITraced<ITraced>
   _appBody: ITraced<AppBody>

   static at (
      α: Addr,
      func: ITraced<ITraced>,
      arg: ITraced<ITraced>,
      appBody: ITraced<AppBody>
   ): App {
      const this_: App = create(α, App)
      this_._func = typeCheck_(func, ITraced)
      this_._arg = typeCheck_(arg, ITraced)
      this_._appBody = typeCheck_(appBody, AppBody)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, func: ITraced, arg: ITraced, appBody: AppBody): App {
      return App.at(
         α,
         __val(keyP(α, 'func'), func),
         __val(keyP(α, 'arg'), arg),
         __val(keyP(α, 'appBody'), appBody)
      )
   }

   __visit <T> (v: TraceVisitor<T>): T {
      return v.is_App(this)
   }

   get func (): ITraced {
      return this._func.val
   }

   get arg (): ITraced {
      return this._arg.val
   }

   get appBody (): AppBody {
      return this._appBody.val
   }
}

export class Match {
   _pattern: ITraced<ITraced>
   _matchBody: ITraced<ITraced>

   static at (α: Addr, pattern: ITraced<ITraced>, matchBody: ITraced<ITraced>): Match {
      const this_: Match = create(α, Match)
      this_._pattern = typeCheck_(pattern, ITraced)
      this_._matchBody = typeCheck_(matchBody, ITraced)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, pattern: ITraced, matchBody: ITraced): Match {
      return Match.at(α, __val(keyP(α, 'pattern'), pattern), __val(keyP(α, 'matchBody'), matchBody))
   }

   get pattern (): ITraced {
      return this._pattern.val
   }

   get matchBody (): ITraced {
      return this._matchBody.val
   }
}

// See 0.6.1 release notes. Also 0.6.4 notes for discussion of expression/trace disparity.
export class MatchAs extends Trace {
   _e: ITraced<ITraced>
   _σ: ITraced<Trie<ITraced>>

   static at (α: Addr, e: ITraced<ITraced>, σ: ITraced<Trie<ITraced>>): MatchAs {
      const this_: MatchAs = create(α, MatchAs)
      this_._e = typeCheck_(e, ITraced)
      this_._σ = typeCheck_(σ, Trie)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, e: ITraced, σ: Trie<ITraced>): MatchAs {
      return MatchAs.at(α, __val(keyP(α, 'e'), e), __val(keyP(α, 'σ'), σ))
   }

   __visit <T> (v: TraceVisitor<T>): T {
      return v.is_MatchAs(this)
   }

   get e (): ITraced {
      return this._e.val
   }

   get σ (): Trie<ITraced> {
      return this._σ.val
   }
}

// Not abstract, so that I can assert it as a runtime type. Shouldn't T extend JoinSemilattice<T>?
export class Trie<T> implements JoinSemilattice<Trie<T>> {
   join (σ: Trie<T>): Trie<T> {
      return join(this, σ)
   }
}

export class ConstrTrie<T> extends Trie<T> {
   _cases: ITraced<FiniteMap<Str, T>>

   static at <T> (α: Addr, cases: ITraced<FiniteMap<Str, T>>): ConstrTrie<T> {
      const this_: ConstrTrie<T> = create(α, ConstrTrie)
      this_._cases = typeCheck_(cases, Tree)
      this_.__version()
      return this_
   }

   static at_ <T> (α: Addr, cases: FiniteMap<Str, T>): ConstrTrie<T> {
      return ConstrTrie.at(α, __val(keyP(α, 'cases'), cases))
   }

   get cases (): FiniteMap<Str, T> {
      return this._cases.val
   }
}

export class VarTrie<T> extends Trie<T> {
   _name: ITraced<Str>
   _body: ITraced<T>

   static at <T> (α: Addr, name: ITraced<Str>, body: ITraced<T>): VarTrie<T> {
      const this_: VarTrie<T> = create(α, VarTrie)
      this_._name = typeCheck_(name, Str)
      this_._body = typeCheck(body, ITraced)
      this_.__version()
      return this_
   }

   static at_ <T> (α: Addr, name: Str, body: T): VarTrie<T> {
      return VarTrie.at(α, __val(keyP(α, 'name'), name), __val(keyP(α, 'body'), body))
   }

   get name (): Str {
      return this._name.val
   }

   get body (): T {
      return this._body.val
   }
}

export class FunTrie<T> extends Trie<T> {
   _body: ITraced<T>

   static at <T> (α: Addr, body: ITraced<T>): FunTrie<T> {
      const this_: FunTrie<T> = create(α, FunTrie)
      this_._body = typeCheck(body, ITraced)
      this_.__version()
      return this_
   }

   static at_ <T> (α: Addr, body: T): FunTrie<T> {
      return FunTrie.at(α, __val(keyP(α, 'body'), body))
   }

   get body (): T {
      return this._body.val
   }
}

// A let is simply a match where the trie is a variable trie.
export class Let extends Trace {
   _e: ITraced<ITraced>
   _σ: ITraced<VarTrie<Object>>

   static at (α: Addr, e: ITraced<ITraced>, σ: ITraced<VarTrie<Object>>): Let {
      const this_: Let = create(α, Let)
      this_._e = typeCheck_(e, ITraced)
      this_._σ = typeCheck_(σ, VarTrie)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, e: ITraced, σ: VarTrie<Object>): Let {
      return Let.at(α, __val(keyP(α, 'e'), e), __val(keyP(α, 'σ'), σ))
   }

   __visit <T> (v: TraceVisitor<T>): T {
      return v.is_Let(this)
   }

   get e (): ITraced {
      return this._e.val
   }

   get σ (): VarTrie<Object> {
      return this._σ.val
   }
}

export class RecDefinition {
   _name: ITraced<Str>
   _func: ITraced<Fun>

   static at (α: Addr, name: ITraced<Str>, func: ITraced<Fun>): RecDefinition {
      const this_: RecDefinition = create(α, RecDefinition)
      this_._name = typeCheck(name, ITraced)
      this_._func = typeCheck_(func, Fun)
      this_.__version()
      return this_
   }

   get name (): Str {
      return this._name.val
   }

   get func (): Fun {
      return this._func.val
   }
}

// Keep binding of recursive definitions to closures separate from the definitions themselves so that
// closures can contain definitions without inducing cycles.
export class RecBinding {
   _def: ITraced<RecDefinition>;
   _valueOpt: ITraced<Prim.Option<Closure>>;

   static at (α: Addr, def: ITraced<RecDefinition>, valueOpt: ITraced<Prim.Option<Closure>>): RecBinding {
      const this_: RecBinding = create(α, RecBinding)
      this_._def = typeCheck_(def, RecDefinition)
      this_._valueOpt = typeCheck_(valueOpt, Prim.Option)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, def: RecDefinition, valueOpt: Prim.Option<Closure>): RecBinding {
      return RecBinding.at(α, __val(keyP(α, 'def'), def), __val(keyP(α, 'valueOpt'), valueOpt))
   }

   get def (): RecDefinition {
      return this._def.val
   }

   get valueOpt (): Prim.Option<Closure> {
      return this._valueOpt.val
   }
}

export class LetRec extends Trace {
   _bindings: ITraced<List<RecBinding>>
   _body: ITraced<Trace>

   static at (α: Addr, bindings: ITraced<List<RecBinding>>, body: ITraced<Trace>): LetRec {
      const this_: LetRec = create(α, LetRec)
      this_._bindings = typeCheck_(bindings, List)
      this_._body = typeCheck_(body, Trace)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, bindings: List<RecBinding>, body: Trace): LetRec {
      return LetRec.at(α, __val(keyP(α, 'bindings'), bindings), __val(keyP(α, 'body'), body))
   }

   __visit <T> (v: TraceVisitor<T>): T {
      return v.is_LetRec(this)
   }

   get bindings (): List<RecBinding> {
      return this._bindings.val
   }

   get body (): Trace {
      return this._body.val
   }
}

export interface JoinSemilattice<T> {
    join (t: T): T
}

// Addressing scheme doesn't yet support "member functions". Plus methods don't allow null receivers.
__def(join)
export function join <T extends JoinSemilattice<T>> (σ: Trie<T>, τ: Trie<T>): Trie<T> {
   const α: Addr = key(join, arguments)
   if (σ === null) {
      return τ
   } else
   if (τ === null) {
      return σ
   } else
   // The instanceof guards turns T into 'any'. Yuk.
   if (σ instanceof FunTrie && τ instanceof FunTrie) {
      const [σʹ, τʹ]: [FunTrie<T>, FunTrie<T>] = [σ, τ]
      return FunTrie.at_(α, join(σʹ.body, τʹ.body))
   } else
   if (σ instanceof VarTrie && τ instanceof VarTrie && eq(σ.name, τ.name)) {
      const [σʹ, τʹ]: [VarTrie<T>, VarTrie<T>] = [σ, τ]
      return VarTrie.at_(α, σʹ.name, join(σʹ.body, τʹ.body))
   } else
   if (σ instanceof ConstrTrie && τ instanceof ConstrTrie) {
      // const [σʹ, τʹ]: [ConstrTrie<T>, ConstrTrie<T>] = [σ, τ]
      return ConstrTrie.at_(α, unionWith(σ.cases, τ.cases, join))
   } else {
      return assertMessage(false, 'Undefined join.', σ, τ)
   }
}
