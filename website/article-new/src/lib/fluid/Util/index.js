import * as $runtime from "../runtime.js";
import * as Control$dApplicative from "../Control.Applicative/index.js";
import * as Control$dBind from "../Control.Bind/index.js";
import * as Control$dCategory from "../Control.Category/index.js";
import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dArray$dNonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dNonEmpty from "../Data.List.NonEmpty/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dProfunctor$dStrong from "../Data.Profunctor.Strong/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Debug from "../Debug/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
const $WithTypeLeft = (_1, _2) => ({tag: "WithTypeLeft", _1, _2});
const fanout = /* #__PURE__ */ Data$dProfunctor$dStrong.fanout(Control$dCategory.categoryFn)(Data$dProfunctor$dStrong.strongFn);
const intercalate = sep => xs => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const b = go$a0, v = go$a1;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = b;
        continue;
      }
      if (v.tag === "Cons") {
        go$a0 = b.init
          ? {init: false, acc: v._1}
          : {init: false, acc: Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(v._1)(sep))(b.acc)};
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go({init: true, acc: Data$dList$dTypes.Nil})(xs).acc;
};
const identity = x => x;
const WithTypeLeft = value0 => value1 => $WithTypeLeft(value0, value1);
const zipList = {zip: Data$dList.zip, zipWith: Data$dList.zipWith};
const zipArray = {zip: /* #__PURE__ */ Data$dArray.zipWith(Data$dTuple.Tuple), zipWith: Data$dArray.zipWith};
const singletonSet = {singleton: Data$dSet.singleton};
const singletonNonEmptySet = {singleton: Data$dSet.singleton};
const singletonNonEmptyList = /* #__PURE__ */ (() => ({singleton: Data$dList$dTypes.applicativeNonEmptyList.pure}))();
const singletonList = /* #__PURE__ */ (() => ({singleton: Data$dList$dTypes.applicativeList.pure}))();
const singletonArray = /* #__PURE__ */ (() => ({singleton: Control$dApplicative.applicativeArray.pure}))();
const lengthList = {length: Data$dList.length};
const lengthArray = {length: Data$dArray.length};
const isEmptySet = {isEmpty: Data$dMap$dInternal.isEmpty};
const isEmptyObject = {isEmpty: Foreign$dObject.isEmpty};
const functor$x60$x215$x124 = dictFunctor => ({map: f => m => $WithTypeLeft(m._1, dictFunctor.map(f)(m._2))});
const zipWith = dict => dict.zipWith;
const zip = dict => dict.zip;
const $$with = x => f => Data$dTuple.$Tuple(x, f(x));
const whenever = v => {
  if (!v) { return v$1 => Data$dMaybe.Nothing; }
  if (v) { return Data$dMaybe.Just; }
  $runtime.fail();
};
const unzip = dictFunctor => fanout(v => dictFunctor.map(Data$dTuple.fst)(v))(v => dictFunctor.map(Data$dTuple.snd)(v));
const unsafeUpdateAt = dict => dict.unsafeUpdateAt;
const unsafeIndex = dict => dict.unsafeIndex;
const unionWithMaybe = dictOrd => {
  const compare = dictOrd.compare;
  return f => m => m$p => {
    const go = v => {
      if (v.tag === "Leaf") { return Data$dMap$dInternal.Leaf; }
      if (v.tag === "Node") { return Data$dMap$dInternal.$$$Map("Node", v._1, v._2, v._3, Data$dMaybe.$Maybe("Just", v._4), go(v._5), go(v._6)); }
      $runtime.fail();
    };
    const go$1 = v => {
      if (v.tag === "Leaf") { return Data$dMap$dInternal.Leaf; }
      if (v.tag === "Node") { return Data$dMap$dInternal.$$$Map("Node", v._1, v._2, v._3, Data$dMaybe.$Maybe("Just", v._4), go$1(v._5), go$1(v._6)); }
      $runtime.fail();
    };
    return Data$dMap$dInternal.unsafeUnionWith(
      compare,
      x => x$1 => {
        if (x.tag === "Just" && x$1.tag === "Just") { return f(x._1)(x$1._1); }
        return Data$dMaybe.Nothing;
      },
      go(m),
      go$1(m$p)
    );
  };
};
const unimplemented = "unimplemented";
const uncurry3 = f => v => f(v._1)(v._2._1)(v._2._2);
const tuple3 = a => b => c => Data$dTuple.$Tuple(a, Data$dTuple.$Tuple(b, c));
const throwLeft = dictMonadError => {
  const $0 = dictMonadError.MonadThrow0();
  const $1 = $0.Monad0().Applicative0().pure;
  return dictShow => x => {
    if (x.tag === "Left") { return $0.throwError(Effect$dException.error(dictShow.show(x._1))); }
    if (x.tag === "Right") { return $1(x._1); }
    $runtime.fail();
  };
};
const $$throw = dictMonadThrow => x => dictMonadThrow.throwError(Effect$dException.error(x));
const withMsg = dictMonadError => {
  const throw2 = $$throw(dictMonadError.MonadThrow0());
  return msg => m => dictMonadError.catchError(m)(e => throw2(Effect$dException.message(e) + (msg === "" ? "" : "\n" + msg)));
};
const withMsg1 = /* #__PURE__ */ withMsg(/* #__PURE__ */ Control$dMonad$dExcept$dTrans.monadErrorExceptT(Data$dIdentity.monadIdentity));
const tail = dict => dict.tail;
const slipl = f => c => a => b => f(a)(b)(c);
const singleton = dict => dict.singleton;
const orElse = dictMonadThrow => v => v1 => {
  if (v1.tag === "Nothing") { return dictMonadThrow.throwError(Effect$dException.error(v)); }
  if (v1.tag === "Just") { return dictMonadThrow.Monad0().Applicative0().pure(v1._1); }
  $runtime.fail();
};
const onlyIf = dictBind => dictAlternative => {
  const empty = dictAlternative.Plus1().empty;
  return b => a => dictBind.bind(b ? dictAlternative.Applicative0().pure() : empty)(() => dictAlternative.Applicative0().pure(a));
};
const om = dictMonad => f => m => x => dictMonad.Bind1().bind(m)(a => f(a)(x));
const nonEmpty = dict => dict.nonEmpty;
const mayEq = dictEq => x => x$p => {
  const $0 = dictEq.eq(x)(x$p);
  if (!$0) { return Data$dMaybe.Nothing; }
  if ($0) { return Data$dMaybe.$Maybe("Just", x); }
  $runtime.fail();
};
const mayFailEq = dictMonadThrow => dictShow => dictEq => x => x$p => orElse(dictMonadThrow)(dictShow.show(x) + " ≠ " + dictShow.show(x$p))((() => {
  const $0 = dictEq.eq(x)(x$p);
  if (!$0) { return Data$dMaybe.Nothing; }
  if ($0) { return Data$dMaybe.$Maybe("Just", x); }
  $runtime.fail();
})());
const length = dict => dict.length;
const isEmpty = dict => dict.isEmpty;
const intersperse = x => xs => intercalate(Data$dList$dTypes.$List("Cons", x, Data$dList$dTypes.Nil))(Data$dList$dTypes.listMap(Data$dList$dTypes.applicativeList.pure)(xs));
const init = dict => dict.init;
const force = v => v();
const findM = dictAlt => {
  const alt = dictAlt.alt;
  return dictMonad => {
    const $0 = dictMonad.Bind1().Apply0();
    return dictFoldable => xs => f => base => dictFoldable.foldr(x => acc => $0.apply($0.Functor0().map(alt)(acc))(f(x)))(dictMonad.Applicative0().pure(base))(xs);
  };
};
const error = msg => Effect$dException.throwException(Effect$dException.error(msg))();
const shapeMismatch = v => Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
const dup = x => Data$dTuple.$Tuple(x, x);
const definitely = v => v1 => {
  if (v1.tag === "Just") { return v1._1; }
  if (v1.tag === "Nothing") { return Effect$dException.throwException(Effect$dException.error("definitely " + v))(); }
  $runtime.fail();
};
const withinBounds = /* #__PURE__ */ definitely("index within bounds");
const unsafeArrayArray = {
  unsafeIndex: xs => i => definitely("index within bounds")(i >= 0 && i < xs.length ? Data$dMaybe.$Maybe("Just", xs[i]) : Data$dMaybe.Nothing),
  unsafeUpdateAt: i => x => x$1 => definitely("index within bounds")(Data$dArray._updateAt(Data$dMaybe.Just, Data$dMaybe.Nothing, i, x, x$1))
};
const unsafeArrayNonEmptyArray = {
  unsafeIndex: xs => i => definitely("index within bounds")(i >= 0 && i < xs.length ? Data$dMaybe.$Maybe("Just", xs[i]) : Data$dMaybe.Nothing),
  unsafeUpdateAt: i => x => x$1 => definitely("index within bounds")(Data$dArray._updateAt(Data$dMaybe.Just, Data$dMaybe.Nothing, i, x, x$1))
};
const nonEmptyArrayNonEmptyArra = {
  nonEmpty: x => definitely("non-empty")(x.length > 0 ? Data$dMaybe.$Maybe("Just", x) : Data$dMaybe.Nothing),
  init: Data$dArray$dNonEmpty.init,
  tail: Data$dArray$dNonEmpty.tail
};
const nonEmptyListNonEmptyList = {
  nonEmpty: x => definitely("non-empty")((() => {
    if (x.tag === "Nil") { return Data$dMaybe.Nothing; }
    if (x.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dNonEmpty.$NonEmpty(x._1, x._2)); }
    $runtime.fail();
  })()),
  init: Data$dList$dNonEmpty.init,
  tail: Data$dList$dNonEmpty.tail
};
const defined = x => {
  if (x.tag === "Right") { return x._1; }
  if (x.tag === "Left") { return Effect$dException.throwException(Effect$dException.error(Effect$dException.showErrorImpl(x._1)))(); }
  $runtime.fail();
};
const definedWith = msg => {
  const $0 = withMsg1(msg);
  return x => defined($0(x));
};
const debug = {logging: false, tracing: true};
const log$p = dictMonadEffect => {
  const $0 = dictMonadEffect.Monad0().Applicative0();
  return msg => $0.pure();
};
const spyWhen = v => v1 => v2 => v3 => {
  if (v) { return Debug._trace(v1 + ":", v4 => Debug._trace(v2(v3), v$1 => v3)); }
  return v3;
};
const spy = /* #__PURE__ */ spyWhen(true);
const spyFunWhenM = dictFunctor => b => s => showIn => showOut => f => x => dictFunctor.map(spyWhen(b)(s + " output")(showOut))(f(spyWhen(b)(s + " input")(showIn)(x)));
const spyFunWhen = b => s => showIn => showOut => f => spyFunWhenM(Data$dIdentity.functorIdentity)(b)(s)(showIn)(showOut)(x => f(x));
const traceWhen = dictApplicative => v => v1 => {
  if (v) { return Debug._trace(v1, v2 => dictApplicative.pure()); }
  return dictApplicative.pure();
};
const trace = dictApplicative => traceWhen(dictApplicative)(true);
const concatM = dictFoldable => dictMonad => dictFoldable.foldr((() => {
  const $0 = dictMonad.Bind1();
  return f => g => a => $0.bind(f(a))(g);
})())(dictMonad.Applicative0().pure);
const checkSatisfies = dictMonadThrow => {
  const $0 = dictMonadThrow.Monad0().Applicative0();
  return dictShow => msg => x => pred => {
    const $1 = pred(x);
    const $2 = dictMonadThrow.throwError(Effect$dException.error(dictShow.show(x) + " doesn't satisfy " + msg));
    if (!$1) { return $2; }
    if ($1) { return $0.pure(); }
    $runtime.fail();
  };
};
const check = dictMonadThrow => v => {
  if (!v) { return $$throw(dictMonadThrow); }
  if (v) {
    const $0 = dictMonadThrow.Monad0().Applicative0().pure();
    return v$1 => $0;
  }
  $runtime.fail();
};
const both = dictCategory => {
  const $0 = dictCategory.Semigroupoid0();
  return dictStrong => f => $0.compose(dictStrong.second(f))(dictStrong.first(f));
};
const bind2Flipped = dictMonad => {
  const Bind1 = dictMonad.Bind1();
  const $0 = Bind1.Apply0();
  return f => x => y => Bind1.bind($0.apply($0.Functor0().map(f)(x))(y))(Control$dBind.identity);
};
const assoc2 = v => Data$dTuple.$Tuple(Data$dTuple.$Tuple(v._1, v._2._1), v._2._2);
const assoc1 = v => Data$dTuple.$Tuple(v._1._1, Data$dTuple.$Tuple(v._1._2, v._2));
const assertWith = v => v1 => {
  if (v1) { return identity; }
  return v2 => Effect$dException.throwException(Effect$dException.error("Assertion failure: " + v))();
};
const assertWhen = v => v1 => {
  if (!v) { return v$1 => identity; }
  if (v) { return x => assertWith(v1)(x()); }
  $runtime.fail();
};
const mustEq = dictEq => dictShow => x => x$p => assertWhen(false)("mustEq")(v => dictEq.eq(x)(x$p))(x);
const validateWhen = b => msg => p => a => assertWhen(b)(msg)(v => p(a))(a);
const validate = /* #__PURE__ */ validateWhen(true);
const assert = /* #__PURE__ */ assertWith("");
const appendList = v => ys => Data$dNonEmpty.$NonEmpty(v._1, Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(ys)(v._2));
const absurd = "absurd";
const definitely$p = /* #__PURE__ */ definitely("absurd");
export {
  
  
  
  
  
  assertWhen,
  assertWith,
  
  
  bind2Flipped,
  
  check,
  
  
  
  defined,
  
  definitely,
  
  
  
  
  
  
  
  
  
  
  
  
  isEmptyObject,
  isEmptySet,
  
  
  
  
  
  mayFailEq,
  
  
  
  nonEmptyListNonEmptyList,
  
  
  orElse,
  
  
  
  
  
  
  
  
  
  spyFunWhen,
  spyFunWhenM,
  spyWhen,
  
  $$throw as throw,
  throwLeft,
  
  traceWhen,
  
  
  
  
  unsafeArrayArray,
  unsafeArrayNonEmptyArray,
  
  
  
  
  
  
  
  withMsg,
  
  
  
  
  
  
};
