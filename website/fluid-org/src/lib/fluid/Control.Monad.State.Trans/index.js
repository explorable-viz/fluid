// | This module defines the state monad transformer, `StateT`.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const StateT = x => x;
const withStateT = f => v => x => v(f(x));
const runStateT = v => v;
const newtypeStateT = {Coercible0: () => {}};
const monadTransStateT = {lift: dictMonad => m => s => dictMonad.Bind1().bind(m)(x => dictMonad.Applicative0().pure(Data$dTuple.$Tuple(x, s)))};
const mapStateT = f => v => x => f(v(x));
const lazyStateT = {defer: f => s => f()(s)};
const functorStateT = dictFunctor => ({map: f => v => s => dictFunctor.map(v1 => Data$dTuple.$Tuple(f(v1._1), v1._2))(v(s))});
const execStateT = dictFunctor => v => s => dictFunctor.map(Data$dTuple.snd)(v(s));
const evalStateT = dictFunctor => v => s => dictFunctor.map(Data$dTuple.fst)(v(s));
const monadStateT = dictMonad => ({Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad)});
const bindStateT = dictMonad => ({bind: v => f => s => dictMonad.Bind1().bind(v(s))(v1 => f(v1._1)(v1._2)), Apply0: () => applyStateT(dictMonad)});
const applyStateT = dictMonad => {
  const $0 = dictMonad.Bind1().Apply0().Functor0();
  const functorStateT1 = {map: f => v => s => $0.map(v1 => Data$dTuple.$Tuple(f(v1._1), v1._2))(v(s))};
  return {
    apply: (() => {
      const $1 = bindStateT(dictMonad);
      return f => a => $1.bind(f)(f$p => $1.bind(a)(a$p => applicativeStateT(dictMonad).pure(f$p(a$p))));
    })(),
    Functor0: () => functorStateT1
  };
};
const applicativeStateT = dictMonad => ({pure: a => s => dictMonad.Applicative0().pure(Data$dTuple.$Tuple(a, s)), Apply0: () => applyStateT(dictMonad)});
const semigroupStateT = dictMonad => {
  const $0 = applyStateT(dictMonad);
  return dictSemigroup => (
    {
      append: (() => {
        const $1 = dictSemigroup.append;
        return a => b => $0.apply($0.Functor0().map($1)(a))(b);
      })()
    }
  );
};
const monadAskStateT = dictMonadAsk => {
  const Monad0 = dictMonadAsk.Monad0();
  const monadStateT1 = {Applicative0: () => applicativeStateT(Monad0), Bind1: () => bindStateT(Monad0)};
  return {
    ask: (() => {
      const $0 = dictMonadAsk.ask;
      return s => Monad0.Bind1().bind($0)(x => Monad0.Applicative0().pure(Data$dTuple.$Tuple(x, s)));
    })(),
    Monad0: () => monadStateT1
  };
};
const monadReaderStateT = dictMonadReader => {
  const monadAskStateT1 = monadAskStateT(dictMonadReader.MonadAsk0());
  return {
    local: x => {
      const $0 = dictMonadReader.local(x);
      return v => x$1 => $0(v(x$1));
    },
    MonadAsk0: () => monadAskStateT1
  };
};
const monadContStateT = dictMonadCont => {
  const $0 = dictMonadCont.Monad0();
  const monadStateT1 = {Applicative0: () => applicativeStateT($0), Bind1: () => bindStateT($0)};
  return {callCC: f => s => dictMonadCont.callCC(c => f(a => s$p => c(Data$dTuple.$Tuple(a, s$p)))(s)), Monad0: () => monadStateT1};
};
const monadEffectState = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const monadStateT1 = {Applicative0: () => applicativeStateT(Monad0), Bind1: () => bindStateT(Monad0)};
  return {
    liftEffect: x => {
      const $0 = dictMonadEffect.liftEffect(x);
      return s => Monad0.Bind1().bind($0)(x$1 => Monad0.Applicative0().pure(Data$dTuple.$Tuple(x$1, s)));
    },
    Monad0: () => monadStateT1
  };
};
const monadRecStateT = dictMonadRec => {
  const Monad0 = dictMonadRec.Monad0();
  const monadStateT1 = {Applicative0: () => applicativeStateT(Monad0), Bind1: () => bindStateT(Monad0)};
  return {
    tailRecM: f => a => s => dictMonadRec.tailRecM(v => Monad0.Bind1().bind(f(v._1)(v._2))(v2 => Monad0.Applicative0().pure((() => {
      if (v2._1.tag === "Loop") { return Control$dMonad$dRec$dClass.$Step("Loop", Data$dTuple.$Tuple(v2._1._1, v2._2)); }
      if (v2._1.tag === "Done") { return Control$dMonad$dRec$dClass.$Step("Done", Data$dTuple.$Tuple(v2._1._1, v2._2)); }
      $runtime.fail();
    })())))(Data$dTuple.$Tuple(a, s)),
    Monad0: () => monadStateT1
  };
};
const monadStateStateT = dictMonad => {
  const monadStateT1 = {Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad)};
  return {state: f => x => dictMonad.Applicative0().pure(f(x)), Monad0: () => monadStateT1};
};
const monadTellStateT = dictMonadTell => {
  const Monad1 = dictMonadTell.Monad1();
  const Semigroup0 = dictMonadTell.Semigroup0();
  const monadStateT1 = {Applicative0: () => applicativeStateT(Monad1), Bind1: () => bindStateT(Monad1)};
  return {
    tell: x => {
      const $0 = dictMonadTell.tell(x);
      return s => Monad1.Bind1().bind($0)(x$1 => Monad1.Applicative0().pure(Data$dTuple.$Tuple(x$1, s)));
    },
    Semigroup0: () => Semigroup0,
    Monad1: () => monadStateT1
  };
};
const monadWriterStateT = dictMonadWriter => {
  const MonadTell1 = dictMonadWriter.MonadTell1();
  const Monad1 = MonadTell1.Monad1();
  const $0 = Monad1.Bind1();
  const $1 = Monad1.Applicative0();
  const Monoid0 = dictMonadWriter.Monoid0();
  const monadTellStateT1 = monadTellStateT(MonadTell1);
  return {
    listen: m => s => $0.bind(dictMonadWriter.listen(m(s)))(v => $1.pure(Data$dTuple.$Tuple(Data$dTuple.$Tuple(v._1._1, v._2), v._1._2))),
    pass: m => s => dictMonadWriter.pass($0.bind(m(s))(v => $1.pure(Data$dTuple.$Tuple(Data$dTuple.$Tuple(v._1._1, v._2), v._1._2)))),
    Monoid0: () => Monoid0,
    MonadTell1: () => monadTellStateT1
  };
};
const monadThrowStateT = dictMonadThrow => {
  const Monad0 = dictMonadThrow.Monad0();
  const monadStateT1 = {Applicative0: () => applicativeStateT(Monad0), Bind1: () => bindStateT(Monad0)};
  return {
    throwError: e => {
      const $0 = dictMonadThrow.throwError(e);
      return s => Monad0.Bind1().bind($0)(x => Monad0.Applicative0().pure(Data$dTuple.$Tuple(x, s)));
    },
    Monad0: () => monadStateT1
  };
};
const monadErrorStateT = dictMonadError => {
  const monadThrowStateT1 = monadThrowStateT(dictMonadError.MonadThrow0());
  return {catchError: v => h => s => dictMonadError.catchError(v(s))(e => h(e)(s)), MonadThrow0: () => monadThrowStateT1};
};
const monoidStateT = dictMonad => {
  const semigroupStateT1 = semigroupStateT(dictMonad);
  return dictMonoid => {
    const semigroupStateT2 = semigroupStateT1(dictMonoid.Semigroup0());
    return {mempty: applicativeStateT(dictMonad).pure(dictMonoid.mempty), Semigroup0: () => semigroupStateT2};
  };
};
const altStateT = dictMonad => dictAlt => {
  const $0 = dictAlt.Functor0();
  const functorStateT1 = {map: f => v => s => $0.map(v1 => Data$dTuple.$Tuple(f(v1._1), v1._2))(v(s))};
  return {alt: v => v1 => s => dictAlt.alt(v(s))(v1(s)), Functor0: () => functorStateT1};
};
const plusStateT = dictMonad => dictPlus => {
  const empty = dictPlus.empty;
  const $0 = dictPlus.Alt0();
  const $1 = $0.Functor0();
  const altStateT2 = (() => {
    const functorStateT1 = {map: f => v => s => $1.map(v1 => Data$dTuple.$Tuple(f(v1._1), v1._2))(v(s))};
    return {alt: v => v1 => s => $0.alt(v(s))(v1(s)), Functor0: () => functorStateT1};
  })();
  return {empty: v => empty, Alt0: () => altStateT2};
};
const alternativeStateT = dictMonad => {
  const applicativeStateT1 = applicativeStateT(dictMonad);
  return dictAlternative => {
    const $0 = dictAlternative.Plus1();
    const empty = $0.empty;
    const plusStateT2 = (() => {
      const $1 = $0.Alt0();
      const $2 = $1.Functor0();
      const functorStateT1 = {map: f => v => s => $2.map(v1 => Data$dTuple.$Tuple(f(v1._1), v1._2))(v(s))};
      const altStateT2 = {alt: v => v1 => s => $1.alt(v(s))(v1(s)), Functor0: () => functorStateT1};
      return {empty: v => empty, Alt0: () => altStateT2};
    })();
    return {Applicative0: () => applicativeStateT1, Plus1: () => plusStateT2};
  };
};
const monadPlusStateT = dictMonadPlus => {
  const Monad0 = dictMonadPlus.Monad0();
  const monadStateT1 = {Applicative0: () => applicativeStateT(Monad0), Bind1: () => bindStateT(Monad0)};
  const alternativeStateT1 = alternativeStateT(Monad0)(dictMonadPlus.Alternative1());
  return {Monad0: () => monadStateT1, Alternative1: () => alternativeStateT1};
};
export {
  
  
  
  applicativeStateT,
  
  bindStateT,
  
  
  
  
  
  
  
  monadEffectState,
  monadErrorStateT,
  
  monadReaderStateT,
  monadRecStateT,
  monadStateStateT,
  
  
  
  
  
  
  
  
  
  
  
};
