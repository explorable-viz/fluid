// | This module defines the _exception monad transformer_ `ExceptT`.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const identity = x => x;
const ExceptT = x => x;
const withExceptT = dictFunctor => f => v => dictFunctor.map(v2 => {
  if (v2.tag === "Right") { return Data$dEither.$Either("Right", v2._1); }
  if (v2.tag === "Left") { return Data$dEither.$Either("Left", f(v2._1)); }
  $runtime.fail();
})(v);
const runExceptT = v => v;
const newtypeExceptT = {Coercible0: () => {}};
const monadTransExceptT = {lift: dictMonad => m => dictMonad.Bind1().bind(m)(a => dictMonad.Applicative0().pure(Data$dEither.$Either("Right", a)))};
const mapExceptT = f => v => f(v);
const functorExceptT = dictFunctor => (
  {
    map: f => dictFunctor.map(m => {
      if (m.tag === "Left") { return Data$dEither.$Either("Left", m._1); }
      if (m.tag === "Right") { return Data$dEither.$Either("Right", f(m._1)); }
      $runtime.fail();
    })
  }
);
const except = dictApplicative => x => dictApplicative.pure(x);
const monadExceptT = dictMonad => ({Applicative0: () => applicativeExceptT(dictMonad), Bind1: () => bindExceptT(dictMonad)});
const bindExceptT = dictMonad => (
  {
    bind: v => k => dictMonad.Bind1().bind(v)(v2 => {
      if (v2.tag === "Left") { return dictMonad.Applicative0().pure(Data$dEither.$Either("Left", v2._1)); }
      if (v2.tag === "Right") { return k(v2._1); }
      $runtime.fail();
    }),
    Apply0: () => applyExceptT(dictMonad)
  }
);
const applyExceptT = dictMonad => {
  const $0 = dictMonad.Bind1().Apply0().Functor0();
  const functorExceptT1 = {
    map: f => $0.map(m => {
      if (m.tag === "Left") { return Data$dEither.$Either("Left", m._1); }
      if (m.tag === "Right") { return Data$dEither.$Either("Right", f(m._1)); }
      $runtime.fail();
    })
  };
  return {
    apply: (() => {
      const $1 = bindExceptT(dictMonad);
      return f => a => $1.bind(f)(f$p => $1.bind(a)(a$p => applicativeExceptT(dictMonad).pure(f$p(a$p))));
    })(),
    Functor0: () => functorExceptT1
  };
};
const applicativeExceptT = dictMonad => ({pure: x => dictMonad.Applicative0().pure(Data$dEither.$Either("Right", x)), Apply0: () => applyExceptT(dictMonad)});
const semigroupExceptT = dictMonad => {
  const $0 = applyExceptT(dictMonad);
  return dictSemigroup => (
    {
      append: (() => {
        const $1 = dictSemigroup.append;
        return a => b => $0.apply($0.Functor0().map($1)(a))(b);
      })()
    }
  );
};
const monadAskExceptT = dictMonadAsk => {
  const Monad0 = dictMonadAsk.Monad0();
  const monadExceptT1 = {Applicative0: () => applicativeExceptT(Monad0), Bind1: () => bindExceptT(Monad0)};
  return {ask: Monad0.Bind1().bind(dictMonadAsk.ask)(a => Monad0.Applicative0().pure(Data$dEither.$Either("Right", a))), Monad0: () => monadExceptT1};
};
const monadReaderExceptT = dictMonadReader => {
  const monadAskExceptT1 = monadAskExceptT(dictMonadReader.MonadAsk0());
  return {local: f => dictMonadReader.local(f), MonadAsk0: () => monadAskExceptT1};
};
const monadContExceptT = dictMonadCont => {
  const $0 = dictMonadCont.Monad0();
  const monadExceptT1 = {Applicative0: () => applicativeExceptT($0), Bind1: () => bindExceptT($0)};
  return {callCC: f => dictMonadCont.callCC(c => f(a => c(Data$dEither.$Either("Right", a)))), Monad0: () => monadExceptT1};
};
const monadEffectExceptT = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const monadExceptT1 = {Applicative0: () => applicativeExceptT(Monad0), Bind1: () => bindExceptT(Monad0)};
  return {liftEffect: x => Monad0.Bind1().bind(dictMonadEffect.liftEffect(x))(a => Monad0.Applicative0().pure(Data$dEither.$Either("Right", a))), Monad0: () => monadExceptT1};
};
const monadRecExceptT = dictMonadRec => {
  const Monad0 = dictMonadRec.Monad0();
  const monadExceptT1 = {Applicative0: () => applicativeExceptT(Monad0), Bind1: () => bindExceptT(Monad0)};
  return {
    tailRecM: f => dictMonadRec.tailRecM(a => Monad0.Bind1().bind(f(a))(m$p => Monad0.Applicative0().pure((() => {
      if (m$p.tag === "Left") { return Control$dMonad$dRec$dClass.$Step("Done", Data$dEither.$Either("Left", m$p._1)); }
      if (m$p.tag === "Right") {
        if (m$p._1.tag === "Loop") { return Control$dMonad$dRec$dClass.$Step("Loop", m$p._1._1); }
        if (m$p._1.tag === "Done") { return Control$dMonad$dRec$dClass.$Step("Done", Data$dEither.$Either("Right", m$p._1._1)); }
      }
      $runtime.fail();
    })()))),
    Monad0: () => monadExceptT1
  };
};
const monadStateExceptT = dictMonadState => {
  const Monad0 = dictMonadState.Monad0();
  const monadExceptT1 = {Applicative0: () => applicativeExceptT(Monad0), Bind1: () => bindExceptT(Monad0)};
  return {state: f => Monad0.Bind1().bind(dictMonadState.state(f))(a => Monad0.Applicative0().pure(Data$dEither.$Either("Right", a))), Monad0: () => monadExceptT1};
};
const monadTellExceptT = dictMonadTell => {
  const Monad1 = dictMonadTell.Monad1();
  const Semigroup0 = dictMonadTell.Semigroup0();
  const monadExceptT1 = {Applicative0: () => applicativeExceptT(Monad1), Bind1: () => bindExceptT(Monad1)};
  return {
    tell: x => Monad1.Bind1().bind(dictMonadTell.tell(x))(a => Monad1.Applicative0().pure(Data$dEither.$Either("Right", a))),
    Semigroup0: () => Semigroup0,
    Monad1: () => monadExceptT1
  };
};
const monadWriterExceptT = dictMonadWriter => {
  const MonadTell1 = dictMonadWriter.MonadTell1();
  const Monad1 = MonadTell1.Monad1();
  const $0 = Monad1.Bind1();
  const $1 = Monad1.Applicative0();
  const Monoid0 = dictMonadWriter.Monoid0();
  const monadTellExceptT1 = monadTellExceptT(MonadTell1);
  return {
    listen: v => $0.bind(dictMonadWriter.listen(v))(v$1 => $1.pure((() => {
      if (v$1._1.tag === "Left") { return Data$dEither.$Either("Left", v$1._1._1); }
      if (v$1._1.tag === "Right") { return Data$dEither.$Either("Right", Data$dTuple.$Tuple(v$1._1._1, v$1._2)); }
      $runtime.fail();
    })())),
    pass: v => dictMonadWriter.pass($0.bind(v)(a => $1.pure((() => {
      if (a.tag === "Left") { return Data$dTuple.$Tuple(Data$dEither.$Either("Left", a._1), identity); }
      if (a.tag === "Right") { return Data$dTuple.$Tuple(Data$dEither.$Either("Right", a._1._1), a._1._2); }
      $runtime.fail();
    })()))),
    Monoid0: () => Monoid0,
    MonadTell1: () => monadTellExceptT1
  };
};
const monadThrowExceptT = dictMonad => {
  const monadExceptT1 = {Applicative0: () => applicativeExceptT(dictMonad), Bind1: () => bindExceptT(dictMonad)};
  return {throwError: x => dictMonad.Applicative0().pure(Data$dEither.$Either("Left", x)), Monad0: () => monadExceptT1};
};
const monadErrorExceptT = dictMonad => {
  const monadThrowExceptT1 = monadThrowExceptT(dictMonad);
  return {
    catchError: v => k => dictMonad.Bind1().bind(v)(v2 => {
      if (v2.tag === "Left") { return k(v2._1); }
      if (v2.tag === "Right") { return dictMonad.Applicative0().pure(Data$dEither.$Either("Right", v2._1)); }
      $runtime.fail();
    }),
    MonadThrow0: () => monadThrowExceptT1
  };
};
const monoidExceptT = dictMonad => {
  const semigroupExceptT1 = semigroupExceptT(dictMonad);
  return dictMonoid => {
    const semigroupExceptT2 = semigroupExceptT1(dictMonoid.Semigroup0());
    return {mempty: applicativeExceptT(dictMonad).pure(dictMonoid.mempty), Semigroup0: () => semigroupExceptT2};
  };
};
const altExceptT = dictSemigroup => dictMonad => {
  const Bind1 = dictMonad.Bind1();
  const $0 = dictMonad.Applicative0();
  const $1 = Bind1.Apply0().Functor0();
  const functorExceptT1 = {
    map: f => $1.map(m => {
      if (m.tag === "Left") { return Data$dEither.$Either("Left", m._1); }
      if (m.tag === "Right") { return Data$dEither.$Either("Right", f(m._1)); }
      $runtime.fail();
    })
  };
  return {
    alt: v => v1 => Bind1.bind(v)(rm => {
      if (rm.tag === "Right") { return $0.pure(Data$dEither.$Either("Right", rm._1)); }
      if (rm.tag === "Left") {
        const $2 = rm._1;
        return Bind1.bind(v1)(rn => {
          if (rn.tag === "Right") { return $0.pure(Data$dEither.$Either("Right", rn._1)); }
          if (rn.tag === "Left") { return $0.pure(Data$dEither.$Either("Left", dictSemigroup.append($2)(rn._1))); }
          $runtime.fail();
        });
      }
      $runtime.fail();
    }),
    Functor0: () => functorExceptT1
  };
};
const plusExceptT = dictMonoid => {
  const mempty = dictMonoid.mempty;
  const altExceptT1 = altExceptT(dictMonoid.Semigroup0());
  return dictMonad => {
    const altExceptT2 = altExceptT1(dictMonad);
    return {empty: monadThrowExceptT(dictMonad).throwError(mempty), Alt0: () => altExceptT2};
  };
};
const alternativeExceptT = dictMonoid => {
  const plusExceptT1 = plusExceptT(dictMonoid);
  return dictMonad => {
    const applicativeExceptT1 = applicativeExceptT(dictMonad);
    const plusExceptT2 = plusExceptT1(dictMonad);
    return {Applicative0: () => applicativeExceptT1, Plus1: () => plusExceptT2};
  };
};
const monadPlusExceptT = dictMonoid => {
  const alternativeExceptT1 = alternativeExceptT(dictMonoid);
  return dictMonad => {
    const monadExceptT1 = {Applicative0: () => applicativeExceptT(dictMonad), Bind1: () => bindExceptT(dictMonad)};
    const alternativeExceptT2 = alternativeExceptT1(dictMonad);
    return {Monad0: () => monadExceptT1, Alternative1: () => alternativeExceptT2};
  };
};
export {
  
  altExceptT,
  
  applicativeExceptT,
  
  bindExceptT,
  
  
  
  
  
  
  monadEffectExceptT,
  monadErrorExceptT,
  
  
  
  
  
  
  monadThrowExceptT,
  
  
  
  
  
  
  
  
};
