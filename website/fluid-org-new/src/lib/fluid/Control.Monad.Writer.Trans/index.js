// | This module defines the writer monad transformer, `WriterT`.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const WriterT = x => x;
const runWriterT = v => v;
const newtypeWriterT = {Coercible0: () => {}};
const monadTransWriterT = dictMonoid => {
  const mempty = dictMonoid.mempty;
  return {lift: dictMonad => m => dictMonad.Bind1().bind(m)(a => dictMonad.Applicative0().pure(Data$dTuple.$Tuple(a, mempty)))};
};
const mapWriterT = f => v => f(v);
const functorWriterT = dictFunctor => ({map: f => dictFunctor.map(v => Data$dTuple.$Tuple(f(v._1), v._2))});
const execWriterT = dictFunctor => v => dictFunctor.map(Data$dTuple.snd)(v);
const applyWriterT = dictSemigroup => dictApply => {
  const Functor0 = dictApply.Functor0();
  const functorWriterT1 = {map: f => Functor0.map(v => Data$dTuple.$Tuple(f(v._1), v._2))};
  return {apply: v => v1 => dictApply.apply(Functor0.map(v3 => v4 => Data$dTuple.$Tuple(v3._1(v4._1), dictSemigroup.append(v3._2)(v4._2)))(v))(v1), Functor0: () => functorWriterT1};
};
const bindWriterT = dictSemigroup => dictBind => {
  const Apply0 = dictBind.Apply0();
  const Functor0 = Apply0.Functor0();
  const functorWriterT1 = {map: f => Functor0.map(v => Data$dTuple.$Tuple(f(v._1), v._2))};
  const applyWriterT2 = {
    apply: v => v1 => Apply0.apply(Functor0.map(v3 => v4 => Data$dTuple.$Tuple(v3._1(v4._1), dictSemigroup.append(v3._2)(v4._2)))(v))(v1),
    Functor0: () => functorWriterT1
  };
  return {
    bind: v => k => dictBind.bind(v)(v1 => {
      const $0 = v1._2;
      return Apply0.Functor0().map(v3 => Data$dTuple.$Tuple(v3._1, dictSemigroup.append($0)(v3._2)))(k(v1._1));
    }),
    Apply0: () => applyWriterT2
  };
};
const semigroupWriterT = dictApply => dictSemigroup => {
  const Functor0 = dictApply.Functor0();
  return dictSemigroup1 => (
    {
      append: a => b => dictApply.apply(Functor0.map(v3 => v4 => Data$dTuple.$Tuple(v3._1(v4._1), dictSemigroup.append(v3._2)(v4._2)))(Functor0.map(v => Data$dTuple.$Tuple(
        dictSemigroup1.append(v._1),
        v._2
      ))(a)))(b)
    }
  );
};
const applicativeWriterT = dictMonoid => {
  const mempty = dictMonoid.mempty;
  const $0 = dictMonoid.Semigroup0();
  return dictApplicative => {
    const $1 = dictApplicative.Apply0();
    const Functor0 = $1.Functor0();
    const applyWriterT2 = (() => {
      const functorWriterT1 = {map: f => Functor0.map(v => Data$dTuple.$Tuple(f(v._1), v._2))};
      return {apply: v => v1 => $1.apply(Functor0.map(v3 => v4 => Data$dTuple.$Tuple(v3._1(v4._1), $0.append(v3._2)(v4._2)))(v))(v1), Functor0: () => functorWriterT1};
    })();
    return {pure: a => dictApplicative.pure(Data$dTuple.$Tuple(a, mempty)), Apply0: () => applyWriterT2};
  };
};
const monadWriterT = dictMonoid => {
  const applicativeWriterT1 = applicativeWriterT(dictMonoid);
  const bindWriterT1 = bindWriterT(dictMonoid.Semigroup0());
  return dictMonad => {
    const applicativeWriterT2 = applicativeWriterT1(dictMonad.Applicative0());
    const bindWriterT2 = bindWriterT1(dictMonad.Bind1());
    return {Applicative0: () => applicativeWriterT2, Bind1: () => bindWriterT2};
  };
};
const monadAskWriterT = dictMonoid => {
  const mempty = dictMonoid.mempty;
  const monadWriterT1 = monadWriterT(dictMonoid);
  return dictMonadAsk => {
    const Monad0 = dictMonadAsk.Monad0();
    const monadWriterT2 = monadWriterT1(Monad0);
    return {ask: Monad0.Bind1().bind(dictMonadAsk.ask)(a => Monad0.Applicative0().pure(Data$dTuple.$Tuple(a, mempty))), Monad0: () => monadWriterT2};
  };
};
const monadReaderWriterT = dictMonoid => {
  const monadAskWriterT1 = monadAskWriterT(dictMonoid);
  return dictMonadReader => {
    const monadAskWriterT2 = monadAskWriterT1(dictMonadReader.MonadAsk0());
    return {local: f => dictMonadReader.local(f), MonadAsk0: () => monadAskWriterT2};
  };
};
const monadContWriterT = dictMonoid => {
  const mempty = dictMonoid.mempty;
  const monadWriterT1 = monadWriterT(dictMonoid);
  return dictMonadCont => {
    const monadWriterT2 = monadWriterT1(dictMonadCont.Monad0());
    return {callCC: f => dictMonadCont.callCC(c => f(a => c(Data$dTuple.$Tuple(a, mempty)))), Monad0: () => monadWriterT2};
  };
};
const monadEffectWriter = dictMonoid => {
  const mempty = dictMonoid.mempty;
  const monadWriterT1 = monadWriterT(dictMonoid);
  return dictMonadEffect => {
    const Monad0 = dictMonadEffect.Monad0();
    const monadWriterT2 = monadWriterT1(Monad0);
    return {liftEffect: x => Monad0.Bind1().bind(dictMonadEffect.liftEffect(x))(a => Monad0.Applicative0().pure(Data$dTuple.$Tuple(a, mempty))), Monad0: () => monadWriterT2};
  };
};
const monadRecWriterT = dictMonoid => {
  const $0 = dictMonoid.Semigroup0();
  const mempty = dictMonoid.mempty;
  const monadWriterT1 = monadWriterT(dictMonoid);
  return dictMonadRec => {
    const Monad0 = dictMonadRec.Monad0();
    const monadWriterT2 = monadWriterT1(Monad0);
    return {
      tailRecM: f => a => dictMonadRec.tailRecM(v => {
        const $1 = v._2;
        return Monad0.Bind1().bind(f(v._1))(v2 => Monad0.Applicative0().pure((() => {
          if (v2._1.tag === "Loop") { return Control$dMonad$dRec$dClass.$Step("Loop", Data$dTuple.$Tuple(v2._1._1, $0.append($1)(v2._2))); }
          if (v2._1.tag === "Done") { return Control$dMonad$dRec$dClass.$Step("Done", Data$dTuple.$Tuple(v2._1._1, $0.append($1)(v2._2))); }
          $runtime.fail();
        })()));
      })(Data$dTuple.$Tuple(a, mempty)),
      Monad0: () => monadWriterT2
    };
  };
};
const monadStateWriterT = dictMonoid => {
  const mempty = dictMonoid.mempty;
  const monadWriterT1 = monadWriterT(dictMonoid);
  return dictMonadState => {
    const Monad0 = dictMonadState.Monad0();
    const monadWriterT2 = monadWriterT1(Monad0);
    return {state: f => Monad0.Bind1().bind(dictMonadState.state(f))(a => Monad0.Applicative0().pure(Data$dTuple.$Tuple(a, mempty))), Monad0: () => monadWriterT2};
  };
};
const monadTellWriterT = dictMonoid => {
  const Semigroup0 = dictMonoid.Semigroup0();
  const monadWriterT1 = monadWriterT(dictMonoid);
  return dictMonad => {
    const monadWriterT2 = monadWriterT1(dictMonad);
    return {
      tell: (() => {
        const $0 = Data$dTuple.Tuple();
        return x => dictMonad.Applicative0().pure($0(x));
      })(),
      Semigroup0: () => Semigroup0,
      Monad1: () => monadWriterT2
    };
  };
};
const monadWriterWriterT = dictMonoid => {
  const monadTellWriterT1 = monadTellWriterT(dictMonoid);
  return dictMonad => {
    const $0 = dictMonad.Bind1();
    const $1 = dictMonad.Applicative0();
    const monadTellWriterT2 = monadTellWriterT1(dictMonad);
    return {
      listen: v => $0.bind(v)(v1 => $1.pure(Data$dTuple.$Tuple(Data$dTuple.$Tuple(v1._1, v1._2), v1._2))),
      pass: v => $0.bind(v)(v1 => $1.pure(Data$dTuple.$Tuple(v1._1._1, v1._1._2(v1._2)))),
      Monoid0: () => dictMonoid,
      MonadTell1: () => monadTellWriterT2
    };
  };
};
const monadThrowWriterT = dictMonoid => {
  const mempty = dictMonoid.mempty;
  const monadWriterT1 = monadWriterT(dictMonoid);
  return dictMonadThrow => {
    const Monad0 = dictMonadThrow.Monad0();
    const monadWriterT2 = monadWriterT1(Monad0);
    return {throwError: e => Monad0.Bind1().bind(dictMonadThrow.throwError(e))(a => Monad0.Applicative0().pure(Data$dTuple.$Tuple(a, mempty))), Monad0: () => monadWriterT2};
  };
};
const monadErrorWriterT = dictMonoid => {
  const monadThrowWriterT1 = monadThrowWriterT(dictMonoid);
  return dictMonadError => {
    const monadThrowWriterT2 = monadThrowWriterT1(dictMonadError.MonadThrow0());
    return {catchError: v => h => dictMonadError.catchError(v)(e => h(e)), MonadThrow0: () => monadThrowWriterT2};
  };
};
const monoidWriterT = dictApplicative => {
  const $0 = dictApplicative.Apply0();
  return dictMonoid => {
    const $1 = dictMonoid.Semigroup0();
    const Functor0 = $0.Functor0();
    return dictMonoid1 => {
      const $2 = dictMonoid1.Semigroup0();
      const semigroupWriterT3 = {
        append: a => b => $0.apply(Functor0.map(v3 => v4 => Data$dTuple.$Tuple(v3._1(v4._1), $1.append(v3._2)(v4._2)))(Functor0.map(v => Data$dTuple.$Tuple($2.append(v._1), v._2))(a)))(b)
      };
      return {mempty: applicativeWriterT(dictMonoid)(dictApplicative).pure(dictMonoid1.mempty), Semigroup0: () => semigroupWriterT3};
    };
  };
};
const altWriterT = dictAlt => {
  const $0 = dictAlt.Functor0();
  const functorWriterT1 = {map: f => $0.map(v => Data$dTuple.$Tuple(f(v._1), v._2))};
  return {alt: v => v1 => dictAlt.alt(v)(v1), Functor0: () => functorWriterT1};
};
const plusWriterT = dictPlus => {
  const $0 = dictPlus.Alt0();
  const $1 = $0.Functor0();
  const altWriterT1 = (() => {
    const functorWriterT1 = {map: f => $1.map(v => Data$dTuple.$Tuple(f(v._1), v._2))};
    return {alt: v => v1 => $0.alt(v)(v1), Functor0: () => functorWriterT1};
  })();
  return {empty: dictPlus.empty, Alt0: () => altWriterT1};
};
const alternativeWriterT = dictMonoid => {
  const applicativeWriterT1 = applicativeWriterT(dictMonoid);
  return dictAlternative => {
    const applicativeWriterT2 = applicativeWriterT1(dictAlternative.Applicative0());
    const $0 = dictAlternative.Plus1();
    const $1 = $0.Alt0();
    const plusWriterT1 = (() => {
      const $2 = $1.Functor0();
      const functorWriterT1 = {map: f => $2.map(v => Data$dTuple.$Tuple(f(v._1), v._2))};
      const altWriterT1 = {alt: v => v1 => $1.alt(v)(v1), Functor0: () => functorWriterT1};
      return {empty: $0.empty, Alt0: () => altWriterT1};
    })();
    return {Applicative0: () => applicativeWriterT2, Plus1: () => plusWriterT1};
  };
};
const monadPlusWriterT = dictMonoid => {
  const monadWriterT1 = monadWriterT(dictMonoid);
  const alternativeWriterT1 = alternativeWriterT(dictMonoid);
  return dictMonadPlus => {
    const monadWriterT2 = monadWriterT1(dictMonadPlus.Monad0());
    const alternativeWriterT2 = alternativeWriterT1(dictMonadPlus.Alternative1());
    return {Monad0: () => monadWriterT2, Alternative1: () => alternativeWriterT2};
  };
};
export {
  
  
  
  
  
  
  
  
  
  
  
  monadEffectWriter,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
};
