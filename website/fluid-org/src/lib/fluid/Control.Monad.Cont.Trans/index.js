// | This module defines the CPS monad transformer.
const ContT = x => x;
const withContT = f => v => k => v(f(k));
const runContT = v => k => v(k);
const newtypeContT = {Coercible0: () => {}};
const monadTransContT = {lift: dictMonad => dictMonad.Bind1().bind};
const mapContT = f => v => k => f(v(k));
const functorContT = dictFunctor => ({map: f => v => k => v(a => k(f(a)))});
const applyContT = dictApply => {
  const functorContT1 = {map: f => v => k => v(a => k(f(a)))};
  return {apply: v => v1 => k => v(g => v1(a => k(g(a)))), Functor0: () => functorContT1};
};
const bindContT = dictBind => {
  const functorContT1 = {map: f => v => k => v(a => k(f(a)))};
  const applyContT1 = {apply: v => v1 => k => v(g => v1(a => k(g(a)))), Functor0: () => functorContT1};
  return {bind: v => k => k$p => v(a => k(a)(k$p)), Apply0: () => applyContT1};
};
const semigroupContT = dictApply => dictSemigroup => (
  {
    append: a => b => k => a(a$1 => {
      const $0 = dictSemigroup.append(a$1);
      return b(a$2 => k($0(a$2)));
    })
  }
);
const applicativeContT = dictApplicative => {
  const functorContT1 = {map: f => v => k => v(a => k(f(a)))};
  const applyContT1 = {apply: v => v1 => k => v(g => v1(a => k(g(a)))), Functor0: () => functorContT1};
  return {pure: a => k => k(a), Apply0: () => applyContT1};
};
const monadContT = dictMonad => {
  const functorContT1 = {map: f => v => k => v(a => k(f(a)))};
  const applicativeContT1 = (() => {
    const applyContT1 = {apply: v => v1 => k => v(g => v1(a => k(g(a)))), Functor0: () => functorContT1};
    return {pure: a => k => k(a), Apply0: () => applyContT1};
  })();
  const functorContT1$1 = {map: f => v => k => v(a => k(f(a)))};
  const bindContT1 = (() => {
    const applyContT1 = {apply: v => v1 => k => v(g => v1(a => k(g(a)))), Functor0: () => functorContT1$1};
    return {bind: v => k => k$p => v(a => k(a)(k$p)), Apply0: () => applyContT1};
  })();
  return {Applicative0: () => applicativeContT1, Bind1: () => bindContT1};
};
const monadAskContT = dictMonadAsk => {
  const Monad0 = dictMonadAsk.Monad0();
  const monadContT1 = monadContT(Monad0);
  return {ask: Monad0.Bind1().bind(dictMonadAsk.ask), Monad0: () => monadContT1};
};
const monadReaderContT = dictMonadReader => {
  const MonadAsk0 = dictMonadReader.MonadAsk0();
  const ask = MonadAsk0.ask;
  const monadAskContT1 = monadAskContT(MonadAsk0);
  return {
    local: f => v => k => MonadAsk0.Monad0().Bind1().bind(ask)(r => dictMonadReader.local(f)(v((() => {
      const $0 = dictMonadReader.local(v$1 => r);
      return x => $0(k(x));
    })()))),
    MonadAsk0: () => monadAskContT1
  };
};
const monadContContT = dictMonad => {
  const monadContT1 = monadContT(dictMonad);
  return {callCC: f => k => f(a => v1 => k(a))(k), Monad0: () => monadContT1};
};
const monadEffectContT = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const monadContT1 = monadContT(Monad0);
  return {
    liftEffect: (() => {
      const $0 = Monad0.Bind1().bind;
      return x => $0(dictMonadEffect.liftEffect(x));
    })(),
    Monad0: () => monadContT1
  };
};
const monadStateContT = dictMonadState => {
  const Monad0 = dictMonadState.Monad0();
  const monadContT1 = monadContT(Monad0);
  return {
    state: (() => {
      const $0 = Monad0.Bind1().bind;
      return x => $0(dictMonadState.state(x));
    })(),
    Monad0: () => monadContT1
  };
};
const monoidContT = dictApplicative => dictMonoid => {
  const $0 = dictMonoid.Semigroup0();
  const semigroupContT2 = {
    append: a => b => k => a(a$1 => {
      const $1 = $0.append(a$1);
      return b(a$2 => k($1(a$2)));
    })
  };
  return {
    mempty: (() => {
      const $1 = dictMonoid.mempty;
      return k => k($1);
    })(),
    Semigroup0: () => semigroupContT2
  };
};
export {
  
  
  
  
  
  
  
  
  
  monadEffectContT,
  
  
  
  
  
  
  
  
};
