import * as Data$dTuple from "../Data.Tuple/index.js";
const Costar = x => x;
const semigroupoidCostar = dictExtend => ({compose: v => v1 => w => v(dictExtend.extend(v1)(w))});
const profunctorCostar = dictFunctor => (
  {
    dimap: f => g => v => {
      const $0 = dictFunctor.map(f);
      return x => g(v($0(x)));
    }
  }
);
const strongCostar = dictComonad => {
  const Functor0 = dictComonad.Extend0().Functor0();
  const profunctorCostar1 = {
    dimap: f => g => v => {
      const $0 = Functor0.map(f);
      return x => g(v($0(x)));
    }
  };
  return {
    first: v => x => Data$dTuple.$Tuple(v(Functor0.map(Data$dTuple.fst)(x)), dictComonad.extract(x)._2),
    second: v => x => Data$dTuple.$Tuple(dictComonad.extract(x)._1, v(Functor0.map(Data$dTuple.snd)(x))),
    Profunctor0: () => profunctorCostar1
  };
};
const newtypeCostar = {Coercible0: () => {}};
const hoistCostar = f => v => x => v(f(x));
const functorCostar = {map: f => v => x => f(v(x))};
const invariantCostar = {imap: f => v => v$1 => x => f(v$1(x))};
const distributiveCostar = {
  distribute: dictFunctor => f => a => dictFunctor.map(v => v(a))(f),
  collect: dictFunctor => f => {
    const $0 = distributiveCostar.distribute(dictFunctor);
    const $1 = dictFunctor.map(f);
    return x => $0($1(x));
  },
  Functor0: () => functorCostar
};
const closedCostar = dictFunctor => {
  const profunctorCostar1 = {
    dimap: f => g => v => {
      const $0 = dictFunctor.map(f);
      return x => g(v($0(x)));
    }
  };
  return {closed: v => g => x => v(dictFunctor.map(v1 => v1(x))(g)), Profunctor0: () => profunctorCostar1};
};
const categoryCostar = dictComonad => {
  const $0 = dictComonad.Extend0();
  const semigroupoidCostar1 = {compose: v => v1 => w => v($0.extend(v1)(w))};
  return {identity: dictComonad.extract, Semigroupoid0: () => semigroupoidCostar1};
};
const bifunctorCostar = dictContravariant => (
  {
    bimap: f => g => v => {
      const $0 = dictContravariant.cmap(f);
      return x => g(v($0(x)));
    }
  }
);
const applyCostar = {apply: v => v1 => a => v(a)(v1(a)), Functor0: () => functorCostar};
const bindCostar = {bind: v => f => x => f(v(x))(x), Apply0: () => applyCostar};
const applicativeCostar = {pure: a => v => a, Apply0: () => applyCostar};
const monadCostar = {Applicative0: () => applicativeCostar, Bind1: () => bindCostar};
export {
  Costar,
  applicativeCostar,
  applyCostar,
  bifunctorCostar,
  bindCostar,
  categoryCostar,
  closedCostar,
  distributiveCostar,
  functorCostar,
  hoistCostar,
  invariantCostar,
  monadCostar,
  newtypeCostar,
  profunctorCostar,
  semigroupoidCostar,
  strongCostar
};
