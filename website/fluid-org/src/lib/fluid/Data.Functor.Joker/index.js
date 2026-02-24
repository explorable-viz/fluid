import * as Data$dEither from "../Data.Either/index.js";
const Joker = x => x;
const showJoker = dictShow => ({show: v => "(Joker " + dictShow.show(v) + ")"});
const profunctorJoker = dictFunctor => ({dimap: v => g => v1 => dictFunctor.map(g)(v1)});
const ordJoker = dictOrd => dictOrd;
const newtypeJoker = {Coercible0: () => {}};
const hoistJoker = f => v => f(v);
const functorJoker = dictFunctor => ({map: f => v => dictFunctor.map(f)(v)});
const eqJoker = dictEq => dictEq;
const choiceJoker = dictFunctor => {
  const profunctorJoker1 = {dimap: v => g => v1 => dictFunctor.map(g)(v1)};
  return {left: v => dictFunctor.map(Data$dEither.Left)(v), right: v => dictFunctor.map(Data$dEither.Right)(v), Profunctor0: () => profunctorJoker1};
};
const bifunctorJoker = dictFunctor => ({bimap: v => g => v1 => dictFunctor.map(g)(v1)});
const biapplyJoker = dictApply => {
  const $0 = dictApply.Functor0();
  const bifunctorJoker1 = {bimap: v => g => v1 => $0.map(g)(v1)};
  return {biapply: v => v1 => dictApply.apply(v)(v1), Bifunctor0: () => bifunctorJoker1};
};
const biapplicativeJoker = dictApplicative => {
  const $0 = dictApplicative.Apply0();
  const $1 = $0.Functor0();
  const biapplyJoker1 = (() => {
    const bifunctorJoker1 = {bimap: v => g => v1 => $1.map(g)(v1)};
    return {biapply: v => v1 => $0.apply(v)(v1), Bifunctor0: () => bifunctorJoker1};
  })();
  return {bipure: v => b => dictApplicative.pure(b), Biapply0: () => biapplyJoker1};
};
const applyJoker = dictApply => {
  const $0 = dictApply.Functor0();
  const functorJoker1 = {map: f => v => $0.map(f)(v)};
  return {apply: v => v1 => dictApply.apply(v)(v1), Functor0: () => functorJoker1};
};
const bindJoker = dictBind => {
  const $0 = dictBind.Apply0();
  const $1 = $0.Functor0();
  const applyJoker1 = (() => {
    const functorJoker1 = {map: f => v => $1.map(f)(v)};
    return {apply: v => v1 => $0.apply(v)(v1), Functor0: () => functorJoker1};
  })();
  return {bind: v => amb => dictBind.bind(v)(x => amb(x)), Apply0: () => applyJoker1};
};
const applicativeJoker = dictApplicative => {
  const $0 = dictApplicative.Apply0();
  const $1 = $0.Functor0();
  const applyJoker1 = (() => {
    const functorJoker1 = {map: f => v => $1.map(f)(v)};
    return {apply: v => v1 => $0.apply(v)(v1), Functor0: () => functorJoker1};
  })();
  return {pure: x => dictApplicative.pure(x), Apply0: () => applyJoker1};
};
const monadJoker = dictMonad => {
  const $0 = dictMonad.Applicative0();
  const $1 = $0.Apply0();
  const applicativeJoker1 = (() => {
    const $2 = $1.Functor0();
    const functorJoker1 = {map: f => v => $2.map(f)(v)};
    const applyJoker1 = {apply: v => v1 => $1.apply(v)(v1), Functor0: () => functorJoker1};
    return {pure: x => $0.pure(x), Apply0: () => applyJoker1};
  })();
  const $2 = dictMonad.Bind1();
  const $3 = $2.Apply0();
  const bindJoker1 = (() => {
    const $4 = $3.Functor0();
    const functorJoker1 = {map: f => v => $4.map(f)(v)};
    const applyJoker1 = {apply: v => v1 => $3.apply(v)(v1), Functor0: () => functorJoker1};
    return {bind: v => amb => $2.bind(v)(x => amb(x)), Apply0: () => applyJoker1};
  })();
  return {Applicative0: () => applicativeJoker1, Bind1: () => bindJoker1};
};
export {
  Joker,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
};
