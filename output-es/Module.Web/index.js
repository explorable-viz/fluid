import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Effect$dAff$dClass from "../Effect.Aff.Class/index.js";
const WebT = x => x;
const monadWebT = dictMonad => Control$dMonad$dReader$dTrans.monadReaderT(dictMonad);
const monadTransWebT = {lift: dictMonad => m => v => m};
const monadThrowErrorWebT = dictMonadThrow => Control$dMonad$dReader$dTrans.monadThrowReaderT(dictMonadThrow);
const monadReaderFileCxtWebT = dictMonad => Control$dMonad$dReader$dTrans.monadReaderReaderT(dictMonad);
const monadErrorErrorWebT = dictMonadError => Control$dMonad$dReader$dTrans.monadErrorReaderT(dictMonadError);
const monadEffectWebT = dictMonadEffect => Control$dMonad$dReader$dTrans.monadEffectReader(dictMonadEffect);
const monadAskFileCxtWebT = dictMonadAsk => {
  const $0 = dictMonadAsk.Monad0();
  const monadReaderT1 = Control$dMonad$dReader$dTrans.monadReaderT($0);
  return {ask: $0.Applicative0().pure, Monad0: () => monadReaderT1};
};
const monadAffWebT = dictMonadAff => Effect$dAff$dClass.monadAffReader(dictMonadAff);
const loadFileWebT = dictMonadAff => dictMonadError => dictLoadFile => {
  const loadFileFromPath = dictLoadFile.loadFileFromPath(dictMonadError)(dictMonadAff);
  return {
    loadFileFromPath: dictMonadError1 => dictMonadAff1 => x => {
      const $0 = loadFileFromPath(x);
      return v => $0;
    }
  };
};
const functorWebT = dictFunctor => (
  {
    map: x => {
      const $0 = dictFunctor.map(x);
      return v => x$1 => $0(v(x$1));
    }
  }
);
const bindWebT = dictBind => Control$dMonad$dReader$dTrans.bindReaderT(dictBind);
const applyWebT = dictApply => {
  const $0 = dictApply.Functor0();
  const functorReaderT1 = {
    map: x => {
      const $1 = $0.map(x);
      return v => x$1 => $1(v(x$1));
    }
  };
  return {apply: v => v1 => r => dictApply.apply(v(r))(v1(r)), Functor0: () => functorReaderT1};
};
const applicativeWebT = dictApplicative => {
  const $0 = dictApplicative.Apply0();
  const $1 = $0.Functor0();
  const functorReaderT1 = {
    map: x => {
      const $2 = $1.map(x);
      return v => x$1 => $2(v(x$1));
    }
  };
  const applyReaderT1 = {apply: v => v1 => r => $0.apply(v(r))(v1(r)), Functor0: () => functorReaderT1};
  return {
    pure: x => {
      const $2 = dictApplicative.pure(x);
      return v => $2;
    },
    Apply0: () => applyReaderT1
  };
};
const runWebT = fileCxt => v => v(fileCxt);
export {
  WebT,
  applicativeWebT,
  applyWebT,
  bindWebT,
  functorWebT,
  loadFileWebT,
  monadAffWebT,
  monadAskFileCxtWebT,
  monadEffectWebT,
  monadErrorErrorWebT,
  monadReaderFileCxtWebT,
  monadThrowErrorWebT,
  monadTransWebT,
  monadWebT,
  runWebT
};
