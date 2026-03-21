import * as Control$dMonad$dError$dClass from "../Control.Monad.Error.Class/index.js";
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Effect$dAff$dClass from "../Effect.Aff.Class/index.js";
import * as Node$dEncoding from "../Node.Encoding/index.js";
import * as Node$dFS$dAff from "../Node.FS.Aff/index.js";
import * as Node$dFS$dAsync from "../Node.FS.Async/index.js";
import * as Node$dFS$dStats from "../Node.FS.Stats/index.js";
const $$try = /* #__PURE__ */ Control$dMonad$dError$dClass.try(Effect$dAff.monadErrorAff);
const NodeT = x => x;
const monadTransNodeT = {lift: dictMonad => m => v => m};
const monadThrowErrorNodeT = dictMonadThrow => Control$dMonad$dReader$dTrans.monadThrowReaderT(dictMonadThrow);
const monadReaderFileCxtNodeT = dictMonad => Control$dMonad$dReader$dTrans.monadReaderReaderT(dictMonad);
const monadNodeT = dictMonad => Control$dMonad$dReader$dTrans.monadReaderT(dictMonad);
const monadErrorErrorNodeT = dictMonadError => Control$dMonad$dReader$dTrans.monadErrorReaderT(dictMonadError);
const monadEffectNodeT = dictMonadEffect => Control$dMonad$dReader$dTrans.monadEffectReader(dictMonadEffect);
const monadAskFileCxtNodeT = dictMonadAsk => {
  const $0 = dictMonadAsk.Monad0();
  const monadReaderT1 = Control$dMonad$dReader$dTrans.monadReaderT($0);
  return {ask: $0.Applicative0().pure, Monad0: () => monadReaderT1};
};
const monadAffNodeT = dictMonadAff => Effect$dAff$dClass.monadAffReader(dictMonadAff);
const functorNodeT = dictFunctor => (
  {
    map: x => {
      const $0 = dictFunctor.map(x);
      return v => x$1 => $0(v(x$1));
    }
  }
);
const bindNodeT = dictBind => Control$dMonad$dReader$dTrans.bindReaderT(dictBind);
const applyNodeT = dictApply => {
  const $0 = dictApply.Functor0();
  const functorReaderT1 = {
    map: x => {
      const $1 = $0.map(x);
      return v => x$1 => $1(v(x$1));
    }
  };
  return {apply: v => v1 => r => dictApply.apply(v(r))(v1(r)), Functor0: () => functorReaderT1};
};
const applicativeNodeT = dictApplicative => {
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
const loadFileNodeT = dictMonad => {
  const Bind1 = dictMonad.Bind1();
  const $0 = Bind1.Apply0().Functor0();
  const $1 = dictMonad.Applicative0();
  return {
    loadFileFromPath: dictMonadError => dictMonadAff => v => Control$dMonad$dReader$dTrans.bindReaderT(Bind1).bind(dictMonadAff.liftAff($$try(Node$dFS$dAff.toAff1(Node$dFS$dAsync.stat)(v))))(stats => {
      if (stats.tag === "Right" && Node$dFS$dStats.isFileImpl(stats._1)) {
        const $2 = $0.map(Data$dMaybe.Just);
        const $3 = dictMonadAff.liftAff(Node$dFS$dAff.toAff2(Node$dFS$dAsync.readTextFile)(Node$dEncoding.UTF8)(v));
        return x => $2($3(x));
      }
      const $2 = $1.pure(Data$dMaybe.Nothing);
      return v$1 => $2;
    })
  };
};
const runNodeT = fileCxt => v => v(fileCxt);
export {
  NodeT,
  applicativeNodeT,
  applyNodeT,
  bindNodeT,
  functorNodeT,
  loadFileNodeT,
  monadAffNodeT,
  monadAskFileCxtNodeT,
  monadEffectNodeT,
  monadErrorErrorNodeT,
  monadNodeT,
  monadReaderFileCxtNodeT,
  monadThrowErrorNodeT,
  monadTransNodeT,
  runNodeT,
  $$try as try
};
