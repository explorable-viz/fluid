import * as $runtime from "../runtime.js";
import * as Affjax from "../Affjax/index.js";
import * as Affjax$dResponseFormat from "../Affjax.ResponseFormat/index.js";
import * as Affjax$dWeb from "../Affjax.Web/index.js";
import * as Control$dApply from "../Control.Apply/index.js";
import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dHTTP$dMethod from "../Data.HTTP.Method/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dMonoid from "../Data.Monoid/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
const bindExceptT = /* #__PURE__ */ Control$dMonad$dExcept$dTrans.bindExceptT(Effect$dAff.monadAff);
const applicativeExceptT = /* #__PURE__ */ Control$dMonad$dExcept$dTrans.applicativeExceptT(Effect$dAff.monadAff);
const Folder = x => x;
const FileCxt = x => x;
const File = x => x;
const showFolder = Data$dShow.showString;
const showFile = Data$dShow.showString;
const show = /* #__PURE__ */ Data$dShow.showArrayImpl(Data$dShow.showStringImpl);
const semigroupFolder = {append: v => v1 => v + "/" + v1};
const semigroupFile = Data$dSemigroup.semigroupString;
const newtypeFolder_ = {Coercible0: () => {}};
const newtypeFile_ = {Coercible0: () => {}};
const monoidFile = Data$dMonoid.monoidString;
const loadFileAff = {
  loadFileFromPath: dictMonadError => dictMonadAff => v => Effect$dAff._bind(bindExceptT.bind(dictMonadAff.liftAff(Effect$dAff._bind(Affjax.request(Affjax$dWeb.driver)({
    method: Data$dEither.$Either("Left", Data$dHTTP$dMethod.HEAD),
    url: v,
    headers: [],
    content: Data$dMaybe.Nothing,
    username: Data$dMaybe.Nothing,
    password: Data$dMaybe.Nothing,
    withCredentials: false,
    responseFormat: Affjax$dResponseFormat.$ResponseFormat("String", Affjax$dResponseFormat.identity),
    timeout: Data$dMaybe.Nothing
  }))(resp => Effect$dAff._pure((() => {
    if (resp.tag === "Right") {
      if (resp._1.status === 200) { return Data$dEither.$Either("Right", Data$dTuple.$Tuple(resp._1, v)); }
      return Data$dEither.$Either("Left", Affjax.RequestFailedError);
    }
    if (resp.tag === "Left") { return Data$dEither.$Either("Left", resp._1); }
    $runtime.fail();
  })()))))(v1 => {
    const $0 = v1._2;
    return bindExceptT.bind(applicativeExceptT.pure())(() => bindExceptT.bind(dictMonadAff.liftAff(Affjax.request(Affjax$dWeb.driver)({
      method: Data$dEither.$Either("Left", Data$dHTTP$dMethod.GET),
      url: $0,
      headers: [],
      content: Data$dMaybe.Nothing,
      username: Data$dMaybe.Nothing,
      password: Data$dMaybe.Nothing,
      withCredentials: false,
      responseFormat: Affjax$dResponseFormat.$ResponseFormat("String", Affjax$dResponseFormat.identity),
      timeout: Data$dMaybe.Nothing
    })))(contents => applicativeExceptT.pure(contents.body)));
  }))(result => Effect$dAff._pure((() => {
    if (result.tag === "Left") { return Data$dMaybe.Nothing; }
    if (result.tag === "Right") { return Data$dMaybe.$Maybe("Just", result._1); }
    $runtime.fail();
  })()))
};
const prependFolder = v => v1 => v + "/" + v1;
const loadFileFromPath = dict => dict.loadFileFromPath;
const loadFileStateT = dictMonadAff => {
  const $0 = dictMonadAff.MonadEffect0().Monad0();
  return dictMonadError => dictLoadFile => {
    const loadFileFromPath1 = dictLoadFile.loadFileFromPath(dictMonadError)(dictMonadAff);
    return {
      loadFileFromPath: dictMonadError1 => dictMonadAff1 => x => {
        const $1 = loadFileFromPath1(x);
        return s => $0.Bind1().bind($1)(x$1 => $0.Applicative0().pure(Data$dTuple.$Tuple(x$1, s)));
      }
    };
  };
};
const loadFileWriterT = dictMonoid => {
  const mempty = dictMonoid.mempty;
  return dictMonadError => dictMonadAff => {
    const $0 = dictMonadAff.MonadEffect0().Monad0();
    return dictLoadFile => {
      const loadFileFromPath1 = dictLoadFile.loadFileFromPath(dictMonadError)(dictMonadAff);
      return {loadFileFromPath: dictMonadError1 => dictMonadAff1 => x => $0.Bind1().bind(loadFileFromPath1(x))(a => $0.Applicative0().pure(Data$dTuple.$Tuple(a, mempty)))};
    };
  };
};
const loadFile = dictLoadFile => dictMonad => {
  const $0 = dictMonad.Applicative0();
  return dictMonadError => {
    const loadFileFromPath2 = dictLoadFile.loadFileFromPath(dictMonadError);
    return dictMonadAff => {
      const loadFileFromPath3 = loadFileFromPath2(dictMonadAff);
      return folders => file => {
        const paths = Control$dApply.arrayApply(Data$dFunctor.arrayMap(prependFolder)(folders))([file]);
        return dictMonad.Bind1().bind(Data$dArray.foldM(dictMonad)(v => v1 => {
          if (v.tag === "Just") { return $0.pure(Data$dMaybe.$Maybe("Just", v._1)); }
          if (v.tag === "Nothing") { return loadFileFromPath3(v1); }
          $runtime.fail();
        })(Data$dMaybe.Nothing)(paths))(result => {
          if (result.tag === "Just") { return $0.pure(result._1); }
          if (result.tag === "Nothing") { return Effect$dException.throwException(Effect$dException.error("File not found in any path: " + show(paths)))(); }
          $runtime.fail();
        });
      };
    };
  };
};
const fluidExtension = ".fld";
export {
  
  
  Folder,
  
  
  
  loadFile,
  loadFileAff,
  
  
  
  
  
  
  
  
  
  
  
  
};
