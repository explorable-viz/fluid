import * as $runtime from "../runtime.js";
import * as App$dFig from "../App.Fig/index.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dView$dUtil from "../App.View.Util/index.js";
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dArgonaut$dDecode$dClass from "../Data.Argonaut.Decode.Class/index.js";
import * as Data$dArgonaut$dDecode$dDecoders from "../Data.Argonaut.Decode.Decoders/index.js";
import * as Data$dArgonaut$dDecode$dError from "../Data.Argonaut.Decode.Error/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Effect$dAff$dClass from "../Effect.Aff.Class/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as File from "../File/index.js";
import * as Util from "../Util/index.js";
import * as Val from "../Val/index.js";
const gDecodeJsonCons = /* #__PURE__ */ Data$dArgonaut$dDecode$dClass.gDecodeJsonCons(/* #__PURE__ */ (() => {
  const $0 = Data$dArgonaut$dDecode$dDecoders.decodeArray(Data$dArgonaut$dCore.caseJsonString(Data$dEither.$Either(
    "Left",
    Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")
  ))(Data$dEither.Right));
  return {
    decodeJsonField: j => {
      if (j.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0(j._1)); }
      return Data$dMaybe.Nothing;
    }
  };
})());
const gDecodeJsonCons1 = /* #__PURE__ */ Data$dArgonaut$dDecode$dClass.gDecodeJsonCons({
  decodeJsonField: j => {
    if (j.tag === "Just") {
      return Data$dMaybe.$Maybe(
        "Just",
        Data$dArgonaut$dCore._caseJson(
          v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Boolean")),
          Data$dEither.Right,
          v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Boolean")),
          v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Boolean")),
          v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Boolean")),
          v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Boolean")),
          j._1
        )
      );
    }
    return Data$dMaybe.Nothing;
  }
});
const decodeJson = /* #__PURE__ */ (() => Data$dArgonaut$dDecode$dClass.decodeRecord(gDecodeJsonCons(gDecodeJsonCons(gDecodeJsonCons1(gDecodeJsonCons1(Data$dArgonaut$dDecode$dClass.gDecodeJsonCons(Data$dArgonaut$dDecode$dClass.decodeFieldMaybe(App$dView$dUtil.decodeJsonFilter))(Data$dArgonaut$dDecode$dClass.gDecodeJsonNil)({
  reflectSymbol: () => "rowFilter"
})()())({reflectSymbol: () => "query"})()())({reflectSymbol: () => "linking"})()())({reflectSymbol: () => "inputs"})()())({reflectSymbol: () => "fluidSrcPath"})()())().decodeJson)();
const loadFig = /* #__PURE__ */ App$dFig.loadFig(/* #__PURE__ */ Effect$dAff$dClass.monadAffReader(Effect$dAff$dClass.monadAffAff))(/* #__PURE__ */ Control$dMonad$dReader$dTrans.monadErrorReaderT(Effect$dAff.monadErrorAff))(/* #__PURE__ */ Control$dMonad$dReader$dTrans.monadReaderReaderT(Effect$dAff.monadAff))({
  loadFileFromPath: dictMonadError1 => dictMonadAff1 => x => {
    const $0 = File.loadFileAff.loadFileFromPath(Effect$dAff.monadErrorAff)(Effect$dAff$dClass.monadAffAff)(x);
    return v => $0;
  }
});
const optionsFromJson = v => (
  {
    fluidSrcPaths: Data$dFunctor.arrayMap(File.Folder)(v.fluidSrcPath),
    inputs: v.inputs,
    query: v.query
      ? Data$dMaybe.$Maybe(
          "Just",
          a => {
            const $0 = Val.asVal(a);
            if ($0.tag === "Just") {
              if ($0._1._2.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($0._1._1, $0._1)); }
              return Data$dMaybe.Nothing;
            }
            if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
            $runtime.fail();
          }
        )
      : Data$dMaybe.Nothing,
    linking: v.linking,
    rowFilter: v.rowFilter
  }
);
const loadFigureSrc = options => divId => fluidSrc => App$dUtil.runAffs_(v => App$dFig.drawFig(v._1)(v._2))([
  (() => {
    const v = decodeJson(options);
    if (v.tag === "Left") {
      return Effect$dException.throwException(Effect$dException.error("JSON decoding failed with " + Data$dArgonaut$dDecode$dError.showJsonDecodeError.show(v._1)))();
    }
    if (v.tag === "Right") {
      const v1 = optionsFromJson(v._1);
      return Effect$dAff._map(v2 => Data$dTuple.$Tuple(divId, v2))(loadFig(v1)(fluidSrc)({fluidSrcPaths: v1.fluidSrcPaths}));
    }
    $runtime.fail();
  })()
]);
const loadFigure = jsonSpec => divId => srcFile => {
  const $0 = Effect$dAff._makeFiber(
    Effect$dAff.ffiUtil,
    Effect$dAff._bind(File.loadFileAff.loadFileFromPath(Effect$dAff.monadErrorAff)(Effect$dAff$dClass.monadAffAff)(srcFile))(fluidSrc => Effect$dAff._liftEffect(loadFigureSrc(jsonSpec)(divId)(Util.definitely("absurd")(fluidSrc))))
  );
  return () => {
    const fiber = $0();
    fiber.run();
  };
};
const loadCode = file => {
  const filename = Util.definitely("loadCode: Filename cannot be empty: " + file)((() => {
    const $0 = Data$dString$dCommon.split("/")(file);
    const $1 = $0.length - 1 | 0;
    if ($1 >= 0 && $1 < $0.length) {
      const $2 = Data$dString$dCommon.split(".")($0[$1]);
      if (0 < $2.length) {
        if ($2[0] === "") { return Data$dMaybe.Nothing; }
        return Data$dMaybe.$Maybe("Just", $2[0]);
      }
    }
    return Data$dMaybe.Nothing;
  })());
  const $0 = Effect$dAff._makeFiber(
    Effect$dAff.ffiUtil,
    Effect$dAff._bind(File.loadFileAff.loadFileFromPath(Effect$dAff.monadErrorAff)(Effect$dAff$dClass.monadAffAff)(file))(fluidSrc => Effect$dAff._liftEffect(App$dFig.drawFile(Data$dTuple.$Tuple(
      filename,
      Util.definitely("loadCode: File not found: " + file)(fluidSrc)
    ))))
  );
  return () => {
    const fiber = $0();
    fiber.run();
  };
};
export {   loadCode,  loadFigure, loadFigureSrc, };
