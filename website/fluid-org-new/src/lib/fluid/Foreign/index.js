// | This module defines types and functions for working with _foreign_
// | data.
// |
// | `ExceptT (NonEmptyList ForeignError) m` is used in this library
// | to encode possible failures when dealing with foreign data.
// |
// | The `Alt` instance for `ExceptT` allows us to accumulate errors,
// | unlike `Either`, which preserves only the last error.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import {isArray, isNull, isUndefined, tagOf, typeOf} from "./foreign.js";
const $ForeignError = (tag, _1, _2) => ({tag, _1, _2});
const ForeignError = value0 => $ForeignError("ForeignError", value0);
const TypeMismatch = value0 => value1 => $ForeignError("TypeMismatch", value0, value1);
const ErrorAtIndex = value0 => value1 => $ForeignError("ErrorAtIndex", value0, value1);
const ErrorAtProperty = value0 => value1 => $ForeignError("ErrorAtProperty", value0, value1);
const unsafeToForeign = Unsafe$dCoerce.unsafeCoerce;
const unsafeFromForeign = Unsafe$dCoerce.unsafeCoerce;
const showForeignError = {
  show: v => {
    if (v.tag === "ForeignError") { return "(ForeignError " + Data$dShow.showStringImpl(v._1) + ")"; }
    if (v.tag === "ErrorAtIndex") { return "(ErrorAtIndex " + Data$dShow.showIntImpl(v._1) + " " + showForeignError.show(v._2) + ")"; }
    if (v.tag === "ErrorAtProperty") { return "(ErrorAtProperty " + Data$dShow.showStringImpl(v._1) + " " + showForeignError.show(v._2) + ")"; }
    if (v.tag === "TypeMismatch") { return "(TypeMismatch " + Data$dShow.showStringImpl(v._1) + " " + Data$dShow.showStringImpl(v._2) + ")"; }
    $runtime.fail();
  }
};
const renderForeignError = v => {
  if (v.tag === "ForeignError") { return v._1; }
  if (v.tag === "ErrorAtIndex") { return "Error at array index " + Data$dShow.showIntImpl(v._1) + ": " + renderForeignError(v._2); }
  if (v.tag === "ErrorAtProperty") { return "Error at property " + Data$dShow.showStringImpl(v._1) + ": " + renderForeignError(v._2); }
  if (v.tag === "TypeMismatch") { return "Type mismatch: expected " + v._1 + ", found " + v._2; }
  $runtime.fail();
};
const readUndefined = dictMonad => {
  const $0 = Control$dMonad$dExcept$dTrans.applicativeExceptT(dictMonad);
  return value => {
    if (isUndefined(value)) { return $0.pure(Data$dMaybe.Nothing); }
    return $0.pure(Data$dMaybe.$Maybe("Just", value));
  };
};
const readNullOrUndefined = dictMonad => {
  const $0 = Control$dMonad$dExcept$dTrans.applicativeExceptT(dictMonad);
  return value => {
    if (isNull(value) || isUndefined(value)) { return $0.pure(Data$dMaybe.Nothing); }
    return $0.pure(Data$dMaybe.$Maybe("Just", value));
  };
};
const readNull = dictMonad => {
  const $0 = Control$dMonad$dExcept$dTrans.applicativeExceptT(dictMonad);
  return value => {
    if (isNull(value)) { return $0.pure(Data$dMaybe.Nothing); }
    return $0.pure(Data$dMaybe.$Maybe("Just", value));
  };
};
const fail = dictMonad => x => Control$dMonad$dExcept$dTrans.monadThrowExceptT(dictMonad).throwError(Data$dNonEmpty.$NonEmpty(x, Data$dList$dTypes.Nil));
const readArray = dictMonad => value => {
  if (isArray(value)) { return Control$dMonad$dExcept$dTrans.applicativeExceptT(dictMonad).pure(value); }
  return Control$dMonad$dExcept$dTrans.monadThrowExceptT(dictMonad).throwError(Data$dNonEmpty.$NonEmpty($ForeignError("TypeMismatch", "array", tagOf(value)), Data$dList$dTypes.Nil));
};
const unsafeReadTagged = dictMonad => tag => value => {
  if (tagOf(value) === tag) { return Control$dMonad$dExcept$dTrans.applicativeExceptT(dictMonad).pure(value); }
  return Control$dMonad$dExcept$dTrans.monadThrowExceptT(dictMonad).throwError(Data$dNonEmpty.$NonEmpty($ForeignError("TypeMismatch", tag, tagOf(value)), Data$dList$dTypes.Nil));
};
const readBoolean = dictMonad => unsafeReadTagged(dictMonad)("Boolean");
const readNumber = dictMonad => unsafeReadTagged(dictMonad)("Number");
const readInt = dictMonad => value => {
  const error = Data$dEither.$Either("Left", Data$dNonEmpty.$NonEmpty($ForeignError("TypeMismatch", "Int", tagOf(value)), Data$dList$dTypes.Nil));
  return dictMonad.Bind1().Apply0().Functor0().map(v2 => {
    if (v2.tag === "Left") { return error; }
    if (v2.tag === "Right") {
      const $0 = Data$dInt.fromNumber(v2._1);
      if ($0.tag === "Nothing") { return error; }
      if ($0.tag === "Just") { return Data$dEither.$Either("Right", $0._1); }
    }
    $runtime.fail();
  })(unsafeReadTagged(dictMonad)("Number")(value));
};
const readString = dictMonad => unsafeReadTagged(dictMonad)("String");
const readChar = dictMonad => value => {
  const error = Data$dEither.$Either("Left", Data$dNonEmpty.$NonEmpty($ForeignError("TypeMismatch", "Char", tagOf(value)), Data$dList$dTypes.Nil));
  return dictMonad.Bind1().Apply0().Functor0().map(v2 => {
    if (v2.tag === "Left") { return error; }
    if (v2.tag === "Right") {
      const $0 = Data$dString$dCodeUnits.toChar(v2._1);
      if ($0.tag === "Nothing") { return error; }
      if ($0.tag === "Just") { return Data$dEither.$Either("Right", $0._1); }
    }
    $runtime.fail();
  })(unsafeReadTagged(dictMonad)("String")(value));
};
const eqForeignError = {
  eq: x => y => {
    if (x.tag === "ForeignError") { return y.tag === "ForeignError" && x._1 === y._1; }
    if (x.tag === "TypeMismatch") { return y.tag === "TypeMismatch" && x._1 === y._1 && x._2 === y._2; }
    if (x.tag === "ErrorAtIndex") { return y.tag === "ErrorAtIndex" && x._1 === y._1 && eqForeignError.eq(x._2)(y._2); }
    return x.tag === "ErrorAtProperty" && y.tag === "ErrorAtProperty" && x._1 === y._1 && eqForeignError.eq(x._2)(y._2);
  }
};
const ordForeignError = {
  compare: x => y => {
    if (x.tag === "ForeignError") {
      if (y.tag === "ForeignError") { return Data$dOrd.ordString.compare(x._1)(y._1); }
      return Data$dOrdering.LT;
    }
    if (y.tag === "ForeignError") { return Data$dOrdering.GT; }
    if (x.tag === "TypeMismatch") {
      if (y.tag === "TypeMismatch") {
        const v = Data$dOrd.ordString.compare(x._1)(y._1);
        if (v === "LT") { return Data$dOrdering.LT; }
        if (v === "GT") { return Data$dOrdering.GT; }
        return Data$dOrd.ordString.compare(x._2)(y._2);
      }
      return Data$dOrdering.LT;
    }
    if (y.tag === "TypeMismatch") { return Data$dOrdering.GT; }
    if (x.tag === "ErrorAtIndex") {
      if (y.tag === "ErrorAtIndex") {
        const v = Data$dOrd.ordInt.compare(x._1)(y._1);
        if (v === "LT") { return Data$dOrdering.LT; }
        if (v === "GT") { return Data$dOrdering.GT; }
        return ordForeignError.compare(x._2)(y._2);
      }
      return Data$dOrdering.LT;
    }
    if (y.tag === "ErrorAtIndex") { return Data$dOrdering.GT; }
    if (x.tag === "ErrorAtProperty" && y.tag === "ErrorAtProperty") {
      const v = Data$dOrd.ordString.compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      return ordForeignError.compare(x._2)(y._2);
    }
    $runtime.fail();
  },
  Eq0: () => eqForeignError
};
export {
  $ForeignError,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  renderForeignError,
  
  
  unsafeReadTagged,
  
};
export * from "./foreign.js";
