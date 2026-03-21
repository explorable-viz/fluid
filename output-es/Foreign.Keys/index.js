// | This module provides functions for working with object properties
// | of Javascript objects.
import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Foreign from "../Foreign/index.js";
import {unsafeKeys} from "./foreign.js";
const keys = dictMonad => value => {
  if (Foreign.isNull(value)) {
    return Control$dMonad$dExcept$dTrans.monadThrowExceptT(dictMonad).throwError(Data$dNonEmpty.$NonEmpty(
      Foreign.$ForeignError("TypeMismatch", "object", "null"),
      Data$dList$dTypes.Nil
    ));
  }
  if (Foreign.isUndefined(value)) {
    return Control$dMonad$dExcept$dTrans.monadThrowExceptT(dictMonad).throwError(Data$dNonEmpty.$NonEmpty(
      Foreign.$ForeignError("TypeMismatch", "object", "undefined"),
      Data$dList$dTypes.Nil
    ));
  }
  if (Foreign.typeOf(value) === "object") { return Control$dMonad$dExcept$dTrans.applicativeExceptT(dictMonad).pure(unsafeKeys(value)); }
  return Control$dMonad$dExcept$dTrans.monadThrowExceptT(dictMonad).throwError(Data$dNonEmpty.$NonEmpty(
    Foreign.$ForeignError("TypeMismatch", "object", Foreign.typeOf(value)),
    Data$dList$dTypes.Nil
  ));
};
export {keys};
export * from "./foreign.js";
