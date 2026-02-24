import * as Data$dVoid from "../Data.Void/index.js";
import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
import {showArrayImpl, showCharImpl, showIntImpl, showNumberImpl, showStringImpl} from "./foreign.js";
const showVoid = {show: Data$dVoid.absurd};
const showUnit = {show: v => "unit"};
const showString = {show: showStringImpl};
const showRecordFieldsNil = {showRecordFields: v => v1 => ""};
const showRecordFields = dict => dict.showRecordFields;
const showRecord = () => () => dictShowRecordFields => ({show: record => "{" + dictShowRecordFields.showRecordFields(Type$dProxy.Proxy)(record) + "}"});
const showProxy = {show: v => "Proxy"};
const showNumber = {show: showNumberImpl};
const showInt = {show: showIntImpl};
const showChar = {show: showCharImpl};
const showBoolean = {
  show: v => {
    if (v) { return "true"; }
    return "false";
  }
};
const show = dict => dict.show;
const showArray = dictShow => ({show: showArrayImpl(dictShow.show)});
const showRecordFieldsCons = dictIsSymbol => dictShowRecordFields => dictShow => (
  {
    showRecordFields: v => record => {
      const key = dictIsSymbol.reflectSymbol(Type$dProxy.Proxy);
      return " " + key + ": " + dictShow.show(Record$dUnsafe.unsafeGet(key)(record)) + "," + dictShowRecordFields.showRecordFields(Type$dProxy.Proxy)(record);
    }
  }
);
const showRecordFieldsConsNil = dictIsSymbol => dictShow => (
  {
    showRecordFields: v => record => {
      const key = dictIsSymbol.reflectSymbol(Type$dProxy.Proxy);
      return " " + key + ": " + dictShow.show(Record$dUnsafe.unsafeGet(key)(record)) + " ";
    }
  }
);
export {
  
  
  showBoolean,
  
  showInt,
  
  
  
  
  
  
  
  showString,
  
  
};
export * from "./foreign.js";
