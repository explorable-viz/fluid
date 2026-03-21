import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Record$dUnsafe$dUnion from "../Record.Unsafe.Union/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
const union = () => l => r => Record$dUnsafe$dUnion.unsafeUnionFn(l, r);
const $$set = dictIsSymbol => () => () => l => b => r => Record$dUnsafe.unsafeSet(dictIsSymbol.reflectSymbol(l))(b)(r);
const nub = () => Unsafe$dCoerce.unsafeCoerce;
const merge = () => () => l => r => Record$dUnsafe$dUnion.unsafeUnionFn(l, r);
const insert = dictIsSymbol => () => () => l => a => r => Record$dUnsafe.unsafeSet(dictIsSymbol.reflectSymbol(l))(a)(r);
const $$get = dictIsSymbol => () => l => r => Record$dUnsafe.unsafeGet(dictIsSymbol.reflectSymbol(l))(r);
const modify = dictIsSymbol => () => () => l => f => r => Record$dUnsafe.unsafeSet(dictIsSymbol.reflectSymbol(l))(f(Record$dUnsafe.unsafeGet(dictIsSymbol.reflectSymbol(l))(r)))(r);
const equalFieldsNil = {equalFields: v => v1 => v2 => true};
const equalFields = dict => dict.equalFields;
const equalFieldsCons = dictIsSymbol => dictEq => () => dictEqualFields => (
  {
    equalFields: v => a => b => dictEq.eq(Record$dUnsafe.unsafeGet(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy))(a))(Record$dUnsafe.unsafeGet(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy))(b)) && dictEqualFields.equalFields(Type$dProxy.Proxy)(a)(b)
  }
);
const equal = () => dictEqualFields => a => b => dictEqualFields.equalFields(Type$dProxy.Proxy)(a)(b);
const disjointUnion = () => () => l => r => Record$dUnsafe$dUnion.unsafeUnionFn(l, r);
const $$delete = dictIsSymbol => () => () => l => r => Record$dUnsafe.unsafeDelete(dictIsSymbol.reflectSymbol(l))(r);
const rename = dictIsSymbol => dictIsSymbol1 => () => () => () => () => prev => next => record => Record$dUnsafe.unsafeSet(dictIsSymbol1.reflectSymbol(next))(Record$dUnsafe.unsafeGet(dictIsSymbol.reflectSymbol(prev))(record))(Record$dUnsafe.unsafeDelete(dictIsSymbol.reflectSymbol(prev))(record));
export {$$delete as delete, disjointUnion, equal, equalFields, equalFieldsCons, equalFieldsNil, $$get as get, insert, merge, modify, nub, rename, $$set as set, union};
