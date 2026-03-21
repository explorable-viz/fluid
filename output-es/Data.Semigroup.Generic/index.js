import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
const genericSemigroupNoConstructors = {"genericAppend'": a => v => a};
const genericSemigroupNoArguments = {"genericAppend'": a => v => a};
const genericSemigroupArgument = dictSemigroup => ({"genericAppend'": v => v1 => dictSemigroup.append(v)(v1)});
const genericAppend$p = dict => dict["genericAppend'"];
const genericSemigroupConstructor = dictGenericSemigroup => ({"genericAppend'": v => v1 => dictGenericSemigroup["genericAppend'"](v)(v1)});
const genericSemigroupProduct = dictGenericSemigroup => dictGenericSemigroup1 => (
  {"genericAppend'": v => v1 => Data$dGeneric$dRep.$Product(dictGenericSemigroup["genericAppend'"](v._1)(v1._1), dictGenericSemigroup1["genericAppend'"](v._2)(v1._2))}
);
const genericAppend = dictGeneric => dictGenericSemigroup => x => y => dictGeneric.to(dictGenericSemigroup["genericAppend'"](dictGeneric.from(x))(dictGeneric.from(y)));
export {genericAppend, genericAppend$p, genericSemigroupArgument, genericSemigroupConstructor, genericSemigroupNoArguments, genericSemigroupNoConstructors, genericSemigroupProduct};
