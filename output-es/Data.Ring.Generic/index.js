import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
const genericSub$p = dict => dict["genericSub'"];
const genericSub = dictGeneric => dictGenericRing => x => y => dictGeneric.to(dictGenericRing["genericSub'"](dictGeneric.from(x))(dictGeneric.from(y)));
const genericRingProduct = dictGenericRing => dictGenericRing1 => (
  {"genericSub'": v => v1 => Data$dGeneric$dRep.$Product(dictGenericRing["genericSub'"](v._1)(v1._1), dictGenericRing1["genericSub'"](v._2)(v1._2))}
);
const genericRingNoArguments = {"genericSub'": v => v1 => Data$dGeneric$dRep.NoArguments};
const genericRingConstructor = dictGenericRing => ({"genericSub'": v => v1 => dictGenericRing["genericSub'"](v)(v1)});
const genericRingArgument = dictRing => ({"genericSub'": v => v1 => dictRing.sub(v)(v1)});
export {genericRingArgument, genericRingConstructor, genericRingNoArguments, genericRingProduct, genericSub, genericSub$p};
