import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
const genericZero$p = dict => dict["genericZero'"];
const genericZero = dictGeneric => dictGenericSemiring => dictGeneric.to(dictGenericSemiring["genericZero'"]);
const genericSemiringNoArguments = {
  "genericAdd'": v => v1 => Data$dGeneric$dRep.NoArguments,
  "genericZero'": Data$dGeneric$dRep.NoArguments,
  "genericMul'": v => v1 => Data$dGeneric$dRep.NoArguments,
  "genericOne'": Data$dGeneric$dRep.NoArguments
};
const genericSemiringArgument = dictSemiring => (
  {"genericAdd'": v => v1 => dictSemiring.add(v)(v1), "genericZero'": dictSemiring.zero, "genericMul'": v => v1 => dictSemiring.mul(v)(v1), "genericOne'": dictSemiring.one}
);
const genericOne$p = dict => dict["genericOne'"];
const genericOne = dictGeneric => dictGenericSemiring => dictGeneric.to(dictGenericSemiring["genericOne'"]);
const genericMul$p = dict => dict["genericMul'"];
const genericMul = dictGeneric => dictGenericSemiring => x => y => dictGeneric.to(dictGenericSemiring["genericMul'"](dictGeneric.from(x))(dictGeneric.from(y)));
const genericAdd$p = dict => dict["genericAdd'"];
const genericSemiringConstructor = dictGenericSemiring => (
  {
    "genericAdd'": v => v1 => dictGenericSemiring["genericAdd'"](v)(v1),
    "genericZero'": dictGenericSemiring["genericZero'"],
    "genericMul'": v => v1 => dictGenericSemiring["genericMul'"](v)(v1),
    "genericOne'": dictGenericSemiring["genericOne'"]
  }
);
const genericSemiringProduct = dictGenericSemiring => {
  const genericZero$p1 = dictGenericSemiring["genericZero'"];
  const genericOne$p1 = dictGenericSemiring["genericOne'"];
  return dictGenericSemiring1 => (
    {
      "genericAdd'": v => v1 => Data$dGeneric$dRep.$Product(dictGenericSemiring["genericAdd'"](v._1)(v1._1), dictGenericSemiring1["genericAdd'"](v._2)(v1._2)),
      "genericZero'": Data$dGeneric$dRep.$Product(genericZero$p1, dictGenericSemiring1["genericZero'"]),
      "genericMul'": v => v1 => Data$dGeneric$dRep.$Product(dictGenericSemiring["genericMul'"](v._1)(v1._1), dictGenericSemiring1["genericMul'"](v._2)(v1._2)),
      "genericOne'": Data$dGeneric$dRep.$Product(genericOne$p1, dictGenericSemiring1["genericOne'"])
    }
  );
};
const genericAdd = dictGeneric => dictGenericSemiring => x => y => dictGeneric.to(dictGenericSemiring["genericAdd'"](dictGeneric.from(x))(dictGeneric.from(y)));
export {
  genericAdd,
  genericAdd$p,
  genericMul,
  genericMul$p,
  genericOne,
  genericOne$p,
  genericSemiringArgument,
  genericSemiringConstructor,
  genericSemiringNoArguments,
  genericSemiringProduct,
  genericZero,
  genericZero$p
};
