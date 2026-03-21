import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
const genericTT$p = dict => dict["genericTT'"];
const genericTT = dictGeneric => dictGenericHeytingAlgebra => dictGeneric.to(dictGenericHeytingAlgebra["genericTT'"]);
const genericNot$p = dict => dict["genericNot'"];
const genericNot = dictGeneric => dictGenericHeytingAlgebra => x => dictGeneric.to(dictGenericHeytingAlgebra["genericNot'"](dictGeneric.from(x)));
const genericImplies$p = dict => dict["genericImplies'"];
const genericImplies = dictGeneric => dictGenericHeytingAlgebra => x => y => dictGeneric.to(dictGenericHeytingAlgebra["genericImplies'"](dictGeneric.from(x))(dictGeneric.from(y)));
const genericHeytingAlgebraNoArguments = {
  "genericFF'": Data$dGeneric$dRep.NoArguments,
  "genericTT'": Data$dGeneric$dRep.NoArguments,
  "genericImplies'": v => v1 => Data$dGeneric$dRep.NoArguments,
  "genericConj'": v => v1 => Data$dGeneric$dRep.NoArguments,
  "genericDisj'": v => v1 => Data$dGeneric$dRep.NoArguments,
  "genericNot'": v => Data$dGeneric$dRep.NoArguments
};
const genericHeytingAlgebraArgument = dictHeytingAlgebra => (
  {
    "genericFF'": dictHeytingAlgebra.ff,
    "genericTT'": dictHeytingAlgebra.tt,
    "genericImplies'": v => v1 => dictHeytingAlgebra.implies(v)(v1),
    "genericConj'": v => v1 => dictHeytingAlgebra.conj(v)(v1),
    "genericDisj'": v => v1 => dictHeytingAlgebra.disj(v)(v1),
    "genericNot'": v => dictHeytingAlgebra.not(v)
  }
);
const genericFF$p = dict => dict["genericFF'"];
const genericFF = dictGeneric => dictGenericHeytingAlgebra => dictGeneric.to(dictGenericHeytingAlgebra["genericFF'"]);
const genericDisj$p = dict => dict["genericDisj'"];
const genericDisj = dictGeneric => dictGenericHeytingAlgebra => x => y => dictGeneric.to(dictGenericHeytingAlgebra["genericDisj'"](dictGeneric.from(x))(dictGeneric.from(y)));
const genericConj$p = dict => dict["genericConj'"];
const genericHeytingAlgebraConstructor = dictGenericHeytingAlgebra => (
  {
    "genericFF'": dictGenericHeytingAlgebra["genericFF'"],
    "genericTT'": dictGenericHeytingAlgebra["genericTT'"],
    "genericImplies'": v => v1 => dictGenericHeytingAlgebra["genericImplies'"](v)(v1),
    "genericConj'": v => v1 => dictGenericHeytingAlgebra["genericConj'"](v)(v1),
    "genericDisj'": v => v1 => dictGenericHeytingAlgebra["genericDisj'"](v)(v1),
    "genericNot'": v => dictGenericHeytingAlgebra["genericNot'"](v)
  }
);
const genericHeytingAlgebraProduct = dictGenericHeytingAlgebra => {
  const genericFF$p1 = dictGenericHeytingAlgebra["genericFF'"];
  const genericTT$p1 = dictGenericHeytingAlgebra["genericTT'"];
  return dictGenericHeytingAlgebra1 => (
    {
      "genericFF'": Data$dGeneric$dRep.$Product(genericFF$p1, dictGenericHeytingAlgebra1["genericFF'"]),
      "genericTT'": Data$dGeneric$dRep.$Product(genericTT$p1, dictGenericHeytingAlgebra1["genericTT'"]),
      "genericImplies'": v => v1 => Data$dGeneric$dRep.$Product(
        dictGenericHeytingAlgebra["genericImplies'"](v._1)(v1._1),
        dictGenericHeytingAlgebra1["genericImplies'"](v._2)(v1._2)
      ),
      "genericConj'": v => v1 => Data$dGeneric$dRep.$Product(dictGenericHeytingAlgebra["genericConj'"](v._1)(v1._1), dictGenericHeytingAlgebra1["genericConj'"](v._2)(v1._2)),
      "genericDisj'": v => v1 => Data$dGeneric$dRep.$Product(dictGenericHeytingAlgebra["genericDisj'"](v._1)(v1._1), dictGenericHeytingAlgebra1["genericDisj'"](v._2)(v1._2)),
      "genericNot'": v => Data$dGeneric$dRep.$Product(dictGenericHeytingAlgebra["genericNot'"](v._1), dictGenericHeytingAlgebra1["genericNot'"](v._2))
    }
  );
};
const genericConj = dictGeneric => dictGenericHeytingAlgebra => x => y => dictGeneric.to(dictGenericHeytingAlgebra["genericConj'"](dictGeneric.from(x))(dictGeneric.from(y)));
export {
  genericConj,
  genericConj$p,
  genericDisj,
  genericDisj$p,
  genericFF,
  genericFF$p,
  genericHeytingAlgebraArgument,
  genericHeytingAlgebraConstructor,
  genericHeytingAlgebraNoArguments,
  genericHeytingAlgebraProduct,
  genericImplies,
  genericImplies$p,
  genericNot,
  genericNot$p,
  genericTT,
  genericTT$p
};
