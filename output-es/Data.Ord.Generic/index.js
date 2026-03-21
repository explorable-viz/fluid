import * as $runtime from "../runtime.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const genericOrdNoConstructors = {"genericCompare'": v => v1 => Data$dOrdering.EQ};
const genericOrdNoArguments = {"genericCompare'": v => v1 => Data$dOrdering.EQ};
const genericOrdArgument = dictOrd => ({"genericCompare'": v => v1 => dictOrd.compare(v)(v1)});
const genericCompare$p = dict => dict["genericCompare'"];
const genericOrdConstructor = dictGenericOrd => ({"genericCompare'": v => v1 => dictGenericOrd["genericCompare'"](v)(v1)});
const genericOrdProduct = dictGenericOrd => dictGenericOrd1 => (
  {
    "genericCompare'": v => v1 => {
      const v2 = dictGenericOrd["genericCompare'"](v._1)(v1._1);
      if (v2 === "EQ") { return dictGenericOrd1["genericCompare'"](v._2)(v1._2); }
      return v2;
    }
  }
);
const genericOrdSum = dictGenericOrd => dictGenericOrd1 => (
  {
    "genericCompare'": v => v1 => {
      if (v.tag === "Inl") {
        if (v1.tag === "Inl") { return dictGenericOrd["genericCompare'"](v._1)(v1._1); }
        if (v1.tag === "Inr") { return Data$dOrdering.LT; }
        $runtime.fail();
      }
      if (v.tag === "Inr") {
        if (v1.tag === "Inr") { return dictGenericOrd1["genericCompare'"](v._1)(v1._1); }
        if (v1.tag === "Inl") { return Data$dOrdering.GT; }
      }
      $runtime.fail();
    }
  }
);
const genericCompare = dictGeneric => dictGenericOrd => x => y => dictGenericOrd["genericCompare'"](dictGeneric.from(x))(dictGeneric.from(y));
export {genericCompare, genericCompare$p, genericOrdArgument, genericOrdConstructor, genericOrdNoArguments, genericOrdNoConstructors, genericOrdProduct, genericOrdSum};
