// | Originally implemented in:
// | https://github.com/garyb/purescript-codec-argonaut
import * as $runtime from "../runtime.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
const $JsonDecodeError = (tag, _1, _2) => ({tag, _1, _2});
const TypeMismatch = value0 => $JsonDecodeError("TypeMismatch", value0);
const UnexpectedValue = value0 => $JsonDecodeError("UnexpectedValue", value0);
const AtIndex = value0 => value1 => $JsonDecodeError("AtIndex", value0, value1);
const AtKey = value0 => value1 => $JsonDecodeError("AtKey", value0, value1);
const Named = value0 => value1 => $JsonDecodeError("Named", value0, value1);
const MissingValue = /* #__PURE__ */ $JsonDecodeError("MissingValue");
const showJsonDecodeError = {
  show: v => {
    if (v.tag === "TypeMismatch") { return "(TypeMismatch " + Data$dShow.showStringImpl(v._1) + ")"; }
    if (v.tag === "UnexpectedValue") { return "(UnexpectedValue " + Data$dArgonaut$dCore.stringify(v._1) + ")"; }
    if (v.tag === "AtIndex") { return "(AtIndex " + Data$dShow.showIntImpl(v._1) + " " + showJsonDecodeError.show(v._2) + ")"; }
    if (v.tag === "AtKey") { return "(AtKey " + Data$dShow.showStringImpl(v._1) + " " + showJsonDecodeError.show(v._2) + ")"; }
    if (v.tag === "Named") { return "(Named " + Data$dShow.showStringImpl(v._1) + " " + showJsonDecodeError.show(v._2) + ")"; }
    if (v.tag === "MissingValue") { return "MissingValue"; }
    $runtime.fail();
  }
};
const printJsonDecodeError = err => {
  const go = v => {
    if (v.tag === "TypeMismatch") { return "  Expected value of type '" + v._1 + "'."; }
    if (v.tag === "UnexpectedValue") { return "  Unexpected value " + Data$dArgonaut$dCore.stringify(v._1) + "."; }
    if (v.tag === "AtIndex") { return "  At array index " + Data$dShow.showIntImpl(v._1) + ":\n" + go(v._2); }
    if (v.tag === "AtKey") { return "  At object key '" + v._1 + "':\n" + go(v._2); }
    if (v.tag === "Named") { return "  Under '" + v._1 + "':\n" + go(v._2); }
    if (v.tag === "MissingValue") { return "  No value was found."; }
    $runtime.fail();
  };
  return "An error occurred while decoding a JSON value:\n" + go(err);
};
const genericJsonDecodeError = {
  to: x => {
    if (x.tag === "Inl") { return $JsonDecodeError("TypeMismatch", x._1); }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return $JsonDecodeError("UnexpectedValue", x._1._1); }
      if (x._1.tag === "Inr") {
        if (x._1._1.tag === "Inl") { return $JsonDecodeError("AtIndex", x._1._1._1._1, x._1._1._1._2); }
        if (x._1._1.tag === "Inr") {
          if (x._1._1._1.tag === "Inl") { return $JsonDecodeError("AtKey", x._1._1._1._1._1, x._1._1._1._1._2); }
          if (x._1._1._1.tag === "Inr") {
            if (x._1._1._1._1.tag === "Inl") { return $JsonDecodeError("Named", x._1._1._1._1._1._1, x._1._1._1._1._1._2); }
            if (x._1._1._1._1.tag === "Inr") { return MissingValue; }
          }
        }
      }
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "TypeMismatch") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
    if (x.tag === "UnexpectedValue") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", x._1)); }
    if (x.tag === "AtIndex") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2)))); }
    if (x.tag === "AtKey") {
      return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2)))));
    }
    if (x.tag === "Named") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2)))))
      );
    }
    if (x.tag === "MissingValue") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments))))
      );
    }
    $runtime.fail();
  }
};
const eqJsonDecodeError = {
  eq: x => y => {
    if (x.tag === "TypeMismatch") { return y.tag === "TypeMismatch" && x._1 === y._1; }
    if (x.tag === "UnexpectedValue") {
      return y.tag === "UnexpectedValue" && Data$dArgonaut$dCore._compare(Data$dOrdering.EQ, Data$dOrdering.GT, Data$dOrdering.LT, x._1, y._1) === "EQ";
    }
    if (x.tag === "AtIndex") { return y.tag === "AtIndex" && x._1 === y._1 && eqJsonDecodeError.eq(x._2)(y._2); }
    if (x.tag === "AtKey") { return y.tag === "AtKey" && x._1 === y._1 && eqJsonDecodeError.eq(x._2)(y._2); }
    if (x.tag === "Named") { return y.tag === "Named" && x._1 === y._1 && eqJsonDecodeError.eq(x._2)(y._2); }
    return x.tag === "MissingValue" && y.tag === "MissingValue";
  }
};
const ordJsonDecodeError = {
  compare: x => y => {
    if (x.tag === "TypeMismatch") {
      if (y.tag === "TypeMismatch") { return Data$dOrd.ordString.compare(x._1)(y._1); }
      return Data$dOrdering.LT;
    }
    if (y.tag === "TypeMismatch") { return Data$dOrdering.GT; }
    if (x.tag === "UnexpectedValue") {
      if (y.tag === "UnexpectedValue") { return Data$dArgonaut$dCore._compare(Data$dOrdering.EQ, Data$dOrdering.GT, Data$dOrdering.LT, x._1, y._1); }
      return Data$dOrdering.LT;
    }
    if (y.tag === "UnexpectedValue") { return Data$dOrdering.GT; }
    if (x.tag === "AtIndex") {
      if (y.tag === "AtIndex") {
        const v = Data$dOrd.ordInt.compare(x._1)(y._1);
        if (v === "LT") { return Data$dOrdering.LT; }
        if (v === "GT") { return Data$dOrdering.GT; }
        return ordJsonDecodeError.compare(x._2)(y._2);
      }
      return Data$dOrdering.LT;
    }
    if (y.tag === "AtIndex") { return Data$dOrdering.GT; }
    if (x.tag === "AtKey") {
      if (y.tag === "AtKey") {
        const v = Data$dOrd.ordString.compare(x._1)(y._1);
        if (v === "LT") { return Data$dOrdering.LT; }
        if (v === "GT") { return Data$dOrdering.GT; }
        return ordJsonDecodeError.compare(x._2)(y._2);
      }
      return Data$dOrdering.LT;
    }
    if (y.tag === "AtKey") { return Data$dOrdering.GT; }
    if (x.tag === "Named") {
      if (y.tag === "Named") {
        const v = Data$dOrd.ordString.compare(x._1)(y._1);
        if (v === "LT") { return Data$dOrdering.LT; }
        if (v === "GT") { return Data$dOrdering.GT; }
        return ordJsonDecodeError.compare(x._2)(y._2);
      }
      return Data$dOrdering.LT;
    }
    if (y.tag === "Named") { return Data$dOrdering.GT; }
    if (x.tag === "MissingValue" && y.tag === "MissingValue") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqJsonDecodeError
};
export {
  $JsonDecodeError,
  AtIndex,
  AtKey,
  MissingValue,
  Named,
  
  
  
  
  
  
  showJsonDecodeError
};
