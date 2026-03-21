import * as $runtime from "../runtime.js";
import * as Data$dEuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
const genericToEnum$p = dict => dict["genericToEnum'"];
const genericToEnum = dictGeneric => dictGenericBoundedEnum => x => {
  const $0 = dictGenericBoundedEnum["genericToEnum'"](x);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", dictGeneric.to($0._1)); }
  return Data$dMaybe.Nothing;
};
const genericSucc$p = dict => dict["genericSucc'"];
const genericSucc = dictGeneric => dictGenericEnum => x => {
  const $0 = dictGenericEnum["genericSucc'"](dictGeneric.from(x));
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", dictGeneric.to($0._1)); }
  return Data$dMaybe.Nothing;
};
const genericPred$p = dict => dict["genericPred'"];
const genericPred = dictGeneric => dictGenericEnum => x => {
  const $0 = dictGenericEnum["genericPred'"](dictGeneric.from(x));
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", dictGeneric.to($0._1)); }
  return Data$dMaybe.Nothing;
};
const genericFromEnum$p = dict => dict["genericFromEnum'"];
const genericFromEnum = dictGeneric => dictGenericBoundedEnum => x => dictGenericBoundedEnum["genericFromEnum'"](dictGeneric.from(x));
const genericEnumSum = dictGenericEnum => dictGenericTop => {
  const genericTop$p = dictGenericTop["genericTop'"];
  return dictGenericEnum1 => dictGenericBottom => {
    const genericBottom$p = dictGenericBottom["genericBottom'"];
    return {
      "genericPred'": v => {
        if (v.tag === "Inl") {
          const $0 = dictGenericEnum["genericPred'"](v._1);
          if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Sum("Inl", $0._1)); }
          return Data$dMaybe.Nothing;
        }
        if (v.tag === "Inr") {
          const v1 = dictGenericEnum1["genericPred'"](v._1);
          if (v1.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Sum("Inl", genericTop$p)); }
          if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Sum("Inr", v1._1)); }
        }
        $runtime.fail();
      },
      "genericSucc'": v => {
        if (v.tag === "Inl") {
          const v1 = dictGenericEnum["genericSucc'"](v._1);
          if (v1.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Sum("Inr", genericBottom$p)); }
          if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Sum("Inl", v1._1)); }
          $runtime.fail();
        }
        if (v.tag === "Inr") {
          const $0 = dictGenericEnum1["genericSucc'"](v._1);
          if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Sum("Inr", $0._1)); }
          return Data$dMaybe.Nothing;
        }
        $runtime.fail();
      }
    };
  };
};
const genericEnumProduct = dictGenericEnum => dictGenericTop => dictGenericBottom => dictGenericEnum1 => dictGenericTop1 => {
  const genericTop$p = dictGenericTop1["genericTop'"];
  return dictGenericBottom1 => {
    const genericBottom$p = dictGenericBottom1["genericBottom'"];
    return {
      "genericPred'": v => {
        const v1 = dictGenericEnum1["genericPred'"](v._2);
        if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Product(v._1, v1._1)); }
        if (v1.tag === "Nothing") {
          const $0 = dictGenericEnum["genericPred'"](v._1);
          if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Product($0._1, genericTop$p)); }
          return Data$dMaybe.Nothing;
        }
        $runtime.fail();
      },
      "genericSucc'": v => {
        const v1 = dictGenericEnum1["genericSucc'"](v._2);
        if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Product(v._1, v1._1)); }
        if (v1.tag === "Nothing") {
          const $0 = dictGenericEnum["genericSucc'"](v._1);
          if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Product($0._1, genericBottom$p)); }
          return Data$dMaybe.Nothing;
        }
        $runtime.fail();
      }
    };
  };
};
const genericEnumNoArguments = {"genericPred'": v => Data$dMaybe.Nothing, "genericSucc'": v => Data$dMaybe.Nothing};
const genericEnumConstructor = dictGenericEnum => (
  {
    "genericPred'": v => {
      const $0 = dictGenericEnum["genericPred'"](v);
      if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1); }
      return Data$dMaybe.Nothing;
    },
    "genericSucc'": v => {
      const $0 = dictGenericEnum["genericSucc'"](v);
      if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1); }
      return Data$dMaybe.Nothing;
    }
  }
);
const genericEnumArgument = dictEnum => (
  {
    "genericPred'": v => {
      const $0 = dictEnum.pred(v);
      if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1); }
      return Data$dMaybe.Nothing;
    },
    "genericSucc'": v => {
      const $0 = dictEnum.succ(v);
      if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1); }
      return Data$dMaybe.Nothing;
    }
  }
);
const genericCardinality$p = dict => dict["genericCardinality'"];
const genericCardinality = dictGeneric => dictGenericBoundedEnum => dictGenericBoundedEnum["genericCardinality'"];
const genericBoundedEnumSum = dictGenericBoundedEnum => {
  const genericCardinality$p1 = dictGenericBoundedEnum["genericCardinality'"];
  return dictGenericBoundedEnum1 => (
    {
      "genericCardinality'": genericCardinality$p1 + dictGenericBoundedEnum1["genericCardinality'"] | 0,
      "genericToEnum'": n => {
        if (n >= 0 && n < genericCardinality$p1) {
          const $0 = dictGenericBoundedEnum["genericToEnum'"](n);
          if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Sum("Inl", $0._1)); }
          return Data$dMaybe.Nothing;
        }
        const $0 = dictGenericBoundedEnum1["genericToEnum'"](n - genericCardinality$p1 | 0);
        if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Sum("Inr", $0._1)); }
        return Data$dMaybe.Nothing;
      },
      "genericFromEnum'": v => {
        if (v.tag === "Inl") { return dictGenericBoundedEnum["genericFromEnum'"](v._1); }
        if (v.tag === "Inr") { return dictGenericBoundedEnum1["genericFromEnum'"](v._1) + genericCardinality$p1 | 0; }
        $runtime.fail();
      }
    }
  );
};
const genericBoundedEnumProduct = dictGenericBoundedEnum => {
  const genericCardinality$p1 = dictGenericBoundedEnum["genericCardinality'"];
  return dictGenericBoundedEnum1 => {
    const genericCardinality$p2 = dictGenericBoundedEnum1["genericCardinality'"];
    return {
      "genericCardinality'": genericCardinality$p1 * genericCardinality$p2 | 0,
      "genericToEnum'": n => {
        const $0 = dictGenericBoundedEnum["genericToEnum'"]($runtime.intDiv(n, genericCardinality$p2));
        if ($0.tag === "Just") {
          const $1 = dictGenericBoundedEnum1["genericToEnum'"](Data$dEuclideanRing.intMod(n)(genericCardinality$p2));
          if ($1.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.$Product($0._1, $1._1)); }
        }
        return Data$dMaybe.Nothing;
      },
      "genericFromEnum'": v1 => (dictGenericBoundedEnum["genericFromEnum'"](v1._1) * genericCardinality$p2 | 0) + dictGenericBoundedEnum1["genericFromEnum'"](v1._2) | 0
    };
  };
};
const genericBoundedEnumNoArguments = {
  "genericCardinality'": 1,
  "genericToEnum'": i => {
    if (i === 0) { return Data$dMaybe.$Maybe("Just", Data$dGeneric$dRep.NoArguments); }
    return Data$dMaybe.Nothing;
  },
  "genericFromEnum'": v => 0
};
const genericBoundedEnumConstructor = dictGenericBoundedEnum => (
  {
    "genericCardinality'": dictGenericBoundedEnum["genericCardinality'"],
    "genericToEnum'": i => {
      const $0 = dictGenericBoundedEnum["genericToEnum'"](i);
      if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1); }
      return Data$dMaybe.Nothing;
    },
    "genericFromEnum'": v => dictGenericBoundedEnum["genericFromEnum'"](v)
  }
);
const genericBoundedEnumArgument = dictBoundedEnum => (
  {
    "genericCardinality'": dictBoundedEnum.cardinality,
    "genericToEnum'": i => {
      const $0 = dictBoundedEnum.toEnum(i);
      if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1); }
      return Data$dMaybe.Nothing;
    },
    "genericFromEnum'": v => dictBoundedEnum.fromEnum(v)
  }
);
export {
  genericBoundedEnumArgument,
  genericBoundedEnumConstructor,
  genericBoundedEnumNoArguments,
  genericBoundedEnumProduct,
  genericBoundedEnumSum,
  genericCardinality,
  genericCardinality$p,
  genericEnumArgument,
  genericEnumConstructor,
  genericEnumNoArguments,
  genericEnumProduct,
  genericEnumSum,
  genericFromEnum,
  genericFromEnum$p,
  genericPred,
  genericPred$p,
  genericSucc,
  genericSucc$p,
  genericToEnum,
  genericToEnum$p
};
