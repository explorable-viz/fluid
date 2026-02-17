import * as $runtime from "../runtime.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dMonoid from "../Data.Monoid/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as JSURI from "../JSURI/index.js";
const show = /* #__PURE__ */ Data$dShow.showArrayImpl(v => "(Tuple " + Data$dShow.showStringImpl(v._1) + " " + (() => {
  if (v._2.tag === "Just") { return "(Just " + Data$dShow.showStringImpl(v._2._1) + ")"; }
  if (v._2.tag === "Nothing") { return "Nothing"; }
  $runtime.fail();
})() + ")");
const traverse = /* #__PURE__ */ (() => Data$dTraversable.traversableArray.traverse(Data$dMaybe.applicativeMaybe))();
const FormURLEncoded = x => x;
const toArray = v => v;
const showFormUrlEncoded = {show: v => "(FormURLEncoded " + show(v) + ")"};
const semigroupFormUrlEncoded = Data$dSemigroup.semigroupArray;
const ordFormUrlEncoded = /* #__PURE__ */ Data$dOrd.ordArray(/* #__PURE__ */ (() => {
  const eqTuple2 = {eq: x => y => x._1 === y._1 && (x._2.tag === "Nothing" ? y._2.tag === "Nothing" : x._2.tag === "Just" && y._2.tag === "Just" && x._2._1 === y._2._1)};
  return {
    compare: x => y => {
      const v = Data$dOrd.ordString.compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      if (x._2.tag === "Nothing") {
        if (y._2.tag === "Nothing") { return Data$dOrdering.EQ; }
        return Data$dOrdering.LT;
      }
      if (y._2.tag === "Nothing") { return Data$dOrdering.GT; }
      if (x._2.tag === "Just" && y._2.tag === "Just") { return Data$dOrd.ordString.compare(x._2._1)(y._2._1); }
      $runtime.fail();
    },
    Eq0: () => eqTuple2
  };
})());
const newtypeFormUrlEncoded = {Coercible0: () => {}};
const monoidFormUrlEncoded = Data$dMonoid.monoidArray;
const fromArray = FormURLEncoded;
const eqFormUrlEncoded = {
  eq: /* #__PURE__ */ Data$dEq.eqArrayImpl(x => y => x._1 === y._1 && (x._2.tag === "Nothing"
    ? y._2.tag === "Nothing"
    : x._2.tag === "Just" && y._2.tag === "Just" && x._2._1 === y._2._1))
};
const encode = /* #__PURE__ */ (() => {
  const $0 = Data$dString$dCommon.joinWith("&");
  const $1 = traverse(v => {
    if (v._2.tag === "Nothing") { return JSURI._encodeFormURLComponent(v$1 => Data$dMaybe.Nothing, Data$dMaybe.Just, v._1); }
    if (v._2.tag === "Just") {
      const $1 = JSURI._encodeFormURLComponent(v$1 => Data$dMaybe.Nothing, Data$dMaybe.Just, v._1);
      if ($1.tag === "Just") {
        const $2 = JSURI._encodeFormURLComponent(v$1 => Data$dMaybe.Nothing, Data$dMaybe.Just, v._2._1);
        if ($2.tag === "Just") { return Data$dMaybe.$Maybe("Just", $1._1 + "=" + $2._1); }
      }
      return Data$dMaybe.Nothing;
    }
    $runtime.fail();
  });
  return x => {
    const $2 = $1(x);
    if ($2.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0($2._1)); }
    return Data$dMaybe.Nothing;
  };
})();
const decode = /* #__PURE__ */ (() => {
  const $0 = traverse((() => {
    const $0 = Data$dString$dCommon.split("=");
    return x => {
      const $1 = $0(x);
      if ($1.length === 2) {
        const $2 = JSURI._decodeFormURLComponent(v => Data$dMaybe.Nothing, Data$dMaybe.Just, $1[0]);
        if ($2.tag === "Just") {
          const $3 = JSURI._decodeFormURLComponent(v => Data$dMaybe.Nothing, Data$dMaybe.Just, $1[1]);
          if ($3.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($2._1, Data$dMaybe.$Maybe("Just", $3._1))); }
        }
        return Data$dMaybe.Nothing;
      }
      if ($1.length === 1) {
        const $2 = JSURI._decodeFormURLComponent(v => Data$dMaybe.Nothing, Data$dMaybe.Just, $1[0]);
        if ($2.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($2._1, Data$dMaybe.Nothing)); }
      }
      return Data$dMaybe.Nothing;
    };
  })());
  const $1 = Data$dString$dCommon.split("&");
  return x => {
    const $2 = $0($1(x));
    if ($2.tag === "Just") { return Data$dMaybe.$Maybe("Just", $2._1); }
    return Data$dMaybe.Nothing;
  };
})();
export {
  
  
  encode,
  
  
  
  
  
  
  
  
  
  
};
