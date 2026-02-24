// | This module defines types and functions for working with nullable types
// | using the FFI.
import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import {null as $$null, notNull, nullable} from "./foreign.js";
const toNullable = v2 => {
  if (v2.tag === "Nothing") { return $$null; }
  if (v2.tag === "Just") { return notNull(v2._1); }
  $runtime.fail();
};
const toMaybe = n => nullable(n, Data$dMaybe.Nothing, Data$dMaybe.Just);
const showNullable = dictShow => (
  {
    show: x => {
      const $0 = nullable(x, Data$dMaybe.Nothing, Data$dMaybe.Just);
      if ($0.tag === "Nothing") { return "null"; }
      if ($0.tag === "Just") { return dictShow.show($0._1); }
      $runtime.fail();
    }
  }
);
const eqNullable = dictEq => (
  {
    eq: x => y => {
      const $0 = nullable(x, Data$dMaybe.Nothing, Data$dMaybe.Just);
      const $1 = nullable(y, Data$dMaybe.Nothing, Data$dMaybe.Just);
      if ($0.tag === "Nothing") { return $1.tag === "Nothing"; }
      return $0.tag === "Just" && $1.tag === "Just" && dictEq.eq($0._1)($1._1);
    }
  }
);
const ordNullable = dictOrd => {
  const eqNullable1 = eqNullable(dictOrd.Eq0());
  return {
    compare: x => y => {
      const $0 = nullable(x, Data$dMaybe.Nothing, Data$dMaybe.Just);
      const $1 = nullable(y, Data$dMaybe.Nothing, Data$dMaybe.Just);
      if ($0.tag === "Nothing") {
        if ($1.tag === "Nothing") { return Data$dOrdering.EQ; }
        return Data$dOrdering.LT;
      }
      if ($1.tag === "Nothing") { return Data$dOrdering.GT; }
      if ($0.tag === "Just" && $1.tag === "Just") { return dictOrd.compare($0._1)($1._1); }
      $runtime.fail();
    },
    Eq0: () => eqNullable1
  };
};
const eq1Nullable = {eq1: dictEq => eqNullable(dictEq).eq};
const ord1Nullable = {compare1: dictOrd => ordNullable(dictOrd).compare, Eq10: () => eq1Nullable};
;
export * from "./foreign.js";
