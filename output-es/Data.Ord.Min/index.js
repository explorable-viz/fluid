import * as $runtime from "../runtime.js";
const Min = x => x;
const showMin = dictShow => ({show: v => "(Min " + dictShow.show(v) + ")"});
const semigroupMin = dictOrd => (
  {
    append: v => v1 => {
      const v$1 = dictOrd.compare(v)(v1);
      if (v$1 === "LT") { return v; }
      if (v$1 === "EQ") { return v; }
      if (v$1 === "GT") { return v1; }
      $runtime.fail();
    }
  }
);
const newtypeMin = {Coercible0: () => {}};
const monoidMin = dictBounded => {
  const $0 = dictBounded.Ord0();
  const semigroupMin1 = {
    append: v => v1 => {
      const v$1 = $0.compare(v)(v1);
      if (v$1 === "LT") { return v; }
      if (v$1 === "EQ") { return v; }
      if (v$1 === "GT") { return v1; }
      $runtime.fail();
    }
  };
  return {mempty: dictBounded.top, Semigroup0: () => semigroupMin1};
};
const eqMin = dictEq => dictEq;
const ordMin = dictOrd => {
  const $0 = dictOrd.Eq0();
  return {compare: v => v1 => dictOrd.compare(v)(v1), Eq0: () => $0};
};
export {Min, eqMin, monoidMin, newtypeMin, ordMin, semigroupMin, showMin};
