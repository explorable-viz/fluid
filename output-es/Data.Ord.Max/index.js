import * as $runtime from "../runtime.js";
const Max = x => x;
const showMax = dictShow => ({show: v => "(Max " + dictShow.show(v) + ")"});
const semigroupMax = dictOrd => (
  {
    append: v => v1 => {
      const v$1 = dictOrd.compare(v)(v1);
      if (v$1 === "LT") { return v1; }
      if (v$1 === "EQ") { return v; }
      if (v$1 === "GT") { return v; }
      $runtime.fail();
    }
  }
);
const newtypeMax = {Coercible0: () => {}};
const monoidMax = dictBounded => {
  const $0 = dictBounded.Ord0();
  const semigroupMax1 = {
    append: v => v1 => {
      const v$1 = $0.compare(v)(v1);
      if (v$1 === "LT") { return v1; }
      if (v$1 === "EQ") { return v; }
      if (v$1 === "GT") { return v; }
      $runtime.fail();
    }
  };
  return {mempty: dictBounded.bottom, Semigroup0: () => semigroupMax1};
};
const eqMax = dictEq => dictEq;
const ordMax = dictOrd => {
  const $0 = dictOrd.Eq0();
  return {compare: v => v1 => dictOrd.compare(v)(v1), Eq0: () => $0};
};
export {Max, eqMax, monoidMax, newtypeMax, ordMax, semigroupMax, showMax};
