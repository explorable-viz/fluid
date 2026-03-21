import * as $runtime from "../runtime.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunction$dMemoize from "../Data.Function.Memoize/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dSemigroup$dFoldable from "../Data.Semigroup.Foldable/index.js";
const memoize2 = /* #__PURE__ */ Data$dFunction$dMemoize.memoize2(Data$dFunction$dMemoize.tabulateNat)(Data$dFunction$dMemoize.tabulateNat);
const minimum = /* #__PURE__ */ Data$dSemigroup$dFoldable.minimum(Data$dOrd.ordInt)(/* #__PURE__ */ Data$dNonEmpty.foldable1NonEmpty(Data$dFoldable.foldableArray));
const editDistance = dictEq => xs => ys => {
  const dist = v => v1 => {
    if (v === 0) { return v1; }
    if (v1 === 0) { return v; }
    return minimum(Data$dNonEmpty.$NonEmpty(
      dist$p$lazy()(v - 1 | 0)(v1) + 1 | 0,
      [dist$p$lazy()(v)(v1 - 1 | 0) + 1 | 0, dictEq.eq(xs[v - 1 | 0])(ys[v1 - 1 | 0]) ? dist$p$lazy()(v - 1 | 0)(v1 - 1 | 0) : 1 + dist$p$lazy()(v - 1 | 0)(v1 - 1 | 0) | 0]
    ));
  };
  const dist$p$lazy = $runtime.binding(() => memoize2(a => b => dist(a)(b)));
  const dist$p = dist$p$lazy();
  return dist$p(xs.length)(ys.length);
};
export {editDistance, memoize2, minimum};
