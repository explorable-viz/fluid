// | This is port of https://github.com/ekmett/ansi-wl-pprint to ps without any ansi stuff as it's
// | used by optparser, later we shuold use [prettyprinter](https://hackage.haskell.org/package/prettyprinter) once
// | [this](https://github.com/pcapriotti/optparse-applicative/issues/273) is fixed.
// | Also see [this](https://github.com/ekmett/ansi-wl-pprint/issues/18)
import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dFunctorWithIndex from "../Data.FunctorWithIndex/index.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dLazy from "../Data.Lazy/index.js";
import * as Data$dList$dLazy from "../Data.List.Lazy/index.js";
import * as Data$dList$dLazy$dTypes from "../Data.List.Lazy.Types/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dShow$dGeneric from "../Data.Show.Generic/index.js";
import * as Data$dString$dCodePoints from "../Data.String.CodePoints/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
import * as Partial from "../Partial/index.js";
const $Doc = (tag, _1, _2) => ({tag, _1, _2});
const $Docs = (tag, _1, _2, _3) => ({tag, _1, _2, _3});
const $LazySimpleDoc = (tag, _1, _2, _3) => ({tag, _1, _2, _3});
const $SimpleDoc = (tag, _1, _2, _3) => ({tag, _1, _2, _3});
const genericShowSum = /* #__PURE__ */ (() => {
  const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "SFail"});
  return dictGenericShow1 => (
    {
      "genericShow'": v => {
        if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
        if (v.tag === "Inr") { return dictGenericShow1["genericShow'"](v._1); }
        $runtime.fail();
      }
    }
  );
})();
const genericShowSum1 = /* #__PURE__ */ (() => {
  const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "SEmpty"});
  return dictGenericShow1 => (
    {
      "genericShow'": v => {
        if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
        if (v.tag === "Inr") { return dictGenericShow1["genericShow'"](v._1); }
        $runtime.fail();
      }
    }
  );
})();
const SCharIsSymbol = {reflectSymbol: () => "SChar"};
const STextIsSymbol = {reflectSymbol: () => "SText"};
const SLineIsSymbol = {reflectSymbol: () => "SLine"};
const max = x => y => {
  const v = Data$dOrd.ordInt.compare(x)(y);
  if (v === "LT") { return y; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return x; }
  $runtime.fail();
};
const min = x => y => {
  const v = Data$dOrd.ordInt.compare(x)(y);
  if (v === "LT") { return x; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return y; }
  $runtime.fail();
};
const toUnfoldable = /* #__PURE__ */ Data$dList$dLazy.toUnfoldable(Data$dUnfoldable.unfoldableArray);
const fromFoldable = /* #__PURE__ */ Data$dFoldable.foldrArray(Data$dList$dLazy$dTypes.cons)(Data$dList$dLazy$dTypes.nil);
const SFail = /* #__PURE__ */ $SimpleDoc("SFail");
const SEmpty = /* #__PURE__ */ $SimpleDoc("SEmpty");
const SChar = value0 => value1 => $SimpleDoc("SChar", value0, value1);
const SText = value0 => value1 => value2 => $SimpleDoc("SText", value0, value1, value2);
const SLine = value0 => value1 => $SimpleDoc("SLine", value0, value1);
const SFail$p = /* #__PURE__ */ $LazySimpleDoc("SFail'");
const SEmpty$p = /* #__PURE__ */ $LazySimpleDoc("SEmpty'");
const SChar$p = value0 => value1 => $LazySimpleDoc("SChar'", value0, value1);
const SText$p = value0 => value1 => value2 => $LazySimpleDoc("SText'", value0, value1, value2);
const SLine$p = value0 => value1 => $LazySimpleDoc("SLine'", value0, value1);
const Fail = /* #__PURE__ */ $Doc("Fail");
const Empty = /* #__PURE__ */ $Doc("Empty");
const Char = value0 => $Doc("Char", value0);
const Text = value0 => value1 => $Doc("Text", value0, value1);
const Line = /* #__PURE__ */ $Doc("Line");
const FlatAlt = value0 => value1 => $Doc("FlatAlt", value0, value1);
const Cat = value0 => value1 => $Doc("Cat", value0, value1);
const Nest = value0 => value1 => $Doc("Nest", value0, value1);
const Union = value0 => value1 => $Doc("Union", value0, value1);
const Column = value0 => $Doc("Column", value0);
const Columns = value0 => $Doc("Columns", value0);
const Nesting = value0 => $Doc("Nesting", value0);
const Nil = /* #__PURE__ */ $Docs("Nil");
const Cons = value0 => value1 => value2 => $Docs("Cons", value0, value1, value2);
const text = v => {
  if (v === "") { return Empty; }
  return $Doc("Text", Data$dString$dCodePoints.toCodePointArray(v).length, v);
};
const squote = /* #__PURE__ */ $Doc("Char", "'");
const spaces = n => {
  if (n <= 0) { return ""; }
  return Data$dString$dCodeUnits.fromCharArray(Data$dArray.replicateImpl(n, " "));
};
const space = /* #__PURE__ */ $Doc("Char", " ");
const simpleDocEq = {
  eq: x => y => {
    if (x.tag === "SFail") { return y.tag === "SFail"; }
    if (x.tag === "SEmpty") { return y.tag === "SEmpty"; }
    if (x.tag === "SChar") { return y.tag === "SChar" && x._1 === y._1 && simpleDocEq.eq(x._2)(y._2); }
    if (x.tag === "SText") { return y.tag === "SText" && x._1 === y._1 && x._2 === y._2 && simpleDocEq.eq(x._3)(y._3); }
    return x.tag === "SLine" && y.tag === "SLine" && x._1 === y._1 && simpleDocEq.eq(x._2)(y._2);
  }
};
const simpleDocOrd = {
  compare: x => y => {
    if (x.tag === "SFail") {
      if (y.tag === "SFail") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y.tag === "SFail") { return Data$dOrdering.GT; }
    if (x.tag === "SEmpty") {
      if (y.tag === "SEmpty") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y.tag === "SEmpty") { return Data$dOrdering.GT; }
    if (x.tag === "SChar") {
      if (y.tag === "SChar") {
        const v = Data$dOrd.ordChar.compare(x._1)(y._1);
        if (v === "LT") { return Data$dOrdering.LT; }
        if (v === "GT") { return Data$dOrdering.GT; }
        return simpleDocOrd.compare(x._2)(y._2);
      }
      return Data$dOrdering.LT;
    }
    if (y.tag === "SChar") { return Data$dOrdering.GT; }
    if (x.tag === "SText") {
      if (y.tag === "SText") {
        const v = Data$dOrd.ordInt.compare(x._1)(y._1);
        if (v === "LT") { return Data$dOrdering.LT; }
        if (v === "GT") { return Data$dOrdering.GT; }
        const v1 = Data$dOrd.ordString.compare(x._2)(y._2);
        if (v1 === "LT") { return Data$dOrdering.LT; }
        if (v1 === "GT") { return Data$dOrdering.GT; }
        return simpleDocOrd.compare(x._3)(y._3);
      }
      return Data$dOrdering.LT;
    }
    if (y.tag === "SText") { return Data$dOrdering.GT; }
    if (x.tag === "SLine" && y.tag === "SLine") {
      const v = Data$dOrd.ordInt.compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      return simpleDocOrd.compare(x._2)(y._2);
    }
    $runtime.fail();
  },
  Eq0: () => simpleDocEq
};
const semi = /* #__PURE__ */ $Doc("Char", ";");
const rparen = /* #__PURE__ */ $Doc("Char", ")");
const renderCompact = /* #__PURE__ */ (() => {
  const scan = v => v1 => {
    if (v1.tag === "Nil") { return SEmpty; }
    if (v1.tag === "Cons") {
      if (v1._1.tag === "Fail") { return SFail; }
      if (v1._1.tag === "Empty") { return scan(v)(v1._2); }
      if (v1._1.tag === "Char") { return $SimpleDoc("SChar", v1._1._1, scan(v + 1 | 0)(v1._2)); }
      if (v1._1.tag === "Text") { return $SimpleDoc("SText", v1._1._1, v1._1._2, scan(v + v1._1._1 | 0)(v1._2)); }
      if (v1._1.tag === "FlatAlt") { return scan(v)(Data$dList$dTypes.$List("Cons", v1._1._1, v1._2)); }
      if (v1._1.tag === "Line") { return $SimpleDoc("SLine", 0, scan(0)(v1._2)); }
      if (v1._1.tag === "Cat") { return scan(v)(Data$dList$dTypes.$List("Cons", v1._1._1, Data$dList$dTypes.$List("Cons", v1._1._2, v1._2))); }
      if (v1._1.tag === "Nest") { return scan(v)(Data$dList$dTypes.$List("Cons", v1._1._2, v1._2)); }
      if (v1._1.tag === "Union") { return scan(v)(Data$dList$dTypes.$List("Cons", v1._1._2, v1._2)); }
      if (v1._1.tag === "Column") { return scan(v)(Data$dList$dTypes.$List("Cons", v1._1._1(v), v1._2)); }
      if (v1._1.tag === "Columns") { return scan(v)(Data$dList$dTypes.$List("Cons", v1._1._1(Data$dMaybe.Nothing), v1._2)); }
      if (v1._1.tag === "Nesting") { return scan(v)(Data$dList$dTypes.$List("Cons", v1._1._1(0), v1._2)); }
    }
    $runtime.fail();
  };
  const $0 = scan(0);
  return x => $0(Data$dList$dTypes.$List("Cons", x, Data$dList$dTypes.Nil));
})();
const rbracket = /* #__PURE__ */ $Doc("Char", "]");
const rbrace = /* #__PURE__ */ $Doc("Char", "}");
const rangle = /* #__PURE__ */ $Doc("Char", ">");
const number = f => {
  const $0 = Data$dShow.showNumberImpl(f);
  if ($0 === "") { return Empty; }
  return $Doc("Text", Data$dString$dCodePoints.toCodePointArray($0).length, $0);
};
const nesting = f => $Doc("Nesting", f);
const nest = i => x => $Doc("Nest", i, x);
const lparen = /* #__PURE__ */ $Doc("Char", "(");
const line = /* #__PURE__ */ $Doc("FlatAlt", Line, /* #__PURE__ */ $Doc("Char", " "));
const lbracket = /* #__PURE__ */ $Doc("Char", "[");
const lbrace = /* #__PURE__ */ $Doc("Char", "{");
const langle = /* #__PURE__ */ $Doc("Char", "<");
const $$int = i => {
  const $0 = Data$dShow.showIntImpl(i);
  if ($0 === "") { return Empty; }
  return $Doc("Text", Data$dString$dCodePoints.toCodePointArray($0).length, $0);
};
const indentation = n => {
  if (n <= 0) { return ""; }
  return Data$dString$dCodeUnits.fromCharArray(Data$dArray.replicateImpl(n, " "));
};
const hardline = Line;
const genericSimpleDoc = {
  to: x => {
    if (x.tag === "Inl") { return SFail; }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return SEmpty; }
      if (x._1.tag === "Inr") {
        if (x._1._1.tag === "Inl") { return $SimpleDoc("SChar", x._1._1._1._1, x._1._1._1._2); }
        if (x._1._1.tag === "Inr") {
          if (x._1._1._1.tag === "Inl") { return $SimpleDoc("SText", x._1._1._1._1._1, x._1._1._1._1._2._1, x._1._1._1._1._2._2); }
          if (x._1._1._1.tag === "Inr") { return $SimpleDoc("SLine", x._1._1._1._1._1, x._1._1._1._1._2); }
        }
      }
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "SFail") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (x.tag === "SEmpty") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (x.tag === "SChar") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2)))); }
    if (x.tag === "SText") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, Data$dGeneric$dRep.$Product(x._2, x._3)))))
      );
    }
    if (x.tag === "SLine") {
      return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Product(x._1, x._2)))));
    }
    $runtime.fail();
  }
};
const showSimpleDoc = {
  show: a => genericShowSum(genericShowSum1((() => {
    const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct({genericShowArgs: v => [Data$dShow.showCharImpl(v)]})({
      genericShowArgs: v => [showSimpleDoc.show(v)]
    }))(SCharIsSymbol);
    const $1 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct({genericShowArgs: v => [Data$dShow.showIntImpl(v)]})(Data$dShow$dGeneric.genericShowArgsProduct({
      genericShowArgs: v => [Data$dShow.showStringImpl(v)]
    })({genericShowArgs: v => [showSimpleDoc.show(v)]})))(STextIsSymbol);
    const $2 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct({genericShowArgs: v => [Data$dShow.showIntImpl(v)]})({
      genericShowArgs: v => [showSimpleDoc.show(v)]
    }))(SLineIsSymbol);
    return {
      "genericShow'": v => {
        if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
        if (v.tag === "Inr") {
          if (v._1.tag === "Inl") { return $1["genericShow'"](v._1._1); }
          if (v._1.tag === "Inr") { return $2["genericShow'"](v._1._1); }
        }
        $runtime.fail();
      }
    };
  })()))["genericShow'"]((() => {
    if (a.tag === "SFail") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (a.tag === "SEmpty") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (a.tag === "SChar") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(a._1, a._2)))); }
    if (a.tag === "SText") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(a._1, Data$dGeneric$dRep.$Product(a._2, a._3)))))
      );
    }
    if (a.tag === "SLine") {
      return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Product(a._1, a._2)))));
    }
    $runtime.fail();
  })())
};
const forceSimpleDoc = v => {
  if (v.tag === "SFail'") { return SFail; }
  if (v.tag === "SEmpty'") { return SEmpty; }
  if (v.tag === "SChar'") { return $SimpleDoc("SChar", v._1, forceSimpleDoc(Data$dLazy.force(v._2))); }
  if (v.tag === "SText'") { return $SimpleDoc("SText", v._1, v._2, forceSimpleDoc(Data$dLazy.force(v._3))); }
  if (v.tag === "SLine'") { return $SimpleDoc("SLine", v._1, forceSimpleDoc(Data$dLazy.force(v._2))); }
  $runtime.fail();
};
const renderFits = fits => rfrac => w => headNode => {
  const r = max(0)(min(w)(Data$dInt.unsafeClamp(Data$dNumber.round(Data$dInt.toNumber(w) * rfrac))));
  const nicest$p = n => k => i => ds => x => y => {
    const x$p = best(n)(k)($Docs("Cons", i, x, ds));
    if (fits(w)(min(n)(k))(min(w - k | 0)((r - k | 0) + n | 0))(x$p)) { return x$p; }
    return best(n)(k)($Docs("Cons", i, y, ds));
  };
  const best = v => v1 => v2 => {
    if (v2.tag === "Nil") { return SEmpty$p; }
    if (v2.tag === "Cons") {
      if (v2._2.tag === "Fail") { return SFail$p; }
      if (v2._2.tag === "Empty") { return best(v)(v1)(v2._3); }
      if (v2._2.tag === "Char") {
        const k$p = v1 + 1 | 0;
        return $LazySimpleDoc("SChar'", v2._2._1, Data$dLazy.defer(v3 => best(v)(k$p)(v2._3)));
      }
      if (v2._2.tag === "Text") {
        const k$p = v1 + v2._2._1 | 0;
        return $LazySimpleDoc("SText'", v2._2._1, v2._2._2, Data$dLazy.defer(v3 => best(v)(k$p)(v2._3)));
      }
      if (v2._2.tag === "Line") { return $LazySimpleDoc("SLine'", v2._1, Data$dLazy.defer(v3 => best(v2._1)(v2._1)(v2._3))); }
      if (v2._2.tag === "FlatAlt") { return best(v)(v1)($Docs("Cons", v2._1, v2._2._1, v2._3)); }
      if (v2._2.tag === "Cat") { return best(v)(v1)($Docs("Cons", v2._1, v2._2._1, $Docs("Cons", v2._1, v2._2._2, v2._3))); }
      if (v2._2.tag === "Nest") { return best(v)(v1)($Docs("Cons", v2._1 + v2._2._1 | 0, v2._2._2, v2._3)); }
      if (v2._2.tag === "Union") { return nicest$p(v)(v1)(v2._1)(v2._3)(v2._2._1)(v2._2._2); }
      if (v2._2.tag === "Column") { return best(v)(v1)($Docs("Cons", v2._1, v2._2._1(v1), v2._3)); }
      if (v2._2.tag === "Columns") { return best(v)(v1)($Docs("Cons", v2._1, v2._2._1(Data$dMaybe.$Maybe("Just", w)), v2._3)); }
      if (v2._2.tag === "Nesting") { return best(v)(v1)($Docs("Cons", v2._1, v2._2._1(v2._1), v2._3)); }
    }
    $runtime.fail();
  };
  return forceSimpleDoc(best(0)(0)($Docs("Cons", 0, headNode, Nil)));
};
const foldr1 = dictMonoid => {
  const mempty = dictMonoid.mempty;
  return f => x => {
    const $0 = Data$dArray.unsnoc(x);
    if ($0.tag === "Nothing") { return mempty; }
    if ($0.tag === "Just") { return Data$dFoldable.foldrArray(f)($0._1.last)($0._1.init); }
    $runtime.fail();
  };
};
const flatten = v => {
  if (v.tag === "FlatAlt") { return v._2; }
  if (v.tag === "Cat") { return $Doc("Cat", flatten(v._1), flatten(v._2)); }
  if (v.tag === "Nest") { return $Doc("Nest", v._1, flatten(v._2)); }
  if (v.tag === "Line") { return Fail; }
  if (v.tag === "Union") { return flatten(v._1); }
  if (v.tag === "Column") { return $Doc("Column", x => flatten(v._1(x))); }
  if (v.tag === "Columns") { return $Doc("Columns", x => flatten(v._1(x))); }
  if (v.tag === "Nesting") { return $Doc("Nesting", x => flatten(v._1(x))); }
  return v;
};
const group = x => $Doc("Union", flatten(x), x);
const softline = /* #__PURE__ */ $Doc(
  "Union",
  /* #__PURE__ */ flatten(/* #__PURE__ */ $Doc("FlatAlt", Line, /* #__PURE__ */ $Doc("Char", " "))),
  /* #__PURE__ */ $Doc("FlatAlt", Line, /* #__PURE__ */ $Doc("Char", " "))
);
const flatAlt = FlatAlt;
const fitsR = fitsR$a0$copy => fitsR$a1$copy => fitsR$a2$copy => fitsR$a3$copy => {
  let fitsR$a0 = fitsR$a0$copy, fitsR$a1 = fitsR$a1$copy, fitsR$a2 = fitsR$a2$copy, fitsR$a3 = fitsR$a3$copy, fitsR$c = true, fitsR$r;
  while (fitsR$c) {
    const v = fitsR$a0, v1 = fitsR$a1, v2 = fitsR$a2, v3 = fitsR$a3;
    if (v2 < 0) {
      fitsR$c = false;
      fitsR$r = false;
      continue;
    }
    if (v3.tag === "SFail'") {
      fitsR$c = false;
      fitsR$r = false;
      continue;
    }
    if (v3.tag === "SEmpty'") {
      fitsR$c = false;
      fitsR$r = true;
      continue;
    }
    if (v3.tag === "SChar'") {
      fitsR$a0 = v;
      fitsR$a1 = v1;
      fitsR$a2 = v2 - 1 | 0;
      fitsR$a3 = Data$dLazy.force(v3._2);
      continue;
    }
    if (v3.tag === "SText'") {
      fitsR$a0 = v;
      fitsR$a1 = v1;
      fitsR$a2 = v2 - v3._1 | 0;
      fitsR$a3 = Data$dLazy.force(v3._3);
      continue;
    }
    if (v3.tag === "SLine'") {
      if (v1 < v3._1) {
        fitsR$a0 = v;
        fitsR$a1 = v1;
        fitsR$a2 = v - v3._1 | 0;
        fitsR$a3 = Data$dLazy.force(v3._2);
        continue;
      }
      fitsR$c = false;
      fitsR$r = true;
      continue;
    }
    $runtime.fail();
  }
  return fitsR$r;
};
const renderSmart = /* #__PURE__ */ renderFits(fitsR);
const fits1 = fits1$a0$copy => fits1$a1$copy => fits1$a2$copy => fits1$a3$copy => {
  let fits1$a0 = fits1$a0$copy, fits1$a1 = fits1$a1$copy, fits1$a2 = fits1$a2$copy, fits1$a3 = fits1$a3$copy, fits1$c = true, fits1$r;
  while (fits1$c) {
    const v = fits1$a0, v1 = fits1$a1, v2 = fits1$a2, v3 = fits1$a3;
    if (v2 < 0) {
      fits1$c = false;
      fits1$r = false;
      continue;
    }
    if (v3.tag === "SFail'") {
      fits1$c = false;
      fits1$r = false;
      continue;
    }
    if (v3.tag === "SEmpty'") {
      fits1$c = false;
      fits1$r = true;
      continue;
    }
    if (v3.tag === "SChar'") {
      fits1$a0 = v;
      fits1$a1 = v1;
      fits1$a2 = v2 - 1 | 0;
      fits1$a3 = Data$dLazy.force(v3._2);
      continue;
    }
    if (v3.tag === "SText'") {
      fits1$a0 = v;
      fits1$a1 = v1;
      fits1$a2 = v2 - v3._1 | 0;
      fits1$a3 = Data$dLazy.force(v3._3);
      continue;
    }
    if (v3.tag === "SLine'") {
      fits1$c = false;
      fits1$r = true;
      continue;
    }
    $runtime.fail();
  }
  return fits1$r;
};
const renderPretty = /* #__PURE__ */ renderFits(fits1);
const equals = /* #__PURE__ */ $Doc("Char", "=");
const empty = Empty;
const linebreak = /* #__PURE__ */ $Doc("FlatAlt", Line, Empty);
const softbreak = /* #__PURE__ */ $Doc("Union", /* #__PURE__ */ flatten(/* #__PURE__ */ $Doc("FlatAlt", Line, Empty)), /* #__PURE__ */ $Doc("FlatAlt", Line, Empty));
const dquote = /* #__PURE__ */ $Doc("Char", "\"");
const dot = /* #__PURE__ */ $Doc("Char", ".");
const displayS = v => {
  if (v.tag === "SFail") { return Partial._crashWith("@SFail@ can not appear uncaught in a rendered @SimpleDoc@"); }
  if (v.tag === "SEmpty") { return ""; }
  if (v.tag === "SChar") { return Data$dString$dCodeUnits.fromCharArray([v._1]) + displayS(v._2); }
  if (v.tag === "SText") { return v._2 + displayS(v._3); }
  if (v.tag === "SLine") { return (v._1 <= 0 ? "\n" : "\n" + Data$dString$dCodeUnits.fromCharArray(Data$dArray.replicateImpl(v._1, " "))) + displayS(v._2); }
  $runtime.fail();
};
const docShow = {show: x => displayS(renderFits(fits1)(0.4)(80)(x))};
const comma = /* #__PURE__ */ $Doc("Char", ",");
const columns = f => $Doc("Columns", f);
const column = f => $Doc("Column", f);
const colon = /* #__PURE__ */ $Doc("Char", ":");
const $$char = v => {
  if (v === "\n") { return $Doc("FlatAlt", Line, $Doc("Char", " ")); }
  return $Doc("Char", v);
};
const bool = b => {
  const $0 = b ? "true" : "false";
  if ($0 === "") { return Empty; }
  return $Doc("Text", Data$dString$dCodePoints.toCodePointArray($0).length, $0);
};
const beside = x => y => $Doc("Cat", x, y);
const docSemigroup = {append: beside};
const docMonoid = {mempty: Empty, Semigroup0: () => docSemigroup};
const foldr11 = /* #__PURE__ */ foldr1(docMonoid);
const string = /* #__PURE__ */ (() => {
  const $0 = Data$dFunctor.arrayMap(text);
  const $1 = Data$dString$dCommon.split("\n");
  return x => Data$dFoldable.foldlArray(v => v1 => {
    if (v.init) { return {init: false, acc: v1}; }
    return {init: false, acc: $Doc("Cat", v.acc, $Doc("Cat", $Doc("FlatAlt", Line, $Doc("Char", " ")), v1))};
  })({init: true, acc: Empty})($0($1(x))).acc;
})();
const enclose = l => r => x => $Doc("Cat", l, $Doc("Cat", x, r));
const braces = x => $Doc("Cat", $Doc("Char", "{"), $Doc("Cat", x, $Doc("Char", "}")));
const brackets = x => $Doc("Cat", $Doc("Char", "["), $Doc("Cat", x, $Doc("Char", "]")));
const dquotes = x => $Doc("Cat", $Doc("Char", "\""), $Doc("Cat", x, $Doc("Char", "\"")));
const parens = x => $Doc("Cat", $Doc("Char", "("), $Doc("Cat", x, $Doc("Char", ")")));
const squotes = x => $Doc("Cat", $Doc("Char", "'"), $Doc("Cat", x, $Doc("Char", "'")));
const hcat = /* #__PURE__ */ foldr11(beside);
const punctuate = p => arr => {
  const lastIdx = arr.length - 1 | 0;
  return Data$dFunctorWithIndex.mapWithIndexArray(idx => d => {
    if (lastIdx === idx) { return d; }
    return $Doc("Cat", d, p);
  })(arr);
};
const width = d => f => $Doc("Column", k1 => $Doc("Cat", d, $Doc("Column", k2 => f(k2 - k1 | 0))));
const fill = f => d => $Doc(
  "Column",
  k1 => $Doc(
    "Cat",
    d,
    $Doc(
      "Column",
      k2 => {
        const $0 = k2 - k1 | 0;
        if ($0 >= f) { return Empty; }
        const $1 = f - $0 | 0;
        const $2 = $1 <= 0 ? "" : Data$dString$dCodeUnits.fromCharArray(Data$dArray.replicateImpl($1, " "));
        if ($2 === "") { return Empty; }
        return $Doc("Text", Data$dString$dCodePoints.toCodePointArray($2).length, $2);
      }
    )
  )
);
const fillBreak = f => x => $Doc(
  "Column",
  k1 => $Doc(
    "Cat",
    x,
    $Doc(
      "Column",
      k2 => {
        const $0 = k2 - k1 | 0;
        if ($0 > f) { return $Doc("Nest", f, $Doc("FlatAlt", Line, Empty)); }
        const $1 = f - $0 | 0;
        const $2 = $1 <= 0 ? "" : Data$dString$dCodeUnits.fromCharArray(Data$dArray.replicateImpl($1, " "));
        if ($2 === "") { return Empty; }
        return $Doc("Text", Data$dString$dCodePoints.toCodePointArray($2).length, $2);
      }
    )
  )
);
const backslash = /* #__PURE__ */ $Doc("Char", "\\");
const appendWithSpace = x => y => $Doc("Cat", x, $Doc("Cat", $Doc("Char", " "), y));
const hsep = /* #__PURE__ */ foldr11(appendWithSpace);
const appendWithSoftline = x => y => $Doc("Cat", x, $Doc("Cat", softline, y));
const fillSep = /* #__PURE__ */ foldr11(appendWithSoftline);
const appendWithSoftbreak = x => y => $Doc("Cat", x, $Doc("Cat", softbreak, y));
const fillCat = /* #__PURE__ */ foldr11(appendWithSoftbreak);
const appendWithLinebreak = x => y => $Doc("Cat", x, $Doc("Cat", $Doc("FlatAlt", Line, Empty), y));
const vcat = /* #__PURE__ */ foldr11(appendWithLinebreak);
const cat = x => {
  const $0 = vcat(x);
  return $Doc("Union", flatten($0), $0);
};
const appendWithLine = x => y => $Doc("Cat", x, $Doc("Cat", $Doc("FlatAlt", Line, $Doc("Char", " ")), y));
const vsep = /* #__PURE__ */ foldr11(appendWithLine);
const sep = x => {
  const $0 = vsep(x);
  return $Doc("Union", flatten($0), $0);
};
const angles = x => $Doc("Cat", $Doc("Char", "<"), $Doc("Cat", x, $Doc("Char", ">")));
const align = d => $Doc("Column", k => $Doc("Nesting", i => $Doc("Nest", k - i | 0, d)));
const encloseSep = left => right => sep$p => ds => {
  if (ds.length === 0) { return $Doc("Cat", left, right); }
  if (ds.length === 1) { return $Doc("Cat", left, $Doc("Cat", ds[0], right)); }
  const $0 = vcat(toUnfoldable(Data$dList$dLazy.zipWith(beside)((() => {
    const $0 = Data$dList$dLazy.repeat(sep$p);
    return Data$dLazy.defer(v => Data$dList$dLazy$dTypes.$Step("Cons", left, $0));
  })())(fromFoldable(ds))));
  return $Doc("Column", k => $Doc("Nesting", i => $Doc("Nest", k - i | 0, $Doc("Cat", $Doc("Union", flatten($0), $0), right))));
};
const list = /* #__PURE__ */ encloseSep(/* #__PURE__ */ $Doc("Char", "["))(/* #__PURE__ */ $Doc("Char", "]"))(/* #__PURE__ */ $Doc("Char", ","));
const semiBraces = /* #__PURE__ */ encloseSep(/* #__PURE__ */ $Doc("Char", "{"))(/* #__PURE__ */ $Doc("Char", "}"))(/* #__PURE__ */ $Doc("Char", ";"));
const tupled = /* #__PURE__ */ encloseSep(/* #__PURE__ */ $Doc("Char", "("))(/* #__PURE__ */ $Doc("Char", ")"))(/* #__PURE__ */ $Doc("Char", ","));
const hang = i => d => $Doc("Column", k => $Doc("Nesting", i$1 => $Doc("Nest", k - i$1 | 0, $Doc("Nest", i, d))));
const indent = i => d => {
  const $0 = i <= 0 ? "" : Data$dString$dCodeUnits.fromCharArray(Data$dArray.replicateImpl(i, " "));
  return $Doc(
    "Column",
    k => $Doc(
      "Nesting",
      i$1 => $Doc("Nest", k - i$1 | 0, $Doc("Nest", i, $Doc("Cat", $0 === "" ? Empty : $Doc("Text", Data$dString$dCodePoints.toCodePointArray($0).length, $0), d)))
    )
  );
};
export {
  $Doc,
  $Docs,
  $LazySimpleDoc,
  $SimpleDoc,
  Cat,
  Char,
  Column,
  Columns,
  Cons,
  Empty,
  Fail,
  FlatAlt,
  Line,
  Nest,
  Nesting,
  Nil,
  SChar,
  SChar$p,
  SCharIsSymbol,
  SEmpty,
  SEmpty$p,
  SFail,
  SFail$p,
  SLine,
  SLine$p,
  SLineIsSymbol,
  SText,
  SText$p,
  STextIsSymbol,
  Text,
  Union,
  align,
  angles,
  appendWithLine,
  appendWithLinebreak,
  appendWithSoftbreak,
  appendWithSoftline,
  appendWithSpace,
  backslash,
  beside,
  bool,
  braces,
  brackets,
  cat,
  $$char as char,
  colon,
  column,
  columns,
  comma,
  displayS,
  docMonoid,
  docSemigroup,
  docShow,
  dot,
  dquote,
  dquotes,
  empty,
  enclose,
  encloseSep,
  equals,
  fill,
  fillBreak,
  fillCat,
  fillSep,
  fits1,
  fitsR,
  flatAlt,
  flatten,
  foldr1,
  foldr11,
  forceSimpleDoc,
  fromFoldable,
  genericShowSum,
  genericShowSum1,
  genericSimpleDoc,
  group,
  hang,
  hardline,
  hcat,
  hsep,
  indent,
  indentation,
  $$int as int,
  langle,
  lbrace,
  lbracket,
  line,
  linebreak,
  list,
  lparen,
  max,
  min,
  nest,
  nesting,
  number,
  parens,
  punctuate,
  rangle,
  rbrace,
  rbracket,
  renderCompact,
  renderFits,
  renderPretty,
  renderSmart,
  rparen,
  semi,
  semiBraces,
  sep,
  showSimpleDoc,
  simpleDocEq,
  simpleDocOrd,
  softbreak,
  softline,
  space,
  spaces,
  squote,
  squotes,
  string,
  text,
  toUnfoldable,
  tupled,
  vcat,
  vsep,
  width
};
