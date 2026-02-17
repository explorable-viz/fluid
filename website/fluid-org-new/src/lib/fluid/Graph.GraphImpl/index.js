import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Control$dMonad$dST$dInternal from "../Control.Monad.ST.Internal/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dGraph from "../Data.Graph/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Foreign$dObject$dST from "../Foreign.Object.ST/index.js";
import * as Graph from "../Graph/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
const $GraphImpl = _1 => ({tag: "GraphImpl", _1});
const eqSet = {eq: v => v1 => Data$dMap$dInternal.eqMap(Graph.eqVertex)(Data$dEq.eqUnit).eq(v)(v1)};
const eq = /* #__PURE__ */ (() => Foreign$dObject.eqObject(eqSet).eq)();
const fromFoldable1 = /* #__PURE__ */ Data$dFoldable.foldlArray(m => a => Data$dMap$dInternal.insert(Graph.ordVertex)(a)()(m))(Data$dMap$dInternal.Leaf);
const mempty = /* #__PURE__ */ (() => Data$dSet.monoidSet(Graph.ordVertex).mempty)();
const toUnfoldable = x => {
  const go = (m$p, z$p) => {
    if (m$p.tag === "Leaf") { return z$p; }
    if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go(m$p._6, z$p))); }
    $runtime.fail();
  };
  const go$1 = go$1$a0$copy => go$1$a1$copy => {
    let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
    while (go$1$c) {
      const source = go$1$a0, memo = go$1$a1;
      if (source.tag === "Nil") {
        const go$2 = go$2$a0$copy => go$2$a1$copy => {
          let go$2$a0 = go$2$a0$copy, go$2$a1 = go$2$a1$copy, go$2$c = true, go$2$r;
          while (go$2$c) {
            const b = go$2$a0, v = go$2$a1;
            if (v.tag === "Nil") {
              go$2$c = false;
              go$2$r = b;
              continue;
            }
            if (v.tag === "Cons") {
              go$2$a0 = Data$dList$dTypes.$List("Cons", v._1, b);
              go$2$a1 = v._2;
              continue;
            }
            $runtime.fail();
          }
          return go$2$r;
        };
        go$1$c = false;
        go$1$r = go$2(Data$dList$dTypes.Nil)(memo);
        continue;
      }
      if (source.tag === "Cons") {
        go$1$a0 = source._2;
        go$1$a1 = Data$dList$dTypes.$List("Cons", source._1, memo);
        continue;
      }
      $runtime.fail();
    }
    return go$1$r;
  };
  return go$1(go(x, Data$dList$dTypes.Nil))(Data$dList$dTypes.Nil);
};
const fromFoldable2 = /* #__PURE__ */ (() => Data$dSet.foldableSet.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil))();
const toUnfoldable1 = /* #__PURE__ */ Foreign$dObject.toAscUnfoldable(Data$dUnfoldable.unfoldableArray);
const fromFoldable3 = /* #__PURE__ */ Data$dMap$dInternal.fromFoldable(Graph.ordVertex)(Data$dFoldable.foldableArray);
const GraphImpl = value0 => $GraphImpl(value0);
const verticesGraphImpl = {
  vertices: v => Foreign$dObject.fold(z => v$1 => a => Data$dMap$dInternal.insert(Graph.ordDVertex$p)(a)()(z))(Data$dMap$dInternal.Leaf)(Foreign$dObject._mapWithKey(
    v._1.out,
    k => v1 => Data$dTuple.$Tuple(k, v1._2)
  ))
};
const eqGraphImpl = {eq: v => v1 => eq(Foreign$dObject._fmapObject(v._1.out, Data$dTuple.fst))(Foreign$dObject._fmapObject(v1._1.out, Data$dTuple.fst))};
const sinks$p = m => fromFoldable1(Data$dFunctor.arrayMap(x => x._1)(Data$dArray.filterImpl(x => x._2._1.tag === "Leaf", Foreign$dObject.toArrayWithKey(Data$dTuple.Tuple)(m))));
const init = αs => () => {
  const obj = {};
  return Control$dMonad$dST$dInternal.monadRecST.tailRecM(v => {
    if (v._1.tag === "Nil") { return () => Control$dMonad$dRec$dClass.$Step("Done", v._2); }
    if (v._1.tag === "Cons") {
      const $0 = v._1._1._1;
      const $1 = v._1._2;
      const $2 = v._2;
      return () => {
        $2[$0] = Data$dTuple.$Tuple(mempty, v._1._1._2);
        return Control$dMonad$dRec$dClass.$Step("Loop", Data$dTuple.$Tuple($1, $2));
      };
    }
    $runtime.fail();
  })(Data$dTuple.$Tuple(αs, obj))();
};
const assertPresent = v => v1 => {
  if (v1.tag === "Nil") { return () => Control$dMonad$dRec$dClass.$Step("Done", undefined); }
  if (v1.tag === "Cons") {
    const $0 = v1._1;
    const $1 = v1._2;
    const $2 = Foreign$dObject$dST.peek($0)(v);
    return () => {
      const $3 = $2();
      const present = (() => {
        if ($3.tag === "Nothing") { return false; }
        if ($3.tag === "Just") { return true; }
        $runtime.fail();
      })();
      return Util.assertWhen(false)($0 + " is an existing vertex")(v2 => present)(() => Control$dMonad$dRec$dClass.$Step("Loop", $1))();
    };
  }
  $runtime.fail();
};
const outMap = αs => es => {
  const $0 = init(αs);
  return () => {
    const out = $0();
    return Control$dMonad$dST$dInternal.monadRecST.tailRecM(v => {
      if (v._1.tag === "Nil") { return () => Control$dMonad$dRec$dClass.$Step("Done", v._2); }
      if (v._1.tag === "Cons") {
        const $1 = v._2;
        const $2 = v._1._2;
        const $3 = v._1._1._1._2;
        const $4 = v._1._1._1._1;
        const $5 = v._1._1._2;
        const $6 = Foreign$dObject$dST.peek($4)($1);
        return () => {
          const $7 = $6();
          if (
            (() => {
              if ($7.tag === "Nothing") { return true; }
              if ($7.tag === "Just") { return Data$dMap$dInternal.eqMap(Graph.eqVertex)(Data$dEq.eqUnit).eq($7._1._1)(mempty); }
              $runtime.fail();
            })()
          ) {
            Control$dMonad$dST$dInternal.monadRecST.tailRecM(assertPresent($1))(toUnfoldable($5))();
            $1[$4] = Data$dTuple.$Tuple($5, $3);
            return Control$dMonad$dRec$dClass.$Step("Loop", Data$dTuple.$Tuple($2, $1));
          }
          return Effect$dException.throwException(Effect$dException.error("Duplicate edge list entry for " + Data$dShow.showStringImpl($4)))()();
        };
      }
      $runtime.fail();
    })(Data$dTuple.$Tuple(es, out))();
  };
};
const addIfMissing = acc => v => {
  const $0 = v._2;
  const $1 = v._1;
  const $2 = Foreign$dObject$dST.peek($1)(acc);
  return () => {
    const v1 = $2();
    if (v1.tag === "Nothing") {
      acc[$1] = Data$dTuple.$Tuple(mempty, $0);
      return acc;
    }
    if (v1.tag === "Just") { return acc; }
    $runtime.fail();
  };
};
const inMap = αs => es => {
  const $0 = init(αs);
  return () => {
    const in_ = $0();
    return Control$dMonad$dST$dInternal.monadRecST.tailRecM(v => {
      if (v._1.tag === "Nil") { return () => Control$dMonad$dRec$dClass.$Step("Done", v._2); }
      if (v._1.tag === "Cons") {
        const $1 = v._1._2;
        const $2 = v._1._1._1._2;
        const $3 = v._1._1._1._1;
        const $4 = Control$dMonad$dST$dInternal.monadRecST.tailRecM(v2 => {
          if (v2._1.tag === "Nil") { return () => Control$dMonad$dRec$dClass.$Step("Done", v2._2); }
          if (v2._1.tag === "Cons") {
            const $4 = v2._2;
            const $5 = v2._1._1;
            const $6 = v2._1._2;
            const $7 = Foreign$dObject$dST.peek($5)($4);
            return () => {
              const v1 = $7();
              const acc$p = (() => {
                if (v1.tag === "Nothing") {
                  $4[$5] = Data$dTuple.$Tuple(Data$dMap$dInternal.$$$Map("Node", 1, 1, $3, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf), $2);
                  return $4;
                }
                if (v1.tag === "Just") {
                  $4[$5] = Data$dTuple.$Tuple(Data$dMap$dInternal.insert(Graph.ordVertex)($3)()(v1._1._1), $2);
                  return $4;
                }
                $runtime.fail();
              })();
              return Control$dMonad$dRec$dClass.$Step("Loop", Data$dTuple.$Tuple($6, acc$p));
            };
          }
          $runtime.fail();
        })(Data$dTuple.$Tuple(toUnfoldable(v._1._1._2), v._2));
        return () => {
          const $5 = $4();
          const acc$p = addIfMissing($5)(Data$dTuple.$Tuple($3, $2))();
          return Control$dMonad$dRec$dClass.$Step("Loop", Data$dTuple.$Tuple($1, acc$p));
        };
      }
      $runtime.fail();
    })(Data$dTuple.$Tuple(es, in_))();
  };
};
const graphGraphImpl = {
  outN: v => α => Util.definitely("in graph")(Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, α, v._1.out))._1,
  vertexData: v => α => Util.definitely("in graph")(Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, α, v._1.out))._2,
  inN: g => graphGraphImpl.outN(graphGraphImpl.op(g)),
  elem: α => v => {
    const $0 = Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, α, v._1.out);
    if ($0.tag === "Nothing") { return false; }
    if ($0.tag === "Just") { return true; }
    $runtime.fail();
  },
  size: v => Foreign$dObject.size(v._1.out),
  sinks: v => v._1.sinks,
  sources: v => v._1.sources,
  op: v => $GraphImpl({out: v._1.in_, in_: v._1.out, sinks: v._1.sources, sources: v._1.sinks, vertices: v._1.vertices}),
  empty: /* #__PURE__ */ $GraphImpl({out: Foreign$dObject.empty, in_: Foreign$dObject.empty, sinks: mempty, sources: mempty, vertices: mempty}),
  fromEdgeList: αs => es => {
    const αs$p = fromFoldable2(αs);
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0, v1 = go$a1;
        if (v1.tag === "Nil") {
          go$c = false;
          go$r = v;
          continue;
        }
        if (v1.tag === "Cons") {
          go$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
          go$a1 = v1._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    const es$p = go(Data$dList$dTypes.Nil)(es);
    const in_ = inMap(αs$p)(es$p)();
    const out = outMap(αs$p)(es$p)();
    return $GraphImpl({out, in_, sinks: sinks$p(out), sources: sinks$p(in_), vertices: Data$dSet.map(Graph.ordVertex)(Graph.Vertex)(Util$dMap.mapObjectString.keys(out))});
  },
  topologicalSort: v => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const v$1 = go$a0, v1 = go$a1;
        if (v1.tag === "Nil") {
          go$c = false;
          go$r = v$1;
          continue;
        }
        if (v1.tag === "Cons") {
          go$a0 = Data$dList$dTypes.$List("Cons", v1._1, v$1);
          go$a1 = v1._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go(Data$dList$dTypes.Nil)(Data$dGraph.topologicalSort(Graph.ordVertex)(fromFoldable3(Data$dFunctor.arrayMap(x => Data$dTuple.$Tuple(
      x._1,
      Data$dTuple.$Tuple(undefined, x._2)
    ))(toUnfoldable1(Foreign$dObject._fmapObject(Foreign$dObject._fmapObject(v._1.out, Data$dTuple.fst), toUnfoldable))))));
  },
  Eq0: () => eqGraphImpl,
  Vertices1: () => verticesGraphImpl
};
export {
  $GraphImpl,
  
  
  
  
  
  
  
  
  
  graphGraphImpl,
  
  
  
  
  
  
  
  verticesGraphImpl
};
