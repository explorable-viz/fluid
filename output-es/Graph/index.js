import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Dict from "../Dict/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Util$dSet from "../Util.Set/index.js";
const fromFoldable2 = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dSet.foldableSet);
const ordTuple = dictOrd1 => {
  const $0 = dictOrd1.Eq0();
  const eqTuple2 = {eq: x => y => x._1 === y._1 && $0.eq(x._2)(y._2)};
  return {
    compare: x => y => {
      const v = Data$dOrd.ordString.compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      return dictOrd1.compare(x._2)(y._2);
    },
    Eq0: () => eqTuple2
  };
};
const fromFoldable3 = /* #__PURE__ */ (() => Data$dSet.foldableSet.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil))();
const Vertex = x => x;
const VertexData = x => x;
const DVertex = x => x;
const typeNameUnit = {typeName: v => "Unit"};
const showVertex = Data$dShow.showString;
const newtypeVertex_ = {Coercible0: () => {}};
const newtypeDVertex$p_ = {Coercible0: () => {}};
const eqVertex = {eq: x => y => x === y};
const ordVertex = {compare: x => y => Data$dOrd.ordString.compare(x)(y), Eq0: () => eqVertex};
const mempty = /* #__PURE__ */ (() => Data$dSet.monoidSet(ordVertex).mempty)();
const unions = /* #__PURE__ */ Data$dSet.unions(Dict.foldableDict)(ordVertex);
const map3 = /* #__PURE__ */ Data$dSet.map(/* #__PURE__ */ (() => {
  const eqTuple2 = {eq: x => y => x._1 === y._1 && x._2 === y._2};
  return {
    compare: x => y => {
      const v = Data$dOrd.ordString.compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      return Data$dOrd.ordString.compare(x._2)(y._2);
    },
    Eq0: () => eqTuple2
  };
})());
const eqDVertex$p = {eq: v => v1 => v._1 === v1._1};
const ordDVertex$p = {compare: v => v1 => Data$dOrd.ordString.compare(v._1)(v1._1), Eq0: () => eqDVertex$p};
const unions1 = /* #__PURE__ */ Data$dSet.unions(Data$dFoldable.foldableArray)(ordDVertex$p);
const member = /* #__PURE__ */ (() => Util$dSet.setSet(ordDVertex$p).member)();
const verticesDVertex = dictFunctor => dictFoldable => ({vertices: dictFoldable.foldl(m => a => Data$dMap$dInternal.insert(ordDVertex$p)(a)()(m))(Data$dMap$dInternal.Leaf)});
const vertices = dict => dict.vertices;
const verticesDict = dictVertices => {
  const vertices1 = dictVertices.vertices;
  return {vertices: d => unions1(Data$dFunctor.arrayMap(vertices1)(Foreign$dObject.values(d)))};
};
const vertexData = dict => dict.vertexData;
const unpack = f => v => v(dictTypeName => f(dictTypeName));
const typeName = dict => dict.typeName;
const typeNameVertexData = {typeName: v => v(dictTypeName => dictTypeName.typeName)};
const topologicalSort = dict => dict.topologicalSort;
const sources = dict => dict.sources;
const size = dict => dict.size;
const sinks = dict => dict.sinks;
const showVertices = αs => "{" + Data$dString$dCommon.joinWith(", ")(Data$dArray.fromFoldableImpl(
  Data$dSet.foldableSet.foldr,
  Data$dSet.map(Data$dOrd.ordString)(Unsafe$dCoerce.unsafeCoerce)(αs)
)) + "}";
const showEdgeList = es => Data$dString$dCommon.joinWith("\n")([
  "digraph G {",
  ...Data$dFunctor.arrayMap(v => "   " + v)([
    "rankdir = RL",
    ...Data$dFunctor.arrayMap(v => v._1._1 + " -> {" + Data$dString$dCommon.joinWith(", ")(Data$dArray.fromFoldableImpl(
      Data$dSet.foldableSet.foldr,
      Data$dSet.map(Data$dOrd.ordString)(Unsafe$dCoerce.unsafeCoerce)(v._2)
    )) + "}")((() => {
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
      return Data$dArray.fromFoldableImpl(Data$dList$dTypes.foldableList.foldr, go(Data$dList$dTypes.Nil)(es));
    })())
  ]),
  "}"
]);
const select𝔹s = dict => dict["select𝔹s"];
const selectαs = dict => dict["selectαs"];
const runQuery = dictOrd => {
  const mapMaybe = Data$dSet.mapMaybe(ordTuple(dictOrd));
  return query => αs => fromFoldable2(mapMaybe(v => {
    const $0 = query(v._2);
    if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(v._1, $0._1._2)); }
    if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  })(αs));
};
const pack = dictTypeName => x => k => k(dictTypeName)(x);
const pack1 = x => k => k(typeNameUnit)(x);
const selectαs𝔹Vertex = dictApply => {
  const $0 = dictApply.Functor0();
  return dictFoldable => {
    const unions2 = Data$dSet.unions(dictFoldable)(ordVertex);
    return {
      "selectαs": v𝔹 => vα => unions2(dictApply.apply($0.map(v => {
        if (v) { return Data$dSet.singleton; }
        return v$1 => mempty;
      })(v𝔹))(vα)),
      "select𝔹s": vα => αs => $0.map(α => member(Data$dTuple.$Tuple(α, pack1()))(αs))(vα)
    };
  };
};
const selectαsDict𝔹DictVertex = dictFunctor => dictApply => {
  const selectαs𝔹Vertex1 = selectαs𝔹Vertex(dictApply);
  return dictFoldable => {
    const selectαs𝔹Vertex2 = selectαs𝔹Vertex1(dictFoldable);
    const selectαs1 = selectαs𝔹Vertex2["selectαs"];
    return {
      "selectαs": d𝔹 => dα => unions(Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(d𝔹, selectαs1))(dα)),
      "select𝔹s": dα => αs => Foreign$dObject._fmapObject(dα, a => selectαs𝔹Vertex2["select𝔹s"](a)(αs))
    };
  };
};
const outN = dict => dict.outN;
const toEdgeList = dictGraph => g => {
  const $0 = v => {
    if (v._1.tag === "Nil") { return Control$dMonad$dRec$dClass.$Step("Done", v._2); }
    if (v._1.tag === "Cons") {
      return Control$dMonad$dRec$dClass.$Step(
        "Loop",
        Data$dTuple.$Tuple(
          v._1._2,
          Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(Data$dTuple.$Tuple(v._1._1, dictGraph.vertexData(g)(v._1._1)), dictGraph.outN(g)(v._1._1)), v._2)
        )
      );
    }
    $runtime.fail();
  };
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Loop") {
        go$a0 = $0(v._1);
        continue;
      }
      if (v.tag === "Done") {
        go$c = false;
        go$r = v._1;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go($0(Data$dTuple.$Tuple(dictGraph.topologicalSort(g), Data$dList$dTypes.Nil)));
};
const showGraph = dictGraph => x => showEdgeList(toEdgeList(dictGraph)(x));
const op = dict => dict.op;
const inN = dict => dict.inN;
const inEdges$p = dictGraph => g => α => fromFoldable3(map3(v => Data$dTuple.$Tuple(v, α))(dictGraph.inN(g)(α)));
const inEdges = dictGraph => g => αs => {
  const $0 = v => {
    if (v._1.tag === "Nil") { return Control$dMonad$dRec$dClass.$Step("Done", v._2); }
    if (v._1.tag === "Cons") {
      return Control$dMonad$dRec$dClass.$Step(
        "Loop",
        Data$dTuple.$Tuple(v._1._2, Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(v._2)(inEdges$p(dictGraph)(g)(v._1._1)))
      );
    }
    $runtime.fail();
  };
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Loop") {
        go$a0 = $0(v._1);
        continue;
      }
      if (v.tag === "Done") {
        go$c = false;
        go$r = v._1;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go($0(Data$dTuple.$Tuple(fromFoldable3(αs), Data$dList$dTypes.Nil)));
};
const outEdges = dictGraph => g => inEdges(dictGraph)(dictGraph.op(g));
const fromEdgeList = dict => dict.fromEdgeList;
const empty = dict => dict.empty;
const elem = dict => dict.elem;
const dvertices = dictGraph => g => αs => Data$dSet.map(ordDVertex$p)(α => Data$dTuple.$Tuple(α, dictGraph.vertexData(g)(α)))(αs);
const addresses = dictVertices => {
  const $0 = Data$dSet.map(ordVertex)(x => x._1);
  return x => $0(dictVertices.vertices(x));
};
export {
  DVertex,
  Vertex,
  VertexData,
  addresses,
  dvertices,
  elem,
  empty,
  eqDVertex$p,
  eqVertex,
  fromEdgeList,
  fromFoldable2,
  fromFoldable3,
  inEdges,
  inEdges$p,
  inN,
  map3,
  member,
  mempty,
  newtypeDVertex$p_,
  newtypeVertex_,
  op,
  ordDVertex$p,
  ordTuple,
  ordVertex,
  outEdges,
  outN,
  pack,
  pack1,
  runQuery,
  selectαs,
  selectαsDict𝔹DictVertex,
  selectαs𝔹Vertex,
  select𝔹s,
  showEdgeList,
  showGraph,
  showVertex,
  showVertices,
  sinks,
  size,
  sources,
  toEdgeList,
  topologicalSort,
  typeName,
  typeNameUnit,
  typeNameVertexData,
  unions,
  unions1,
  unpack,
  vertexData,
  vertices,
  verticesDVertex,
  verticesDict
};
