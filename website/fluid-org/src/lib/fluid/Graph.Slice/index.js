import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Graph from "../Graph/index.js";
import * as Graph$dWithGraph from "../Graph.WithGraph/index.js";
import * as Util from "../Util/index.js";
import * as Util$dSet from "../Util.Set/index.js";
const pure = /* #__PURE__ */ (() => Control$dMonad$dState$dTrans.applicativeStateT(Data$dIdentity.monadIdentity).pure)();
const lookup = k => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Leaf") {
        go$c = false;
        go$r = Data$dMaybe.Nothing;
        continue;
      }
      if (v.tag === "Node") {
        const v1 = Data$dOrd.ordString.compare(k)(v._3);
        if (v1 === "LT") {
          go$a0 = v._5;
          continue;
        }
        if (v1 === "GT") {
          go$a0 = v._6;
          continue;
        }
        if (v1 === "EQ") {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", v._4);
          continue;
        }
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go;
};
const extend = /* #__PURE__ */ (() => Graph$dWithGraph.monadWithGraphWithGraphT(Data$dIdentity.monadIdentity).extend)();
const tailRecM = /* #__PURE__ */ (() => Control$dMonad$dState$dTrans.monadRecStateT(Control$dMonad$dRec$dClass.monadRecIdentity).tailRecM)();
const member = /* #__PURE__ */ (() => Util$dSet.setSet(Graph.ordVertex).member)();
const fromFoldable = /* #__PURE__ */ (() => Data$dSet.foldableSet.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil))();
const fwdSlice = dictGraph => {
  const runWithGraph_spy = Graph$dWithGraph.runWithGraphT_spy1(dictGraph);
  return v => {
    const $0 = v._2;
    const $1 = v._1;
    return runWithGraph_spy(tailRecM(v1 => {
      if (v1.es.tag === "Nil") { return pure(Control$dMonad$dRec$dClass.$Step("Done", undefined)); }
      if (v1.es.tag === "Cons") {
        const $2 = lookup(v1.es._1._1)(v1.pending);
        const βs = (() => {
          if ($2.tag === "Nothing") { return Data$dMap$dInternal.$$$Map("Node", 1, 1, v1.es._1._2, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf); }
          if ($2.tag === "Just") { return Data$dMap$dInternal.insert(Graph.ordVertex)(v1.es._1._2)()($2._1); }
          $runtime.fail();
        })();
        if (Data$dMap$dInternal.eqMap(Graph.eqVertex)(Data$dEq.eqUnit).eq(βs)(dictGraph.outN($0)(v1.es._1._1))) {
          return Control$dMonad$dState$dTrans.bindStateT(Data$dIdentity.monadIdentity).bind(extend(Data$dTuple.$Tuple(v1.es._1._1, dictGraph.vertexData($0)(v1.es._1._1)))(βs))(() => pure(Control$dMonad$dRec$dClass.$Step(
            "Loop",
            {
              pending: Data$dMap$dInternal.delete(Graph.ordVertex)(v1.es._1._1)(v1.pending),
              es: Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(v1.es._2)(Graph.inEdges$p(dictGraph)($0)(v1.es._1._1))
            }
          )));
        }
        return pure(Control$dMonad$dRec$dClass.$Step("Loop", {pending: Data$dMap$dInternal.insert(Graph.ordVertex)(v1.es._1._1)(βs)(v1.pending), es: v1.es._2}));
      }
      $runtime.fail();
    })({pending: Data$dMap$dInternal.Leaf, es: Graph.inEdges(dictGraph)($0)($1)}))(Util.assertWhen(false)("inputs are sinks")(v$1 => Data$dMap$dInternal.unsafeDifference(
      Graph.ordVertex.compare,
      $1,
      dictGraph.sinks($0)
    ).tag === "Leaf")(Data$dSet.map(Graph.ordDVertex$p)(α => Data$dTuple.$Tuple(α, dictGraph.vertexData($0)(α)))($1)))._1;
  };
};
const bwdSlice = dictGraph => {
  const runWithGraph_spy = Graph$dWithGraph.runWithGraphT_spy1(dictGraph);
  const addresses = Graph.addresses(dictGraph.Vertices1());
  return v => {
    const $0 = v._2;
    const $1 = v._1;
    return runWithGraph_spy(tailRecM(v1 => {
      if (v1["αs"].tag === "Nil") {
        if (v1.pending.tag === "Nil") { return pure(Control$dMonad$dRec$dClass.$Step("Done", undefined)); }
        if (v1.pending.tag === "Cons") {
          const $2 = v1.pending._1._1._2;
          const $3 = v1.pending._1._1._1;
          if (member($3)(v1.visited)) { return pure(Control$dMonad$dRec$dClass.$Step("Loop", {visited: v1.visited, "αs": Data$dList$dTypes.Nil, pending: v1.pending._2})); }
          return Control$dMonad$dState$dTrans.bindStateT(Data$dIdentity.monadIdentity).bind(extend(Data$dTuple.$Tuple($3, $2))(v1.pending._1._2))(() => pure(Control$dMonad$dRec$dClass.$Step(
            "Loop",
            {visited: Data$dMap$dInternal.insert(Graph.ordVertex)($3)()(v1.visited), "αs": Data$dList$dTypes.Nil, pending: v1.pending._2}
          )));
        }
        $runtime.fail();
      }
      if (v1["αs"].tag === "Cons") {
        const βs = dictGraph.outN($0)(v1["αs"]._1);
        return pure(Control$dMonad$dRec$dClass.$Step(
          "Loop",
          {
            visited: v1.visited,
            "αs": Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(v1["αs"]._2)(fromFoldable(βs)),
            pending: Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(Data$dTuple.$Tuple(v1["αs"]._1, dictGraph.vertexData($0)(v1["αs"]._1)), βs), v1.pending)
          }
        ));
      }
      $runtime.fail();
    })({
      visited: Data$dMap$dInternal.Leaf,
      "αs": fromFoldable(Util.assertWhen(false)("inputs are sinks")(v$1 => Data$dMap$dInternal.unsafeDifference(Graph.ordVertex.compare, $1, addresses($0)).tag === "Leaf")($1)),
      pending: Data$dList$dTypes.Nil
    }))(Data$dMap$dInternal.Leaf)._1;
  };
};
export {bwdSlice,   fwdSlice,    };
