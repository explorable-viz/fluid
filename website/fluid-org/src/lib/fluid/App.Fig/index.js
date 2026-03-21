import * as $runtime from "../runtime.js";
import * as App$dCodeMirror from "../App.CodeMirror/index.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as App$dView from "../App.View/index.js";
import * as App$dView$dUtil from "../App.View.Util/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dHeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Dict from "../Dict/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Eval from "../Eval/index.js";
import * as Expr from "../Expr/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Graph from "../Graph/index.js";
import * as Graph$dGraphImpl from "../Graph.GraphImpl/index.js";
import * as Graph$dSlice from "../Graph.Slice/index.js";
import * as Lattice from "../Lattice/index.js";
import * as Module from "../Module/index.js";
import * as Pretty from "../Pretty/index.js";
import * as Pretty$dDoc from "../Pretty.Doc/index.js";
import * as Primitive$dDefs from "../Primitive.Defs/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Util$dSet from "../Util.Set/index.js";
import * as Val from "../Val/index.js";
const fromFoldable = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dSet.foldableSet);
const ordVal = /* #__PURE__ */ Val.ordVal(Graph.ordVertex);
const mapMaybe = /* #__PURE__ */ Data$dSet.mapMaybe(/* #__PURE__ */ (() => {
  const $0 = ordVal.Eq0();
  const eqTuple2 = {eq: x => y => x._1 === y._1 && $0.eq(x._2)(y._2)};
  return {
    compare: x => y => {
      const v = Data$dOrd.ordString.compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      return ordVal.compare(x._2)(y._2);
    },
    Eq0: () => eqTuple2
  };
})());
const botOf2 = {
  botOf: m => {
    if (m.tag === "Inert") { return App$dUtil.Inert; }
    if (m.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: false, transient: false}); }
    $runtime.fail();
  }
};
const select𝔹s = /* #__PURE__ */ (() => Graph.selectαs𝔹Vertex(Val.applyVal)(Val.foldableVal)["select𝔹s"])();
const fromFoldable1 = /* #__PURE__ */ Foreign$dObject.fromFoldable(Dict.foldableDict);
const fromFoldable2 = /* #__PURE__ */ Data$dFoldable.foldlArray(m => a => Data$dMap$dInternal.insert(Data$dOrd.ordString)(a)()(m))(Data$dMap$dInternal.Leaf);
const graphGC = /* #__PURE__ */ Eval.graphGC(Graph$dGraphImpl.graphGraphImpl);
const graphGC1 = /* #__PURE__ */ graphGC(Val.applyVal)(Val.applyEnvExpr)(Val.foldableVal)(Val.foldableEnvExpr);
const graphGC2 = /* #__PURE__ */ graphGC(Val.applyEnvExpr)(Val.applyVal)(Val.foldableEnvExpr)(Val.foldableVal);
const deMorgan = x => x$1 => Val.functorEnvExpr.map(Data$dHeytingAlgebra.boolNot)(x(Foreign$dObject._fmapObject(x$1, Val.functorVal.map(Data$dHeytingAlgebra.boolNot))));
const setSet = /* #__PURE__ */ Util$dSet.setSet(Graph.ordDVertex$p);
const selectαs𝔹Vertex = /* #__PURE__ */ Graph.selectαs𝔹Vertex(Val.applyEnv)(Val.foldableEnv);
const bwdSlice = /* #__PURE__ */ Graph$dSlice.bwdSlice(Graph$dGraphImpl.graphGraphImpl);
const member = /* #__PURE__ */ (() => Util$dSet.setSet(Graph.ordVertex).member)();
const runQuery = /* #__PURE__ */ Graph.runQuery(ordVal);
const primary = x => Foreign$dObject._fmapObject(
  x,
  Val.functorVal.map(v => {
    if (v.tag === "Inert") { return App$dUtil.Inert; }
    if (v.tag === "Reactive") { return App$dUtil.$SelState("Reactive", v._1 ? App$dUtil.Primary : App$dUtil.None); }
    $runtime.fail();
  })
);
const primary1 = x => Val.functorVal.map(v => {
  if (v.tag === "Inert") { return App$dUtil.Inert; }
  if (v.tag === "Reactive") { return App$dUtil.$SelState("Reactive", v._1 ? App$dUtil.Primary : App$dUtil.None); }
  $runtime.fail();
})(x);
const primaryOrSecondary = /* #__PURE__ */ App$dUtil.primaryOrSecondary(Val.applyEnv);
const primaryOrSecondary1 = /* #__PURE__ */ App$dUtil.primaryOrSecondary(Val.applyVal);
const difference1 = /* #__PURE__ */ (() => Util$dSet.setSet(Data$dOrd.ordString).difference)();
const sequence_ = /* #__PURE__ */ Data$dFoldable.traverse_(Effect.applicativeEffect)(Dict.foldableDict)(Data$dFoldable.identity);
const for_ = /* #__PURE__ */ Data$dFoldable.for_(Effect.applicativeEffect)(Data$dSet.foldableSet);
const ιfromαs = dictGraph => g => {
  const $0 = mapMaybe(α => {
    const $0 = Val.asVal(dictGraph.vertexData(g)(α));
    if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(α, $0._1)); }
    return Data$dMaybe.Nothing;
  });
  return x => fromFoldable($0(x));
};
const unprojExpr = dictBoundedMeetSemilattice => v => {
  const $0 = v._2;
  return {
    fwd: γ => Val.$EnvExpr(
      γ,
      Expr.functorExpr.map((() => {
        const $1 = dictBoundedMeetSemilattice.top;
        return v$1 => $1;
      })())($0)
    ),
    bwd: v1 => v1._1
  };
};
const str = {output: "output", input: "input", intermediate: "intermediate"};
const setOutputView = δvw => fig => (
  {
    spec: fig.spec,
    s: fig.s,
    "γ": fig["γ"],
    v: fig.v,
    "ι": fig["ι"],
    dir: fig.dir,
    linkedInputs: fig.linkedInputs,
    linkedOutputs: fig.linkedOutputs,
    linkIntermediates: fig.linkIntermediates,
    in_views: fig.in_views,
    in_roots: fig.in_roots,
    out_view: fig.out_view.tag === "Just" ? Data$dMaybe.$Maybe("Just", δvw(fig.out_view._1)) : Data$dMaybe.Nothing,
    intermediate_views: fig.intermediate_views,
    inerts: fig.inerts
  }
);
const setIntermediateView = v => δvw => fig => (
  {
    spec: fig.spec,
    s: fig.s,
    "γ": fig["γ"],
    v: fig.v,
    "ι": fig["ι"],
    dir: fig.dir,
    linkedInputs: fig.linkedInputs,
    linkedOutputs: fig.linkedOutputs,
    linkIntermediates: fig.linkIntermediates,
    in_views: fig.in_views,
    in_roots: fig.in_roots,
    out_view: fig.out_view,
    intermediate_views: (() => {
      const $0 = Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, v, fig.intermediate_views);
      const $1 = (() => {
        if ($0.tag === "Just") { return $0._1; }
        if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
        $runtime.fail();
      })();
      const $2 = $1.tag === "Just" ? Data$dMaybe.$Maybe("Just", δvw($1._1)) : Data$dMaybe.Nothing;
      return Foreign$dObject.mutate($3 => () => {
        $3[v] = $2;
        return $3;
      })(fig.intermediate_views);
    })(),
    inerts: fig.inerts
  }
);
const setInputView = x => δvw => fig => (
  {
    spec: fig.spec,
    s: fig.s,
    "γ": fig["γ"],
    v: fig.v,
    "ι": fig["ι"],
    dir: fig.dir,
    linkedInputs: fig.linkedInputs,
    linkedOutputs: fig.linkedOutputs,
    linkIntermediates: fig.linkIntermediates,
    in_views: (() => {
      const $0 = Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, x, fig.in_views);
      const $1 = (() => {
        if ($0.tag === "Just") { return $0._1; }
        if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
        $runtime.fail();
      })();
      const $2 = $1.tag === "Just" ? Data$dMaybe.$Maybe("Just", δvw($1._1)) : Data$dMaybe.Nothing;
      return Foreign$dObject.mutate($3 => () => {
        $3[x] = $2;
        return $3;
      })(fig.in_views);
    })(),
    in_roots: fig.in_roots,
    out_view: fig.out_view,
    intermediate_views: fig.intermediate_views,
    inerts: fig.inerts
  }
);
const selectOutput = δv => v => {
  const v2 = δv(v.v);
  if (v2._2 === "Persistent") {
    if (v.dir.persistent === "LinkedInputs" || v.dir.persistent !== "LinkedOutputs") {
      return {
        spec: v.spec,
        s: v.s,
        "γ": Foreign$dObject._fmapObject(v["γ"], Val.functorVal.map(botOf2.botOf)),
        v: v2._1,
        "ι": v["ι"],
        dir: {persistent: App$dView$dUtil.LinkedOutputs, transient: v.dir.transient},
        linkedInputs: v.linkedInputs,
        linkedOutputs: v.linkedOutputs,
        linkIntermediates: v.linkIntermediates,
        in_views: v.in_views,
        in_roots: v.in_roots,
        out_view: v.out_view,
        intermediate_views: v.intermediate_views,
        inerts: v.inerts
      };
    }
    return {
      spec: v.spec,
      s: v.s,
      "γ": v["γ"],
      v: v2._1,
      "ι": v["ι"],
      dir: v.dir,
      linkedInputs: v.linkedInputs,
      linkedOutputs: v.linkedOutputs,
      linkIntermediates: v.linkIntermediates,
      in_views: v.in_views,
      in_roots: v.in_roots,
      out_view: v.out_view,
      intermediate_views: v.intermediate_views,
      inerts: v.inerts
    };
  }
  if (v2._2 === "Transient" && (v.dir.transient === "LinkedInputs" || v.dir.transient !== "LinkedOutputs")) {
    return {
      spec: v.spec,
      s: v.s,
      "γ": v["γ"],
      v: v2._1,
      "ι": v["ι"],
      dir: {transient: App$dView$dUtil.LinkedOutputs, persistent: v.dir.persistent},
      linkedInputs: v.linkedInputs,
      linkedOutputs: v.linkedOutputs,
      linkIntermediates: v.linkIntermediates,
      in_views: v.in_views,
      in_roots: v.in_roots,
      out_view: v.out_view,
      intermediate_views: v.intermediate_views,
      inerts: v.inerts
    };
  }
  return {
    spec: v.spec,
    s: v.s,
    "γ": v["γ"],
    v: v2._1,
    "ι": v["ι"],
    dir: v.dir,
    linkedInputs: v.linkedInputs,
    linkedOutputs: v.linkedOutputs,
    linkIntermediates: v.linkIntermediates,
    in_views: v.in_views,
    in_roots: v.in_roots,
    out_view: v.out_view,
    intermediate_views: v.intermediate_views,
    inerts: v.inerts
  };
};
const selectIntermediate = v => δv => v1 => {
  const v3 = App$dUtil$dSelector.envVal(v)(δv)(v1["ι"]);
  if (v3._2 === "Transient") {
    if (v1.dir.transient === "LinkedInputs" || v1.dir.transient === "LinkedOutputs" || v1.dir.transient !== "Intermediates") {
      return {
        spec: v1.spec,
        s: v1.s,
        "γ": v1["γ"],
        v: v1.v,
        "ι": v3._1,
        dir: {transient: App$dView$dUtil.Intermediates, persistent: v1.dir.persistent},
        linkedInputs: v1.linkedInputs,
        linkedOutputs: v1.linkedOutputs,
        linkIntermediates: v1.linkIntermediates,
        in_views: v1.in_views,
        in_roots: v1.in_roots,
        out_view: v1.out_view,
        intermediate_views: v1.intermediate_views,
        inerts: v1.inerts
      };
    }
    return {
      spec: v1.spec,
      s: v1.s,
      "γ": v1["γ"],
      v: v1.v,
      "ι": v3._1,
      dir: v1.dir,
      linkedInputs: v1.linkedInputs,
      linkedOutputs: v1.linkedOutputs,
      linkIntermediates: v1.linkIntermediates,
      in_views: v1.in_views,
      in_roots: v1.in_roots,
      out_view: v1.out_view,
      intermediate_views: v1.intermediate_views,
      inerts: v1.inerts
    };
  }
  return {
    spec: v1.spec,
    s: v1.s,
    "γ": v1["γ"],
    v: v1.v,
    "ι": v1["ι"],
    dir: v1.dir,
    linkedInputs: v1.linkedInputs,
    linkedOutputs: v1.linkedOutputs,
    linkIntermediates: v1.linkIntermediates,
    in_views: v1.in_views,
    in_roots: v1.in_roots,
    out_view: v1.out_view,
    intermediate_views: v1.intermediate_views,
    inerts: v1.inerts
  };
};
const selectInput = x => δv => v => {
  const v2 = App$dUtil$dSelector.envVal(x)(δv)(v["γ"]);
  if (v2._2 === "Persistent") {
    if (v.dir.persistent !== "LinkedInputs") {
      return {
        spec: v.spec,
        s: v.s,
        "γ": v2._1,
        v: Val.functorVal.map(botOf2.botOf)(v.v),
        "ι": v["ι"],
        dir: {persistent: App$dView$dUtil.LinkedInputs, transient: v.dir.transient},
        linkedInputs: v.linkedInputs,
        linkedOutputs: v.linkedOutputs,
        linkIntermediates: v.linkIntermediates,
        in_views: v.in_views,
        in_roots: v.in_roots,
        out_view: v.out_view,
        intermediate_views: v.intermediate_views,
        inerts: v.inerts
      };
    }
    return {
      spec: v.spec,
      s: v.s,
      "γ": v2._1,
      v: v.v,
      "ι": v["ι"],
      dir: v.dir,
      linkedInputs: v.linkedInputs,
      linkedOutputs: v.linkedOutputs,
      linkIntermediates: v.linkIntermediates,
      in_views: v.in_views,
      in_roots: v.in_roots,
      out_view: v.out_view,
      intermediate_views: v.intermediate_views,
      inerts: v.inerts
    };
  }
  if (v2._2 === "Transient" && v.dir.transient !== "LinkedInputs") {
    return {
      spec: v.spec,
      s: v.s,
      "γ": v2._1,
      v: v.v,
      "ι": v["ι"],
      dir: {transient: App$dView$dUtil.LinkedInputs, persistent: v.dir.persistent},
      linkedInputs: v.linkedInputs,
      linkedOutputs: v.linkedOutputs,
      linkIntermediates: v.linkIntermediates,
      in_views: v.in_views,
      in_roots: v.in_roots,
      out_view: v.out_view,
      intermediate_views: v.intermediate_views,
      inerts: v.inerts
    };
  }
  return {
    spec: v.spec,
    s: v.s,
    "γ": v2._1,
    v: v.v,
    "ι": v["ι"],
    dir: v.dir,
    linkedInputs: v.linkedInputs,
    linkedOutputs: v.linkedOutputs,
    linkIntermediates: v.linkIntermediates,
    in_views: v.in_views,
    in_roots: v.in_roots,
    out_view: v.out_view,
    intermediate_views: v.intermediate_views,
    inerts: v.inerts
  };
};
const rebuildι = inerts => αs => ι => fromFoldable1(Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(
  Foreign$dObject._fmapObject(ι, v => select𝔹s(v)(inerts)),
  inert => v => Data$dTuple.$Tuple(v._1, Val.applyVal.apply(Val.applyVal.apply(Val.functorVal.map(App$dUtil.selStates)(inert))(v._2.persistent))(v._2.transient))
))(Foreign$dObject._fmapObject(ι, v => Data$dTuple.$Tuple(v._1, {persistent: select𝔹s(v)(αs.persistent), transient: select𝔹s(v)(αs.transient)}))));
const loadCode = s => ed => {
  const $0 = App$dCodeMirror.dispatch(ed);
  const $1 = App$dCodeMirror.update(ed.state)([{changes: {from: 0, to: App$dCodeMirror.getContentsLength(ed), insert: s}}]);
  return () => {
    const $2 = $1();
    return $0($2)();
  };
};
const lift = dictApply => dictApply1 => {
  const $0 = dictApply1.Functor0();
  return selState_f => f => v => {
    const $1 = f($0.map(App$dUtil.to𝔹)(v));
    return Data$dTuple.$Tuple(dictApply.apply(selState_f)($1._1), $1._2);
  };
};
const lift1 = /* #__PURE__ */ lift(Val.applyEnv)(Val.applyVal);
const lift2 = /* #__PURE__ */ lift(Val.applyVal)(Val.applyEnv);
const loadFig = dictMonadAff => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const $0 = Monad0.Bind1();
  const prepConfig = Module.prepConfig(dictMonadAff);
  const graphEval = Eval.graphEval(dictMonadAff);
  return dictMonadError => {
    const prepConfig1 = prepConfig(dictMonadError);
    return dictMonadReader => {
      const prepConfig2 = prepConfig1(dictMonadReader);
      const graphEval1 = graphEval(dictMonadReader);
      return dictLoadFile => {
        const prepConfig3 = prepConfig2(dictLoadFile);
        const graphEval2 = graphEval1(dictLoadFile)(dictMonadError);
        return v => fluidSrc => {
          const $1 = v.inputs;
          const $2 = v.linking;
          return $0.bind(prepConfig3(Primitive$dDefs.primitives)(fluidSrc))(v1 => {
            const $3 = v1.s;
            return $0.bind(graphEval2(v1.gconfig)(v1.e))(v2 => {
              const $4 = v2.g;
              const $5 = v2["outα"];
              const $6 = v2["inα"]._1;
              const opEval = {
                g: Graph$dGraphImpl.$GraphImpl({out: v2.g._1.in_, in_: v2.g._1.out, sinks: v2.g._1.sources, sources: v2.g._1.sinks, vertices: v2.g._1.vertices}),
                graph_fwd: v2.graph_fwd,
                graph_bwd: v2.graph_bwd,
                "inα": v2["outα"],
                "outα": v2["inα"]
              };
              const inputs$p = fromFoldable2($1);
              const v3 = Val.functorEnvExpr.map(v$1 => {})(v2["inα"]);
              const $7 = Val.unrestrictGC(Lattice.boundedMeetSemilatticeBoo)(v3._1)(inputs$p);
              const $8 = unprojExpr(Lattice.boundedMeetSemilatticeBoo)(Val.$EnvExpr(v3._1, v3._2));
              const $9 = Foreign$dObject.filterWithKey(x => {
                const $9 = Util$dSet.setSet(Data$dOrd.ordString).member(x)(inputs$p);
                return v$1 => $9;
              })($6);
              const $10 = Foreign$dObject._fmapObject($6, Val.functorVal.map(v$1 => false));
              const $11 = Val.functorVal.map(v$1 => false)($5);
              const graphgc_op = graphGC1(opEval);
              const graphgc = graphGC2(v2);
              const inertFwd = Graph$dGraphImpl.verticesGraphImpl.vertices(graphgc.fwd($8.fwd($7.fwd($10)))._2);
              const inertBwd = setSet.difference(Graph$dGraphImpl.verticesGraphImpl.vertices($4))(Graph$dGraphImpl.verticesGraphImpl.vertices(graphgc.bwd(Val.functorVal.map(v$1 => true)($5))._2));
              const $12 = selectαs𝔹Vertex["select𝔹s"]($6)(inertBwd);
              const $13 = select𝔹s($5)(inertFwd);
              const $14 = Foreign$dObject._fmapObject($12, Val.functorVal.map(App$dUtil.selState));
              const $15 = Val.functorVal.map(App$dUtil.selState)($13);
              const demands = lift1($14)(v6 => {
                const $16 = graphgc.bwd(v6);
                return Data$dTuple.$Tuple($7.bwd($8.bwd($16._1)), $16._2);
              });
              const demandedBy = lift2($15)(γ1 => graphgc_op.bwd(deMorgan(x => $8.fwd($7.fwd(x)))(γ1)));
              return Monad0.Applicative0().pure({
                spec: v,
                s: $3,
                "γ": Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(
                  Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(
                    Foreign$dObject._fmapObject($12, Val.functorVal.map(App$dUtil.selStates)),
                    Val.applyVal.apply
                  ))($10),
                  Val.applyVal.apply
                ))($10),
                v: Val.applyVal.apply(Val.applyVal.apply(Val.functorVal.map(App$dUtil.selStates)($13))($11))($11),
                "ι": Foreign$dObject.empty,
                linkedOutputs: selType => v6 => {
                  const v$p = Val.functorVal.map(s => {
                    const $16 = (() => {
                      if (selType === "Persistent") { return v$1 => v$1.persistent; }
                      if (selType === "Transient") { return v$1 => v$1.transient; }
                      $runtime.fail();
                    })();
                    if (s.tag === "Inert") { return App$dUtil.Inert; }
                    if (s.tag === "Reactive") { return App$dUtil.$SelState("Reactive", $16(s._1)); }
                    $runtime.fail();
                  })(v6);
                  const v7 = demands(v$p);
                  return Data$dTuple.$Tuple(v7._1, Data$dTuple.$Tuple($2 ? demandedBy(v7._1)._1 : v$p, Graph$dGraphImpl.verticesGraphImpl.vertices(v7._2)));
                },
                linkedInputs: selType => γ1 => {
                  const γ$p = Foreign$dObject._fmapObject(
                    γ1,
                    Val.functorVal.map(s => {
                      const $16 = (() => {
                        if (selType === "Persistent") { return v$1 => v$1.persistent; }
                        if (selType === "Transient") { return v$1 => v$1.transient; }
                        $runtime.fail();
                      })();
                      if (s.tag === "Inert") { return App$dUtil.Inert; }
                      if (s.tag === "Reactive") { return App$dUtil.$SelState("Reactive", $16(s._1)); }
                      $runtime.fail();
                    })
                  );
                  const v6 = demandedBy(γ$p);
                  return Data$dTuple.$Tuple($2 ? demands(v6._1)._1 : γ$p, Data$dTuple.$Tuple(v6._1, Graph$dGraphImpl.verticesGraphImpl.vertices(v6._2)));
                },
                linkIntermediates: ι => {
                  const αs = selectαs𝔹Vertex["selectαs"](Foreign$dObject._fmapObject(
                    ι,
                    Val.functorVal.map(x => {
                      if (x.tag === "Inert") { return false; }
                      if (x.tag === "Reactive") { return x._1.transient; }
                      $runtime.fail();
                    })
                  ))(ιfromαs(Graph$dGraphImpl.graphGraphImpl)($4)(Util$dMap.mapObjectString.keys(ι)));
                  return Data$dTuple.$Tuple(
                    Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject($14, Val.applyVal.apply))(selectαs𝔹Vertex["select𝔹s"]($6)(Graph$dGraphImpl.verticesGraphImpl.vertices(bwdSlice(Data$dTuple.$Tuple(
                      αs,
                      v2.g
                    ))))),
                    Data$dTuple.$Tuple(
                      Val.applyVal.apply($15)(select𝔹s($5)(Graph$dGraphImpl.verticesGraphImpl.vertices(bwdSlice(Data$dTuple.$Tuple(αs, opEval.g))))),
                      Data$dSet.map(Graph.ordDVertex$p)(α => Data$dTuple.$Tuple(
                        α,
                        Util.definitely("in graph")(Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, α, $4._1.out))._2
                      ))(αs)
                    )
                  );
                },
                dir: {persistent: App$dView$dUtil.LinkedOutputs, transient: App$dView$dUtil.LinkedOutputs},
                in_views: Foreign$dObject._fmapObject($9, v$1 => Data$dMaybe.Nothing),
                out_view: Data$dMaybe.Nothing,
                intermediate_views: Foreign$dObject.empty,
                in_roots: Foreign$dObject.fold(z => v$1 => a => Data$dMap$dInternal.insert(Graph.ordVertex)(a)()(z))(Data$dMap$dInternal.Leaf)(Foreign$dObject._fmapObject(
                  $9,
                  v6 => v6._1
                )),
                inerts: Data$dMap$dInternal.unsafeIntersectionWith(Graph.ordDVertex$p.compare, Data$dFunction.const, inertFwd, inertBwd)
              });
            });
          });
        };
      };
    };
  };
};
const intermediates = v => αs => {
  if (v.spec.query.tag === "Nothing") { return Foreign$dObject.empty; }
  if (v.spec.query.tag === "Just") {
    return rebuildι(v.inerts)(αs)(Foreign$dObject.filterWithKey(x => {
      const $0 = !member(x)(v.in_roots);
      return v$1 => $0;
    })(runQuery(v.spec.query._1)(setSet.union(αs.persistent)(αs.transient))));
  }
  $runtime.fail();
};
const selectionResult = v => {
  const v2 = (() => {
    if (v.dir.persistent === "LinkedOutputs") {
      const $0 = v.linkedOutputs(App$dUtil.Persistent)(v.v);
      return Data$dTuple.$Tuple(primary($0._1), Data$dTuple.$Tuple(primaryOrSecondary1(App$dUtil.Persistent)(v.v)($0._2._1), $0._2._2));
    }
    if (v.dir.persistent === "LinkedInputs") {
      const $0 = v.linkedInputs(App$dUtil.Persistent)(v["γ"]);
      return Data$dTuple.$Tuple(primaryOrSecondary(App$dUtil.Persistent)(v["γ"])($0._1), Data$dTuple.$Tuple(primary1($0._2._1), $0._2._2));
    }
    if (v.dir.persistent === "Intermediates") { return Effect$dException.throwException(Effect$dException.error("absurd"))(); }
    $runtime.fail();
  })();
  const v3 = (() => {
    if (v.dir.transient === "LinkedOutputs") {
      const $0 = v.linkedOutputs(App$dUtil.Transient)(v.v);
      return Data$dTuple.$Tuple(primary($0._1), Data$dTuple.$Tuple(primaryOrSecondary1(App$dUtil.Transient)(v.v)($0._2._1), $0._2._2));
    }
    if (v.dir.transient === "LinkedInputs") {
      const $0 = v.linkedInputs(App$dUtil.Transient)(v["γ"]);
      return Data$dTuple.$Tuple(primaryOrSecondary(App$dUtil.Transient)(v["γ"])($0._1), Data$dTuple.$Tuple(primary1($0._2._1), $0._2._2));
    }
    if (v.dir.transient === "Intermediates") {
      const $0 = v.linkIntermediates(v["ι"]);
      return Data$dTuple.$Tuple(primary($0._1), Data$dTuple.$Tuple(primary1($0._2._1), $0._2._2));
    }
    $runtime.fail();
  })();
  return {
    v: Util.spyWhen(false)("Mediating outputs")(x => Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(Pretty.prettyVal(Val.highlightableUnit).pretty(Val.functorVal.map(v$1 => {})(x)))._1)(Val.applyVal.apply(Val.functorVal.map(v4 => v5 => {
      if (v4.tag === "Inert") { return App$dUtil.Inert; }
      if (v5.tag === "Inert") { return App$dUtil.Inert; }
      if (v4.tag === "Reactive" && v5.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: v4._1, transient: v5._1}); }
      $runtime.fail();
    })(v2._2._1))(v3._2._1)),
    "γ": Util.spyWhen(false)("Mediating inputs")(x => Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(Pretty.prettyEnv(Val.highlightableUnit).pretty(Foreign$dObject._fmapObject(
      x,
      Val.functorVal.map(v$1 => {})
    )))._1)(Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(
      Foreign$dObject._fmapObject(
        v2._1,
        Val.functorVal.map(v4 => v5 => {
          if (v4.tag === "Inert") { return App$dUtil.Inert; }
          if (v5.tag === "Inert") { return App$dUtil.Inert; }
          if (v4.tag === "Reactive" && v5.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: v4._1, transient: v5._1}); }
          $runtime.fail();
        })
      ),
      Val.applyVal.apply
    ))(v3._1)),
    "ι": intermediates(v)({persistent: v2._2._2, transient: v3._2._2})
  };
};
const drawFig = divId => v => {
  const $0 = v.spec;
  const v1 = selectionResult(v);
  const $1 = v1["ι"];
  const unused = difference1(Util$dMap.mapObjectString.keys(v["ι"]))(Util$dMap.mapObjectString.keys($1));
  const $2 = drawFig(divId);
  const redraw = x => $2(x({
    "ι": $1,
    dir: v.dir,
    in_roots: v.in_roots,
    in_views: v.in_views,
    inerts: v.inerts,
    intermediate_views: v.intermediate_views,
    linkIntermediates: v.linkIntermediates,
    linkedInputs: v.linkedInputs,
    linkedOutputs: v.linkedOutputs,
    out_view: v.out_view,
    s: v.s,
    spec: v.spec,
    v: v.v,
    "γ": v["γ"]
  }));
  const prefix = divId + "-intermediate";
  const in_views = Foreign$dObject._mapWithKey(v1["γ"], App$dView.view$p()($0));
  const $3 = App$dView$dUtil.drawView({divId, suffix: "output", view: App$dView.view$p()($0)("output")(v1.v)})(x => redraw(selectOutput(x)));
  return () => {
    $3();
    sequence_(Foreign$dObject._mapWithKey(in_views, x => view => App$dView$dUtil.drawView({divId: divId + "-input", suffix: x, view})(x$1 => redraw(selectInput(x)(x$1)))))();
    for_(unused)(α => {
      const $4 = App$dView$dUtil$dD3.rootSelect("#" + prefix + "-" + α);
      return () => {
        const $5 = $4();
        return App$dView$dUtil$dD3.remove($5)();
      };
    })();
    return sequence_(Foreign$dObject._mapWithKey(
      $1,
      α => v3 => App$dView$dUtil.drawView({
        divId: prefix,
        suffix: α,
        view: App$dView.view$p()($0)("intermediate")(Val.functorVal.map(m => {
          if (m.tag === "Inert") { return App$dUtil.Inert; }
          if (m.tag === "Reactive") {
            return App$dUtil.$SelState(
              "Reactive",
              {persistent: m._1.persistent ? App$dUtil.Primary : App$dUtil.None, transient: m._1.transient ? App$dUtil.Primary : App$dUtil.None}
            );
          }
          $runtime.fail();
        })(v3))
      })(x => redraw(selectIntermediate(α)(x)))
    ))();
  };
};
const codeMirrorDiv = v => "codemirror-" + v;
const drawFile = v => {
  const $0 = App$dCodeMirror.addEditorView("codemirror-" + v._1);
  return () => {
    const $1 = $0();
    return loadCode(v._2)($1)();
  };
};
export {
  
  
  
  
  
  drawFig,
  drawFile,
  
  
  
  
  
  
  
  
  
  
  
  
  loadFig,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
};
