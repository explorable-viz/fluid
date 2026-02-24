import * as $runtime from "../runtime.js";
import * as Control$dBind from "../Control.Bind/index.js";
import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as DataType from "../DataType/index.js";
import * as Dict from "../Dict/index.js";
import * as Effect$dAff$dClass from "../Effect.Aff.Class/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Expr from "../Expr/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Graph from "../Graph/index.js";
import * as Graph$dGraphImpl from "../Graph.GraphImpl/index.js";
import * as Graph$dSlice from "../Graph.Slice/index.js";
import * as Graph$dWithGraph from "../Graph.WithGraph/index.js";
import * as Pretty from "../Pretty/index.js";
import * as Pretty$dDoc from "../Pretty.Doc/index.js";
import * as Primitive from "../Primitive/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Util$dPair from "../Util.Pair/index.js";
import * as Util$dSet from "../Util.Set/index.js";
import * as Val from "../Val/index.js";
const setSet = /* #__PURE__ */ Util$dSet.setSet(Graph.ordVertex);
const disjointUnion = /* #__PURE__ */ Util$dMap.disjointUnion(Val.mapEnvStringVal);
const fromFoldable = /* #__PURE__ */ (() => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const b = go$a0, v = go$a1;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = b;
        continue;
      }
      if (v.tag === "Cons") {
        go$a0 = Data$dMap$dInternal.insert(Data$dOrd.ordString)(v._1)()(b);
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  const $0 = go(Data$dMap$dInternal.Leaf);
  return x => $0((() => {
    const go$1 = (m$p, z$p) => {
      if (m$p.tag === "Leaf") { return z$p; }
      if (m$p.tag === "Node") { return go$1(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go$1(m$p._6, z$p))); }
      $runtime.fail();
    };
    return go$1(x, Data$dList$dTypes.Nil);
  })());
})();
const show2 = /* #__PURE__ */ (() => Data$dSet.showSet(Data$dShow.showString).show)();
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
const union1 = /* #__PURE__ */ (() => Util$dSet.setSet(Data$dOrd.ordString).union)();
const fv = /* #__PURE__ */ (() => Expr.fVDict(Expr.fVElim).fv)();
const fromFoldable1 = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dList$dTypes.foldableList);
const greaterThanOrEq = a1 => a2 => {
  const v = Data$dOrd.ordInt.compare(a1._1)(a2._1);
  if (v === "LT") { return false; }
  return v === "GT" || a1._2 >= a2._2;
};
const show3 = v => "(Tuple " + Data$dShow.showIntImpl(v._1) + " " + Data$dShow.showIntImpl(v._2) + ")";
const lookup1 = k => {
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
const traverse2 = /* #__PURE__ */ (() => Data$dList$dTypes.traversableList.traverse(Data$dMaybe.applicativeMaybe))();
const fwdSlice = /* #__PURE__ */ Graph$dSlice.fwdSlice(Graph$dGraphImpl.graphGraphImpl);
const bwdSlice = /* #__PURE__ */ Graph$dSlice.bwdSlice(Graph$dGraphImpl.graphGraphImpl);
const mempty = /* #__PURE__ */ (() => Data$dSet.monoidSet(Graph.ordVertex).mempty)();
const withOp = dictGraph => v => ({g: dictGraph.op(v.g), graph_fwd: v.graph_fwd, graph_bwd: v.graph_bwd, "inα": v["outα"], "outα": v["inα"]});
const toGC = dictGraph => dictApply => dictApply1 => dictFoldable => dictFoldable1 => v => ({fwd: x => v.fwd(x)._1, bwd: x => v.bwd(x)._1});
const patternMismatch = s => s$p => "Pattern mismatch: found " + s + ", expected " + s$p;
const matchMany = dictMonadWithGraphAlloc => {
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const $0 = Monad0.Applicative0();
  const $1 = Monad0.Bind1();
  const $$throw = Util.throw(dictMonadWithGraphAlloc.MonadError1().MonadThrow0());
  return v => v1 => {
    if (v.tag === "Nil") { return $0.pure(Data$dTuple.$Tuple(Foreign$dObject.empty, Data$dTuple.$Tuple(v1, setSet.empty))); }
    if (v.tag === "Cons") {
      if (v1.tag === "ContElim") {
        const $2 = v._2;
        return $1.bind(match(dictMonadWithGraphAlloc)(v._1)(v1._1))(v3 => {
          const $3 = v3._2._2;
          const $4 = v3._1;
          return $1.bind(matchMany(dictMonadWithGraphAlloc)($2)(v3._2._1))(v4 => $0.pure(Data$dTuple.$Tuple(
            disjointUnion($4)(v4._1),
            Data$dTuple.$Tuple(v4._2._1, setSet.union($3)(v4._2._2))
          )));
        });
      }
      if (v1.tag === "ContExpr") {
        return $$throw(Data$dShow.showIntImpl((() => {
          const go = go$a0$copy => go$a1$copy => {
            let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
            while (go$c) {
              const b = go$a0, v$1 = go$a1;
              if (v$1.tag === "Nil") {
                go$c = false;
                go$r = b;
                continue;
              }
              if (v$1.tag === "Cons") {
                go$a0 = b + 1 | 0;
                go$a1 = v$1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$r;
          };
          return go(0)(v._2) + 1 | 0;
        })()) + " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?");
      }
    }
    $runtime.fail();
  };
};
const match = dictMonadWithGraphAlloc => {
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const $0 = Monad0.Applicative0();
  const Bind1 = Monad0.Bind1();
  const MonadError1 = dictMonadWithGraphAlloc.MonadError1();
  const withMsg = Util.withMsg(MonadError1);
  const consistentWith = DataType.consistentWith(MonadError1);
  const MonadThrow0 = MonadError1.MonadThrow0();
  return v => v1 => {
    if (v1.tag === "ElimVar") {
      if (v1._1 === "_") { return $0.pure(Data$dTuple.$Tuple(Foreign$dObject.empty, Data$dTuple.$Tuple(v1._2, setSet.empty))); }
      const $1 = v1._1;
      return $0.pure(Data$dTuple.$Tuple(
        (() => {
          const $2 = {};
          $2[$1] = v;
          return $2;
        })(),
        Data$dTuple.$Tuple(v1._2, setSet.empty)
      ));
    }
    if (v1.tag === "ElimConstr") {
      if (v._3.tag === "Constr") {
        const $1 = v._3._1;
        const $2 = v1._1;
        const $3 = v._3._2;
        const $4 = v._1;
        return Bind1.bind(withMsg("Pattern mismatch")(consistentWith(Data$dMap$dInternal.$$$Map("Node", 1, 1, $1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(Util$dMap.mapObjectString.keys($2))))(() => Bind1.bind(Util.orElse(MonadThrow0)("Incomplete patterns: no branch for " + DataType.showCtr($1))(Foreign$dObject._lookup(
          Data$dMaybe.Nothing,
          Data$dMaybe.Just,
          $1,
          $2
        )))(κ => Bind1.bind(matchMany(dictMonadWithGraphAlloc)($3)(κ))(v2 => $0.pure(Data$dTuple.$Tuple(
          v2._1,
          Data$dTuple.$Tuple(v2._2._1, Data$dMap$dInternal.insert(Graph.ordVertex)($4)()(v2._2._2))
        )))));
      }
      return Bind1.bind(DataType.dataTypeForSetCtr.dataTypeFor(MonadThrow0)(Util$dMap.mapObjectString.keys(v1._1)))(d => MonadThrow0.throwError(Effect$dException.error("Pattern mismatch: found " + Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(Pretty.prettyVal(Val.highlightableVertex).pretty(v))._1 + ", expected " + d._1)));
    }
    if (v1.tag === "ElimDict") {
      if (v._3.tag === "Dictionary") {
        const $1 = v1._1;
        const $2 = v._3._1;
        const $3 = v._1;
        const $4 = v1._2;
        return Bind1.bind(Util.check(MonadThrow0)(Data$dMap$dInternal.unsafeDifference(Data$dOrd.ordString.compare, $1, fromFoldable(Util$dMap.mapObjectString.keys($2))).tag === "Leaf")("Pattern mismatch: found " + show2(Util$dMap.mapObjectString.keys($2)) + ", expected " + show2($1)))(() => Bind1.bind(matchMany(dictMonadWithGraphAlloc)(Data$dList$dTypes.listMap(k => Util$dMap.get(Data$dShow.showString)(Util$dMap.mapObjectString)(k)($2)._2)(toUnfoldable($1)))($4))(v2 => $0.pure(Data$dTuple.$Tuple(
          v2._1,
          Data$dTuple.$Tuple(v2._2._1, Data$dMap$dInternal.insert(Graph.ordVertex)($3)()(v2._2._2))
        ))));
      }
      return MonadThrow0.throwError(Effect$dException.error("Pattern mismatch: found " + Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(Pretty.prettyVal(Val.highlightableVertex).pretty(v))._1 + ", expected " + show2(v1._1)));
    }
    $runtime.fail();
  };
};
const graphGC = dictGraph => {
  const $0 = dictGraph.Vertices1();
  return dictApply => {
    const selectαs𝔹Vertex = Graph.selectαs𝔹Vertex(dictApply);
    return dictApply1 => {
      const selectαs𝔹Vertex1 = Graph.selectαs𝔹Vertex(dictApply1);
      return dictFoldable => {
        const selectαs𝔹Vertex2 = selectαs𝔹Vertex(dictFoldable);
        return dictFoldable1 => {
          const selectαs𝔹Vertex3 = selectαs𝔹Vertex1(dictFoldable1);
          return v => {
            const $1 = v.g;
            const $2 = v["inα"];
            const $3 = v["outα"];
            return {
              fwd: in𝔹 => {
                const g$p = v.graph_fwd(selectαs𝔹Vertex2["selectαs"](in𝔹)($2))($1);
                return Data$dTuple.$Tuple(selectαs𝔹Vertex3["select𝔹s"]($3)($0.vertices(g$p)), g$p);
              },
              bwd: out𝔹 => {
                const g$p = v.graph_bwd(selectαs𝔹Vertex3["selectαs"](out𝔹)($3))($1);
                return Data$dTuple.$Tuple(selectαs𝔹Vertex2["select𝔹s"]($2)($0.vertices(g$p)), g$p);
              }
            };
          };
        };
      };
    };
  };
};
const closeDefs = dictMonadWithGraphAlloc => {
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const traverse2$1 = Dict.traversableDict.traverse(Monad0.Applicative0());
  const val = Val.val(dictMonadWithGraphAlloc);
  return γ => ρ => αs => Monad0.Bind1().Apply0().Functor0().map(Val.Env)(traverse2$1(σ => {
    const ρ$p = Val.forDefs(ρ)(σ);
    return val(Data$dMaybe.Nothing)(αs)(Val.$BaseVal(
      "Fun",
      Val.$Fun(
        "Closure",
        (() => {
          const $0 = union1(fv(ρ$p))(Expr.fVElim.fv(σ));
          return Foreign$dObject.filterWithKey(x => {
            const $1 = Util$dSet.setSet(Data$dOrd.ordString).member(x)($0);
            return v => $1;
          })(γ);
        })(),
        ρ$p,
        σ
      )
    ));
  })(ρ));
};
const evalVal = dictMonadWithGraphAlloc => {
  const MonadError1 = dictMonadWithGraphAlloc.MonadError1();
  const checkArity = DataType.checkArity(MonadError1);
  const check = Util.check(MonadError1.MonadThrow0());
  return dictMonadReader => dictMonadAff => {
    const Monad0 = dictMonadAff.MonadEffect0().Monad0();
    const Applicative0 = Monad0.Applicative0();
    const Bind1 = Monad0.Bind1();
    const $0 = Bind1.Apply0().Functor0();
    const traverse3 = Data$dList$dTypes.traversableList.traverse(Applicative0);
    const traverse4 = Util$dPair.traversablePair.traverse(Applicative0);
    const sequence1 = Data$dTraversable.traversableArray.traverse(Applicative0)(Data$dTraversable.identity);
    return dictLoadFile => v => v1 => v2 => {
      if (v1.tag === "Int") { return Applicative0.pure(Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(v1._1, Val.$BaseVal("Int", v1._2)))); }
      if (v1.tag === "Float") { return Applicative0.pure(Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(v1._1, Val.$BaseVal("Float", v1._2)))); }
      if (v1.tag === "Str") { return Applicative0.pure(Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(v1._1, Val.$BaseVal("Str", v1._2)))); }
      if (v1.tag === "Dictionary") {
        const $1 = v1._1;
        return Bind1.bind($0.map(Util$dPair.unzip)(traverse3(traverse4((() => {
          const $2 = $$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(Data$dMaybe.Nothing)(v);
          return a => $2(a)(v2);
        })()))(v1._2)))(v3 => {
          const v4 = Data$dList.unzip(Data$dList$dTypes.listMap(v$1 => Data$dTuple.$Tuple(v$1._3.tag === "Str" ? v$1._3._1 : Primitive.typeError(v$1._3)("Str"), v$1._1))(v3._1));
          return Applicative0.pure(Data$dMaybe.$Maybe(
            "Just",
            Data$dTuple.$Tuple(
              $1,
              Val.$BaseVal(
                "Dictionary",
                fromFoldable1((() => {
                  const go = go$a0$copy => go$a1$copy => go$a2$copy => {
                    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
                    while (go$c) {
                      const v$1 = go$a0, v1$1 = go$a1, v2$1 = go$a2;
                      if (v$1.tag === "Nil") {
                        go$c = false;
                        go$r = v2$1;
                        continue;
                      }
                      if (v1$1.tag === "Nil") {
                        go$c = false;
                        go$r = v2$1;
                        continue;
                      }
                      if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                        go$a0 = v$1._2;
                        go$a1 = v1$1._2;
                        go$a2 = Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v$1._1, v1$1._1), v2$1);
                        continue;
                      }
                      $runtime.fail();
                    }
                    return go$r;
                  };
                  const go$1 = go$1$a0$copy => go$1$a1$copy => {
                    let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
                    while (go$1$c) {
                      const v$1 = go$1$a0, v1$1 = go$1$a1;
                      if (v1$1.tag === "Nil") {
                        go$1$c = false;
                        go$1$r = v$1;
                        continue;
                      }
                      if (v1$1.tag === "Cons") {
                        go$1$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$1);
                        go$1$a1 = v1$1._2;
                        continue;
                      }
                      $runtime.fail();
                    }
                    return go$1$r;
                  };
                  const go$2 = go$2$a0$copy => go$2$a1$copy => go$2$a2$copy => {
                    let go$2$a0 = go$2$a0$copy, go$2$a1 = go$2$a1$copy, go$2$a2 = go$2$a2$copy, go$2$c = true, go$2$r;
                    while (go$2$c) {
                      const v$1 = go$2$a0, v1$1 = go$2$a1, v2$1 = go$2$a2;
                      if (v$1.tag === "Nil") {
                        go$2$c = false;
                        go$2$r = v2$1;
                        continue;
                      }
                      if (v1$1.tag === "Nil") {
                        go$2$c = false;
                        go$2$r = v2$1;
                        continue;
                      }
                      if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                        go$2$a0 = v$1._2;
                        go$2$a1 = v1$1._2;
                        go$2$a2 = Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v$1._1, v1$1._1), v2$1);
                        continue;
                      }
                      $runtime.fail();
                    }
                    return go$2$r;
                  };
                  const go$3 = go$3$a0$copy => go$3$a1$copy => {
                    let go$3$a0 = go$3$a0$copy, go$3$a1 = go$3$a1$copy, go$3$c = true, go$3$r;
                    while (go$3$c) {
                      const v$1 = go$3$a0, v1$1 = go$3$a1;
                      if (v1$1.tag === "Nil") {
                        go$3$c = false;
                        go$3$r = v$1;
                        continue;
                      }
                      if (v1$1.tag === "Cons") {
                        go$3$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$1);
                        go$3$a1 = v1$1._2;
                        continue;
                      }
                      $runtime.fail();
                    }
                    return go$3$r;
                  };
                  return go$3(Data$dList$dTypes.Nil)(go$2(v4._1)(go$1(Data$dList$dTypes.Nil)(go(v4._2)(v3._2)(Data$dList$dTypes.Nil)))(Data$dList$dTypes.Nil));
                })())
              )
            )
          ));
        });
      }
      if (v1.tag === "Constr") {
        const $1 = v1._2;
        const $2 = v1._3;
        const $3 = v1._1;
        return Bind1.bind(checkArity($1)((() => {
          const go = go$a0$copy => go$a1$copy => {
            let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
            while (go$c) {
              const b = go$a0, v$1 = go$a1;
              if (v$1.tag === "Nil") {
                go$c = false;
                go$r = b;
                continue;
              }
              if (v$1.tag === "Cons") {
                go$a0 = b + 1 | 0;
                go$a1 = v$1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$r;
          };
          return go(0)($2);
        })()))(() => Bind1.bind(traverse3((() => {
          const $4 = $$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(Data$dMaybe.Nothing)(v);
          return a => $4(a)(v2);
        })())($2))(vs => Applicative0.pure(Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($3, Val.$BaseVal("Constr", $1, vs))))));
      }
      if (v1.tag === "Matrix") {
        const $1 = v1._2;
        const $2 = v1._3._1;
        const $3 = v1._3._2;
        const $4 = v1._1;
        return Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(Data$dMaybe.Nothing)(v)(v1._4)(v2))(v3 => {
          const v5 = Primitive.intPair.unpack(v3._3);
          const $5 = v5._1._1;
          const $6 = v5._2._1;
          const $7 = v5._1._2;
          const $8 = v5._2._2;
          return Bind1.bind(check(greaterThanOrEq(Data$dTuple.$Tuple($5, $6))(Data$dTuple.$Tuple(1, 1)))("array must be at least (" + show3(Data$dTuple.$Tuple(1, 1)) + "); got (" + show3(Data$dTuple.$Tuple(
            $5,
            $6
          )) + ")"))(() => Bind1.bind(sequence1(Control$dBind.arrayBind(Data$dArray.rangeImpl(0, $5 - 1 | 0))(i => [
            sequence1(Control$dBind.arrayBind(Data$dArray.rangeImpl(0, $6 - 1 | 0))(j => [
              $$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(Data$dMaybe.Nothing)(Foreign$dObject.unionWith(v$1 => Util$dMap.identity)(v)(disjointUnion((() => {
                const $9 = {};
                $9[$2] = Val.$Val($7, Data$dMaybe.Nothing, Val.$BaseVal("Int", i));
                return $9;
              })())((() => {
                const $9 = {};
                $9[$3] = Val.$Val($8, Data$dMaybe.Nothing, Val.$BaseVal("Int", j));
                return $9;
              })())))($1)(v2)
            ]))
          ])))(vss => Applicative0.pure(Data$dMaybe.$Maybe(
            "Just",
            Data$dTuple.$Tuple($4, Val.$BaseVal("Matrix", Data$dTuple.$Tuple(vss, Data$dTuple.$Tuple(Data$dTuple.$Tuple($5, $7), Data$dTuple.$Tuple($6, $8)))))
          ))));
        });
      }
      if (v1.tag === "Lambda") {
        return Applicative0.pure(Data$dMaybe.$Maybe(
          "Just",
          Data$dTuple.$Tuple(
            v1._1,
            Val.$BaseVal(
              "Fun",
              Val.$Fun(
                "Closure",
                (() => {
                  const $1 = Expr.fVElim.fv(v1._2);
                  return Foreign$dObject.filterWithKey(x => {
                    const $2 = Util$dSet.setSet(Data$dOrd.ordString).member(x)($1);
                    return v$1 => $2;
                  })(v);
                })(),
                Foreign$dObject.empty,
                v1._2
              )
            )
          )
        ));
      }
      return Applicative0.pure(Data$dMaybe.Nothing);
    };
  };
};
const $$eval = dictMonadWithGraphAlloc => {
  const $$new = dictMonadWithGraphAlloc.new(Val.typeNameVal);
  const MonadError1 = dictMonadWithGraphAlloc.MonadError1();
  const withMsg = Util.withMsg(MonadError1);
  const MonadThrow0 = MonadError1.MonadThrow0();
  const match1 = match(dictMonadWithGraphAlloc);
  const closeDefs1 = closeDefs(dictMonadWithGraphAlloc);
  return dictMonadReader => dictMonadAff => {
    const Monad0 = dictMonadAff.MonadEffect0().Monad0();
    const Bind1 = Monad0.Bind1();
    const traceWhen = Util.traceWhen(Monad0.Applicative0());
    return dictLoadFile => doc_opt => γ => e0 => αs => {
      const funName = funName$a0$copy => {
        let funName$a0 = funName$a0$copy, funName$c = true, funName$r;
        while (funName$c) {
          const v = funName$a0;
          if (v.tag === "Var") {
            funName$c = false;
            funName$r = v._1;
            continue;
          }
          if (v.tag === "Op") {
            funName$c = false;
            funName$r = v._1;
            continue;
          }
          if (v.tag === "App") {
            funName$a0 = v._1;
            continue;
          }
          funName$c = false;
          funName$r = "unknown";
        }
        return funName$r;
      };
      return Bind1.bind(evalVal(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(γ)(e0)(αs))(αu_opt => {
        if (αu_opt.tag === "Just") { return $$new(a => Val.Val(a)(doc_opt))(Data$dMap$dInternal.insert(Graph.ordVertex)(αu_opt._1._1)()(αs))(αu_opt._1._2); }
        if (αu_opt.tag === "Nothing") {
          if (e0.tag === "Var") {
            const $0 = e0._1;
            return Bind1.bind(traceWhen((() => {
              if (doc_opt.tag === "Nothing") { return false; }
              if (doc_opt.tag === "Just") { return true; }
              $runtime.fail();
            })())("Discarding doc (variable " + $0 + ")"))(() => withMsg("Variable lookup")(Util$dMap.lookup$p(MonadThrow0)(Data$dShow.showString)(Val.mapEnvStringVal)($0)(γ)));
          }
          if (e0.tag === "Op") {
            const $0 = e0._1;
            return Bind1.bind(traceWhen((() => {
              if (doc_opt.tag === "Nothing") { return false; }
              if (doc_opt.tag === "Just") { return true; }
              $runtime.fail();
            })())("Discarding doc (operator " + $0 + ")"))(() => withMsg("Variable lookup")(Util$dMap.lookup$p(MonadThrow0)(Data$dShow.showString)(Val.mapEnvStringVal)($0)(γ)));
          }
          if (e0.tag === "DProject") {
            const $0 = e0._1;
            const $1 = e0._2;
            return Bind1.bind(traceWhen((() => {
              if (doc_opt.tag === "Nothing") { return false; }
              if (doc_opt.tag === "Just") { return true; }
              $runtime.fail();
            })())("Discarding doc (projection)"))(() => Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(Data$dMaybe.Nothing)(γ)($0)(αs))(v => Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(Data$dMaybe.Nothing)(γ)($1)(αs))(v$p => {
              if (v._3.tag === "Dictionary") {
                if (v$p._3.tag === "Str") {
                  return withMsg("Dict lookup")(Util.orElse(MonadThrow0)("Key \"" + v$p._3._1 + "\" not found")((() => {
                    const $2 = Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, v$p._3._1, v._3._1);
                    if ($2.tag === "Just") { return Data$dMaybe.$Maybe("Just", $2._1._2); }
                    return Data$dMaybe.Nothing;
                  })()));
                }
                return MonadThrow0.throwError(Effect$dException.error("Found " + Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(Pretty.prettyVal(Val.highlightableVertex).pretty(v$p))._1 + ", expected string"));
              }
              return MonadThrow0.throwError(Effect$dException.error("Found " + Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(Pretty.prettyVal(Val.highlightableVertex).pretty(v))._1 + ", expected dict"));
            })));
          }
          if (e0.tag === "App") {
            const $0 = e0._1;
            const $1 = e0._2;
            return Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(Data$dMaybe.Nothing)(γ)($0)(αs))(v => Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(Data$dMaybe.Nothing)(γ)($1)(αs))(v$p => withMsg("In " + funName($0))(apply(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(doc_opt)(v)(v$p))));
          }
          if (e0.tag === "Let") {
            const $0 = e0._2;
            const $1 = e0._1._1;
            return Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(Data$dMaybe.Nothing)(γ)(e0._1._2)(αs))(v => Bind1.bind(withMsg("In variable def")(match1(v)($1)))(v1 => $$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(doc_opt)(Foreign$dObject.unionWith(v$1 => Util$dMap.identity)(γ)(v1._1))($0)(v1._2._2)));
          }
          if (e0.tag === "LetRec") {
            const $0 = e0._2;
            const $1 = e0._1._1;
            return Bind1.bind(closeDefs1(γ)(e0._1._2)(Data$dMap$dInternal.insert(Graph.ordVertex)($1)()(αs)))(γ$p => $$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(doc_opt)(Foreign$dObject.unionWith(v => Util$dMap.identity)(γ)(γ$p))($0)(Data$dMap$dInternal.insert(Graph.ordVertex)($1)()(αs)));
          }
          if (e0.tag === "DocExpr") {
            const $0 = e0._2;
            return Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(Data$dMaybe.Nothing)(γ)(e0._1)(αs))(v => Bind1.bind(traceWhen((() => {
              if (doc_opt.tag === "Nothing") { return false; }
              if (doc_opt.tag === "Just") { return true; }
              $runtime.fail();
            })())("Outer doc trumps inner doc"))(() => $$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(Data$dMaybe.$Maybe(
              "Just",
              (() => {
                if (doc_opt.tag === "Nothing") { return v; }
                if (doc_opt.tag === "Just") { return doc_opt._1; }
                $runtime.fail();
              })()
            ))(γ)($0)(αs)));
          }
          return Effect$dException.throwException(Effect$dException.error("absurd"))();
        }
        $runtime.fail();
      });
    };
  };
};
const apply = dictMonadWithGraphAlloc => {
  const closeDefs1 = closeDefs(dictMonadWithGraphAlloc);
  const match1 = match(dictMonadWithGraphAlloc);
  const val = Val.val(dictMonadWithGraphAlloc);
  const MonadError1 = dictMonadWithGraphAlloc.MonadError1();
  const MonadThrow0 = MonadError1.MonadThrow0();
  return dictMonadReader => dictMonadAff => {
    const Bind1 = dictMonadAff.MonadEffect0().Monad0().Bind1();
    return dictLoadFile => v => v1 => v2 => {
      const $0 = v3 => MonadThrow0.throwError(Effect$dException.error("Found " + Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(Pretty.prettyVal(Val.highlightableVertex).pretty(v3))._1 + ", expected function"));
      if (v1._3.tag === "Fun") {
        if (v1._3._1.tag === "Closure") {
          const $1 = v1._1;
          const $2 = v1._3._1._1;
          const $3 = v1._3._1._3;
          return Bind1.bind(closeDefs1($2)(v1._3._1._2)(Data$dMap$dInternal.$$$Map("Node", 1, 1, $1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf)))(γ2 => Bind1.bind(match1(v2)($3))(v4 => $$eval(dictMonadWithGraphAlloc)(dictMonadReader)(dictMonadAff)(dictLoadFile)(v)(Foreign$dObject.unionWith(v$1 => Util$dMap.identity)(Foreign$dObject.unionWith(v$1 => Util$dMap.identity)($2)(γ2))(v4._1))(v4._2._1.tag === "ContExpr"
            ? v4._2._1._1
            : Effect$dException.throwException(Effect$dException.error("Expression expected"))())(Data$dMap$dInternal.insert(Graph.ordVertex)($1)()(v4._2._2))));
        }
        if (v1._3._1.tag === "Foreign") {
          const $1 = v1._3._1._1._2;
          const vs$p = Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.$List("Cons", v2, Data$dList$dTypes.Nil))(v1._3._1._2);
          if (
            (() => {
              const go = go$a0$copy => go$a1$copy => {
                let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
                while (go$c) {
                  const b = go$a0, v$1 = go$a1;
                  if (v$1.tag === "Nil") {
                    go$c = false;
                    go$r = b;
                    continue;
                  }
                  if (v$1.tag === "Cons") {
                    go$a0 = b + 1 | 0;
                    go$a1 = v$1._2;
                    continue;
                  }
                  $runtime.fail();
                }
                return go$r;
              };
              return $1._1.arity > go(0)(vs$p);
            })()
          ) {
            return val(v)(Data$dMap$dInternal.$$$Map("Node", 1, 1, v1._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(Val.$BaseVal(
              "Fun",
              Val.$Fun("Foreign", Data$dTuple.$Tuple(v1._3._1._1._1, $1), vs$p)
            ));
          }
          return $1._1.op(dictMonadWithGraphAlloc)(MonadError1)(dictMonadAff)(dictMonadReader)(dictLoadFile)(v)(vs$p);
        }
        if (v1._3._1.tag === "PartialConstr") {
          const $1 = v1._1;
          const n = Util.defined(DataType.arity(Control$dMonad$dExcept$dTrans.monadThrowExceptT(Data$dIdentity.monadIdentity))(v1._3._1._1));
          const v$p = (() => {
            const go = go$a0$copy => go$a1$copy => {
              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
              while (go$c) {
                const b = go$a0, v$1 = go$a1;
                if (v$1.tag === "Nil") {
                  go$c = false;
                  go$r = b;
                  continue;
                }
                if (v$1.tag === "Cons") {
                  go$a0 = b + 1 | 0;
                  go$a1 = v$1._2;
                  continue;
                }
                $runtime.fail();
              }
              return go$r;
            };
            return go(0)(v1._3._1._2) < (n - 1 | 0);
          })()
            ? Val.$BaseVal(
                "Fun",
                Val.$Fun(
                  "PartialConstr",
                  v1._3._1._1,
                  Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.$List("Cons", v2, Data$dList$dTypes.Nil))(v1._3._1._2)
                )
              )
            : Val.$BaseVal(
                "Constr",
                v1._3._1._1,
                Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.$List("Cons", v2, Data$dList$dTypes.Nil))(v1._3._1._2)
              );
          return Bind1.bind(Util.check(MonadThrow0)((() => {
            const go = go$a0$copy => go$a1$copy => {
              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
              while (go$c) {
                const b = go$a0, v$1 = go$a1;
                if (v$1.tag === "Nil") {
                  go$c = false;
                  go$r = b;
                  continue;
                }
                if (v$1.tag === "Cons") {
                  go$a0 = b + 1 | 0;
                  go$a1 = v$1._2;
                  continue;
                }
                $runtime.fail();
              }
              return go$r;
            };
            return go(0)(v1._3._1._2) < n;
          })())("Too many arguments to " + DataType.showCtr(v1._3._1._1)))(() => val(v)(Data$dMap$dInternal.$$$Map(
            "Node",
            1,
            1,
            $1,
            undefined,
            Data$dMap$dInternal.Leaf,
            Data$dMap$dInternal.Leaf
          ))(v$p));
        }
      }
      return $0(v2);
    };
  };
};
const eval_module = dictMonadWithGraphAlloc => {
  const eval1 = $$eval(dictMonadWithGraphAlloc);
  const match1 = match(dictMonadWithGraphAlloc);
  const closeDefs1 = closeDefs(dictMonadWithGraphAlloc);
  return dictMonadReader => {
    const eval2 = eval1(dictMonadReader);
    return dictMonadAff => {
      const Monad0 = dictMonadAff.MonadEffect0().Monad0();
      const $0 = Monad0.Bind1();
      const eval3 = eval2(dictMonadAff);
      return dictLoadFile => {
        const eval4 = eval3(dictLoadFile);
        return γ => {
          const go = v => v1 => v2 => {
            if (v1.tag === "Nil") { return Monad0.Applicative0().pure(v); }
            if (v1.tag === "Cons") {
              if (v1._1.tag === "Left") {
                const $1 = v1._2;
                const $2 = v1._1._1._1;
                return $0.bind(eval4(Data$dMaybe.Nothing)(Foreign$dObject.unionWith(v$1 => Util$dMap.identity)(γ)(v))(v1._1._1._2)(v2))(v3 => $0.bind(match1(v3)($2))(v4 => go(Foreign$dObject.unionWith(v$1 => Util$dMap.identity)(v)(v4._1))($1)(v4._2._2)));
              }
              if (v1._1.tag === "Right") {
                const $1 = v1._2;
                return $0.bind(closeDefs1(Foreign$dObject.unionWith(v$1 => Util$dMap.identity)(γ)(v))(v1._1._1._2)(Data$dMap$dInternal.insert(Graph.ordVertex)(v1._1._1._1)()(v2)))(γ$p$p => go(Foreign$dObject.unionWith(v$1 => Util$dMap.identity)(v)(γ$p$p))($1)(v2));
              }
            }
            $runtime.fail();
          };
          return go(Foreign$dObject.empty);
        };
      };
    };
  };
};
const eval_primitives = dictMonadWithGraphAlloc => {
  const eval_module1 = eval_module(dictMonadWithGraphAlloc);
  return dictMonadReader => {
    const eval_module2 = eval_module1(dictMonadReader);
    return dictMonadAff => {
      const Monad0 = dictMonadAff.MonadEffect0().Monad0();
      const $0 = Monad0.Bind1();
      const eval_module3 = eval_module2(dictMonadAff);
      const $1 = Monad0.Applicative0();
      return dictLoadFile => {
        const eval_module4 = eval_module3(dictLoadFile);
        return primitives => v => {
          const $2 = v.graph;
          const $3 = v.modules;
          const $4 = v.roots;
          return $0.bind(Data$dList.foldM(Monad0)(γs => name => {
            const v1 = Util.definitely("deps evaluated")((() => {
              const $5 = lookup1(name)($2);
              if ($5.tag === "Just") {
                const $6 = traverse2(dep => lookup1(dep)(γs))($5._1);
                if ($6.tag === "Just") {
                  const $7 = lookup1(name)($3);
                  if ($7.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($7._1, $6._1)); }
                  if ($7.tag === "Nothing") { return Data$dMaybe.Nothing; }
                  $runtime.fail();
                }
                if ($6.tag === "Nothing") { return Data$dMaybe.Nothing; }
                $runtime.fail();
              }
              if ($5.tag === "Nothing") { return Data$dMaybe.Nothing; }
              $runtime.fail();
            })());
            return $0.bind(eval_module4((() => {
              const go = go$a0$copy => go$a1$copy => {
                let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
                while (go$c) {
                  const b = go$a0, v$1 = go$a1;
                  if (v$1.tag === "Nil") {
                    go$c = false;
                    go$r = b;
                    continue;
                  }
                  if (v$1.tag === "Cons") {
                    go$a0 = Foreign$dObject.unionWith(v$2 => Util$dMap.identity)(b)(v$1._1);
                    go$a1 = v$1._2;
                    continue;
                  }
                  $runtime.fail();
                }
                return go$r;
              };
              return go(primitives)(v1._2);
            })())(v1._1)(setSet.empty))(γ$p => $1.pure(Data$dMap$dInternal.insert(Data$dOrd.ordString)(name)(γ$p)(γs)));
          })(Data$dMap$dInternal.Leaf)(v.topsorted))(γs => $1.pure((() => {
            const go = go$a0$copy => go$a1$copy => {
              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
              while (go$c) {
                const b = go$a0, v$1 = go$a1;
                if (v$1.tag === "Nil") {
                  go$c = false;
                  go$r = b;
                  continue;
                }
                if (v$1.tag === "Cons") {
                  go$a0 = Foreign$dObject.unionWith(v$2 => Util$dMap.identity)(b)(v$1._1);
                  go$a1 = v$1._2;
                  continue;
                }
                $runtime.fail();
              }
              return go$r;
            };
            return go(primitives)(Data$dList$dTypes.listMap(dep => Util.definitely("has env")(lookup1(dep)(γs)))($4));
          })()));
        };
      };
    };
  };
};
const graphEval = dictMonadAff => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const bindStateT = Control$dMonad$dState$dTrans.bindStateT(Monad0);
  const $0 = Graph$dWithGraph.monadAllocAllocT(Monad0);
  const fresh1 = $0.fresh;
  const alloc = Expr.traversableExpr.traverse($0.Monad0().Applicative0())(v => fresh1);
  const runWithGraphT_spy = Graph$dWithGraph.runWithGraphT_spy({
    Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(Monad0),
    Bind1: () => Control$dMonad$dState$dTrans.bindStateT(Monad0)
  })(Graph$dGraphImpl.graphGraphImpl);
  const monadAffState = Effect$dAff$dClass.monadAffState(dictMonadAff);
  const monadAffState1 = Effect$dAff$dClass.monadAffState(monadAffState);
  const $1 = monadAffState.MonadEffect0().Monad0();
  const $2 = dictMonadAff.MonadEffect0().Monad0();
  const applicativeStateT = Control$dMonad$dState$dTrans.applicativeStateT(Monad0);
  return dictMonadReader => {
    const monadReaderStateT = Control$dMonad$dState$dTrans.monadReaderStateT(Control$dMonad$dState$dTrans.monadReaderStateT(dictMonadReader));
    return dictLoadFile => dictMonadError => {
      const eval1 = $$eval(Graph$dWithGraph.monadWithGraphAllocWithGr(dictMonadError))(monadReaderStateT)(monadAffState1)((() => {
        const loadFileFromPath1 = dictLoadFile.loadFileFromPath(dictMonadError)(dictMonadAff);
        return {
          loadFileFromPath: dictMonadError1 => dictMonadAff1 => x => {
            const $3 = loadFileFromPath1(x);
            return s => $1.Bind1().bind(s$1 => $2.Bind1().bind($3)(x$1 => $2.Applicative0().pure(Data$dTuple.$Tuple(x$1, s$1))))(x$1 => $1.Applicative0().pure(Data$dTuple.$Tuple(
              x$1,
              s
            )));
          }
        };
      })());
      return v => e => {
        const $3 = v["γ"];
        const $4 = Util.spyFunWhen(false)("fwdSlice")(x => Data$dTuple.$Tuple(Graph.showVertices(x._1), Graph.showEdgeList(Graph.toEdgeList(Graph$dGraphImpl.graphGraphImpl)(x._2))))(Graph.showGraph(Graph$dGraphImpl.graphGraphImpl))(fwdSlice);
        const $5 = Util.spyFunWhen(false)("bwdSlice")(x => Data$dTuple.$Tuple(Graph.showVertices(x._1), Graph.showEdgeList(Graph.toEdgeList(Graph$dGraphImpl.graphGraphImpl)(x._2))))(Graph.showGraph(Graph$dGraphImpl.graphGraphImpl))(bwdSlice);
        return Monad0.Bind1().bind(Graph$dWithGraph.runAllocT(Monad0)(bindStateT.bind(alloc(e))(eα => bindStateT.bind(runWithGraphT_spy(eval1(Data$dMaybe.Nothing)($3)(eα)(mempty))(Val.verticesEnvExprVertex.vertices(Val.$EnvExpr(
          $3,
          eα
        ))))(v1 => {
          const $6 = v1._1;
          const $7 = v1._2;
          return bindStateT.bind(applicativeStateT.pure())(() => applicativeStateT.pure(Data$dTuple.$Tuple($6, Data$dTuple.$Tuple(Val.$EnvExpr($3, eα), $7))));
        })))(v.n))(v1 => Monad0.Applicative0().pure({
          g: v1._2._2._1,
          graph_fwd: a => b => $4(Data$dTuple.$Tuple(a, b)),
          graph_bwd: a => b => $5(Data$dTuple.$Tuple(a, b)),
          "inα": v1._2._2._2._1,
          "outα": v1._2._2._2._2
        }));
      };
    };
  };
};
export {
  apply,
  
  
  
  
  
  
  eval_primitives,
  
  
  
  
  graphEval,
  graphGC,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
};
