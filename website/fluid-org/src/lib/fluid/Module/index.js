import * as $runtime from "../runtime.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dAff$dClass from "../Effect.Aff.Class/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Eval from "../Eval/index.js";
import * as Expr from "../Expr/index.js";
import * as File from "../File/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Graph from "../Graph/index.js";
import * as Graph$dGraphImpl from "../Graph.GraphImpl/index.js";
import * as Graph$dWithGraph from "../Graph.WithGraph/index.js";
import * as Lattice from "../Lattice/index.js";
import * as Parse from "../Parse/index.js";
import * as SExpr from "../SExpr/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Util$dSet from "../Util.Set/index.js";
import * as Val from "../Val/index.js";
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
const all = /* #__PURE__ */ (() => Data$dList$dTypes.foldableList.foldMap((() => {
  const semigroupConj1 = {append: v => v1 => v && v1};
  return {mempty: true, Semigroup0: () => semigroupConj1};
})()))();
const elem = /* #__PURE__ */ (() => {
  const any1 = Data$dList$dTypes.foldableList.foldMap((() => {
    const semigroupDisj1 = {append: v => v1 => v || v1};
    return {mempty: false, Semigroup0: () => semigroupDisj1};
  })());
  return x => any1($0 => x === $0);
})();
const fromFoldable = /* #__PURE__ */ (() => Data$dSet.foldableSet.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil))();
const boundedLattice = {BoundedJoinSemilattice0: () => Lattice.boundedJoinSemilatticeUni, BoundedMeetSemilattice1: () => Lattice.boundedMeetSemilatticeUni};
const member = k => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Leaf") {
        go$c = false;
        go$r = false;
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
          go$r = true;
          continue;
        }
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go;
};
const unions = /* #__PURE__ */ Data$dSet.unions(Data$dList$dTypes.foldableList)(Graph.ordDVertex$p);
const union = /* #__PURE__ */ (() => Util$dSet.setSet(Graph.ordDVertex$p).union)();
const prelude = "lib/prelude";
const loadModuleGraph = dictMonadAff => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const $0 = Monad0.Bind1();
  const $1 = Monad0.Applicative0();
  return dictMonadError => {
    const throwLeft = Util.throwLeft(dictMonadError)(Data$dShow.showString);
    const withMsg = Util.withMsg(dictMonadError);
    const desugarModuleFwd = SExpr.moduleFwd(dictMonadError)(boundedLattice);
    return dictMonadReader => {
      const ask = dictMonadReader.MonadAsk0().ask;
      return dictLoadFile => {
        const loadFile = File.loadFile(dictLoadFile)(Monad0)(dictMonadError)(dictMonadAff);
        return roots => {
          const collectModules = visited => graph => modules => imports => {
            if (imports.tag === "Nil") { return $1.pure(Data$dTuple.$Tuple(graph, modules)); }
            if (imports.tag === "Cons") {
              if (member(imports._1)(visited)) { return collectModules(visited)(graph)(modules)(imports._2); }
              return $0.bind($0.bind(ask)(v => $0.bind(loadFile(v.fluidSrcPaths)(imports._1 + ".fld"))(src => $0.bind(withMsg("Loading module " + imports._1)(throwLeft(Parse.parse(Parse.withImports(Parse.module_))(src))))(v1 => {
                const $2 = v1._2;
                return $0.bind(desugarModuleFwd(v1._1))(mod$p => $1.pure(Data$dTuple.$Tuple(
                  mod$p,
                  imports._1 === "lib/prelude" ? $2 : Data$dList$dTypes.$List("Cons", "lib/prelude", $2)
                )));
              }))))(v => collectModules(Data$dMap$dInternal.insert(Data$dOrd.ordString)(imports._1)()(visited))(Data$dMap$dInternal.insert(Data$dOrd.ordString)(imports._1)(v._2)(graph))(Data$dMap$dInternal.insert(Data$dOrd.ordString)(imports._1)(v._1)(modules))(Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(imports._2)(v._2)));
            }
            $runtime.fail();
          };
          return $0.bind(collectModules(Data$dMap$dInternal.Leaf)(Data$dMap$dInternal.Leaf)(Data$dMap$dInternal.Leaf)(roots))(v => {
            const $2 = v._1;
            return $1.pure({
              roots,
              topsorted: (() => {
                const go = go$a0$copy => go$a1$copy => {
                  let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
                  while (go$c) {
                    const v$1 = go$a0, v1 = go$a1;
                    if (v$1.tag === "Nil") {
                      const go$1 = go$1$a0$copy => go$1$a1$copy => {
                        let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
                        while (go$1$c) {
                          const v$2 = go$1$a0, v1$1 = go$1$a1;
                          if (v1$1.tag === "Nil") {
                            go$1$c = false;
                            go$1$r = v$2;
                            continue;
                          }
                          if (v1$1.tag === "Cons") {
                            go$1$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$2);
                            go$1$a1 = v1$1._2;
                            continue;
                          }
                          $runtime.fail();
                        }
                        return go$1$r;
                      };
                      go$c = false;
                      go$r = go$1(Data$dList$dTypes.Nil)(v1);
                      continue;
                    }
                    const go$1 = go$1$a0$copy => go$1$a1$copy => {
                      let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
                      while (go$1$c) {
                        const b = go$1$a0, v$2 = go$1$a1;
                        if (v$2.tag === "Nil") {
                          go$1$c = false;
                          go$1$r = b;
                          continue;
                        }
                        if (v$2.tag === "Cons") {
                          go$1$a0 = b.tag === "Nothing" && (() => {
                            const v2 = lookup(v$2._1)($2);
                            if (v2.tag === "Nothing") { return true; }
                            if (v2.tag === "Just") { return all(dep => !elem(dep)(v$1))(v2._1); }
                            $runtime.fail();
                          })()
                            ? Data$dMaybe.$Maybe("Just", v$2._1)
                            : b;
                          go$1$a1 = v$2._2;
                          continue;
                        }
                        $runtime.fail();
                      }
                      return go$1$r;
                    };
                    const v2 = go$1(Data$dMaybe.Nothing)(v$1);
                    if (v2.tag === "Nothing") {
                      go$c = false;
                      go$r = Effect$dException.throwException(Effect$dException.error("Modules contain circular imports"))();
                      continue;
                    }
                    if (v2.tag === "Just") {
                      go$a0 = Data$dList.deleteBy(Data$dEq.eqStringImpl)(v2._1)(v$1);
                      go$a1 = Data$dList$dTypes.$List("Cons", v2._1, v1);
                      continue;
                    }
                    $runtime.fail();
                  }
                  return go$r;
                };
                return go(fromFoldable((() => {
                  const go$1 = v$1 => {
                    if (v$1.tag === "Leaf") { return Data$dMap$dInternal.Leaf; }
                    if (v$1.tag === "Node") { return Data$dMap$dInternal.$$$Map("Node", v$1._1, v$1._2, v$1._3, undefined, go$1(v$1._5), go$1(v$1._6)); }
                    $runtime.fail();
                  };
                  return go$1($2);
                })()))(Data$dList$dTypes.Nil);
              })(),
              graph: $2,
              modules: v._2
            });
          });
        };
      };
    };
  };
};
const initialConfig = dictMonadAff => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const $0 = Control$dMonad$dState$dTrans.bindStateT(Monad0);
  const $1 = Graph$dWithGraph.monadAllocAllocT(Monad0);
  const Applicative0 = $1.Monad0().Applicative0();
  const fresh1 = $1.fresh;
  const alloc1 = Val.traversableEnv.traverse(Applicative0)(v => fresh1);
  const applicativeStateT = Control$dMonad$dState$dTrans.applicativeStateT(Monad0);
  const traverse1 = Data$dMap$dInternal.traversableMap.traverse(applicativeStateT);
  const alloc2 = Expr.traversableModule.traverse(Applicative0)(v => fresh1);
  const runWithGraphT_spy = Graph$dWithGraph.runWithGraphT_spy({
    Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(Monad0),
    Bind1: () => Control$dMonad$dState$dTrans.bindStateT(Monad0)
  })(Graph$dGraphImpl.graphGraphImpl);
  const monadAffState = Effect$dAff$dClass.monadAffState(dictMonadAff);
  const monadAffState1 = Effect$dAff$dClass.monadAffState(monadAffState);
  const $2 = monadAffState.MonadEffect0().Monad0();
  const $3 = dictMonadAff.MonadEffect0().Monad0();
  return dictMonadError => {
    const eval_primitives = Eval.eval_primitives(Graph$dWithGraph.monadWithGraphAllocWithGr(dictMonadError));
    return dictMonadReader => {
      const eval_primitives1 = eval_primitives(Control$dMonad$dState$dTrans.monadReaderStateT(Control$dMonad$dState$dTrans.monadReaderStateT(dictMonadReader)))(monadAffState1);
      return dictLoadFile => {
        const eval_primitives2 = eval_primitives1((() => {
          const loadFileFromPath1 = dictLoadFile.loadFileFromPath(dictMonadError)(dictMonadAff);
          return {
            loadFileFromPath: dictMonadError1 => dictMonadAff1 => x => {
              const $4 = loadFileFromPath1(x);
              return s => $2.Bind1().bind(s$1 => $3.Bind1().bind($4)(x$1 => $3.Applicative0().pure(Data$dTuple.$Tuple(x$1, s$1))))(x$1 => $2.Applicative0().pure(Data$dTuple.$Tuple(
                x$1,
                s
              )));
            }
          };
        })());
        return dictFV => e => primitives => moduleCxt => Monad0.Bind1().bind(Graph$dWithGraph.runAllocT(Monad0)($0.bind(alloc1(primitives))(primitives$p => $0.bind(traverse1(alloc2)(moduleCxt.modules))(modules$p => $0.bind(runWithGraphT_spy(eval_primitives2(primitives$p)({
          modules: modules$p,
          graph: moduleCxt.graph,
          roots: moduleCxt.roots,
          topsorted: moduleCxt.topsorted
        }))(union(Val.unions1(Data$dList$dTypes.listMap(Val.verticesValVertex.vertices)(Util$dMap.mapObjectString.values(primitives$p))))(unions(Data$dList$dTypes.listMap(Expr.verticesModuleVertex.vertices)((() => {
          const go = (m$p, z$p) => {
            if (m$p.tag === "Leaf") { return z$p; }
            if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._4, go(m$p._6, z$p))); }
            $runtime.fail();
          };
          return go(modules$p, Data$dList$dTypes.Nil);
        })())))))(v => applicativeStateT.pure(Data$dTuple.$Tuple(
          primitives$p,
          Data$dTuple.$Tuple(
            modules$p,
            (() => {
              const $4 = dictFV.fv(e);
              return Foreign$dObject.filterWithKey(x => {
                const $5 = Util$dSet.setSet(Data$dOrd.ordString).member(x)($4);
                return v$1 => $5;
              })(v._2);
            })()
          )
        ))))))(0))(v => Monad0.Applicative0().pure({n: v._1, primitives: v._2._2._1, "γ": v._2._2._2._2}));
      };
    };
  };
};
const prepConfig = dictMonadAff => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const $0 = Monad0.Bind1();
  const loadModuleGraph1 = loadModuleGraph(dictMonadAff);
  const initialConfig1 = initialConfig(dictMonadAff);
  return dictMonadError => {
    const throwLeft = Util.throwLeft(dictMonadError)(Data$dShow.showString);
    const loadModuleGraph2 = loadModuleGraph1(dictMonadError);
    const desug1 = SExpr.exprFwd(boundedLattice)(dictMonadError)(Lattice.joinSemilatticeUnit);
    const initialConfig2 = initialConfig1(dictMonadError);
    return dictMonadReader => {
      const loadModuleGraph3 = loadModuleGraph2(dictMonadReader);
      const initialConfig3 = initialConfig2(dictMonadReader);
      return dictLoadFile => {
        const loadModuleGraph4 = loadModuleGraph3(dictLoadFile);
        const initialConfig4 = initialConfig3(dictLoadFile)(Expr.fVExpr);
        return primitives => fluidSrc => $0.bind(throwLeft(Parse.parse(Parse.withImports(Parse.expr))(fluidSrc)))(v => {
          const $1 = v._1;
          return $0.bind(loadModuleGraph4(Data$dList$dTypes.$List("Cons", "lib/prelude", v._2)))(moduleCxt => $0.bind(desug1($1))(e => $0.bind(initialConfig4(e)(primitives)(moduleCxt))(gconfig => Monad0.Applicative0().pure({
            s: $1,
            e,
            gconfig
          }))));
        });
      };
    };
  };
};
export {         prepConfig,  };
