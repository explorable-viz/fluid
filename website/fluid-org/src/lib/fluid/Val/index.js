import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dBitraversable from "../Data.Bitraversable/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
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
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Dict from "../Dict/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Expr from "../Expr/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Graph from "../Graph/index.js";
import * as Lattice from "../Lattice/index.js";
import * as Pretty$dDoc from "../Pretty.Doc/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Util$dSet from "../Util.Set/index.js";
const $BaseVal = (tag, _1, _2) => ({tag, _1, _2});
const $EnvExpr = (_1, _2) => ({tag: "EnvExpr", _1, _2});
const $ForeignOp$p = _1 => ({tag: "ForeignOp'", _1});
const $Fun = (tag, _1, _2, _3) => ({tag, _1, _2, _3});
const $Val = (_1, _2, _3) => ({tag: "Val", _1, _2, _3});
const setSet = /* #__PURE__ */ Util$dSet.setSet(Graph.ordDVertex$p);
const unions = /* #__PURE__ */ Data$dSet.unions(Data$dFoldable.foldableArray)(Graph.ordDVertex$p);
const vertices = /* #__PURE__ */ (() => Graph.verticesDict(Expr.verticesElimVertex).vertices)();
const unions1 = /* #__PURE__ */ Data$dSet.unions(Data$dList$dTypes.foldableList)(Graph.ordDVertex$p);
const foldMap = /* #__PURE__ */ Foreign$dObject.foldMap(/* #__PURE__ */ Data$dSet.monoidSet(Graph.ordDVertex$p));
const identity = x => x;
const ordTuple = dictOrd1 => {
  const $0 = dictOrd1.Eq0();
  const eqTuple2 = {eq: x => y => x._1 === y._1 && $0.eq(x._2)(y._2)};
  return {
    compare: x => y => {
      const v = Data$dOrd.ordInt.compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      return dictOrd1.compare(x._2)(y._2);
    },
    Eq0: () => eqTuple2
  };
};
const boundedLattice = {BoundedJoinSemilattice0: () => Lattice.boundedJoinSemilatticeUni, BoundedMeetSemilattice1: () => Lattice.boundedMeetSemilatticeUni};
const boundedLattice1 = {BoundedJoinSemilattice0: () => Lattice.boundedJoinSemilatticeBoo, BoundedMeetSemilattice1: () => Lattice.boundedMeetSemilatticeBoo};
const setSet1 = /* #__PURE__ */ Util$dSet.setSet(Data$dOrd.ordString);
const toUnfoldable1 = x => {
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
const fromFoldable1 = /* #__PURE__ */ (() => {
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
const MatrixDim = x => x;
const DictKey = x => x;
const MatrixRep = x => x;
const Val = value0 => value1 => value2 => $Val(value0, value1, value2);
const Int = value0 => $BaseVal("Int", value0);
const Float = value0 => $BaseVal("Float", value0);
const Str = value0 => $BaseVal("Str", value0);
const Constr = value0 => value1 => $BaseVal("Constr", value0, value1);
const Dictionary = value0 => $BaseVal("Dictionary", value0);
const Matrix = value0 => $BaseVal("Matrix", value0);
const Fun = value0 => $BaseVal("Fun", value0);
const DictRep = x => x;
const Closure = value0 => value1 => value2 => $Fun("Closure", value0, value1, value2);
const Foreign = value0 => value1 => $Fun("Foreign", value0, value1);
const PartialConstr = value0 => value1 => $Fun("PartialConstr", value0, value1);
const Env = x => x;
const ForeignOp = x => x;
const ForeignOp$p = value0 => $ForeignOp$p(value0);
const EnvExpr = value0 => value1 => $EnvExpr(value0, value1);
const typeNameVal = {typeName: v => "Val"};
const pack = x => k => k(typeNameVal)(x);
const typeNameMatrixDim = {typeName: v => "MatrixDim"};
const pack1 = x => k => k(typeNameMatrixDim)(x);
const verticesMatrixDimVertex = {
  vertices: v => Data$dMap$dInternal.$$$Map("Node", 1, 1, Data$dTuple.$Tuple(v._2, pack1(v)), undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf)
};
const typeNameDictKey = {typeName: v => "DictKey"};
const pack2 = x => k => k(typeNameDictKey)(x);
const verticesDictKeyVertex = {
  vertices: v => Data$dMap$dInternal.$$$Map("Node", 1, 1, Data$dTuple.$Tuple(v._2, pack2(v)), undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf)
};
const verticesValVertex = {
  vertices: v => setSet.union(Data$dMap$dInternal.$$$Map("Node", 1, 1, Data$dTuple.$Tuple(v._1, pack(v)), undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(verticesBaseValVertex.vertices(v._3))
};
const verticesMatrixRepVertex = {
  vertices: v => setSet.union(unions(Data$dArray.concat(Data$dFunctor.arrayMap(Data$dFunctor.arrayMap(verticesValVertex.vertices))(v._1))))(setSet.union(Data$dMap$dInternal.$$$Map(
    "Node",
    1,
    1,
    Data$dTuple.$Tuple(v._2._1._2, pack1(v._2._1)),
    undefined,
    Data$dMap$dInternal.Leaf,
    Data$dMap$dInternal.Leaf
  ))(Data$dMap$dInternal.$$$Map("Node", 1, 1, Data$dTuple.$Tuple(v._2._2._2, pack1(v._2._2)), undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf)))
};
const verticesFunVertex = {
  vertices: v => {
    if (v.tag === "Closure") { return setSet.union(verticesEnvVertex.vertices(v._1))(setSet.union(vertices(v._2))(Expr.verticesElimVertex.vertices(v._3))); }
    if (v.tag === "Foreign") { return unions1(Data$dList$dTypes.listMap(verticesValVertex.vertices)(v._2)); }
    if (v.tag === "PartialConstr") { return unions1(Data$dList$dTypes.listMap(verticesValVertex.vertices)(v._2)); }
    $runtime.fail();
  }
};
const verticesEnvVertex = {vertices: v => unions1(Data$dList$dTypes.listMap(verticesValVertex.vertices)(Util$dMap.mapObjectString.values(v)))};
const verticesDictRepVertex = {
  vertices: v => foldMap(k => v1 => setSet.union(Data$dMap$dInternal.$$$Map(
    "Node",
    1,
    1,
    Data$dTuple.$Tuple(v1._1, pack2(Data$dTuple.$Tuple(k, v1._1))),
    undefined,
    Data$dMap$dInternal.Leaf,
    Data$dMap$dInternal.Leaf
  ))(verticesValVertex.vertices(v1._2)))(v)
};
const verticesBaseValVertex = {
  vertices: v => {
    if (v.tag === "Int") { return setSet.empty; }
    if (v.tag === "Float") { return setSet.empty; }
    if (v.tag === "Str") { return setSet.empty; }
    if (v.tag === "Constr") { return unions1(Data$dList$dTypes.listMap(verticesValVertex.vertices)(v._2)); }
    if (v.tag === "Dictionary") { return verticesDictRepVertex.vertices(v._1); }
    if (v.tag === "Matrix") { return verticesMatrixRepVertex.vertices(v._1); }
    if (v.tag === "Fun") { return verticesFunVertex.vertices(v._1); }
    $runtime.fail();
  }
};
const verticesEnvExprVertex = {
  vertices: v => setSet.union(unions1(Data$dList$dTypes.listMap(verticesValVertex.vertices)(Util$dMap.mapObjectString.values(v._1))))(Expr.verticesExprVertex.vertices(v._2))
};
const newtypeEnv_ = {Coercible0: () => {}};
const joinSemilatticeMatrixDim = dictJoinSemilattice => (
  {
    join: v => v1 => {
      const $0 = v._1;
      const $1 = v1._1;
      return Data$dTuple.$Tuple(Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0), dictJoinSemilattice.join(v._2)(v1._2));
    }
  }
);
const isEmptyEnv = {isEmpty: v => Foreign$dObject.isEmpty(v)};
const setEnvString = {
  empty: Foreign$dObject.empty,
  filter: p => v => Foreign$dObject.filterWithKey(x => {
    const $0 = p(x);
    return v$1 => $0;
  })(v),
  size: v => Foreign$dObject.size(v),
  member: x => v => Object.hasOwn(v, x),
  difference: v => v1 => Util$dSet.setObjectString.difference(v)(v1),
  union: v => v1 => Foreign$dObject.union(v)(v1),
  IsEmpty0: () => isEmptyEnv
};
const mapEnvStringVal = {
  maplet: k => v => {
    const $0 = {};
    $0[k] = v;
    return $0;
  },
  keys: v => Util$dMap.mapObjectString.keys(v),
  values: v => Util$dMap.mapObjectString.values(v),
  filterKeys: p => v => Foreign$dObject.filterWithKey(x => {
    const $0 = p(x);
    return v$1 => $0;
  })(v),
  unionWith: f => v => v1 => Foreign$dObject.unionWith(f)(v)(v1),
  lookup: k => v => Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, k, v),
  delete: k => v => Foreign$dObject.mutate($0 => () => {
    delete $0[k];
    return $0;
  })(v),
  insert: k => v => v1 => Foreign$dObject.mutate($0 => () => {
    $0[k] = v;
    return $0;
  })(v1),
  toUnfoldable: dictUnfoldable => Foreign$dObject.toAscUnfoldable(dictUnfoldable),
  Set0: () => setEnvString
};
const highlightableVertex = {
  highlightIf: v => doc => Pretty$dDoc.$Doc("Concat", doc, Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "_"), Pretty$dDoc.$Doc("Text", "⟨" + v + "⟩")))
};
const highlightableUnit = {highlightIf: v => identity};
const highlightableBoolean = {
  highlightIf: v => {
    if (!v) { return identity; }
    if (v) { return doc => Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "⸨"), Pretty$dDoc.$Doc("Concat", doc, Pretty$dDoc.$Doc("Text", "⸩"))); }
    $runtime.fail();
  }
};
const functorMatrixDim = {map: f => m => Data$dTuple.$Tuple(m._1, f(m._2))};
const functorVal = {
  map: f => m => $Val(
    f(m._1),
    (() => {
      const $0 = functorVal.map(f);
      if (m._2.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0(m._2._1)); }
      return Data$dMaybe.Nothing;
    })(),
    functorBaseVal.map(f)(m._3)
  )
};
const functorMatrixRep = {
  map: f => m => Data$dTuple.$Tuple(
    Data$dFunctor.arrayMap(Data$dFunctor.arrayMap(functorVal.map(f)))(m._1),
    Data$dTuple.$Tuple(Data$dTuple.$Tuple(m._2._1._1, f(m._2._1._2)), Data$dTuple.$Tuple(m._2._2._1, f(m._2._2._2)))
  )
};
const functorFun = {
  map: f => m => {
    if (m.tag === "Closure") { return $Fun("Closure", functorEnv.map(f)(m._1), Foreign$dObject._fmapObject(m._2, Expr.functorElim.map(f)), Expr.functorElim.map(f)(m._3)); }
    if (m.tag === "Foreign") { return $Fun("Foreign", m._1, Data$dList$dTypes.listMap(functorVal.map(f))(m._2)); }
    if (m.tag === "PartialConstr") { return $Fun("PartialConstr", m._1, Data$dList$dTypes.listMap(functorVal.map(f))(m._2)); }
    $runtime.fail();
  }
};
const functorEnv = {map: f => m => Foreign$dObject._fmapObject(m, functorVal.map(f))};
const functorDictRep = {map: f => m => Foreign$dObject._fmapObject(m, v => Data$dTuple.$Tuple(f(v._1), functorVal.map(f)(v._2)))};
const functorBaseVal = {
  map: f => m => {
    if (m.tag === "Int") { return $BaseVal("Int", m._1); }
    if (m.tag === "Float") { return $BaseVal("Float", m._1); }
    if (m.tag === "Str") { return $BaseVal("Str", m._1); }
    if (m.tag === "Constr") { return $BaseVal("Constr", m._1, Data$dList$dTypes.listMap(functorVal.map(f))(m._2)); }
    if (m.tag === "Dictionary") { return $BaseVal("Dictionary", functorDictRep.map(f)(m._1)); }
    if (m.tag === "Matrix") { return $BaseVal("Matrix", functorMatrixRep.map(f)(m._1)); }
    if (m.tag === "Fun") { return $BaseVal("Fun", functorFun.map(f)(m._1)); }
    $runtime.fail();
  }
};
const botOfUnit$x215Raw$x215 = dictBoundedJoinSemilattice => (
  {
    botOf: (() => {
      const $0 = dictBoundedJoinSemilattice.bot;
      const $1 = functorVal.map((() => {
        const $1 = dictBoundedJoinSemilattice.bot;
        return v => $1;
      })());
      return x => Data$dTuple.$Tuple($0, $1(x._2));
    })()
  }
);
const functorEnvExpr = {map: f => m => $EnvExpr(Foreign$dObject._fmapObject(m._1, functorVal.map(f)), Expr.functorExpr.map(f)(m._2))};
const foldableMatrixDim = {foldl: f => z => m => f(z)(m._2), foldr: f => z => m => f(m._2)(z), foldMap: dictMonoid => f => m => f(m._2)};
const traversableMatrixDim = {
  traverse: dictApplicative => f => m => dictApplicative.Apply0().Functor0().map(v1 => v1)(Data$dTraversable.traversableTuple.traverse(dictApplicative)(f)(m)),
  sequence: dictApplicative => v => traversableMatrixDim.traverse(dictApplicative)(identity)(v),
  Functor0: () => functorMatrixDim,
  Foldable1: () => foldableMatrixDim
};
const foldableVal = {
  foldl: f => z => m => foldableBaseVal.foldl(f)((() => {
    const $0 = foldableVal.foldl(f);
    const $1 = f(z)(m._1);
    if (m._2.tag === "Nothing") { return $1; }
    if (m._2.tag === "Just") { return $0($1)(m._2._1); }
    $runtime.fail();
  })())(m._3),
  foldr: f => z => m => f(m._1)((() => {
    const $0 = foldableVal.foldr(f);
    const $1 = foldableBaseVal.foldr(f)(z)(m._3);
    if (m._2.tag === "Nothing") { return $1; }
    if (m._2.tag === "Just") { return $0($1)(m._2._1); }
    $runtime.fail();
  })()),
  foldMap: dictMonoid => {
    const $0 = dictMonoid.Semigroup0();
    const mempty = dictMonoid.mempty;
    return f => m => $0.append(f(m._1))($0.append((() => {
      const $1 = foldableVal.foldMap(dictMonoid)(f);
      if (m._2.tag === "Nothing") { return mempty; }
      if (m._2.tag === "Just") { return $1(m._2._1); }
      $runtime.fail();
    })())(foldableBaseVal.foldMap(dictMonoid)(f)(m._3)));
  }
};
const foldableMatrixRep = {
  foldl: f => acc => v => Data$dFoldable.foldlArray(Data$dFoldable.foldlArray(foldableVal.foldl(f)))(f(f(acc)(v._2._1._2))(v._2._2._2))(v._1),
  foldr: f => Data$dFoldable.foldrDefault(foldableMatrixRep)(f),
  foldMap: dictMonoid => f => foldableMatrixRep.foldl(acc => x => dictMonoid.Semigroup0().append(acc)(f(x)))(dictMonoid.mempty)
};
const foldableFun = {
  foldl: f => z => m => {
    if (m.tag === "Closure") { return Expr.foldableElim.foldl(f)(Foreign$dObject.fold(z$1 => v => Expr.foldableElim.foldl(f)(z$1))(foldableEnv.foldl(f)(z)(m._1))(m._2))(m._3); }
    if (m.tag === "Foreign") {
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
            go$a0 = foldableVal.foldl(f)(b)(v._1);
            go$a1 = v._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      return go(z)(m._2);
    }
    if (m.tag === "PartialConstr") {
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
            go$a0 = foldableVal.foldl(f)(b)(v._1);
            go$a1 = v._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      return go(z)(m._2);
    }
    $runtime.fail();
  },
  foldr: f => z => m => {
    if (m.tag === "Closure") {
      return foldableEnv.foldr(f)(Data$dFoldable.foldrArray(b => a => Expr.foldableElim.foldr(f)(a)(b))(Expr.foldableElim.foldr(f)(z)(m._3))(Foreign$dObject.values(m._2)))(m._1);
    }
    if (m.tag === "Foreign") { return Data$dList$dTypes.foldableList.foldr(b => a => foldableVal.foldr(f)(a)(b))(z)(m._2); }
    if (m.tag === "PartialConstr") { return Data$dList$dTypes.foldableList.foldr(b => a => foldableVal.foldr(f)(a)(b))(z)(m._2); }
    $runtime.fail();
  },
  foldMap: dictMonoid => {
    const $0 = dictMonoid.Semigroup0();
    const foldMap1 = Foreign$dObject.foldMap(dictMonoid);
    const foldMap9 = Expr.foldableElim.foldMap(dictMonoid);
    const foldMap10 = Data$dList$dTypes.foldableList.foldMap(dictMonoid);
    return f => m => {
      if (m.tag === "Closure") {
        return $0.append(foldableEnv.foldMap(dictMonoid)(f)(m._1))($0.append((() => {
          const $1 = foldMap9(f);
          return foldMap1(v => $1)(m._2);
        })())(foldMap9(f)(m._3)));
      }
      if (m.tag === "Foreign") { return foldMap10(foldableVal.foldMap(dictMonoid)(f))(m._2); }
      if (m.tag === "PartialConstr") { return foldMap10(foldableVal.foldMap(dictMonoid)(f))(m._2); }
      $runtime.fail();
    };
  }
};
const foldableEnv = {
  foldl: f => z => m => Foreign$dObject.fold(z$1 => v => foldableVal.foldl(f)(z$1))(z)(m),
  foldr: f => z => m => Data$dFoldable.foldrArray(b => a => foldableVal.foldr(f)(a)(b))(z)(Foreign$dObject.values(m)),
  foldMap: dictMonoid => {
    const foldMap1 = Foreign$dObject.foldMap(dictMonoid);
    return f => m => {
      const $0 = foldableVal.foldMap(dictMonoid)(f);
      return foldMap1(v => $0)(m);
    };
  }
};
const foldableDictRep = {
  foldl: f => acc => v => Foreign$dObject.fold(z => v$1 => v1 => foldableVal.foldl(f)(f(z)(v1._1))(v1._2))(acc)(v),
  foldr: f => Data$dFoldable.foldrDefault(foldableDictRep)(f),
  foldMap: dictMonoid => f => foldableDictRep.foldl(acc => x => dictMonoid.Semigroup0().append(acc)(f(x)))(dictMonoid.mempty)
};
const foldableBaseVal = {
  foldl: f => z => m => {
    if (m.tag === "Int") { return z; }
    if (m.tag === "Float") { return z; }
    if (m.tag === "Str") { return z; }
    if (m.tag === "Constr") {
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
            go$a0 = foldableVal.foldl(f)(b)(v._1);
            go$a1 = v._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      return go(z)(m._2);
    }
    if (m.tag === "Dictionary") { return foldableDictRep.foldl(f)(z)(m._1); }
    if (m.tag === "Matrix") { return foldableMatrixRep.foldl(f)(z)(m._1); }
    if (m.tag === "Fun") { return foldableFun.foldl(f)(z)(m._1); }
    $runtime.fail();
  },
  foldr: f => z => m => {
    if (m.tag === "Int") { return z; }
    if (m.tag === "Float") { return z; }
    if (m.tag === "Str") { return z; }
    if (m.tag === "Constr") { return Data$dList$dTypes.foldableList.foldr(b => a => foldableVal.foldr(f)(a)(b))(z)(m._2); }
    if (m.tag === "Dictionary") { return Data$dFoldable.foldrDefault(foldableDictRep)(f)(z)(m._1); }
    if (m.tag === "Matrix") { return Data$dFoldable.foldrDefault(foldableMatrixRep)(f)(z)(m._1); }
    if (m.tag === "Fun") { return foldableFun.foldr(f)(z)(m._1); }
    $runtime.fail();
  },
  foldMap: dictMonoid => {
    const mempty = dictMonoid.mempty;
    const foldMap8 = Data$dList$dTypes.foldableList.foldMap(dictMonoid);
    return f => m => {
      if (m.tag === "Int") { return mempty; }
      if (m.tag === "Float") { return mempty; }
      if (m.tag === "Str") { return mempty; }
      if (m.tag === "Constr") { return foldMap8(foldableVal.foldMap(dictMonoid)(f))(m._2); }
      if (m.tag === "Dictionary") { return foldableDictRep.foldMap(dictMonoid)(f)(m._1); }
      if (m.tag === "Matrix") { return foldableMatrixRep.foldMap(dictMonoid)(f)(m._1); }
      if (m.tag === "Fun") { return foldableFun.foldMap(dictMonoid)(f)(m._1); }
      $runtime.fail();
    };
  }
};
const foldableEnvExpr = {
  foldl: f => z => m => Expr.foldableExpr.foldl(f)(Foreign$dObject.fold(z$1 => v => foldableVal.foldl(f)(z$1))(z)(m._1))(m._2),
  foldr: f => z => m => foldableEnv.foldr(f)(Expr.foldableExpr.foldr(f)(z)(m._2))(m._1),
  foldMap: dictMonoid => {
    const foldMap8 = foldableEnv.foldMap(dictMonoid);
    const foldMap9 = Expr.foldableExpr.foldMap(dictMonoid);
    return f => m => dictMonoid.Semigroup0().append(foldMap8(f)(m._1))(foldMap9(f)(m._2));
  }
};
const traversableVal = {
  traverse: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    return f => m => Apply0.apply(Apply0.apply(Apply0.Functor0().map(v3 => v4 => v5 => $Val(v3, v4, v5))(f(m._1)))(Data$dTraversable.traversableMaybe.traverse(dictApplicative)(traversableVal.traverse(dictApplicative)(f))(m._2)))(traversableBaseVal.traverse(dictApplicative)(f)(m._3));
  },
  sequence: dictApplicative => v => traversableVal.traverse(dictApplicative)(identity)(v),
  Functor0: () => functorVal,
  Foldable1: () => foldableVal
};
const traversableMatrixRep = {
  traverse: dictApplicative => {
    const bitraverse1 = Data$dBitraversable.bitraversableTuple.bitraverse(dictApplicative);
    const traverse9 = Data$dTraversable.traversableArray.traverse(dictApplicative);
    return f => v => dictApplicative.Apply0().Functor0().map(MatrixRep)(bitraverse1(traverse9(traverse9(traversableVal.traverse(dictApplicative)(f))))(bitraverse1(traversableMatrixDim.traverse(dictApplicative)(f))(traversableMatrixDim.traverse(dictApplicative)(f)))(v));
  },
  sequence: dictApplicative => traversableMatrixRep.traverse(dictApplicative)(Data$dTraversable.identity),
  Functor0: () => functorMatrixRep,
  Foldable1: () => foldableMatrixRep
};
const traversableFun = {
  traverse: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    const $0 = Apply0.Functor0();
    const traverse9 = Dict.traversableDict.traverse(dictApplicative);
    const traverse10 = Expr.traversableElim.traverse(dictApplicative);
    const traverse11 = Data$dList$dTypes.traversableList.traverse(dictApplicative);
    return f => m => {
      if (m.tag === "Closure") {
        return Apply0.apply(Apply0.apply($0.map(v3 => v4 => v5 => $Fun("Closure", v3, v4, v5))(traversableEnv.traverse(dictApplicative)(f)(m._1)))(traverse9(traverse10(f))(m._2)))(traverse10(f)(m._3));
      }
      if (m.tag === "Foreign") {
        const $1 = m._1;
        return $0.map(v2 => $Fun("Foreign", $1, v2))(traverse11(traversableVal.traverse(dictApplicative)(f))(m._2));
      }
      if (m.tag === "PartialConstr") {
        const $1 = m._1;
        return $0.map(v2 => $Fun("PartialConstr", $1, v2))(traverse11(traversableVal.traverse(dictApplicative)(f))(m._2));
      }
      $runtime.fail();
    };
  },
  sequence: dictApplicative => v => traversableFun.traverse(dictApplicative)(identity)(v),
  Functor0: () => functorFun,
  Foldable1: () => foldableFun
};
const traversableEnv = {
  traverse: dictApplicative => {
    const traverse9 = Dict.traversableDict.traverse(dictApplicative);
    return f => m => dictApplicative.Apply0().Functor0().map(v1 => v1)(traverse9(traversableVal.traverse(dictApplicative)(f))(m));
  },
  sequence: dictApplicative => v => traversableEnv.traverse(dictApplicative)(identity)(v),
  Functor0: () => functorEnv,
  Foldable1: () => foldableEnv
};
const traversableDictRep = {
  traverse: dictApplicative => {
    const traverse9 = Dict.traversableDict.traverse(dictApplicative);
    const bitraverse1 = Data$dBitraversable.bitraversableTuple.bitraverse(dictApplicative);
    return f => v => dictApplicative.Apply0().Functor0().map(DictRep)(traverse9(bitraverse1(f)(traversableVal.traverse(dictApplicative)(f)))(v));
  },
  sequence: dictApplicative => traversableDictRep.traverse(dictApplicative)(Data$dTraversable.identity),
  Functor0: () => functorDictRep,
  Foldable1: () => foldableDictRep
};
const traversableBaseVal = {
  traverse: dictApplicative => {
    const $0 = dictApplicative.Apply0().Functor0();
    const traverse9 = Data$dList$dTypes.traversableList.traverse(dictApplicative);
    return f => m => {
      if (m.tag === "Int") { return dictApplicative.pure($BaseVal("Int", m._1)); }
      if (m.tag === "Float") { return dictApplicative.pure($BaseVal("Float", m._1)); }
      if (m.tag === "Str") { return dictApplicative.pure($BaseVal("Str", m._1)); }
      if (m.tag === "Constr") {
        const $1 = m._1;
        return $0.map(v2 => $BaseVal("Constr", $1, v2))(traverse9(traversableVal.traverse(dictApplicative)(f))(m._2));
      }
      if (m.tag === "Dictionary") { return $0.map(v1 => $BaseVal("Dictionary", v1))(traversableDictRep.traverse(dictApplicative)(f)(m._1)); }
      if (m.tag === "Matrix") { return $0.map(v1 => $BaseVal("Matrix", v1))(traversableMatrixRep.traverse(dictApplicative)(f)(m._1)); }
      if (m.tag === "Fun") { return $0.map(v1 => $BaseVal("Fun", v1))(traversableFun.traverse(dictApplicative)(f)(m._1)); }
      $runtime.fail();
    };
  },
  sequence: dictApplicative => v => traversableBaseVal.traverse(dictApplicative)(identity)(v),
  Functor0: () => functorBaseVal,
  Foldable1: () => foldableBaseVal
};
const traversableEnvExpr = {
  traverse: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    const traverse9 = traversableEnv.traverse(dictApplicative);
    const traverse10 = Expr.traversableExpr.traverse(dictApplicative);
    return f => m => Apply0.apply(Apply0.Functor0().map(v2 => v3 => $EnvExpr(v2, v3))(traverse9(f)(m._1)))(traverse10(f)(m._2));
  },
  sequence: dictApplicative => v => traversableEnvExpr.traverse(dictApplicative)(identity)(v),
  Functor0: () => functorEnvExpr,
  Foldable1: () => foldableEnvExpr
};
const expandableMatrixDimRawMat = dictBoundedJoinSemilattice => (
  {
    expand: v => v1 => {
      const $0 = v._1;
      const $1 = v1._1;
      return Data$dTuple.$Tuple(Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0), v._2);
    }
  }
);
const expandableValRawVal = dictBoundedJoinSemilattice => (
  {
    expand: v => v1 => $Val(
      v._1,
      Lattice.expandableMaybeMaybe(expandableValRawVal(dictBoundedJoinSemilattice)).expand(v._2)(v1._2),
      expandableBaseValRawBaseV(dictBoundedJoinSemilattice).expand(v._3)(v1._3)
    )
  }
);
const expandableMatrixRepRawMat = dictBoundedJoinSemilattice => {
  const $0 = expandableMatrixDimRawMat(dictBoundedJoinSemilattice);
  return {
    expand: v => v1 => Data$dTuple.$Tuple(
      (() => {
        const expand1 = expandableValRawVal(dictBoundedJoinSemilattice).expand;
        return Data$dArray.zipWithImpl(xs => Data$dArray.zipWith(expand1)(xs), v._1, v1._1);
      })(),
      Data$dTuple.$Tuple($0.expand(v._2._1)(v1._2._1), $0.expand(v._2._2)(v1._2._2))
    )
  };
};
const expandableFunRawFun = dictBoundedJoinSemilattice => {
  const expandableElimRawElim = Expr.expandableElimRawElim(dictBoundedJoinSemilattice);
  return {
    expand: v => v1 => {
      if (v.tag === "Closure") {
        if (v1.tag === "Closure") {
          return $Fun(
            "Closure",
            expandableEnvRawEnv(dictBoundedJoinSemilattice).expand(v._1)(v1._1),
            Lattice.expandableDictDict({
              botOf: Expr.functorElim.map((() => {
                const $0 = dictBoundedJoinSemilattice.bot;
                return v$1 => $0;
              })())
            })(expandableElimRawElim).expand(v._2)(v1._2),
            expandableElimRawElim.expand(v._3)(v1._3)
          );
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Foreign") {
        if (v1.tag === "Foreign") {
          return $Fun(
            "Foreign",
            v._1,
            (() => {
              const $0 = expandableValRawVal(dictBoundedJoinSemilattice);
              const go = go$a0$copy => go$a1$copy => go$a2$copy => {
                let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
                while (go$c) {
                  const v$1 = go$a0, v1$1 = go$a1, v2 = go$a2;
                  if (v$1.tag === "Nil") {
                    go$c = false;
                    go$r = v2;
                    continue;
                  }
                  if (v1$1.tag === "Nil") {
                    go$c = false;
                    go$r = v2;
                    continue;
                  }
                  if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                    go$a0 = v$1._2;
                    go$a1 = v1$1._2;
                    go$a2 = Data$dList$dTypes.$List("Cons", $0.expand(v$1._1)(v1$1._1), v2);
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
              return go$1(Data$dList$dTypes.Nil)(go(v._2)(v1._2)(Data$dList$dTypes.Nil));
            })()
          );
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "PartialConstr" && v1.tag === "PartialConstr") {
        const $0 = v._1;
        const $1 = v1._1;
        return $Fun(
          "PartialConstr",
          Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0),
          (() => {
            const $2 = expandableValRawVal(dictBoundedJoinSemilattice);
            const go = go$a0$copy => go$a1$copy => go$a2$copy => {
              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
              while (go$c) {
                const v$1 = go$a0, v1$1 = go$a1, v2 = go$a2;
                if (v$1.tag === "Nil") {
                  go$c = false;
                  go$r = v2;
                  continue;
                }
                if (v1$1.tag === "Nil") {
                  go$c = false;
                  go$r = v2;
                  continue;
                }
                if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                  go$a0 = v$1._2;
                  go$a1 = v1$1._2;
                  go$a2 = Data$dList$dTypes.$List("Cons", $2.expand(v$1._1)(v1$1._1), v2);
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
            return go$1(Data$dList$dTypes.Nil)(go(v._2)(v1._2)(Data$dList$dTypes.Nil));
          })()
        );
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  };
};
const expandableEnvRawEnv = dictBoundedJoinSemilattice => {
  const expandableDictDict = Lattice.expandableDictDict({
    botOf: functorVal.map((() => {
      const $0 = dictBoundedJoinSemilattice.bot;
      return v => $0;
    })())
  });
  return {expand: v => v1 => expandableDictDict(expandableValRawVal(dictBoundedJoinSemilattice)).expand(v)(v1)};
};
const expandableDictRepRawDictR = dictBoundedJoinSemilattice => {
  const expandableDictDict = Lattice.expandableDictDict(botOfUnit$x215Raw$x215(dictBoundedJoinSemilattice));
  return {
    expand: v => v1 => expandableDictDict((() => {
      const $0 = expandableValRawVal(dictBoundedJoinSemilattice);
      return {expand: v$1 => v1$1 => Data$dTuple.$Tuple(v$1._1, $0.expand(v$1._2)(v1$1._2))};
    })()).expand(v)(v1)
  };
};
const expandableBaseValRawBaseV = dictBoundedJoinSemilattice => (
  {
    expand: v => v1 => {
      if (v.tag === "Int") {
        if (v1.tag === "Int") {
          const $0 = v._1;
          const $1 = v1._1;
          return $BaseVal("Int", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Float") {
        if (v1.tag === "Float") {
          const $0 = v._1;
          const $1 = v1._1;
          return $BaseVal("Float", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Str") {
        if (v1.tag === "Str") {
          const $0 = v._1;
          const $1 = v1._1;
          return $BaseVal("Str", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Dictionary") {
        if (v1.tag === "Dictionary") { return $BaseVal("Dictionary", expandableDictRepRawDictR(dictBoundedJoinSemilattice).expand(v._1)(v1._1)); }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Constr") {
        if (v1.tag === "Constr") {
          const $0 = v._1;
          const $1 = v1._1;
          return $BaseVal(
            "Constr",
            Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0),
            (() => {
              const $2 = expandableValRawVal(dictBoundedJoinSemilattice);
              const go = go$a0$copy => go$a1$copy => go$a2$copy => {
                let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
                while (go$c) {
                  const v$1 = go$a0, v1$1 = go$a1, v2 = go$a2;
                  if (v$1.tag === "Nil") {
                    go$c = false;
                    go$r = v2;
                    continue;
                  }
                  if (v1$1.tag === "Nil") {
                    go$c = false;
                    go$r = v2;
                    continue;
                  }
                  if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                    go$a0 = v$1._2;
                    go$a1 = v1$1._2;
                    go$a2 = Data$dList$dTypes.$List("Cons", $2.expand(v$1._1)(v1$1._1), v2);
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
              return go$1(Data$dList$dTypes.Nil)(go(v._2)(v1._2)(Data$dList$dTypes.Nil));
            })()
          );
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Matrix") {
        if (v1.tag === "Matrix") { return $BaseVal("Matrix", expandableMatrixRepRawMat(dictBoundedJoinSemilattice).expand(v._1)(v1._1)); }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Fun" && v1.tag === "Fun") { return $BaseVal("Fun", expandableFunRawFun(dictBoundedJoinSemilattice).expand(v._1)(v1._1)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  }
);
const eqMatrixDim = dictEq => ({eq: x => y => x._1 === y._1 && dictEq.eq(x._2)(y._2)});
const ordMatrixDim = dictOrd => {
  const $0 = dictOrd.Eq0();
  const eqMatrixDim1 = {eq: x => y => x._1 === y._1 && $0.eq(x._2)(y._2)};
  return {compare: x => y => ordTuple(dictOrd).compare(x)(y), Eq0: () => eqMatrixDim1};
};
const eqForeignOp = {eq: v => v1 => v._1 === v1._1};
const ordForeignOp = {compare: v => v1 => Data$dOrd.ordString.compare(v._1)(v1._1), Eq0: () => eqForeignOp};
const eqVal = dictEq => (
  {
    eq: x => y => {
      const $0 = eqVal(dictEq);
      return dictEq.eq(x._1)(y._1) && (x._2.tag === "Nothing" ? y._2.tag === "Nothing" : x._2.tag === "Just" && y._2.tag === "Just" && $0.eq(x._2._1)(y._2._1)) && eqBaseVal(dictEq).eq(x._3)(y._3);
    }
  }
);
const eqMatrixRep = dictEq => (
  {
    eq: x => y => Data$dEq.eqArrayImpl(Data$dEq.eqArrayImpl(eqVal(dictEq).eq))(x._1)(y._1) && x._2._1._1 === y._2._1._1 && dictEq.eq(x._2._1._2)(y._2._1._2) && x._2._2._1 === y._2._2._1 && dictEq.eq(x._2._2._2)(y._2._2._2)
  }
);
const eqFun = dictEq => {
  const eqElim = Expr.eqElim(dictEq);
  return {
    eq: x => y => {
      if (x.tag === "Closure") { return y.tag === "Closure" && eqEnv(dictEq).eq(x._1)(y._1) && Foreign$dObject.eqObject(eqElim).eq(x._2)(y._2) && eqElim.eq(x._3)(y._3); }
      if (x.tag === "Foreign") {
        return y.tag === "Foreign" && (() => {
          const $0 = eqVal(dictEq);
          return x._1._1 === y._1._1 && (() => {
            const go = v => v1 => v2 => {
              if (!v2) { return false; }
              if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
              return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
            };
            return go(x._2)(y._2)(true);
          })();
        })();
      }
      return x.tag === "PartialConstr" && y.tag === "PartialConstr" && (() => {
        const $0 = eqVal(dictEq);
        return x._1 === y._1 && (() => {
          const go = v => v1 => v2 => {
            if (!v2) { return false; }
            if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
            return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
          };
          return go(x._2)(y._2)(true);
        })();
      })();
    }
  };
};
const eqEnv = dictEq => ({eq: x => y => Foreign$dObject.eqObject(eqVal(dictEq)).eq(x)(y)});
const eqDictRep = dictEq => (
  {
    eq: x => y => {
      const $0 = eqVal(dictEq);
      return Foreign$dObject.eqObject({eq: x$1 => y$1 => dictEq.eq(x$1._1)(y$1._1) && $0.eq(x$1._2)(y$1._2)}).eq(x)(y);
    }
  }
);
const eqBaseVal = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "Int") { return y.tag === "Int" && x._1 === y._1; }
      if (x.tag === "Float") { return y.tag === "Float" && x._1 === y._1; }
      if (x.tag === "Str") { return y.tag === "Str" && x._1 === y._1; }
      if (x.tag === "Constr") {
        return y.tag === "Constr" && (() => {
          const $0 = eqVal(dictEq);
          return x._1 === y._1 && (() => {
            const go = v => v1 => v2 => {
              if (!v2) { return false; }
              if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
              return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
            };
            return go(x._2)(y._2)(true);
          })();
        })();
      }
      if (x.tag === "Dictionary") { return y.tag === "Dictionary" && eqDictRep(dictEq).eq(x._1)(y._1); }
      if (x.tag === "Matrix") { return y.tag === "Matrix" && eqMatrixRep(dictEq).eq(x._1)(y._1); }
      return x.tag === "Fun" && y.tag === "Fun" && eqFun(dictEq).eq(x._1)(y._1);
    }
  }
);
const eqEnvExpr = dictEq => ({eq: x => y => Foreign$dObject.eqObject(eqVal(dictEq)).eq(x._1)(y._1) && Expr.eqExpr(dictEq).eq(x._2)(y._2)});
const ordVal = dictOrd => {
  const eqVal1 = eqVal(dictOrd.Eq0());
  return {
    compare: x => y => {
      const v = dictOrd.compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      const $0 = ordVal(dictOrd);
      const v1 = (() => {
        if (x._2.tag === "Nothing") {
          if (y._2.tag === "Nothing") { return Data$dOrdering.EQ; }
          return Data$dOrdering.LT;
        }
        if (y._2.tag === "Nothing") { return Data$dOrdering.GT; }
        if (x._2.tag === "Just" && y._2.tag === "Just") { return $0.compare(x._2._1)(y._2._1); }
        $runtime.fail();
      })();
      if (v1 === "LT") { return Data$dOrdering.LT; }
      if (v1 === "GT") { return Data$dOrdering.GT; }
      return ordBaseVal(dictOrd).compare(x._3)(y._3);
    },
    Eq0: () => eqVal1
  };
};
const ordMatrixRep = dictOrd => {
  const ordMatrixDim1 = ordMatrixDim(dictOrd);
  const eqMatrixRep1 = eqMatrixRep(dictOrd.Eq0());
  return {
    compare: x => y => {
      const v = Data$dOrd.ordArray(Data$dOrd.ordArray(ordVal(dictOrd))).compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      const v$1 = ordMatrixDim1.compare(x._2._1)(y._2._1);
      if (v$1 === "LT") { return Data$dOrdering.LT; }
      if (v$1 === "GT") { return Data$dOrdering.GT; }
      return ordMatrixDim1.compare(x._2._2)(y._2._2);
    },
    Eq0: () => eqMatrixRep1
  };
};
const ordFun = dictOrd => {
  const ordElim = Expr.ordElim(dictOrd);
  const eqFun1 = eqFun(dictOrd.Eq0());
  return {
    compare: x => y => {
      if (x.tag === "Closure") {
        if (y.tag === "Closure") {
          const v = ordEnv(dictOrd).compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          const v1 = Dict.ordDict(ordElim).compare(x._2)(y._2);
          if (v1 === "LT") { return Data$dOrdering.LT; }
          if (v1 === "GT") { return Data$dOrdering.GT; }
          return ordElim.compare(x._3)(y._3);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Closure") { return Data$dOrdering.GT; }
      if (x.tag === "Foreign") {
        if (y.tag === "Foreign") {
          const v = Data$dOrd.ordString.compare(x._1._1)(y._1._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return Data$dList$dTypes.ordList(ordVal(dictOrd)).compare(x._2)(y._2);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Foreign") { return Data$dOrdering.GT; }
      if (x.tag === "PartialConstr" && y.tag === "PartialConstr") {
        const v = Data$dOrd.ordString.compare(x._1)(y._1);
        if (v === "LT") { return Data$dOrdering.LT; }
        if (v === "GT") { return Data$dOrdering.GT; }
        return Data$dList$dTypes.ordList(ordVal(dictOrd)).compare(x._2)(y._2);
      }
      $runtime.fail();
    },
    Eq0: () => eqFun1
  };
};
const ordEnv = dictOrd => {
  const $0 = dictOrd.Eq0();
  const eqEnv1 = {eq: x => y => Foreign$dObject.eqObject(eqVal($0)).eq(x)(y)};
  return {compare: x => y => Dict.ordDict(ordVal(dictOrd)).compare(x)(y), Eq0: () => eqEnv1};
};
const ordDictRep = dictOrd => {
  const $0 = dictOrd.Eq0();
  const eqDictRep1 = eqDictRep(dictOrd.Eq0());
  return {
    compare: x => y => Dict.ordDict((() => {
      const $1 = ordVal(dictOrd);
      const $2 = $1.Eq0();
      const eqTuple2 = {eq: x$1 => y$1 => $0.eq(x$1._1)(y$1._1) && $2.eq(x$1._2)(y$1._2)};
      return {
        compare: x$1 => y$1 => {
          const v = dictOrd.compare(x$1._1)(y$1._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return $1.compare(x$1._2)(y$1._2);
        },
        Eq0: () => eqTuple2
      };
    })()).compare(x)(y),
    Eq0: () => eqDictRep1
  };
};
const ordBaseVal = dictOrd => {
  const eqBaseVal1 = eqBaseVal(dictOrd.Eq0());
  return {
    compare: x => y => {
      if (x.tag === "Int") {
        if (y.tag === "Int") { return Data$dOrd.ordInt.compare(x._1)(y._1); }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Int") { return Data$dOrdering.GT; }
      if (x.tag === "Float") {
        if (y.tag === "Float") { return Data$dOrd.ordNumber.compare(x._1)(y._1); }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Float") { return Data$dOrdering.GT; }
      if (x.tag === "Str") {
        if (y.tag === "Str") { return Data$dOrd.ordString.compare(x._1)(y._1); }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Str") { return Data$dOrdering.GT; }
      if (x.tag === "Constr") {
        if (y.tag === "Constr") {
          const v = Data$dOrd.ordString.compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return Data$dList$dTypes.ordList(ordVal(dictOrd)).compare(x._2)(y._2);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Constr") { return Data$dOrdering.GT; }
      if (x.tag === "Dictionary") {
        if (y.tag === "Dictionary") { return ordDictRep(dictOrd).compare(x._1)(y._1); }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Dictionary") { return Data$dOrdering.GT; }
      if (x.tag === "Matrix") {
        if (y.tag === "Matrix") { return ordMatrixRep(dictOrd).compare(x._1)(y._1); }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Matrix") { return Data$dOrdering.GT; }
      if (x.tag === "Fun" && y.tag === "Fun") { return ordFun(dictOrd).compare(x._1)(y._1); }
      $runtime.fail();
    },
    Eq0: () => eqBaseVal1
  };
};
const ordEnvExpr = dictOrd => {
  const eqEnvExpr1 = eqEnvExpr(dictOrd.Eq0());
  return {
    compare: x => y => {
      const v = ordEnv(dictOrd).compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      return Expr.ordExpr(dictOrd).compare(x._2)(y._2);
    },
    Eq0: () => eqEnvExpr1
  };
};
const applyMatrixDim = {
  apply: v => v1 => {
    const $0 = v._1;
    const $1 = v1._1;
    return Data$dTuple.$Tuple(Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0), v._2(v1._2));
  },
  Functor0: () => functorMatrixDim
};
const applyVal = {
  apply: v => v1 => {
    if (v._2.tag === "Nothing") {
      if (v1._2.tag === "Nothing") { return $Val(v._1(v1._1), Data$dMaybe.Nothing, applyBaseVal.apply(v._3)(v1._3)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v._2.tag === "Just" && v1._2.tag === "Just") { return $Val(v._1(v1._1), Data$dMaybe.$Maybe("Just", applyVal.apply(v._2._1)(v1._2._1)), applyBaseVal.apply(v._3)(v1._3)); }
    return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
  },
  Functor0: () => functorVal
};
const applyMatrixRep = {
  apply: v => v1 => Data$dTuple.$Tuple(
    Data$dArray.zipWithImpl(Data$dArray.zipWith(applyVal.apply), v._1, v1._1),
    Data$dTuple.$Tuple(applyMatrixDim.apply(v._2._1)(v1._2._1), applyMatrixDim.apply(v._2._2)(v1._2._2))
  ),
  Functor0: () => functorMatrixRep
};
const applyFun = {
  apply: v => v1 => {
    if (v.tag === "Closure") {
      if (v1.tag === "Closure") {
        return $Fun(
          "Closure",
          applyEnv.apply(v._1)(v1._1),
          Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(v._2, Expr.applyElim.apply))(v1._2),
          Expr.applyElim.apply(v._3)(v1._3)
        );
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Foreign") {
      if (v1.tag === "Foreign") {
        return $Fun(
          "Foreign",
          v._1,
          (() => {
            const go = go$a0$copy => go$a1$copy => go$a2$copy => {
              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
              while (go$c) {
                const v$1 = go$a0, v1$1 = go$a1, v2 = go$a2;
                if (v$1.tag === "Nil") {
                  go$c = false;
                  go$r = v2;
                  continue;
                }
                if (v1$1.tag === "Nil") {
                  go$c = false;
                  go$r = v2;
                  continue;
                }
                if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                  go$a0 = v$1._2;
                  go$a1 = v1$1._2;
                  go$a2 = Data$dList$dTypes.$List("Cons", applyVal.apply(v$1._1)(v1$1._1), v2);
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
            return go$1(Data$dList$dTypes.Nil)(go(v._2)(v1._2)(Data$dList$dTypes.Nil));
          })()
        );
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "PartialConstr" && v1.tag === "PartialConstr") {
      const $0 = v._1;
      const $1 = v1._1;
      return $Fun(
        "PartialConstr",
        Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0),
        (() => {
          const go = go$a0$copy => go$a1$copy => go$a2$copy => {
            let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
            while (go$c) {
              const v$1 = go$a0, v1$1 = go$a1, v2 = go$a2;
              if (v$1.tag === "Nil") {
                go$c = false;
                go$r = v2;
                continue;
              }
              if (v1$1.tag === "Nil") {
                go$c = false;
                go$r = v2;
                continue;
              }
              if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                go$a0 = v$1._2;
                go$a1 = v1$1._2;
                go$a2 = Data$dList$dTypes.$List("Cons", applyVal.apply(v$1._1)(v1$1._1), v2);
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
          return go$1(Data$dList$dTypes.Nil)(go(v._2)(v1._2)(Data$dList$dTypes.Nil));
        })()
      );
    }
    return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
  },
  Functor0: () => functorFun
};
const applyEnv = {apply: v => v1 => Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(v, applyVal.apply))(v1), Functor0: () => functorEnv};
const applyDictRep = {
  apply: v => v1 => Util$dMap.intersectionWith_Object(v2 => {
    const $0 = v2._2;
    return v3 => Data$dTuple.$Tuple(v2._1(v3._1), applyVal.apply($0)(v3._2));
  })(v)(v1),
  Functor0: () => functorDictRep
};
const applyBaseVal = {
  apply: v => v1 => {
    if (v.tag === "Int") {
      if (v1.tag === "Int") {
        const $0 = v._1;
        const $1 = v1._1;
        return $BaseVal("Int", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Float") {
      if (v1.tag === "Float") {
        const $0 = v._1;
        const $1 = v1._1;
        return $BaseVal("Float", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Str") {
      if (v1.tag === "Str") {
        const $0 = v._1;
        const $1 = v1._1;
        return $BaseVal("Str", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Constr") {
      if (v1.tag === "Constr") {
        const $0 = v._1;
        const $1 = v1._1;
        return $BaseVal(
          "Constr",
          Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0),
          (() => {
            const go = go$a0$copy => go$a1$copy => go$a2$copy => {
              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
              while (go$c) {
                const v$1 = go$a0, v1$1 = go$a1, v2 = go$a2;
                if (v$1.tag === "Nil") {
                  go$c = false;
                  go$r = v2;
                  continue;
                }
                if (v1$1.tag === "Nil") {
                  go$c = false;
                  go$r = v2;
                  continue;
                }
                if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                  go$a0 = v$1._2;
                  go$a1 = v1$1._2;
                  go$a2 = Data$dList$dTypes.$List("Cons", applyVal.apply(v$1._1)(v1$1._1), v2);
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
            return go$1(Data$dList$dTypes.Nil)(go(v._2)(v1._2)(Data$dList$dTypes.Nil));
          })()
        );
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Dictionary") {
      if (v1.tag === "Dictionary") { return $BaseVal("Dictionary", applyDictRep.apply(v._1)(v1._1)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Matrix") {
      if (v1.tag === "Matrix") { return $BaseVal("Matrix", applyMatrixRep.apply(v._1)(v1._1)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Fun" && v1.tag === "Fun") { return $BaseVal("Fun", applyFun.apply(v._1)(v1._1)); }
    return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
  },
  Functor0: () => functorBaseVal
};
const applyEnvExpr = {
  apply: v => v1 => $EnvExpr(Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(v._1, applyVal.apply))(v1._1), Expr.applyExpr.apply(v._2)(v1._2)),
  Functor0: () => functorEnvExpr
};
const meetSemilatticeEnv = dictMeetSemilattice => (
  {
    meet: (() => {
      const $0 = dictMeetSemilattice.meet;
      return a => b => Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(Foreign$dObject._fmapObject(a, functorVal.map($0)), applyVal.apply))(b);
    })()
  }
);
const meetSemilatticeVal = dictMeetSemilattice => (
  {
    meet: (() => {
      const $0 = dictMeetSemilattice.meet;
      return a => b => applyVal.apply(functorVal.map($0)(a))(b);
    })()
  }
);
const joinSemilatticeVal = dictJoinSemilattice => (
  {
    join: v => v1 => $Val(
      dictJoinSemilattice.join(v._1)(v1._1),
      Lattice.joinSemilatticeMaybe(joinSemilatticeVal(dictJoinSemilattice)).join(v._2)(v1._2),
      joinSemilatticeBaseVal(dictJoinSemilattice).join(v._3)(v1._3)
    )
  }
);
const joinSemilatticeMatrixRep = dictJoinSemilattice => {
  const $0 = joinSemilatticeMatrixDim(dictJoinSemilattice);
  return {
    join: v => v1 => Data$dTuple.$Tuple(
      Lattice.joinSemilatticeArray(Lattice.joinSemilatticeArray(joinSemilatticeVal(dictJoinSemilattice))).join(v._1)(v1._1),
      Data$dTuple.$Tuple($0.join(v._2._1)(v1._2._1), $0.join(v._2._2)(v1._2._2))
    )
  };
};
const joinSemilatticeFun = dictJoinSemilattice => {
  const joinSemilatticeElim = Expr.joinSemilatticeElim(dictJoinSemilattice);
  return {
    join: v => v1 => {
      if (v.tag === "Closure") {
        if (v1.tag === "Closure") {
          return $Fun(
            "Closure",
            joinSemilatticeEnv(dictJoinSemilattice).join(v._1)(v1._1),
            Foreign$dObject.unionWith(joinSemilatticeElim.join)(v._2)(v1._2),
            joinSemilatticeElim.join(v._3)(v1._3)
          );
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Foreign") {
        if (v1.tag === "Foreign") { return $Fun("Foreign", v._1, Lattice.joinSemilatticeList(joinSemilatticeVal(dictJoinSemilattice)).join(v._2)(v1._2)); }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "PartialConstr" && v1.tag === "PartialConstr") {
        const $0 = v._1;
        const $1 = v1._1;
        return $Fun("PartialConstr", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0), Lattice.joinSemilatticeList(joinSemilatticeVal(dictJoinSemilattice)).join(v._2)(v1._2));
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  };
};
const joinSemilatticeEnv = dictJoinSemilattice => ({join: v => v1 => Foreign$dObject.unionWith(joinSemilatticeVal(dictJoinSemilattice).join)(v)(v1)});
const joinSemilatticeDictRep = dictJoinSemilattice => (
  {
    join: v => v1 => {
      const $0 = joinSemilatticeVal(dictJoinSemilattice);
      return Foreign$dObject.unionWith(v$1 => v1$1 => Data$dTuple.$Tuple(dictJoinSemilattice.join(v$1._1)(v1$1._1), $0.join(v$1._2)(v1$1._2)))(v)(v1);
    }
  }
);
const joinSemilatticeBaseVal = dictJoinSemilattice => {
  const join = dictJoinSemilattice.join;
  return {
    join: v => v1 => {
      if (v.tag === "Int") {
        if (v1.tag === "Int") {
          const $0 = v._1;
          const $1 = v1._1;
          return $BaseVal("Int", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
      }
      if (v.tag === "Float") {
        if (v1.tag === "Float") {
          const $0 = v._1;
          const $1 = v1._1;
          return $BaseVal("Float", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
      }
      if (v.tag === "Str") {
        if (v1.tag === "Str") {
          const $0 = v._1;
          const $1 = v1._1;
          return $BaseVal("Str", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
      }
      if (v.tag === "Dictionary") {
        if (v1.tag === "Dictionary") { return $BaseVal("Dictionary", joinSemilatticeDictRep(dictJoinSemilattice).join(v._1)(v1._1)); }
        return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
      }
      if (v.tag === "Constr") {
        if (v1.tag === "Constr") {
          const $0 = v._1;
          const $1 = v1._1;
          return $BaseVal("Constr", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0), Lattice.joinSemilatticeList(joinSemilatticeVal(dictJoinSemilattice)).join(v._2)(v1._2));
        }
        return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
      }
      if (v.tag === "Matrix") {
        if (v1.tag === "Matrix") { return $BaseVal("Matrix", joinSemilatticeMatrixRep(dictJoinSemilattice).join(v._1)(v1._1)); }
        return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
      }
      if (v.tag === "Fun" && v1.tag === "Fun") { return $BaseVal("Fun", joinSemilatticeFun(dictJoinSemilattice).join(v._1)(v1._1)); }
      return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
    }
  };
};
const annUnit = {Highlightable0: () => highlightableUnit, BoundedLattice1: () => boundedLattice};
const annBoolean = {Highlightable0: () => highlightableBoolean, BoundedLattice1: () => boundedLattice1};
const val = dictMonadWithGraphAlloc => {
  const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
  return doc_opt => $$new(a => Val(a)(doc_opt));
};
const unrestrictGC = dictBoundedMeetSemilattice => γ => xs => {
  const unfound = setSet1.difference(xs)(Util$dMap.mapObjectString.keys(γ));
  return Util.assertWith("Variable(s) " + Data$dString$dCommon.joinWith(", ")(Data$dFunctor.arrayMap(Data$dShow.showStringImpl)(Data$dArray.fromFoldableImpl(
    Data$dSet.foldableSet.foldr,
    unfound
  ))) + " are in environment ")(unfound.tag === "Leaf")({
    fwd: γ$p => Util.assertWith("")(Data$dMap$dInternal.unsafeDifference(Data$dOrd.ordString.compare, Util$dMap.mapObjectString.keys(γ$p), Util$dMap.mapObjectString.keys(γ)).tag === "Leaf")(Foreign$dObject.union(γ$p)(Util$dSet.setObjectString.difference((() => {
      const $0 = dictBoundedMeetSemilattice.top;
      return Foreign$dObject._fmapObject(γ, functorVal.map(v => $0));
    })())(γ$p))),
    bwd: γ$p => Util.assertWith("")(Data$dMap$dInternal.eqMap(Data$dEq.eqString)(Data$dEq.eqUnit).eq(Util$dMap.mapObjectString.keys(γ$p))(Util$dMap.mapObjectString.keys(γ)))(Foreign$dObject.filterWithKey(x => {
      const $0 = Util$dSet.setSet(Data$dOrd.ordString).member(x)(xs);
      return v => $0;
    })(γ$p))
  });
};
const reaches = ρ => xs => {
  const dom_ρ = Util$dMap.mapObjectString.keys(ρ);
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = v1;
        continue;
      }
      if (v.tag === "Cons") {
        if (setSet1.member(v._1)(v1)) {
          go$a0 = v._2;
          go$a1 = v1;
          continue;
        }
        go$a0 = Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(v._2)(toUnfoldable1(Data$dMap$dInternal.unsafeIntersectionWith(
          Data$dOrd.ordString.compare,
          Data$dFunction.const,
          Expr.fVElim.fv(Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)(v._1)(ρ)),
          dom_ρ
        )));
        go$a1 = setSet1.union(Data$dMap$dInternal.$$$Map("Node", 1, 1, v._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(v1);
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(toUnfoldable1(xs))(setSet1.empty);
};
const matrixPut = i => j => δv => v => {
  const vs_i = Util.unsafeArrayArray.unsafeIndex(v._1)(i);
  return Data$dTuple.$Tuple(
    Util.definitely("index within bounds")(Data$dArray._updateAt(
      Data$dMaybe.Just,
      Data$dMaybe.Nothing,
      i,
      Util.definitely("index within bounds")(Data$dArray._updateAt(Data$dMaybe.Just, Data$dMaybe.Nothing, j, δv(Util.unsafeArrayArray.unsafeIndex(vs_i)(j)), vs_i)),
      v._1
    )),
    Data$dTuple.$Tuple(v._2._1, v._2._2)
  );
};
const matrixGet = i => j => v => Util.definitely("index out of bounds!")((() => {
  if (i >= 0 && i < v._1.length) {
    const $0 = v._1[i];
    if (j >= 0 && j < $0.length) { return Data$dMaybe.$Maybe("Just", $0[j]); }
  }
  return Data$dMaybe.Nothing;
})());
const highlightIf = dict => dict.highlightIf;
const highlightable$x215 = dictHighlightable => ({highlightIf: v => doc => dictHighlightable.highlightIf(v._1)(doc)});
const ann$x215 = dictAnn => {
  const $0 = dictAnn.Highlightable0();
  const highlightable$x2151 = {highlightIf: v => doc => $0.highlightIf(v._1)(doc)};
  const BoundedLattice1 = dictAnn.BoundedLattice1();
  const $1 = BoundedLattice1.BoundedJoinSemilattice0();
  const bot1 = $1.bot;
  const $2 = $1.JoinSemilattice0();
  const $3 = BoundedLattice1.BoundedMeetSemilattice1();
  const top1 = $3.top;
  const $4 = $3.MeetSemilattice0();
  return dictBoundedLattice => {
    const $5 = dictBoundedLattice.BoundedJoinSemilattice0();
    const $6 = $5.JoinSemilattice0();
    const $7 = (() => {
      const joinSemilattice$x2152 = {join: v => v1 => Data$dTuple.$Tuple($2.join(v._1)(v1._1), $6.join(v._2)(v1._2))};
      return {bot: Data$dTuple.$Tuple(bot1, $5.bot), JoinSemilattice0: () => joinSemilattice$x2152};
    })();
    const $8 = dictBoundedLattice.BoundedMeetSemilattice1();
    const $9 = $8.MeetSemilattice0();
    const $10 = (() => {
      const meetSemilattice$x2152 = {meet: v => v1 => Data$dTuple.$Tuple($4.meet(v._1)(v1._1), $9.meet(v._2)(v1._2))};
      return {top: Data$dTuple.$Tuple(top1, $8.top), MeetSemilattice0: () => meetSemilattice$x2152};
    })();
    return {Highlightable0: () => highlightable$x2151, BoundedLattice1: () => ({BoundedJoinSemilattice0: () => $7, BoundedMeetSemilattice1: () => $10})};
  };
};
const forDefs = ρ => σ => {
  const $0 = reaches(ρ)(Data$dMap$dInternal.unsafeIntersectionWith(
    Data$dOrd.ordString.compare,
    Data$dFunction.const,
    Expr.fVElim.fv(σ),
    fromFoldable1(Util$dMap.mapObjectString.keys(ρ))
  ));
  return Foreign$dObject.filterWithKey(x => {
    const $1 = Util$dSet.setSet(Data$dOrd.ordString).member(x)($0);
    return v => $1;
  })(ρ);
};
const asVal = e => {
  if (e(dictTypeName => dictTypeName.typeName) === "Val") { return Data$dMaybe.$Maybe("Just", e(dictTypeName => Unsafe$dCoerce.unsafeCoerce)); }
  return Data$dMaybe.Nothing;
};
export {
  $BaseVal,
  $EnvExpr,
  $ForeignOp$p,
  $Fun,
  $Val,
  
  
  
  DictRep,
  Dictionary,
  Env,
  
  Float,
  
  
  
  
  Int,
  Matrix,
  
  
  
  Str,
  Val,
  
  
  
  
  
  applyEnv,
  applyEnvExpr,
  
  
  
  applyVal,
  asVal,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  foldableEnv,
  foldableEnvExpr,
  
  
  
  foldableVal,
  forDefs,
  
  functorBaseVal,
  
  
  functorEnvExpr,
  
  
  
  functorVal,
  
  
  highlightableBoolean,
  highlightableUnit,
  highlightableVertex,
  identity,
  
  
  
  
  
  
  
  
  mapEnvStringVal,
  matrixGet,
  matrixPut,
  
  
  
  
  
  
  
  
  
  
  
  
  ordVal,
  
  
  
  
  
  
  
  
  
  
  traversableEnv,
  
  
  
  
  
  
  
  typeNameVal,
  
  unions1,
  unrestrictGC,
  val,
  
  
  
  
  verticesEnvExprVertex,
  
  
  
  
  verticesValVertex
};
