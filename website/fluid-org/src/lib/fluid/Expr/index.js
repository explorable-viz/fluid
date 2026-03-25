import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Dict from "../Dict/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Graph from "../Graph/index.js";
import * as Lattice from "../Lattice/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Util$dPair from "../Util.Pair/index.js";
import * as Util$dSet from "../Util.Set/index.js";
const $Cont = (tag, _1) => ({tag, _1});
const $Elim = (tag, _1, _2) => ({tag, _1, _2});
const $Expr = (tag, _1, _2, _3, _4) => ({tag, _1, _2, _3, _4});
const $RecDefs = (_1, _2) => ({tag: "RecDefs", _1, _2});
const $VarDef = (_1, _2) => ({tag: "VarDef", _1, _2});
const union = /* #__PURE__ */ (() => Util$dSet.setSet(Graph.ordDVertex$p).union)();
const unions1 = /* #__PURE__ */ Data$dSet.unions(Data$dList$dTypes.foldableList)(Graph.ordDVertex$p);
const identity = x => x;
const compare3 = x => y => {
  const v = Data$dOrd.ordString.compare(x._1)(y._1);
  if (v === "LT") { return Data$dOrdering.LT; }
  if (v === "GT") { return Data$dOrdering.GT; }
  return Data$dOrd.ordString.compare(x._2)(y._2);
};
const compare4 = /* #__PURE__ */ (() => Data$dSet.ordSet(Data$dOrd.ordString).compare)();
const setSet = /* #__PURE__ */ Util$dSet.setSet(Data$dOrd.ordString);
const unions2 = /* #__PURE__ */ Data$dSet.unions(Dict.foldableDict)(Data$dOrd.ordString);
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
const unions3 = /* #__PURE__ */ Data$dSet.unions(Data$dList$dTypes.foldableList)(Data$dOrd.ordString);
const asMaplet = /* #__PURE__ */ Util$dMap.asMaplet(Dict.mapDictString);
const ContExpr = value0 => $Cont("ContExpr", value0);
const ContElim = value0 => $Cont("ContElim", value0);
const Var = value0 => $Expr("Var", value0);
const Op = value0 => $Expr("Op", value0);
const Int = value0 => value1 => $Expr("Int", value0, value1);
const Float = value0 => value1 => $Expr("Float", value0, value1);
const Str = value0 => value1 => $Expr("Str", value0, value1);
const Dictionary = value0 => value1 => $Expr("Dictionary", value0, value1);
const Constr = value0 => value1 => value2 => $Expr("Constr", value0, value1, value2);
const Matrix = value0 => value1 => value2 => value3 => $Expr("Matrix", value0, value1, value2, value3);
const Lambda = value0 => value1 => $Expr("Lambda", value0, value1);
const DProject = value0 => value1 => $Expr("DProject", value0, value1);
const App = value0 => value1 => $Expr("App", value0, value1);
const Let = value0 => value1 => $Expr("Let", value0, value1);
const LetRec = value0 => value1 => $Expr("LetRec", value0, value1);
const DocExpr = value0 => value1 => $Expr("DocExpr", value0, value1);
const ElimVar = value0 => value1 => $Elim("ElimVar", value0, value1);
const ElimConstr = value0 => $Elim("ElimConstr", value0);
const ElimDict = value0 => value1 => $Elim("ElimDict", value0, value1);
const VarDef = value0 => value1 => $VarDef(value0, value1);
const RecDefs = value0 => value1 => $RecDefs(value0, value1);
const Module = x => x;
const typeNameRecDefs = {typeName: v => "RecDefs"};
const pack = x => k => k(typeNameRecDefs)(x);
const typeNameExpr = {typeName: v => "Expr"};
const pack1 = x => k => k(typeNameExpr)(x);
const verticesVarDefVertex = {vertices: v => union(verticesElimVertex.vertices(v._1))(verticesExprVertex.vertices(v._2))};
const verticesRecDefsVertex = {
  vertices: v => union(Data$dMap$dInternal.$$$Map("Node", 1, 1, Data$dTuple.$Tuple(v._1, pack(v)), undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(Graph.verticesDict(verticesElimVertex).vertices(v._2))
};
const verticesExprVertex = {
  vertices: v => {
    if (v.tag === "Var") { return Data$dMap$dInternal.Leaf; }
    if (v.tag === "Op") { return Data$dMap$dInternal.Leaf; }
    if (v.tag === "Int") { return Data$dMap$dInternal.$$$Map("Node", 1, 1, Data$dTuple.$Tuple(v._1, pack1(v)), undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf); }
    if (v.tag === "Float") { return Data$dMap$dInternal.$$$Map("Node", 1, 1, Data$dTuple.$Tuple(v._1, pack1(v)), undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf); }
    if (v.tag === "Str") { return Data$dMap$dInternal.$$$Map("Node", 1, 1, Data$dTuple.$Tuple(v._1, pack1(v)), undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf); }
    if (v.tag === "Dictionary") {
      return union(Data$dMap$dInternal.$$$Map("Node", 1, 1, Data$dTuple.$Tuple(v._1, pack1(v)), undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(unions1(Data$dList$dTypes.listMap(v1 => union(verticesExprVertex.vertices(v1._1))(verticesExprVertex.vertices(v1._2)))(v._2)));
    }
    if (v.tag === "Constr") {
      return union(Data$dMap$dInternal.$$$Map("Node", 1, 1, Data$dTuple.$Tuple(v._1, pack1(v)), undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(unions1(Data$dList$dTypes.listMap(verticesExprVertex.vertices)(v._3)));
    }
    if (v.tag === "Matrix") {
      return union(Data$dMap$dInternal.$$$Map("Node", 1, 1, Data$dTuple.$Tuple(v._1, pack1(v)), undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(union(verticesExprVertex.vertices(v._2))(verticesExprVertex.vertices(v._4)));
    }
    if (v.tag === "Lambda") {
      return union(Data$dMap$dInternal.$$$Map("Node", 1, 1, Data$dTuple.$Tuple(v._1, pack1(v)), undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(verticesElimVertex.vertices(v._2));
    }
    if (v.tag === "DProject") { return union(verticesExprVertex.vertices(v._1))(verticesExprVertex.vertices(v._2)); }
    if (v.tag === "App") { return union(verticesExprVertex.vertices(v._1))(verticesExprVertex.vertices(v._2)); }
    if (v.tag === "Let") { return union(verticesVarDefVertex.vertices(v._1))(verticesExprVertex.vertices(v._2)); }
    if (v.tag === "LetRec") { return union(verticesRecDefsVertex.vertices(v._1))(verticesExprVertex.vertices(v._2)); }
    if (v.tag === "DocExpr") { return union(verticesExprVertex.vertices(v._1))(verticesExprVertex.vertices(v._2)); }
    $runtime.fail();
  }
};
const verticesElimVertex = {
  vertices: v => {
    if (v.tag === "ElimVar") { return verticesContVertex.vertices(v._2); }
    if (v.tag === "ElimConstr") { return Graph.verticesDict(verticesContVertex).vertices(v._1); }
    if (v.tag === "ElimDict") { return verticesContVertex.vertices(v._2); }
    $runtime.fail();
  }
};
const verticesContVertex = {
  vertices: v => {
    if (v.tag === "ContExpr") { return verticesExprVertex.vertices(v._1); }
    if (v.tag === "ContElim") { return verticesElimVertex.vertices(v._1); }
    $runtime.fail();
  }
};
const verticesModuleVertex = {
  vertices: v => unions1(Data$dList$dTypes.listMap(v1 => {
    if (v1.tag === "Left") { return verticesVarDefVertex.vertices(v1._1); }
    if (v1.tag === "Right") { return verticesRecDefsVertex.vertices(v1._1); }
    $runtime.fail();
  })(v))
};
const newtypeModule_ = {Coercible0: () => {}};
const joinSemilatticeRecDefs = dictJoinSemilattice => (
  {join: v => v1 => $RecDefs(dictJoinSemilattice.join(v._1)(v1._1), Foreign$dObject.unionWith(joinSemilatticeElim(dictJoinSemilattice).join)(v._2)(v1._2))}
);
const joinSemilatticeExpr = dictJoinSemilattice => (
  {
    join: v => v1 => {
      if (v.tag === "Var") {
        if (v1.tag === "Var") {
          const $0 = v._1;
          const $1 = v1._1;
          return $Expr("Var", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Op") {
        if (v1.tag === "Op") {
          const $0 = v._1;
          const $1 = v1._1;
          return $Expr("Op", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Int") {
        if (v1.tag === "Int") {
          const $0 = v._2;
          const $1 = v1._2;
          return $Expr("Int", dictJoinSemilattice.join(v._1)(v1._1), Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Str") {
        if (v1.tag === "Str") {
          const $0 = v._2;
          const $1 = v1._2;
          return $Expr("Str", dictJoinSemilattice.join(v._1)(v1._1), Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Float") {
        if (v1.tag === "Float") {
          const $0 = v._2;
          const $1 = v1._2;
          return $Expr("Float", dictJoinSemilattice.join(v._1)(v1._1), Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Dictionary") {
        if (v1.tag === "Dictionary") {
          return $Expr(
            "Dictionary",
            dictJoinSemilattice.join(v._1)(v1._1),
            Lattice.joinSemilatticeList((() => {
              const $0 = joinSemilatticeExpr(dictJoinSemilattice);
              return {join: v$1 => v1$1 => Util$dPair.$Pair($0.join(v$1._1)(v1$1._1), $0.join(v$1._2)(v1$1._2))};
            })()).join(v._2)(v1._2)
          );
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Constr") {
        if (v1.tag === "Constr") {
          const $0 = v._2;
          const $1 = v1._2;
          return $Expr(
            "Constr",
            dictJoinSemilattice.join(v._1)(v1._1),
            Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0),
            Lattice.joinSemilatticeList(joinSemilatticeExpr(dictJoinSemilattice)).join(v._3)(v1._3)
          );
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Matrix") {
        if (v1.tag === "Matrix") {
          const $0 = v._3._1;
          const $1 = v1._3._1;
          const $2 = v._3._2;
          const $3 = v1._3._2;
          return $Expr(
            "Matrix",
            dictJoinSemilattice.join(v._1)(v1._1),
            joinSemilatticeExpr(dictJoinSemilattice).join(v._2)(v1._2),
            Data$dTuple.$Tuple(Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0), Util.assertWhen(false)("mustEq")(v$1 => $2 === $3)($2)),
            joinSemilatticeExpr(dictJoinSemilattice).join(v._4)(v1._4)
          );
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Lambda") {
        if (v1.tag === "Lambda") { return $Expr("Lambda", dictJoinSemilattice.join(v._1)(v1._1), joinSemilatticeElim(dictJoinSemilattice).join(v._2)(v1._2)); }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "DProject") {
        if (v1.tag === "DProject") {
          return $Expr("DProject", joinSemilatticeExpr(dictJoinSemilattice).join(v._1)(v1._1), joinSemilatticeExpr(dictJoinSemilattice).join(v._2)(v1._2));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "App") {
        if (v1.tag === "App") { return $Expr("App", joinSemilatticeExpr(dictJoinSemilattice).join(v._1)(v1._1), joinSemilatticeExpr(dictJoinSemilattice).join(v._2)(v1._2)); }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "LetRec") {
        if (v1.tag === "LetRec") {
          return $Expr("LetRec", joinSemilatticeRecDefs(dictJoinSemilattice).join(v._1)(v1._1), joinSemilatticeExpr(dictJoinSemilattice).join(v._2)(v1._2));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "DocExpr" && v1.tag === "DocExpr") {
        return $Expr("DocExpr", joinSemilatticeExpr(dictJoinSemilattice).join(v._1)(v1._1), joinSemilatticeExpr(dictJoinSemilattice).join(v._2)(v1._2));
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  }
);
const joinSemilatticeElim = dictJoinSemilattice => (
  {
    join: v => v1 => {
      if (v.tag === "ElimVar") {
        if (v1.tag === "ElimVar") {
          const $0 = v._1;
          const $1 = v1._1;
          return $Elim("ElimVar", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0), joinSemilatticeCont(dictJoinSemilattice).join(v._2)(v1._2));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "ElimConstr") {
        if (v1.tag === "ElimConstr") { return $Elim("ElimConstr", Foreign$dObject.unionWith(joinSemilatticeCont(dictJoinSemilattice).join)(v._1)(v1._1)); }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "ElimDict" && v1.tag === "ElimDict") {
        const $0 = v._1;
        const $1 = v1._1;
        return $Elim(
          "ElimDict",
          Util.assertWhen(false)("mustEq")(v$1 => Data$dMap$dInternal.eqMap(Data$dEq.eqString)(Data$dEq.eqUnit).eq($0)($1))($0),
          joinSemilatticeCont(dictJoinSemilattice).join(v._2)(v1._2)
        );
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  }
);
const joinSemilatticeCont = dictJoinSemilattice => (
  {
    join: v => v1 => {
      if (v.tag === "ContExpr") {
        if (v1.tag === "ContExpr") { return $Cont("ContExpr", joinSemilatticeExpr(dictJoinSemilattice).join(v._1)(v1._1)); }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "ContElim" && v1.tag === "ContElim") { return $Cont("ContElim", joinSemilatticeElim(dictJoinSemilattice).join(v._1)(v1._1)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  }
);
const joinSemilatticeVarDef = dictJoinSemilattice => (
  {join: v => v1 => $VarDef(joinSemilatticeElim(dictJoinSemilattice).join(v._1)(v1._1), joinSemilatticeExpr(dictJoinSemilattice).join(v._2)(v1._2))}
);
const functorVarDef = {map: f => m => $VarDef(functorElim.map(f)(m._1), functorExpr.map(f)(m._2))};
const functorRecDefs = {map: f => m => $RecDefs(f(m._1), Foreign$dObject._fmapObject(m._2, functorElim.map(f)))};
const functorExpr = {
  map: f => m => {
    if (m.tag === "Var") { return $Expr("Var", m._1); }
    if (m.tag === "Op") { return $Expr("Op", m._1); }
    if (m.tag === "Int") { return $Expr("Int", f(m._1), m._2); }
    if (m.tag === "Float") { return $Expr("Float", f(m._1), m._2); }
    if (m.tag === "Str") { return $Expr("Str", f(m._1), m._2); }
    if (m.tag === "Dictionary") {
      return $Expr(
        "Dictionary",
        f(m._1),
        Data$dList$dTypes.listMap((() => {
          const $0 = functorExpr.map(f);
          return v => Util$dPair.$Pair($0(v._1), $0(v._2));
        })())(m._2)
      );
    }
    if (m.tag === "Constr") { return $Expr("Constr", f(m._1), m._2, Data$dList$dTypes.listMap(functorExpr.map(f))(m._3)); }
    if (m.tag === "Matrix") { return $Expr("Matrix", f(m._1), functorExpr.map(f)(m._2), m._3, functorExpr.map(f)(m._4)); }
    if (m.tag === "Lambda") { return $Expr("Lambda", f(m._1), functorElim.map(f)(m._2)); }
    if (m.tag === "DProject") { return $Expr("DProject", functorExpr.map(f)(m._1), functorExpr.map(f)(m._2)); }
    if (m.tag === "App") { return $Expr("App", functorExpr.map(f)(m._1), functorExpr.map(f)(m._2)); }
    if (m.tag === "Let") { return $Expr("Let", functorVarDef.map(f)(m._1), functorExpr.map(f)(m._2)); }
    if (m.tag === "LetRec") { return $Expr("LetRec", functorRecDefs.map(f)(m._1), functorExpr.map(f)(m._2)); }
    if (m.tag === "DocExpr") { return $Expr("DocExpr", functorExpr.map(f)(m._1), functorExpr.map(f)(m._2)); }
    $runtime.fail();
  }
};
const functorElim = {
  map: f => m => {
    if (m.tag === "ElimVar") { return $Elim("ElimVar", m._1, functorCont.map(f)(m._2)); }
    if (m.tag === "ElimConstr") { return $Elim("ElimConstr", Foreign$dObject._fmapObject(m._1, functorCont.map(f))); }
    if (m.tag === "ElimDict") { return $Elim("ElimDict", m._1, functorCont.map(f)(m._2)); }
    $runtime.fail();
  }
};
const functorCont = {
  map: f => m => {
    if (m.tag === "ContExpr") { return $Cont("ContExpr", functorExpr.map(f)(m._1)); }
    if (m.tag === "ContElim") { return $Cont("ContElim", functorElim.map(f)(m._1)); }
    $runtime.fail();
  }
};
const functorModule = {
  map: f => m => Data$dList$dTypes.listMap(v2 => {
    if (v2.tag === "Left") { return Data$dEither.$Either("Left", functorVarDef.map(f)(v2._1)); }
    if (v2.tag === "Right") { return Data$dEither.$Either("Right", functorRecDefs.map(f)(v2._1)); }
    $runtime.fail();
  })(m)
};
const foldableVarDef = {
  foldl: f => z => m => foldableExpr.foldl(f)(foldableElim.foldl(f)(z)(m._1))(m._2),
  foldr: f => z => m => foldableElim.foldr(f)(foldableExpr.foldr(f)(z)(m._2))(m._1),
  foldMap: dictMonoid => f => m => dictMonoid.Semigroup0().append(foldableElim.foldMap(dictMonoid)(f)(m._1))(foldableExpr.foldMap(dictMonoid)(f)(m._2))
};
const foldableRecDefs = {
  foldl: f => z => m => {
    const $0 = foldableElim.foldl(f);
    return Foreign$dObject.fold(z$1 => v => $0(z$1))(f(z)(m._1))(m._2);
  },
  foldr: f => z => m => f(m._1)((() => {
    const $0 = foldableElim.foldr(f);
    return Data$dFoldable.foldrArray(b => a => $0(a)(b))(z)(Foreign$dObject.values(m._2));
  })()),
  foldMap: dictMonoid => {
    const foldMap1 = Foreign$dObject.foldMap(dictMonoid);
    return f => m => dictMonoid.Semigroup0().append(f(m._1))((() => {
      const $0 = foldableElim.foldMap(dictMonoid)(f);
      return foldMap1(v => $0)(m._2);
    })());
  }
};
const foldableExpr = {
  foldl: f => z => m => {
    if (m.tag === "Var") { return z; }
    if (m.tag === "Op") { return z; }
    if (m.tag === "Int") { return f(z)(m._1); }
    if (m.tag === "Float") { return f(z)(m._1); }
    if (m.tag === "Str") { return f(z)(m._1); }
    if (m.tag === "Dictionary") {
      const $0 = foldableExpr.foldl(f);
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
            go$a0 = $0($0(b)(v._1._1))(v._1._2);
            go$a1 = v._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      return go(f(z)(m._1))(m._2);
    }
    if (m.tag === "Constr") {
      const $0 = foldableExpr.foldl(f);
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
            go$a0 = $0(b)(v._1);
            go$a1 = v._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      return go(f(z)(m._1))(m._3);
    }
    if (m.tag === "Matrix") { return foldableExpr.foldl(f)(foldableExpr.foldl(f)(f(z)(m._1))(m._2))(m._4); }
    if (m.tag === "Lambda") { return foldableElim.foldl(f)(f(z)(m._1))(m._2); }
    if (m.tag === "DProject") { return foldableExpr.foldl(f)(foldableExpr.foldl(f)(z)(m._1))(m._2); }
    if (m.tag === "App") { return foldableExpr.foldl(f)(foldableExpr.foldl(f)(z)(m._1))(m._2); }
    if (m.tag === "Let") { return foldableExpr.foldl(f)(foldableVarDef.foldl(f)(z)(m._1))(m._2); }
    if (m.tag === "LetRec") { return foldableExpr.foldl(f)(foldableRecDefs.foldl(f)(z)(m._1))(m._2); }
    if (m.tag === "DocExpr") { return foldableExpr.foldl(f)(foldableExpr.foldl(f)(z)(m._1))(m._2); }
    $runtime.fail();
  },
  foldr: f => z => m => {
    if (m.tag === "Var") { return z; }
    if (m.tag === "Op") { return z; }
    if (m.tag === "Int") { return f(m._1)(z); }
    if (m.tag === "Float") { return f(m._1)(z); }
    if (m.tag === "Str") { return f(m._1)(z); }
    if (m.tag === "Dictionary") {
      return f(m._1)(Data$dList$dTypes.foldableList.foldr((() => {
        const $0 = foldableExpr.foldr(f);
        const $1 = Data$dFoldable.foldrDefault(Util$dPair.foldablePair)(b => a => $0(a)(b));
        return b => a => $1(a)(b);
      })())(z)(m._2));
    }
    if (m.tag === "Constr") {
      return f(m._1)(Data$dList$dTypes.foldableList.foldr((() => {
        const $0 = foldableExpr.foldr(f);
        return b => a => $0(a)(b);
      })())(z)(m._3));
    }
    if (m.tag === "Matrix") { return f(m._1)(foldableExpr.foldr(f)(foldableExpr.foldr(f)(z)(m._4))(m._2)); }
    if (m.tag === "Lambda") { return f(m._1)(foldableElim.foldr(f)(z)(m._2)); }
    if (m.tag === "DProject") { return foldableExpr.foldr(f)(foldableExpr.foldr(f)(z)(m._2))(m._1); }
    if (m.tag === "App") { return foldableExpr.foldr(f)(foldableExpr.foldr(f)(z)(m._2))(m._1); }
    if (m.tag === "Let") { return foldableVarDef.foldr(f)(foldableExpr.foldr(f)(z)(m._2))(m._1); }
    if (m.tag === "LetRec") { return foldableRecDefs.foldr(f)(foldableExpr.foldr(f)(z)(m._2))(m._1); }
    if (m.tag === "DocExpr") { return foldableExpr.foldr(f)(foldableExpr.foldr(f)(z)(m._2))(m._1); }
    $runtime.fail();
  },
  foldMap: dictMonoid => {
    const mempty = dictMonoid.mempty;
    const $0 = dictMonoid.Semigroup0();
    const foldMap3 = Data$dList$dTypes.foldableList.foldMap(dictMonoid);
    return f => m => {
      if (m.tag === "Var") { return mempty; }
      if (m.tag === "Op") { return mempty; }
      if (m.tag === "Int") { return f(m._1); }
      if (m.tag === "Float") { return f(m._1); }
      if (m.tag === "Str") { return f(m._1); }
      if (m.tag === "Dictionary") { return $0.append(f(m._1))(foldMap3(Util$dPair.foldablePair.foldMap(dictMonoid)(foldableExpr.foldMap(dictMonoid)(f)))(m._2)); }
      if (m.tag === "Constr") { return $0.append(f(m._1))(foldMap3(foldableExpr.foldMap(dictMonoid)(f))(m._3)); }
      if (m.tag === "Matrix") { return $0.append(f(m._1))($0.append(foldableExpr.foldMap(dictMonoid)(f)(m._2))(foldableExpr.foldMap(dictMonoid)(f)(m._4))); }
      if (m.tag === "Lambda") { return $0.append(f(m._1))(foldableElim.foldMap(dictMonoid)(f)(m._2)); }
      if (m.tag === "DProject") { return $0.append(foldableExpr.foldMap(dictMonoid)(f)(m._1))(foldableExpr.foldMap(dictMonoid)(f)(m._2)); }
      if (m.tag === "App") { return $0.append(foldableExpr.foldMap(dictMonoid)(f)(m._1))(foldableExpr.foldMap(dictMonoid)(f)(m._2)); }
      if (m.tag === "Let") { return $0.append(foldableVarDef.foldMap(dictMonoid)(f)(m._1))(foldableExpr.foldMap(dictMonoid)(f)(m._2)); }
      if (m.tag === "LetRec") { return $0.append(foldableRecDefs.foldMap(dictMonoid)(f)(m._1))(foldableExpr.foldMap(dictMonoid)(f)(m._2)); }
      if (m.tag === "DocExpr") { return $0.append(foldableExpr.foldMap(dictMonoid)(f)(m._1))(foldableExpr.foldMap(dictMonoid)(f)(m._2)); }
      $runtime.fail();
    };
  }
};
const foldableElim = {
  foldl: f => z => m => {
    if (m.tag === "ElimVar") { return foldableCont.foldl(f)(z)(m._2); }
    if (m.tag === "ElimConstr") {
      const $0 = foldableCont.foldl(f);
      return Foreign$dObject.fold(z$1 => v => $0(z$1))(z)(m._1);
    }
    if (m.tag === "ElimDict") { return foldableCont.foldl(f)(z)(m._2); }
    $runtime.fail();
  },
  foldr: f => z => m => {
    if (m.tag === "ElimVar") { return foldableCont.foldr(f)(z)(m._2); }
    if (m.tag === "ElimConstr") {
      const $0 = foldableCont.foldr(f);
      return Data$dFoldable.foldrArray(b => a => $0(a)(b))(z)(Foreign$dObject.values(m._1));
    }
    if (m.tag === "ElimDict") { return foldableCont.foldr(f)(z)(m._2); }
    $runtime.fail();
  },
  foldMap: dictMonoid => {
    const foldMap1 = Foreign$dObject.foldMap(dictMonoid);
    return f => m => {
      if (m.tag === "ElimVar") { return foldableCont.foldMap(dictMonoid)(f)(m._2); }
      if (m.tag === "ElimConstr") {
        const $0 = foldableCont.foldMap(dictMonoid)(f);
        return foldMap1(v => $0)(m._1);
      }
      if (m.tag === "ElimDict") { return foldableCont.foldMap(dictMonoid)(f)(m._2); }
      $runtime.fail();
    };
  }
};
const foldableCont = {
  foldl: f => z => m => {
    if (m.tag === "ContExpr") { return foldableExpr.foldl(f)(z)(m._1); }
    if (m.tag === "ContElim") { return foldableElim.foldl(f)(z)(m._1); }
    $runtime.fail();
  },
  foldr: f => z => m => {
    if (m.tag === "ContExpr") { return foldableExpr.foldr(f)(z)(m._1); }
    if (m.tag === "ContElim") { return foldableElim.foldr(f)(z)(m._1); }
    $runtime.fail();
  },
  foldMap: dictMonoid => f => m => {
    if (m.tag === "ContExpr") { return foldableExpr.foldMap(dictMonoid)(f)(m._1); }
    if (m.tag === "ContElim") { return foldableElim.foldMap(dictMonoid)(f)(m._1); }
    $runtime.fail();
  }
};
const traversableVarDef = {
  traverse: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    return f => m => Apply0.apply(Apply0.Functor0().map(v2 => v3 => $VarDef(v2, v3))(traversableElim.traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2));
  },
  sequence: dictApplicative => v => traversableVarDef.traverse(dictApplicative)(identity)(v),
  Functor0: () => functorVarDef,
  Foldable1: () => foldableVarDef
};
const traversableRecDefs = {
  traverse: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    const traverse5 = Dict.traversableDict.traverse(dictApplicative);
    return f => m => Apply0.apply(Apply0.Functor0().map(v2 => v3 => $RecDefs(v2, v3))(f(m._1)))(traverse5(traversableElim.traverse(dictApplicative)(f))(m._2));
  },
  sequence: dictApplicative => v => traversableRecDefs.traverse(dictApplicative)(identity)(v),
  Functor0: () => functorRecDefs,
  Foldable1: () => foldableRecDefs
};
const traversableExpr = {
  traverse: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    const $0 = Apply0.Functor0();
    const traverse5 = Data$dList$dTypes.traversableList.traverse(dictApplicative);
    const traverse6 = Util$dPair.traversablePair.traverse(dictApplicative);
    return f => m => {
      if (m.tag === "Var") { return dictApplicative.pure($Expr("Var", m._1)); }
      if (m.tag === "Op") { return dictApplicative.pure($Expr("Op", m._1)); }
      if (m.tag === "Int") {
        const $1 = m._2;
        return $0.map(v2 => $Expr("Int", v2, $1))(f(m._1));
      }
      if (m.tag === "Float") {
        const $1 = m._2;
        return $0.map(v2 => $Expr("Float", v2, $1))(f(m._1));
      }
      if (m.tag === "Str") {
        const $1 = m._2;
        return $0.map(v2 => $Expr("Str", v2, $1))(f(m._1));
      }
      if (m.tag === "Dictionary") {
        return Apply0.apply($0.map(v2 => v3 => $Expr("Dictionary", v2, v3))(f(m._1)))(traverse5(traverse6(traversableExpr.traverse(dictApplicative)(f)))(m._2));
      }
      if (m.tag === "Constr") {
        const $1 = m._2;
        return Apply0.apply($0.map(v3 => v4 => $Expr("Constr", v3, $1, v4))(f(m._1)))(traverse5(traversableExpr.traverse(dictApplicative)(f))(m._3));
      }
      if (m.tag === "Matrix") {
        const $1 = m._3;
        return Apply0.apply(Apply0.apply($0.map(v4 => v5 => v6 => $Expr("Matrix", v4, v5, $1, v6))(f(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2)))(traversableExpr.traverse(dictApplicative)(f)(m._4));
      }
      if (m.tag === "Lambda") { return Apply0.apply($0.map(v2 => v3 => $Expr("Lambda", v2, v3))(f(m._1)))(traversableElim.traverse(dictApplicative)(f)(m._2)); }
      if (m.tag === "DProject") {
        return Apply0.apply($0.map(v2 => v3 => $Expr("DProject", v2, v3))(traversableExpr.traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "App") {
        return Apply0.apply($0.map(v2 => v3 => $Expr("App", v2, v3))(traversableExpr.traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "Let") {
        return Apply0.apply($0.map(v2 => v3 => $Expr("Let", v2, v3))(traversableVarDef.traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "LetRec") {
        return Apply0.apply($0.map(v2 => v3 => $Expr("LetRec", v2, v3))(traversableRecDefs.traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "DocExpr") {
        return Apply0.apply($0.map(v2 => v3 => $Expr("DocExpr", v2, v3))(traversableExpr.traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2));
      }
      $runtime.fail();
    };
  },
  sequence: dictApplicative => v => traversableExpr.traverse(dictApplicative)(identity)(v),
  Functor0: () => functorExpr,
  Foldable1: () => foldableExpr
};
const traversableElim = {
  traverse: dictApplicative => {
    const $0 = dictApplicative.Apply0().Functor0();
    const traverse5 = Dict.traversableDict.traverse(dictApplicative);
    return f => m => {
      if (m.tag === "ElimVar") {
        const $1 = m._1;
        return $0.map(v2 => $Elim("ElimVar", $1, v2))(traversableCont.traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "ElimConstr") { return $0.map(v1 => $Elim("ElimConstr", v1))(traverse5(traversableCont.traverse(dictApplicative)(f))(m._1)); }
      if (m.tag === "ElimDict") {
        const $1 = m._1;
        return $0.map(v2 => $Elim("ElimDict", $1, v2))(traversableCont.traverse(dictApplicative)(f)(m._2));
      }
      $runtime.fail();
    };
  },
  sequence: dictApplicative => v => traversableElim.traverse(dictApplicative)(identity)(v),
  Functor0: () => functorElim,
  Foldable1: () => foldableElim
};
const traversableCont = {
  traverse: dictApplicative => {
    const $0 = dictApplicative.Apply0().Functor0();
    return f => m => {
      if (m.tag === "ContExpr") { return $0.map(v1 => $Cont("ContExpr", v1))(traversableExpr.traverse(dictApplicative)(f)(m._1)); }
      if (m.tag === "ContElim") { return $0.map(v1 => $Cont("ContElim", v1))(traversableElim.traverse(dictApplicative)(f)(m._1)); }
      $runtime.fail();
    };
  },
  sequence: dictApplicative => v => traversableCont.traverse(dictApplicative)(identity)(v),
  Functor0: () => functorCont,
  Foldable1: () => foldableCont
};
const expandableVarDefRawVarDef = dictBoundedJoinSemilattice => (
  {expand: v => v1 => $VarDef(expandableElimRawElim(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2))}
);
const expandableRecDefsRawRecDe = dictBoundedJoinSemilattice => {
  const expandableDictDict = Lattice.expandableDictDict({
    botOf: functorElim.map((() => {
      const $0 = dictBoundedJoinSemilattice.bot;
      return v => $0;
    })())
  });
  return {expand: v => v1 => $RecDefs(v._1, expandableDictDict(expandableElimRawElim(dictBoundedJoinSemilattice)).expand(v._2)(v1._2))};
};
const expandableExprRawExpr = dictBoundedJoinSemilattice => (
  {
    expand: v => v1 => {
      if (v.tag === "Var") {
        if (v1.tag === "Var") {
          const $0 = v._1;
          const $1 = v1._1;
          return $Expr("Var", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Op") {
        if (v1.tag === "Op") {
          const $0 = v._1;
          const $1 = v1._1;
          return $Expr("Op", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Int") {
        if (v1.tag === "Int") {
          const $0 = v._2;
          const $1 = v1._2;
          return $Expr("Int", v._1, Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Str") {
        if (v1.tag === "Str") {
          const $0 = v._2;
          const $1 = v1._2;
          return $Expr("Str", v._1, Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Float") {
        if (v1.tag === "Float") {
          const $0 = v._2;
          const $1 = v1._2;
          return $Expr("Float", v._1, Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Dictionary") {
        if (v1.tag === "Dictionary") {
          return $Expr(
            "Dictionary",
            v._1,
            (() => {
              const $0 = expandableExprRawExpr(dictBoundedJoinSemilattice);
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
                    go$a2 = Data$dList$dTypes.$List("Cons", Util$dPair.$Pair($0.expand(v$1._1._1)(v1$1._1._1), $0.expand(v$1._1._2)(v1$1._1._2)), v2);
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
      if (v.tag === "Constr") {
        if (v1.tag === "Constr") {
          const $0 = v._2;
          const $1 = v1._2;
          return $Expr(
            "Constr",
            v._1,
            Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0),
            (() => {
              const $2 = expandableExprRawExpr(dictBoundedJoinSemilattice);
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
              return go$1(Data$dList$dTypes.Nil)(go(v._3)(v1._3)(Data$dList$dTypes.Nil));
            })()
          );
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Matrix") {
        if (v1.tag === "Matrix") {
          const $0 = v._3._1;
          const $1 = v1._3._1;
          const $2 = v._3._2;
          const $3 = v1._3._2;
          return $Expr(
            "Matrix",
            v._1,
            expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2),
            Data$dTuple.$Tuple(Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0), Util.assertWhen(false)("mustEq")(v$1 => $2 === $3)($2)),
            expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._4)(v1._4)
          );
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Lambda") {
        if (v1.tag === "Lambda") { return $Expr("Lambda", v._1, expandableElimRawElim(dictBoundedJoinSemilattice).expand(v._2)(v1._2)); }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "DProject") {
        if (v1.tag === "DProject") {
          return $Expr("DProject", expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "App") {
        if (v1.tag === "App") {
          return $Expr("App", expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Let") {
        if (v1.tag === "Let") {
          return $Expr("Let", expandableVarDefRawVarDef(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "LetRec") {
        if (v1.tag === "LetRec") {
          return $Expr("LetRec", expandableRecDefsRawRecDe(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "DocExpr" && v1.tag === "DocExpr") {
        return $Expr("DocExpr", expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  }
);
const expandableElimRawElim = dictBoundedJoinSemilattice => {
  const expandableDictDict = Lattice.expandableDictDict({
    botOf: functorCont.map((() => {
      const $0 = dictBoundedJoinSemilattice.bot;
      return v => $0;
    })())
  });
  return {
    expand: v => v1 => {
      if (v.tag === "ElimVar") {
        if (v1.tag === "ElimVar") {
          const $0 = v._1;
          const $1 = v1._1;
          return $Elim("ElimVar", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0), expandableContRawCont(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
        }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "ElimConstr") {
        if (v1.tag === "ElimConstr") { return $Elim("ElimConstr", expandableDictDict(expandableContRawCont(dictBoundedJoinSemilattice)).expand(v._1)(v1._1)); }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "ElimDict" && v1.tag === "ElimDict") {
        const $0 = v._1;
        const $1 = v1._1;
        return $Elim(
          "ElimDict",
          Util.assertWhen(false)("mustEq")(v$1 => Data$dMap$dInternal.eqMap(Data$dEq.eqString)(Data$dEq.eqUnit).eq($0)($1))($0),
          expandableContRawCont(dictBoundedJoinSemilattice).expand(v._2)(v1._2)
        );
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  };
};
const expandableContRawCont = dictBoundedJoinSemilattice => (
  {
    expand: v => v1 => {
      if (v.tag === "ContExpr") {
        if (v1.tag === "ContExpr") { return $Cont("ContExpr", expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._1)(v1._1)); }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "ContElim" && v1.tag === "ContElim") { return $Cont("ContElim", expandableElimRawElim(dictBoundedJoinSemilattice).expand(v._1)(v1._1)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  }
);
const eqVarDef = dictEq => ({eq: x => y => eqElim(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2)});
const eqRecDefs = dictEq => ({eq: x => y => dictEq.eq(x._1)(y._1) && Foreign$dObject.eqObject(eqElim(dictEq)).eq(x._2)(y._2)});
const eqExpr = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "Var") { return y.tag === "Var" && x._1 === y._1; }
      if (x.tag === "Op") { return y.tag === "Op" && x._1 === y._1; }
      if (x.tag === "Int") { return y.tag === "Int" && dictEq.eq(x._1)(y._1) && x._2 === y._2; }
      if (x.tag === "Float") { return y.tag === "Float" && dictEq.eq(x._1)(y._1) && x._2 === y._2; }
      if (x.tag === "Str") { return y.tag === "Str" && dictEq.eq(x._1)(y._1) && x._2 === y._2; }
      if (x.tag === "Dictionary") {
        return y.tag === "Dictionary" && (() => {
          const $0 = eqExpr(dictEq);
          return dictEq.eq(x._1)(y._1) && (() => {
            const go = v => v1 => v2 => {
              if (!v2) { return false; }
              if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
              return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1._1)(v._1._1) && $0.eq(v1._1._2)(v._1._2));
            };
            return go(x._2)(y._2)(true);
          })();
        })();
      }
      if (x.tag === "Constr") {
        return y.tag === "Constr" && (() => {
          const $0 = eqExpr(dictEq);
          return dictEq.eq(x._1)(y._1) && x._2 === y._2 && (() => {
            const go = v => v1 => v2 => {
              if (!v2) { return false; }
              if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
              return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
            };
            return go(x._3)(y._3)(true);
          })();
        })();
      }
      if (x.tag === "Matrix") {
        return y.tag === "Matrix" && dictEq.eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2) && x._3._1 === y._3._1 && x._3._2 === y._3._2 && eqExpr(dictEq).eq(x._4)(y._4);
      }
      if (x.tag === "Lambda") { return y.tag === "Lambda" && dictEq.eq(x._1)(y._1) && eqElim(dictEq).eq(x._2)(y._2); }
      if (x.tag === "DProject") { return y.tag === "DProject" && eqExpr(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2); }
      if (x.tag === "App") { return y.tag === "App" && eqExpr(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2); }
      if (x.tag === "Let") { return y.tag === "Let" && eqVarDef(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2); }
      if (x.tag === "LetRec") { return y.tag === "LetRec" && eqRecDefs(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2); }
      return x.tag === "DocExpr" && y.tag === "DocExpr" && eqExpr(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2);
    }
  }
);
const eqElim = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "ElimVar") { return y.tag === "ElimVar" && x._1 === y._1 && eqCont(dictEq).eq(x._2)(y._2); }
      if (x.tag === "ElimConstr") { return y.tag === "ElimConstr" && Foreign$dObject.eqObject(eqCont(dictEq)).eq(x._1)(y._1); }
      return x.tag === "ElimDict" && y.tag === "ElimDict" && Data$dMap$dInternal.eqMap(Data$dEq.eqString)(Data$dEq.eqUnit).eq(x._1)(y._1) && eqCont(dictEq).eq(x._2)(y._2);
    }
  }
);
const eqCont = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "ContExpr") { return y.tag === "ContExpr" && eqExpr(dictEq).eq(x._1)(y._1); }
      return x.tag === "ContElim" && y.tag === "ContElim" && eqElim(dictEq).eq(x._1)(y._1);
    }
  }
);
const ordVarDef = dictOrd => {
  const eqVarDef1 = eqVarDef(dictOrd.Eq0());
  return {
    compare: x => y => {
      const v = ordElim(dictOrd).compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      return ordExpr(dictOrd).compare(x._2)(y._2);
    },
    Eq0: () => eqVarDef1
  };
};
const ordRecDefs = dictOrd => {
  const eqRecDefs1 = eqRecDefs(dictOrd.Eq0());
  return {
    compare: x => y => {
      const v = dictOrd.compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      return Dict.ordDict(ordElim(dictOrd)).compare(x._2)(y._2);
    },
    Eq0: () => eqRecDefs1
  };
};
const ordExpr = dictOrd => {
  const eqExpr1 = eqExpr(dictOrd.Eq0());
  return {
    compare: x => y => {
      if (x.tag === "Var") {
        if (y.tag === "Var") { return Data$dOrd.ordString.compare(x._1)(y._1); }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Var") { return Data$dOrdering.GT; }
      if (x.tag === "Op") {
        if (y.tag === "Op") { return Data$dOrd.ordString.compare(x._1)(y._1); }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Op") { return Data$dOrdering.GT; }
      if (x.tag === "Int") {
        if (y.tag === "Int") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return Data$dOrd.ordInt.compare(x._2)(y._2);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Int") { return Data$dOrdering.GT; }
      if (x.tag === "Float") {
        if (y.tag === "Float") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return Data$dOrd.ordNumber.compare(x._2)(y._2);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Float") { return Data$dOrdering.GT; }
      if (x.tag === "Str") {
        if (y.tag === "Str") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return Data$dOrd.ordString.compare(x._2)(y._2);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Str") { return Data$dOrdering.GT; }
      if (x.tag === "Dictionary") {
        if (y.tag === "Dictionary") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return Data$dList$dTypes.ordList((() => {
            const $0 = ordExpr(dictOrd);
            const $1 = $0.Eq0();
            const eqPair1 = {eq: x$1 => y$1 => $1.eq(x$1._1)(y$1._1) && $1.eq(x$1._2)(y$1._2)};
            return {
              compare: x$1 => y$1 => {
                const v$1 = $0.compare(x$1._1)(y$1._1);
                if (v$1 === "LT") { return Data$dOrdering.LT; }
                if (v$1 === "GT") { return Data$dOrdering.GT; }
                return $0.compare(x$1._2)(y$1._2);
              },
              Eq0: () => eqPair1
            };
          })()).compare(x._2)(y._2);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Dictionary") { return Data$dOrdering.GT; }
      if (x.tag === "Constr") {
        if (y.tag === "Constr") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          const v1 = Data$dOrd.ordString.compare(x._2)(y._2);
          if (v1 === "LT") { return Data$dOrdering.LT; }
          if (v1 === "GT") { return Data$dOrdering.GT; }
          return Data$dList$dTypes.ordList(ordExpr(dictOrd)).compare(x._3)(y._3);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Constr") { return Data$dOrdering.GT; }
      if (x.tag === "Matrix") {
        if (y.tag === "Matrix") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          const v1 = ordExpr(dictOrd).compare(x._2)(y._2);
          if (v1 === "LT") { return Data$dOrdering.LT; }
          if (v1 === "GT") { return Data$dOrdering.GT; }
          const v2 = compare3(x._3)(y._3);
          if (v2 === "LT") { return Data$dOrdering.LT; }
          if (v2 === "GT") { return Data$dOrdering.GT; }
          return ordExpr(dictOrd).compare(x._4)(y._4);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Matrix") { return Data$dOrdering.GT; }
      if (x.tag === "Lambda") {
        if (y.tag === "Lambda") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return ordElim(dictOrd).compare(x._2)(y._2);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Lambda") { return Data$dOrdering.GT; }
      if (x.tag === "DProject") {
        if (y.tag === "DProject") {
          const v = ordExpr(dictOrd).compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return ordExpr(dictOrd).compare(x._2)(y._2);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "DProject") { return Data$dOrdering.GT; }
      if (x.tag === "App") {
        if (y.tag === "App") {
          const v = ordExpr(dictOrd).compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return ordExpr(dictOrd).compare(x._2)(y._2);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "App") { return Data$dOrdering.GT; }
      if (x.tag === "Let") {
        if (y.tag === "Let") {
          const v = ordVarDef(dictOrd).compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return ordExpr(dictOrd).compare(x._2)(y._2);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Let") { return Data$dOrdering.GT; }
      if (x.tag === "LetRec") {
        if (y.tag === "LetRec") {
          const v = ordRecDefs(dictOrd).compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return ordExpr(dictOrd).compare(x._2)(y._2);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "LetRec") { return Data$dOrdering.GT; }
      if (x.tag === "DocExpr" && y.tag === "DocExpr") {
        const v = ordExpr(dictOrd).compare(x._1)(y._1);
        if (v === "LT") { return Data$dOrdering.LT; }
        if (v === "GT") { return Data$dOrdering.GT; }
        return ordExpr(dictOrd).compare(x._2)(y._2);
      }
      $runtime.fail();
    },
    Eq0: () => eqExpr1
  };
};
const ordElim = dictOrd => {
  const eqElim1 = eqElim(dictOrd.Eq0());
  return {
    compare: x => y => {
      if (x.tag === "ElimVar") {
        if (y.tag === "ElimVar") {
          const v = Data$dOrd.ordString.compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return ordCont(dictOrd).compare(x._2)(y._2);
        }
        return Data$dOrdering.LT;
      }
      if (y.tag === "ElimVar") { return Data$dOrdering.GT; }
      if (x.tag === "ElimConstr") {
        if (y.tag === "ElimConstr") { return Dict.ordDict(ordCont(dictOrd)).compare(x._1)(y._1); }
        return Data$dOrdering.LT;
      }
      if (y.tag === "ElimConstr") { return Data$dOrdering.GT; }
      if (x.tag === "ElimDict" && y.tag === "ElimDict") {
        const v = compare4(x._1)(y._1);
        if (v === "LT") { return Data$dOrdering.LT; }
        if (v === "GT") { return Data$dOrdering.GT; }
        return ordCont(dictOrd).compare(x._2)(y._2);
      }
      $runtime.fail();
    },
    Eq0: () => eqElim1
  };
};
const ordCont = dictOrd => {
  const eqCont1 = eqCont(dictOrd.Eq0());
  return {
    compare: x => y => {
      if (x.tag === "ContExpr") {
        if (y.tag === "ContExpr") { return ordExpr(dictOrd).compare(x._1)(y._1); }
        return Data$dOrdering.LT;
      }
      if (y.tag === "ContExpr") { return Data$dOrdering.GT; }
      if (x.tag === "ContElim" && y.tag === "ContElim") { return ordElim(dictOrd).compare(x._1)(y._1); }
      $runtime.fail();
    },
    Eq0: () => eqCont1
  };
};
const applyRecDefs = {
  apply: v => v1 => $RecDefs(v._1(v1._1), Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(v._2, applyElim.apply))(v1._2)),
  Functor0: () => functorRecDefs
};
const applyExpr = {
  apply: v => v1 => {
    if (v.tag === "Var") {
      if (v1.tag === "Var") {
        const $0 = v._1;
        const $1 = v1._1;
        return $Expr("Var", Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Op") {
      if (v1.tag === "Op") { return $Expr("Op", v._1); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Int") {
      if (v1.tag === "Int") {
        const $0 = v._2;
        const $1 = v1._2;
        return $Expr("Int", v._1(v1._1), Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Float") {
      if (v1.tag === "Float") {
        const $0 = v._2;
        const $1 = v1._2;
        return $Expr("Float", v._1(v1._1), Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Str") {
      if (v1.tag === "Str") {
        const $0 = v._2;
        const $1 = v1._2;
        return $Expr("Str", v._1(v1._1), Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0));
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Dictionary") {
      if (v1.tag === "Dictionary") {
        return $Expr(
          "Dictionary",
          v._1(v1._1),
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
                  go$a2 = Data$dList$dTypes.$List("Cons", Util$dPair.$Pair(applyExpr.apply(v$1._1._1)(v1$1._1._1), applyExpr.apply(v$1._1._2)(v1$1._1._2)), v2);
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
    if (v.tag === "Constr") {
      if (v1.tag === "Constr") {
        const $0 = v._2;
        const $1 = v1._2;
        return $Expr(
          "Constr",
          v._1(v1._1),
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
                  go$a2 = Data$dList$dTypes.$List("Cons", applyExpr.apply(v$1._1)(v1$1._1), v2);
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
            return go$1(Data$dList$dTypes.Nil)(go(v._3)(v1._3)(Data$dList$dTypes.Nil));
          })()
        );
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Matrix") {
      if (v1.tag === "Matrix") {
        const $0 = v._3._1;
        const $1 = v1._3._1;
        const $2 = v._3._2;
        const $3 = v1._3._2;
        return $Expr(
          "Matrix",
          v._1(v1._1),
          applyExpr.apply(v._2)(v1._2),
          Data$dTuple.$Tuple(Util.assertWhen(false)("mustEq")(v$1 => $0 === $1)($0), Util.assertWhen(false)("mustEq")(v$1 => $2 === $3)($2)),
          applyExpr.apply(v._4)(v1._4)
        );
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Lambda") {
      if (v1.tag === "Lambda") { return $Expr("Lambda", v._1(v1._1), applyElim.apply(v._2)(v1._2)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "DProject") {
      if (v1.tag === "DProject") { return $Expr("DProject", applyExpr.apply(v._1)(v1._1), applyExpr.apply(v._2)(v1._2)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "App") {
      if (v1.tag === "App") { return $Expr("App", applyExpr.apply(v._1)(v1._1), applyExpr.apply(v._2)(v1._2)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Let") {
      if (v1.tag === "Let") { return $Expr("Let", $VarDef(applyElim.apply(v._1._1)(v1._1._1), applyExpr.apply(v._1._2)(v1._1._2)), applyExpr.apply(v._2)(v1._2)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "LetRec") {
      if (v1.tag === "LetRec") { return $Expr("LetRec", applyRecDefs.apply(v._1)(v1._1), applyExpr.apply(v._2)(v1._2)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "DocExpr" && v1.tag === "DocExpr") { return $Expr("DocExpr", applyExpr.apply(v._1)(v1._1), applyExpr.apply(v._2)(v1._2)); }
    return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
  },
  Functor0: () => functorExpr
};
const applyElim = {
  apply: v => v1 => {
    if (v.tag === "ElimVar") {
      if (v1.tag === "ElimVar") { return $Elim("ElimVar", v._1, applyCont.apply(v._2)(v1._2)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "ElimConstr") {
      if (v1.tag === "ElimConstr") {
        return $Elim("ElimConstr", Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(v._1, applyCont.apply))(v1._1));
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "ElimDict" && v1.tag === "ElimDict") { return $Elim("ElimDict", v._1, applyCont.apply(v._2)(v1._2)); }
    return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
  },
  Functor0: () => functorElim
};
const applyCont = {
  apply: v => v1 => {
    if (v.tag === "ContExpr") {
      if (v1.tag === "ContExpr") { return $Cont("ContExpr", applyExpr.apply(v._1)(v1._1)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "ContElim" && v1.tag === "ContElim") { return $Cont("ContElim", applyElim.apply(v._1)(v1._1)); }
    return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
  },
  Functor0: () => functorCont
};
const meetSemilatticeExpr = dictMeetSemilattice => (
  {
    meet: (() => {
      const $0 = dictMeetSemilattice.meet;
      return a => b => applyExpr.apply(functorExpr.map($0)(a))(b);
    })()
  }
);
const applyVarDef = {apply: v => v1 => $VarDef(applyElim.apply(v._1)(v1._1), applyExpr.apply(v._2)(v1._2)), Functor0: () => functorVarDef};
const applyModule = {
  apply: v => v1 => {
    if (v.tag === "Nil") {
      if (v1.tag === "Nil") { return Data$dList$dTypes.Nil; }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Cons" && v1.tag === "Cons") {
      if (v._1.tag === "Left") {
        if (v1._1.tag === "Left") { return Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", applyVarDef.apply(v._1._1)(v1._1._1)), applyModule.apply(v._2)(v1._2)); }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v._1.tag === "Right" && v1._1.tag === "Right") {
        return Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Right", applyRecDefs.apply(v._1._1)(v1._1._1)), applyModule.apply(v._2)(v1._2));
      }
    }
    return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
  },
  Functor0: () => functorModule
};
const fv = dict => dict.fv;
const fVDict = dictFV => {
  const fv1 = dictFV.fv;
  return {fv: ρ => setSet.difference(unions2(Foreign$dObject._fmapObject(ρ, fv1)))(fromFoldable(Util$dMap.mapObjectString.keys(ρ)))};
};
const fVList = dictFV => {
  const fv1 = dictFV.fv;
  return {fv: xs => unions3(Data$dList$dTypes.listMap(fv1)(xs))};
};
const fVMaybe = dictFV => (
  {
    fv: v => {
      if (v.tag === "Nothing") { return Data$dMap$dInternal.Leaf; }
      if (v.tag === "Just") { return dictFV.fv(v._1); }
      $runtime.fail();
    }
  }
);
const fV$x215 = dictFV => dictFV1 => ({fv: v => setSet.union(dictFV.fv(v._1))(dictFV1.fv(v._2))});
const foldlModuleDef = v => v1 => v2 => {
  if (v2.tag === "Left") { return foldableVarDef.foldl(v)(v1)(v2._1); }
  if (v2.tag === "Right") { return foldableRecDefs.foldl(v)(v1)(v2._1); }
  $runtime.fail();
};
const foldableModule = {
  foldl: v => v1 => v2 => {
    if (v2.tag === "Nil") { return v1; }
    if (v2.tag === "Cons") {
      if (v2._1.tag === "Left") {
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
              go$a0 = foldlModuleDef(v)(b)(v$1._1);
              go$a1 = v$1._2;
              continue;
            }
            $runtime.fail();
          }
          return go$r;
        };
        return go(foldableVarDef.foldl(v)(v1)(v2._1._1))(v2._2);
      }
      if (v2._1.tag === "Right") {
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
              go$a0 = foldlModuleDef(v)(b)(v$1._1);
              go$a1 = v$1._2;
              continue;
            }
            $runtime.fail();
          }
          return go$r;
        };
        return go(foldableRecDefs.foldl(v)(v1)(v2._1._1))(v2._2);
      }
    }
    $runtime.fail();
  },
  foldr: f => Data$dFoldable.foldrDefault(foldableModule)(f),
  foldMap: dictMonoid => f => foldableModule.foldl(acc => x => dictMonoid.Semigroup0().append(acc)(f(x)))(dictMonoid.mempty)
};
const traversableModule = {
  traverse: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    const $0 = Apply0.Functor0();
    const traverse5 = traversableVarDef.traverse(dictApplicative);
    const traverse6 = traversableRecDefs.traverse(dictApplicative);
    return v => v1 => {
      if (v1.tag === "Nil") { return dictApplicative.pure(Data$dList$dTypes.Nil); }
      if (v1.tag === "Cons") {
        if (v1._1.tag === "Left") {
          return $0.map(Module)(Apply0.apply(Apply0.Functor0().map(Data$dList$dTypes.Cons)($0.map(Data$dEither.Left)(traverse5(v)(v1._1._1))))($0.map(Unsafe$dCoerce.unsafeCoerce)(traversableModule.traverse(dictApplicative)(v)(v1._2))));
        }
        if (v1._1.tag === "Right") {
          return $0.map(Module)(Apply0.apply(Apply0.Functor0().map(Data$dList$dTypes.Cons)($0.map(Data$dEither.Right)(traverse6(v)(v1._1._1))))($0.map(Unsafe$dCoerce.unsafeCoerce)(traversableModule.traverse(dictApplicative)(v)(v1._2))));
        }
      }
      $runtime.fail();
    };
  },
  sequence: dictApplicative => traversableModule.traverse(dictApplicative)(Data$dTraversable.identity),
  Functor0: () => functorModule,
  Foldable1: () => foldableModule
};
const bv = dict => dict.bv;
const bVElim = {
  bv: v => {
    if (v.tag === "ElimVar") {
      return setSet.union(Data$dMap$dInternal.$$$Map("Node", 1, 1, v._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(bVCont.bv(v._2));
    }
    if (v.tag === "ElimConstr") { return bVCont.bv(asMaplet(v._1)._2); }
    if (v.tag === "ElimDict") { return bVCont.bv(v._2); }
    $runtime.fail();
  }
};
const bVCont = {
  bv: v => {
    if (v.tag === "ContElim") { return bVElim.bv(v._1); }
    if (v.tag === "ContExpr") { return Data$dMap$dInternal.Leaf; }
    $runtime.fail();
  }
};
const bVVarDef = {bv: v => bVElim.bv(v._1)};
const fVVarDef = {fv: v => fVExpr.fv(v._2)};
const fVRecDefs = {fv: v => fVDict(fVElim).fv(v._2)};
const fVExpr = {
  fv: v => {
    if (v.tag === "Var") { return Data$dMap$dInternal.$$$Map("Node", 1, 1, v._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf); }
    if (v.tag === "Op") { return Data$dMap$dInternal.$$$Map("Node", 1, 1, v._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf); }
    if (v.tag === "Int") { return Data$dMap$dInternal.Leaf; }
    if (v.tag === "Float") { return Data$dMap$dInternal.Leaf; }
    if (v.tag === "Str") { return Data$dMap$dInternal.Leaf; }
    if (v.tag === "Dictionary") { return unions3(Data$dList$dTypes.listMap(v1 => setSet.union(fVExpr.fv(v1._1))(fVExpr.fv(v1._2)))(v._2)); }
    if (v.tag === "Constr") { return unions3(Data$dList$dTypes.listMap(fVExpr.fv)(v._3)); }
    if (v.tag === "Matrix") { return setSet.union(fVExpr.fv(v._2))(fVExpr.fv(v._4)); }
    if (v.tag === "Lambda") { return fVElim.fv(v._2); }
    if (v.tag === "DProject") { return setSet.union(fVExpr.fv(v._1))(fVExpr.fv(v._2)); }
    if (v.tag === "App") { return setSet.union(fVExpr.fv(v._1))(fVExpr.fv(v._2)); }
    if (v.tag === "Let") { return setSet.union(fVExpr.fv(v._1._2))(setSet.difference(fVExpr.fv(v._2))(bVElim.bv(v._1._1))); }
    if (v.tag === "LetRec") { return setSet.union(fVDict(fVElim).fv(v._1._2))(fVExpr.fv(v._2)); }
    if (v.tag === "DocExpr") { return setSet.union(fVExpr.fv(v._1))(fVExpr.fv(v._2)); }
    $runtime.fail();
  }
};
const fVElim = {
  fv: v => {
    if (v.tag === "ElimVar") {
      return setSet.difference(fVCont.fv(v._2))(Data$dMap$dInternal.$$$Map("Node", 1, 1, v._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf));
    }
    if (v.tag === "ElimConstr") { return unions2(Foreign$dObject._fmapObject(v._1, fVCont.fv)); }
    if (v.tag === "ElimDict") { return fVCont.fv(v._2); }
    $runtime.fail();
  }
};
const fVCont = {
  fv: v => {
    if (v.tag === "ContElim") { return fVElim.fv(v._1); }
    if (v.tag === "ContExpr") { return fVExpr.fv(v._1); }
    $runtime.fail();
  }
};
const asExpr = v => {
  if (v.tag === "ContExpr") { return v._1; }
  return Effect$dException.throwException(Effect$dException.error("Expression expected"))();
};
const asElim = v => {
  if (v.tag === "ContElim") { return v._1; }
  return Effect$dException.throwException(Effect$dException.error("Eliminator expected"))();
};
export {
  $Cont,
  $Elim,
  $Expr,
  
  
  App,
  Constr,
  
  ContExpr,
  DProject,
  Dictionary,
  
  
  ElimDict,
  ElimVar,
  
  
  Lambda,
  Let,
  LetRec,
  Matrix,
  Module,
  
  RecDefs,
  
  
  VarDef,
  
  applyElim,
  applyExpr,
  
  
  
  asElim,
  
  
  
  
  
  
  
  
  
  eqElim,
  eqExpr,
  
  
  
  expandableElimRawElim,
  
  
  
  
  
  fVDict,
  fVElim,
  fVExpr,
  
  
  
  
  
  foldableElim,
  foldableExpr,
  
  
  
  
  
  
  functorElim,
  functorExpr,
  
  
  
  
  
  
  joinSemilatticeElim,
  
  
  
  
  
  
  ordElim,
  ordExpr,
  
  
  
  
  
  
  traversableElim,
  traversableExpr,
  traversableModule,
  
  
  
  
  
  
  
  
  
  verticesElimVertex,
  verticesExprVertex,
  verticesModuleVertex,
  
  
};
