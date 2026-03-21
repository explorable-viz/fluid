// | The _cofree comonad_ for a `Functor`.
import * as Control$dMonad$dFree from "../Control.Monad.Free/index.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dLazy from "../Data.Lazy/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const identity = x => x;
const state = /* #__PURE__ */ (() => Control$dMonad$dState$dTrans.monadStateStateT(Data$dIdentity.monadIdentity).state)();
const monadRecStateT = /* #__PURE__ */ Control$dMonad$dState$dTrans.monadRecStateT(Control$dMonad$dRec$dClass.monadRecIdentity);
const tail = v => Data$dLazy.force(v)._2;
const mkCofree = a => t => Data$dLazy.defer(v => Data$dTuple.$Tuple(a, t));
const lazyCofree = {defer: k => Data$dLazy.defer(v => Data$dLazy.force(k()))};
const hoistCofree = dictFunctor => nat => v => {
  const $0 = dictFunctor.map(hoistCofree(dictFunctor)(nat));
  return Data$dLazy.defer(v$1 => {
    const $1 = Data$dLazy.force(v);
    return Data$dTuple.$Tuple($1._1, nat($0($1._2)));
  });
};
const head = v => Data$dLazy.force(v)._1;
const functorCofree = dictFunctor => (
  {
    map: f => {
      const loop = v => Data$dLazy.defer(v$1 => {
        const $0 = Data$dLazy.force(v);
        return Data$dTuple.$Tuple(f($0._1), dictFunctor.map(loop)($0._2));
      });
      return loop;
    }
  }
);
const functorWithIndexCofree = dictFunctor => {
  const functorCofree1 = functorCofree(dictFunctor);
  return {
    mapWithIndex: f => {
      const loop = n => v => Data$dLazy.defer(v$1 => {
        const $0 = Data$dLazy.force(v);
        return Data$dTuple.$Tuple(f(n)($0._1), dictFunctor.map(loop(n + 1 | 0))($0._2));
      });
      return loop(0);
    },
    Functor0: () => functorCofree1
  };
};
const foldableCofree = dictFoldable => (
  {
    foldr: f => {
      const go = fa => b => f(Data$dLazy.force(fa)._1)(dictFoldable.foldr(go)(b)(Data$dLazy.force(fa)._2));
      return b => a => go(a)(b);
    },
    foldl: f => {
      const go = b => fa => dictFoldable.foldl(go)(f(b)(Data$dLazy.force(fa)._1))(Data$dLazy.force(fa)._2);
      return go;
    },
    foldMap: dictMonoid => {
      const foldMap1 = dictFoldable.foldMap(dictMonoid);
      return f => {
        const go = fa => dictMonoid.Semigroup0().append(f(Data$dLazy.force(fa)._1))(foldMap1(go)(Data$dLazy.force(fa)._2));
        return go;
      };
    }
  }
);
const foldableWithIndexCofree = dictFoldable => {
  const foldableCofree1 = foldableCofree(dictFoldable);
  return {
    foldrWithIndex: f => {
      const go = ix => b => fa => f(ix)(Data$dLazy.force(fa)._1)(dictFoldable.foldr((() => {
        const $0 = go(ix + 1 | 0);
        return b$1 => a => $0(a)(b$1);
      })())(b)(Data$dLazy.force(fa)._2));
      return go(0);
    },
    foldlWithIndex: f => {
      const go = ix => b => fa => dictFoldable.foldl(go(ix + 1 | 0))(f(ix)(b)(Data$dLazy.force(fa)._1))(Data$dLazy.force(fa)._2);
      return go(0);
    },
    foldMapWithIndex: dictMonoid => {
      const foldMap1 = dictFoldable.foldMap(dictMonoid);
      return f => {
        const go = ix => fa => dictMonoid.Semigroup0().append(f(ix)(Data$dLazy.force(fa)._1))(foldMap1(go(ix + 1 | 0))(Data$dLazy.force(fa)._2));
        return go(0);
      };
    },
    Foldable0: () => foldableCofree1
  };
};
const traversableCofree = dictTraversable => {
  const functorCofree1 = functorCofree(dictTraversable.Functor0());
  const foldableCofree1 = foldableCofree(dictTraversable.Foldable1());
  return {
    sequence: dictApplicative => traversableCofree(dictTraversable).traverse(dictApplicative)(identity),
    traverse: dictApplicative => {
      const Apply0 = dictApplicative.Apply0();
      const traverse1 = dictTraversable.traverse(dictApplicative);
      return f => {
        const loop = ta => Apply0.apply(Apply0.Functor0().map(mkCofree)(f(Data$dLazy.force(ta)._1)))(traverse1(loop)(Data$dLazy.force(ta)._2));
        return loop;
      };
    },
    Functor0: () => functorCofree1,
    Foldable1: () => foldableCofree1
  };
};
const traversableWithIndexCofree = dictTraversable => {
  const functorWithIndexCofree1 = functorWithIndexCofree(dictTraversable.Functor0());
  const foldableWithIndexCofree1 = foldableWithIndexCofree(dictTraversable.Foldable1());
  const traversableCofree1 = traversableCofree(dictTraversable);
  return {
    traverseWithIndex: dictApplicative => {
      const Apply0 = dictApplicative.Apply0();
      const traverse1 = dictTraversable.traverse(dictApplicative);
      return f => {
        const loop = ix => ta => Apply0.apply(Apply0.Functor0().map(mkCofree)(f(ix)(Data$dLazy.force(ta)._1)))(traverse1(loop(ix + 1 | 0))(Data$dLazy.force(ta)._2));
        return loop(0);
      };
    },
    FunctorWithIndex0: () => functorWithIndexCofree1,
    FoldableWithIndex1: () => foldableWithIndexCofree1,
    Traversable2: () => traversableCofree1
  };
};
const extendCofree = dictFunctor => {
  const functorCofree1 = functorCofree(dictFunctor);
  return {
    extend: f => {
      const loop = v => Data$dLazy.defer(v$1 => Data$dTuple.$Tuple(f(v), dictFunctor.map(loop)(Data$dLazy.force(v)._2)));
      return loop;
    },
    Functor0: () => functorCofree1
  };
};
const eqCofree = dictEq1 => dictEq => (
  {eq: x => y => dictEq.eq(Data$dLazy.force(x)._1)(Data$dLazy.force(y)._1) && dictEq1.eq1(eqCofree(dictEq1)(dictEq))(Data$dLazy.force(x)._2)(Data$dLazy.force(y)._2)}
);
const ordCofree = dictOrd1 => {
  const eqCofree1 = eqCofree(dictOrd1.Eq10());
  return dictOrd => {
    const eqCofree2 = eqCofree1(dictOrd.Eq0());
    return {
      compare: x => y => {
        const v = dictOrd.compare(Data$dLazy.force(x)._1)(Data$dLazy.force(y)._1);
        if (v === "EQ") { return dictOrd1.compare1(ordCofree(dictOrd1)(dictOrd))(Data$dLazy.force(x)._2)(Data$dLazy.force(y)._2); }
        return v;
      },
      Eq0: () => eqCofree2
    };
  };
};
const eq1Cofree = dictEq1 => ({eq1: dictEq => eqCofree(dictEq1)(dictEq).eq});
const ord1Cofree = dictOrd1 => {
  const ordCofree1 = ordCofree(dictOrd1);
  const $0 = dictOrd1.Eq10();
  const eq1Cofree1 = {eq1: dictEq => eqCofree($0)(dictEq).eq};
  return {compare1: dictOrd => ordCofree1(dictOrd).compare, Eq10: () => eq1Cofree1};
};
const deferCofree = x => Data$dLazy.defer(x);
const semigroupCofree = dictApply => dictSemigroup => (
  {
    append: x => y => Data$dLazy.defer(v => Data$dTuple.$Tuple(
      dictSemigroup.append(Data$dLazy.force(x)._1)(Data$dLazy.force(y)._1),
      dictApply.apply(dictApply.Functor0().map(semigroupCofree(dictApply)(dictSemigroup).append)(Data$dLazy.force(x)._2))(Data$dLazy.force(y)._2)
    ))
  }
);
const monoidCofree = dictApplicative => {
  const semigroupCofree1 = semigroupCofree(dictApplicative.Apply0());
  return dictMonoid => {
    const mempty = dictMonoid.mempty;
    const semigroupCofree2 = semigroupCofree1(dictMonoid.Semigroup0());
    return {mempty: Data$dLazy.defer(v => Data$dTuple.$Tuple(mempty, dictApplicative.pure(monoidCofree(dictApplicative)(dictMonoid).mempty))), Semigroup0: () => semigroupCofree2};
  };
};
const comonadCofree = dictFunctor => {
  const extendCofree1 = extendCofree(dictFunctor);
  return {extract: head, Extend0: () => extendCofree1};
};
const explore = dictFunctor => {
  const runFreeM = Control$dMonad$dFree.runFreeM(dictFunctor)(monadRecStateT);
  return dictFunctor1 => pair => m => w => {
    const v = runFreeM(ff => state(cof => pair(dictFunctor.map(Data$dTuple.Tuple)(ff))(Data$dLazy.force(cof)._2)))(m)(w);
    return v._1(Data$dLazy.force(v._2)._1);
  };
};
const exploreM = dictFunctor => dictFunctor1 => dictMonadRec => {
  const runFreeM1 = Control$dMonad$dFree.runFreeM(dictFunctor)(Control$dMonad$dState$dTrans.monadRecStateT(dictMonadRec));
  return pair => m => w => dictMonadRec.Monad0().Bind1().Apply0().Functor0().map(v => v._1(Data$dLazy.force(v._2)._1))(runFreeM1(ff => cof => pair(dictFunctor.map(Data$dTuple.Tuple)(ff))(Data$dLazy.force(cof)._2))(m)(w));
};
const buildCofree = dictFunctor => k => s => Data$dLazy.defer(v => {
  const $0 = k(s);
  return Data$dTuple.$Tuple($0._1, dictFunctor.map(buildCofree(dictFunctor)(k))($0._2));
});
const monadCofree = dictAlternative => ({Applicative0: () => applicativeCofree(dictAlternative), Bind1: () => bindCofree(dictAlternative)});
const bindCofree = dictAlternative => {
  const Alt0 = dictAlternative.Plus1().Alt0();
  return {
    bind: fa => f => {
      const loop = fa$p => {
        const fh = f(Data$dLazy.force(fa$p)._1);
        const $0 = Data$dLazy.force(fh)._1;
        const $1 = Alt0.alt(Data$dLazy.force(fh)._2)(Alt0.Functor0().map(loop)(Data$dLazy.force(fa$p)._2));
        return Data$dLazy.defer(v => Data$dTuple.$Tuple($0, $1));
      };
      return loop(fa);
    },
    Apply0: () => applyCofree(dictAlternative)
  };
};
const applyCofree = dictAlternative => {
  const functorCofree1 = functorCofree(dictAlternative.Plus1().Alt0().Functor0());
  return {
    apply: (() => {
      const $0 = bindCofree(dictAlternative);
      return f => a => $0.bind(f)(f$p => $0.bind(a)(a$p => applicativeCofree(dictAlternative).pure(f$p(a$p))));
    })(),
    Functor0: () => functorCofree1
  };
};
const applicativeCofree = dictAlternative => {
  const empty = dictAlternative.Plus1().empty;
  return {pure: a => Data$dLazy.defer(v => Data$dTuple.$Tuple(a, empty)), Apply0: () => applyCofree(dictAlternative)};
};
export {
  applicativeCofree,
  applyCofree,
  bindCofree,
  buildCofree,
  comonadCofree,
  deferCofree,
  eq1Cofree,
  eqCofree,
  explore,
  exploreM,
  extendCofree,
  foldableCofree,
  foldableWithIndexCofree,
  functorCofree,
  functorWithIndexCofree,
  head,
  hoistCofree,
  identity,
  lazyCofree,
  mkCofree,
  monadCofree,
  monadRecStateT,
  monoidCofree,
  ord1Cofree,
  ordCofree,
  semigroupCofree,
  state,
  tail,
  traversableCofree,
  traversableWithIndexCofree
};
