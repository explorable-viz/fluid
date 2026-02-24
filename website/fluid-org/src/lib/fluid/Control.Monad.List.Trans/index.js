// | This module defines the list monad transformer, `ListT`.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dLazy from "../Data.Lazy/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const $Step = (tag, _1, _2) => ({tag, _1, _2});
const identity = x => x;
const Yield = value0 => value1 => $Step("Yield", value0, value1);
const Skip = value0 => $Step("Skip", value0);
const Done = /* #__PURE__ */ $Step("Done");
const ListT = x => x;
const wrapLazy = dictApplicative => v => dictApplicative.pure($Step("Skip", v));
const wrapEffect = dictFunctor => v => dictFunctor.map(x => $Step("Skip", Data$dLazy.defer(v$1 => x)))(v);
const unfold = dictMonad => f => z => dictMonad.Bind1().Apply0().Functor0().map(v => {
  if (v.tag === "Just") {
    const $0 = v._1._1;
    return $Step("Yield", v._1._2, Data$dLazy.defer(v1 => unfold(dictMonad)(f)($0)));
  }
  if (v.tag === "Nothing") { return Done; }
  $runtime.fail();
})(f(z));
const uncons = dictMonad => {
  const $0 = dictMonad.Applicative0();
  return v => dictMonad.Bind1().bind(v)(v1 => {
    if (v1.tag === "Yield") { return $0.pure(Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(v1._1, Data$dLazy.force(v1._2)))); }
    if (v1.tag === "Skip") { return uncons(dictMonad)(Data$dLazy.force(v1._1)); }
    if (v1.tag === "Done") { return $0.pure(Data$dMaybe.Nothing); }
    $runtime.fail();
  });
};
const tail = dictMonad => {
  const uncons1 = uncons(dictMonad);
  return l => dictMonad.Bind1().Apply0().Functor0().map(v1 => {
    if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", v1._1._2); }
    return Data$dMaybe.Nothing;
  })(uncons1(l));
};
const takeWhile = dictApplicative => {
  const $0 = dictApplicative.Apply0().Functor0();
  return f => v => $0.map(v$1 => {
    if (v$1.tag === "Yield") {
      if (f(v$1._1)) {
        return $Step(
          "Yield",
          v$1._1,
          (() => {
            const $1 = takeWhile(dictApplicative)(f);
            return Data$dLazy.defer(v$2 => $1(Data$dLazy.force(v$1._2)));
          })()
        );
      }
      return Done;
    }
    if (v$1.tag === "Skip") {
      const $1 = v$1._1;
      return $Step(
        "Skip",
        (() => {
          const $2 = takeWhile(dictApplicative)(f);
          return Data$dLazy.defer(v$2 => $2(Data$dLazy.force($1)));
        })()
      );
    }
    if (v$1.tag === "Done") { return Done; }
    $runtime.fail();
  })(v);
};
const scanl = dictMonad => f => b => l => unfold(dictMonad)(v => {
  const $0 = v._1;
  return dictMonad.Bind1().Apply0().Functor0().map(v1 => {
    if (v1.tag === "Yield") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(Data$dTuple.$Tuple(f($0)(v1._1), Data$dLazy.force(v1._2)), $0)); }
    if (v1.tag === "Skip") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(Data$dTuple.$Tuple($0, Data$dLazy.force(v1._1)), $0)); }
    if (v1.tag === "Done") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  })(v._2);
})(Data$dTuple.$Tuple(b, l));
const prepend$p = dictApplicative => h => t => dictApplicative.pure($Step("Yield", h, t));
const prepend = dictApplicative => h => t => dictApplicative.pure($Step("Yield", h, Data$dLazy.defer(v => t)));
const nil = dictApplicative => dictApplicative.pure(Done);
const singleton = dictApplicative => {
  const nil1 = dictApplicative.pure(Done);
  return a => dictApplicative.pure($Step("Yield", a, Data$dLazy.defer(v => nil1)));
};
const take = dictApplicative => {
  const nil1 = dictApplicative.pure(Done);
  const $0 = dictApplicative.Apply0().Functor0();
  return v => v1 => {
    if (v === 0) { return nil1; }
    return $0.map(v2 => {
      if (v2.tag === "Yield") {
        const $1 = v2._2;
        return $Step(
          "Yield",
          v2._1,
          (() => {
            const $2 = take(dictApplicative)(v - 1 | 0);
            return Data$dLazy.defer(v$1 => $2(Data$dLazy.force($1)));
          })()
        );
      }
      if (v2.tag === "Skip") {
        const $1 = v2._1;
        return $Step(
          "Skip",
          (() => {
            const $2 = take(dictApplicative)(v);
            return Data$dLazy.defer(v$1 => $2(Data$dLazy.force($1)));
          })()
        );
      }
      if (v2.tag === "Done") { return Done; }
      $runtime.fail();
    })(v1);
  };
};
const zipWith$p = dictMonad => {
  const Applicative0 = dictMonad.Applicative0();
  const nil1 = Applicative0.pure(Done);
  const Bind1 = dictMonad.Bind1();
  const Functor0 = Bind1.Apply0().Functor0();
  const uncons1 = uncons(dictMonad);
  return f => fa => fb => Functor0.map(x => $Step("Skip", Data$dLazy.defer(v => x)))(Bind1.bind(uncons1(fa))(ua => Bind1.bind(uncons1(fb))(ub => {
    if (ub.tag === "Nothing") { return Applicative0.pure(nil1); }
    if (ua.tag === "Nothing") { return Applicative0.pure(nil1); }
    if (ua.tag === "Just" && ub.tag === "Just") {
      const $0 = ua._1._2;
      const $1 = ub._1._2;
      return Functor0.map((() => {
        const $2 = Data$dLazy.defer(v2 => zipWith$p(dictMonad)(f)($0)($1));
        return a => Applicative0.pure($Step("Yield", a, $2));
      })())(f(ua._1._1)(ub._1._1));
    }
    $runtime.fail();
  })));
};
const zipWith = dictMonad => {
  const zipWith$p1 = zipWith$p(dictMonad);
  return f => zipWith$p1(a => b => dictMonad.Applicative0().pure(f(a)(b)));
};
const newtypeListT = {Coercible0: () => {}};
const mapMaybe = dictFunctor => f => v => dictFunctor.map(v$1 => {
  if (v$1.tag === "Yield") {
    const $0 = f(v$1._1);
    if ($0.tag === "Just") {
      return $Step(
        "Yield",
        $0._1,
        (() => {
          const $1 = mapMaybe(dictFunctor)(f);
          return Data$dLazy.defer(v$2 => $1(Data$dLazy.force(v$1._2)));
        })()
      );
    }
    return $Step(
      "Skip",
      (() => {
        const $1 = mapMaybe(dictFunctor)(f);
        return Data$dLazy.defer(v$2 => $1(Data$dLazy.force(v$1._2)));
      })()
    );
  }
  if (v$1.tag === "Skip") {
    const $0 = v$1._1;
    return $Step(
      "Skip",
      (() => {
        const $1 = mapMaybe(dictFunctor)(f);
        return Data$dLazy.defer(v$2 => $1(Data$dLazy.force($0)));
      })()
    );
  }
  if (v$1.tag === "Done") { return Done; }
  $runtime.fail();
})(v);
const iterate = dictMonad => f => a => unfold(dictMonad)(x => dictMonad.Applicative0().pure(Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(f(x), x))))(a);
const repeat = dictMonad => iterate(dictMonad)(identity);
const head = dictMonad => {
  const uncons1 = uncons(dictMonad);
  return l => dictMonad.Bind1().Apply0().Functor0().map(v1 => {
    if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", v1._1._1); }
    return Data$dMaybe.Nothing;
  })(uncons1(l));
};
const functorListT = dictFunctor => (
  {
    map: f => v => dictFunctor.map(v$1 => {
      if (v$1.tag === "Yield") {
        const $0 = v$1._2;
        return $Step(
          "Yield",
          f(v$1._1),
          (() => {
            const $1 = functorListT(dictFunctor).map(f);
            return Data$dLazy.defer(v$2 => $1(Data$dLazy.force($0)));
          })()
        );
      }
      if (v$1.tag === "Skip") {
        const $0 = v$1._1;
        return $Step(
          "Skip",
          (() => {
            const $1 = functorListT(dictFunctor).map(f);
            return Data$dLazy.defer(v$2 => $1(Data$dLazy.force($0)));
          })()
        );
      }
      if (v$1.tag === "Done") { return Done; }
      $runtime.fail();
    })(v)
  }
);
const fromEffect = dictApplicative => {
  const nil1 = dictApplicative.pure(Done);
  return fa => dictApplicative.Apply0().Functor0().map((() => {
    const $0 = Data$dLazy.defer(v => nil1);
    return a => $Step("Yield", a, $0);
  })())(fa);
};
const monadTransListT = {lift: dictMonad => fromEffect(dictMonad.Applicative0())};
const foldlRec$p = dictMonadRec => {
  const Monad0 = dictMonadRec.Monad0();
  const $0 = Monad0.Applicative0();
  const $1 = Monad0.Bind1();
  const uncons1 = uncons(Monad0);
  return f => a => b => dictMonadRec.tailRecM(o => {
    const $2 = o.a;
    return $1.bind(uncons1(o.b))(v => {
      if (v.tag === "Nothing") { return $0.pure(Control$dMonad$dRec$dClass.$Step("Done", $2)); }
      if (v.tag === "Just") {
        const $3 = v._1._2;
        return $1.bind(f($2)(v._1._1))(b$p => $0.pure(Control$dMonad$dRec$dClass.$Step("Loop", {a: b$p, b: $3})));
      }
      $runtime.fail();
    });
  })({a, b});
};
const runListTRec = dictMonadRec => foldlRec$p(dictMonadRec)(v => v1 => dictMonadRec.Monad0().Applicative0().pure())();
const foldlRec = dictMonadRec => {
  const Monad0 = dictMonadRec.Monad0();
  const $0 = Monad0.Applicative0();
  const uncons1 = uncons(Monad0);
  return f => a => b => dictMonadRec.tailRecM(o => {
    const $1 = o.a;
    return Monad0.Bind1().bind(uncons1(o.b))(v => {
      if (v.tag === "Nothing") { return $0.pure(Control$dMonad$dRec$dClass.$Step("Done", $1)); }
      if (v.tag === "Just") { return $0.pure(Control$dMonad$dRec$dClass.$Step("Loop", {a: f($1)(v._1._1), b: v._1._2})); }
      $runtime.fail();
    });
  })({a, b});
};
const foldl$p = dictMonad => {
  const $0 = dictMonad.Bind1();
  const uncons1 = uncons(dictMonad);
  return f => {
    const loop = b => l => $0.bind(uncons1(l))(v => {
      if (v.tag === "Nothing") { return dictMonad.Applicative0().pure(b); }
      if (v.tag === "Just") {
        const $1 = v._1._2;
        return $0.bind(f(b)(v._1._1))(a => loop(a)($1));
      }
      $runtime.fail();
    });
    return loop;
  };
};
const runListT = dictMonad => foldl$p(dictMonad)(v => v1 => dictMonad.Applicative0().pure())();
const foldl = dictMonad => {
  const uncons1 = uncons(dictMonad);
  return f => {
    const loop = b => l => dictMonad.Bind1().bind(uncons1(l))(v => {
      if (v.tag === "Nothing") { return dictMonad.Applicative0().pure(b); }
      if (v.tag === "Just") { return loop(f(b)(v._1._1))(v._1._2); }
      $runtime.fail();
    });
    return loop;
  };
};
const filter = dictFunctor => f => v => dictFunctor.map(v$1 => {
  if (v$1.tag === "Yield") {
    const $0 = v$1._2;
    const $1 = filter(dictFunctor)(f);
    const s$p = Data$dLazy.defer(v$2 => $1(Data$dLazy.force($0)));
    if (f(v$1._1)) { return $Step("Yield", v$1._1, s$p); }
    return $Step("Skip", s$p);
  }
  if (v$1.tag === "Skip") {
    const $0 = v$1._1;
    return $Step(
      "Skip",
      (() => {
        const $1 = filter(dictFunctor)(f);
        return Data$dLazy.defer(v$2 => $1(Data$dLazy.force($0)));
      })()
    );
  }
  if (v$1.tag === "Done") { return Done; }
  $runtime.fail();
})(v);
const dropWhile = dictApplicative => {
  const $0 = dictApplicative.Apply0().Functor0();
  return f => v => $0.map(v$1 => {
    if (v$1.tag === "Yield") {
      if (f(v$1._1)) {
        return $Step(
          "Skip",
          (() => {
            const $1 = dropWhile(dictApplicative)(f);
            return Data$dLazy.defer(v$2 => $1(Data$dLazy.force(v$1._2)));
          })()
        );
      }
      return $Step("Yield", v$1._1, v$1._2);
    }
    if (v$1.tag === "Skip") {
      const $1 = v$1._1;
      return $Step(
        "Skip",
        (() => {
          const $2 = dropWhile(dictApplicative)(f);
          return Data$dLazy.defer(v$2 => $2(Data$dLazy.force($1)));
        })()
      );
    }
    if (v$1.tag === "Done") { return Done; }
    $runtime.fail();
  })(v);
};
const drop = dictApplicative => {
  const $0 = dictApplicative.Apply0().Functor0();
  return v => v1 => {
    if (v === 0) { return v1; }
    return $0.map(v2 => {
      if (v2.tag === "Yield") {
        const $1 = v2._2;
        return $Step(
          "Skip",
          (() => {
            const $2 = drop(dictApplicative)(v - 1 | 0);
            return Data$dLazy.defer(v$1 => $2(Data$dLazy.force($1)));
          })()
        );
      }
      if (v2.tag === "Skip") {
        const $1 = v2._1;
        return $Step(
          "Skip",
          (() => {
            const $2 = drop(dictApplicative)(v);
            return Data$dLazy.defer(v$1 => $2(Data$dLazy.force($1)));
          })()
        );
      }
      if (v2.tag === "Done") { return Done; }
      $runtime.fail();
    })(v1);
  };
};
const cons = dictApplicative => lh => t => dictApplicative.pure($Step("Yield", Data$dLazy.force(lh), t));
const unfoldable1ListT = dictMonad => {
  const Applicative0 = dictMonad.Applicative0();
  const singleton1 = singleton(Applicative0);
  return {
    unfoldr1: f => b => {
      const go = v => {
        if (v._2.tag === "Nothing") { return singleton1(v._1); }
        if (v._2.tag === "Just") {
          const $0 = v._1;
          const $1 = v._2._1;
          return Applicative0.pure($Step("Yield", Data$dLazy.force(Data$dLazy.defer(v$1 => $0)), Data$dLazy.defer(v1 => go(f($1)))));
        }
        $runtime.fail();
      };
      return go(f(b));
    }
  };
};
const unfoldableListT = dictMonad => {
  const Applicative0 = dictMonad.Applicative0();
  const nil1 = Applicative0.pure(Done);
  const unfoldable1ListT1 = unfoldable1ListT(dictMonad);
  return {
    unfoldr: f => b => {
      const go = v => {
        if (v.tag === "Nothing") { return nil1; }
        if (v.tag === "Just") {
          const $0 = v._1._1;
          const $1 = v._1._2;
          return Applicative0.pure($Step("Yield", Data$dLazy.force(Data$dLazy.defer(v$1 => $0)), Data$dLazy.defer(v1 => go(f($1)))));
        }
        $runtime.fail();
      };
      return go(f(b));
    },
    Unfoldable10: () => unfoldable1ListT1
  };
};
const semigroupListT = dictApplicative => ({append: concat(dictApplicative)});
const concat = dictApplicative => {
  const $0 = dictApplicative.Apply0().Functor0();
  return x => y => $0.map(v => {
    if (v.tag === "Yield") {
      const $1 = v._2;
      return $Step("Yield", v._1, Data$dLazy.defer(v$1 => concat(dictApplicative)(Data$dLazy.force($1))(y)));
    }
    if (v.tag === "Skip") {
      const $1 = v._1;
      return $Step("Skip", Data$dLazy.defer(v$1 => concat(dictApplicative)(Data$dLazy.force($1))(y)));
    }
    if (v.tag === "Done") { return $Step("Skip", Data$dLazy.defer(v$1 => y)); }
    $runtime.fail();
  })(x);
};
const monoidListT = dictApplicative => {
  const semigroupListT1 = {append: concat(dictApplicative)};
  return {mempty: dictApplicative.pure(Done), Semigroup0: () => semigroupListT1};
};
const catMaybes = dictFunctor => mapMaybe(dictFunctor)(identity);
const monadListT = dictMonad => ({Applicative0: () => applicativeListT(dictMonad), Bind1: () => bindListT(dictMonad)});
const bindListT = dictMonad => {
  const append = concat(dictMonad.Applicative0());
  const $0 = dictMonad.Bind1().Apply0().Functor0();
  return {
    bind: fa => f => $0.map(v => {
      if (v.tag === "Yield") {
        const $1 = v._1;
        const $2 = v._2;
        return $Step("Skip", Data$dLazy.defer(v$1 => append(f($1))(bindListT(dictMonad).bind(Data$dLazy.force($2))(f))));
      }
      if (v.tag === "Skip") {
        const $1 = v._1;
        return $Step("Skip", Data$dLazy.defer(v$1 => bindListT(dictMonad).bind(Data$dLazy.force($1))(f)));
      }
      if (v.tag === "Done") { return Done; }
      $runtime.fail();
    })(fa),
    Apply0: () => applyListT(dictMonad)
  };
};
const applyListT = dictMonad => {
  const functorListT1 = functorListT(dictMonad.Bind1().Apply0().Functor0());
  return {
    apply: (() => {
      const $0 = bindListT(dictMonad);
      return f => a => $0.bind(f)(f$p => $0.bind(a)(a$p => applicativeListT(dictMonad).pure(f$p(a$p))));
    })(),
    Functor0: () => functorListT1
  };
};
const applicativeListT = dictMonad => ({pure: singleton(dictMonad.Applicative0()), Apply0: () => applyListT(dictMonad)});
const monadEffectListT = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const monadListT1 = {Applicative0: () => ({pure: singleton(Monad0.Applicative0()), Apply0: () => applyListT(Monad0)}), Bind1: () => bindListT(Monad0)};
  return {
    liftEffect: (() => {
      const $0 = fromEffect(Monad0.Applicative0());
      return x => $0(dictMonadEffect.liftEffect(x));
    })(),
    Monad0: () => monadListT1
  };
};
const altListT = dictApplicative => {
  const functorListT1 = functorListT(dictApplicative.Apply0().Functor0());
  return {alt: concat(dictApplicative), Functor0: () => functorListT1};
};
const plusListT = dictMonad => {
  const Applicative0 = dictMonad.Applicative0();
  const altListT1 = altListT(Applicative0);
  return {empty: Applicative0.pure(Done), Alt0: () => altListT1};
};
const alternativeListT = dictMonad => {
  const applicativeListT1 = {pure: singleton(dictMonad.Applicative0()), Apply0: () => applyListT(dictMonad)};
  const plusListT1 = plusListT(dictMonad);
  return {Applicative0: () => applicativeListT1, Plus1: () => plusListT1};
};
const monadPlusListT = dictMonad => {
  const monadListT1 = {Applicative0: () => ({pure: singleton(dictMonad.Applicative0()), Apply0: () => applyListT(dictMonad)}), Bind1: () => bindListT(dictMonad)};
  const alternativeListT1 = alternativeListT(dictMonad);
  return {Monad0: () => monadListT1, Alternative1: () => alternativeListT1};
};
export {
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  fromEffect,
  
  
  
  
  
  monadEffectListT,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
};
