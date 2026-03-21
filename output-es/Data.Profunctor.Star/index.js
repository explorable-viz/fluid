import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const Star = x => x;
const semigroupoidStar = dictBind => ({compose: v => v1 => x => dictBind.bind(v1(x))(v)});
const profunctorStar = dictFunctor => (
  {
    dimap: f => g => v => {
      const $0 = dictFunctor.map(g);
      return x => $0(v(f(x)));
    }
  }
);
const strongStar = dictFunctor => {
  const profunctorStar1 = {
    dimap: f => g => v => {
      const $0 = dictFunctor.map(g);
      return x => $0(v(f(x)));
    }
  };
  return {
    first: v => v1 => {
      const $0 = v1._2;
      return dictFunctor.map(v2 => Data$dTuple.$Tuple(v2, $0))(v(v1._1));
    },
    second: v => v1 => dictFunctor.map(Data$dTuple.Tuple(v1._1))(v(v1._2)),
    Profunctor0: () => profunctorStar1
  };
};
const newtypeStar = {Coercible0: () => {}};
const invariantStar = dictInvariant => (
  {
    imap: f => g => v => {
      const $0 = dictInvariant.imap(f)(g);
      return x => $0(v(x));
    }
  }
);
const hoistStar = f => v => x => f(v(x));
const functorStar = dictFunctor => (
  {
    map: f => v => {
      const $0 = dictFunctor.map(f);
      return x => $0(v(x));
    }
  }
);
const distributiveStar = dictDistributive => {
  const $0 = dictDistributive.Functor0();
  const functorStar1 = {
    map: f => v => {
      const $1 = $0.map(f);
      return x => $1(v(x));
    }
  };
  return {
    distribute: dictFunctor => {
      const collect1 = dictDistributive.collect(dictFunctor);
      return f => a => collect1(v => v(a))(f);
    },
    collect: dictFunctor => f => {
      const $1 = distributiveStar(dictDistributive).distribute(dictFunctor);
      const $2 = dictFunctor.map(f);
      return x => $1($2(x));
    },
    Functor0: () => functorStar1
  };
};
const closedStar = dictDistributive => {
  const distribute = dictDistributive.distribute(Data$dFunctor.functorFn);
  const $0 = dictDistributive.Functor0();
  const profunctorStar1 = {
    dimap: f => g => v => {
      const $1 = $0.map(g);
      return x => $1(v(f(x)));
    }
  };
  return {closed: v => g => distribute(x => v(g(x))), Profunctor0: () => profunctorStar1};
};
const choiceStar = dictApplicative => {
  const Functor0 = dictApplicative.Apply0().Functor0();
  const profunctorStar1 = {
    dimap: f => g => v => {
      const $0 = Functor0.map(g);
      return x => $0(v(f(x)));
    }
  };
  return {
    left: v => {
      const $0 = Functor0.map(Data$dEither.Left);
      return v2 => {
        if (v2.tag === "Left") { return $0(v(v2._1)); }
        if (v2.tag === "Right") { return dictApplicative.pure(Data$dEither.$Either("Right", v2._1)); }
        $runtime.fail();
      };
    },
    right: v => {
      const $0 = Functor0.map(Data$dEither.Right);
      return v2 => {
        if (v2.tag === "Left") { return dictApplicative.pure(Data$dEither.$Either("Left", v2._1)); }
        if (v2.tag === "Right") { return $0(v(v2._1)); }
        $runtime.fail();
      };
    },
    Profunctor0: () => profunctorStar1
  };
};
const categoryStar = dictMonad => {
  const $0 = dictMonad.Bind1();
  const semigroupoidStar1 = {compose: v => v1 => x => $0.bind(v1(x))(v)};
  return {identity: dictMonad.Applicative0().pure, Semigroupoid0: () => semigroupoidStar1};
};
const applyStar = dictApply => {
  const $0 = dictApply.Functor0();
  const functorStar1 = {
    map: f => v => {
      const $1 = $0.map(f);
      return x => $1(v(x));
    }
  };
  return {apply: v => v1 => a => dictApply.apply(v(a))(v1(a)), Functor0: () => functorStar1};
};
const bindStar = dictBind => {
  const $0 = dictBind.Apply0();
  const $1 = $0.Functor0();
  const applyStar1 = (() => {
    const functorStar1 = {
      map: f => v => {
        const $2 = $1.map(f);
        return x => $2(v(x));
      }
    };
    return {apply: v => v1 => a => $0.apply(v(a))(v1(a)), Functor0: () => functorStar1};
  })();
  return {bind: v => f => x => dictBind.bind(v(x))(a => f(a)(x)), Apply0: () => applyStar1};
};
const applicativeStar = dictApplicative => {
  const $0 = dictApplicative.Apply0();
  const $1 = $0.Functor0();
  const applyStar1 = (() => {
    const functorStar1 = {
      map: f => v => {
        const $2 = $1.map(f);
        return x => $2(v(x));
      }
    };
    return {apply: v => v1 => a => $0.apply(v(a))(v1(a)), Functor0: () => functorStar1};
  })();
  return {pure: a => v => dictApplicative.pure(a), Apply0: () => applyStar1};
};
const monadStar = dictMonad => {
  const $0 = dictMonad.Applicative0();
  const $1 = $0.Apply0();
  const applicativeStar1 = (() => {
    const $2 = $1.Functor0();
    const functorStar1 = {
      map: f => v => {
        const $3 = $2.map(f);
        return x => $3(v(x));
      }
    };
    const applyStar1 = {apply: v => v1 => a => $1.apply(v(a))(v1(a)), Functor0: () => functorStar1};
    return {pure: a => v => $0.pure(a), Apply0: () => applyStar1};
  })();
  const bindStar1 = bindStar(dictMonad.Bind1());
  return {Applicative0: () => applicativeStar1, Bind1: () => bindStar1};
};
const altStar = dictAlt => {
  const $0 = dictAlt.Functor0();
  const functorStar1 = {
    map: f => v => {
      const $1 = $0.map(f);
      return x => $1(v(x));
    }
  };
  return {alt: v => v1 => a => dictAlt.alt(v(a))(v1(a)), Functor0: () => functorStar1};
};
const plusStar = dictPlus => {
  const empty = dictPlus.empty;
  const $0 = dictPlus.Alt0();
  const $1 = $0.Functor0();
  const altStar1 = (() => {
    const functorStar1 = {
      map: f => v => {
        const $2 = $1.map(f);
        return x => $2(v(x));
      }
    };
    return {alt: v => v1 => a => $0.alt(v(a))(v1(a)), Functor0: () => functorStar1};
  })();
  return {empty: v => empty, Alt0: () => altStar1};
};
const alternativeStar = dictAlternative => {
  const $0 = dictAlternative.Applicative0();
  const $1 = $0.Apply0();
  const applicativeStar1 = (() => {
    const $2 = $1.Functor0();
    const functorStar1 = {
      map: f => v => {
        const $3 = $2.map(f);
        return x => $3(v(x));
      }
    };
    const applyStar1 = {apply: v => v1 => a => $1.apply(v(a))(v1(a)), Functor0: () => functorStar1};
    return {pure: a => v => $0.pure(a), Apply0: () => applyStar1};
  })();
  const $2 = dictAlternative.Plus1();
  const empty = $2.empty;
  const plusStar1 = (() => {
    const $3 = $2.Alt0();
    const $4 = $3.Functor0();
    const functorStar1 = {
      map: f => v => {
        const $5 = $4.map(f);
        return x => $5(v(x));
      }
    };
    const altStar1 = {alt: v => v1 => a => $3.alt(v(a))(v1(a)), Functor0: () => functorStar1};
    return {empty: v => empty, Alt0: () => altStar1};
  })();
  return {Applicative0: () => applicativeStar1, Plus1: () => plusStar1};
};
const monadPlusStar = dictMonadPlus => {
  const monadStar1 = monadStar(dictMonadPlus.Monad0());
  const alternativeStar1 = alternativeStar(dictMonadPlus.Alternative1());
  return {Monad0: () => monadStar1, Alternative1: () => alternativeStar1};
};
export {
  Star,
  altStar,
  alternativeStar,
  applicativeStar,
  applyStar,
  bindStar,
  categoryStar,
  choiceStar,
  closedStar,
  distributiveStar,
  functorStar,
  hoistStar,
  invariantStar,
  monadPlusStar,
  monadStar,
  newtypeStar,
  plusStar,
  profunctorStar,
  semigroupoidStar,
  strongStar
};
