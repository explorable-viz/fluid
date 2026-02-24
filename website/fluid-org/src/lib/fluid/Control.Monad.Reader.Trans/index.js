// | This module defines the reader monad transformer, `ReaderT`.
const ReaderT = x => x;
const withReaderT = f => v => x => v(f(x));
const runReaderT = v => v;
const newtypeReaderT = {Coercible0: () => {}};
const monadTransReaderT = {lift: dictMonad => x => v => x};
const mapReaderT = f => v => x => f(v(x));
const functorReaderT = dictFunctor => (
  {
    map: x => {
      const $0 = dictFunctor.map(x);
      return v => x$1 => $0(v(x$1));
    }
  }
);
const distributiveReaderT = dictDistributive => {
  const $0 = dictDistributive.Functor0();
  const functorReaderT1 = {
    map: x => {
      const $1 = $0.map(x);
      return v => x$1 => $1(v(x$1));
    }
  };
  return {
    distribute: dictFunctor => {
      const collect1 = dictDistributive.collect(dictFunctor);
      return a => e => collect1(r => r(e))(a);
    },
    collect: dictFunctor => f => {
      const $1 = distributiveReaderT(dictDistributive).distribute(dictFunctor);
      const $2 = dictFunctor.map(f);
      return x => $1($2(x));
    },
    Functor0: () => functorReaderT1
  };
};
const applyReaderT = dictApply => {
  const $0 = dictApply.Functor0();
  const functorReaderT1 = {
    map: x => {
      const $1 = $0.map(x);
      return v => x$1 => $1(v(x$1));
    }
  };
  return {apply: v => v1 => r => dictApply.apply(v(r))(v1(r)), Functor0: () => functorReaderT1};
};
const bindReaderT = dictBind => {
  const $0 = dictBind.Apply0();
  const $1 = $0.Functor0();
  const applyReaderT1 = (() => {
    const functorReaderT1 = {
      map: x => {
        const $2 = $1.map(x);
        return v => x$1 => $2(v(x$1));
      }
    };
    return {apply: v => v1 => r => $0.apply(v(r))(v1(r)), Functor0: () => functorReaderT1};
  })();
  return {bind: v => k => r => dictBind.bind(v(r))(a => k(a)(r)), Apply0: () => applyReaderT1};
};
const semigroupReaderT = dictApply => {
  const $0 = dictApply.Functor0();
  return dictSemigroup => (
    {
      append: (() => {
        const $1 = dictSemigroup.append;
        return a => b => {
          const $2 = $0.map($1);
          return r => dictApply.apply($2(a(r)))(b(r));
        };
      })()
    }
  );
};
const applicativeReaderT = dictApplicative => {
  const $0 = dictApplicative.Apply0();
  const $1 = $0.Functor0();
  const functorReaderT1 = {
    map: x => {
      const $2 = $1.map(x);
      return v => x$1 => $2(v(x$1));
    }
  };
  const applyReaderT1 = {apply: v => v1 => r => $0.apply(v(r))(v1(r)), Functor0: () => functorReaderT1};
  return {
    pure: x => {
      const $2 = dictApplicative.pure(x);
      return v => $2;
    },
    Apply0: () => applyReaderT1
  };
};
const monadReaderT = dictMonad => {
  const $0 = dictMonad.Applicative0();
  const $1 = $0.Apply0();
  const applicativeReaderT1 = (() => {
    const $2 = $1.Functor0();
    const functorReaderT1 = {
      map: x => {
        const $3 = $2.map(x);
        return v => x$1 => $3(v(x$1));
      }
    };
    const applyReaderT1 = {apply: v => v1 => r => $1.apply(v(r))(v1(r)), Functor0: () => functorReaderT1};
    return {
      pure: x => {
        const $3 = $0.pure(x);
        return v => $3;
      },
      Apply0: () => applyReaderT1
    };
  })();
  const bindReaderT1 = bindReaderT(dictMonad.Bind1());
  return {Applicative0: () => applicativeReaderT1, Bind1: () => bindReaderT1};
};
const monadAskReaderT = dictMonad => {
  const monadReaderT1 = monadReaderT(dictMonad);
  return {ask: dictMonad.Applicative0().pure, Monad0: () => monadReaderT1};
};
const monadReaderReaderT = dictMonad => {
  const monadReaderT1 = monadReaderT(dictMonad);
  const monadAskReaderT1 = {ask: dictMonad.Applicative0().pure, Monad0: () => monadReaderT1};
  return {local: withReaderT, MonadAsk0: () => monadAskReaderT1};
};
const monadContReaderT = dictMonadCont => {
  const monadReaderT1 = monadReaderT(dictMonadCont.Monad0());
  return {
    callCC: f => r => dictMonadCont.callCC(c => f(x => {
      const $0 = c(x);
      return v => $0;
    })(r)),
    Monad0: () => monadReaderT1
  };
};
const monadEffectReader = dictMonadEffect => {
  const monadReaderT1 = monadReaderT(dictMonadEffect.Monad0());
  return {
    liftEffect: x => {
      const $0 = dictMonadEffect.liftEffect(x);
      return v => $0;
    },
    Monad0: () => monadReaderT1
  };
};
const monadRecReaderT = dictMonadRec => {
  const Monad0 = dictMonadRec.Monad0();
  const $0 = Monad0.Bind1();
  const pure = Monad0.Applicative0().pure;
  const monadReaderT1 = monadReaderT(Monad0);
  return {tailRecM: k => a => r => dictMonadRec.tailRecM(a$p => $0.bind(k(a$p)(r))(pure))(a), Monad0: () => monadReaderT1};
};
const monadStateReaderT = dictMonadState => {
  const monadReaderT1 = monadReaderT(dictMonadState.Monad0());
  return {
    state: x => {
      const $0 = dictMonadState.state(x);
      return v => $0;
    },
    Monad0: () => monadReaderT1
  };
};
const monadTellReaderT = dictMonadTell => {
  const Semigroup0 = dictMonadTell.Semigroup0();
  const monadReaderT1 = monadReaderT(dictMonadTell.Monad1());
  return {
    tell: x => {
      const $0 = dictMonadTell.tell(x);
      return v => $0;
    },
    Semigroup0: () => Semigroup0,
    Monad1: () => monadReaderT1
  };
};
const monadWriterReaderT = dictMonadWriter => {
  const Monoid0 = dictMonadWriter.Monoid0();
  const monadTellReaderT1 = monadTellReaderT(dictMonadWriter.MonadTell1());
  return {listen: v => x => dictMonadWriter.listen(v(x)), pass: v => x => dictMonadWriter.pass(v(x)), Monoid0: () => Monoid0, MonadTell1: () => monadTellReaderT1};
};
const monadThrowReaderT = dictMonadThrow => {
  const monadReaderT1 = monadReaderT(dictMonadThrow.Monad0());
  return {
    throwError: x => {
      const $0 = dictMonadThrow.throwError(x);
      return v => $0;
    },
    Monad0: () => monadReaderT1
  };
};
const monadErrorReaderT = dictMonadError => {
  const monadThrowReaderT1 = monadThrowReaderT(dictMonadError.MonadThrow0());
  return {catchError: v => h => r => dictMonadError.catchError(v(r))(e => h(e)(r)), MonadThrow0: () => monadThrowReaderT1};
};
const monoidReaderT = dictApplicative => {
  const $0 = dictApplicative.Apply0();
  const $1 = $0.Functor0();
  return dictMonoid => {
    const semigroupReaderT2 = {
      append: (() => {
        const $2 = dictMonoid.Semigroup0().append;
        return a => b => {
          const $3 = $1.map($2);
          return r => $0.apply($3(a(r)))(b(r));
        };
      })()
    };
    return {
      mempty: (() => {
        const $2 = dictApplicative.pure(dictMonoid.mempty);
        return v => $2;
      })(),
      Semigroup0: () => semigroupReaderT2
    };
  };
};
const altReaderT = dictAlt => {
  const $0 = dictAlt.Functor0();
  const functorReaderT1 = {
    map: x => {
      const $1 = $0.map(x);
      return v => x$1 => $1(v(x$1));
    }
  };
  return {alt: v => v1 => r => dictAlt.alt(v(r))(v1(r)), Functor0: () => functorReaderT1};
};
const plusReaderT = dictPlus => {
  const $0 = dictPlus.Alt0();
  const $1 = $0.Functor0();
  const altReaderT1 = (() => {
    const functorReaderT1 = {
      map: x => {
        const $2 = $1.map(x);
        return v => x$1 => $2(v(x$1));
      }
    };
    return {alt: v => v1 => r => $0.alt(v(r))(v1(r)), Functor0: () => functorReaderT1};
  })();
  return {
    empty: (() => {
      const $2 = dictPlus.empty;
      return v => $2;
    })(),
    Alt0: () => altReaderT1
  };
};
const alternativeReaderT = dictAlternative => {
  const $0 = dictAlternative.Applicative0();
  const $1 = $0.Apply0();
  const applicativeReaderT1 = (() => {
    const $2 = $1.Functor0();
    const functorReaderT1 = {
      map: x => {
        const $3 = $2.map(x);
        return v => x$1 => $3(v(x$1));
      }
    };
    const applyReaderT1 = {apply: v => v1 => r => $1.apply(v(r))(v1(r)), Functor0: () => functorReaderT1};
    return {
      pure: x => {
        const $3 = $0.pure(x);
        return v => $3;
      },
      Apply0: () => applyReaderT1
    };
  })();
  const $2 = dictAlternative.Plus1();
  const $3 = $2.Alt0();
  const plusReaderT1 = (() => {
    const $4 = $3.Functor0();
    const functorReaderT1 = {
      map: x => {
        const $5 = $4.map(x);
        return v => x$1 => $5(v(x$1));
      }
    };
    const altReaderT1 = {alt: v => v1 => r => $3.alt(v(r))(v1(r)), Functor0: () => functorReaderT1};
    return {
      empty: (() => {
        const $5 = $2.empty;
        return v => $5;
      })(),
      Alt0: () => altReaderT1
    };
  })();
  return {Applicative0: () => applicativeReaderT1, Plus1: () => plusReaderT1};
};
const monadPlusReaderT = dictMonadPlus => {
  const monadReaderT1 = monadReaderT(dictMonadPlus.Monad0());
  const alternativeReaderT1 = alternativeReaderT(dictMonadPlus.Alternative1());
  return {Monad0: () => monadReaderT1, Alternative1: () => alternativeReaderT1};
};
export {
  
  
  
  
  
  
  
  
  
  
  
  monadEffectReader,
  monadErrorReaderT,
  
  monadReaderReaderT,
  
  
  
  
  
  
  
  
  
  
  
  
  
};
