import * as $runtime from "../runtime.js";
import * as Control$dMonad from "../Control.Monad/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Effect from "../Effect/index.js";
const $Step = (tag, _1) => ({tag, _1});
const Loop = value0 => $Step("Loop", value0);
const Done = value0 => $Step("Done", value0);
const tailRecM = dict => dict.tailRecM;
const tailRecM2 = dictMonadRec => f => a => b => dictMonadRec.tailRecM(o => f(o.a)(o.b))({a, b});
const tailRecM3 = dictMonadRec => f => a => b => c => dictMonadRec.tailRecM(o => f(o.a)(o.b)(o.c))({a, b, c});
const untilJust = dictMonadRec => {
  const $0 = dictMonadRec.Monad0().Bind1().Apply0().Functor0();
  return m => dictMonadRec.tailRecM(v => $0.map(v1 => {
    if (v1.tag === "Nothing") { return $Step("Loop", undefined); }
    if (v1.tag === "Just") { return $Step("Done", v1._1); }
    $runtime.fail();
  })(m))();
};
const whileJust = dictMonoid => {
  const mempty = dictMonoid.mempty;
  return dictMonadRec => {
    const $0 = dictMonadRec.Monad0().Bind1().Apply0().Functor0();
    return m => dictMonadRec.tailRecM(v => $0.map(v1 => {
      if (v1.tag === "Nothing") { return $Step("Done", v); }
      if (v1.tag === "Just") { return $Step("Loop", dictMonoid.Semigroup0().append(v)(v1._1)); }
      $runtime.fail();
    })(m))(mempty);
  };
};
const tailRec = f => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Loop") {
        go$a0 = f(v._1);
        continue;
      }
      if (v.tag === "Done") {
        go$c = false;
        go$r = v._1;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return x => go(f(x));
};
const tailRec2 = f => a => b => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Loop") {
        go$a0 = f(v._1.a)(v._1.b);
        continue;
      }
      if (v.tag === "Done") {
        go$c = false;
        go$r = v._1;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(f(a)(b));
};
const tailRec3 = f => a => b => c => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Loop") {
        go$a0 = f(v._1.a)(v._1.b)(v._1.c);
        continue;
      }
      if (v.tag === "Done") {
        go$c = false;
        go$r = v._1;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(f(a)(b)(c));
};
const monadRecMaybe = {
  tailRecM: f => a0 => {
    const $0 = v => {
      if (v.tag === "Nothing") { return $Step("Done", Data$dMaybe.Nothing); }
      if (v.tag === "Just") {
        if (v._1.tag === "Loop") { return $Step("Loop", f(v._1._1)); }
        if (v._1.tag === "Done") { return $Step("Done", Data$dMaybe.$Maybe("Just", v._1._1)); }
      }
      $runtime.fail();
    };
    const go = go$a0$copy => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v.tag === "Loop") {
          go$a0 = $0(v._1);
          continue;
        }
        if (v.tag === "Done") {
          go$c = false;
          go$r = v._1;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go($0(f(a0)));
  },
  Monad0: () => Data$dMaybe.monadMaybe
};
const monadRecIdentity = {
  tailRecM: f => {
    const go = go$a0$copy => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v.tag === "Loop") {
          go$a0 = f(v._1);
          continue;
        }
        if (v.tag === "Done") {
          go$c = false;
          go$r = v._1;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return x => go(f(x));
  },
  Monad0: () => Data$dIdentity.monadIdentity
};
const monadRecFunction = {
  tailRecM: f => a0 => e => {
    const go = go$a0$copy => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v.tag === "Loop") {
          go$a0 = f(v._1)(e);
          continue;
        }
        if (v.tag === "Done") {
          go$c = false;
          go$r = v._1;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go(f(a0)(e));
  },
  Monad0: () => Control$dMonad.monadFn
};
const monadRecEither = {
  tailRecM: f => a0 => {
    const $0 = v => {
      if (v.tag === "Left") { return $Step("Done", Data$dEither.$Either("Left", v._1)); }
      if (v.tag === "Right") {
        if (v._1.tag === "Loop") { return $Step("Loop", f(v._1._1)); }
        if (v._1.tag === "Done") { return $Step("Done", Data$dEither.$Either("Right", v._1._1)); }
      }
      $runtime.fail();
    };
    const go = go$a0$copy => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v.tag === "Loop") {
          go$a0 = $0(v._1);
          continue;
        }
        if (v.tag === "Done") {
          go$c = false;
          go$r = v._1;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go($0(f(a0)));
  },
  Monad0: () => Data$dEither.monadEither
};
const monadRecEffect = {
  tailRecM: f => a => {
    const $0 = f(a);
    return () => {
      const $1 = $0();
      let r = $1;
      Effect.untilE(() => {
        const v = r;
        if (v.tag === "Loop") {
          const e = f(v._1)();
          r = e;
          return false;
        }
        if (v.tag === "Done") { return true; }
        $runtime.fail();
      })();
      const a$p = r;
      if (a$p.tag === "Done") { return a$p._1; }
      $runtime.fail();
    };
  },
  Monad0: () => Effect.monadEffect
};
const loop3 = a => b => c => $Step("Loop", {a, b, c});
const loop2 = a => b => $Step("Loop", {a, b});
const functorStep = {
  map: f => m => {
    if (m.tag === "Loop") { return $Step("Loop", m._1); }
    if (m.tag === "Done") { return $Step("Done", f(m._1)); }
    $runtime.fail();
  }
};
const forever = dictMonadRec => {
  const $0 = dictMonadRec.Monad0().Bind1().Apply0().Functor0();
  return ma => dictMonadRec.tailRecM(u => $0.map(v => $Step("Loop", u))(ma))();
};
const bifunctorStep = {
  bimap: v => v1 => v2 => {
    if (v2.tag === "Loop") { return $Step("Loop", v(v2._1)); }
    if (v2.tag === "Done") { return $Step("Done", v1(v2._1)); }
    $runtime.fail();
  }
};
export {
  $Step,
  
  Loop,
  
  
  
  
  
  
  
  
  monadRecIdentity,
  
  
  
  
  
  
  
  
  
};
