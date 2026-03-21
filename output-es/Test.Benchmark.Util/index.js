import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMonoid from "../Data.Monoid/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import {microtime} from "./foreign.js";
const fold = /* #__PURE__ */ (() => Data$dFoldable.foldableArray.foldMap(Data$dMonoid.monoidString)(Data$dFoldable.identity))();
const show = /* #__PURE__ */ (() => Data$dList$dTypes.showList(Data$dShow.showNumber).show)();
const BenchRow = x => x;
const BenchAcc = x => x;
const showBenchAcc = {
  show: v => fold(Data$dArray.intersperse("\n")([
    fold(Data$dArray.intersperse(",")([
      "Test-Name",
      ...(() => {
        const go = v$1 => {
          if (v$1.tag === "Leaf") { return Data$dMap$dInternal.Leaf; }
          if (v$1.tag === "Node") { return Data$dMap$dInternal.$$$Map("Node", v$1._1, v$1._2, v$1._3, undefined, go(v$1._5), go(v$1._6)); }
          $runtime.fail();
        };
        return Data$dArray.fromFoldableImpl(
          Data$dSet.foldableSet.foldr,
          go((() => {
            if (0 < v.length) { return v[0]._2; }
            $runtime.fail();
          })())
        );
      })()
    ])),
    ...Data$dFunctor.arrayMap(v2 => fold(Data$dArray.intersperse(",")([
      v2._1,
      ...Data$dFunctor.arrayMap(show)((() => {
        const go = (m$p, z$p) => {
          if (m$p.tag === "Leaf") { return z$p; }
          if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._4, go(m$p._6, z$p))); }
          $runtime.fail();
        };
        return Data$dArray.fromFoldableImpl(Data$dList$dTypes.foldableList.foldr, go(v2._2, Data$dList$dTypes.Nil));
      })())
    ])))(v)
  ]))
};
const newtypeBenchRow_ = {Coercible0: () => {}};
const semigroupBenchRow = {append: m1 => m2 => Data$dMap$dInternal.unsafeUnionWith(Data$dOrd.ordString.compare, Data$dList.union(Data$dEq.eqNumber), m1, m2)};
const monoidBenchRow = {mempty: Data$dMap$dInternal.Leaf, Semigroup0: () => semigroupBenchRow};
const stdDev = ns$p => Data$dNumber.sqrt((() => {
  const $0 = Data$dList$dTypes.listMap(x => Data$dNumber.pow((() => {
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
          go$a0 = b + v._1;
          go$a1 = v._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return x - go(0.0)(ns$p) / Data$dInt.toNumber((() => {
      const go$1 = go$1$a0$copy => go$1$a1$copy => {
        let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
        while (go$1$c) {
          const b = go$1$a0, v = go$1$a1;
          if (v.tag === "Nil") {
            go$1$c = false;
            go$1$r = b;
            continue;
          }
          if (v.tag === "Cons") {
            go$1$a0 = b + 1 | 0;
            go$1$a1 = v._2;
            continue;
          }
          $runtime.fail();
        }
        return go$1$r;
      };
      return go$1(0)(ns$p);
    })());
  })())(2.0))(ns$p);
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
        go$a0 = b + v._1;
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(0.0)($0) / Data$dInt.toNumber((() => {
    const go$1 = go$1$a0$copy => go$1$a1$copy => {
      let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
      while (go$1$c) {
        const b = go$1$a0, v = go$1$a1;
        if (v.tag === "Nil") {
          go$1$c = false;
          go$1$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$1$a0 = b + 1 | 0;
          go$1$a1 = v._2;
          continue;
        }
        $runtime.fail();
      }
      return go$1$r;
    };
    return go$1(0)($0);
  })());
})());
const stdErr = nums => stdDev(nums) / Data$dNumber.sqrt(Data$dInt.toNumber((() => {
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
        go$a0 = b + 1 | 0;
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(0)(nums);
})()));
const recordGraphSize = dictGraph => dictMonadWriter => g => dictMonadWriter.MonadTell1().tell(Data$dMap$dInternal.$$$Map(
  "Node",
  1,
  1,
  "Graph-Nodes",
  Data$dList$dTypes.$List("Cons", Data$dInt.toNumber(dictGraph.size(g)), Data$dList$dTypes.Nil),
  Data$dMap$dInternal.Leaf,
  Data$dMap$dInternal.Leaf
));
const microtime$p = dictMonadEffect => dictMonadEffect.liftEffect(microtime);
const time = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const $0 = Monad0.Bind1();
  const microtime$p1 = dictMonadEffect.liftEffect(microtime);
  return m => $0.bind(microtime$p1)(t1 => $0.bind(m())(x => $0.bind(microtime$p1)(t2 => Monad0.Applicative0().pure(Data$dTuple.$Tuple(t2 - t1, x)))));
};
const logAs = dictMonadEffect => tag => s => dictMonadEffect.liftEffect(Effect$dConsole.log(tag + ": " + s));
const logTimeWhen = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const Bind1 = Monad0.Bind1();
  const time1 = time(dictMonadEffect);
  return v => v1 => v2 => {
    if (!v) { return v2(); }
    if (v) {
      return Bind1.bind(time1(v2))(v3 => {
        const $0 = v3._2;
        return Bind1.bind(dictMonadEffect.liftEffect(Effect$dConsole.log(v1 + ": " + Data$dShow.showNumberImpl(v3._1))))(() => Monad0.Applicative0().pure($0));
      });
    }
    $runtime.fail();
  };
};
const divRow = v => n => {
  const go = v$1 => {
    if (v$1.tag === "Leaf") { return Data$dMap$dInternal.Leaf; }
    if (v$1.tag === "Node") {
      return Data$dMap$dInternal.$$$Map(
        "Node",
        v$1._1,
        v$1._2,
        v$1._3,
        Data$dList$dTypes.$List(
          "Cons",
          (() => {
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
                  go$1$a0 = b + v$2._1;
                  go$1$a1 = v$2._2;
                  continue;
                }
                $runtime.fail();
              }
              return go$1$r;
            };
            return go$1(0.0)(v$1._4) / Data$dInt.toNumber(n);
          })(),
          Data$dList$dTypes.$List("Cons", stdErr(v$1._4), Data$dList$dTypes.Nil)
        ),
        go(v$1._5),
        go(v$1._6)
      );
    }
    $runtime.fail();
  };
  return go(v);
};
const benchmark$p = dictMonadWriter => name => m => dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const Bind1 = Monad0.Bind1();
  const Applicative0 = Monad0.Applicative0();
  const time1 = time(dictMonadEffect);
  return dictMonadError => Bind1.bind(Applicative0.pure())(() => Bind1.bind(time1(m))(v => {
    const $0 = v._2;
    return Bind1.bind(dictMonadWriter.MonadTell1().tell(Data$dMap$dInternal.$$$Map(
      "Node",
      1,
      1,
      name,
      Data$dList$dTypes.$List("Cons", v._1, Data$dList$dTypes.Nil),
      Data$dMap$dInternal.Leaf,
      Data$dMap$dInternal.Leaf
    )))(() => Applicative0.pure($0));
  }));
};
const benchmark = dictMonadWriter => name => benchmark$p(dictMonadWriter)(name);
export {
  BenchAcc,
  BenchRow,
  benchmark,
  benchmark$p,
  divRow,
  fold,
  logAs,
  logTimeWhen,
  microtime$p,
  monoidBenchRow,
  newtypeBenchRow_,
  recordGraphSize,
  semigroupBenchRow,
  show,
  showBenchAcc,
  stdDev,
  stdErr,
  time
};
export * from "./foreign.js";
