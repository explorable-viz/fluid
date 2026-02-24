import * as $runtime from "../runtime.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Graph from "../Graph/index.js";
import * as Util from "../Util/index.js";
import * as Util$dSet from "../Util.Set/index.js";
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
        go$a0 = Data$dMap$dInternal.insert(Graph.ordVertex)(v._1)()(b);
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(Data$dMap$dInternal.Leaf);
})();
const mempty = /* #__PURE__ */ (() => Data$dSet.monoidSet(Graph.ordDVertex$p).mempty)();
const difference = /* #__PURE__ */ (() => Util$dSet.setSet(Graph.ordVertex).difference)();
const monadWithGraphWithGraphT = dictMonad => {
  const $0 = dictMonad.Bind1().Apply0().Functor0();
  const $1 = Control$dMonad$dState$dTrans.monadStateStateT(dictMonad);
  const monadStateT = {Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)};
  return {
    extend: α => αs => {
      const $2 = Data$dList$dTypes.Cons(Data$dTuple.$Tuple(α, αs));
      const $3 = $1.state(s => Data$dTuple.$Tuple(undefined, $2(s)));
      return s => $0.map(v1 => Data$dTuple.$Tuple(undefined, v1._2))($3(s));
    },
    Monad0: () => monadStateT
  };
};
const monadAllocAllocT = dictMonad => {
  const monadStateT = {Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)};
  return {
    fresh: Control$dMonad$dState$dTrans.bindStateT(dictMonad).bind(Control$dMonad$dState$dTrans.monadStateStateT(dictMonad).state(s => {
      const s$p = 1 + s | 0;
      return Data$dTuple.$Tuple(s$p, s$p);
    }))(n => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad).pure(Data$dShow.showIntImpl(n))),
    Monad0: () => monadStateT
  };
};
const runAllocT = dictMonad => m => n => dictMonad.Bind1().bind(m(n))(v => dictMonad.Applicative0().pure(Data$dTuple.$Tuple(
  v._2,
  Data$dTuple.$Tuple(
    fromFoldable(Data$dList$dTypes.listMap(x => Data$dShow.showIntImpl(x))((() => {
      const $0 = n + 1 | 0;
      if (v._2 < $0) { return Data$dList$dTypes.Nil; }
      if ($0 === v._2) { return Data$dList$dTypes.$List("Cons", $0, Data$dList$dTypes.Nil); }
      const go = go$a0$copy => go$a1$copy => go$a2$copy => go$a3$copy => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$a3 = go$a3$copy, go$c = true, go$r;
        while (go$c) {
          const s = go$a0, e = go$a1, step = go$a2, rest = go$a3;
          if (s === e) {
            go$c = false;
            go$r = Data$dList$dTypes.$List("Cons", s, rest);
            continue;
          }
          go$a0 = s + step | 0;
          go$a1 = e;
          go$a2 = step;
          go$a3 = Data$dList$dTypes.$List("Cons", s, rest);
        }
        return go$r;
      };
      return go(v._2)($0)($0 > v._2 ? 1 : -1)(Data$dList$dTypes.Nil);
    })())),
    v._1
  )
)));
const runAlloc = m => x => runAllocT(Data$dIdentity.monadIdentity)(m)(x);
const $$new = dict => dict.new;
const fresh = dict => dict.fresh;
const monadAllocWithGraphAllocT = dictMonad => {
  const monadStateT = {Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)};
  const monadStateT1 = {Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(monadStateT), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(monadStateT)};
  return {
    fresh: (() => {
      const $0 = monadAllocAllocT(dictMonad).fresh;
      return s => monadStateT.Bind1().bind($0)(x => monadStateT.Applicative0().pure(Data$dTuple.$Tuple(x, s)));
    })(),
    Monad0: () => monadStateT1
  };
};
const freezeGraph = dictMonad => {
  const $0 = dictMonad.Bind1().Apply0().Functor0();
  return dictGraph => m => αs => {
    const $1 = dictGraph.fromEdgeList(αs);
    return $0.map(v => Data$dTuple.$Tuple($1(Util.spyWhen(false)("runWithGraphT edge list")(Graph.showEdgeList)(v._1)), v._2))($0.map(Data$dTuple.swap)(m(Data$dList$dTypes.Nil)));
  };
};
const runWithGraphT = dictMonad => {
  const freezeGraph1 = freezeGraph(dictMonad);
  return dictGraph => {
    const freezeGraph2 = freezeGraph1(dictGraph);
    return m => αs => dictMonad.Bind1().bind(freezeGraph2(m)(αs))(v => {
      const $0 = v._1;
      return Util.assertWhen(false)("edgeListGC")(v1 => dictGraph.Eq0().eq($0)(dictGraph.fromEdgeList(mempty)(Graph.toEdgeList(dictGraph)($0))))(dictMonad.Applicative0().pure(Data$dTuple.$Tuple(
        $0,
        v._2
      )));
    });
  };
};
const runWithGraphT1 = /* #__PURE__ */ runWithGraphT(Data$dIdentity.monadIdentity);
const runWithGraph = dictGraph => runWithGraphT1(dictGraph);
const runWithGraphT_spy = dictMonad => {
  const runWithGraphT2 = runWithGraphT(dictMonad);
  const spyFunWhenM = Util.spyFunWhenM(dictMonad.Bind1().Apply0().Functor0());
  return dictGraph => {
    const $0 = runWithGraphT2(dictGraph);
    const $1 = spyFunWhenM(false)("runWithGraphT")((() => {
      const $1 = Data$dSet.map(Graph.ordVertex)(x => x._1);
      return x => Graph.showVertices($1(x));
    })())(x => Graph.showEdgeList(Graph.toEdgeList(dictGraph)(x._1)));
    return x => $1($0(x));
  };
};
const runWithGraphT_spy1 = /* #__PURE__ */ runWithGraphT_spy(Data$dIdentity.monadIdentity);
const runWithGraph_spy = dictGraph => runWithGraphT_spy1(dictGraph);
const extend = dict => dict.extend;
const monadWithGraphAllocWithGr = dictMonadError => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  const monadStateT = {Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(Monad0), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(Monad0)};
  const bindStateT = Control$dMonad$dState$dTrans.bindStateT(monadStateT);
  const monadAllocWithGraphAllocT1 = monadAllocWithGraphAllocT(Monad0);
  const fresh1 = monadAllocWithGraphAllocT1.fresh;
  const monadWithGraphWithGraphT1 = monadWithGraphWithGraphT(monadStateT);
  const monadErrorStateT = Control$dMonad$dState$dTrans.monadErrorStateT(Control$dMonad$dState$dTrans.monadErrorStateT(dictMonadError));
  return {
    new: dictTypeName => constr => αs => vd => bindStateT.bind(fresh1)(α => {
      const v = constr(α)(vd);
      return bindStateT.bind(monadWithGraphWithGraphT1.extend(Data$dTuple.$Tuple(α, k => k(dictTypeName)(v)))(αs))(() => Control$dMonad$dState$dTrans.applicativeStateT(monadStateT).pure(v));
    }),
    MonadAlloc0: () => monadAllocWithGraphAllocT1,
    MonadError1: () => monadErrorStateT,
    MonadWithGraph2: () => monadWithGraphWithGraphT1
  };
};
const alloc_check = dictVertices => {
  const addresses = Graph.addresses(dictVertices);
  return dictMonadError => {
    const MonadThrow0 = dictMonadError.MonadThrow0();
    const Monad0 = MonadThrow0.Monad0();
    return msg => m => Monad0.Bind1().bind(runAllocT(Monad0)(m)(0))(v => Util.check(MonadThrow0)(Util.spyWhen(true)(Data$dShow.showIntImpl(v._1) + " allocations, unaccounted for")(Graph.showVertices)(difference(v._2._1)(addresses(v._2._2))).tag === "Leaf")("alloc " + msg + " round-trip"));
  };
};
const alloc = dictMonadAlloc => {
  const Applicative0 = dictMonadAlloc.Monad0().Applicative0();
  const fresh1 = dictMonadAlloc.fresh;
  return dictTraversable => dictTraversable.traverse(Applicative0)(v => fresh1);
};
export {
  
  
  
  
  
  
  
  
  monadAllocAllocT,
  
  monadWithGraphAllocWithGr,
  monadWithGraphWithGraphT,
  
  
  runAllocT,
  
  
  
  runWithGraphT_spy,
  runWithGraphT_spy1,
  
};
