import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
const identity = x => x;
const distributiveIdentity = {
  distribute: dictFunctor => dictFunctor.map(Unsafe$dCoerce.unsafeCoerce),
  collect: dictFunctor => f => dictFunctor.map(x => f(x)),
  Functor0: () => Data$dIdentity.functorIdentity
};
const distribute = dict => dict.distribute;
const distributiveFunction = {
  distribute: dictFunctor => a => e => dictFunctor.map(v => v(e))(a),
  collect: dictFunctor => f => {
    const $0 = distributiveFunction.distribute(dictFunctor);
    const $1 = dictFunctor.map(f);
    return x => $0($1(x));
  },
  Functor0: () => Data$dFunctor.functorFn
};
const cotraverse = dictDistributive => dictFunctor => {
  const distribute2 = dictDistributive.distribute(dictFunctor);
  return f => {
    const $0 = dictDistributive.Functor0().map(f);
    return x => $0(distribute2(x));
  };
};
const collectDefault = dictDistributive => dictFunctor => {
  const distribute2 = dictDistributive.distribute(dictFunctor);
  return f => {
    const $0 = dictFunctor.map(f);
    return x => distribute2($0(x));
  };
};
const distributiveTuple = dictTypeEquals => {
  const from = dictTypeEquals.proof(a => a);
  return {
    collect: dictFunctor => {
      const distribute2 = distributiveTuple(dictTypeEquals).distribute(dictFunctor);
      return f => {
        const $0 = dictFunctor.map(f);
        return x => distribute2($0(x));
      };
    },
    distribute: dictFunctor => {
      const $0 = Data$dTuple.Tuple(from());
      const $1 = dictFunctor.map(Data$dTuple.snd);
      return x => $0($1(x));
    },
    Functor0: () => Data$dTuple.functorTuple
  };
};
const collect = dict => dict.collect;
const distributeDefault = dictDistributive => dictFunctor => dictDistributive.collect(dictFunctor)(identity);
export {collect, collectDefault, cotraverse, distribute, distributeDefault, distributiveFunction, distributiveIdentity, distributiveTuple, identity};
