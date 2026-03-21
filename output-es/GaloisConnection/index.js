import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Util from "../Util/index.js";
const identity = x => x;
const GC = x => x;
const semigroupoidGaloisConnect = {compose: v => v1 => ({fwd: x => v.fwd(v1.fwd(x)), bwd: x => v1.bwd(v.bwd(x))})};
const newtypeGaloisConnection_ = {Coercible0: () => {}};
const categoryGaloisConnection = {identity: {fwd: identity, bwd: identity}, Semigroupoid0: () => semigroupoidGaloisConnect};
const unsnd = dictBoundedMeetSemilattice => {
  const top = dictBoundedMeetSemilattice.top;
  return {fwd: b => Data$dTuple.$Tuple(top, b), bwd: Data$dTuple.snd};
};
const unfst = dictBoundedMeetSemilattice => {
  const top = dictBoundedMeetSemilattice.top;
  return {fwd: a => Data$dTuple.$Tuple(a, top), bwd: Data$dTuple.fst};
};
const splitStrong = v => v1 => ({fwd: x => Data$dTuple.$Tuple(v.fwd(x._1), v1.fwd(x._2)), bwd: x => Data$dTuple.$Tuple(v.bwd(x._1), v1.bwd(x._2))});
const second = v => (
  {fwd: x => Data$dTuple.$Tuple(categoryGaloisConnection.identity.fwd(x._1), v.fwd(x._2)), bwd: x => Data$dTuple.$Tuple(categoryGaloisConnection.identity.bwd(x._1), v.bwd(x._2))}
);
const join = dictJoinSemilattice => ({fwd: Util.dup, bwd: v => dictJoinSemilattice.join(v._1)(v._2)});
const first = v => (
  {fwd: x => Data$dTuple.$Tuple(v.fwd(x._1), categoryGaloisConnection.identity.fwd(x._2)), bwd: x => Data$dTuple.$Tuple(v.bwd(x._1), categoryGaloisConnection.identity.bwd(x._2))}
);
const fanout = dictNeg => dictJoinSemilattice => f => g => ({fwd: x => Data$dTuple.$Tuple(f.fwd(x), g.fwd(x)), bwd: x => dictJoinSemilattice.join(f.bwd(x._1))(g.bwd(x._2))});
const deMorgan = dictNeg => dictNeg1 => x => x$1 => dictNeg1.neg(x(dictNeg.neg(x$1)));
const dual = dictNeg => dictNeg1 => v => ({fwd: x => dictNeg.neg(v.bwd(dictNeg1.neg(x))), bwd: x => dictNeg1.neg(v.fwd(dictNeg.neg(x)))});
const meet = dictNeg => dictJoinSemilattice => (
  {
    fwd: x => dictNeg.neg(dictJoinSemilattice.join(dictNeg.neg(x._1))(dictNeg.neg(x._2))),
    bwd: x => {
      const $0 = dictNeg.neg(x);
      return Data$dTuple.$Tuple(dictNeg.neg($0), dictNeg.neg($0));
    }
  }
);
const relatedInputs = dictNeg => dictNeg1 => dictJoinSemilattice => f => (
  {
    fwd: x => dictNeg.neg(f.bwd(dictNeg1.neg(dictNeg1.neg(dictJoinSemilattice.join(dictNeg1.neg(f.fwd(x._1)))(dictNeg1.neg(categoryGaloisConnection.identity.fwd(x._2))))))),
    bwd: x => {
      const $0 = dictNeg1.neg(dictNeg1.neg(f.fwd(dictNeg.neg(x))));
      return Data$dTuple.$Tuple(f.bwd(dictNeg1.neg($0)), categoryGaloisConnection.identity.bwd(dictNeg1.neg($0)));
    }
  }
);
const relatedOutputs = dictNeg => dictJoinSemilattice => dictNeg1 => f => (
  {
    fwd: x => f.fwd(dictNeg.neg(dictJoinSemilattice.join(dictNeg.neg(dictNeg.neg(f.bwd(dictNeg1.neg(x._1)))))(dictNeg.neg(categoryGaloisConnection.identity.fwd(x._2))))),
    bwd: x => {
      const $0 = dictNeg.neg(f.bwd(x));
      return Data$dTuple.$Tuple(dictNeg1.neg(f.fwd(dictNeg.neg(dictNeg.neg($0)))), categoryGaloisConnection.identity.bwd(dictNeg.neg($0)));
    }
  }
);
export {
  GC,
  categoryGaloisConnection,
  deMorgan,
  dual,
  fanout,
  first,
  identity,
  join,
  meet,
  newtypeGaloisConnection_,
  relatedInputs,
  relatedOutputs,
  second,
  semigroupoidGaloisConnect,
  splitStrong,
  unfst,
  unsnd
};
