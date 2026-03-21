import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Lattice from "../Lattice/index.js";
import * as SExpr from "../SExpr/index.js";
import * as Util from "../Util/index.js";
const desug1 = /* #__PURE__ */ (() => {
  const $0 = {BoundedJoinSemilattice0: () => Lattice.boundedJoinSemilatticeUni, BoundedMeetSemilattice1: () => Lattice.boundedMeetSemilatticeUni};
  return SExpr.exprFwd($0)(Control$dMonad$dExcept$dTrans.monadErrorExceptT(Data$dIdentity.monadIdentity))($0.BoundedJoinSemilattice0().JoinSemilattice0());
})();
const desugGC = dictMonadError => dictEq => dictBoundedLattice => {
  const desug2 = SExpr.exprFwd(dictBoundedLattice)(Control$dMonad$dExcept$dTrans.monadErrorExceptT(Data$dIdentity.monadIdentity))(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0());
  return s => dictMonadError.MonadThrow0().Monad0().Applicative0().pure({
    gc: {fwd: s$p => Util.defined(desug2(s$p)), bwd: e$p => SExpr.exprBwd(dictBoundedLattice.BoundedJoinSemilattice0())(e$p)(s)},
    e: Util.defined(desug1(s))
  });
};
export {desug1, desugGC};
