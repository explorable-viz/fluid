import * as $runtime from "../runtime.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Util from "../Util/index.js";
import * as Util$dSet from "../Util.Set/index.js";
const union = /* #__PURE__ */ (() => Util$dSet.setSet(Data$dOrd.ordString).union)();
const varAnon = "_";
const val = Data$dTuple.snd;
const showBind = dictShow => x => x$1 => Data$dTuple.$Tuple(x, dictShow.show(x$1));
const mustGeq = x => y => Util.definitely("greater")((() => {
  const $0 = x === y;
  if (!$0) { return Data$dMaybe.Nothing; }
  if ($0) { return Data$dMaybe.$Maybe("Just", x); }
  $runtime.fail();
})());
const keys = v => {
  if (v.tag === "Nil") { return Data$dMap$dInternal.Leaf; }
  if (v.tag === "Cons") { return union(Data$dMap$dInternal.$$$Map("Node", 1, 1, v._1._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(keys(v._2)); }
  $runtime.fail();
};
const key = Data$dTuple.fst;
export { keys,     };
