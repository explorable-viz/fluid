import * as $runtime from "../runtime.js";
import * as Control$dMonad$dError$dClass from "../Control.Monad.Error.Class/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
import * as Options$dApplicative$dInternal$dUtils from "../Options.Applicative.Internal.Utils/index.js";
import {execSyncCommand} from "./foreign.js";
const unWords = xs => Data$dFoldable.foldlArray(v => v1 => {
  if (v.init) { return {init: false, acc: v1}; }
  return {init: false, acc: v.acc + " " + v1};
})({init: true, acc: ""})(xs).acc;
const $$try = /* #__PURE__ */ Control$dMonad$dError$dClass.try(Control$dMonad$dError$dClass.monadErrorEffect);
const requote = /* #__PURE__ */ (() => {
  const $0 = Data$dFoldable.foldrArray(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
  const goX = v => {
    if (v.tag === "Nil") { return Data$dList$dTypes.Nil; }
    if (v.tag === "Cons") {
      if (v._1 === "\\" && v._2.tag === "Cons") { return Data$dList$dTypes.$List("Cons", v._2._1, goX(v._2._2)); }
      return Data$dList$dTypes.$List("Cons", v._1, goX(v._2));
    }
    $runtime.fail();
  };
  const $goXgoN = ($goXgoN$b$copy, $goXgoN$a0$copy) => {
    let $goXgoN$b = $goXgoN$b$copy, $goXgoN$a0 = $goXgoN$a0$copy, $goXgoN$c = true, $goXgoN$r;
    while ($goXgoN$c) {
      if ($goXgoN$b === 0) {
        const v = $goXgoN$a0;
        if (v.tag === "Cons") {
          if (v._1 === "'") {
            $goXgoN$b = 1;
            $goXgoN$a0 = v._2;
            continue;
          }
          $goXgoN$c = false;
          $goXgoN$r = Data$dList$dTypes.$List("Cons", v._1, goX$1(v._2));
          continue;
        }
        if (v.tag === "Nil") {
          $goXgoN$c = false;
          $goXgoN$r = Data$dList$dTypes.Nil;
          continue;
        }
        $runtime.fail();
      }
      if ($goXgoN$b === 1) {
        const v = $goXgoN$a0;
        if (v.tag === "Cons") {
          if (v._2.tag === "Cons" && v._2._1 === "'" && v._1 === "\\") {
            $goXgoN$c = false;
            $goXgoN$r = Data$dList$dTypes.$List("Cons", "'", goN(v._2._2));
            continue;
          }
          if (v._1 === "'") {
            $goXgoN$b = 0;
            $goXgoN$a0 = v._2;
            continue;
          }
          $goXgoN$c = false;
          $goXgoN$r = Data$dList$dTypes.$List("Cons", v._1, goN(v._2));
          continue;
        }
        if (v.tag === "Nil") {
          $goXgoN$c = false;
          $goXgoN$r = Data$dList$dTypes.Nil;
          continue;
        }
        $runtime.fail();
      }
    }
    return $goXgoN$r;
  };
  const goX$1 = v => $goXgoN(0, v);
  const goN = v => $goXgoN(1, v);
  const goX$2 = v => {
    if (v.tag === "Cons") {
      if (v._2.tag === "Cons" && v._1 === "\\") {
        if (Data$dArray.elem(Data$dEq.eqChar)(v._2._1)(["$", "`", "\"", "\\", "\n"])) { return Data$dList$dTypes.$List("Cons", v._2._1, goX$2(v._2._2)); }
        return Data$dList$dTypes.$List("Cons", "\\", Data$dList$dTypes.$List("Cons", v._2._1, goX$2(v._2._2)));
      }
      if (v._1 === "\"") { return v._2; }
      return Data$dList$dTypes.$List("Cons", v._1, goX$2(v._2));
    }
    if (v.tag === "Nil") { return Data$dList$dTypes.Nil; }
    $runtime.fail();
  };
  const $1 = Data$dUnfoldable.unfoldableArray.unfoldr(xs => {
    if (xs.tag === "Nil") { return Data$dMaybe.Nothing; }
    if (xs.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(xs._1, xs._2)); }
    $runtime.fail();
  });
  return x => {
    const $2 = $0(Data$dString$dCodeUnits.toCharArray(x));
    return Data$dString$dCodeUnits.fromCharArray($1(Data$dList$dTypes.$List(
      "Cons",
      "'",
      Data$dList$dTypes.foldableList.foldr(v => v1 => {
        if (v === "'") { return Data$dList$dTypes.$List("Cons", "'", Data$dList$dTypes.$List("Cons", "\\", Data$dList$dTypes.$List("Cons", "'", v1))); }
        return Data$dList$dTypes.$List("Cons", v, v1);
      })(Data$dList$dTypes.$List("Cons", "'", Data$dList$dTypes.Nil))((() => {
        if ($2.tag === "Cons") {
          if ($2._1 === "'") { return goX$1($2._2); }
          if ($2._1 === "\"") { return goX$2($2._2); }
        }
        return goX($2);
      })())
    )));
  };
})();
const listIOCompleter = ss => s => () => {
  const a$p = ss();
  return Data$dArray.filterImpl(Options$dApplicative$dInternal$dUtils.startsWith(s), a$p);
};
const listCompleter = x => listIOCompleter(() => x);
const bashCompleter = action => word => {
  const $0 = $$try(execSyncCommand("bash -c " + unWords(["compgen", "-A", action, "--", requote(word)])));
  return () => {
    const result = $0();
    const $1 = (() => {
      if (result.tag === "Left") { return ""; }
      if (result.tag === "Right") { return result._1; }
      $runtime.fail();
    })();
    return $1 === "" ? [] : Data$dString$dCommon.split("\n")($1);
  };
};
export {bashCompleter, listCompleter, listIOCompleter, requote, $$try as try, unWords};
export * from "./foreign.js";
