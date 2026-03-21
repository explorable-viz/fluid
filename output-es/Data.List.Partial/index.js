// | Partial helper functions for working with strict linked lists.
import * as $runtime from "../runtime.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
const tail = () => v => {
  if (v.tag === "Cons") { return v._2; }
  $runtime.fail();
};
const last = last$a0$copy => last$a1$copy => {
  let last$a0 = last$a0$copy, last$a1 = last$a1$copy, last$c = true, last$r;
  while (last$c) {
    const $__unused = last$a0, v = last$a1;
    if (v.tag === "Cons") {
      if (v._2.tag === "Nil") {
        last$c = false;
        last$r = v._1;
        continue;
      }
      last$a0 = undefined;
      last$a1 = v._2;
      continue;
    }
    $runtime.fail();
  }
  return last$r;
};
const init = () => v => {
  if (v.tag === "Cons") {
    if (v._2.tag === "Nil") { return Data$dList$dTypes.Nil; }
    return Data$dList$dTypes.$List("Cons", v._1, init()(v._2));
  }
  $runtime.fail();
};
const head = () => v => {
  if (v.tag === "Cons") { return v._1; }
  $runtime.fail();
};
export {head, init, last, tail};
