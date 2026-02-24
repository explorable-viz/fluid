import * as $runtime from "../runtime.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
const $KickUp = (_1, _2, _3) => ({tag: "KickUp", _1, _2, _3});
const $$$Set = (tag, _1, _2, _3, _4, _5) => ({tag, _1, _2, _3, _4, _5});
const $TreeContext = (tag, _1, _2, _3, _4) => ({tag, _1, _2, _3, _4});
const Leaf = /* #__PURE__ */ $$$Set("Leaf");
const Two = value0 => value1 => value2 => $$$Set("Two", value0, value1, value2);
const Three = value0 => value1 => value2 => value3 => value4 => $$$Set("Three", value0, value1, value2, value3, value4);
const TwoLeft = value0 => value1 => $TreeContext("TwoLeft", value0, value1);
const TwoRight = value0 => value1 => $TreeContext("TwoRight", value0, value1);
const ThreeLeft = value0 => value1 => value2 => value3 => $TreeContext("ThreeLeft", value0, value1, value2, value3);
const ThreeMiddle = value0 => value1 => value2 => value3 => $TreeContext("ThreeMiddle", value0, value1, value2, value3);
const ThreeRight = value0 => value1 => value2 => value3 => $TreeContext("ThreeRight", value0, value1, value2, value3);
const KickUp = value0 => value1 => value2 => $KickUp(value0, value1, value2);
const fromZipper = fromZipper$a0$copy => fromZipper$a1$copy => {
  let fromZipper$a0 = fromZipper$a0$copy, fromZipper$a1 = fromZipper$a1$copy, fromZipper$c = true, fromZipper$r;
  while (fromZipper$c) {
    const v = fromZipper$a0, v1 = fromZipper$a1;
    if (v.tag === "Nil") {
      fromZipper$c = false;
      fromZipper$r = v1;
      continue;
    }
    if (v.tag === "Cons") {
      if (v._1.tag === "TwoLeft") {
        fromZipper$a0 = v._2;
        fromZipper$a1 = $$$Set("Two", v1, v._1._1, v._1._2);
        continue;
      }
      if (v._1.tag === "TwoRight") {
        fromZipper$a0 = v._2;
        fromZipper$a1 = $$$Set("Two", v._1._1, v._1._2, v1);
        continue;
      }
      if (v._1.tag === "ThreeLeft") {
        fromZipper$a0 = v._2;
        fromZipper$a1 = $$$Set("Three", v1, v._1._1, v._1._2, v._1._3, v._1._4);
        continue;
      }
      if (v._1.tag === "ThreeMiddle") {
        fromZipper$a0 = v._2;
        fromZipper$a1 = $$$Set("Three", v._1._1, v._1._2, v1, v._1._3, v._1._4);
        continue;
      }
      if (v._1.tag === "ThreeRight") {
        fromZipper$a0 = v._2;
        fromZipper$a1 = $$$Set("Three", v._1._1, v._1._2, v._1._3, v._1._4, v1);
        continue;
      }
    }
    $runtime.fail();
  }
  return fromZipper$r;
};
const insertAndLookupBy = comp => k => orig => {
  const up = up$a0$copy => up$a1$copy => {
    let up$a0 = up$a0$copy, up$a1 = up$a1$copy, up$c = true, up$r;
    while (up$c) {
      const v = up$a0, v1 = up$a1;
      if (v.tag === "Nil") {
        up$c = false;
        up$r = $$$Set("Two", v1._1, v1._2, v1._3);
        continue;
      }
      if (v.tag === "Cons") {
        if (v._1.tag === "TwoLeft") {
          up$c = false;
          up$r = fromZipper(v._2)($$$Set("Three", v1._1, v1._2, v1._3, v._1._1, v._1._2));
          continue;
        }
        if (v._1.tag === "TwoRight") {
          up$c = false;
          up$r = fromZipper(v._2)($$$Set("Three", v._1._1, v._1._2, v1._1, v1._2, v1._3));
          continue;
        }
        if (v._1.tag === "ThreeLeft") {
          up$a0 = v._2;
          up$a1 = $KickUp($$$Set("Two", v1._1, v1._2, v1._3), v._1._1, $$$Set("Two", v._1._2, v._1._3, v._1._4));
          continue;
        }
        if (v._1.tag === "ThreeMiddle") {
          up$a0 = v._2;
          up$a1 = $KickUp($$$Set("Two", v._1._1, v._1._2, v1._1), v1._2, $$$Set("Two", v1._3, v._1._3, v._1._4));
          continue;
        }
        if (v._1.tag === "ThreeRight") {
          up$a0 = v._2;
          up$a1 = $KickUp($$$Set("Two", v._1._1, v._1._2, v._1._3), v._1._4, $$$Set("Two", v1._1, v1._2, v1._3));
          continue;
        }
      }
      $runtime.fail();
    }
    return up$r;
  };
  const down = down$a0$copy => down$a1$copy => {
    let down$a0 = down$a0$copy, down$a1 = down$a1$copy, down$c = true, down$r;
    while (down$c) {
      const v = down$a0, v1 = down$a1;
      if (v1.tag === "Leaf") {
        down$c = false;
        down$r = {found: false, result: up(v)($KickUp(Leaf, k, Leaf))};
        continue;
      }
      if (v1.tag === "Two") {
        const v2 = comp(k)(v1._2);
        if (v2 === "EQ") {
          down$c = false;
          down$r = {found: true, result: orig};
          continue;
        }
        if (v2 === "LT") {
          down$a0 = Data$dList$dTypes.$List("Cons", $TreeContext("TwoLeft", v1._2, v1._3), v);
          down$a1 = v1._1;
          continue;
        }
        down$a0 = Data$dList$dTypes.$List("Cons", $TreeContext("TwoRight", v1._1, v1._2), v);
        down$a1 = v1._3;
        continue;
      }
      if (v1.tag === "Three") {
        const v2 = comp(k)(v1._2);
        if (v2 === "EQ") {
          down$c = false;
          down$r = {found: true, result: orig};
          continue;
        }
        const v3 = comp(k)(v1._4);
        if (v3 === "EQ") {
          down$c = false;
          down$r = {found: true, result: orig};
          continue;
        }
        if (v2 === "LT") {
          down$a0 = Data$dList$dTypes.$List("Cons", $TreeContext("ThreeLeft", v1._2, v1._3, v1._4, v1._5), v);
          down$a1 = v1._1;
          continue;
        }
        if (v2 === "GT" && v3 === "LT") {
          down$a0 = Data$dList$dTypes.$List("Cons", $TreeContext("ThreeMiddle", v1._1, v1._2, v1._4, v1._5), v);
          down$a1 = v1._3;
          continue;
        }
        down$a0 = Data$dList$dTypes.$List("Cons", $TreeContext("ThreeRight", v1._1, v1._2, v1._3, v1._4), v);
        down$a1 = v1._5;
        continue;
      }
      $runtime.fail();
    }
    return down$r;
  };
  return down(Data$dList$dTypes.Nil)(orig);
};
const emptySet = Leaf;
export {    Leaf,          insertAndLookupBy};
