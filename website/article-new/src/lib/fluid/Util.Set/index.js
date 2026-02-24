import * as $runtime from "../runtime.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Util from "../Util/index.js";
const setSet = dictOrd => (
  {
    empty: Data$dMap$dInternal.Leaf,
    filter: Data$dSet.filter(dictOrd),
    size: Data$dMap$dInternal.size,
    difference: (() => {
      const compare = dictOrd.compare;
      return m1 => m2 => Data$dMap$dInternal.unsafeDifference(compare, m1, m2);
    })(),
    member: k => {
      const go = go$a0$copy => {
        let go$a0 = go$a0$copy, go$c = true, go$r;
        while (go$c) {
          const v = go$a0;
          if (v.tag === "Leaf") {
            go$c = false;
            go$r = false;
            continue;
          }
          if (v.tag === "Node") {
            const v1 = dictOrd.compare(k)(v._3);
            if (v1 === "LT") {
              go$a0 = v._5;
              continue;
            }
            if (v1 === "GT") {
              go$a0 = v._6;
              continue;
            }
            if (v1 === "EQ") {
              go$c = false;
              go$r = true;
              continue;
            }
          }
          $runtime.fail();
        }
        return go$r;
      };
      return go;
    },
    union: (() => {
      const compare = dictOrd.compare;
      return m1 => m2 => Data$dMap$dInternal.unsafeUnionWith(compare, Data$dFunction.const, m1, m2);
    })(),
    IsEmpty0: () => Util.isEmptySet
  }
);
const setObjectString = {
  empty: Foreign$dObject.empty,
  filter: Foreign$dObject.filterKeys,
  size: Foreign$dObject.size,
  difference: x => y => Data$dFoldable.foldlArray(b => a => Foreign$dObject.mutate($0 => () => {
    delete $0[a];
    return $0;
  })(b))(x)(Object.keys(y)),
  member: Foreign$dObject.member,
  union: Foreign$dObject.union,
  IsEmpty0: () => Util.isEmptyObject
};
const union = dict => dict.union;
const size = dict => dict.size;
const member = dict => dict.member;
const filter = dict => dict.filter;
const empty = dict => dict.empty;
const unions = dictFoldable => dictSet => dictFoldable.foldl(dictSet.union)(dictSet.empty);
const difference = dict => dict.difference;
export {    setObjectString, setSet,   };
