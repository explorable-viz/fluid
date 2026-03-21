import * as $runtime from "../runtime.js";
import * as Control$dMonad$dGen from "../Control.Monad.Gen/index.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dChar$dGen from "../Data.Char.Gen/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dString$dGen from "../Data.String.Gen/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
const foldable1NonEmpty = /* #__PURE__ */ Data$dNonEmpty.foldable1NonEmpty(Data$dFoldable.foldableArray);
const min = x => y => {
  const v = Data$dOrd.ordInt.compare(x)(y);
  if (v === "LT") { return x; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return y; }
  $runtime.fail();
};
const genJson = dictMonadGen => {
  const Monad0 = dictMonadGen.Monad0();
  const Bind1 = Monad0.Bind1();
  const $0 = Bind1.Apply0().Functor0();
  const chooseBool = dictMonadGen.chooseBool;
  const oneOf = Control$dMonad$dGen.oneOf(dictMonadGen)(foldable1NonEmpty);
  const $1 = Monad0.Applicative0();
  const chooseBool$1 = dictMonadGen.chooseBool;
  return dictMonadRec => {
    const genUnicodeString = Data$dString$dGen.genString(dictMonadRec)(dictMonadGen)(Data$dChar$dGen.genUnicodeChar(dictMonadGen));
    const unfoldable = Control$dMonad$dGen.unfoldable(dictMonadRec)(dictMonadGen)(Data$dUnfoldable.unfoldableArray);
    return dictLazy => {
      const genLeaf = oneOf(Data$dNonEmpty.$NonEmpty(
        $1.pure(Data$dArgonaut$dCore.jsonNull),
        [
          $0.map(Data$dArgonaut$dCore.fromBoolean)(chooseBool),
          $0.map(Data$dArgonaut$dCore.fromNumber)(dictMonadGen.chooseFloat(-1000000.0)(1000000.0)),
          $0.map(Data$dArgonaut$dCore.fromString)(genUnicodeString)
        ]
      ));
      const genJArray = $0.map(Data$dArgonaut$dCore.fromArray)(unfoldable(dictLazy.defer(v => genJson(dictMonadGen)(dictMonadRec)(dictLazy))));
      const genJObject = Bind1.bind(unfoldable(genUnicodeString))(Data$dArray.foldM(Monad0)(obj => k => Bind1.bind(genJson(dictMonadGen)(dictMonadRec)(dictLazy))(v => $1.pure((() => {
        const $2 = Data$dArgonaut$dCore.jsonSingletonObject(k)(v);
        return Data$dArgonaut$dCore._caseJson(
          v$1 => $2,
          v$1 => $2,
          v$1 => $2,
          v$1 => $2,
          v$1 => $2,
          x => Data$dArgonaut$dCore.fromObject(Foreign$dObject.mutate($3 => () => {
            $3[k] = v;
            return $3;
          })(x)),
          obj
        );
      })())))(Data$dArgonaut$dCore.jsonEmptyObject));
      return dictMonadGen.resize(min(5))(dictMonadGen.sized(size => {
        if (size > 1) {
          return dictMonadGen.resize(v => v - 1 | 0)(dictMonadGen.Monad0().Bind1().bind(chooseBool$1)(v => {
            if (v) { return genJArray; }
            return genJObject;
          }));
        }
        return genLeaf;
      }));
    };
  };
};
export {foldable1NonEmpty, genJson, min};
