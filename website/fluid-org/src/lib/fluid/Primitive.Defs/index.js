import * as $runtime from "../runtime.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dArgonaut$dDecode$dError from "../Data.Argonaut.Decode.Error/index.js";
import * as Data$dArgonaut$dDecode$dParser from "../Data.Argonaut.Decode.Parser/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dEuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import * as Data$dRing from "../Data.Ring/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import * as Data$dSemiring from "../Data.Semiring/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodePoints from "../Data.String.CodePoints/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dRegex from "../Data.String.Regex/index.js";
import * as Data$dString$dRegex$dFlags from "../Data.String.Regex.Flags/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
import * as Debug from "../Debug/index.js";
import * as Dict from "../Dict/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Eval from "../Eval/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Graph from "../Graph/index.js";
import * as Lattice from "../Lattice/index.js";
import * as Primitive from "../Primitive/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Val from "../Val/index.js";
const toUnfoldable = /* #__PURE__ */ Foreign$dObject.toUnfoldable(Data$dUnfoldable.unfoldableArray);
const fromFoldable = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dFoldable.foldableArray);
const toUnfoldable2 = /* #__PURE__ */ Foreign$dObject.toAscUnfoldable(Data$dList$dTypes.unfoldableList);
const disjointUnion = /* #__PURE__ */ Util$dMap.disjointUnion(Dict.mapDictString);
const fromFoldable1 = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dList$dTypes.foldableList);
const unary = /* #__PURE__ */ Primitive.unary(Lattice.boundedJoinSemilatticeUni);
const binary = /* #__PURE__ */ Primitive.binary(Lattice.boundedJoinSemilatticeUni);
const binaryZero = /* #__PURE__ */ Primitive.binaryZero(Lattice.boundedJoinSemilatticeUni);
const binaryZero1 = /* #__PURE__ */ (() => binaryZero({isZero: Primitive.fanin(Primitive.isZeroInt.isZero)(Primitive.isZeroNumber.isZero)}))();
const binaryZero2 = /* #__PURE__ */ binaryZero(Primitive.isZeroInt);
const times = /* #__PURE__ */ Primitive.union(Primitive.asIntIntOrNumber)(Primitive.asNumberIntOrNumber)(Primitive.asIntNumber)(Primitive.asIntNumber)(Data$dSemiring.intMul)(Data$dSemiring.numMul);
const split = /* #__PURE__ */ Data$dTuple.$Tuple(
  "search",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 2,
    op: dictMonadWithGraphAlloc => {
      const val = Val.val(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => {
          const $0 = dictMonadAff.MonadEffect0().Monad0().Bind1();
          return dictMonadReader => dictLoadFile => v => v1 => {
            if (v1.tag === "Cons" && v1._1._3.tag === "Int" && v1._2.tag === "Cons" && v1._2._1._3.tag === "Str" && v1._2._2.tag === "Nil") {
              const $1 = v1._1._3._1;
              const $2 = v1._2._1._3._1;
              const αs = Data$dMap$dInternal.insert(Graph.ordVertex)(v1._2._1._1)()(Data$dMap$dInternal.$$$Map(
                "Node",
                1,
                1,
                v1._1._1,
                undefined,
                Data$dMap$dInternal.Leaf,
                Data$dMap$dInternal.Leaf
              ));
              return $0.bind(val(Data$dMaybe.Nothing)(αs)(Val.$BaseVal("Str", Data$dString$dCodePoints.take($1)($2))))(before => $0.bind(val(Data$dMaybe.Nothing)(αs)(Val.$BaseVal(
                "Str",
                Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(Data$dString$dCodePoints.take($1)($2)))($2)
              )))(after => val(v)(αs)(Val.$BaseVal("Constr", "Pair", Data$dList$dTypes.$List("Cons", before, Data$dList$dTypes.$List("Cons", after, Data$dList$dTypes.Nil))))));
            }
            return $$throw("Int and string expected");
          };
        };
      };
    }
  })
);
const search = /* #__PURE__ */ Data$dTuple.$Tuple(
  "search",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 2,
    op: dictMonadWithGraphAlloc => {
      const val = Val.val(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => dictMonadReader => dictLoadFile => v => v1 => {
          if (v1.tag === "Cons" && v1._1._3.tag === "Str" && v1._2.tag === "Cons" && v1._2._1._3.tag === "Str" && v1._2._2.tag === "Nil") {
            const $0 = v1._2._1._3._1;
            const v2 = Data$dString$dRegex.regex(v1._1._3._1)(Data$dString$dRegex$dFlags.noFlags);
            if (v2.tag === "Left") { return $$throw("Regex expected: " + v2._1); }
            if (v2.tag === "Right") {
              const αs = Data$dMap$dInternal.insert(Graph.ordVertex)(v1._2._1._1)()(Data$dMap$dInternal.$$$Map(
                "Node",
                1,
                1,
                v1._1._1,
                undefined,
                Data$dMap$dInternal.Leaf,
                Data$dMap$dInternal.Leaf
              ));
              const v3 = Data$dString$dRegex.search(v2._1)($0);
              if (v3.tag === "Nothing") { return val(v)(αs)(Val.$BaseVal("Constr", "None", Data$dList$dTypes.Nil)); }
              if (v3.tag === "Just") {
                return dictMonadAff.MonadEffect0().Monad0().Bind1().bind(val(Data$dMaybe.Nothing)(αs)(Val.$BaseVal("Int", v3._1)))(v4 => val(v)(αs)(Val.$BaseVal(
                  "Constr",
                  "Some",
                  Data$dList$dTypes.$List("Cons", v4, Data$dList$dTypes.Nil)
                )));
              }
            }
            $runtime.fail();
          }
          return $$throw("Two strings expected");
        };
      };
    }
  })
);
const rem = Data$dInt.rem;
const quot = Data$dInt.quot;
const pow = /* #__PURE__ */ Primitive.union(Primitive.asNumberIntOrNumber)(Primitive.asNumberIntOrNumber)(Primitive.asIntNumber)(Primitive.asIntNumber)(x => y => Data$dNumber.pow(Data$dInt.toNumber(x))(Data$dInt.toNumber(y)))(Data$dNumber.pow);
const plus = /* #__PURE__ */ Primitive.union(Primitive.asIntIntOrNumber)(Primitive.asNumberIntOrNumber)(Primitive.asIntNumber)(Primitive.asIntNumber)(Data$dSemiring.intAdd)(Data$dSemiring.numAdd);
const numToStr = v2 => {
  if (v2.tag === "Left") { return Data$dShow.showIntImpl(v2._1); }
  if (v2.tag === "Right") { return Data$dShow.showNumberImpl(v2._1); }
  $runtime.fail();
};
const notEquals = /* #__PURE__ */ Primitive.union(Primitive.asBooleanBoolean)(Primitive.asBooleanBoolean)(Primitive.asIntNumberOrString)(Primitive.asIntNumberOrString)(x => y => x !== y)(/* #__PURE__ */ Primitive.union(Primitive.asBooleanBoolean)(Primitive.asBooleanBoolean)(Primitive.asNumberString)(Primitive.asNumberString)(x => y => x !== y)(x => y => x !== y));
const minus = /* #__PURE__ */ Primitive.union(Primitive.asIntIntOrNumber)(Primitive.asNumberIntOrNumber)(Primitive.asIntNumber)(Primitive.asIntNumber)(Data$dRing.intSub)(Data$dRing.numSub);
const matrixUpdate = /* #__PURE__ */ Data$dTuple.$Tuple(
  "matrixUpdate",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 3,
    op: dictMonadWithGraphAlloc => {
      const val = Val.val(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => dictMonadReader => dictLoadFile => v => v1 => {
          if (
            v1.tag === "Cons" && v1._1._3.tag === "Matrix" && v1._2.tag === "Cons" && v1._2._1._3.tag === "Constr" && v1._2._1._3._2.tag === "Cons" && v1._2._1._3._2._1._3.tag === "Int" && v1._2._1._3._2._2.tag === "Cons" && v1._2._1._3._2._2._1._3.tag === "Int" && v1._2._1._3._2._2._2.tag === "Nil" && v1._2._2.tag === "Cons" && v1._2._2._2.tag === "Nil" && v1._2._1._3._1 === "Pair"
          ) {
            const $0 = v1._2._2._1;
            return val(v)(Data$dMap$dInternal.$$$Map("Node", 1, 1, v1._1._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(Val.$BaseVal(
              "Matrix",
              Val.matrixPut(v1._2._1._3._2._1._3._1)(v1._2._1._3._2._2._1._3._1)(v$1 => $0)(v1._1._3._1)
            ));
          }
          return $$throw("Matrix, pair of integers and value expected");
        };
      };
    }
  })
);
const matrixLookup = /* #__PURE__ */ Data$dTuple.$Tuple(
  "!",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 2,
    op: dictMonadWithGraphAlloc => dictMonadError => {
      const $$throw = Util.throw(dictMonadError.MonadThrow0());
      return dictMonadAff => dictMonadReader => dictLoadFile => v => v1 => {
        if (
          v1.tag === "Cons" && v1._1._3.tag === "Matrix" && v1._2.tag === "Cons" && v1._2._1._3.tag === "Constr" && v1._2._1._3._2.tag === "Cons" && v1._2._1._3._2._1._3.tag === "Int" && v1._2._1._3._2._2.tag === "Cons" && v1._2._1._3._2._2._1._3.tag === "Int" && v1._2._1._3._2._2._2.tag === "Nil" && v1._2._2.tag === "Nil" && v1._2._1._3._1 === "Pair"
        ) {
          return dictMonadAff.MonadEffect0().Monad0().Applicative0().pure(Val.matrixGet(v1._2._1._3._2._1._3._1)(v1._2._1._3._2._2._1._3._1)(v1._1._3._1));
        }
        return $$throw("Matrix and pair of integers expected");
      };
    }
  })
);
const log = v2 => {
  if (v2.tag === "Left") { return Data$dNumber.log(Data$dInt.toNumber(v2._1)); }
  if (v2.tag === "Right") { return Data$dNumber.log(v2._1); }
  $runtime.fail();
};
const lessThanEquals = /* #__PURE__ */ Primitive.union(Primitive.asBooleanBoolean)(Primitive.asBooleanBoolean)(Primitive.asIntNumberOrString)(Primitive.asIntNumberOrString)(a1 => a2 => a1 <= a2)(/* #__PURE__ */ Primitive.union(Primitive.asBooleanBoolean)(Primitive.asBooleanBoolean)(Primitive.asNumberString)(Primitive.asNumberString)(a1 => a2 => a1 <= a2)(a1 => a2 => a1 <= a2));
const lessThan = /* #__PURE__ */ Primitive.union(Primitive.asBooleanBoolean)(Primitive.asBooleanBoolean)(Primitive.asIntNumberOrString)(Primitive.asIntNumberOrString)(a1 => a2 => a1 < a2)(/* #__PURE__ */ Primitive.union(Primitive.asBooleanBoolean)(Primitive.asBooleanBoolean)(Primitive.asNumberString)(Primitive.asNumberString)(a1 => a2 => a1 < a2)(a1 => a2 => a1 < a2));
const insert = /* #__PURE__ */ Data$dTuple.$Tuple(
  "insert",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 3,
    op: dictMonadWithGraphAlloc => {
      const val = Val.val(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => dictMonadReader => dictLoadFile => v => v1 => {
          if (v1.tag === "Cons" && v1._1._3.tag === "Dictionary" && v1._2.tag === "Cons" && v1._2._1._3.tag === "Str" && v1._2._2.tag === "Cons" && v1._2._2._2.tag === "Nil") {
            const $0 = v1._2._1._3._1;
            return val(v)(Data$dMap$dInternal.$$$Map("Node", 1, 1, v1._1._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(Val.$BaseVal(
              "Dictionary",
              Foreign$dObject.mutate($1 => () => {
                $1[$0] = Data$dTuple.$Tuple(v1._2._1._1, v1._2._2._1);
                return $1;
              })(v1._1._3._1)
            ));
          }
          return $$throw("Dictionary, key and value expected");
        };
      };
    }
  })
);
const greaterThanEquals = /* #__PURE__ */ Primitive.union(Primitive.asBooleanBoolean)(Primitive.asBooleanBoolean)(Primitive.asIntNumberOrString)(Primitive.asIntNumberOrString)(a1 => a2 => a1 >= a2)(/* #__PURE__ */ Primitive.union(Primitive.asBooleanBoolean)(Primitive.asBooleanBoolean)(Primitive.asNumberString)(Primitive.asNumberString)(a1 => a2 => a1 >= a2)(a1 => a2 => a1 >= a2));
const greaterThan = /* #__PURE__ */ Primitive.union(Primitive.asBooleanBoolean)(Primitive.asBooleanBoolean)(Primitive.asIntNumberOrString)(Primitive.asIntNumberOrString)(a1 => a2 => a1 > a2)(/* #__PURE__ */ Primitive.union(Primitive.asBooleanBoolean)(Primitive.asBooleanBoolean)(Primitive.asNumberString)(Primitive.asNumberString)(a1 => a2 => a1 > a2)(a1 => a2 => a1 > a2));
const $$get = /* #__PURE__ */ Data$dTuple.$Tuple(
  "get",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 2,
    op: dictMonadWithGraphAlloc => {
      const val = Val.val(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => dictMonadReader => dictLoadFile => v => v1 => {
          if (v1.tag === "Cons" && v1._1._3.tag === "Str" && v1._2.tag === "Cons" && v1._2._1._3.tag === "Dictionary" && v1._2._2.tag === "Nil") {
            const v2 = Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, v1._1._3._1, v1._2._1._3._1);
            if (v2.tag === "Nothing") {
              return val(v)(Data$dMap$dInternal.$$$Map("Node", 1, 1, v1._1._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(Val.$BaseVal(
                "Constr",
                "None",
                Data$dList$dTypes.Nil
              ));
            }
            if (v2.tag === "Just") {
              return val(v)(Data$dMap$dInternal.insert(Graph.ordVertex)(v2._1._1)()(Data$dMap$dInternal.$$$Map(
                "Node",
                1,
                1,
                v1._1._1,
                undefined,
                Data$dMap$dInternal.Leaf,
                Data$dMap$dInternal.Leaf
              )))(Val.$BaseVal("Constr", "Some", Data$dList$dTypes.$List("Cons", v2._1._2, Data$dList$dTypes.Nil)));
            }
            $runtime.fail();
          }
          return $$throw("String and dictionary expected");
        };
      };
    }
  })
);
const fromJson = dictMonadWithGraphAlloc => {
  const val = Val.val(dictMonadWithGraphAlloc);
  return dictMonadEffect => {
    const Monad0 = dictMonadEffect.Monad0();
    const $0 = Monad0.Bind1();
    const Applicative0 = Monad0.Applicative0();
    const traverse2 = Data$dTraversable.traversableArray.traverse(Applicative0);
    const traverse2$1 = Data$dTraversable.traversableArray.traverse(Applicative0);
    return doc_opt => Data$dArgonaut$dCore.caseJson(v => Effect$dException.throwException(Effect$dException.error("Error, Null JSON value cannot be converted to Val"))())(b => val(doc_opt)(Data$dMap$dInternal.Leaf)(Val.$BaseVal(
      "Constr",
      b ? "True" : "False",
      Data$dList$dTypes.Nil
    )))(n => {
      const v = Data$dInt.fromNumber(n);
      if (v.tag === "Just") { return val(doc_opt)(Data$dMap$dInternal.Leaf)(Val.$BaseVal("Int", v._1)); }
      if (v.tag === "Nothing") { return val(doc_opt)(Data$dMap$dInternal.Leaf)(Val.$BaseVal("Float", n)); }
      $runtime.fail();
    })(s => val(doc_opt)(Data$dMap$dInternal.Leaf)(Val.$BaseVal("Str", s)))(xs => {
      const toList = v => v1 => {
        if (v1.tag === "Nil") { return val(v)(Data$dMap$dInternal.Leaf)(Val.$BaseVal("Constr", "Nil", Data$dList$dTypes.Nil)); }
        if (v1.tag === "Cons") {
          const $1 = v1._1;
          return $0.bind(toList(Data$dMaybe.Nothing)(v1._2))(v$p => val(v)(Data$dMap$dInternal.Leaf)(Val.$BaseVal(
            "Constr",
            ":",
            Data$dList$dTypes.$List("Cons", $1, Data$dList$dTypes.$List("Cons", v$p, Data$dList$dTypes.Nil))
          )));
        }
        $runtime.fail();
      };
      return $0.bind(traverse2$1(fromJson(dictMonadWithGraphAlloc)(dictMonadEffect)(Data$dMaybe.Nothing))(xs))(vs => toList(doc_opt)((() => {
        const len = vs.length;
        const go = go$a0$copy => go$a1$copy => {
          let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
          while (go$c) {
            const source = go$a0, memo = go$a1;
            if (source < len) {
              go$a0 = source + 1 | 0;
              go$a1 = Data$dList$dTypes.$List("Cons", vs[source], memo);
              continue;
            }
            const go$1 = go$1$a0$copy => go$1$a1$copy => {
              let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
              while (go$1$c) {
                const b = go$1$a0, v = go$1$a1;
                if (v.tag === "Nil") {
                  go$1$c = false;
                  go$1$r = b;
                  continue;
                }
                if (v.tag === "Cons") {
                  go$1$a0 = Data$dList$dTypes.$List("Cons", v._1, b);
                  go$1$a1 = v._2;
                  continue;
                }
                $runtime.fail();
              }
              return go$1$r;
            };
            go$c = false;
            go$r = go$1(Data$dList$dTypes.Nil)(memo);
          }
          return go$r;
        };
        return go(0)(Data$dList$dTypes.Nil);
      })()));
    })(obj => $0.bind(traverse2(v => {
      const $1 = v._1;
      const $2 = v._2;
      return $0.bind(val(doc_opt)(Data$dMap$dInternal.Leaf)(Val.$BaseVal("Str", $1)))(v1 => {
        const $3 = v1._1;
        return $0.bind(fromJson(dictMonadWithGraphAlloc)(dictMonadEffect)(Data$dMaybe.Nothing)($2))(v2 => Applicative0.pure(Data$dTuple.$Tuple($1, Data$dTuple.$Tuple($3, v2))));
      });
    })(toUnfoldable(obj)))(entries => val(doc_opt)(Data$dMap$dInternal.Leaf)(Val.$BaseVal("Dictionary", fromFoldable(entries)))));
  };
};
const loadJson = /* #__PURE__ */ Data$dTuple.$Tuple(
  "load_json",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 1,
    op: dictMonadWithGraphAlloc => {
      const fromJson1 = fromJson(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => {
          const MonadEffect0 = dictMonadAff.MonadEffect0();
          const Bind1 = MonadEffect0.Monad0().Bind1();
          const fromJson2 = fromJson1(MonadEffect0);
          return dictMonadReader => dictLoadFile => {
            const loadFileFromPath = dictLoadFile.loadFileFromPath(dictMonadError)(dictMonadAff);
            return v => v1 => {
              if (v1.tag === "Cons" && v1._1._3.tag === "Str" && v1._2.tag === "Nil") {
                return Bind1.bind(Bind1.Apply0().Functor0().map(Util.definitely("File \"" + v1._1._3._1 + "\" exists"))(loadFileFromPath(v1._1._3._1)))(str => {
                  const v2 = Data$dArgonaut$dDecode$dParser.parseJson(str);
                  if (v2.tag === "Left") { return $$throw("Failed to parse JSON: " + Data$dArgonaut$dDecode$dError.showJsonDecodeError.show(v2._1)); }
                  if (v2.tag === "Right") { return fromJson2(v)(v2._1); }
                  $runtime.fail();
                });
              }
              return $$throw("String expected");
            };
          };
        };
      };
    }
  })
);
const foldl_with_index = /* #__PURE__ */ Data$dTuple.$Tuple(
  "foldl_with_index",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 3,
    op: dictMonadWithGraphAlloc => {
      const apply = Eval.apply(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => {
          const Monad0 = dictMonadAff.MonadEffect0().Monad0();
          const Bind1 = Monad0.Bind1();
          const $0 = Bind1.Apply0().Functor0();
          return dictMonadReader => {
            const apply1 = apply(dictMonadReader)(dictMonadAff);
            return dictLoadFile => {
              const apply2 = apply1(dictLoadFile);
              return v => v1 => {
                if (v1.tag === "Cons" && v1._2.tag === "Cons" && v1._2._2.tag === "Cons" && v1._2._2._1._3.tag === "Dictionary" && v1._2._2._2.tag === "Nil") {
                  const $1 = v1._1;
                  const go = go$a0$copy => go$a1$copy => {
                    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
                    while (go$c) {
                      const b = go$a0, v$1 = go$a1;
                      if (v$1.tag === "Nil") {
                        go$c = false;
                        go$r = b;
                        continue;
                      }
                      if (v$1.tag === "Cons") {
                        go$a0 = (() => {
                          const $2 = v$1._1;
                          return Monad0.Bind1().bind(b)(a => {
                            const $3 = a._1;
                            return $0.map(v5 => Data$dTuple.$Tuple(v5, Data$dMaybe.Nothing))(Bind1.bind(Bind1.bind(apply2(Data$dMaybe.Nothing)($1)(Val.$Val(
                              $2._2._1,
                              Data$dMaybe.Nothing,
                              Val.$BaseVal("Str", $2._1)
                            )))((() => {
                              const $4 = apply2(Data$dMaybe.Nothing);
                              return a$1 => $4(a$1)($3);
                            })()))((() => {
                              const $4 = apply2(a._2);
                              const $5 = $2._2._2;
                              return a$1 => $4(a$1)($5);
                            })()));
                          });
                        })();
                        go$a1 = v$1._2;
                        continue;
                      }
                      $runtime.fail();
                    }
                    return go$r;
                  };
                  return $0.map(Data$dTuple.fst)(go(Monad0.Applicative0().pure(Data$dTuple.$Tuple(v1._2._1, v)))(toUnfoldable2(v1._2._2._1._3._1)));
                }
                return $$throw("Function, value and dictionary expected");
              };
            };
          };
        };
      };
    }
  })
);
const find_str = /* #__PURE__ */ Data$dTuple.$Tuple(
  "find_str",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 2,
    op: dictMonadWithGraphAlloc => {
      const val = Val.val(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => dictMonadReader => dictLoadFile => v => v1 => {
          if (v1.tag === "Cons" && v1._1._3.tag === "Str" && v1._2.tag === "Cons" && v1._2._1._3.tag === "Str" && v1._2._2.tag === "Nil") {
            return val(v)(Data$dMap$dInternal.insert(Graph.ordVertex)(v1._2._1._1)()(Data$dMap$dInternal.$$$Map(
              "Node",
              1,
              1,
              v1._1._1,
              undefined,
              Data$dMap$dInternal.Leaf,
              Data$dMap$dInternal.Leaf
            )))(Val.$BaseVal(
              "Int",
              (() => {
                const $0 = Data$dString$dCodePoints.indexOf(v1._1._3._1)(v1._2._1._3._1);
                if ($0.tag === "Nothing") { return -1; }
                if ($0.tag === "Just") { return $0._1; }
                $runtime.fail();
              })()
            ));
          }
          return $$throw("Two strings expected");
        };
      };
    }
  })
);
const extern = dictBoundedJoinSemilattice => {
  const bot = dictBoundedJoinSemilattice.bot;
  return v => Data$dTuple.$Tuple(v._1, Val.$Val(bot, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(v._1, v._2), Data$dList$dTypes.Nil))));
};
const error_ = /* #__PURE__ */ Data$dTuple.$Tuple(
  "error",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 1,
    op: dictMonadWithGraphAlloc => dictMonadError => {
      const $$throw = Util.throw(dictMonadError.MonadThrow0());
      return dictMonadAff => dictMonadReader => dictLoadFile => v => v1 => {
        if (v1.tag === "Cons" && v1._1._3.tag === "Str" && v1._2.tag === "Nil") {
          return dictMonadAff.MonadEffect0().Monad0().Applicative0().pure(Effect$dException.throwException(Effect$dException.error(v1._1._3._1))());
        }
        return $$throw("String expected");
      };
    }
  })
);
const equals = /* #__PURE__ */ Primitive.union(Primitive.asBooleanBoolean)(Primitive.asBooleanBoolean)(Primitive.asIntNumberOrString)(Primitive.asIntNumberOrString)(Data$dEq.eqIntImpl)(/* #__PURE__ */ Primitive.unionStr(Primitive.asBooleanBoolean)(Primitive.asNumberString)(Data$dEq.eqNumberImpl)(Data$dEq.eqStringImpl));
const divide = /* #__PURE__ */ Primitive.union(Primitive.asNumberIntOrNumber)(Primitive.asNumberIntOrNumber)(Primitive.asIntNumber)(Primitive.asIntNumber)(x => y => Data$dInt.toNumber(x) / Data$dInt.toNumber(y))(Data$dEuclideanRing.numDiv);
const div = x => y => Data$dInt.unsafeClamp(Data$dNumber.floor(Data$dInt.toNumber(x) / Data$dInt.toNumber(y)));
const mod = x => y => x - (y * div(x)(y) | 0) | 0;
const dims = /* #__PURE__ */ Data$dTuple.$Tuple(
  "dims",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 1,
    op: dictMonadWithGraphAlloc => {
      const val = Val.val(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => {
          const $0 = dictMonadAff.MonadEffect0().Monad0().Bind1();
          return dictMonadReader => dictLoadFile => v => v1 => {
            if (v1.tag === "Cons" && v1._1._3.tag === "Matrix" && v1._2.tag === "Nil") {
              const $1 = v1._1._3._1._2._2._1;
              const $2 = v1._1._1;
              const $3 = v1._1._3._1._2._2._2;
              return $0.bind(val(Data$dMaybe.Nothing)(Data$dMap$dInternal.$$$Map("Node", 1, 1, v1._1._3._1._2._1._2, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(Val.$BaseVal(
                "Int",
                v1._1._3._1._2._1._1
              )))(v11 => $0.bind(val(Data$dMaybe.Nothing)(Data$dMap$dInternal.$$$Map("Node", 1, 1, $3, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(Val.$BaseVal(
                "Int",
                $1
              )))(v2 => val(v)(Data$dMap$dInternal.$$$Map("Node", 1, 1, $2, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(Val.$BaseVal(
                "Constr",
                "Pair",
                Data$dList$dTypes.$List("Cons", v11, Data$dList$dTypes.$List("Cons", v2, Data$dList$dTypes.Nil))
              ))));
            }
            return $$throw("Matrix expected");
          };
        };
      };
    }
  })
);
const dict_map = /* #__PURE__ */ Data$dTuple.$Tuple(
  "dict_map",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 2,
    op: dictMonadWithGraphAlloc => {
      const apply = Eval.apply(dictMonadWithGraphAlloc);
      const val = Val.val(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => {
          const Monad0 = dictMonadAff.MonadEffect0().Monad0();
          const Bind1 = Monad0.Bind1();
          const traverse2 = Dict.traversableDict.traverse(Monad0.Applicative0());
          return dictMonadReader => {
            const apply1 = apply(dictMonadReader)(dictMonadAff);
            return dictLoadFile => {
              const apply2 = apply1(dictLoadFile);
              return v => v1 => {
                if (v1.tag === "Cons" && v1._2.tag === "Cons" && v1._2._1._3.tag === "Dictionary" && v1._2._2.tag === "Nil") {
                  const $0 = v1._1;
                  const $1 = v1._2._1._1;
                  return Bind1.bind(traverse2(v3 => {
                    const $2 = v3._1;
                    return Bind1.Apply0().Functor0().map(v4 => Data$dTuple.$Tuple($2, v4))(apply2(Data$dMaybe.Nothing)($0)(v3._2));
                  })(v1._2._1._3._1))(d$p => val(v)(Data$dMap$dInternal.$$$Map("Node", 1, 1, $1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(Val.$BaseVal(
                    "Dictionary",
                    d$p
                  )));
                }
                return $$throw("Function and dictionary expected");
              };
            };
          };
        };
      };
    }
  })
);
const dict_intersectionWith = /* #__PURE__ */ Data$dTuple.$Tuple(
  "dict_intersectionWith",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 3,
    op: dictMonadWithGraphAlloc => {
      const apply = Eval.apply(dictMonadWithGraphAlloc);
      const val = Val.val(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => {
          const Monad0 = dictMonadAff.MonadEffect0().Monad0();
          const Bind1 = Monad0.Bind1();
          const Applicative0 = Monad0.Applicative0();
          const $0 = Bind1.Apply0().Functor0();
          return dictMonadReader => {
            const apply1 = apply(dictMonadReader)(dictMonadAff);
            return dictLoadFile => {
              const apply2 = apply1(dictLoadFile);
              return v => v1 => {
                if (
                  v1.tag === "Cons" && v1._2.tag === "Cons" && v1._2._1._3.tag === "Dictionary" && v1._2._2.tag === "Cons" && v1._2._2._1._3.tag === "Dictionary" && v1._2._2._2.tag === "Nil"
                ) {
                  const $1 = v1._1;
                  const $2 = v1._2._1._1;
                  const $3 = v1._2._2._1._1;
                  return Bind1.bind($0.map(Val.Dictionary)($0.map(Val.DictRep)(Dict.traversableDict.traverse(Applicative0)(Dict.identity)(Util$dMap.intersectionWith_Object(v3 => v4 => {
                    const $4 = v3._1;
                    const $5 = v4._1;
                    return Bind1.bind(Bind1.bind(apply2(Data$dMaybe.Nothing)($1)(v3._2))((() => {
                      const $6 = apply2(Data$dMaybe.Nothing);
                      const $7 = v4._2;
                      return a => $6(a)($7);
                    })()))(v5 => Bind1.bind(val(Data$dMaybe.Nothing)(Data$dMap$dInternal.insert(Graph.ordVertex)($5)()(Data$dMap$dInternal.$$$Map(
                      "Node",
                      1,
                      1,
                      $4,
                      undefined,
                      Data$dMap$dInternal.Leaf,
                      Data$dMap$dInternal.Leaf
                    )))(v5._3))(v6 => Applicative0.pure(Data$dTuple.$Tuple(v6._1, v5))));
                  })(v1._2._1._3._1)(v1._2._2._1._3._1)))))(v$p => val(v)(Data$dMap$dInternal.insert(Graph.ordVertex)($3)()(Data$dMap$dInternal.$$$Map(
                    "Node",
                    1,
                    1,
                    $2,
                    undefined,
                    Data$dMap$dInternal.Leaf,
                    Data$dMap$dInternal.Leaf
                  )))(v$p));
                }
                return $$throw("Function and two dictionaries expected");
              };
            };
          };
        };
      };
    }
  })
);
const dict_disjointUnion = /* #__PURE__ */ Data$dTuple.$Tuple(
  "dict_disjointUnion",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 2,
    op: dictMonadWithGraphAlloc => {
      const val = Val.val(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => dictMonadReader => dictLoadFile => v => v1 => {
          if (v1.tag === "Cons" && v1._1._3.tag === "Dictionary" && v1._2.tag === "Cons" && v1._2._1._3.tag === "Dictionary" && v1._2._2.tag === "Nil") {
            return val(v)(Data$dMap$dInternal.insert(Graph.ordVertex)(v1._2._1._1)()(Data$dMap$dInternal.$$$Map(
              "Node",
              1,
              1,
              v1._1._1,
              undefined,
              Data$dMap$dInternal.Leaf,
              Data$dMap$dInternal.Leaf
            )))(Val.$BaseVal("Dictionary", disjointUnion(v1._1._3._1)(v1._2._1._3._1)));
          }
          return $$throw("Dictionaries expected");
        };
      };
    }
  })
);
const dict_difference = /* #__PURE__ */ Data$dTuple.$Tuple(
  "dict_difference",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 2,
    op: dictMonadWithGraphAlloc => {
      const val = Val.val(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => dictMonadReader => dictLoadFile => v => v1 => {
          if (v1.tag === "Cons" && v1._1._3.tag === "Dictionary" && v1._2.tag === "Cons" && v1._2._1._3.tag === "Dictionary" && v1._2._2.tag === "Nil") {
            return val(v)(Data$dMap$dInternal.insert(Graph.ordVertex)(v1._2._1._1)()(Data$dMap$dInternal.$$$Map(
              "Node",
              1,
              1,
              v1._1._1,
              undefined,
              Data$dMap$dInternal.Leaf,
              Data$dMap$dInternal.Leaf
            )))(Val.$BaseVal("Dictionary", Util$dMap.mapFObjectString.difference(v1._1._3._1)(v1._2._1._3._1)));
          }
          return $$throw("Dictionaries expected.");
        };
      };
    }
  })
);
const dict = /* #__PURE__ */ Data$dTuple.$Tuple(
  "dict",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 1,
    op: dictMonadWithGraphAlloc => {
      const val = Val.val(dictMonadWithGraphAlloc);
      return dictMonadError => {
        const $$throw = Util.throw(dictMonadError.MonadThrow0());
        return dictMonadAff => dictMonadReader => dictLoadFile => v => v1 => {
          if (v1.tag === "Cons" && v1._2.tag === "Nil") {
            const kvs$p = dictMonadError1 => {
              const MonadThrow0 = dictMonadError1.MonadThrow0();
              const Monad0 = MonadThrow0.Monad0();
              const $0 = Monad0.Applicative0();
              return v3 => {
                if (v3._3.tag === "Constr") {
                  if (v3._3._2.tag === "Nil") {
                    if (v3._3._1 === "Nil") {
                      return $0.pure(Data$dTuple.$Tuple(
                        Data$dMap$dInternal.$$$Map("Node", 1, 1, v3._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf),
                        Data$dList$dTypes.Nil
                      ));
                    }
                    return MonadThrow0.throwError(Effect$dException.error("List of (key, value) pairs expected"));
                  }
                  if (
                    v3._3._2.tag === "Cons" && v3._3._2._1._3.tag === "Constr" && v3._3._2._1._3._2.tag === "Cons" && v3._3._2._1._3._2._1._3.tag === "Str" && v3._3._2._1._3._2._2.tag === "Cons" && v3._3._2._1._3._2._2._2.tag === "Nil" && v3._3._2._2.tag === "Cons" && v3._3._2._2._2.tag === "Nil" && v3._3._1 === ":" && v3._3._2._1._3._1 === "Pair"
                  ) {
                    const $1 = v3._3._2._1._3._2._1._3._1;
                    const $2 = v3._3._2._1._3._2._2._1;
                    const $3 = v3._1;
                    const $4 = v3._3._2._1._3._2._1._1;
                    const $5 = v3._3._2._1._1;
                    return Monad0.Bind1().bind(kvs$p(dictMonadError1)(v3._3._2._2._1))(v4 => $0.pure(Data$dTuple.$Tuple(
                      Data$dMap$dInternal.insert(Graph.ordVertex)($3)()(Data$dMap$dInternal.insert(Graph.ordVertex)($5)()(v4._1)),
                      Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple($1, Data$dTuple.$Tuple($4, $2)), v4._2)
                    )));
                  }
                }
                return MonadThrow0.throwError(Effect$dException.error("List of (key, value) pairs expected"));
              };
            };
            return dictMonadAff.MonadEffect0().Monad0().Bind1().bind(kvs$p(dictMonadError)(v1._1))(v3 => val(v)(v3._1)(Val.$BaseVal("Dictionary", fromFoldable1(v3._2))));
          }
          return $$throw("Single argument expected");
        };
      };
    }
  })
);
const debugLog = /* #__PURE__ */ Data$dTuple.$Tuple(
  "debug_log",
  /* #__PURE__ */ Val.$ForeignOp$p({
    arity: 1,
    op: dictMonadWithGraphAlloc => dictMonadError => {
      const $$throw = Util.throw(dictMonadError.MonadThrow0());
      return dictMonadAff => dictMonadReader => dictLoadFile => v => v1 => {
        if (v1.tag === "Cons" && v1._2.tag === "Nil") {
          const $0 = v1._1;
          return dictMonadAff.MonadEffect0().Monad0().Applicative0().pure(Debug._trace($0, v$1 => $0));
        }
        return $$throw("Single argument expected");
      };
    }
  })
);
const concat = Data$dSemigroup.concatString;
const primitives = /* #__PURE__ */ (() => fromFoldable([
  Data$dTuple.$Tuple(":", Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("PartialConstr", ":", Data$dList$dTypes.Nil)))),
  unary("ceiling")({i: Primitive.number, o: Primitive.int, fwd: Data$dInt.ceil}),
  Data$dTuple.$Tuple(
    debugLog._1,
    Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(debugLog._1, debugLog._2), Data$dList$dTypes.Nil)))
  ),
  Data$dTuple.$Tuple(dims._1, Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(dims._1, dims._2), Data$dList$dTypes.Nil)))),
  Data$dTuple.$Tuple(error_._1, Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(error_._1, error_._2), Data$dList$dTypes.Nil)))),
  Data$dTuple.$Tuple(
    loadJson._1,
    Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(loadJson._1, loadJson._2), Data$dList$dTypes.Nil)))
  ),
  unary("float")({
    i: Primitive.string,
    o: Primitive.number,
    fwd: x => Util.definitely("absurd")(Data$dNumber.fromStringImpl(x, Data$dNumber.isFinite, Data$dMaybe.Just, Data$dMaybe.Nothing))
  }),
  unary("floor")({i: Primitive.number, o: Primitive.int, fwd: Data$dInt.floor}),
  unary("log")({i: Primitive.intOrNumber, o: Primitive.number, fwd: log}),
  unary("num_to_str")({i: Primitive.intOrNumber, o: Primitive.string, fwd: numToStr}),
  binary("+")({
    i1: Primitive.intOrNumber,
    i2: Primitive.intOrNumber,
    o: Primitive.intOrNumber,
    fwd: Primitive.union(Primitive.asIntIntOrNumber)(Primitive.asNumberIntOrNumber)(Primitive.asIntNumber)(Primitive.asIntNumber)(Data$dSemiring.intAdd)(Data$dSemiring.numAdd)
  }),
  binary("-")({
    i1: Primitive.intOrNumber,
    i2: Primitive.intOrNumber,
    o: Primitive.intOrNumber,
    fwd: Primitive.union(Primitive.asIntIntOrNumber)(Primitive.asNumberIntOrNumber)(Primitive.asIntNumber)(Primitive.asIntNumber)(Data$dRing.intSub)(Data$dRing.numSub)
  }),
  binaryZero1("*")({
    i: Primitive.intOrNumber,
    o: Primitive.intOrNumber,
    fwd: Primitive.union(Primitive.asIntIntOrNumber)(Primitive.asNumberIntOrNumber)(Primitive.asIntNumber)(Primitive.asIntNumber)(Data$dSemiring.intMul)(Data$dSemiring.numMul)
  }),
  binaryZero1("**")({i: Primitive.intOrNumber, o: Primitive.intOrNumber, fwd: pow}),
  binaryZero1("/")({i: Primitive.intOrNumber, o: Primitive.intOrNumber, fwd: divide}),
  binary("==")({
    i1: Primitive.intOrNumberOrString,
    i2: Primitive.intOrNumberOrString,
    o: Primitive.boolean,
    fwd: Primitive.union(Primitive.asBooleanBoolean)(Primitive.asBooleanBoolean)(Primitive.asIntNumberOrString)(Primitive.asIntNumberOrString)(Data$dEq.eqIntImpl)(Primitive.unionStr(Primitive.asBooleanBoolean)(Primitive.asNumberString)(Data$dEq.eqNumberImpl)(Data$dEq.eqStringImpl))
  }),
  binary("/=")({i1: Primitive.intOrNumberOrString, i2: Primitive.intOrNumberOrString, o: Primitive.boolean, fwd: notEquals}),
  binary("<")({i1: Primitive.intOrNumberOrString, i2: Primitive.intOrNumberOrString, o: Primitive.boolean, fwd: lessThan}),
  binary(">")({i1: Primitive.intOrNumberOrString, i2: Primitive.intOrNumberOrString, o: Primitive.boolean, fwd: greaterThan}),
  binary("<=")({i1: Primitive.intOrNumberOrString, i2: Primitive.intOrNumberOrString, o: Primitive.boolean, fwd: lessThanEquals}),
  binary(">=")({i1: Primitive.intOrNumberOrString, i2: Primitive.intOrNumberOrString, o: Primitive.boolean, fwd: greaterThanEquals}),
  binary("++")({i1: Primitive.string, i2: Primitive.string, o: Primitive.string, fwd: Data$dSemigroup.concatString}),
  Data$dTuple.$Tuple(
    matrixLookup._1,
    Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(matrixLookup._1, matrixLookup._2), Data$dList$dTypes.Nil)))
  ),
  Data$dTuple.$Tuple(
    dict_difference._1,
    Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(dict_difference._1, dict_difference._2), Data$dList$dTypes.Nil)))
  ),
  Data$dTuple.$Tuple(
    dict_disjointUnion._1,
    Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(dict_disjointUnion._1, dict_disjointUnion._2), Data$dList$dTypes.Nil)))
  ),
  Data$dTuple.$Tuple(
    foldl_with_index._1,
    Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(foldl_with_index._1, foldl_with_index._2), Data$dList$dTypes.Nil)))
  ),
  Data$dTuple.$Tuple($$get._1, Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple($$get._1, $$get._2), Data$dList$dTypes.Nil)))),
  Data$dTuple.$Tuple(insert._1, Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(insert._1, insert._2), Data$dList$dTypes.Nil)))),
  Data$dTuple.$Tuple(
    dict_intersectionWith._1,
    Val.$Val(
      undefined,
      Data$dMaybe.Nothing,
      Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(dict_intersectionWith._1, dict_intersectionWith._2), Data$dList$dTypes.Nil))
    )
  ),
  Data$dTuple.$Tuple(
    dict_map._1,
    Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(dict_map._1, dict_map._2), Data$dList$dTypes.Nil)))
  ),
  Data$dTuple.$Tuple(dict._1, Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(dict._1, dict._2), Data$dList$dTypes.Nil)))),
  Data$dTuple.$Tuple(
    matrixUpdate._1,
    Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(matrixUpdate._1, matrixUpdate._2), Data$dList$dTypes.Nil)))
  ),
  Data$dTuple.$Tuple(
    find_str._1,
    Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(find_str._1, find_str._2), Data$dList$dTypes.Nil)))
  ),
  Data$dTuple.$Tuple(search._1, Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(search._1, search._2), Data$dList$dTypes.Nil)))),
  Data$dTuple.$Tuple(split._1, Val.$Val(undefined, Data$dMaybe.Nothing, Val.$BaseVal("Fun", Val.$Fun("Foreign", Data$dTuple.$Tuple(split._1, split._2), Data$dList$dTypes.Nil)))),
  binaryZero2("//")({i: Primitive.int, o: Primitive.int, fwd: div}),
  binaryZero2("%")({i: Primitive.int, o: Primitive.int, fwd: mod}),
  binaryZero2("quot")({i: Primitive.int, o: Primitive.int, fwd: Data$dInt.quot}),
  binaryZero2("rem")({i: Primitive.int, o: Primitive.int, fwd: Data$dInt.rem})
]))();
export {
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  primitives,
  
  
  
  
  
  
  
  
};
