(() => {
  // output-es/runtime.js
  function binding(init2) {
    let state = 0;
    let value;
    return () => {
      if (state === 2) {
        return value;
      }
      if (state === 1) {
        throw new Error("Binding demanded before initialized");
      }
      state = 1;
      value = init2();
      state = 2;
      return value;
    };
  }
  function fail() {
    throw new Error("Failed pattern match");
  }

  // output-es/Type.Proxy/index.js
  var $Proxy = () => ({ tag: "Proxy" });
  var $$Proxy = /* @__PURE__ */ $Proxy();

  // output-es/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showNumberImpl = function(n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };
  var showCharImpl = function(c) {
    var code = c.charCodeAt(0);
    if (code < 32 || code === 127) {
      switch (c) {
        case "\x07":
          return "'\\a'";
        case "\b":
          return "'\\b'";
        case "\f":
          return "'\\f'";
        case "\n":
          return "'\\n'";
        case "\r":
          return "'\\r'";
        case "	":
          return "'\\t'";
        case "\v":
          return "'\\v'";
      }
      return "'\\" + code.toString(10) + "'";
    }
    return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      function(c, i) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i + 1;
        var empty5 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty5;
      }
    ) + '"';
  };
  var showArrayImpl = function(f) {
    return function(xs) {
      var ss = [];
      for (var i = 0, l = xs.length; i < l; i++) {
        ss[i] = f(xs[i]);
      }
      return "[" + ss.join(",") + "]";
    };
  };
  var cons = function(head) {
    return function(tail) {
      return [head].concat(tail);
    };
  };
  var intercalate = function(separator) {
    return function(xs) {
      return xs.join(separator);
    };
  };

  // output-es/Data.Show/index.js
  var showString = { show: showStringImpl };
  var showNumber = { show: showNumberImpl };
  var showInt = { show: showIntImpl };

  // output-es/Data.Ordering/index.js
  var $Ordering = (tag) => ({ tag });
  var LT = /* @__PURE__ */ $Ordering("LT");
  var GT = /* @__PURE__ */ $Ordering("GT");
  var EQ = /* @__PURE__ */ $Ordering("EQ");

  // output-es/Data.Unit/foreign.js
  var unit = void 0;

  // output-es/Data.Maybe/index.js
  var $Maybe = (tag, _1) => ({ tag, _1 });
  var Nothing = /* @__PURE__ */ $Maybe("Nothing");
  var Just = (value0) => $Maybe("Just", value0);
  var isNothing = (v2) => {
    if (v2.tag === "Nothing") {
      return true;
    }
    if (v2.tag === "Just") {
      return false;
    }
    fail();
  };
  var functorMaybe = {
    map: (v) => (v1) => {
      if (v1.tag === "Just") {
        return $Maybe("Just", v(v1._1));
      }
      return Nothing;
    }
  };
  var applyMaybe = {
    apply: (v) => (v1) => {
      if (v.tag === "Just") {
        if (v1.tag === "Just") {
          return $Maybe("Just", v._1(v1._1));
        }
        return Nothing;
      }
      if (v.tag === "Nothing") {
        return Nothing;
      }
      fail();
    },
    Functor0: () => functorMaybe
  };
  var applicativeMaybe = { pure: Just, Apply0: () => applyMaybe };

  // output-es/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var ceil = Math.ceil;
  var floor = Math.floor;
  var log = Math.log;
  var pow = function(n) {
    return function(p) {
      return Math.pow(n, p);
    };
  };

  // output-es/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };
  var quot = function(x2) {
    return function(y2) {
      return x2 / y2 | 0;
    };
  };
  var rem = function(x2) {
    return function(y2) {
      return x2 % y2;
    };
  };

  // output-es/Data.Int/index.js
  var fromNumber = /* @__PURE__ */ fromNumberImpl(Just)(Nothing);
  var unsafeClamp = (x2) => {
    if (!isFiniteImpl(x2)) {
      return 0;
    }
    if (x2 >= toNumber(2147483647)) {
      return 2147483647;
    }
    if (x2 <= toNumber(-2147483648)) {
      return -2147483648;
    }
    const $1 = fromNumber(x2);
    if ($1.tag === "Nothing") {
      return 0;
    }
    if ($1.tag === "Just") {
      return $1._1;
    }
    fail();
  };
  var floor2 = (x2) => unsafeClamp(floor(x2));
  var ceil2 = (x2) => unsafeClamp(ceil(x2));

  // output-es/Data.Function/index.js
  var $$const = (a) => (v) => a;
  var applyFlipped = (x2) => (f) => f(x2);

  // output-es/Control.Semigroupoid/index.js
  var semigroupoidFn = { compose: (f) => (g) => (x2) => f(g(x2)) };

  // output-es/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output-es/Data.Functor/index.js
  var functorArray = { map: arrayMap };

  // output-es/Control.Apply/index.js
  var identity = (x2) => x2;

  // output-es/Control.Bind/index.js
  var identity2 = (x2) => x2;

  // output-es/Data.Either/index.js
  var $Either = (tag, _1) => ({ tag, _1 });
  var Left = (value0) => $Either("Left", value0);
  var Right = (value0) => $Either("Right", value0);
  var functorEither = {
    map: (f) => (m) => {
      if (m.tag === "Left") {
        return $Either("Left", m._1);
      }
      if (m.tag === "Right") {
        return $Either("Right", f(m._1));
      }
      fail();
    }
  };
  var choose = (dictAlt) => {
    const map1 = dictAlt.Functor0().map;
    return (a) => (b) => dictAlt.alt(map1(Left)(a))(map1(Right)(b));
  };
  var applyEither = {
    apply: (v) => (v1) => {
      if (v.tag === "Left") {
        return $Either("Left", v._1);
      }
      if (v.tag === "Right") {
        if (v1.tag === "Left") {
          return $Either("Left", v1._1);
        }
        if (v1.tag === "Right") {
          return $Either("Right", v._1(v1._1));
        }
        fail();
      }
      fail();
    },
    Functor0: () => functorEither
  };
  var bindEither = {
    bind: (v2) => {
      if (v2.tag === "Left") {
        return (v) => $Either("Left", v2._1);
      }
      if (v2.tag === "Right") {
        return (f) => f(v2._1);
      }
      fail();
    },
    Apply0: () => applyEither
  };
  var applicativeEither = { pure: Right, Apply0: () => applyEither };
  var monadEither = { Applicative0: () => applicativeEither, Bind1: () => bindEither };

  // output-es/Data.Identity/index.js
  var Identity = (x2) => x2;
  var functorIdentity = { map: (f) => (m) => f(m) };
  var applyIdentity = { apply: (v) => (v1) => v(v1), Functor0: () => functorIdentity };
  var bindIdentity = { bind: (v) => (f) => f(v), Apply0: () => applyIdentity };
  var applicativeIdentity = { pure: Identity, Apply0: () => applyIdentity };
  var monadIdentity = { Applicative0: () => applicativeIdentity, Bind1: () => bindIdentity };

  // output-es/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };

  // output-es/Effect/index.js
  var applyEffect = {
    apply: (f) => (a) => () => {
      const f$p = f();
      const a$p = a();
      return applicativeEffect.pure(f$p(a$p))();
    },
    Functor0: () => functorEffect
  };
  var applicativeEffect = { pure: pureE, Apply0: () => applyEffect };
  var functorEffect = {
    map: (f) => (a) => () => {
      const a$p = a();
      return f(a$p);
    }
  };

  // output-es/Control.Monad.Rec.Class/index.js
  var $Step = (tag, _1) => ({ tag, _1 });
  var Loop = (value0) => $Step("Loop", value0);
  var monadRecIdentity = {
    tailRecM: (f) => {
      const go = (go$a0$copy) => {
        let go$a0 = go$a0$copy, go$c = true, go$r;
        while (go$c) {
          const v = go$a0;
          if (v.tag === "Loop") {
            go$a0 = f(v._1);
            continue;
          }
          if (v.tag === "Done") {
            go$c = false;
            go$r = v._1;
            continue;
          }
          fail();
        }
        ;
        return go$r;
      };
      return (x2) => go(f(x2));
    },
    Monad0: () => monadIdentity
  };
  var bifunctorStep = {
    bimap: (v) => (v1) => (v2) => {
      if (v2.tag === "Loop") {
        return $Step("Loop", v(v2._1));
      }
      if (v2.tag === "Done") {
        return $Step("Done", v1(v2._1));
      }
      fail();
    }
  };

  // output-es/Data.NonEmpty/index.js
  var $NonEmpty = (_1, _2) => ({ tag: "NonEmpty", _1, _2 });
  var NonEmpty = (value0) => (value1) => $NonEmpty(value0, value1);
  var functorNonEmpty = (dictFunctor) => ({ map: (f) => (m) => $NonEmpty(f(m._1), dictFunctor.map(f)(m._2)) });
  var traversableNonEmpty = (dictTraversable) => {
    const functorNonEmpty1 = functorNonEmpty(dictTraversable.Functor0());
    const $2 = dictTraversable.Foldable1();
    const foldableNonEmpty1 = {
      foldMap: (dictMonoid) => {
        const append1 = dictMonoid.Semigroup0().append;
        const foldMap1 = $2.foldMap(dictMonoid);
        return (f) => (v) => append1(f(v._1))(foldMap1(f)(v._2));
      },
      foldl: (f) => (b) => (v) => $2.foldl(f)(f(b)(v._1))(v._2),
      foldr: (f) => (b) => (v) => f(v._1)($2.foldr(f)(b)(v._2))
    };
    return {
      sequence: (dictApplicative) => {
        const Apply0 = dictApplicative.Apply0();
        const map22 = Apply0.Functor0().map;
        const sequence12 = dictTraversable.sequence(dictApplicative);
        return (v) => Apply0.apply(map22(NonEmpty)(v._1))(sequence12(v._2));
      },
      traverse: (dictApplicative) => {
        const Apply0 = dictApplicative.Apply0();
        const map22 = Apply0.Functor0().map;
        const traverse13 = dictTraversable.traverse(dictApplicative);
        return (f) => (v) => Apply0.apply(map22(NonEmpty)(f(v._1)))(traverse13(f)(v._2));
      },
      Functor0: () => functorNonEmpty1,
      Foldable1: () => foldableNonEmpty1
    };
  };
  var foldable1NonEmpty = (dictFoldable) => {
    const foldableNonEmpty1 = {
      foldMap: (dictMonoid) => {
        const append1 = dictMonoid.Semigroup0().append;
        const foldMap1 = dictFoldable.foldMap(dictMonoid);
        return (f) => (v) => append1(f(v._1))(foldMap1(f)(v._2));
      },
      foldl: (f) => (b) => (v) => dictFoldable.foldl(f)(f(b)(v._1))(v._2),
      foldr: (f) => (b) => (v) => f(v._1)(dictFoldable.foldr(f)(b)(v._2))
    };
    return {
      foldMap1: (dictSemigroup) => (f) => (v) => dictFoldable.foldl((s) => (a1) => dictSemigroup.append(s)(f(a1)))(f(v._1))(v._2),
      foldr1: (f) => (v) => {
        const $4 = f(v._1);
        const $5 = dictFoldable.foldr((a1) => {
          const $6 = f(a1);
          return (x2) => $Maybe(
            "Just",
            (() => {
              if (x2.tag === "Nothing") {
                return a1;
              }
              if (x2.tag === "Just") {
                return $6(x2._1);
              }
              fail();
            })()
          );
        })(Nothing)(v._2);
        if ($5.tag === "Nothing") {
          return v._1;
        }
        if ($5.tag === "Just") {
          return $4($5._1);
        }
        fail();
      },
      foldl1: (f) => (v) => dictFoldable.foldl(f)(v._1)(v._2),
      Foldable0: () => foldableNonEmpty1
    };
  };

  // output-es/Data.Tuple/index.js
  var $Tuple = (_1, _2) => ({ tag: "Tuple", _1, _2 });
  var Tuple = (value0) => (value1) => $Tuple(value0, value1);
  var snd = (v) => v._2;
  var functorTuple = { map: (f) => (m) => $Tuple(m._1, f(m._2)) };
  var fst = (v) => v._1;
  var ordTuple = (dictOrd) => {
    const $1 = dictOrd.Eq0();
    return (dictOrd1) => {
      const $3 = dictOrd1.Eq0();
      const eqTuple2 = { eq: (x2) => (y2) => $1.eq(x2._1)(y2._1) && $3.eq(x2._2)(y2._2) };
      return {
        compare: (x2) => (y2) => {
          const v = dictOrd.compare(x2._1)(y2._1);
          if (v.tag === "LT") {
            return LT;
          }
          if (v.tag === "GT") {
            return GT;
          }
          return dictOrd1.compare(x2._2)(y2._2);
        },
        Eq0: () => eqTuple2
      };
    };
  };

  // output-es/Data.List.Types/index.js
  var $List = (tag, _1, _2) => ({ tag, _1, _2 });
  var identity3 = (x2) => x2;
  var Nil = /* @__PURE__ */ $List("Nil");
  var Cons = (value0) => (value1) => $List("Cons", value0, value1);
  var listMap = (f) => {
    const chunkedRevMap = (chunkedRevMap$a0$copy) => (chunkedRevMap$a1$copy) => {
      let chunkedRevMap$a0 = chunkedRevMap$a0$copy, chunkedRevMap$a1 = chunkedRevMap$a1$copy, chunkedRevMap$c = true, chunkedRevMap$r;
      while (chunkedRevMap$c) {
        const chunksAcc = chunkedRevMap$a0, v = chunkedRevMap$a1;
        const $4 = (chunksAcc1, xs) => {
          const reverseUnrolledMap = (reverseUnrolledMap$a0$copy) => (reverseUnrolledMap$a1$copy) => {
            let reverseUnrolledMap$a0 = reverseUnrolledMap$a0$copy, reverseUnrolledMap$a1 = reverseUnrolledMap$a1$copy, reverseUnrolledMap$c = true, reverseUnrolledMap$r;
            while (reverseUnrolledMap$c) {
              const v1 = reverseUnrolledMap$a0, acc = reverseUnrolledMap$a1;
              if (v1.tag === "Cons") {
                if (v1._1.tag === "Cons") {
                  if (v1._1._2.tag === "Cons") {
                    if (v1._1._2._2.tag === "Cons") {
                      reverseUnrolledMap$a0 = v1._2;
                      reverseUnrolledMap$a1 = $List("Cons", f(v1._1._1), $List("Cons", f(v1._1._2._1), $List("Cons", f(v1._1._2._2._1), acc)));
                      continue;
                    }
                    reverseUnrolledMap$c = false;
                    reverseUnrolledMap$r = acc;
                    continue;
                  }
                  reverseUnrolledMap$c = false;
                  reverseUnrolledMap$r = acc;
                  continue;
                }
                reverseUnrolledMap$c = false;
                reverseUnrolledMap$r = acc;
                continue;
              }
              reverseUnrolledMap$c = false;
              reverseUnrolledMap$r = acc;
              continue;
            }
            ;
            return reverseUnrolledMap$r;
          };
          return reverseUnrolledMap(chunksAcc1)((() => {
            if (xs.tag === "Cons") {
              if (xs._2.tag === "Cons") {
                if (xs._2._2.tag === "Nil") {
                  return $List("Cons", f(xs._1), $List("Cons", f(xs._2._1), Nil));
                }
                return Nil;
              }
              if (xs._2.tag === "Nil") {
                return $List("Cons", f(xs._1), Nil);
              }
              return Nil;
            }
            return Nil;
          })());
        };
        if (v.tag === "Cons") {
          if (v._2.tag === "Cons") {
            if (v._2._2.tag === "Cons") {
              chunkedRevMap$a0 = $List("Cons", v, chunksAcc);
              chunkedRevMap$a1 = v._2._2._2;
              continue;
            }
            chunkedRevMap$c = false;
            chunkedRevMap$r = $4(chunksAcc, v);
            continue;
          }
          chunkedRevMap$c = false;
          chunkedRevMap$r = $4(chunksAcc, v);
          continue;
        }
        chunkedRevMap$c = false;
        chunkedRevMap$r = $4(chunksAcc, v);
        continue;
      }
      ;
      return chunkedRevMap$r;
    };
    return chunkedRevMap(Nil);
  };
  var functorList = { map: listMap };
  var functorNonEmptyList = /* @__PURE__ */ functorNonEmpty(functorList);
  var foldableList = {
    foldr: (f) => (b) => {
      const $2 = foldableList.foldl((b$1) => (a) => f(a)(b$1))(b);
      const $3 = (() => {
        const go = (go$a0$copy) => (go$a1$copy) => {
          let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
          while (go$c) {
            const acc = go$a0, v = go$a1;
            if (v.tag === "Nil") {
              go$c = false;
              go$r = acc;
              continue;
            }
            if (v.tag === "Cons") {
              go$a0 = $List("Cons", v._1, acc);
              go$a1 = v._2;
              continue;
            }
            fail();
          }
          ;
          return go$r;
        };
        return go(Nil);
      })();
      return (x2) => $2($3(x2));
    },
    foldl: (f) => {
      const go = (go$a0$copy) => (go$a1$copy) => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
        while (go$c) {
          const b = go$a0, v = go$a1;
          if (v.tag === "Nil") {
            go$c = false;
            go$r = b;
            continue;
          }
          if (v.tag === "Cons") {
            go$a0 = f(b)(v._1);
            go$a1 = v._2;
            continue;
          }
          fail();
        }
        ;
        return go$r;
      };
      return go;
    },
    foldMap: (dictMonoid) => {
      const append2 = dictMonoid.Semigroup0().append;
      return (f) => foldableList.foldl((acc) => {
        const $4 = append2(acc);
        return (x2) => $4(f(x2));
      })(dictMonoid.mempty);
    }
  };
  var foldableNonEmptyList = {
    foldMap: (dictMonoid) => {
      const append1 = dictMonoid.Semigroup0().append;
      const foldMap1 = foldableList.foldMap(dictMonoid);
      return (f) => (v) => append1(f(v._1))(foldMap1(f)(v._2));
    },
    foldl: (f) => (b) => (v) => {
      const go = (go$a0$copy) => (go$a1$copy) => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
        while (go$c) {
          const b$1 = go$a0, v$1 = go$a1;
          if (v$1.tag === "Nil") {
            go$c = false;
            go$r = b$1;
            continue;
          }
          if (v$1.tag === "Cons") {
            go$a0 = f(b$1)(v$1._1);
            go$a1 = v$1._2;
            continue;
          }
          fail();
        }
        ;
        return go$r;
      };
      return go(f(b)(v._1))(v._2);
    },
    foldr: (f) => (b) => (v) => f(v._1)(foldableList.foldr(f)(b)(v._2))
  };
  var semigroupNonEmptyList = { append: (v) => (as$p) => $NonEmpty(v._1, foldableList.foldr(Cons)($List("Cons", as$p._1, as$p._2))(v._2)) };
  var traversableList = {
    traverse: (dictApplicative) => {
      const Apply0 = dictApplicative.Apply0();
      const map1 = Apply0.Functor0().map;
      const map5 = Apply0.Functor0().map;
      return (f) => {
        const $5 = map1((() => {
          const go = (go$a0$copy) => (go$a1$copy) => {
            let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
            while (go$c) {
              const b = go$a0, v = go$a1;
              if (v.tag === "Nil") {
                go$c = false;
                go$r = b;
                continue;
              }
              if (v.tag === "Cons") {
                go$a0 = $List("Cons", v._1, b);
                go$a1 = v._2;
                continue;
              }
              fail();
            }
            ;
            return go$r;
          };
          return go(Nil);
        })());
        const $6 = (() => {
          const go = (go$a0$copy) => (go$a1$copy) => {
            let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
            while (go$c) {
              const b = go$a0, v = go$a1;
              if (v.tag === "Nil") {
                go$c = false;
                go$r = b;
                continue;
              }
              if (v.tag === "Cons") {
                go$a0 = Apply0.apply(map5((b$1) => (a) => $List("Cons", a, b$1))(b))(f(v._1));
                go$a1 = v._2;
                continue;
              }
              fail();
            }
            ;
            return go$r;
          };
          return go(dictApplicative.pure(Nil));
        })();
        return (x2) => $5($6(x2));
      };
    },
    sequence: (dictApplicative) => traversableList.traverse(dictApplicative)(identity3),
    Functor0: () => functorList,
    Foldable1: () => foldableList
  };
  var traversableNonEmptyList = /* @__PURE__ */ traversableNonEmpty(traversableList);
  var unfoldable1List = {
    unfoldr1: (f) => (b) => {
      const go = (go$a0$copy) => (go$a1$copy) => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
        while (go$c) {
          const source2 = go$a0, memo = go$a1;
          const v = f(source2);
          if (v._2.tag === "Just") {
            go$a0 = v._2._1;
            go$a1 = $List("Cons", v._1, memo);
            continue;
          }
          if (v._2.tag === "Nothing") {
            const go$1 = (go$1$a0$copy) => (go$1$a1$copy) => {
              let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
              while (go$1$c) {
                const b$1 = go$1$a0, v$1 = go$1$a1;
                if (v$1.tag === "Nil") {
                  go$1$c = false;
                  go$1$r = b$1;
                  continue;
                }
                if (v$1.tag === "Cons") {
                  go$1$a0 = $List("Cons", v$1._1, b$1);
                  go$1$a1 = v$1._2;
                  continue;
                }
                fail();
              }
              ;
              return go$1$r;
            };
            go$c = false;
            go$r = go$1(Nil)($List("Cons", v._1, memo));
            continue;
          }
          fail();
        }
        ;
        return go$r;
      };
      return go(b)(Nil);
    }
  };
  var unfoldableList = {
    unfoldr: (f) => (b) => {
      const go = (go$a0$copy) => (go$a1$copy) => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
        while (go$c) {
          const source2 = go$a0, memo = go$a1;
          const v = f(source2);
          if (v.tag === "Nothing") {
            const go$1 = (go$1$a0$copy) => (go$1$a1$copy) => {
              let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
              while (go$1$c) {
                const b$1 = go$1$a0, v$1 = go$1$a1;
                if (v$1.tag === "Nil") {
                  go$1$c = false;
                  go$1$r = b$1;
                  continue;
                }
                if (v$1.tag === "Cons") {
                  go$1$a0 = $List("Cons", v$1._1, b$1);
                  go$1$a1 = v$1._2;
                  continue;
                }
                fail();
              }
              ;
              return go$1$r;
            };
            go$c = false;
            go$r = go$1(Nil)(memo);
            continue;
          }
          if (v.tag === "Just") {
            go$a0 = v._1._2;
            go$a1 = $List("Cons", v._1._1, memo);
            continue;
          }
          fail();
        }
        ;
        return go$r;
      };
      return go(b)(Nil);
    },
    Unfoldable10: () => unfoldable1List
  };
  var applyList = {
    apply: (v) => (v1) => {
      if (v.tag === "Nil") {
        return Nil;
      }
      if (v.tag === "Cons") {
        return foldableList.foldr(Cons)(applyList.apply(v._2)(v1))(listMap(v._1)(v1));
      }
      fail();
    },
    Functor0: () => functorList
  };
  var applyNonEmptyList = {
    apply: (v) => (v1) => $NonEmpty(
      v._1(v1._1),
      foldableList.foldr(Cons)(applyList.apply($List("Cons", v._1, v._2))(v1._2))(applyList.apply(v._2)($List("Cons", v1._1, Nil)))
    ),
    Functor0: () => functorNonEmptyList
  };
  var bindList = {
    bind: (v) => (v1) => {
      if (v.tag === "Nil") {
        return Nil;
      }
      if (v.tag === "Cons") {
        return foldableList.foldr(Cons)(bindList.bind(v._2)(v1))(v1(v._1));
      }
      fail();
    },
    Apply0: () => applyList
  };
  var bindNonEmptyList = {
    bind: (v) => (f) => {
      const v1 = f(v._1);
      return $NonEmpty(
        v1._1,
        foldableList.foldr(Cons)(bindList.bind(v._2)((x2) => {
          const $4 = f(x2);
          return $List("Cons", $4._1, $4._2);
        }))(v1._2)
      );
    },
    Apply0: () => applyNonEmptyList
  };
  var applicativeList = { pure: (a) => $List("Cons", a, Nil), Apply0: () => applyList };

  // output-es/Data.List/index.js
  var identity4 = (x2) => x2;
  var updateAt = (v) => (v1) => (v2) => {
    if (v2.tag === "Cons") {
      if (v === 0) {
        return $Maybe("Just", $List("Cons", v1, v2._2));
      }
      const $3 = updateAt(v - 1 | 0)(v1)(v2._2);
      if ($3.tag === "Just") {
        return $Maybe("Just", $List("Cons", v2._1, $3._1));
      }
      return Nothing;
    }
    return Nothing;
  };
  var unzip = /* @__PURE__ */ (() => foldableList.foldr((v) => (v1) => $Tuple(
    $List("Cons", v._1, v1._1),
    $List("Cons", v._2, v1._2)
  ))($Tuple(Nil, Nil)))();
  var toUnfoldable = (dictUnfoldable) => dictUnfoldable.unfoldr((xs) => {
    if (xs.tag === "Nil") {
      return Nothing;
    }
    if (xs.tag === "Cons") {
      return $Maybe("Just", $Tuple(xs._1, xs._2));
    }
    fail();
  });
  var span = (v) => (v1) => {
    if (v1.tag === "Cons") {
      if (v(v1._1)) {
        const v2 = span(v)(v1._2);
        return { init: $List("Cons", v1._1, v2.init), rest: v2.rest };
      }
      return { init: Nil, rest: v1 };
    }
    return { init: Nil, rest: v1 };
  };
  var sortBy = (cmp) => {
    const merge = (v) => (v1) => {
      if (v.tag === "Cons") {
        if (v1.tag === "Cons") {
          if (cmp(v._1)(v1._1).tag === "GT") {
            return $List("Cons", v1._1, merge(v)(v1._2));
          }
          return $List("Cons", v._1, merge(v._2)(v1));
        }
        if (v1.tag === "Nil") {
          return v;
        }
        fail();
      }
      if (v.tag === "Nil") {
        return v1;
      }
      if (v1.tag === "Nil") {
        return v;
      }
      fail();
    };
    const mergePairs = (v) => {
      if (v.tag === "Cons") {
        if (v._2.tag === "Cons") {
          return $List("Cons", merge(v._1)(v._2._1), mergePairs(v._2._2));
        }
        return v;
      }
      return v;
    };
    const mergeAll = (mergeAll$a0$copy) => {
      let mergeAll$a0 = mergeAll$a0$copy, mergeAll$c = true, mergeAll$r;
      while (mergeAll$c) {
        const v = mergeAll$a0;
        if (v.tag === "Cons") {
          if (v._2.tag === "Nil") {
            mergeAll$c = false;
            mergeAll$r = v._1;
            continue;
          }
          mergeAll$a0 = mergePairs(v);
          continue;
        }
        mergeAll$a0 = mergePairs(v);
        continue;
      }
      ;
      return mergeAll$r;
    };
    const $sequedesceascen = ($sequedesceascen$b$copy, $sequedesceascen$a0$copy, $sequedesceascen$a1$copy, $sequedesceascen$a2$copy) => {
      let $sequedesceascen$b = $sequedesceascen$b$copy;
      let $sequedesceascen$a0 = $sequedesceascen$a0$copy;
      let $sequedesceascen$a1 = $sequedesceascen$a1$copy;
      let $sequedesceascen$a2 = $sequedesceascen$a2$copy;
      let $sequedesceascen$c = true;
      let $sequedesceascen$r;
      while ($sequedesceascen$c) {
        if ($sequedesceascen$b === 0) {
          const v = $sequedesceascen$a0;
          if (v.tag === "Cons") {
            if (v._2.tag === "Cons") {
              if (cmp(v._1)(v._2._1).tag === "GT") {
                $sequedesceascen$b = 1;
                $sequedesceascen$a0 = v._2._1;
                $sequedesceascen$a1 = $List("Cons", v._1, Nil);
                $sequedesceascen$a2 = v._2._2;
                continue;
              }
              $sequedesceascen$b = 2;
              $sequedesceascen$a0 = v._2._1;
              $sequedesceascen$a1 = (v1) => $List("Cons", v._1, v1);
              $sequedesceascen$a2 = v._2._2;
              continue;
            }
            $sequedesceascen$c = false;
            $sequedesceascen$r = $List("Cons", v, Nil);
            continue;
          }
          $sequedesceascen$c = false;
          $sequedesceascen$r = $List("Cons", v, Nil);
          continue;
        }
        if ($sequedesceascen$b === 1) {
          const a = $sequedesceascen$a0, as = $sequedesceascen$a1, v = $sequedesceascen$a2;
          if (v.tag === "Cons") {
            if (cmp(a)(v._1).tag === "GT") {
              $sequedesceascen$b = 1;
              $sequedesceascen$a0 = v._1;
              $sequedesceascen$a1 = $List("Cons", a, as);
              $sequedesceascen$a2 = v._2;
              continue;
            }
            $sequedesceascen$c = false;
            $sequedesceascen$r = $List("Cons", $List("Cons", a, as), sequences(v));
            continue;
          }
          $sequedesceascen$c = false;
          $sequedesceascen$r = $List("Cons", $List("Cons", a, as), sequences(v));
          continue;
        }
        if ($sequedesceascen$b === 2) {
          const a = $sequedesceascen$a0, as = $sequedesceascen$a1, v = $sequedesceascen$a2;
          if (v.tag === "Cons") {
            if ((() => {
              const $8 = cmp(a)(v._1);
              return $8.tag === "LT" || !($8.tag === "GT");
            })()) {
              $sequedesceascen$b = 2;
              $sequedesceascen$a0 = v._1;
              $sequedesceascen$a1 = (ys) => as($List("Cons", a, ys));
              $sequedesceascen$a2 = v._2;
              continue;
            }
            $sequedesceascen$c = false;
            $sequedesceascen$r = $List("Cons", as($List("Cons", a, Nil)), sequences(v));
            continue;
          }
          $sequedesceascen$c = false;
          $sequedesceascen$r = $List("Cons", as($List("Cons", a, Nil)), sequences(v));
          continue;
        }
      }
      ;
      return $sequedesceascen$r;
    };
    const sequences = (v) => $sequedesceascen(0, v);
    const descending = (a) => (as) => (v) => $sequedesceascen(1, a, as, v);
    const ascending3 = (a) => (as) => (v) => $sequedesceascen(2, a, as, v);
    return (x2) => mergeAll(sequences(x2));
  };
  var reverse = /* @__PURE__ */ (() => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const acc = go$a0, v = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = acc;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = $List("Cons", v._1, acc);
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      ;
      return go$r;
    };
    return go(Nil);
  })();
  var unsnoc = (lst) => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0, v1 = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = Nothing;
          continue;
        }
        if (v.tag === "Cons") {
          if (v._2.tag === "Nil") {
            go$c = false;
            go$r = $Maybe("Just", { revInit: v1, last: v._1 });
            continue;
          }
          go$a0 = v._2;
          go$a1 = $List("Cons", v._1, v1);
          continue;
        }
        fail();
      }
      ;
      return go$r;
    };
    const $2 = go(lst)(Nil);
    if ($2.tag === "Just") {
      return $Maybe("Just", { init: reverse($2._1.revInit), last: $2._1.last });
    }
    return Nothing;
  };
  var zipWith = (f) => (xs) => (ys) => {
    const go = (go$a0$copy) => (go$a1$copy) => (go$a2$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0, v1 = go$a1, acc = go$a2;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = acc;
          continue;
        }
        if (v1.tag === "Nil") {
          go$c = false;
          go$r = acc;
          continue;
        }
        if (v.tag === "Cons") {
          if (v1.tag === "Cons") {
            go$a0 = v._2;
            go$a1 = v1._2;
            go$a2 = $List("Cons", f(v._1)(v1._1), acc);
            continue;
          }
          fail();
        }
        fail();
      }
      ;
      return go$r;
    };
    return reverse(go(xs)(ys)(Nil));
  };
  var range = (start2) => (end) => {
    if (start2 === end) {
      return $List("Cons", start2, Nil);
    }
    const go = (go$a0$copy) => (go$a1$copy) => (go$a2$copy) => (go$a3$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$a3 = go$a3$copy, go$c = true, go$r;
      while (go$c) {
        const s = go$a0, e = go$a1, step = go$a2, rest = go$a3;
        if (s === e) {
          go$c = false;
          go$r = $List("Cons", s, rest);
          continue;
        }
        go$a0 = s + step | 0;
        go$a1 = e;
        go$a2 = step;
        go$a3 = $List("Cons", s, rest);
        continue;
      }
      ;
      return go$r;
    };
    return go(end)(start2)((() => {
      if (start2 > end) {
        return 1;
      }
      return -1;
    })())(Nil);
  };
  var manyRec = (dictMonadRec) => {
    const bind1 = dictMonadRec.Monad0().Bind1().bind;
    return (dictAlternative) => {
      const Alt0 = dictAlternative.Plus1().Alt0();
      const map1 = Alt0.Functor0().map;
      const pure2 = dictAlternative.Applicative0().pure;
      return (p) => dictMonadRec.tailRecM((acc) => bind1(Alt0.alt(map1(Loop)(p))(pure2($Step("Done", unit))))((aa) => pure2(bifunctorStep.bimap((v) => $List(
        "Cons",
        v,
        acc
      ))((v) => reverse(acc))(aa))))(Nil);
    };
  };
  var some = (dictAlternative) => {
    const apply3 = dictAlternative.Applicative0().Apply0().apply;
    const map1 = dictAlternative.Plus1().Alt0().Functor0().map;
    return (dictLazy) => (v) => apply3(map1(Cons)(v))(dictLazy.defer((v1) => many(dictAlternative)(dictLazy)(v)));
  };
  var many = (dictAlternative) => {
    const alt2 = dictAlternative.Plus1().Alt0().alt;
    const pure2 = dictAlternative.Applicative0().pure;
    return (dictLazy) => (v) => alt2(some(dictAlternative)(dictLazy)(v))(pure2(Nil));
  };
  var index = (index$a0$copy) => (index$a1$copy) => {
    let index$a0 = index$a0$copy, index$a1 = index$a1$copy, index$c = true, index$r;
    while (index$c) {
      const v = index$a0, v1 = index$a1;
      if (v.tag === "Nil") {
        index$c = false;
        index$r = Nothing;
        continue;
      }
      if (v.tag === "Cons") {
        if (v1 === 0) {
          index$c = false;
          index$r = $Maybe("Just", v._1);
          continue;
        }
        index$a0 = v._2;
        index$a1 = v1 - 1 | 0;
        continue;
      }
      fail();
    }
    ;
    return index$r;
  };
  var groupBy = (v) => (v1) => {
    if (v1.tag === "Nil") {
      return Nil;
    }
    if (v1.tag === "Cons") {
      const v2 = span(v(v1._1))(v1._2);
      return $List("Cons", $NonEmpty(v1._1, v2.init), groupBy(v)(v2.rest));
    }
    fail();
  };
  var deleteBy = (v) => (v1) => (v2) => {
    if (v2.tag === "Nil") {
      return Nil;
    }
    if (v2.tag === "Cons") {
      if (v(v1)(v2._1)) {
        return v2._2;
      }
      return $List("Cons", v2._1, deleteBy(v)(v1)(v2._2));
    }
    fail();
  };
  var difference = (dictEq) => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, v = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = deleteBy(dictEq.eq)(v._1)(b);
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      ;
      return go$r;
    };
    return go;
  };

  // output-es/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output-es/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init2) {
      return function(xs) {
        var acc = init2;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init2) {
      return function(xs) {
        var acc = init2;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output-es/Data.Foldable/index.js
  var identity5 = (x2) => x2;
  var monoidEndo = /* @__PURE__ */ (() => {
    const semigroupEndo1 = { append: (v) => (v1) => (x2) => v(v1(x2)) };
    return { mempty: (x2) => x2, Semigroup0: () => semigroupEndo1 };
  })();
  var traverse_ = (dictApplicative) => {
    const $1 = dictApplicative.Apply0();
    const map5 = $1.Functor0().map;
    return (dictFoldable) => (f) => dictFoldable.foldr((x2) => {
      const $6 = f(x2);
      return (b) => $1.apply(map5((v) => identity)($6))(b);
    })(dictApplicative.pure(unit));
  };
  var foldableTuple = { foldr: (f) => (z) => (v) => f(v._2)(z), foldl: (f) => (z) => (v) => f(z)(v._2), foldMap: (dictMonoid) => (f) => (v) => f(v._2) };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: (dictMonoid) => {
      const append = dictMonoid.Semigroup0().append;
      return (f) => foldableArray.foldr((x2) => (acc) => append(f(x2))(acc))(dictMonoid.mempty);
    }
  };
  var foldrDefault = (dictFoldable) => {
    const foldMap2 = dictFoldable.foldMap(monoidEndo);
    return (c) => (u) => (xs) => foldMap2((x2) => c(x2))(xs)(u);
  };

  // output-es/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;
  var eqNumberImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;
  var eqArrayImpl = function(f) {
    return function(xs) {
      return function(ys) {
        if (xs.length !== ys.length)
          return false;
        for (var i = 0; i < xs.length; i++) {
          if (!f(xs[i])(ys[i]))
            return false;
        }
        return true;
      };
    };
  };

  // output-es/Data.Eq/index.js
  var eqUnit = { eq: (v) => (v1) => true };
  var eqString = { eq: eqStringImpl };
  var eqNumber = { eq: eqNumberImpl };
  var eqInt = { eq: eqIntImpl };
  var eqChar = { eq: eqCharImpl };

  // output-es/Data.Lazy/foreign.js
  var defer = function(thunk) {
    var v = null;
    return function() {
      if (thunk === void 0)
        return v;
      v = thunk();
      thunk = void 0;
      return v;
    };
  };
  var force = function(l) {
    return l();
  };

  // output-es/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq2) {
      return function(gt) {
        return function(x2) {
          return function(y2) {
            return x2 < y2 ? lt : x2 === y2 ? eq2 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;

  // output-es/Data.Ord/index.js
  var ordString = { compare: /* @__PURE__ */ ordStringImpl(LT)(EQ)(GT), Eq0: () => eqString };
  var ordInt = { compare: /* @__PURE__ */ ordIntImpl(LT)(EQ)(GT), Eq0: () => eqInt };

  // output-es/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust3) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value = b;
              while (true) {
                var tuple = f(value);
                result.push(fst2(tuple));
                var maybe = snd2(tuple);
                if (isNothing2(maybe))
                  return result;
                value = fromJust3(maybe);
              }
            };
          };
        };
      };
    };
  };

  // output-es/Data.Unfoldable1/index.js
  var fromJust = (v) => {
    if (v.tag === "Just") {
      return v._1;
    }
    fail();
  };
  var unfoldable1Array = { unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust)(fst)(snd) };

  // output-es/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust3) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value = b;
              while (true) {
                var maybe = f(value);
                if (isNothing2(maybe))
                  return result;
                var tuple = fromJust3(maybe);
                result.push(fst2(tuple));
                value = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output-es/Data.Unfoldable/index.js
  var fromJust2 = (v) => {
    if (v.tag === "Just") {
      return v._1;
    }
    fail();
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust2)(fst)(snd),
    Unfoldable10: () => unfoldable1Array
  };
  var replicate = (dictUnfoldable) => (n) => (v) => dictUnfoldable.unfoldr((i) => {
    if (i <= 0) {
      return Nothing;
    }
    return $Maybe("Just", $Tuple(v, i - 1 | 0));
  })(n);

  // output-es/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output-es/Data.Map.Internal/index.js
  var $KickUp = (_1, _2, _3, _4) => ({ tag: "KickUp", _1, _2, _3, _4 });
  var $Map = (tag, _1, _2, _3, _4, _5, _6, _7) => ({ tag, _1, _2, _3, _4, _5, _6, _7 });
  var $TreeContext = (tag, _1, _2, _3, _4, _5, _6) => ({ tag, _1, _2, _3, _4, _5, _6 });
  var Leaf2 = /* @__PURE__ */ $Map("Leaf");
  var toUnfoldable2 = (dictUnfoldable) => (m) => {
    const go = (go$a0$copy) => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = Nothing;
          continue;
        }
        if (v.tag === "Cons") {
          if (v._1.tag === "Leaf") {
            go$a0 = v._2;
            continue;
          }
          if (v._1.tag === "Two") {
            if (v._1._1.tag === "Leaf") {
              if (v._1._4.tag === "Leaf") {
                go$c = false;
                go$r = $Maybe("Just", $Tuple($Tuple(v._1._2, v._1._3), v._2));
                continue;
              }
              go$c = false;
              go$r = $Maybe("Just", $Tuple($Tuple(v._1._2, v._1._3), $List("Cons", v._1._4, v._2)));
              continue;
            }
            go$a0 = $List(
              "Cons",
              v._1._1,
              $List("Cons", $Map("Two", Leaf2, v._1._2, v._1._3, Leaf2), $List("Cons", v._1._4, v._2))
            );
            continue;
          }
          if (v._1.tag === "Three") {
            go$a0 = $List(
              "Cons",
              v._1._1,
              $List(
                "Cons",
                $Map("Two", Leaf2, v._1._2, v._1._3, Leaf2),
                $List("Cons", v._1._4, $List("Cons", $Map("Two", Leaf2, v._1._5, v._1._6, Leaf2), $List("Cons", v._1._7, v._2)))
              )
            );
            continue;
          }
          fail();
        }
        fail();
      }
      ;
      return go$r;
    };
    return dictUnfoldable.unfoldr(go)($List("Cons", m, Nil));
  };
  var lookup = (dictOrd) => (k) => {
    const go = (go$a0$copy) => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v.tag === "Leaf") {
          go$c = false;
          go$r = Nothing;
          continue;
        }
        if (v.tag === "Two") {
          const v2 = dictOrd.compare(k)(v._2);
          if (v2.tag === "EQ") {
            go$c = false;
            go$r = $Maybe("Just", v._3);
            continue;
          }
          if (v2.tag === "LT") {
            go$a0 = v._1;
            continue;
          }
          go$a0 = v._4;
          continue;
        }
        if (v.tag === "Three") {
          const v3 = dictOrd.compare(k)(v._2);
          if (v3.tag === "EQ") {
            go$c = false;
            go$r = $Maybe("Just", v._3);
            continue;
          }
          const v4 = dictOrd.compare(k)(v._5);
          if (v4.tag === "EQ") {
            go$c = false;
            go$r = $Maybe("Just", v._6);
            continue;
          }
          if (v3.tag === "LT") {
            go$a0 = v._1;
            continue;
          }
          if (v4.tag === "GT") {
            go$a0 = v._7;
            continue;
          }
          go$a0 = v._4;
          continue;
        }
        fail();
      }
      ;
      return go$r;
    };
    return go;
  };
  var fromZipper = (fromZipper$a0$copy) => (fromZipper$a1$copy) => (fromZipper$a2$copy) => {
    let fromZipper$a0 = fromZipper$a0$copy, fromZipper$a1 = fromZipper$a1$copy, fromZipper$a2 = fromZipper$a2$copy, fromZipper$c = true, fromZipper$r;
    while (fromZipper$c) {
      const dictOrd = fromZipper$a0, v = fromZipper$a1, tree = fromZipper$a2;
      if (v.tag === "Nil") {
        fromZipper$c = false;
        fromZipper$r = tree;
        continue;
      }
      if (v.tag === "Cons") {
        if (v._1.tag === "TwoLeft") {
          fromZipper$a0 = dictOrd;
          fromZipper$a1 = v._2;
          fromZipper$a2 = $Map("Two", tree, v._1._1, v._1._2, v._1._3);
          continue;
        }
        if (v._1.tag === "TwoRight") {
          fromZipper$a0 = dictOrd;
          fromZipper$a1 = v._2;
          fromZipper$a2 = $Map("Two", v._1._1, v._1._2, v._1._3, tree);
          continue;
        }
        if (v._1.tag === "ThreeLeft") {
          fromZipper$a0 = dictOrd;
          fromZipper$a1 = v._2;
          fromZipper$a2 = $Map("Three", tree, v._1._1, v._1._2, v._1._3, v._1._4, v._1._5, v._1._6);
          continue;
        }
        if (v._1.tag === "ThreeMiddle") {
          fromZipper$a0 = dictOrd;
          fromZipper$a1 = v._2;
          fromZipper$a2 = $Map("Three", v._1._1, v._1._2, v._1._3, tree, v._1._4, v._1._5, v._1._6);
          continue;
        }
        if (v._1.tag === "ThreeRight") {
          fromZipper$a0 = dictOrd;
          fromZipper$a1 = v._2;
          fromZipper$a2 = $Map("Three", v._1._1, v._1._2, v._1._3, v._1._4, v._1._5, v._1._6, tree);
          continue;
        }
        fail();
      }
      fail();
    }
    ;
    return fromZipper$r;
  };
  var insert = (dictOrd) => (k) => (v) => {
    const up = (up$a0$copy) => (up$a1$copy) => {
      let up$a0 = up$a0$copy, up$a1 = up$a1$copy, up$c = true, up$r;
      while (up$c) {
        const v1 = up$a0, v2 = up$a1;
        if (v1.tag === "Nil") {
          up$c = false;
          up$r = $Map("Two", v2._1, v2._2, v2._3, v2._4);
          continue;
        }
        if (v1.tag === "Cons") {
          if (v1._1.tag === "TwoLeft") {
            up$c = false;
            up$r = fromZipper(dictOrd)(v1._2)($Map("Three", v2._1, v2._2, v2._3, v2._4, v1._1._1, v1._1._2, v1._1._3));
            continue;
          }
          if (v1._1.tag === "TwoRight") {
            up$c = false;
            up$r = fromZipper(dictOrd)(v1._2)($Map("Three", v1._1._1, v1._1._2, v1._1._3, v2._1, v2._2, v2._3, v2._4));
            continue;
          }
          if (v1._1.tag === "ThreeLeft") {
            up$a0 = v1._2;
            up$a1 = $KickUp($Map("Two", v2._1, v2._2, v2._3, v2._4), v1._1._1, v1._1._2, $Map("Two", v1._1._3, v1._1._4, v1._1._5, v1._1._6));
            continue;
          }
          if (v1._1.tag === "ThreeMiddle") {
            up$a0 = v1._2;
            up$a1 = $KickUp($Map("Two", v1._1._1, v1._1._2, v1._1._3, v2._1), v2._2, v2._3, $Map("Two", v2._4, v1._1._4, v1._1._5, v1._1._6));
            continue;
          }
          if (v1._1.tag === "ThreeRight") {
            up$a0 = v1._2;
            up$a1 = $KickUp($Map("Two", v1._1._1, v1._1._2, v1._1._3, v1._1._4), v1._1._5, v1._1._6, $Map("Two", v2._1, v2._2, v2._3, v2._4));
            continue;
          }
          fail();
        }
        fail();
      }
      ;
      return up$r;
    };
    const down = (down$a0$copy) => (down$a1$copy) => {
      let down$a0 = down$a0$copy, down$a1 = down$a1$copy, down$c = true, down$r;
      while (down$c) {
        const ctx = down$a0, v1 = down$a1;
        if (v1.tag === "Leaf") {
          down$c = false;
          down$r = up(ctx)($KickUp(Leaf2, k, v, Leaf2));
          continue;
        }
        if (v1.tag === "Two") {
          const v2 = dictOrd.compare(k)(v1._2);
          if (v2.tag === "EQ") {
            down$c = false;
            down$r = fromZipper(dictOrd)(ctx)($Map("Two", v1._1, k, v, v1._4));
            continue;
          }
          if (v2.tag === "LT") {
            down$a0 = $List("Cons", $TreeContext("TwoLeft", v1._2, v1._3, v1._4), ctx);
            down$a1 = v1._1;
            continue;
          }
          down$a0 = $List("Cons", $TreeContext("TwoRight", v1._1, v1._2, v1._3), ctx);
          down$a1 = v1._4;
          continue;
        }
        if (v1.tag === "Three") {
          const v3 = dictOrd.compare(k)(v1._2);
          if (v3.tag === "EQ") {
            down$c = false;
            down$r = fromZipper(dictOrd)(ctx)($Map("Three", v1._1, k, v, v1._4, v1._5, v1._6, v1._7));
            continue;
          }
          const v4 = dictOrd.compare(k)(v1._5);
          if (v4.tag === "EQ") {
            down$c = false;
            down$r = fromZipper(dictOrd)(ctx)($Map("Three", v1._1, v1._2, v1._3, v1._4, k, v, v1._7));
            continue;
          }
          if (v3.tag === "LT") {
            down$a0 = $List("Cons", $TreeContext("ThreeLeft", v1._2, v1._3, v1._4, v1._5, v1._6, v1._7), ctx);
            down$a1 = v1._1;
            continue;
          }
          if (v3.tag === "GT") {
            if (v4.tag === "LT") {
              down$a0 = $List("Cons", $TreeContext("ThreeMiddle", v1._1, v1._2, v1._3, v1._5, v1._6, v1._7), ctx);
              down$a1 = v1._4;
              continue;
            }
            down$a0 = $List("Cons", $TreeContext("ThreeRight", v1._1, v1._2, v1._3, v1._4, v1._5, v1._6), ctx);
            down$a1 = v1._7;
            continue;
          }
          down$a0 = $List("Cons", $TreeContext("ThreeRight", v1._1, v1._2, v1._3, v1._4, v1._5, v1._6), ctx);
          down$a1 = v1._7;
          continue;
        }
        fail();
      }
      ;
      return down$r;
    };
    return down(Nil);
  };
  var pop = (dictOrd) => (k) => {
    const up = (up$a0$copy) => (up$a1$copy) => {
      let up$a0 = up$a0$copy, up$a1 = up$a1$copy, up$c = true, up$r;
      while (up$c) {
        const ctxs = up$a0, tree = up$a1;
        if (ctxs.tag === "Nil") {
          up$c = false;
          up$r = tree;
          continue;
        }
        if (ctxs.tag === "Cons") {
          const $5 = (a, b, c, d, k1, k2, k3, v1, v2, v3) => fromZipper(dictOrd)(ctxs._2)($Map("Two", $Map("Two", a, k1, v1, b), k2, v2, $Map("Two", c, k3, v3, d)));
          const $6 = (a, b, c, d, k1, k2, k3, v1, v2, v3) => fromZipper(dictOrd)(ctxs._2)($Map("Two", $Map("Two", a, k1, v1, b), k2, v2, $Map("Two", c, k3, v3, d)));
          const $7 = (a, b, c, d, k1, k2, k3, v1, v2, v3) => fromZipper(dictOrd)(ctxs._2)($Map("Two", $Map("Three", a, k1, v1, b, k2, v2, c), k3, v3, d));
          const $8 = (a, b, c, d, k1, k2, k3, v1, v2, v3) => fromZipper(dictOrd)(ctxs._2)($Map("Two", $Map("Three", a, k1, v1, b, k2, v2, c), k3, v3, d));
          const $9 = (a, b, c, d, k1, k2, k3, v1, v2, v3) => fromZipper(dictOrd)(ctxs._2)($Map("Two", a, k1, v1, $Map("Three", b, k2, v2, c, k3, v3, d)));
          const $10 = (a, b, c, d, k1, k2, k3, v1, v2, v3) => fromZipper(dictOrd)(ctxs._2)($Map("Two", a, k1, v1, $Map("Three", b, k2, v2, c, k3, v3, d)));
          const $11 = (a, b, c, d, e, k1, k2, k3, k4, v1, v2, v3, v4) => fromZipper(dictOrd)(ctxs._2)($Map(
            "Three",
            $Map("Two", a, k1, v1, b),
            k2,
            v2,
            $Map("Two", c, k3, v3, d),
            k4,
            v4,
            e
          ));
          const $12 = (a, b, c, d, e, k1, k2, k3, k4, v1, v2, v3, v4) => fromZipper(dictOrd)(ctxs._2)($Map(
            "Three",
            $Map("Two", a, k1, v1, b),
            k2,
            v2,
            $Map("Two", c, k3, v3, d),
            k4,
            v4,
            e
          ));
          const $13 = (a, b, c, d, e, k1, k2, k3, k4, v1, v2, v3, v4) => fromZipper(dictOrd)(ctxs._2)($Map(
            "Three",
            a,
            k1,
            v1,
            $Map("Two", b, k2, v2, c),
            k3,
            v3,
            $Map("Two", d, k4, v4, e)
          ));
          const $14 = (a, b, c, d, e, k1, k2, k3, k4, v1, v2, v3, v4) => fromZipper(dictOrd)(ctxs._2)($Map(
            "Three",
            a,
            k1,
            v1,
            $Map("Two", b, k2, v2, c),
            k3,
            v3,
            $Map("Two", d, k4, v4, e)
          ));
          if (tree.tag === "Leaf") {
            if (ctxs._1.tag === "TwoLeft") {
              if (ctxs._1._3.tag === "Leaf") {
                up$c = false;
                up$r = fromZipper(dictOrd)(ctxs._2)($Map("Two", Leaf2, ctxs._1._1, ctxs._1._2, Leaf2));
                continue;
              }
              if (ctxs._1._3.tag === "Two") {
                up$a0 = ctxs._2;
                up$a1 = $Map("Three", tree, ctxs._1._1, ctxs._1._2, ctxs._1._3._1, ctxs._1._3._2, ctxs._1._3._3, ctxs._1._3._4);
                continue;
              }
              if (ctxs._1._3.tag === "Three") {
                up$c = false;
                up$r = $5(tree, ctxs._1._3._1, ctxs._1._3._4, ctxs._1._3._7, ctxs._1._1, ctxs._1._3._2, ctxs._1._3._5, ctxs._1._2, ctxs._1._3._3, ctxs._1._3._6);
                continue;
              }
              up$c = false;
              up$r = _crashWith("The impossible happened in partial function `up`.");
              continue;
            }
            if (ctxs._1.tag === "TwoRight") {
              if (ctxs._1._1.tag === "Leaf") {
                up$c = false;
                up$r = fromZipper(dictOrd)(ctxs._2)($Map("Two", Leaf2, ctxs._1._2, ctxs._1._3, Leaf2));
                continue;
              }
              if (ctxs._1._1.tag === "Two") {
                up$a0 = ctxs._2;
                up$a1 = $Map("Three", ctxs._1._1._1, ctxs._1._1._2, ctxs._1._1._3, ctxs._1._1._4, ctxs._1._2, ctxs._1._3, tree);
                continue;
              }
              if (ctxs._1._1.tag === "Three") {
                up$c = false;
                up$r = $6(ctxs._1._1._1, ctxs._1._1._4, ctxs._1._1._7, tree, ctxs._1._1._2, ctxs._1._1._5, ctxs._1._2, ctxs._1._1._3, ctxs._1._1._6, ctxs._1._3);
                continue;
              }
              up$c = false;
              up$r = _crashWith("The impossible happened in partial function `up`.");
              continue;
            }
            if (ctxs._1.tag === "ThreeLeft") {
              if (ctxs._1._6.tag === "Leaf") {
                if (ctxs._1._3.tag === "Leaf") {
                  up$c = false;
                  up$r = fromZipper(dictOrd)(ctxs._2)($Map("Three", Leaf2, ctxs._1._1, ctxs._1._2, Leaf2, ctxs._1._4, ctxs._1._5, Leaf2));
                  continue;
                }
                if (ctxs._1._3.tag === "Two") {
                  up$c = false;
                  up$r = $7(tree, ctxs._1._3._1, ctxs._1._3._4, ctxs._1._6, ctxs._1._1, ctxs._1._3._2, ctxs._1._4, ctxs._1._2, ctxs._1._3._3, ctxs._1._5);
                  continue;
                }
                if (ctxs._1._3.tag === "Three") {
                  up$c = false;
                  up$r = $11(
                    tree,
                    ctxs._1._3._1,
                    ctxs._1._3._4,
                    ctxs._1._3._7,
                    ctxs._1._6,
                    ctxs._1._1,
                    ctxs._1._3._2,
                    ctxs._1._3._5,
                    ctxs._1._4,
                    ctxs._1._2,
                    ctxs._1._3._3,
                    ctxs._1._3._6,
                    ctxs._1._5
                  );
                  continue;
                }
                up$c = false;
                up$r = _crashWith("The impossible happened in partial function `up`.");
                continue;
              }
              if (ctxs._1._3.tag === "Two") {
                up$c = false;
                up$r = $7(tree, ctxs._1._3._1, ctxs._1._3._4, ctxs._1._6, ctxs._1._1, ctxs._1._3._2, ctxs._1._4, ctxs._1._2, ctxs._1._3._3, ctxs._1._5);
                continue;
              }
              if (ctxs._1._3.tag === "Three") {
                up$c = false;
                up$r = $11(
                  tree,
                  ctxs._1._3._1,
                  ctxs._1._3._4,
                  ctxs._1._3._7,
                  ctxs._1._6,
                  ctxs._1._1,
                  ctxs._1._3._2,
                  ctxs._1._3._5,
                  ctxs._1._4,
                  ctxs._1._2,
                  ctxs._1._3._3,
                  ctxs._1._3._6,
                  ctxs._1._5
                );
                continue;
              }
              up$c = false;
              up$r = _crashWith("The impossible happened in partial function `up`.");
              continue;
            }
            if (ctxs._1.tag === "ThreeMiddle") {
              if (ctxs._1._1.tag === "Leaf") {
                if (ctxs._1._6.tag === "Leaf") {
                  up$c = false;
                  up$r = fromZipper(dictOrd)(ctxs._2)($Map("Three", Leaf2, ctxs._1._2, ctxs._1._3, Leaf2, ctxs._1._4, ctxs._1._5, Leaf2));
                  continue;
                }
                if (ctxs._1._6.tag === "Two") {
                  up$c = false;
                  up$r = $9(ctxs._1._1, tree, ctxs._1._6._1, ctxs._1._6._4, ctxs._1._2, ctxs._1._4, ctxs._1._6._2, ctxs._1._3, ctxs._1._5, ctxs._1._6._3);
                  continue;
                }
                if (ctxs._1._6.tag === "Three") {
                  up$c = false;
                  up$r = $13(
                    ctxs._1._1,
                    tree,
                    ctxs._1._6._1,
                    ctxs._1._6._4,
                    ctxs._1._6._7,
                    ctxs._1._2,
                    ctxs._1._4,
                    ctxs._1._6._2,
                    ctxs._1._6._5,
                    ctxs._1._3,
                    ctxs._1._5,
                    ctxs._1._6._3,
                    ctxs._1._6._6
                  );
                  continue;
                }
                up$c = false;
                up$r = _crashWith("The impossible happened in partial function `up`.");
                continue;
              }
              if (ctxs._1._1.tag === "Two") {
                up$c = false;
                up$r = $8(ctxs._1._1._1, ctxs._1._1._4, tree, ctxs._1._6, ctxs._1._1._2, ctxs._1._2, ctxs._1._4, ctxs._1._1._3, ctxs._1._3, ctxs._1._5);
                continue;
              }
              if (ctxs._1._6.tag === "Two") {
                up$c = false;
                up$r = $9(ctxs._1._1, tree, ctxs._1._6._1, ctxs._1._6._4, ctxs._1._2, ctxs._1._4, ctxs._1._6._2, ctxs._1._3, ctxs._1._5, ctxs._1._6._3);
                continue;
              }
              if (ctxs._1._1.tag === "Three") {
                up$c = false;
                up$r = $12(
                  ctxs._1._1._1,
                  ctxs._1._1._4,
                  ctxs._1._1._7,
                  tree,
                  ctxs._1._6,
                  ctxs._1._1._2,
                  ctxs._1._1._5,
                  ctxs._1._2,
                  ctxs._1._4,
                  ctxs._1._1._3,
                  ctxs._1._1._6,
                  ctxs._1._3,
                  ctxs._1._5
                );
                continue;
              }
              if (ctxs._1._6.tag === "Three") {
                up$c = false;
                up$r = $13(
                  ctxs._1._1,
                  tree,
                  ctxs._1._6._1,
                  ctxs._1._6._4,
                  ctxs._1._6._7,
                  ctxs._1._2,
                  ctxs._1._4,
                  ctxs._1._6._2,
                  ctxs._1._6._5,
                  ctxs._1._3,
                  ctxs._1._5,
                  ctxs._1._6._3,
                  ctxs._1._6._6
                );
                continue;
              }
              up$c = false;
              up$r = _crashWith("The impossible happened in partial function `up`.");
              continue;
            }
            if (ctxs._1.tag === "ThreeRight") {
              if (ctxs._1._1.tag === "Leaf") {
                if (ctxs._1._4.tag === "Leaf") {
                  up$c = false;
                  up$r = fromZipper(dictOrd)(ctxs._2)($Map("Three", Leaf2, ctxs._1._2, ctxs._1._3, Leaf2, ctxs._1._5, ctxs._1._6, Leaf2));
                  continue;
                }
                if (ctxs._1._4.tag === "Two") {
                  up$c = false;
                  up$r = $10(ctxs._1._1, ctxs._1._4._1, ctxs._1._4._4, tree, ctxs._1._2, ctxs._1._4._2, ctxs._1._5, ctxs._1._3, ctxs._1._4._3, ctxs._1._6);
                  continue;
                }
                if (ctxs._1._4.tag === "Three") {
                  up$c = false;
                  up$r = $14(
                    ctxs._1._1,
                    ctxs._1._4._1,
                    ctxs._1._4._4,
                    ctxs._1._4._7,
                    tree,
                    ctxs._1._2,
                    ctxs._1._4._2,
                    ctxs._1._4._5,
                    ctxs._1._5,
                    ctxs._1._3,
                    ctxs._1._4._3,
                    ctxs._1._4._6,
                    ctxs._1._6
                  );
                  continue;
                }
                up$c = false;
                up$r = _crashWith("The impossible happened in partial function `up`.");
                continue;
              }
              if (ctxs._1._4.tag === "Two") {
                up$c = false;
                up$r = $10(ctxs._1._1, ctxs._1._4._1, ctxs._1._4._4, tree, ctxs._1._2, ctxs._1._4._2, ctxs._1._5, ctxs._1._3, ctxs._1._4._3, ctxs._1._6);
                continue;
              }
              if (ctxs._1._4.tag === "Three") {
                up$c = false;
                up$r = $14(
                  ctxs._1._1,
                  ctxs._1._4._1,
                  ctxs._1._4._4,
                  ctxs._1._4._7,
                  tree,
                  ctxs._1._2,
                  ctxs._1._4._2,
                  ctxs._1._4._5,
                  ctxs._1._5,
                  ctxs._1._3,
                  ctxs._1._4._3,
                  ctxs._1._4._6,
                  ctxs._1._6
                );
                continue;
              }
              up$c = false;
              up$r = _crashWith("The impossible happened in partial function `up`.");
              continue;
            }
            up$c = false;
            up$r = _crashWith("The impossible happened in partial function `up`.");
            continue;
          }
          if (ctxs._1.tag === "TwoLeft") {
            if (ctxs._1._3.tag === "Two") {
              up$a0 = ctxs._2;
              up$a1 = $Map("Three", tree, ctxs._1._1, ctxs._1._2, ctxs._1._3._1, ctxs._1._3._2, ctxs._1._3._3, ctxs._1._3._4);
              continue;
            }
            if (ctxs._1._3.tag === "Three") {
              up$c = false;
              up$r = $5(tree, ctxs._1._3._1, ctxs._1._3._4, ctxs._1._3._7, ctxs._1._1, ctxs._1._3._2, ctxs._1._3._5, ctxs._1._2, ctxs._1._3._3, ctxs._1._3._6);
              continue;
            }
            up$c = false;
            up$r = _crashWith("The impossible happened in partial function `up`.");
            continue;
          }
          if (ctxs._1.tag === "TwoRight") {
            if (ctxs._1._1.tag === "Two") {
              up$a0 = ctxs._2;
              up$a1 = $Map("Three", ctxs._1._1._1, ctxs._1._1._2, ctxs._1._1._3, ctxs._1._1._4, ctxs._1._2, ctxs._1._3, tree);
              continue;
            }
            if (ctxs._1._1.tag === "Three") {
              up$c = false;
              up$r = $6(ctxs._1._1._1, ctxs._1._1._4, ctxs._1._1._7, tree, ctxs._1._1._2, ctxs._1._1._5, ctxs._1._2, ctxs._1._1._3, ctxs._1._1._6, ctxs._1._3);
              continue;
            }
            up$c = false;
            up$r = _crashWith("The impossible happened in partial function `up`.");
            continue;
          }
          if (ctxs._1.tag === "ThreeLeft") {
            if (ctxs._1._3.tag === "Two") {
              up$c = false;
              up$r = $7(tree, ctxs._1._3._1, ctxs._1._3._4, ctxs._1._6, ctxs._1._1, ctxs._1._3._2, ctxs._1._4, ctxs._1._2, ctxs._1._3._3, ctxs._1._5);
              continue;
            }
            if (ctxs._1._3.tag === "Three") {
              up$c = false;
              up$r = $11(
                tree,
                ctxs._1._3._1,
                ctxs._1._3._4,
                ctxs._1._3._7,
                ctxs._1._6,
                ctxs._1._1,
                ctxs._1._3._2,
                ctxs._1._3._5,
                ctxs._1._4,
                ctxs._1._2,
                ctxs._1._3._3,
                ctxs._1._3._6,
                ctxs._1._5
              );
              continue;
            }
            up$c = false;
            up$r = _crashWith("The impossible happened in partial function `up`.");
            continue;
          }
          if (ctxs._1.tag === "ThreeMiddle") {
            if (ctxs._1._1.tag === "Two") {
              up$c = false;
              up$r = $8(ctxs._1._1._1, ctxs._1._1._4, tree, ctxs._1._6, ctxs._1._1._2, ctxs._1._2, ctxs._1._4, ctxs._1._1._3, ctxs._1._3, ctxs._1._5);
              continue;
            }
            if (ctxs._1._6.tag === "Two") {
              up$c = false;
              up$r = $9(ctxs._1._1, tree, ctxs._1._6._1, ctxs._1._6._4, ctxs._1._2, ctxs._1._4, ctxs._1._6._2, ctxs._1._3, ctxs._1._5, ctxs._1._6._3);
              continue;
            }
            if (ctxs._1._1.tag === "Three") {
              up$c = false;
              up$r = $12(
                ctxs._1._1._1,
                ctxs._1._1._4,
                ctxs._1._1._7,
                tree,
                ctxs._1._6,
                ctxs._1._1._2,
                ctxs._1._1._5,
                ctxs._1._2,
                ctxs._1._4,
                ctxs._1._1._3,
                ctxs._1._1._6,
                ctxs._1._3,
                ctxs._1._5
              );
              continue;
            }
            if (ctxs._1._6.tag === "Three") {
              up$c = false;
              up$r = $13(
                ctxs._1._1,
                tree,
                ctxs._1._6._1,
                ctxs._1._6._4,
                ctxs._1._6._7,
                ctxs._1._2,
                ctxs._1._4,
                ctxs._1._6._2,
                ctxs._1._6._5,
                ctxs._1._3,
                ctxs._1._5,
                ctxs._1._6._3,
                ctxs._1._6._6
              );
              continue;
            }
            up$c = false;
            up$r = _crashWith("The impossible happened in partial function `up`.");
            continue;
          }
          if (ctxs._1.tag === "ThreeRight") {
            if (ctxs._1._4.tag === "Two") {
              up$c = false;
              up$r = $10(ctxs._1._1, ctxs._1._4._1, ctxs._1._4._4, tree, ctxs._1._2, ctxs._1._4._2, ctxs._1._5, ctxs._1._3, ctxs._1._4._3, ctxs._1._6);
              continue;
            }
            if (ctxs._1._4.tag === "Three") {
              up$c = false;
              up$r = $14(
                ctxs._1._1,
                ctxs._1._4._1,
                ctxs._1._4._4,
                ctxs._1._4._7,
                tree,
                ctxs._1._2,
                ctxs._1._4._2,
                ctxs._1._4._5,
                ctxs._1._5,
                ctxs._1._3,
                ctxs._1._4._3,
                ctxs._1._4._6,
                ctxs._1._6
              );
              continue;
            }
            up$c = false;
            up$r = _crashWith("The impossible happened in partial function `up`.");
            continue;
          }
          up$c = false;
          up$r = _crashWith("The impossible happened in partial function `up`.");
          continue;
        }
        fail();
      }
      ;
      return up$r;
    };
    const removeMaxNode = (removeMaxNode$a0$copy) => (removeMaxNode$a1$copy) => {
      let removeMaxNode$a0 = removeMaxNode$a0$copy, removeMaxNode$a1 = removeMaxNode$a1$copy, removeMaxNode$c = true, removeMaxNode$r;
      while (removeMaxNode$c) {
        const ctx = removeMaxNode$a0, m = removeMaxNode$a1;
        if (m.tag === "Two") {
          if (m._1.tag === "Leaf") {
            if (m._4.tag === "Leaf") {
              removeMaxNode$c = false;
              removeMaxNode$r = up(ctx)(Leaf2);
              continue;
            }
            removeMaxNode$a0 = $List("Cons", $TreeContext("TwoRight", m._1, m._2, m._3), ctx);
            removeMaxNode$a1 = m._4;
            continue;
          }
          removeMaxNode$a0 = $List("Cons", $TreeContext("TwoRight", m._1, m._2, m._3), ctx);
          removeMaxNode$a1 = m._4;
          continue;
        }
        if (m.tag === "Three") {
          if (m._1.tag === "Leaf") {
            if (m._4.tag === "Leaf") {
              if (m._7.tag === "Leaf") {
                removeMaxNode$c = false;
                removeMaxNode$r = up($List("Cons", $TreeContext("TwoRight", Leaf2, m._2, m._3), ctx))(Leaf2);
                continue;
              }
              removeMaxNode$a0 = $List("Cons", $TreeContext("ThreeRight", m._1, m._2, m._3, m._4, m._5, m._6), ctx);
              removeMaxNode$a1 = m._7;
              continue;
            }
            removeMaxNode$a0 = $List("Cons", $TreeContext("ThreeRight", m._1, m._2, m._3, m._4, m._5, m._6), ctx);
            removeMaxNode$a1 = m._7;
            continue;
          }
          removeMaxNode$a0 = $List("Cons", $TreeContext("ThreeRight", m._1, m._2, m._3, m._4, m._5, m._6), ctx);
          removeMaxNode$a1 = m._7;
          continue;
        }
        removeMaxNode$c = false;
        removeMaxNode$r = _crashWith("The impossible happened in partial function `removeMaxNode`.");
        continue;
      }
      ;
      return removeMaxNode$r;
    };
    const maxNode = (maxNode$a0$copy) => {
      let maxNode$a0 = maxNode$a0$copy, maxNode$c = true, maxNode$r;
      while (maxNode$c) {
        const m = maxNode$a0;
        if (m.tag === "Two") {
          if (m._4.tag === "Leaf") {
            maxNode$c = false;
            maxNode$r = { key: m._2, value: m._3 };
            continue;
          }
          maxNode$a0 = m._4;
          continue;
        }
        if (m.tag === "Three") {
          if (m._7.tag === "Leaf") {
            maxNode$c = false;
            maxNode$r = { key: m._5, value: m._6 };
            continue;
          }
          maxNode$a0 = m._7;
          continue;
        }
        maxNode$c = false;
        maxNode$r = _crashWith("The impossible happened in partial function `maxNode`.");
        continue;
      }
      ;
      return maxNode$r;
    };
    const down = (down$a0$copy) => (down$a1$copy) => {
      let down$a0 = down$a0$copy, down$a1 = down$a1$copy, down$c = true, down$r;
      while (down$c) {
        const ctx = down$a0, m = down$a1;
        if (m.tag === "Leaf") {
          down$c = false;
          down$r = Nothing;
          continue;
        }
        if (m.tag === "Two") {
          const v = dictOrd.compare(k)(m._2);
          if (v.tag === "EQ") {
            if (m._4.tag === "Leaf") {
              down$c = false;
              down$r = $Maybe("Just", $Tuple(m._3, up(ctx)(Leaf2)));
              continue;
            }
            const max4 = maxNode(m._1);
            down$c = false;
            down$r = $Maybe(
              "Just",
              $Tuple(m._3, removeMaxNode($List("Cons", $TreeContext("TwoLeft", max4.key, max4.value, m._4), ctx))(m._1))
            );
            continue;
          }
          if (v.tag === "LT") {
            down$a0 = $List("Cons", $TreeContext("TwoLeft", m._2, m._3, m._4), ctx);
            down$a1 = m._1;
            continue;
          }
          down$a0 = $List("Cons", $TreeContext("TwoRight", m._1, m._2, m._3), ctx);
          down$a1 = m._4;
          continue;
        }
        if (m.tag === "Three") {
          const v = dictOrd.compare(k)(m._5);
          const v3 = dictOrd.compare(k)(m._2);
          if ((() => {
            if (m._1.tag === "Leaf") {
              if (m._4.tag === "Leaf") {
                return m._7.tag === "Leaf";
              }
              return false;
            }
            return false;
          })()) {
            if (v3.tag === "EQ") {
              down$c = false;
              down$r = $Maybe("Just", $Tuple(m._3, fromZipper(dictOrd)(ctx)($Map("Two", Leaf2, m._5, m._6, Leaf2))));
              continue;
            }
            if (v.tag === "EQ") {
              down$c = false;
              down$r = $Maybe("Just", $Tuple(m._6, fromZipper(dictOrd)(ctx)($Map("Two", Leaf2, m._2, m._3, Leaf2))));
              continue;
            }
            if (v3.tag === "LT") {
              down$a0 = $List("Cons", $TreeContext("ThreeLeft", m._2, m._3, m._4, m._5, m._6, m._7), ctx);
              down$a1 = m._1;
              continue;
            }
            if (v3.tag === "GT") {
              if (v.tag === "LT") {
                down$a0 = $List("Cons", $TreeContext("ThreeMiddle", m._1, m._2, m._3, m._5, m._6, m._7), ctx);
                down$a1 = m._4;
                continue;
              }
              down$a0 = $List("Cons", $TreeContext("ThreeRight", m._1, m._2, m._3, m._4, m._5, m._6), ctx);
              down$a1 = m._7;
              continue;
            }
            down$a0 = $List("Cons", $TreeContext("ThreeRight", m._1, m._2, m._3, m._4, m._5, m._6), ctx);
            down$a1 = m._7;
            continue;
          }
          if (v3.tag === "EQ") {
            const max4 = maxNode(m._1);
            down$c = false;
            down$r = $Maybe(
              "Just",
              $Tuple(m._3, removeMaxNode($List("Cons", $TreeContext("ThreeLeft", max4.key, max4.value, m._4, m._5, m._6, m._7), ctx))(m._1))
            );
            continue;
          }
          if (v.tag === "EQ") {
            const max4 = maxNode(m._4);
            down$c = false;
            down$r = $Maybe(
              "Just",
              $Tuple(m._6, removeMaxNode($List("Cons", $TreeContext("ThreeMiddle", m._1, m._2, m._3, max4.key, max4.value, m._7), ctx))(m._4))
            );
            continue;
          }
          if (v3.tag === "LT") {
            down$a0 = $List("Cons", $TreeContext("ThreeLeft", m._2, m._3, m._4, m._5, m._6, m._7), ctx);
            down$a1 = m._1;
            continue;
          }
          if (v3.tag === "GT") {
            if (v.tag === "LT") {
              down$a0 = $List("Cons", $TreeContext("ThreeMiddle", m._1, m._2, m._3, m._5, m._6, m._7), ctx);
              down$a1 = m._4;
              continue;
            }
            down$a0 = $List("Cons", $TreeContext("ThreeRight", m._1, m._2, m._3, m._4, m._5, m._6), ctx);
            down$a1 = m._7;
            continue;
          }
          down$a0 = $List("Cons", $TreeContext("ThreeRight", m._1, m._2, m._3, m._4, m._5, m._6), ctx);
          down$a1 = m._7;
          continue;
        }
        fail();
      }
      ;
      return down$r;
    };
    return down(Nil);
  };
  var foldableMap = {
    foldr: (f) => (z) => (m) => {
      if (m.tag === "Leaf") {
        return z;
      }
      if (m.tag === "Two") {
        return foldableMap.foldr(f)(f(m._3)(foldableMap.foldr(f)(z)(m._4)))(m._1);
      }
      if (m.tag === "Three") {
        return foldableMap.foldr(f)(f(m._3)(foldableMap.foldr(f)(f(m._6)(foldableMap.foldr(f)(z)(m._7)))(m._4)))(m._1);
      }
      fail();
    },
    foldl: (f) => (z) => (m) => {
      if (m.tag === "Leaf") {
        return z;
      }
      if (m.tag === "Two") {
        return foldableMap.foldl(f)(f(foldableMap.foldl(f)(z)(m._1))(m._3))(m._4);
      }
      if (m.tag === "Three") {
        return foldableMap.foldl(f)(f(foldableMap.foldl(f)(f(foldableMap.foldl(f)(z)(m._1))(m._3))(m._4))(m._6))(m._7);
      }
      fail();
    },
    foldMap: (dictMonoid) => {
      const append2 = dictMonoid.Semigroup0().append;
      return (f) => (m) => {
        if (m.tag === "Leaf") {
          return dictMonoid.mempty;
        }
        if (m.tag === "Two") {
          return append2(foldableMap.foldMap(dictMonoid)(f)(m._1))(append2(f(m._3))(foldableMap.foldMap(dictMonoid)(f)(m._4)));
        }
        if (m.tag === "Three") {
          return append2(foldableMap.foldMap(dictMonoid)(f)(m._1))(append2(f(m._3))(append2(foldableMap.foldMap(dictMonoid)(f)(m._4))(append2(f(m._6))(foldableMap.foldMap(dictMonoid)(f)(m._7)))));
        }
        fail();
      };
    }
  };
  var foldableWithIndexMap = {
    foldrWithIndex: (f) => (z) => (m) => {
      if (m.tag === "Leaf") {
        return z;
      }
      if (m.tag === "Two") {
        return foldableWithIndexMap.foldrWithIndex(f)(f(m._2)(m._3)(foldableWithIndexMap.foldrWithIndex(f)(z)(m._4)))(m._1);
      }
      if (m.tag === "Three") {
        return foldableWithIndexMap.foldrWithIndex(f)(f(m._2)(m._3)(foldableWithIndexMap.foldrWithIndex(f)(f(m._5)(m._6)(foldableWithIndexMap.foldrWithIndex(f)(z)(m._7)))(m._4)))(m._1);
      }
      fail();
    },
    foldlWithIndex: (f) => (z) => (m) => {
      if (m.tag === "Leaf") {
        return z;
      }
      if (m.tag === "Two") {
        return foldableWithIndexMap.foldlWithIndex(f)(f(m._2)(foldableWithIndexMap.foldlWithIndex(f)(z)(m._1))(m._3))(m._4);
      }
      if (m.tag === "Three") {
        return foldableWithIndexMap.foldlWithIndex(f)(f(m._5)(foldableWithIndexMap.foldlWithIndex(f)(f(m._2)(foldableWithIndexMap.foldlWithIndex(f)(z)(m._1))(m._3))(m._4))(m._6))(m._7);
      }
      fail();
    },
    foldMapWithIndex: (dictMonoid) => {
      const append2 = dictMonoid.Semigroup0().append;
      return (f) => (m) => {
        if (m.tag === "Leaf") {
          return dictMonoid.mempty;
        }
        if (m.tag === "Two") {
          return append2(foldableWithIndexMap.foldMapWithIndex(dictMonoid)(f)(m._1))(append2(f(m._2)(m._3))(foldableWithIndexMap.foldMapWithIndex(dictMonoid)(f)(m._4)));
        }
        if (m.tag === "Three") {
          return append2(foldableWithIndexMap.foldMapWithIndex(dictMonoid)(f)(m._1))(append2(f(m._2)(m._3))(append2(foldableWithIndexMap.foldMapWithIndex(dictMonoid)(f)(m._4))(append2(f(m._5)(m._6))(foldableWithIndexMap.foldMapWithIndex(dictMonoid)(f)(m._7)))));
        }
        fail();
      };
    },
    Foldable0: () => foldableMap
  };
  var eqMap = (dictEq) => (dictEq1) => {
    const eq1 = eqArrayImpl((x2) => (y2) => dictEq.eq(x2._1)(y2._1) && dictEq1.eq(x2._2)(y2._2));
    return { eq: (m1) => (m2) => eq1(toUnfoldable2(unfoldableArray)(m1))(toUnfoldable2(unfoldableArray)(m2)) };
  };
  var fromFoldable = (dictOrd) => (dictFoldable) => dictFoldable.foldl((m) => (v) => insert(dictOrd)(v._1)(v._2)(m))(Leaf2);
  var $$delete = (dictOrd) => (k) => (m) => {
    const $3 = pop(dictOrd)(k)(m);
    if ($3.tag === "Nothing") {
      return m;
    }
    if ($3.tag === "Just") {
      return $3._1._2;
    }
    fail();
  };
  var alter = (dictOrd) => (f) => (k) => (m) => {
    const v = f(lookup(dictOrd)(k)(m));
    if (v.tag === "Nothing") {
      return $$delete(dictOrd)(k)(m);
    }
    if (v.tag === "Just") {
      return insert(dictOrd)(k)(v._1)(m);
    }
    fail();
  };
  var unionWith = (dictOrd) => (f) => (m1) => (m2) => foldableWithIndexMap.foldlWithIndex((k) => (m) => (v) => alter(dictOrd)((() => {
    const $7 = f(v);
    return (x2) => $Maybe(
      "Just",
      (() => {
        if (x2.tag === "Nothing") {
          return v;
        }
        if (x2.tag === "Just") {
          return $7(x2._1);
        }
        fail();
      })()
    );
  })())(k)(m))(m2)(m1);

  // output-es/Effect.Exception/foreign.js
  function showErrorImpl(err) {
    return err.stack || err.toString();
  }
  function error(msg) {
    return new Error(msg);
  }
  function message(e) {
    return e.message;
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output-es/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output-es/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a) {
      return function() {
        return f(a());
      };
    };
  };
  var pure_ = function(a) {
    return function() {
      return a;
    };
  };
  var bind_ = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };
  var run = function(f) {
    return f();
  };
  function whileST(f) {
    return function(a) {
      return function() {
        while (f()) {
          a();
        }
      };
    };
  }
  var foreach = function(as) {
    return function(f) {
      return function() {
        for (var i = 0, l = as.length; i < l; i++) {
          f(as[i])();
        }
      };
    };
  };
  function newSTRef(val2) {
    return function() {
      return { value: val2 };
    };
  }

  // output-es/Control.Monad.ST.Internal/index.js
  var functorST = { map: map_ };
  var monadST = { Applicative0: () => applicativeST, Bind1: () => bindST };
  var bindST = { bind: bind_, Apply0: () => applyST };
  var applyST = {
    apply: (f) => (a) => () => {
      const f$p = f();
      const a$p = a();
      return applicativeST.pure(f$p(a$p))();
    },
    Functor0: () => functorST
  };
  var applicativeST = { pure: pure_, Apply0: () => applyST };
  var monadRecST = {
    tailRecM: (f) => (a) => {
      const $2 = bind_(f(a))(newSTRef);
      return () => {
        const r = $2();
        whileST(() => {
          const $4 = r.value;
          return $4.tag === "Loop";
        })(() => {
          const v = r.value;
          if (v.tag === "Loop") {
            const e = f(v._1)();
            r.value = e;
            return unit;
          }
          if (v.tag === "Done") {
            return unit;
          }
          fail();
        })();
        const $5 = r.value;
        return (() => {
          if ($5.tag === "Done") {
            return $5._1;
          }
          fail();
        })();
      };
    },
    Monad0: () => monadST
  };

  // output-es/Data.Array.ST/foreign.js
  function newSTArray() {
    return [];
  }
  var pushAll = function(as) {
    return function(xs) {
      return function() {
        return xs.push.apply(xs, as);
      };
    };
  };
  var unsafeFreeze = function(xs) {
    return function() {
      return xs;
    };
  };
  function copyImpl(xs) {
    return function() {
      return xs.slice();
    };
  }
  var thaw = copyImpl;
  var sortByImpl = function() {
    function mergeFromTo(compare, fromOrdering, xs1, xs2, from, to) {
      var mid;
      var i;
      var j;
      var k;
      var x2;
      var y2;
      var c;
      mid = from + (to - from >> 1);
      if (mid - from > 1)
        mergeFromTo(compare, fromOrdering, xs2, xs1, from, mid);
      if (to - mid > 1)
        mergeFromTo(compare, fromOrdering, xs2, xs1, mid, to);
      i = from;
      j = mid;
      k = from;
      while (i < mid && j < to) {
        x2 = xs2[i];
        y2 = xs2[j];
        c = fromOrdering(compare(x2)(y2));
        if (c > 0) {
          xs1[k++] = y2;
          ++j;
        } else {
          xs1[k++] = x2;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

  // output-es/Data.Array.ST/index.js
  var withArray = (f) => (xs) => {
    const $2 = thaw(xs);
    return () => {
      const result = $2();
      f(result)();
      return unsafeFreeze(result)();
    };
  };

  // output-es/Unsafe.Coerce/foreign.js
  var unsafeCoerce = function(x2) {
    return x2;
  };

  // output-es/Data.Bifunctor/index.js
  var bifunctorTuple = { bimap: (f) => (g) => (v) => $Tuple(f(v._1), g(v._2)) };
  var bifunctorEither = {
    bimap: (v) => (v1) => (v2) => {
      if (v2.tag === "Left") {
        return $Either("Left", v(v2._1));
      }
      if (v2.tag === "Right") {
        return $Either("Right", v1(v2._1));
      }
      fail();
    }
  };

  // output-es/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a) {
      return [a];
    }
    function array2(a) {
      return function(b) {
        return [a, b];
      };
    }
    function array3(a) {
      return function(b) {
        return function(c) {
          return [a, b, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply3) {
      return function(map5) {
        return function(pure2) {
          return function(f) {
            return function(array) {
              function go(bot, top2) {
                switch (top2 - bot) {
                  case 0:
                    return pure2([]);
                  case 1:
                    return map5(array1)(f(array[bot]));
                  case 2:
                    return apply3(map5(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply3(apply3(map5(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                    return apply3(map5(concat2)(go(bot, pivot)))(go(pivot, top2));
                }
              }
              return go(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output-es/Data.Traversable/index.js
  var identity7 = (x2) => x2;
  var traversableTuple = {
    traverse: (dictApplicative) => {
      const map5 = dictApplicative.Apply0().Functor0().map;
      return (f) => (v) => map5(Tuple(v._1))(f(v._2));
    },
    sequence: (dictApplicative) => {
      const map5 = dictApplicative.Apply0().Functor0().map;
      return (v) => map5(Tuple(v._1))(v._2);
    },
    Functor0: () => functorTuple,
    Foldable1: () => foldableTuple
  };
  var traversableArray = {
    traverse: (dictApplicative) => {
      const Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(Apply0.apply)(Apply0.Functor0().map)(dictApplicative.pure);
    },
    sequence: (dictApplicative) => traversableArray.traverse(dictApplicative)(identity7),
    Functor0: () => functorArray,
    Foldable1: () => foldableArray
  };

  // output-es/Data.Array/foreign.js
  var range2 = function(start2) {
    return function(end) {
      var step = start2 > end ? -1 : 1;
      var result = new Array(step * (end - start2) + 1);
      var i = start2, n = 0;
      while (i !== end) {
        result[n++] = i;
        i += step;
      }
      result[n] = i;
      return result;
    };
  };
  var replicateFill = function(count) {
    return function(value) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value) {
      var result = [];
      var n = 0;
      for (var i = 0; i < count; i++) {
        result[n++] = value;
      }
      return result;
    };
  };
  var replicate2 = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons2(head, tail) {
      this.head = head;
      this.tail = tail;
    }
    var emptyList = {};
    function curryCons(head) {
      return function(tail) {
        return new Cons2(head, tail);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr) {
      return function(xs) {
        return listToArray(foldr(curryCons)(emptyList)(xs));
      };
    };
  }();
  var unconsImpl = function(empty5) {
    return function(next) {
      return function(xs) {
        return xs.length === 0 ? empty5({}) : next(xs[0])(xs.slice(1));
      };
    };
  };
  var indexImpl = function(just) {
    return function(nothing) {
      return function(xs) {
        return function(i) {
          return i < 0 || i >= xs.length ? nothing : just(xs[i]);
        };
      };
    };
  };
  var findIndexImpl = function(just) {
    return function(nothing) {
      return function(f) {
        return function(xs) {
          for (var i = 0, l = xs.length; i < l; i++) {
            if (f(xs[i]))
              return just(i);
          }
          return nothing;
        };
      };
    };
  };
  var _updateAt = function(just) {
    return function(nothing) {
      return function(i) {
        return function(a) {
          return function(l) {
            if (i < 0 || i >= l.length)
              return nothing;
            var l1 = l.slice();
            l1[i] = a;
            return just(l1);
          };
        };
      };
    };
  };
  var sortByImpl2 = function() {
    function mergeFromTo(compare, fromOrdering, xs1, xs2, from, to) {
      var mid;
      var i;
      var j;
      var k;
      var x2;
      var y2;
      var c;
      mid = from + (to - from >> 1);
      if (mid - from > 1)
        mergeFromTo(compare, fromOrdering, xs2, xs1, from, mid);
      if (to - mid > 1)
        mergeFromTo(compare, fromOrdering, xs2, xs1, mid, to);
      i = from;
      j = mid;
      k = from;
      while (i < mid && j < to) {
        x2 = xs2[i];
        y2 = xs2[j];
        c = fromOrdering(compare(x2)(y2));
        if (c > 0) {
          xs1[k++] = y2;
          ++j;
        } else {
          xs1[k++] = x2;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();
  var slice = function(s) {
    return function(e) {
      return function(l) {
        return l.slice(s, e);
      };
    };
  };
  var zipWith2 = function(f) {
    return function(xs) {
      return function(ys) {
        var l = xs.length < ys.length ? xs.length : ys.length;
        var result = new Array(l);
        for (var i = 0; i < l; i++) {
          result[i] = f(xs[i])(ys[i]);
        }
        return result;
      };
    };
  };

  // output-es/Data.Array/index.js
  var zip = /* @__PURE__ */ zipWith2(Tuple);
  var updateAt2 = /* @__PURE__ */ _updateAt(Just)(Nothing);
  var uncons = /* @__PURE__ */ unconsImpl((v) => Nothing)((x2) => (xs) => $Maybe("Just", { head: x2, tail: xs }));
  var toUnfoldable3 = (dictUnfoldable) => (xs) => {
    const len = xs.length;
    return dictUnfoldable.unfoldr((i) => {
      if (i < len) {
        return $Maybe("Just", $Tuple(xs[i], i + 1 | 0));
      }
      return Nothing;
    })(0);
  };
  var sortBy2 = (comp) => sortByImpl2(comp)((v) => {
    if (v.tag === "GT") {
      return 1;
    }
    if (v.tag === "EQ") {
      return 0;
    }
    if (v.tag === "LT") {
      return -1;
    }
    fail();
  });
  var sortWith = (dictOrd) => (f) => sortBy2((x2) => (y2) => dictOrd.compare(f(x2))(f(y2)));
  var index2 = /* @__PURE__ */ indexImpl(Just)(Nothing);
  var findIndex = /* @__PURE__ */ findIndexImpl(Just)(Nothing);
  var notElem = (dictEq) => (a) => (arr) => {
    const $3 = findIndex((v) => dictEq.eq(v)(a))(arr);
    if ($3.tag === "Nothing") {
      return true;
    }
    if ($3.tag === "Just") {
      return false;
    }
    fail();
  };
  var elem = (dictEq) => (a) => (arr) => {
    const $3 = findIndex((v) => dictEq.eq(v)(a))(arr);
    if ($3.tag === "Nothing") {
      return false;
    }
    if ($3.tag === "Just") {
      return true;
    }
    fail();
  };
  var cons3 = (x2) => (xs) => concatArray([x2])(xs);
  var some2 = (dictAlternative) => {
    const apply1 = dictAlternative.Applicative0().Apply0().apply;
    const map32 = dictAlternative.Plus1().Alt0().Functor0().map;
    return (dictLazy) => (v) => apply1(map32(cons3)(v))(dictLazy.defer((v1) => many2(dictAlternative)(dictLazy)(v)));
  };
  var many2 = (dictAlternative) => {
    const alt2 = dictAlternative.Plus1().Alt0().alt;
    const pure1 = dictAlternative.Applicative0().pure;
    return (dictLazy) => (v) => alt2(some2(dictAlternative)(dictLazy)(v))(pure1([]));
  };

  // output-es/Foreign.Object.ST/foreign.js
  var newImpl = function() {
    return {};
  };
  function poke2(k) {
    return function(v) {
      return function(m) {
        return function() {
          m[k] = v;
          return m;
        };
      };
    };
  }
  var deleteImpl = function(k) {
    return function(m) {
      return function() {
        delete m[k];
        return m;
      };
    };
  };

  // output-es/Foreign.Object/foreign.js
  function _copyST(m) {
    return function() {
      var r = {};
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r[k] = m[k];
        }
      }
      return r;
    };
  }
  var empty = {};
  function runST(f) {
    return f();
  }
  function _fmapObject(m0, f) {
    var m = {};
    for (var k in m0) {
      if (hasOwnProperty.call(m0, k)) {
        m[k] = f(m0[k]);
      }
    }
    return m;
  }
  function _mapWithKey(m0, f) {
    var m = {};
    for (var k in m0) {
      if (hasOwnProperty.call(m0, k)) {
        m[k] = f(k)(m0[k]);
      }
    }
    return m;
  }
  function _foldM(bind) {
    return function(f) {
      return function(mz) {
        return function(m) {
          var acc = mz;
          function g(k2) {
            return function(z) {
              return f(z)(k2)(m[k2]);
            };
          }
          for (var k in m) {
            if (hasOwnProperty.call(m, k)) {
              acc = bind(acc)(g(k));
            }
          }
          return acc;
        };
      };
    };
  }
  function all2(f) {
    return function(m) {
      for (var k in m) {
        if (hasOwnProperty.call(m, k) && !f(k)(m[k]))
          return false;
      }
      return true;
    };
  }
  function size(m) {
    var s = 0;
    for (var k in m) {
      if (hasOwnProperty.call(m, k)) {
        ++s;
      }
    }
    return s;
  }
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output-es/Foreign.Object/index.js
  var identity8 = (x2) => x2;
  var values = /* @__PURE__ */ toArrayWithKey((v) => (v1) => v1);
  var toAscUnfoldable = (dictUnfoldable) => {
    const $1 = sortWith(ordString)(fst);
    const $2 = toArrayWithKey(Tuple);
    return (x2) => toUnfoldable3(dictUnfoldable)($1($2(x2)));
  };
  var mutate = (f) => (m) => runST((() => {
    const $2 = _copyST(m);
    return () => {
      const s = $2();
      f(s)();
      return s;
    };
  })());
  var mapWithKey = (f) => (m) => _mapWithKey(m, f);
  var isEmpty = /* @__PURE__ */ all2((v) => (v1) => false);
  var functorObject = { map: (f) => (m) => _fmapObject(m, f) };
  var functorWithIndexObject = { mapWithIndex: mapWithKey, Functor0: () => functorObject };
  var fromFoldable2 = (dictFoldable) => {
    const fromFoldable17 = fromFoldableImpl(dictFoldable.foldr);
    return (l) => runST(() => {
      const s = newImpl();
      foreach(fromFoldable17(l))((v) => () => {
        poke2(v._1)(v._2)(s)();
        return unit;
      })();
      return s;
    });
  };
  var foldM = (dictMonad) => {
    const bind1 = dictMonad.Bind1().bind;
    const pure1 = dictMonad.Applicative0().pure;
    return (f) => (z) => _foldM(bind1)(f)(pure1(z));
  };
  var foldM1 = /* @__PURE__ */ foldM(monadST);
  var union = (m) => mutate((s) => foldM1((s$p) => (k) => (v) => poke2(k)(v)(s$p))(s)(m));
  var unionWith2 = (f) => (m1) => (m2) => mutate((s1) => foldM1((s2) => (k) => (v1) => poke2(k)(_lookup(v1, (v2) => f(v1)(v2), k, m2))(s2))(s1)(m1))(m2);
  var fold = /* @__PURE__ */ _foldM(applyFlipped);
  var foldMap = (dictMonoid) => {
    const append1 = dictMonoid.Semigroup0().append;
    return (f) => fold((acc) => (k) => (v) => append1(acc)(f(k)(v)))(dictMonoid.mempty);
  };
  var foldableObject = {
    foldl: (f) => fold((z) => (v) => f(z)),
    foldr: (f) => (z) => (m) => foldrArray(f)(z)(values(m)),
    foldMap: (dictMonoid) => {
      const foldMap1 = foldMap(dictMonoid);
      return (f) => foldMap1((v) => f);
    }
  };
  var foldableWithIndexObject = {
    foldlWithIndex: (f) => fold((b) => (a) => f(a)(b)),
    foldrWithIndex: (f) => (z) => (m) => foldrArray((v) => f(v._1)(v._2))(z)(toArrayWithKey(Tuple)(m)),
    foldMapWithIndex: (dictMonoid) => foldMap(dictMonoid),
    Foldable0: () => foldableObject
  };
  var traversableWithIndexObject = {
    traverseWithIndex: (dictApplicative) => {
      const Apply0 = dictApplicative.Apply0();
      const map5 = Apply0.Functor0().map;
      return (f) => (ms) => fold((acc) => (k) => (v) => Apply0.apply(map5((b) => (a) => mutate(poke2(k)(a))(b))(acc))(f(k)(v)))(dictApplicative.pure(empty))(ms);
    },
    FunctorWithIndex0: () => functorWithIndexObject,
    FoldableWithIndex1: () => foldableWithIndexObject,
    Traversable2: () => traversableObject
  };
  var traversableObject = {
    traverse: (dictApplicative) => {
      const $1 = traversableWithIndexObject.traverseWithIndex(dictApplicative);
      return (x2) => $1((v) => x2);
    },
    sequence: (dictApplicative) => traversableObject.traverse(dictApplicative)(identity8),
    Functor0: () => functorObject,
    Foldable1: () => foldableObject
  };
  var filterWithKey = (predicate) => (m) => runST(() => {
    const m$p = newImpl();
    return foldM1((acc) => (k) => (v) => {
      if (predicate(k)(v)) {
        return poke2(k)(v)(acc);
      }
      return () => acc;
    })(m$p)(m)();
  });
  var alter2 = (f) => (k) => (m) => {
    const v = f(_lookup(Nothing, Just, k, m));
    if (v.tag === "Nothing") {
      return mutate(deleteImpl(k))(m);
    }
    if (v.tag === "Just") {
      return mutate(poke2(k)(v._1))(m);
    }
    fail();
  };
  var update = (f) => (k) => (m) => alter2((v2) => {
    if (v2.tag === "Nothing") {
      return Nothing;
    }
    if (v2.tag === "Just") {
      return f(v2._1);
    }
    fail();
  })(k)(m);

  // output-es/Control.Category/index.js
  var categoryFn = { identity: (x2) => x2, Semigroupoid0: () => semigroupoidFn };

  // output-es/Data.Profunctor/index.js
  var profunctorFn = { dimap: (a2b) => (c2d) => (b2c) => (x2) => c2d(b2c(a2b(x2))) };

  // output-es/Data.Profunctor.Strong/index.js
  var strongFn = /* @__PURE__ */ (() => ({ first: (a2b) => (v) => $Tuple(a2b(v._1), v._2), second: functorTuple.map, Profunctor0: () => profunctorFn }))();

  // output-es/Util/index.js
  var identity9 = (x2) => x2;
  var intercalate2 = (sep) => (xs) => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, v = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = (() => {
            if (b.init) {
              return { init: false, acc: v._1 };
            }
            return { init: false, acc: foldableList.foldr(Cons)(foldableList.foldr(Cons)(v._1)(sep))(b.acc) };
          })();
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      ;
      return go$r;
    };
    return go({ init: true, acc: Nil })(xs).acc;
  };
  var $$with = (msg) => bifunctorEither.bimap((msg$p) => {
    if (msg === "") {
      return msg$p + "";
    }
    return msg$p + ("\n" + msg);
  })(identity9);
  var mayFailEq = (dictShow) => (dictEq) => (x2) => (x$p) => {
    const $4 = dictShow.show(x2) + (" \u2260 " + dictShow.show(x$p));
    const $5 = dictEq.eq(x2)(x$p);
    if (!$5) {
      return $Either("Left", $4);
    }
    if ($5) {
      return $Either("Right", x2);
    }
    fail();
  };
  var intersperse = (x2) => (xs) => intercalate2($List("Cons", x2, Nil))(listMap(applicativeList.pure)(xs));
  var error2 = (msg) => unsafePerformEffect(throwException(error(msg)));
  var successful = (v) => {
    if (v.tag === "Left") {
      return unsafePerformEffect(throwException(error(v._1)));
    }
    if (v.tag === "Right") {
      return v._1;
    }
    fail();
  };
  var definitely = (v) => (v1) => {
    if (v1.tag === "Just") {
      return v1._1;
    }
    if (v1.tag === "Nothing") {
      return unsafePerformEffect(throwException(error(v)));
    }
    fail();
  };
  var mustEq = (dictEq) => (dictShow) => (x2) => (x$p) => definitely(dictShow.show(x2) + (" equal to " + dictShow.show(x$p)))((() => {
    const $4 = dictEq.eq(x2)(x$p);
    if (!$4) {
      return Nothing;
    }
    if ($4) {
      return $Maybe("Just", x2);
    }
    fail();
  })());
  var unsafeUpdateAt = (i) => (x2) => {
    const $2 = updateAt2(i)(x2);
    return (x$1) => definitely("index within bounds")($2(x$1));
  };
  var bind2Flipped = (dictMonad) => {
    const Bind1 = dictMonad.Bind1();
    const $2 = Bind1.Apply0();
    const map5 = $2.Functor0().map;
    return (f) => (x2) => (y2) => Bind1.bind($2.apply(map5(f)(x2))(y2))(identity2);
  };
  var nonEmpty = (v) => {
    if (v.tag === "Nil") {
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "Cons") {
      return $NonEmpty(v._1, v._2);
    }
    fail();
  };

  // output-es/Dict/foreign.js
  function intersectionWith(f) {
    return function(m1) {
      return function(m2) {
        var m = {};
        for (var k in m1) {
          if (hasOwnProperty.call(m1, k) && hasOwnProperty.call(m2, k)) {
            m[k] = f(m1[k])(m2[k]);
          }
        }
        return m;
      };
    };
  }

  // output-es/Dict/index.js
  var toUnfoldable1 = /* @__PURE__ */ toAscUnfoldable(unfoldableList);
  var keys2 = /* @__PURE__ */ (() => {
    const $0 = foldlArray((m) => (a) => insert(ordString)(a)(unit)(m))(Leaf2);
    return (x2) => $0(keys(x2));
  })();
  var $$get = (k) => {
    const $1 = definitely('Key "' + (k + '" exists in dictionary'));
    return (x2) => $1(_lookup(Nothing, Just, k, x2));
  };
  var disjointUnion_inv = (ks) => (m) => $Tuple(
    filterWithKey((x2) => {
      const $3 = lookup(ordString)(x2)(ks);
      const $4 = (() => {
        if ($3.tag === "Nothing") {
          return false;
        }
        if ($3.tag === "Just") {
          return true;
        }
        fail();
      })();
      return (v) => $4;
    })(m),
    filterWithKey((x2) => {
      const $3 = lookup(ordString)(x2)(ks);
      const $4 = (() => {
        if ($3.tag === "Nothing") {
          return true;
        }
        if ($3.tag === "Just") {
          return false;
        }
        fail();
      })();
      return (v) => $4;
    })(m)
  );
  var difference2 = (m1) => (m2) => foldlArray((b) => (a) => mutate(deleteImpl(a))(b))(m1)(keys(m2));
  var asSingletonMap = (m) => (() => {
    if (size(m) === 1) {
      return identity9;
    }
    return (v1) => unsafePerformEffect(throwException(error("Assertion failure")));
  })()(definitely("singleton map")((() => {
    const $1 = toUnfoldable1(m);
    if ($1.tag === "Nil") {
      return Nothing;
    }
    if ($1.tag === "Cons") {
      return $Maybe("Just", $1._1);
    }
    fail();
  })()));

  // output-es/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b) {
    return !b;
  };

  // output-es/Data.Set/index.js
  var fromFoldable1 = /* @__PURE__ */ (() => fromFoldableImpl(foldableList.foldr))();
  var tailRecM2 = (f) => (a) => (b) => monadRecST.tailRecM((o) => f(o.a)(o.b))({ a, b });
  var union2 = (dictOrd) => (v) => (v1) => unionWith(dictOrd)($$const)(v)(v1);
  var toUnfoldable4 = (dictUnfoldable) => {
    const $1 = toUnfoldable(dictUnfoldable);
    return (x2) => $1(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(x2));
  };
  var toUnfoldable12 = /* @__PURE__ */ toUnfoldable4(unfoldableArray);
  var showSet = (dictShow) => {
    const show4 = showArrayImpl(dictShow.show);
    return { show: (s) => "(fromFoldable " + (show4(toUnfoldable12(s)) + ")") };
  };
  var foldableSet = {
    foldMap: (dictMonoid) => {
      const foldMap1 = foldableList.foldMap(dictMonoid);
      return (f) => {
        const $3 = foldMap1(f);
        return (x2) => $3(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(x2));
      };
    },
    foldl: (f) => (x2) => {
      const $2 = (() => {
        const go = (go$a0$copy) => (go$a1$copy) => {
          let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
          while (go$c) {
            const b = go$a0, v = go$a1;
            if (v.tag === "Nil") {
              go$c = false;
              go$r = b;
              continue;
            }
            if (v.tag === "Cons") {
              go$a0 = f(b)(v._1);
              go$a1 = v._2;
              continue;
            }
            fail();
          }
          ;
          return go$r;
        };
        return go(x2);
      })();
      return (x$1) => $2(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(x$1));
    },
    foldr: (f) => (x2) => {
      const $2 = foldableList.foldr(f)(x2);
      return (x$1) => $2(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(x$1));
    }
  };
  var intersection = (dictOrd) => {
    const fromFoldable32 = foldlArray((m) => (a) => insert(dictOrd)(a)(unit)(m))(Leaf2);
    return (s1) => (s2) => {
      const rs = fromFoldable1(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(s2));
      const rl = rs.length;
      const ls = fromFoldable1(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(s1));
      const ll = ls.length;
      return fromFoldable32(run(bind_(() => {
        const acc = newSTArray();
        return tailRecM2((l) => (r) => {
          if (l < ll && r < rl) {
            const v = dictOrd.compare(ls[l])(rs[r]);
            if (v.tag === "EQ") {
              return () => {
                pushAll([ls[l]])(acc)();
                return $Step("Loop", { a: l + 1 | 0, b: r + 1 | 0 });
              };
            }
            if (v.tag === "LT") {
              return () => $Step("Loop", { a: l + 1 | 0, b: r });
            }
            if (v.tag === "GT") {
              return () => $Step("Loop", { a: l, b: r + 1 | 0 });
            }
            fail();
          }
          return () => $Step("Done", acc);
        })(0)(0)();
      })(unsafeFreeze)));
    };
  };
  var map = (dictOrd) => (f) => foldableSet.foldl((m) => (a) => insert(dictOrd)(f(a))(unit)(m))(Leaf2);
  var difference3 = (dictOrd) => (s1) => (s2) => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, v = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = $$delete(dictOrd)(v._1)(b);
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      ;
      return go$r;
    };
    return go(s1)(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(s2));
  };

  // output-es/Util.Pair/index.js
  var $Pair = (_1, _2) => ({ tag: "Pair", _1, _2 });
  var Pair = (value0) => (value1) => $Pair(value0, value1);
  var functorPair = { map: (f) => (v) => $Pair(f(v._1), f(v._2)) };
  var foldablePair = {
    foldl: (f) => (z) => (v) => f(f(z)(v._1))(v._2),
    foldr: (f) => foldrDefault(foldablePair)(f),
    foldMap: (dictMonoid) => (f) => {
      const append = dictMonoid.Semigroup0().append;
      return foldablePair.foldl((acc) => (x2) => append(acc)(f(x2)))(dictMonoid.mempty);
    }
  };
  var traversablePair = {
    traverse: (dictApplicative) => {
      const Apply0 = dictApplicative.Apply0();
      const map5 = Apply0.Functor0().map;
      return (f) => (v) => Apply0.apply(map5(Pair)(f(v._1)))(f(v._2));
    },
    sequence: (dictApplicative) => traversablePair.traverse(dictApplicative)(identity7),
    Functor0: () => functorPair,
    Foldable1: () => foldablePair
  };
  var toTuple = (v) => $Tuple(v._1, v._2);
  var fromTuple = (v) => $Pair(v._1, v._2);

  // output-es/Lattice/index.js
  var identity10 = (x2) => x2;
  var foldM2 = (f) => (b0) => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, v = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = bindEither.bind(b)((a) => f(a)(v._1));
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      ;
      return go$r;
    };
    return go($Either("Right", b0));
  };
  var toUnfoldable5 = /* @__PURE__ */ toAscUnfoldable(unfoldableList);
  var length2 = /* @__PURE__ */ foldlArray((c) => (v) => 1 + c | 0)(0);
  var sequence = /* @__PURE__ */ (() => traversableArray.traverse(applicativeEither)(identity7))();
  var sequence1 = /* @__PURE__ */ (() => traversableList.traverse(applicativeEither)(identity3))();
  var meetSemilatticeUnit = { meet: (v) => identity10 };
  var meetSemilatticeBoolean = { meet: boolConj };
  var boundedMeetSemilatticeUni = { top: unit, MeetSemilattice0: () => meetSemilatticeUnit };
  var boundedMeetSemilatticeBoo = { top: true, MeetSemilattice0: () => meetSemilatticeBoolean };
  var mayFailUpdate = (dictJoinSemilattice) => (m) => (v) => {
    const v2 = _lookup(Nothing, Just, v._1, m);
    if (v2.tag === "Nothing") {
      return $Either("Right", mutate(poke2(v._1)(v._2))(m));
    }
    if (v2.tag === "Just") {
      const $4 = dictJoinSemilattice.maybeJoin(v2._1)(v._2);
      if ($4.tag === "Left") {
        return $Either("Left", $4._1);
      }
      if ($4.tag === "Right") {
        return $Either("Right", update((v$1) => $Maybe("Just", $4._1))(v._1)(m));
      }
      fail();
    }
    fail();
  };
  var joinSemilatticeBoolean = {
    join: boolDisj,
    maybeJoin: (x2) => (y2) => $Either("Right", joinSemilatticeBoolean.join(x2)(y2)),
    neg: boolNot
  };
  var boundedJoinSemilatticeBoo = { bot: false, JoinSemilattice0: () => joinSemilatticeBoolean };
  var boundedLatticeBoolean = { BoundedJoinSemilattice0: () => boundedJoinSemilatticeBoo, BoundedMeetSemilattice1: () => boundedMeetSemilatticeBoo };
  var joinSemilatticeDict = (dictJoinSemilattice) => ({
    join: unionWith2(dictJoinSemilattice.join),
    maybeJoin: (m) => (m$p) => foldM2(mayFailUpdate(dictJoinSemilattice))(m)(toUnfoldable5(m$p)),
    neg: functorObject.map(dictJoinSemilattice.neg)
  });
  var joinSemilatticeUnit = { join: (v) => identity10, maybeJoin: (x2) => (y2) => $Either("Right", joinSemilatticeUnit.join(x2)(y2)), neg: identity10 };
  var boundedJoinSemilatticeUni = { bot: unit, JoinSemilattice0: () => joinSemilatticeUnit };
  var boundedLatticeUnit = { BoundedJoinSemilattice0: () => boundedJoinSemilatticeUni, BoundedMeetSemilattice1: () => boundedMeetSemilatticeUni };
  var expandablePairPair = (dictExpandable) => ({ expand: (v) => (v1) => $Pair(dictExpandable.expand(v._1)(v1._1), dictExpandable.expand(v._2)(v1._2)) });
  var definedJoin = (dictJoinSemilattice) => (x2) => {
    const $2 = dictJoinSemilattice.maybeJoin(x2);
    return (x$1) => successful($$with("Join undefined")($2(x$1)));
  };
  var joinSemilatticeArray = (dictJoinSemilattice) => ({
    join: (xs) => definedJoin(joinSemilatticeArray(dictJoinSemilattice))(xs),
    maybeJoin: (xs) => (ys) => {
      if (length2(xs) === length2(ys)) {
        return sequence(zipWith2(dictJoinSemilattice.maybeJoin)(xs)(ys));
      }
      return $Either("Left", "Mismatched array lengths");
    },
    neg: arrayMap(dictJoinSemilattice.neg)
  });
  var joinSemilatticeList = (dictJoinSemilattice) => ({
    join: (xs) => definedJoin(joinSemilatticeList(dictJoinSemilattice))(xs),
    maybeJoin: (xs) => (ys) => {
      if ((() => {
        const go = (go$a0$copy) => (go$a1$copy) => {
          let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
          while (go$c) {
            const b = go$a0, v = go$a1;
            if (v.tag === "Nil") {
              go$c = false;
              go$r = b;
              continue;
            }
            if (v.tag === "Cons") {
              go$a0 = 1 + b | 0;
              go$a1 = v._2;
              continue;
            }
            fail();
          }
          ;
          return go$r;
        };
        const go$1 = (go$1$a0$copy) => (go$1$a1$copy) => {
          let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
          while (go$1$c) {
            const b = go$1$a0, v = go$1$a1;
            if (v.tag === "Nil") {
              go$1$c = false;
              go$1$r = b;
              continue;
            }
            if (v.tag === "Cons") {
              go$1$a0 = 1 + b | 0;
              go$1$a1 = v._2;
              continue;
            }
            fail();
          }
          ;
          return go$1$r;
        };
        return go(0)(xs) === go$1(0)(ys);
      })()) {
        return sequence1(zipWith(dictJoinSemilattice.maybeJoin)(xs)(ys));
      }
      return $Either("Left", "Mismatched list lengths");
    },
    neg: listMap(dictJoinSemilattice.neg)
  });
  var joinSemilatticePair = (dictJoinSemilattice) => ({
    join: (ab) => definedJoin(joinSemilatticePair(dictJoinSemilattice))(ab),
    maybeJoin: (v) => (v1) => applyEither.apply((() => {
      const $3 = dictJoinSemilattice.maybeJoin(v._1)(v1._1);
      if ($3.tag === "Left") {
        return $Either("Left", $3._1);
      }
      if ($3.tag === "Right") {
        return $Either("Right", Pair($3._1));
      }
      fail();
    })())(dictJoinSemilattice.maybeJoin(v._2)(v1._2)),
    neg: functorPair.map(dictJoinSemilattice.neg)
  });
  var joinSemilattice$x215 = (dictJoinSemilattice) => (dictJoinSemilattice1) => ({
    join: (ab) => definedJoin(joinSemilattice$x215(dictJoinSemilattice)(dictJoinSemilattice1))(ab),
    maybeJoin: (v) => (v1) => {
      const $4 = dictJoinSemilattice.maybeJoin(v._1)(v1._1);
      return applyEither.apply((() => {
        if ($4.tag === "Left") {
          return $Either("Left", $4._1);
        }
        if ($4.tag === "Right") {
          return $Either("Right", Tuple($4._1));
        }
        fail();
      })())(dictJoinSemilattice1.maybeJoin(v._2)(v1._2));
    },
    neg: functorTuple.map(dictJoinSemilattice1.neg)
  });
  var expandableDictDict = (dictBotOf) => (dictExpandable) => ({
    expand: (kvs) => (kvs$p) => (() => {
      if (difference3(ordString)(keys2(kvs))(keys2(kvs$p)).tag === "Leaf") {
        return identity9;
      }
      return (v1) => unsafePerformEffect(throwException(error("Assertion failure")));
    })()(union(intersectionWith(dictExpandable.expand)(kvs)(kvs$p))(_fmapObject(difference2(kvs$p)(kvs), dictBotOf.botOf)))
  });
  var botOfUnit$x215Raw$x215 = (dictFunctor) => (dictBoundedJoinSemilattice) => ({
    botOf: (() => {
      const $2 = dictFunctor.map((v) => dictBoundedJoinSemilattice.bot);
      return (x2) => $Tuple(dictBoundedJoinSemilattice.bot, $2(x2._2));
    })()
  });

  // output-es/Data.Profunctor.Choice/index.js
  var identity11 = (x2) => x2;
  var fanin = (dictCategory) => {
    const $1 = dictCategory.Semigroupoid0();
    const $2 = dictCategory.Semigroupoid0();
    return (dictChoice) => {
      const dimap = dictChoice.Profunctor0().dimap;
      return (l) => (r) => $1.compose(dimap((v2) => {
        if (v2.tag === "Left") {
          return v2._1;
        }
        if (v2.tag === "Right") {
          return v2._1;
        }
        fail();
      })(identity11)(dictCategory.identity))($2.compose(dictChoice.right(r))(dictChoice.left(l)));
    };
  };
  var choiceFn = /* @__PURE__ */ (() => ({
    left: (v) => (v1) => {
      if (v1.tag === "Left") {
        return $Either("Left", v(v1._1));
      }
      if (v1.tag === "Right") {
        return $Either("Right", v1._1);
      }
      fail();
    },
    right: functorEither.map,
    Profunctor0: () => profunctorFn
  }))();

  // output-es/Data.String.Unsafe/foreign.js
  var charAt = function(i) {
    return function(s) {
      if (i >= 0 && i < s.length)
        return s.charAt(i);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output-es/Data.String.CodeUnits/foreign.js
  var fromCharArray = function(a) {
    return a.join("");
  };
  var toCharArray = function(s) {
    return s.split("");
  };
  var singleton = function(c) {
    return c;
  };
  var _charAt = function(just) {
    return function(nothing) {
      return function(i) {
        return function(s) {
          return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
        };
      };
    };
  };
  var _toChar = function(just) {
    return function(nothing) {
      return function(s) {
        return s.length === 1 ? just(s) : nothing;
      };
    };
  };
  var length3 = function(s) {
    return s.length;
  };
  var _indexOf = function(just) {
    return function(nothing) {
      return function(x2) {
        return function(s) {
          var i = s.indexOf(x2);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
  var drop = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };
  var splitAt = function(i) {
    return function(s) {
      return { before: s.substring(0, i), after: s.substring(i) };
    };
  };

  // output-es/Data.String.CodeUnits/index.js
  var uncons2 = (v) => {
    if (v === "") {
      return Nothing;
    }
    return $Maybe("Just", { head: charAt(0)(v), tail: drop(1)(v) });
  };
  var toChar = /* @__PURE__ */ _toChar(Just)(Nothing);
  var stripPrefix = (v) => (str) => {
    const v1 = splitAt(length3(v))(str);
    if (v1.before === v) {
      return $Maybe("Just", v1.after);
    }
    return Nothing;
  };
  var indexOf = /* @__PURE__ */ _indexOf(Just)(Nothing);
  var contains = (pat) => {
    const $1 = indexOf(pat);
    return (x2) => {
      const $3 = $1(x2);
      if ($3.tag === "Nothing") {
        return false;
      }
      if ($3.tag === "Just") {
        return true;
      }
      fail();
    };
  };
  var charAt2 = /* @__PURE__ */ _charAt(Just)(Nothing);

  // output-es/Data.CodePoint.Unicode.Internal/index.js
  var $UnicodeCategory = (tag) => ({ tag });
  var NUMCAT_LU = /* @__PURE__ */ $UnicodeCategory("NUMCAT_LU");
  var NUMCAT_LL = /* @__PURE__ */ $UnicodeCategory("NUMCAT_LL");
  var NUMCAT_LT = /* @__PURE__ */ $UnicodeCategory("NUMCAT_LT");
  var NUMCAT_LM = /* @__PURE__ */ $UnicodeCategory("NUMCAT_LM");
  var NUMCAT_LO = /* @__PURE__ */ $UnicodeCategory("NUMCAT_LO");
  var NUMCAT_MN = /* @__PURE__ */ $UnicodeCategory("NUMCAT_MN");
  var NUMCAT_MC = /* @__PURE__ */ $UnicodeCategory("NUMCAT_MC");
  var NUMCAT_ME = /* @__PURE__ */ $UnicodeCategory("NUMCAT_ME");
  var NUMCAT_ND = /* @__PURE__ */ $UnicodeCategory("NUMCAT_ND");
  var NUMCAT_NL = /* @__PURE__ */ $UnicodeCategory("NUMCAT_NL");
  var NUMCAT_NO = /* @__PURE__ */ $UnicodeCategory("NUMCAT_NO");
  var NUMCAT_PC = /* @__PURE__ */ $UnicodeCategory("NUMCAT_PC");
  var NUMCAT_PD = /* @__PURE__ */ $UnicodeCategory("NUMCAT_PD");
  var NUMCAT_PS = /* @__PURE__ */ $UnicodeCategory("NUMCAT_PS");
  var NUMCAT_PE = /* @__PURE__ */ $UnicodeCategory("NUMCAT_PE");
  var NUMCAT_PI = /* @__PURE__ */ $UnicodeCategory("NUMCAT_PI");
  var NUMCAT_PF = /* @__PURE__ */ $UnicodeCategory("NUMCAT_PF");
  var NUMCAT_PO = /* @__PURE__ */ $UnicodeCategory("NUMCAT_PO");
  var NUMCAT_SM = /* @__PURE__ */ $UnicodeCategory("NUMCAT_SM");
  var NUMCAT_SC = /* @__PURE__ */ $UnicodeCategory("NUMCAT_SC");
  var NUMCAT_SK = /* @__PURE__ */ $UnicodeCategory("NUMCAT_SK");
  var NUMCAT_SO = /* @__PURE__ */ $UnicodeCategory("NUMCAT_SO");
  var NUMCAT_ZS = /* @__PURE__ */ $UnicodeCategory("NUMCAT_ZS");
  var NUMCAT_ZL = /* @__PURE__ */ $UnicodeCategory("NUMCAT_ZL");
  var NUMCAT_ZP = /* @__PURE__ */ $UnicodeCategory("NUMCAT_ZP");
  var NUMCAT_CC = /* @__PURE__ */ $UnicodeCategory("NUMCAT_CC");
  var NUMCAT_CF = /* @__PURE__ */ $UnicodeCategory("NUMCAT_CF");
  var NUMCAT_CS = /* @__PURE__ */ $UnicodeCategory("NUMCAT_CS");
  var NUMCAT_CO = /* @__PURE__ */ $UnicodeCategory("NUMCAT_CO");
  var NUMCAT_CN = /* @__PURE__ */ $UnicodeCategory("NUMCAT_CN");
  var rule1 = { category: 2, unicodeCat: NUMCAT_ZS, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var spacechars = [
    { start: 32, length: 1, convRule: rule1 },
    { start: 160, length: 1, convRule: rule1 },
    { start: 5760, length: 1, convRule: rule1 },
    { start: 8192, length: 11, convRule: rule1 },
    { start: 8239, length: 1, convRule: rule1 },
    { start: 8287, length: 1, convRule: rule1 },
    { start: 12288, length: 1, convRule: rule1 }
  ];
  var rule162 = { category: 67108864, unicodeCat: NUMCAT_ZP, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule161 = { category: 33554432, unicodeCat: NUMCAT_ZL, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule13 = { category: 8192, unicodeCat: NUMCAT_SO, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule170 = { category: 8192, unicodeCat: NUMCAT_SO, possible: 1, updist: 0, lowdist: 26, titledist: 0 };
  var rule171 = { category: 8192, unicodeCat: NUMCAT_SO, possible: 1, updist: -26, lowdist: 0, titledist: -26 };
  var rule6 = { category: 64, unicodeCat: NUMCAT_SM, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule10 = { category: 1024, unicodeCat: NUMCAT_SK, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule3 = { category: 8, unicodeCat: NUMCAT_SC, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule4 = { category: 16, unicodeCat: NUMCAT_PS, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule2 = { category: 4, unicodeCat: NUMCAT_PO, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule15 = { category: 32768, unicodeCat: NUMCAT_PI, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule19 = { category: 262144, unicodeCat: NUMCAT_PF, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule5 = { category: 32, unicodeCat: NUMCAT_PE, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule7 = { category: 128, unicodeCat: NUMCAT_PD, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule11 = { category: 2048, unicodeCat: NUMCAT_PC, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule17 = { category: 131072, unicodeCat: NUMCAT_NO, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule128 = { category: 16777216, unicodeCat: NUMCAT_NL, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule168 = { category: 16777216, unicodeCat: NUMCAT_NL, possible: 1, updist: 0, lowdist: 16, titledist: 0 };
  var rule169 = { category: 16777216, unicodeCat: NUMCAT_NL, possible: 1, updist: -16, lowdist: 0, titledist: -16 };
  var rule8 = { category: 256, unicodeCat: NUMCAT_ND, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule92 = { category: 2097152, unicodeCat: NUMCAT_MN, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule93 = { category: 2097152, unicodeCat: NUMCAT_MN, possible: 1, updist: 84, lowdist: 0, titledist: 84 };
  var rule119 = { category: 4194304, unicodeCat: NUMCAT_ME, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule124 = { category: 8388608, unicodeCat: NUMCAT_MC, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var nullrule = { category: 512, unicodeCat: NUMCAT_CN, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule104 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 8, titledist: 0 };
  var rule107 = { category: 512, unicodeCat: NUMCAT_LU, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule115 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -60, titledist: 0 };
  var rule117 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -7, titledist: 0 };
  var rule118 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 80, titledist: 0 };
  var rule120 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 15, titledist: 0 };
  var rule122 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 48, titledist: 0 };
  var rule125 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 7264, titledist: 0 };
  var rule127 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 38864, titledist: 0 };
  var rule137 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -3008, titledist: 0 };
  var rule142 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -7615, titledist: 0 };
  var rule144 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -8, titledist: 0 };
  var rule153 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -74, titledist: 0 };
  var rule156 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -86, titledist: 0 };
  var rule157 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -100, titledist: 0 };
  var rule158 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -112, titledist: 0 };
  var rule159 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -128, titledist: 0 };
  var rule160 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -126, titledist: 0 };
  var rule163 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -7517, titledist: 0 };
  var rule164 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -8383, titledist: 0 };
  var rule165 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -8262, titledist: 0 };
  var rule166 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 28, titledist: 0 };
  var rule172 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -10743, titledist: 0 };
  var rule173 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -3814, titledist: 0 };
  var rule174 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -10727, titledist: 0 };
  var rule177 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -10780, titledist: 0 };
  var rule178 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -10749, titledist: 0 };
  var rule179 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -10783, titledist: 0 };
  var rule180 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -10782, titledist: 0 };
  var rule181 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -10815, titledist: 0 };
  var rule183 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -35332, titledist: 0 };
  var rule184 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -42280, titledist: 0 };
  var rule186 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -42308, titledist: 0 };
  var rule187 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -42319, titledist: 0 };
  var rule188 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -42315, titledist: 0 };
  var rule189 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -42305, titledist: 0 };
  var rule190 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -42258, titledist: 0 };
  var rule191 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -42282, titledist: 0 };
  var rule192 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -42261, titledist: 0 };
  var rule193 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 928, titledist: 0 };
  var rule194 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -48, titledist: 0 };
  var rule195 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -42307, titledist: 0 };
  var rule196 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -35384, titledist: 0 };
  var rule201 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 40, titledist: 0 };
  var rule203 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 34, titledist: 0 };
  var rule22 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 1, titledist: 0 };
  var rule24 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -199, titledist: 0 };
  var rule26 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -121, titledist: 0 };
  var rule29 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 210, titledist: 0 };
  var rule30 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 206, titledist: 0 };
  var rule31 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 205, titledist: 0 };
  var rule32 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 79, titledist: 0 };
  var rule33 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 202, titledist: 0 };
  var rule34 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 203, titledist: 0 };
  var rule35 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 207, titledist: 0 };
  var rule37 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 211, titledist: 0 };
  var rule38 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 209, titledist: 0 };
  var rule40 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 213, titledist: 0 };
  var rule42 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 214, titledist: 0 };
  var rule43 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 218, titledist: 0 };
  var rule44 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 217, titledist: 0 };
  var rule45 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 219, titledist: 0 };
  var rule47 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 2, titledist: 1 };
  var rule51 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -97, titledist: 0 };
  var rule52 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -56, titledist: 0 };
  var rule53 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -130, titledist: 0 };
  var rule54 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 10795, titledist: 0 };
  var rule55 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -163, titledist: 0 };
  var rule56 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 10792, titledist: 0 };
  var rule58 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: -195, titledist: 0 };
  var rule59 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 69, titledist: 0 };
  var rule60 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 71, titledist: 0 };
  var rule9 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 32, titledist: 0 };
  var rule94 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 116, titledist: 0 };
  var rule95 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 38, titledist: 0 };
  var rule96 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 37, titledist: 0 };
  var rule97 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 64, titledist: 0 };
  var rule98 = { category: 512, unicodeCat: NUMCAT_LU, possible: 1, updist: 0, lowdist: 63, titledist: 0 };
  var rule151 = { category: 524288, unicodeCat: NUMCAT_LT, possible: 1, updist: 0, lowdist: -8, titledist: 0 };
  var rule154 = { category: 524288, unicodeCat: NUMCAT_LT, possible: 1, updist: 0, lowdist: -9, titledist: 0 };
  var rule48 = { category: 524288, unicodeCat: NUMCAT_LT, possible: 1, updist: -1, lowdist: 1, titledist: 0 };
  var rule14 = { category: 16384, unicodeCat: NUMCAT_LO, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule91 = { category: 1048576, unicodeCat: NUMCAT_LM, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule100 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -37, lowdist: 0, titledist: -37 };
  var rule101 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -31, lowdist: 0, titledist: -31 };
  var rule102 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -64, lowdist: 0, titledist: -64 };
  var rule103 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -63, lowdist: 0, titledist: -63 };
  var rule105 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -62, lowdist: 0, titledist: -62 };
  var rule106 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -57, lowdist: 0, titledist: -57 };
  var rule108 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -47, lowdist: 0, titledist: -47 };
  var rule109 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -54, lowdist: 0, titledist: -54 };
  var rule110 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -8, lowdist: 0, titledist: -8 };
  var rule111 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -86, lowdist: 0, titledist: -86 };
  var rule112 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -80, lowdist: 0, titledist: -80 };
  var rule113 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 7, lowdist: 0, titledist: 7 };
  var rule114 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -116, lowdist: 0, titledist: -116 };
  var rule116 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -96, lowdist: 0, titledist: -96 };
  var rule12 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -32, lowdist: 0, titledist: -32 };
  var rule121 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -15, lowdist: 0, titledist: -15 };
  var rule123 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -48, lowdist: 0, titledist: -48 };
  var rule126 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 3008, lowdist: 0, titledist: 0 };
  var rule129 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -6254, lowdist: 0, titledist: -6254 };
  var rule130 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -6253, lowdist: 0, titledist: -6253 };
  var rule131 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -6244, lowdist: 0, titledist: -6244 };
  var rule132 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -6242, lowdist: 0, titledist: -6242 };
  var rule133 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -6243, lowdist: 0, titledist: -6243 };
  var rule134 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -6236, lowdist: 0, titledist: -6236 };
  var rule135 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -6181, lowdist: 0, titledist: -6181 };
  var rule136 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 35266, lowdist: 0, titledist: 35266 };
  var rule138 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 35332, lowdist: 0, titledist: 35332 };
  var rule139 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 3814, lowdist: 0, titledist: 3814 };
  var rule140 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 35384, lowdist: 0, titledist: 35384 };
  var rule141 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -59, lowdist: 0, titledist: -59 };
  var rule143 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 8, lowdist: 0, titledist: 8 };
  var rule145 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 74, lowdist: 0, titledist: 74 };
  var rule146 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 86, lowdist: 0, titledist: 86 };
  var rule147 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 100, lowdist: 0, titledist: 100 };
  var rule148 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 128, lowdist: 0, titledist: 128 };
  var rule149 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 112, lowdist: 0, titledist: 112 };
  var rule150 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 126, lowdist: 0, titledist: 126 };
  var rule152 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 9, lowdist: 0, titledist: 9 };
  var rule155 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -7205, lowdist: 0, titledist: -7205 };
  var rule167 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -28, lowdist: 0, titledist: -28 };
  var rule175 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -10795, lowdist: 0, titledist: -10795 };
  var rule176 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -10792, lowdist: 0, titledist: -10792 };
  var rule18 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 743, lowdist: 0, titledist: 743 };
  var rule182 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -7264, lowdist: 0, titledist: -7264 };
  var rule185 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 48, lowdist: 0, titledist: 48 };
  var rule197 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -928, lowdist: 0, titledist: -928 };
  var rule198 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -38864, lowdist: 0, titledist: -38864 };
  var rule20 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule202 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -40, lowdist: 0, titledist: -40 };
  var rule204 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -34, lowdist: 0, titledist: -34 };
  var rule21 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 121, lowdist: 0, titledist: 121 };
  var rule23 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -1, lowdist: 0, titledist: -1 };
  var rule25 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -232, lowdist: 0, titledist: -232 };
  var rule27 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -300, lowdist: 0, titledist: -300 };
  var rule28 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 195, lowdist: 0, titledist: 195 };
  var rule36 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 97, lowdist: 0, titledist: 97 };
  var rule39 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 163, lowdist: 0, titledist: 163 };
  var rule41 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 130, lowdist: 0, titledist: 130 };
  var rule46 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 56, lowdist: 0, titledist: 56 };
  var rule49 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -2, lowdist: 0, titledist: -1 };
  var rule50 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -79, lowdist: 0, titledist: -79 };
  var rule57 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 10815, lowdist: 0, titledist: 10815 };
  var rule61 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 10783, lowdist: 0, titledist: 10783 };
  var rule62 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 10780, lowdist: 0, titledist: 10780 };
  var rule63 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 10782, lowdist: 0, titledist: 10782 };
  var rule64 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -210, lowdist: 0, titledist: -210 };
  var rule65 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -206, lowdist: 0, titledist: -206 };
  var rule66 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -205, lowdist: 0, titledist: -205 };
  var rule67 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -202, lowdist: 0, titledist: -202 };
  var rule68 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -203, lowdist: 0, titledist: -203 };
  var rule69 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 42319, lowdist: 0, titledist: 42319 };
  var rule70 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 42315, lowdist: 0, titledist: 42315 };
  var rule71 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -207, lowdist: 0, titledist: -207 };
  var rule72 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 42280, lowdist: 0, titledist: 42280 };
  var rule73 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 42308, lowdist: 0, titledist: 42308 };
  var rule74 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -209, lowdist: 0, titledist: -209 };
  var rule75 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -211, lowdist: 0, titledist: -211 };
  var rule76 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 10743, lowdist: 0, titledist: 10743 };
  var rule77 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 42305, lowdist: 0, titledist: 42305 };
  var rule78 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 10749, lowdist: 0, titledist: 10749 };
  var rule79 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -213, lowdist: 0, titledist: -213 };
  var rule80 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -214, lowdist: 0, titledist: -214 };
  var rule81 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 10727, lowdist: 0, titledist: 10727 };
  var rule82 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -218, lowdist: 0, titledist: -218 };
  var rule83 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 42307, lowdist: 0, titledist: 42307 };
  var rule84 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 42282, lowdist: 0, titledist: 42282 };
  var rule85 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -69, lowdist: 0, titledist: -69 };
  var rule86 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -217, lowdist: 0, titledist: -217 };
  var rule87 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -71, lowdist: 0, titledist: -71 };
  var rule88 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -219, lowdist: 0, titledist: -219 };
  var rule89 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 42261, lowdist: 0, titledist: 42261 };
  var rule90 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: 42258, lowdist: 0, titledist: 42258 };
  var rule99 = { category: 4096, unicodeCat: NUMCAT_LL, possible: 1, updist: -38, lowdist: 0, titledist: -38 };
  var rule199 = { category: 134217728, unicodeCat: NUMCAT_CS, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule200 = { category: 268435456, unicodeCat: NUMCAT_CO, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule16 = { category: 65536, unicodeCat: NUMCAT_CF, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var rule0 = { category: 1, unicodeCat: NUMCAT_CC, possible: 0, updist: 0, lowdist: 0, titledist: 0 };
  var convchars = [
    { start: 65, length: 26, convRule: rule9 },
    { start: 97, length: 26, convRule: rule12 },
    { start: 181, length: 1, convRule: rule18 },
    { start: 192, length: 23, convRule: rule9 },
    { start: 216, length: 7, convRule: rule9 },
    { start: 224, length: 23, convRule: rule12 },
    { start: 248, length: 7, convRule: rule12 },
    { start: 255, length: 1, convRule: rule21 },
    { start: 256, length: 1, convRule: rule22 },
    { start: 257, length: 1, convRule: rule23 },
    { start: 258, length: 1, convRule: rule22 },
    { start: 259, length: 1, convRule: rule23 },
    { start: 260, length: 1, convRule: rule22 },
    { start: 261, length: 1, convRule: rule23 },
    { start: 262, length: 1, convRule: rule22 },
    { start: 263, length: 1, convRule: rule23 },
    { start: 264, length: 1, convRule: rule22 },
    { start: 265, length: 1, convRule: rule23 },
    { start: 266, length: 1, convRule: rule22 },
    { start: 267, length: 1, convRule: rule23 },
    { start: 268, length: 1, convRule: rule22 },
    { start: 269, length: 1, convRule: rule23 },
    { start: 270, length: 1, convRule: rule22 },
    { start: 271, length: 1, convRule: rule23 },
    { start: 272, length: 1, convRule: rule22 },
    { start: 273, length: 1, convRule: rule23 },
    { start: 274, length: 1, convRule: rule22 },
    { start: 275, length: 1, convRule: rule23 },
    { start: 276, length: 1, convRule: rule22 },
    { start: 277, length: 1, convRule: rule23 },
    { start: 278, length: 1, convRule: rule22 },
    { start: 279, length: 1, convRule: rule23 },
    { start: 280, length: 1, convRule: rule22 },
    { start: 281, length: 1, convRule: rule23 },
    { start: 282, length: 1, convRule: rule22 },
    { start: 283, length: 1, convRule: rule23 },
    { start: 284, length: 1, convRule: rule22 },
    { start: 285, length: 1, convRule: rule23 },
    { start: 286, length: 1, convRule: rule22 },
    { start: 287, length: 1, convRule: rule23 },
    { start: 288, length: 1, convRule: rule22 },
    { start: 289, length: 1, convRule: rule23 },
    { start: 290, length: 1, convRule: rule22 },
    { start: 291, length: 1, convRule: rule23 },
    { start: 292, length: 1, convRule: rule22 },
    { start: 293, length: 1, convRule: rule23 },
    { start: 294, length: 1, convRule: rule22 },
    { start: 295, length: 1, convRule: rule23 },
    { start: 296, length: 1, convRule: rule22 },
    { start: 297, length: 1, convRule: rule23 },
    { start: 298, length: 1, convRule: rule22 },
    { start: 299, length: 1, convRule: rule23 },
    { start: 300, length: 1, convRule: rule22 },
    { start: 301, length: 1, convRule: rule23 },
    { start: 302, length: 1, convRule: rule22 },
    { start: 303, length: 1, convRule: rule23 },
    { start: 304, length: 1, convRule: rule24 },
    { start: 305, length: 1, convRule: rule25 },
    { start: 306, length: 1, convRule: rule22 },
    { start: 307, length: 1, convRule: rule23 },
    { start: 308, length: 1, convRule: rule22 },
    { start: 309, length: 1, convRule: rule23 },
    { start: 310, length: 1, convRule: rule22 },
    { start: 311, length: 1, convRule: rule23 },
    { start: 313, length: 1, convRule: rule22 },
    { start: 314, length: 1, convRule: rule23 },
    { start: 315, length: 1, convRule: rule22 },
    { start: 316, length: 1, convRule: rule23 },
    { start: 317, length: 1, convRule: rule22 },
    { start: 318, length: 1, convRule: rule23 },
    { start: 319, length: 1, convRule: rule22 },
    { start: 320, length: 1, convRule: rule23 },
    { start: 321, length: 1, convRule: rule22 },
    { start: 322, length: 1, convRule: rule23 },
    { start: 323, length: 1, convRule: rule22 },
    { start: 324, length: 1, convRule: rule23 },
    { start: 325, length: 1, convRule: rule22 },
    { start: 326, length: 1, convRule: rule23 },
    { start: 327, length: 1, convRule: rule22 },
    { start: 328, length: 1, convRule: rule23 },
    { start: 330, length: 1, convRule: rule22 },
    { start: 331, length: 1, convRule: rule23 },
    { start: 332, length: 1, convRule: rule22 },
    { start: 333, length: 1, convRule: rule23 },
    { start: 334, length: 1, convRule: rule22 },
    { start: 335, length: 1, convRule: rule23 },
    { start: 336, length: 1, convRule: rule22 },
    { start: 337, length: 1, convRule: rule23 },
    { start: 338, length: 1, convRule: rule22 },
    { start: 339, length: 1, convRule: rule23 },
    { start: 340, length: 1, convRule: rule22 },
    { start: 341, length: 1, convRule: rule23 },
    { start: 342, length: 1, convRule: rule22 },
    { start: 343, length: 1, convRule: rule23 },
    { start: 344, length: 1, convRule: rule22 },
    { start: 345, length: 1, convRule: rule23 },
    { start: 346, length: 1, convRule: rule22 },
    { start: 347, length: 1, convRule: rule23 },
    { start: 348, length: 1, convRule: rule22 },
    { start: 349, length: 1, convRule: rule23 },
    { start: 350, length: 1, convRule: rule22 },
    { start: 351, length: 1, convRule: rule23 },
    { start: 352, length: 1, convRule: rule22 },
    { start: 353, length: 1, convRule: rule23 },
    { start: 354, length: 1, convRule: rule22 },
    { start: 355, length: 1, convRule: rule23 },
    { start: 356, length: 1, convRule: rule22 },
    { start: 357, length: 1, convRule: rule23 },
    { start: 358, length: 1, convRule: rule22 },
    { start: 359, length: 1, convRule: rule23 },
    { start: 360, length: 1, convRule: rule22 },
    { start: 361, length: 1, convRule: rule23 },
    { start: 362, length: 1, convRule: rule22 },
    { start: 363, length: 1, convRule: rule23 },
    { start: 364, length: 1, convRule: rule22 },
    { start: 365, length: 1, convRule: rule23 },
    { start: 366, length: 1, convRule: rule22 },
    { start: 367, length: 1, convRule: rule23 },
    { start: 368, length: 1, convRule: rule22 },
    { start: 369, length: 1, convRule: rule23 },
    { start: 370, length: 1, convRule: rule22 },
    { start: 371, length: 1, convRule: rule23 },
    { start: 372, length: 1, convRule: rule22 },
    { start: 373, length: 1, convRule: rule23 },
    { start: 374, length: 1, convRule: rule22 },
    { start: 375, length: 1, convRule: rule23 },
    { start: 376, length: 1, convRule: rule26 },
    { start: 377, length: 1, convRule: rule22 },
    { start: 378, length: 1, convRule: rule23 },
    { start: 379, length: 1, convRule: rule22 },
    { start: 380, length: 1, convRule: rule23 },
    { start: 381, length: 1, convRule: rule22 },
    { start: 382, length: 1, convRule: rule23 },
    { start: 383, length: 1, convRule: rule27 },
    { start: 384, length: 1, convRule: rule28 },
    { start: 385, length: 1, convRule: rule29 },
    { start: 386, length: 1, convRule: rule22 },
    { start: 387, length: 1, convRule: rule23 },
    { start: 388, length: 1, convRule: rule22 },
    { start: 389, length: 1, convRule: rule23 },
    { start: 390, length: 1, convRule: rule30 },
    { start: 391, length: 1, convRule: rule22 },
    { start: 392, length: 1, convRule: rule23 },
    { start: 393, length: 2, convRule: rule31 },
    { start: 395, length: 1, convRule: rule22 },
    { start: 396, length: 1, convRule: rule23 },
    { start: 398, length: 1, convRule: rule32 },
    { start: 399, length: 1, convRule: rule33 },
    { start: 400, length: 1, convRule: rule34 },
    { start: 401, length: 1, convRule: rule22 },
    { start: 402, length: 1, convRule: rule23 },
    { start: 403, length: 1, convRule: rule31 },
    { start: 404, length: 1, convRule: rule35 },
    { start: 405, length: 1, convRule: rule36 },
    { start: 406, length: 1, convRule: rule37 },
    { start: 407, length: 1, convRule: rule38 },
    { start: 408, length: 1, convRule: rule22 },
    { start: 409, length: 1, convRule: rule23 },
    { start: 410, length: 1, convRule: rule39 },
    { start: 412, length: 1, convRule: rule37 },
    { start: 413, length: 1, convRule: rule40 },
    { start: 414, length: 1, convRule: rule41 },
    { start: 415, length: 1, convRule: rule42 },
    { start: 416, length: 1, convRule: rule22 },
    { start: 417, length: 1, convRule: rule23 },
    { start: 418, length: 1, convRule: rule22 },
    { start: 419, length: 1, convRule: rule23 },
    { start: 420, length: 1, convRule: rule22 },
    { start: 421, length: 1, convRule: rule23 },
    { start: 422, length: 1, convRule: rule43 },
    { start: 423, length: 1, convRule: rule22 },
    { start: 424, length: 1, convRule: rule23 },
    { start: 425, length: 1, convRule: rule43 },
    { start: 428, length: 1, convRule: rule22 },
    { start: 429, length: 1, convRule: rule23 },
    { start: 430, length: 1, convRule: rule43 },
    { start: 431, length: 1, convRule: rule22 },
    { start: 432, length: 1, convRule: rule23 },
    { start: 433, length: 2, convRule: rule44 },
    { start: 435, length: 1, convRule: rule22 },
    { start: 436, length: 1, convRule: rule23 },
    { start: 437, length: 1, convRule: rule22 },
    { start: 438, length: 1, convRule: rule23 },
    { start: 439, length: 1, convRule: rule45 },
    { start: 440, length: 1, convRule: rule22 },
    { start: 441, length: 1, convRule: rule23 },
    { start: 444, length: 1, convRule: rule22 },
    { start: 445, length: 1, convRule: rule23 },
    { start: 447, length: 1, convRule: rule46 },
    { start: 452, length: 1, convRule: rule47 },
    { start: 453, length: 1, convRule: rule48 },
    { start: 454, length: 1, convRule: rule49 },
    { start: 455, length: 1, convRule: rule47 },
    { start: 456, length: 1, convRule: rule48 },
    { start: 457, length: 1, convRule: rule49 },
    { start: 458, length: 1, convRule: rule47 },
    { start: 459, length: 1, convRule: rule48 },
    { start: 460, length: 1, convRule: rule49 },
    { start: 461, length: 1, convRule: rule22 },
    { start: 462, length: 1, convRule: rule23 },
    { start: 463, length: 1, convRule: rule22 },
    { start: 464, length: 1, convRule: rule23 },
    { start: 465, length: 1, convRule: rule22 },
    { start: 466, length: 1, convRule: rule23 },
    { start: 467, length: 1, convRule: rule22 },
    { start: 468, length: 1, convRule: rule23 },
    { start: 469, length: 1, convRule: rule22 },
    { start: 470, length: 1, convRule: rule23 },
    { start: 471, length: 1, convRule: rule22 },
    { start: 472, length: 1, convRule: rule23 },
    { start: 473, length: 1, convRule: rule22 },
    { start: 474, length: 1, convRule: rule23 },
    { start: 475, length: 1, convRule: rule22 },
    { start: 476, length: 1, convRule: rule23 },
    { start: 477, length: 1, convRule: rule50 },
    { start: 478, length: 1, convRule: rule22 },
    { start: 479, length: 1, convRule: rule23 },
    { start: 480, length: 1, convRule: rule22 },
    { start: 481, length: 1, convRule: rule23 },
    { start: 482, length: 1, convRule: rule22 },
    { start: 483, length: 1, convRule: rule23 },
    { start: 484, length: 1, convRule: rule22 },
    { start: 485, length: 1, convRule: rule23 },
    { start: 486, length: 1, convRule: rule22 },
    { start: 487, length: 1, convRule: rule23 },
    { start: 488, length: 1, convRule: rule22 },
    { start: 489, length: 1, convRule: rule23 },
    { start: 490, length: 1, convRule: rule22 },
    { start: 491, length: 1, convRule: rule23 },
    { start: 492, length: 1, convRule: rule22 },
    { start: 493, length: 1, convRule: rule23 },
    { start: 494, length: 1, convRule: rule22 },
    { start: 495, length: 1, convRule: rule23 },
    { start: 497, length: 1, convRule: rule47 },
    { start: 498, length: 1, convRule: rule48 },
    { start: 499, length: 1, convRule: rule49 },
    { start: 500, length: 1, convRule: rule22 },
    { start: 501, length: 1, convRule: rule23 },
    { start: 502, length: 1, convRule: rule51 },
    { start: 503, length: 1, convRule: rule52 },
    { start: 504, length: 1, convRule: rule22 },
    { start: 505, length: 1, convRule: rule23 },
    { start: 506, length: 1, convRule: rule22 },
    { start: 507, length: 1, convRule: rule23 },
    { start: 508, length: 1, convRule: rule22 },
    { start: 509, length: 1, convRule: rule23 },
    { start: 510, length: 1, convRule: rule22 },
    { start: 511, length: 1, convRule: rule23 },
    { start: 512, length: 1, convRule: rule22 },
    { start: 513, length: 1, convRule: rule23 },
    { start: 514, length: 1, convRule: rule22 },
    { start: 515, length: 1, convRule: rule23 },
    { start: 516, length: 1, convRule: rule22 },
    { start: 517, length: 1, convRule: rule23 },
    { start: 518, length: 1, convRule: rule22 },
    { start: 519, length: 1, convRule: rule23 },
    { start: 520, length: 1, convRule: rule22 },
    { start: 521, length: 1, convRule: rule23 },
    { start: 522, length: 1, convRule: rule22 },
    { start: 523, length: 1, convRule: rule23 },
    { start: 524, length: 1, convRule: rule22 },
    { start: 525, length: 1, convRule: rule23 },
    { start: 526, length: 1, convRule: rule22 },
    { start: 527, length: 1, convRule: rule23 },
    { start: 528, length: 1, convRule: rule22 },
    { start: 529, length: 1, convRule: rule23 },
    { start: 530, length: 1, convRule: rule22 },
    { start: 531, length: 1, convRule: rule23 },
    { start: 532, length: 1, convRule: rule22 },
    { start: 533, length: 1, convRule: rule23 },
    { start: 534, length: 1, convRule: rule22 },
    { start: 535, length: 1, convRule: rule23 },
    { start: 536, length: 1, convRule: rule22 },
    { start: 537, length: 1, convRule: rule23 },
    { start: 538, length: 1, convRule: rule22 },
    { start: 539, length: 1, convRule: rule23 },
    { start: 540, length: 1, convRule: rule22 },
    { start: 541, length: 1, convRule: rule23 },
    { start: 542, length: 1, convRule: rule22 },
    { start: 543, length: 1, convRule: rule23 },
    { start: 544, length: 1, convRule: rule53 },
    { start: 546, length: 1, convRule: rule22 },
    { start: 547, length: 1, convRule: rule23 },
    { start: 548, length: 1, convRule: rule22 },
    { start: 549, length: 1, convRule: rule23 },
    { start: 550, length: 1, convRule: rule22 },
    { start: 551, length: 1, convRule: rule23 },
    { start: 552, length: 1, convRule: rule22 },
    { start: 553, length: 1, convRule: rule23 },
    { start: 554, length: 1, convRule: rule22 },
    { start: 555, length: 1, convRule: rule23 },
    { start: 556, length: 1, convRule: rule22 },
    { start: 557, length: 1, convRule: rule23 },
    { start: 558, length: 1, convRule: rule22 },
    { start: 559, length: 1, convRule: rule23 },
    { start: 560, length: 1, convRule: rule22 },
    { start: 561, length: 1, convRule: rule23 },
    { start: 562, length: 1, convRule: rule22 },
    { start: 563, length: 1, convRule: rule23 },
    { start: 570, length: 1, convRule: rule54 },
    { start: 571, length: 1, convRule: rule22 },
    { start: 572, length: 1, convRule: rule23 },
    { start: 573, length: 1, convRule: rule55 },
    { start: 574, length: 1, convRule: rule56 },
    { start: 575, length: 2, convRule: rule57 },
    { start: 577, length: 1, convRule: rule22 },
    { start: 578, length: 1, convRule: rule23 },
    { start: 579, length: 1, convRule: rule58 },
    { start: 580, length: 1, convRule: rule59 },
    { start: 581, length: 1, convRule: rule60 },
    { start: 582, length: 1, convRule: rule22 },
    { start: 583, length: 1, convRule: rule23 },
    { start: 584, length: 1, convRule: rule22 },
    { start: 585, length: 1, convRule: rule23 },
    { start: 586, length: 1, convRule: rule22 },
    { start: 587, length: 1, convRule: rule23 },
    { start: 588, length: 1, convRule: rule22 },
    { start: 589, length: 1, convRule: rule23 },
    { start: 590, length: 1, convRule: rule22 },
    { start: 591, length: 1, convRule: rule23 },
    { start: 592, length: 1, convRule: rule61 },
    { start: 593, length: 1, convRule: rule62 },
    { start: 594, length: 1, convRule: rule63 },
    { start: 595, length: 1, convRule: rule64 },
    { start: 596, length: 1, convRule: rule65 },
    { start: 598, length: 2, convRule: rule66 },
    { start: 601, length: 1, convRule: rule67 },
    { start: 603, length: 1, convRule: rule68 },
    { start: 604, length: 1, convRule: rule69 },
    { start: 608, length: 1, convRule: rule66 },
    { start: 609, length: 1, convRule: rule70 },
    { start: 611, length: 1, convRule: rule71 },
    { start: 613, length: 1, convRule: rule72 },
    { start: 614, length: 1, convRule: rule73 },
    { start: 616, length: 1, convRule: rule74 },
    { start: 617, length: 1, convRule: rule75 },
    { start: 618, length: 1, convRule: rule73 },
    { start: 619, length: 1, convRule: rule76 },
    { start: 620, length: 1, convRule: rule77 },
    { start: 623, length: 1, convRule: rule75 },
    { start: 625, length: 1, convRule: rule78 },
    { start: 626, length: 1, convRule: rule79 },
    { start: 629, length: 1, convRule: rule80 },
    { start: 637, length: 1, convRule: rule81 },
    { start: 640, length: 1, convRule: rule82 },
    { start: 642, length: 1, convRule: rule83 },
    { start: 643, length: 1, convRule: rule82 },
    { start: 647, length: 1, convRule: rule84 },
    { start: 648, length: 1, convRule: rule82 },
    { start: 649, length: 1, convRule: rule85 },
    { start: 650, length: 2, convRule: rule86 },
    { start: 652, length: 1, convRule: rule87 },
    { start: 658, length: 1, convRule: rule88 },
    { start: 669, length: 1, convRule: rule89 },
    { start: 670, length: 1, convRule: rule90 },
    { start: 837, length: 1, convRule: rule93 },
    { start: 880, length: 1, convRule: rule22 },
    { start: 881, length: 1, convRule: rule23 },
    { start: 882, length: 1, convRule: rule22 },
    { start: 883, length: 1, convRule: rule23 },
    { start: 886, length: 1, convRule: rule22 },
    { start: 887, length: 1, convRule: rule23 },
    { start: 891, length: 3, convRule: rule41 },
    { start: 895, length: 1, convRule: rule94 },
    { start: 902, length: 1, convRule: rule95 },
    { start: 904, length: 3, convRule: rule96 },
    { start: 908, length: 1, convRule: rule97 },
    { start: 910, length: 2, convRule: rule98 },
    { start: 913, length: 17, convRule: rule9 },
    { start: 931, length: 9, convRule: rule9 },
    { start: 940, length: 1, convRule: rule99 },
    { start: 941, length: 3, convRule: rule100 },
    { start: 945, length: 17, convRule: rule12 },
    { start: 962, length: 1, convRule: rule101 },
    { start: 963, length: 9, convRule: rule12 },
    { start: 972, length: 1, convRule: rule102 },
    { start: 973, length: 2, convRule: rule103 },
    { start: 975, length: 1, convRule: rule104 },
    { start: 976, length: 1, convRule: rule105 },
    { start: 977, length: 1, convRule: rule106 },
    { start: 981, length: 1, convRule: rule108 },
    { start: 982, length: 1, convRule: rule109 },
    { start: 983, length: 1, convRule: rule110 },
    { start: 984, length: 1, convRule: rule22 },
    { start: 985, length: 1, convRule: rule23 },
    { start: 986, length: 1, convRule: rule22 },
    { start: 987, length: 1, convRule: rule23 },
    { start: 988, length: 1, convRule: rule22 },
    { start: 989, length: 1, convRule: rule23 },
    { start: 990, length: 1, convRule: rule22 },
    { start: 991, length: 1, convRule: rule23 },
    { start: 992, length: 1, convRule: rule22 },
    { start: 993, length: 1, convRule: rule23 },
    { start: 994, length: 1, convRule: rule22 },
    { start: 995, length: 1, convRule: rule23 },
    { start: 996, length: 1, convRule: rule22 },
    { start: 997, length: 1, convRule: rule23 },
    { start: 998, length: 1, convRule: rule22 },
    { start: 999, length: 1, convRule: rule23 },
    { start: 1e3, length: 1, convRule: rule22 },
    { start: 1001, length: 1, convRule: rule23 },
    { start: 1002, length: 1, convRule: rule22 },
    { start: 1003, length: 1, convRule: rule23 },
    { start: 1004, length: 1, convRule: rule22 },
    { start: 1005, length: 1, convRule: rule23 },
    { start: 1006, length: 1, convRule: rule22 },
    { start: 1007, length: 1, convRule: rule23 },
    { start: 1008, length: 1, convRule: rule111 },
    { start: 1009, length: 1, convRule: rule112 },
    { start: 1010, length: 1, convRule: rule113 },
    { start: 1011, length: 1, convRule: rule114 },
    { start: 1012, length: 1, convRule: rule115 },
    { start: 1013, length: 1, convRule: rule116 },
    { start: 1015, length: 1, convRule: rule22 },
    { start: 1016, length: 1, convRule: rule23 },
    { start: 1017, length: 1, convRule: rule117 },
    { start: 1018, length: 1, convRule: rule22 },
    { start: 1019, length: 1, convRule: rule23 },
    { start: 1021, length: 3, convRule: rule53 },
    { start: 1024, length: 16, convRule: rule118 },
    { start: 1040, length: 32, convRule: rule9 },
    { start: 1072, length: 32, convRule: rule12 },
    { start: 1104, length: 16, convRule: rule112 },
    { start: 1120, length: 1, convRule: rule22 },
    { start: 1121, length: 1, convRule: rule23 },
    { start: 1122, length: 1, convRule: rule22 },
    { start: 1123, length: 1, convRule: rule23 },
    { start: 1124, length: 1, convRule: rule22 },
    { start: 1125, length: 1, convRule: rule23 },
    { start: 1126, length: 1, convRule: rule22 },
    { start: 1127, length: 1, convRule: rule23 },
    { start: 1128, length: 1, convRule: rule22 },
    { start: 1129, length: 1, convRule: rule23 },
    { start: 1130, length: 1, convRule: rule22 },
    { start: 1131, length: 1, convRule: rule23 },
    { start: 1132, length: 1, convRule: rule22 },
    { start: 1133, length: 1, convRule: rule23 },
    { start: 1134, length: 1, convRule: rule22 },
    { start: 1135, length: 1, convRule: rule23 },
    { start: 1136, length: 1, convRule: rule22 },
    { start: 1137, length: 1, convRule: rule23 },
    { start: 1138, length: 1, convRule: rule22 },
    { start: 1139, length: 1, convRule: rule23 },
    { start: 1140, length: 1, convRule: rule22 },
    { start: 1141, length: 1, convRule: rule23 },
    { start: 1142, length: 1, convRule: rule22 },
    { start: 1143, length: 1, convRule: rule23 },
    { start: 1144, length: 1, convRule: rule22 },
    { start: 1145, length: 1, convRule: rule23 },
    { start: 1146, length: 1, convRule: rule22 },
    { start: 1147, length: 1, convRule: rule23 },
    { start: 1148, length: 1, convRule: rule22 },
    { start: 1149, length: 1, convRule: rule23 },
    { start: 1150, length: 1, convRule: rule22 },
    { start: 1151, length: 1, convRule: rule23 },
    { start: 1152, length: 1, convRule: rule22 },
    { start: 1153, length: 1, convRule: rule23 },
    { start: 1162, length: 1, convRule: rule22 },
    { start: 1163, length: 1, convRule: rule23 },
    { start: 1164, length: 1, convRule: rule22 },
    { start: 1165, length: 1, convRule: rule23 },
    { start: 1166, length: 1, convRule: rule22 },
    { start: 1167, length: 1, convRule: rule23 },
    { start: 1168, length: 1, convRule: rule22 },
    { start: 1169, length: 1, convRule: rule23 },
    { start: 1170, length: 1, convRule: rule22 },
    { start: 1171, length: 1, convRule: rule23 },
    { start: 1172, length: 1, convRule: rule22 },
    { start: 1173, length: 1, convRule: rule23 },
    { start: 1174, length: 1, convRule: rule22 },
    { start: 1175, length: 1, convRule: rule23 },
    { start: 1176, length: 1, convRule: rule22 },
    { start: 1177, length: 1, convRule: rule23 },
    { start: 1178, length: 1, convRule: rule22 },
    { start: 1179, length: 1, convRule: rule23 },
    { start: 1180, length: 1, convRule: rule22 },
    { start: 1181, length: 1, convRule: rule23 },
    { start: 1182, length: 1, convRule: rule22 },
    { start: 1183, length: 1, convRule: rule23 },
    { start: 1184, length: 1, convRule: rule22 },
    { start: 1185, length: 1, convRule: rule23 },
    { start: 1186, length: 1, convRule: rule22 },
    { start: 1187, length: 1, convRule: rule23 },
    { start: 1188, length: 1, convRule: rule22 },
    { start: 1189, length: 1, convRule: rule23 },
    { start: 1190, length: 1, convRule: rule22 },
    { start: 1191, length: 1, convRule: rule23 },
    { start: 1192, length: 1, convRule: rule22 },
    { start: 1193, length: 1, convRule: rule23 },
    { start: 1194, length: 1, convRule: rule22 },
    { start: 1195, length: 1, convRule: rule23 },
    { start: 1196, length: 1, convRule: rule22 },
    { start: 1197, length: 1, convRule: rule23 },
    { start: 1198, length: 1, convRule: rule22 },
    { start: 1199, length: 1, convRule: rule23 },
    { start: 1200, length: 1, convRule: rule22 },
    { start: 1201, length: 1, convRule: rule23 },
    { start: 1202, length: 1, convRule: rule22 },
    { start: 1203, length: 1, convRule: rule23 },
    { start: 1204, length: 1, convRule: rule22 },
    { start: 1205, length: 1, convRule: rule23 },
    { start: 1206, length: 1, convRule: rule22 },
    { start: 1207, length: 1, convRule: rule23 },
    { start: 1208, length: 1, convRule: rule22 },
    { start: 1209, length: 1, convRule: rule23 },
    { start: 1210, length: 1, convRule: rule22 },
    { start: 1211, length: 1, convRule: rule23 },
    { start: 1212, length: 1, convRule: rule22 },
    { start: 1213, length: 1, convRule: rule23 },
    { start: 1214, length: 1, convRule: rule22 },
    { start: 1215, length: 1, convRule: rule23 },
    { start: 1216, length: 1, convRule: rule120 },
    { start: 1217, length: 1, convRule: rule22 },
    { start: 1218, length: 1, convRule: rule23 },
    { start: 1219, length: 1, convRule: rule22 },
    { start: 1220, length: 1, convRule: rule23 },
    { start: 1221, length: 1, convRule: rule22 },
    { start: 1222, length: 1, convRule: rule23 },
    { start: 1223, length: 1, convRule: rule22 },
    { start: 1224, length: 1, convRule: rule23 },
    { start: 1225, length: 1, convRule: rule22 },
    { start: 1226, length: 1, convRule: rule23 },
    { start: 1227, length: 1, convRule: rule22 },
    { start: 1228, length: 1, convRule: rule23 },
    { start: 1229, length: 1, convRule: rule22 },
    { start: 1230, length: 1, convRule: rule23 },
    { start: 1231, length: 1, convRule: rule121 },
    { start: 1232, length: 1, convRule: rule22 },
    { start: 1233, length: 1, convRule: rule23 },
    { start: 1234, length: 1, convRule: rule22 },
    { start: 1235, length: 1, convRule: rule23 },
    { start: 1236, length: 1, convRule: rule22 },
    { start: 1237, length: 1, convRule: rule23 },
    { start: 1238, length: 1, convRule: rule22 },
    { start: 1239, length: 1, convRule: rule23 },
    { start: 1240, length: 1, convRule: rule22 },
    { start: 1241, length: 1, convRule: rule23 },
    { start: 1242, length: 1, convRule: rule22 },
    { start: 1243, length: 1, convRule: rule23 },
    { start: 1244, length: 1, convRule: rule22 },
    { start: 1245, length: 1, convRule: rule23 },
    { start: 1246, length: 1, convRule: rule22 },
    { start: 1247, length: 1, convRule: rule23 },
    { start: 1248, length: 1, convRule: rule22 },
    { start: 1249, length: 1, convRule: rule23 },
    { start: 1250, length: 1, convRule: rule22 },
    { start: 1251, length: 1, convRule: rule23 },
    { start: 1252, length: 1, convRule: rule22 },
    { start: 1253, length: 1, convRule: rule23 },
    { start: 1254, length: 1, convRule: rule22 },
    { start: 1255, length: 1, convRule: rule23 },
    { start: 1256, length: 1, convRule: rule22 },
    { start: 1257, length: 1, convRule: rule23 },
    { start: 1258, length: 1, convRule: rule22 },
    { start: 1259, length: 1, convRule: rule23 },
    { start: 1260, length: 1, convRule: rule22 },
    { start: 1261, length: 1, convRule: rule23 },
    { start: 1262, length: 1, convRule: rule22 },
    { start: 1263, length: 1, convRule: rule23 },
    { start: 1264, length: 1, convRule: rule22 },
    { start: 1265, length: 1, convRule: rule23 },
    { start: 1266, length: 1, convRule: rule22 },
    { start: 1267, length: 1, convRule: rule23 },
    { start: 1268, length: 1, convRule: rule22 },
    { start: 1269, length: 1, convRule: rule23 },
    { start: 1270, length: 1, convRule: rule22 },
    { start: 1271, length: 1, convRule: rule23 },
    { start: 1272, length: 1, convRule: rule22 },
    { start: 1273, length: 1, convRule: rule23 },
    { start: 1274, length: 1, convRule: rule22 },
    { start: 1275, length: 1, convRule: rule23 },
    { start: 1276, length: 1, convRule: rule22 },
    { start: 1277, length: 1, convRule: rule23 },
    { start: 1278, length: 1, convRule: rule22 },
    { start: 1279, length: 1, convRule: rule23 },
    { start: 1280, length: 1, convRule: rule22 },
    { start: 1281, length: 1, convRule: rule23 },
    { start: 1282, length: 1, convRule: rule22 },
    { start: 1283, length: 1, convRule: rule23 },
    { start: 1284, length: 1, convRule: rule22 },
    { start: 1285, length: 1, convRule: rule23 },
    { start: 1286, length: 1, convRule: rule22 },
    { start: 1287, length: 1, convRule: rule23 },
    { start: 1288, length: 1, convRule: rule22 },
    { start: 1289, length: 1, convRule: rule23 },
    { start: 1290, length: 1, convRule: rule22 },
    { start: 1291, length: 1, convRule: rule23 },
    { start: 1292, length: 1, convRule: rule22 },
    { start: 1293, length: 1, convRule: rule23 },
    { start: 1294, length: 1, convRule: rule22 },
    { start: 1295, length: 1, convRule: rule23 },
    { start: 1296, length: 1, convRule: rule22 },
    { start: 1297, length: 1, convRule: rule23 },
    { start: 1298, length: 1, convRule: rule22 },
    { start: 1299, length: 1, convRule: rule23 },
    { start: 1300, length: 1, convRule: rule22 },
    { start: 1301, length: 1, convRule: rule23 },
    { start: 1302, length: 1, convRule: rule22 },
    { start: 1303, length: 1, convRule: rule23 },
    { start: 1304, length: 1, convRule: rule22 },
    { start: 1305, length: 1, convRule: rule23 },
    { start: 1306, length: 1, convRule: rule22 },
    { start: 1307, length: 1, convRule: rule23 },
    { start: 1308, length: 1, convRule: rule22 },
    { start: 1309, length: 1, convRule: rule23 },
    { start: 1310, length: 1, convRule: rule22 },
    { start: 1311, length: 1, convRule: rule23 },
    { start: 1312, length: 1, convRule: rule22 },
    { start: 1313, length: 1, convRule: rule23 },
    { start: 1314, length: 1, convRule: rule22 },
    { start: 1315, length: 1, convRule: rule23 },
    { start: 1316, length: 1, convRule: rule22 },
    { start: 1317, length: 1, convRule: rule23 },
    { start: 1318, length: 1, convRule: rule22 },
    { start: 1319, length: 1, convRule: rule23 },
    { start: 1320, length: 1, convRule: rule22 },
    { start: 1321, length: 1, convRule: rule23 },
    { start: 1322, length: 1, convRule: rule22 },
    { start: 1323, length: 1, convRule: rule23 },
    { start: 1324, length: 1, convRule: rule22 },
    { start: 1325, length: 1, convRule: rule23 },
    { start: 1326, length: 1, convRule: rule22 },
    { start: 1327, length: 1, convRule: rule23 },
    { start: 1329, length: 38, convRule: rule122 },
    { start: 1377, length: 38, convRule: rule123 },
    { start: 4256, length: 38, convRule: rule125 },
    { start: 4295, length: 1, convRule: rule125 },
    { start: 4301, length: 1, convRule: rule125 },
    { start: 4304, length: 43, convRule: rule126 },
    { start: 4349, length: 3, convRule: rule126 },
    { start: 5024, length: 80, convRule: rule127 },
    { start: 5104, length: 6, convRule: rule104 },
    { start: 5112, length: 6, convRule: rule110 },
    { start: 7296, length: 1, convRule: rule129 },
    { start: 7297, length: 1, convRule: rule130 },
    { start: 7298, length: 1, convRule: rule131 },
    { start: 7299, length: 2, convRule: rule132 },
    { start: 7301, length: 1, convRule: rule133 },
    { start: 7302, length: 1, convRule: rule134 },
    { start: 7303, length: 1, convRule: rule135 },
    { start: 7304, length: 1, convRule: rule136 },
    { start: 7312, length: 43, convRule: rule137 },
    { start: 7357, length: 3, convRule: rule137 },
    { start: 7545, length: 1, convRule: rule138 },
    { start: 7549, length: 1, convRule: rule139 },
    { start: 7566, length: 1, convRule: rule140 },
    { start: 7680, length: 1, convRule: rule22 },
    { start: 7681, length: 1, convRule: rule23 },
    { start: 7682, length: 1, convRule: rule22 },
    { start: 7683, length: 1, convRule: rule23 },
    { start: 7684, length: 1, convRule: rule22 },
    { start: 7685, length: 1, convRule: rule23 },
    { start: 7686, length: 1, convRule: rule22 },
    { start: 7687, length: 1, convRule: rule23 },
    { start: 7688, length: 1, convRule: rule22 },
    { start: 7689, length: 1, convRule: rule23 },
    { start: 7690, length: 1, convRule: rule22 },
    { start: 7691, length: 1, convRule: rule23 },
    { start: 7692, length: 1, convRule: rule22 },
    { start: 7693, length: 1, convRule: rule23 },
    { start: 7694, length: 1, convRule: rule22 },
    { start: 7695, length: 1, convRule: rule23 },
    { start: 7696, length: 1, convRule: rule22 },
    { start: 7697, length: 1, convRule: rule23 },
    { start: 7698, length: 1, convRule: rule22 },
    { start: 7699, length: 1, convRule: rule23 },
    { start: 7700, length: 1, convRule: rule22 },
    { start: 7701, length: 1, convRule: rule23 },
    { start: 7702, length: 1, convRule: rule22 },
    { start: 7703, length: 1, convRule: rule23 },
    { start: 7704, length: 1, convRule: rule22 },
    { start: 7705, length: 1, convRule: rule23 },
    { start: 7706, length: 1, convRule: rule22 },
    { start: 7707, length: 1, convRule: rule23 },
    { start: 7708, length: 1, convRule: rule22 },
    { start: 7709, length: 1, convRule: rule23 },
    { start: 7710, length: 1, convRule: rule22 },
    { start: 7711, length: 1, convRule: rule23 },
    { start: 7712, length: 1, convRule: rule22 },
    { start: 7713, length: 1, convRule: rule23 },
    { start: 7714, length: 1, convRule: rule22 },
    { start: 7715, length: 1, convRule: rule23 },
    { start: 7716, length: 1, convRule: rule22 },
    { start: 7717, length: 1, convRule: rule23 },
    { start: 7718, length: 1, convRule: rule22 },
    { start: 7719, length: 1, convRule: rule23 },
    { start: 7720, length: 1, convRule: rule22 },
    { start: 7721, length: 1, convRule: rule23 },
    { start: 7722, length: 1, convRule: rule22 },
    { start: 7723, length: 1, convRule: rule23 },
    { start: 7724, length: 1, convRule: rule22 },
    { start: 7725, length: 1, convRule: rule23 },
    { start: 7726, length: 1, convRule: rule22 },
    { start: 7727, length: 1, convRule: rule23 },
    { start: 7728, length: 1, convRule: rule22 },
    { start: 7729, length: 1, convRule: rule23 },
    { start: 7730, length: 1, convRule: rule22 },
    { start: 7731, length: 1, convRule: rule23 },
    { start: 7732, length: 1, convRule: rule22 },
    { start: 7733, length: 1, convRule: rule23 },
    { start: 7734, length: 1, convRule: rule22 },
    { start: 7735, length: 1, convRule: rule23 },
    { start: 7736, length: 1, convRule: rule22 },
    { start: 7737, length: 1, convRule: rule23 },
    { start: 7738, length: 1, convRule: rule22 },
    { start: 7739, length: 1, convRule: rule23 },
    { start: 7740, length: 1, convRule: rule22 },
    { start: 7741, length: 1, convRule: rule23 },
    { start: 7742, length: 1, convRule: rule22 },
    { start: 7743, length: 1, convRule: rule23 },
    { start: 7744, length: 1, convRule: rule22 },
    { start: 7745, length: 1, convRule: rule23 },
    { start: 7746, length: 1, convRule: rule22 },
    { start: 7747, length: 1, convRule: rule23 },
    { start: 7748, length: 1, convRule: rule22 },
    { start: 7749, length: 1, convRule: rule23 },
    { start: 7750, length: 1, convRule: rule22 },
    { start: 7751, length: 1, convRule: rule23 },
    { start: 7752, length: 1, convRule: rule22 },
    { start: 7753, length: 1, convRule: rule23 },
    { start: 7754, length: 1, convRule: rule22 },
    { start: 7755, length: 1, convRule: rule23 },
    { start: 7756, length: 1, convRule: rule22 },
    { start: 7757, length: 1, convRule: rule23 },
    { start: 7758, length: 1, convRule: rule22 },
    { start: 7759, length: 1, convRule: rule23 },
    { start: 7760, length: 1, convRule: rule22 },
    { start: 7761, length: 1, convRule: rule23 },
    { start: 7762, length: 1, convRule: rule22 },
    { start: 7763, length: 1, convRule: rule23 },
    { start: 7764, length: 1, convRule: rule22 },
    { start: 7765, length: 1, convRule: rule23 },
    { start: 7766, length: 1, convRule: rule22 },
    { start: 7767, length: 1, convRule: rule23 },
    { start: 7768, length: 1, convRule: rule22 },
    { start: 7769, length: 1, convRule: rule23 },
    { start: 7770, length: 1, convRule: rule22 },
    { start: 7771, length: 1, convRule: rule23 },
    { start: 7772, length: 1, convRule: rule22 },
    { start: 7773, length: 1, convRule: rule23 },
    { start: 7774, length: 1, convRule: rule22 },
    { start: 7775, length: 1, convRule: rule23 },
    { start: 7776, length: 1, convRule: rule22 },
    { start: 7777, length: 1, convRule: rule23 },
    { start: 7778, length: 1, convRule: rule22 },
    { start: 7779, length: 1, convRule: rule23 },
    { start: 7780, length: 1, convRule: rule22 },
    { start: 7781, length: 1, convRule: rule23 },
    { start: 7782, length: 1, convRule: rule22 },
    { start: 7783, length: 1, convRule: rule23 },
    { start: 7784, length: 1, convRule: rule22 },
    { start: 7785, length: 1, convRule: rule23 },
    { start: 7786, length: 1, convRule: rule22 },
    { start: 7787, length: 1, convRule: rule23 },
    { start: 7788, length: 1, convRule: rule22 },
    { start: 7789, length: 1, convRule: rule23 },
    { start: 7790, length: 1, convRule: rule22 },
    { start: 7791, length: 1, convRule: rule23 },
    { start: 7792, length: 1, convRule: rule22 },
    { start: 7793, length: 1, convRule: rule23 },
    { start: 7794, length: 1, convRule: rule22 },
    { start: 7795, length: 1, convRule: rule23 },
    { start: 7796, length: 1, convRule: rule22 },
    { start: 7797, length: 1, convRule: rule23 },
    { start: 7798, length: 1, convRule: rule22 },
    { start: 7799, length: 1, convRule: rule23 },
    { start: 7800, length: 1, convRule: rule22 },
    { start: 7801, length: 1, convRule: rule23 },
    { start: 7802, length: 1, convRule: rule22 },
    { start: 7803, length: 1, convRule: rule23 },
    { start: 7804, length: 1, convRule: rule22 },
    { start: 7805, length: 1, convRule: rule23 },
    { start: 7806, length: 1, convRule: rule22 },
    { start: 7807, length: 1, convRule: rule23 },
    { start: 7808, length: 1, convRule: rule22 },
    { start: 7809, length: 1, convRule: rule23 },
    { start: 7810, length: 1, convRule: rule22 },
    { start: 7811, length: 1, convRule: rule23 },
    { start: 7812, length: 1, convRule: rule22 },
    { start: 7813, length: 1, convRule: rule23 },
    { start: 7814, length: 1, convRule: rule22 },
    { start: 7815, length: 1, convRule: rule23 },
    { start: 7816, length: 1, convRule: rule22 },
    { start: 7817, length: 1, convRule: rule23 },
    { start: 7818, length: 1, convRule: rule22 },
    { start: 7819, length: 1, convRule: rule23 },
    { start: 7820, length: 1, convRule: rule22 },
    { start: 7821, length: 1, convRule: rule23 },
    { start: 7822, length: 1, convRule: rule22 },
    { start: 7823, length: 1, convRule: rule23 },
    { start: 7824, length: 1, convRule: rule22 },
    { start: 7825, length: 1, convRule: rule23 },
    { start: 7826, length: 1, convRule: rule22 },
    { start: 7827, length: 1, convRule: rule23 },
    { start: 7828, length: 1, convRule: rule22 },
    { start: 7829, length: 1, convRule: rule23 },
    { start: 7835, length: 1, convRule: rule141 },
    { start: 7838, length: 1, convRule: rule142 },
    { start: 7840, length: 1, convRule: rule22 },
    { start: 7841, length: 1, convRule: rule23 },
    { start: 7842, length: 1, convRule: rule22 },
    { start: 7843, length: 1, convRule: rule23 },
    { start: 7844, length: 1, convRule: rule22 },
    { start: 7845, length: 1, convRule: rule23 },
    { start: 7846, length: 1, convRule: rule22 },
    { start: 7847, length: 1, convRule: rule23 },
    { start: 7848, length: 1, convRule: rule22 },
    { start: 7849, length: 1, convRule: rule23 },
    { start: 7850, length: 1, convRule: rule22 },
    { start: 7851, length: 1, convRule: rule23 },
    { start: 7852, length: 1, convRule: rule22 },
    { start: 7853, length: 1, convRule: rule23 },
    { start: 7854, length: 1, convRule: rule22 },
    { start: 7855, length: 1, convRule: rule23 },
    { start: 7856, length: 1, convRule: rule22 },
    { start: 7857, length: 1, convRule: rule23 },
    { start: 7858, length: 1, convRule: rule22 },
    { start: 7859, length: 1, convRule: rule23 },
    { start: 7860, length: 1, convRule: rule22 },
    { start: 7861, length: 1, convRule: rule23 },
    { start: 7862, length: 1, convRule: rule22 },
    { start: 7863, length: 1, convRule: rule23 },
    { start: 7864, length: 1, convRule: rule22 },
    { start: 7865, length: 1, convRule: rule23 },
    { start: 7866, length: 1, convRule: rule22 },
    { start: 7867, length: 1, convRule: rule23 },
    { start: 7868, length: 1, convRule: rule22 },
    { start: 7869, length: 1, convRule: rule23 },
    { start: 7870, length: 1, convRule: rule22 },
    { start: 7871, length: 1, convRule: rule23 },
    { start: 7872, length: 1, convRule: rule22 },
    { start: 7873, length: 1, convRule: rule23 },
    { start: 7874, length: 1, convRule: rule22 },
    { start: 7875, length: 1, convRule: rule23 },
    { start: 7876, length: 1, convRule: rule22 },
    { start: 7877, length: 1, convRule: rule23 },
    { start: 7878, length: 1, convRule: rule22 },
    { start: 7879, length: 1, convRule: rule23 },
    { start: 7880, length: 1, convRule: rule22 },
    { start: 7881, length: 1, convRule: rule23 },
    { start: 7882, length: 1, convRule: rule22 },
    { start: 7883, length: 1, convRule: rule23 },
    { start: 7884, length: 1, convRule: rule22 },
    { start: 7885, length: 1, convRule: rule23 },
    { start: 7886, length: 1, convRule: rule22 },
    { start: 7887, length: 1, convRule: rule23 },
    { start: 7888, length: 1, convRule: rule22 },
    { start: 7889, length: 1, convRule: rule23 },
    { start: 7890, length: 1, convRule: rule22 },
    { start: 7891, length: 1, convRule: rule23 },
    { start: 7892, length: 1, convRule: rule22 },
    { start: 7893, length: 1, convRule: rule23 },
    { start: 7894, length: 1, convRule: rule22 },
    { start: 7895, length: 1, convRule: rule23 },
    { start: 7896, length: 1, convRule: rule22 },
    { start: 7897, length: 1, convRule: rule23 },
    { start: 7898, length: 1, convRule: rule22 },
    { start: 7899, length: 1, convRule: rule23 },
    { start: 7900, length: 1, convRule: rule22 },
    { start: 7901, length: 1, convRule: rule23 },
    { start: 7902, length: 1, convRule: rule22 },
    { start: 7903, length: 1, convRule: rule23 },
    { start: 7904, length: 1, convRule: rule22 },
    { start: 7905, length: 1, convRule: rule23 },
    { start: 7906, length: 1, convRule: rule22 },
    { start: 7907, length: 1, convRule: rule23 },
    { start: 7908, length: 1, convRule: rule22 },
    { start: 7909, length: 1, convRule: rule23 },
    { start: 7910, length: 1, convRule: rule22 },
    { start: 7911, length: 1, convRule: rule23 },
    { start: 7912, length: 1, convRule: rule22 },
    { start: 7913, length: 1, convRule: rule23 },
    { start: 7914, length: 1, convRule: rule22 },
    { start: 7915, length: 1, convRule: rule23 },
    { start: 7916, length: 1, convRule: rule22 },
    { start: 7917, length: 1, convRule: rule23 },
    { start: 7918, length: 1, convRule: rule22 },
    { start: 7919, length: 1, convRule: rule23 },
    { start: 7920, length: 1, convRule: rule22 },
    { start: 7921, length: 1, convRule: rule23 },
    { start: 7922, length: 1, convRule: rule22 },
    { start: 7923, length: 1, convRule: rule23 },
    { start: 7924, length: 1, convRule: rule22 },
    { start: 7925, length: 1, convRule: rule23 },
    { start: 7926, length: 1, convRule: rule22 },
    { start: 7927, length: 1, convRule: rule23 },
    { start: 7928, length: 1, convRule: rule22 },
    { start: 7929, length: 1, convRule: rule23 },
    { start: 7930, length: 1, convRule: rule22 },
    { start: 7931, length: 1, convRule: rule23 },
    { start: 7932, length: 1, convRule: rule22 },
    { start: 7933, length: 1, convRule: rule23 },
    { start: 7934, length: 1, convRule: rule22 },
    { start: 7935, length: 1, convRule: rule23 },
    { start: 7936, length: 8, convRule: rule143 },
    { start: 7944, length: 8, convRule: rule144 },
    { start: 7952, length: 6, convRule: rule143 },
    { start: 7960, length: 6, convRule: rule144 },
    { start: 7968, length: 8, convRule: rule143 },
    { start: 7976, length: 8, convRule: rule144 },
    { start: 7984, length: 8, convRule: rule143 },
    { start: 7992, length: 8, convRule: rule144 },
    { start: 8e3, length: 6, convRule: rule143 },
    { start: 8008, length: 6, convRule: rule144 },
    { start: 8017, length: 1, convRule: rule143 },
    { start: 8019, length: 1, convRule: rule143 },
    { start: 8021, length: 1, convRule: rule143 },
    { start: 8023, length: 1, convRule: rule143 },
    { start: 8025, length: 1, convRule: rule144 },
    { start: 8027, length: 1, convRule: rule144 },
    { start: 8029, length: 1, convRule: rule144 },
    { start: 8031, length: 1, convRule: rule144 },
    { start: 8032, length: 8, convRule: rule143 },
    { start: 8040, length: 8, convRule: rule144 },
    { start: 8048, length: 2, convRule: rule145 },
    { start: 8050, length: 4, convRule: rule146 },
    { start: 8054, length: 2, convRule: rule147 },
    { start: 8056, length: 2, convRule: rule148 },
    { start: 8058, length: 2, convRule: rule149 },
    { start: 8060, length: 2, convRule: rule150 },
    { start: 8064, length: 8, convRule: rule143 },
    { start: 8072, length: 8, convRule: rule151 },
    { start: 8080, length: 8, convRule: rule143 },
    { start: 8088, length: 8, convRule: rule151 },
    { start: 8096, length: 8, convRule: rule143 },
    { start: 8104, length: 8, convRule: rule151 },
    { start: 8112, length: 2, convRule: rule143 },
    { start: 8115, length: 1, convRule: rule152 },
    { start: 8120, length: 2, convRule: rule144 },
    { start: 8122, length: 2, convRule: rule153 },
    { start: 8124, length: 1, convRule: rule154 },
    { start: 8126, length: 1, convRule: rule155 },
    { start: 8131, length: 1, convRule: rule152 },
    { start: 8136, length: 4, convRule: rule156 },
    { start: 8140, length: 1, convRule: rule154 },
    { start: 8144, length: 2, convRule: rule143 },
    { start: 8152, length: 2, convRule: rule144 },
    { start: 8154, length: 2, convRule: rule157 },
    { start: 8160, length: 2, convRule: rule143 },
    { start: 8165, length: 1, convRule: rule113 },
    { start: 8168, length: 2, convRule: rule144 },
    { start: 8170, length: 2, convRule: rule158 },
    { start: 8172, length: 1, convRule: rule117 },
    { start: 8179, length: 1, convRule: rule152 },
    { start: 8184, length: 2, convRule: rule159 },
    { start: 8186, length: 2, convRule: rule160 },
    { start: 8188, length: 1, convRule: rule154 },
    { start: 8486, length: 1, convRule: rule163 },
    { start: 8490, length: 1, convRule: rule164 },
    { start: 8491, length: 1, convRule: rule165 },
    { start: 8498, length: 1, convRule: rule166 },
    { start: 8526, length: 1, convRule: rule167 },
    { start: 8544, length: 16, convRule: rule168 },
    { start: 8560, length: 16, convRule: rule169 },
    { start: 8579, length: 1, convRule: rule22 },
    { start: 8580, length: 1, convRule: rule23 },
    { start: 9398, length: 26, convRule: rule170 },
    { start: 9424, length: 26, convRule: rule171 },
    { start: 11264, length: 47, convRule: rule122 },
    { start: 11312, length: 47, convRule: rule123 },
    { start: 11360, length: 1, convRule: rule22 },
    { start: 11361, length: 1, convRule: rule23 },
    { start: 11362, length: 1, convRule: rule172 },
    { start: 11363, length: 1, convRule: rule173 },
    { start: 11364, length: 1, convRule: rule174 },
    { start: 11365, length: 1, convRule: rule175 },
    { start: 11366, length: 1, convRule: rule176 },
    { start: 11367, length: 1, convRule: rule22 },
    { start: 11368, length: 1, convRule: rule23 },
    { start: 11369, length: 1, convRule: rule22 },
    { start: 11370, length: 1, convRule: rule23 },
    { start: 11371, length: 1, convRule: rule22 },
    { start: 11372, length: 1, convRule: rule23 },
    { start: 11373, length: 1, convRule: rule177 },
    { start: 11374, length: 1, convRule: rule178 },
    { start: 11375, length: 1, convRule: rule179 },
    { start: 11376, length: 1, convRule: rule180 },
    { start: 11378, length: 1, convRule: rule22 },
    { start: 11379, length: 1, convRule: rule23 },
    { start: 11381, length: 1, convRule: rule22 },
    { start: 11382, length: 1, convRule: rule23 },
    { start: 11390, length: 2, convRule: rule181 },
    { start: 11392, length: 1, convRule: rule22 },
    { start: 11393, length: 1, convRule: rule23 },
    { start: 11394, length: 1, convRule: rule22 },
    { start: 11395, length: 1, convRule: rule23 },
    { start: 11396, length: 1, convRule: rule22 },
    { start: 11397, length: 1, convRule: rule23 },
    { start: 11398, length: 1, convRule: rule22 },
    { start: 11399, length: 1, convRule: rule23 },
    { start: 11400, length: 1, convRule: rule22 },
    { start: 11401, length: 1, convRule: rule23 },
    { start: 11402, length: 1, convRule: rule22 },
    { start: 11403, length: 1, convRule: rule23 },
    { start: 11404, length: 1, convRule: rule22 },
    { start: 11405, length: 1, convRule: rule23 },
    { start: 11406, length: 1, convRule: rule22 },
    { start: 11407, length: 1, convRule: rule23 },
    { start: 11408, length: 1, convRule: rule22 },
    { start: 11409, length: 1, convRule: rule23 },
    { start: 11410, length: 1, convRule: rule22 },
    { start: 11411, length: 1, convRule: rule23 },
    { start: 11412, length: 1, convRule: rule22 },
    { start: 11413, length: 1, convRule: rule23 },
    { start: 11414, length: 1, convRule: rule22 },
    { start: 11415, length: 1, convRule: rule23 },
    { start: 11416, length: 1, convRule: rule22 },
    { start: 11417, length: 1, convRule: rule23 },
    { start: 11418, length: 1, convRule: rule22 },
    { start: 11419, length: 1, convRule: rule23 },
    { start: 11420, length: 1, convRule: rule22 },
    { start: 11421, length: 1, convRule: rule23 },
    { start: 11422, length: 1, convRule: rule22 },
    { start: 11423, length: 1, convRule: rule23 },
    { start: 11424, length: 1, convRule: rule22 },
    { start: 11425, length: 1, convRule: rule23 },
    { start: 11426, length: 1, convRule: rule22 },
    { start: 11427, length: 1, convRule: rule23 },
    { start: 11428, length: 1, convRule: rule22 },
    { start: 11429, length: 1, convRule: rule23 },
    { start: 11430, length: 1, convRule: rule22 },
    { start: 11431, length: 1, convRule: rule23 },
    { start: 11432, length: 1, convRule: rule22 },
    { start: 11433, length: 1, convRule: rule23 },
    { start: 11434, length: 1, convRule: rule22 },
    { start: 11435, length: 1, convRule: rule23 },
    { start: 11436, length: 1, convRule: rule22 },
    { start: 11437, length: 1, convRule: rule23 },
    { start: 11438, length: 1, convRule: rule22 },
    { start: 11439, length: 1, convRule: rule23 },
    { start: 11440, length: 1, convRule: rule22 },
    { start: 11441, length: 1, convRule: rule23 },
    { start: 11442, length: 1, convRule: rule22 },
    { start: 11443, length: 1, convRule: rule23 },
    { start: 11444, length: 1, convRule: rule22 },
    { start: 11445, length: 1, convRule: rule23 },
    { start: 11446, length: 1, convRule: rule22 },
    { start: 11447, length: 1, convRule: rule23 },
    { start: 11448, length: 1, convRule: rule22 },
    { start: 11449, length: 1, convRule: rule23 },
    { start: 11450, length: 1, convRule: rule22 },
    { start: 11451, length: 1, convRule: rule23 },
    { start: 11452, length: 1, convRule: rule22 },
    { start: 11453, length: 1, convRule: rule23 },
    { start: 11454, length: 1, convRule: rule22 },
    { start: 11455, length: 1, convRule: rule23 },
    { start: 11456, length: 1, convRule: rule22 },
    { start: 11457, length: 1, convRule: rule23 },
    { start: 11458, length: 1, convRule: rule22 },
    { start: 11459, length: 1, convRule: rule23 },
    { start: 11460, length: 1, convRule: rule22 },
    { start: 11461, length: 1, convRule: rule23 },
    { start: 11462, length: 1, convRule: rule22 },
    { start: 11463, length: 1, convRule: rule23 },
    { start: 11464, length: 1, convRule: rule22 },
    { start: 11465, length: 1, convRule: rule23 },
    { start: 11466, length: 1, convRule: rule22 },
    { start: 11467, length: 1, convRule: rule23 },
    { start: 11468, length: 1, convRule: rule22 },
    { start: 11469, length: 1, convRule: rule23 },
    { start: 11470, length: 1, convRule: rule22 },
    { start: 11471, length: 1, convRule: rule23 },
    { start: 11472, length: 1, convRule: rule22 },
    { start: 11473, length: 1, convRule: rule23 },
    { start: 11474, length: 1, convRule: rule22 },
    { start: 11475, length: 1, convRule: rule23 },
    { start: 11476, length: 1, convRule: rule22 },
    { start: 11477, length: 1, convRule: rule23 },
    { start: 11478, length: 1, convRule: rule22 },
    { start: 11479, length: 1, convRule: rule23 },
    { start: 11480, length: 1, convRule: rule22 },
    { start: 11481, length: 1, convRule: rule23 },
    { start: 11482, length: 1, convRule: rule22 },
    { start: 11483, length: 1, convRule: rule23 },
    { start: 11484, length: 1, convRule: rule22 },
    { start: 11485, length: 1, convRule: rule23 },
    { start: 11486, length: 1, convRule: rule22 },
    { start: 11487, length: 1, convRule: rule23 },
    { start: 11488, length: 1, convRule: rule22 },
    { start: 11489, length: 1, convRule: rule23 },
    { start: 11490, length: 1, convRule: rule22 },
    { start: 11491, length: 1, convRule: rule23 },
    { start: 11499, length: 1, convRule: rule22 },
    { start: 11500, length: 1, convRule: rule23 },
    { start: 11501, length: 1, convRule: rule22 },
    { start: 11502, length: 1, convRule: rule23 },
    { start: 11506, length: 1, convRule: rule22 },
    { start: 11507, length: 1, convRule: rule23 },
    { start: 11520, length: 38, convRule: rule182 },
    { start: 11559, length: 1, convRule: rule182 },
    { start: 11565, length: 1, convRule: rule182 },
    { start: 42560, length: 1, convRule: rule22 },
    { start: 42561, length: 1, convRule: rule23 },
    { start: 42562, length: 1, convRule: rule22 },
    { start: 42563, length: 1, convRule: rule23 },
    { start: 42564, length: 1, convRule: rule22 },
    { start: 42565, length: 1, convRule: rule23 },
    { start: 42566, length: 1, convRule: rule22 },
    { start: 42567, length: 1, convRule: rule23 },
    { start: 42568, length: 1, convRule: rule22 },
    { start: 42569, length: 1, convRule: rule23 },
    { start: 42570, length: 1, convRule: rule22 },
    { start: 42571, length: 1, convRule: rule23 },
    { start: 42572, length: 1, convRule: rule22 },
    { start: 42573, length: 1, convRule: rule23 },
    { start: 42574, length: 1, convRule: rule22 },
    { start: 42575, length: 1, convRule: rule23 },
    { start: 42576, length: 1, convRule: rule22 },
    { start: 42577, length: 1, convRule: rule23 },
    { start: 42578, length: 1, convRule: rule22 },
    { start: 42579, length: 1, convRule: rule23 },
    { start: 42580, length: 1, convRule: rule22 },
    { start: 42581, length: 1, convRule: rule23 },
    { start: 42582, length: 1, convRule: rule22 },
    { start: 42583, length: 1, convRule: rule23 },
    { start: 42584, length: 1, convRule: rule22 },
    { start: 42585, length: 1, convRule: rule23 },
    { start: 42586, length: 1, convRule: rule22 },
    { start: 42587, length: 1, convRule: rule23 },
    { start: 42588, length: 1, convRule: rule22 },
    { start: 42589, length: 1, convRule: rule23 },
    { start: 42590, length: 1, convRule: rule22 },
    { start: 42591, length: 1, convRule: rule23 },
    { start: 42592, length: 1, convRule: rule22 },
    { start: 42593, length: 1, convRule: rule23 },
    { start: 42594, length: 1, convRule: rule22 },
    { start: 42595, length: 1, convRule: rule23 },
    { start: 42596, length: 1, convRule: rule22 },
    { start: 42597, length: 1, convRule: rule23 },
    { start: 42598, length: 1, convRule: rule22 },
    { start: 42599, length: 1, convRule: rule23 },
    { start: 42600, length: 1, convRule: rule22 },
    { start: 42601, length: 1, convRule: rule23 },
    { start: 42602, length: 1, convRule: rule22 },
    { start: 42603, length: 1, convRule: rule23 },
    { start: 42604, length: 1, convRule: rule22 },
    { start: 42605, length: 1, convRule: rule23 },
    { start: 42624, length: 1, convRule: rule22 },
    { start: 42625, length: 1, convRule: rule23 },
    { start: 42626, length: 1, convRule: rule22 },
    { start: 42627, length: 1, convRule: rule23 },
    { start: 42628, length: 1, convRule: rule22 },
    { start: 42629, length: 1, convRule: rule23 },
    { start: 42630, length: 1, convRule: rule22 },
    { start: 42631, length: 1, convRule: rule23 },
    { start: 42632, length: 1, convRule: rule22 },
    { start: 42633, length: 1, convRule: rule23 },
    { start: 42634, length: 1, convRule: rule22 },
    { start: 42635, length: 1, convRule: rule23 },
    { start: 42636, length: 1, convRule: rule22 },
    { start: 42637, length: 1, convRule: rule23 },
    { start: 42638, length: 1, convRule: rule22 },
    { start: 42639, length: 1, convRule: rule23 },
    { start: 42640, length: 1, convRule: rule22 },
    { start: 42641, length: 1, convRule: rule23 },
    { start: 42642, length: 1, convRule: rule22 },
    { start: 42643, length: 1, convRule: rule23 },
    { start: 42644, length: 1, convRule: rule22 },
    { start: 42645, length: 1, convRule: rule23 },
    { start: 42646, length: 1, convRule: rule22 },
    { start: 42647, length: 1, convRule: rule23 },
    { start: 42648, length: 1, convRule: rule22 },
    { start: 42649, length: 1, convRule: rule23 },
    { start: 42650, length: 1, convRule: rule22 },
    { start: 42651, length: 1, convRule: rule23 },
    { start: 42786, length: 1, convRule: rule22 },
    { start: 42787, length: 1, convRule: rule23 },
    { start: 42788, length: 1, convRule: rule22 },
    { start: 42789, length: 1, convRule: rule23 },
    { start: 42790, length: 1, convRule: rule22 },
    { start: 42791, length: 1, convRule: rule23 },
    { start: 42792, length: 1, convRule: rule22 },
    { start: 42793, length: 1, convRule: rule23 },
    { start: 42794, length: 1, convRule: rule22 },
    { start: 42795, length: 1, convRule: rule23 },
    { start: 42796, length: 1, convRule: rule22 },
    { start: 42797, length: 1, convRule: rule23 },
    { start: 42798, length: 1, convRule: rule22 },
    { start: 42799, length: 1, convRule: rule23 },
    { start: 42802, length: 1, convRule: rule22 },
    { start: 42803, length: 1, convRule: rule23 },
    { start: 42804, length: 1, convRule: rule22 },
    { start: 42805, length: 1, convRule: rule23 },
    { start: 42806, length: 1, convRule: rule22 },
    { start: 42807, length: 1, convRule: rule23 },
    { start: 42808, length: 1, convRule: rule22 },
    { start: 42809, length: 1, convRule: rule23 },
    { start: 42810, length: 1, convRule: rule22 },
    { start: 42811, length: 1, convRule: rule23 },
    { start: 42812, length: 1, convRule: rule22 },
    { start: 42813, length: 1, convRule: rule23 },
    { start: 42814, length: 1, convRule: rule22 },
    { start: 42815, length: 1, convRule: rule23 },
    { start: 42816, length: 1, convRule: rule22 },
    { start: 42817, length: 1, convRule: rule23 },
    { start: 42818, length: 1, convRule: rule22 },
    { start: 42819, length: 1, convRule: rule23 },
    { start: 42820, length: 1, convRule: rule22 },
    { start: 42821, length: 1, convRule: rule23 },
    { start: 42822, length: 1, convRule: rule22 },
    { start: 42823, length: 1, convRule: rule23 },
    { start: 42824, length: 1, convRule: rule22 },
    { start: 42825, length: 1, convRule: rule23 },
    { start: 42826, length: 1, convRule: rule22 },
    { start: 42827, length: 1, convRule: rule23 },
    { start: 42828, length: 1, convRule: rule22 },
    { start: 42829, length: 1, convRule: rule23 },
    { start: 42830, length: 1, convRule: rule22 },
    { start: 42831, length: 1, convRule: rule23 },
    { start: 42832, length: 1, convRule: rule22 },
    { start: 42833, length: 1, convRule: rule23 },
    { start: 42834, length: 1, convRule: rule22 },
    { start: 42835, length: 1, convRule: rule23 },
    { start: 42836, length: 1, convRule: rule22 },
    { start: 42837, length: 1, convRule: rule23 },
    { start: 42838, length: 1, convRule: rule22 },
    { start: 42839, length: 1, convRule: rule23 },
    { start: 42840, length: 1, convRule: rule22 },
    { start: 42841, length: 1, convRule: rule23 },
    { start: 42842, length: 1, convRule: rule22 },
    { start: 42843, length: 1, convRule: rule23 },
    { start: 42844, length: 1, convRule: rule22 },
    { start: 42845, length: 1, convRule: rule23 },
    { start: 42846, length: 1, convRule: rule22 },
    { start: 42847, length: 1, convRule: rule23 },
    { start: 42848, length: 1, convRule: rule22 },
    { start: 42849, length: 1, convRule: rule23 },
    { start: 42850, length: 1, convRule: rule22 },
    { start: 42851, length: 1, convRule: rule23 },
    { start: 42852, length: 1, convRule: rule22 },
    { start: 42853, length: 1, convRule: rule23 },
    { start: 42854, length: 1, convRule: rule22 },
    { start: 42855, length: 1, convRule: rule23 },
    { start: 42856, length: 1, convRule: rule22 },
    { start: 42857, length: 1, convRule: rule23 },
    { start: 42858, length: 1, convRule: rule22 },
    { start: 42859, length: 1, convRule: rule23 },
    { start: 42860, length: 1, convRule: rule22 },
    { start: 42861, length: 1, convRule: rule23 },
    { start: 42862, length: 1, convRule: rule22 },
    { start: 42863, length: 1, convRule: rule23 },
    { start: 42873, length: 1, convRule: rule22 },
    { start: 42874, length: 1, convRule: rule23 },
    { start: 42875, length: 1, convRule: rule22 },
    { start: 42876, length: 1, convRule: rule23 },
    { start: 42877, length: 1, convRule: rule183 },
    { start: 42878, length: 1, convRule: rule22 },
    { start: 42879, length: 1, convRule: rule23 },
    { start: 42880, length: 1, convRule: rule22 },
    { start: 42881, length: 1, convRule: rule23 },
    { start: 42882, length: 1, convRule: rule22 },
    { start: 42883, length: 1, convRule: rule23 },
    { start: 42884, length: 1, convRule: rule22 },
    { start: 42885, length: 1, convRule: rule23 },
    { start: 42886, length: 1, convRule: rule22 },
    { start: 42887, length: 1, convRule: rule23 },
    { start: 42891, length: 1, convRule: rule22 },
    { start: 42892, length: 1, convRule: rule23 },
    { start: 42893, length: 1, convRule: rule184 },
    { start: 42896, length: 1, convRule: rule22 },
    { start: 42897, length: 1, convRule: rule23 },
    { start: 42898, length: 1, convRule: rule22 },
    { start: 42899, length: 1, convRule: rule23 },
    { start: 42900, length: 1, convRule: rule185 },
    { start: 42902, length: 1, convRule: rule22 },
    { start: 42903, length: 1, convRule: rule23 },
    { start: 42904, length: 1, convRule: rule22 },
    { start: 42905, length: 1, convRule: rule23 },
    { start: 42906, length: 1, convRule: rule22 },
    { start: 42907, length: 1, convRule: rule23 },
    { start: 42908, length: 1, convRule: rule22 },
    { start: 42909, length: 1, convRule: rule23 },
    { start: 42910, length: 1, convRule: rule22 },
    { start: 42911, length: 1, convRule: rule23 },
    { start: 42912, length: 1, convRule: rule22 },
    { start: 42913, length: 1, convRule: rule23 },
    { start: 42914, length: 1, convRule: rule22 },
    { start: 42915, length: 1, convRule: rule23 },
    { start: 42916, length: 1, convRule: rule22 },
    { start: 42917, length: 1, convRule: rule23 },
    { start: 42918, length: 1, convRule: rule22 },
    { start: 42919, length: 1, convRule: rule23 },
    { start: 42920, length: 1, convRule: rule22 },
    { start: 42921, length: 1, convRule: rule23 },
    { start: 42922, length: 1, convRule: rule186 },
    { start: 42923, length: 1, convRule: rule187 },
    { start: 42924, length: 1, convRule: rule188 },
    { start: 42925, length: 1, convRule: rule189 },
    { start: 42926, length: 1, convRule: rule186 },
    { start: 42928, length: 1, convRule: rule190 },
    { start: 42929, length: 1, convRule: rule191 },
    { start: 42930, length: 1, convRule: rule192 },
    { start: 42931, length: 1, convRule: rule193 },
    { start: 42932, length: 1, convRule: rule22 },
    { start: 42933, length: 1, convRule: rule23 },
    { start: 42934, length: 1, convRule: rule22 },
    { start: 42935, length: 1, convRule: rule23 },
    { start: 42936, length: 1, convRule: rule22 },
    { start: 42937, length: 1, convRule: rule23 },
    { start: 42938, length: 1, convRule: rule22 },
    { start: 42939, length: 1, convRule: rule23 },
    { start: 42940, length: 1, convRule: rule22 },
    { start: 42941, length: 1, convRule: rule23 },
    { start: 42942, length: 1, convRule: rule22 },
    { start: 42943, length: 1, convRule: rule23 },
    { start: 42946, length: 1, convRule: rule22 },
    { start: 42947, length: 1, convRule: rule23 },
    { start: 42948, length: 1, convRule: rule194 },
    { start: 42949, length: 1, convRule: rule195 },
    { start: 42950, length: 1, convRule: rule196 },
    { start: 42951, length: 1, convRule: rule22 },
    { start: 42952, length: 1, convRule: rule23 },
    { start: 42953, length: 1, convRule: rule22 },
    { start: 42954, length: 1, convRule: rule23 },
    { start: 42997, length: 1, convRule: rule22 },
    { start: 42998, length: 1, convRule: rule23 },
    { start: 43859, length: 1, convRule: rule197 },
    { start: 43888, length: 80, convRule: rule198 },
    { start: 65313, length: 26, convRule: rule9 },
    { start: 65345, length: 26, convRule: rule12 },
    { start: 66560, length: 40, convRule: rule201 },
    { start: 66600, length: 40, convRule: rule202 },
    { start: 66736, length: 36, convRule: rule201 },
    { start: 66776, length: 36, convRule: rule202 },
    { start: 68736, length: 51, convRule: rule97 },
    { start: 68800, length: 51, convRule: rule102 },
    { start: 71840, length: 32, convRule: rule9 },
    { start: 71872, length: 32, convRule: rule12 },
    { start: 93760, length: 32, convRule: rule9 },
    { start: 93792, length: 32, convRule: rule12 },
    { start: 125184, length: 34, convRule: rule203 },
    { start: 125218, length: 34, convRule: rule204 }
  ];
  var bsearch = (a) => (array) => (size3) => (compare) => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const i = go$a0, k = go$a1;
        if (i > k || i >= array.length) {
          go$c = false;
          go$r = Nothing;
          continue;
        }
        const j = unsafeClamp(floor(toNumber(i + k | 0) / 2));
        const b = array[j];
        const v = compare(a)(b);
        if (v.tag === "EQ") {
          go$c = false;
          go$r = $Maybe("Just", b);
          continue;
        }
        if (v.tag === "GT") {
          go$a0 = j + 1 | 0;
          go$a1 = k;
          continue;
        }
        go$a0 = i;
        go$a1 = j - 1 | 0;
        continue;
      }
      ;
      return go$r;
    };
    return go(0)(size3);
  };
  var blkCmp = (v) => (v1) => {
    if (v.start >= v1.start && v.start < (v1.start + v1.length | 0)) {
      return EQ;
    }
    if (v.start > v1.start) {
      return GT;
    }
    return LT;
  };
  var getRule = (blocks) => (unichar) => (size3) => {
    const maybeCharBlock = bsearch({ start: unichar, length: 1, convRule: nullrule })(blocks)(size3)(blkCmp);
    if (maybeCharBlock.tag === "Nothing") {
      return Nothing;
    }
    if (maybeCharBlock.tag === "Just") {
      return $Maybe("Just", maybeCharBlock._1.convRule);
    }
    fail();
  };
  var caseConv = (f) => ($$char2) => {
    const maybeConversionRule = getRule(convchars)($$char2)(1332);
    if (maybeConversionRule.tag === "Nothing") {
      return $$char2;
    }
    if (maybeConversionRule.tag === "Just") {
      return $$char2 + f(maybeConversionRule._1) | 0;
    }
    fail();
  };
  var uTowlower = /* @__PURE__ */ caseConv((v) => v.lowdist);
  var uTowupper = /* @__PURE__ */ caseConv((v) => v.updist);
  var checkAttrS = (categories) => ($$char2) => {
    const maybeConversionRule = getRule(spacechars)($$char2)(7);
    if (maybeConversionRule.tag === "Nothing") {
      return false;
    }
    if (maybeConversionRule.tag === "Just") {
      const $3 = findIndex((v) => v === maybeConversionRule._1.category)(categories);
      if ($3.tag === "Nothing") {
        return false;
      }
      if ($3.tag === "Just") {
        return true;
      }
      fail();
    }
    fail();
  };
  var allchars = [
    { start: 0, length: 32, convRule: rule0 },
    { start: 32, length: 1, convRule: rule1 },
    { start: 33, length: 3, convRule: rule2 },
    { start: 36, length: 1, convRule: rule3 },
    { start: 37, length: 3, convRule: rule2 },
    { start: 40, length: 1, convRule: rule4 },
    { start: 41, length: 1, convRule: rule5 },
    { start: 42, length: 1, convRule: rule2 },
    { start: 43, length: 1, convRule: rule6 },
    { start: 44, length: 1, convRule: rule2 },
    { start: 45, length: 1, convRule: rule7 },
    { start: 46, length: 2, convRule: rule2 },
    { start: 48, length: 10, convRule: rule8 },
    { start: 58, length: 2, convRule: rule2 },
    { start: 60, length: 3, convRule: rule6 },
    { start: 63, length: 2, convRule: rule2 },
    { start: 65, length: 26, convRule: rule9 },
    { start: 91, length: 1, convRule: rule4 },
    { start: 92, length: 1, convRule: rule2 },
    { start: 93, length: 1, convRule: rule5 },
    { start: 94, length: 1, convRule: rule10 },
    { start: 95, length: 1, convRule: rule11 },
    { start: 96, length: 1, convRule: rule10 },
    { start: 97, length: 26, convRule: rule12 },
    { start: 123, length: 1, convRule: rule4 },
    { start: 124, length: 1, convRule: rule6 },
    { start: 125, length: 1, convRule: rule5 },
    { start: 126, length: 1, convRule: rule6 },
    { start: 127, length: 33, convRule: rule0 },
    { start: 160, length: 1, convRule: rule1 },
    { start: 161, length: 1, convRule: rule2 },
    { start: 162, length: 4, convRule: rule3 },
    { start: 166, length: 1, convRule: rule13 },
    { start: 167, length: 1, convRule: rule2 },
    { start: 168, length: 1, convRule: rule10 },
    { start: 169, length: 1, convRule: rule13 },
    { start: 170, length: 1, convRule: rule14 },
    { start: 171, length: 1, convRule: rule15 },
    { start: 172, length: 1, convRule: rule6 },
    { start: 173, length: 1, convRule: rule16 },
    { start: 174, length: 1, convRule: rule13 },
    { start: 175, length: 1, convRule: rule10 },
    { start: 176, length: 1, convRule: rule13 },
    { start: 177, length: 1, convRule: rule6 },
    { start: 178, length: 2, convRule: rule17 },
    { start: 180, length: 1, convRule: rule10 },
    { start: 181, length: 1, convRule: rule18 },
    { start: 182, length: 2, convRule: rule2 },
    { start: 184, length: 1, convRule: rule10 },
    { start: 185, length: 1, convRule: rule17 },
    { start: 186, length: 1, convRule: rule14 },
    { start: 187, length: 1, convRule: rule19 },
    { start: 188, length: 3, convRule: rule17 },
    { start: 191, length: 1, convRule: rule2 },
    { start: 192, length: 23, convRule: rule9 },
    { start: 215, length: 1, convRule: rule6 },
    { start: 216, length: 7, convRule: rule9 },
    { start: 223, length: 1, convRule: rule20 },
    { start: 224, length: 23, convRule: rule12 },
    { start: 247, length: 1, convRule: rule6 },
    { start: 248, length: 7, convRule: rule12 },
    { start: 255, length: 1, convRule: rule21 },
    { start: 256, length: 1, convRule: rule22 },
    { start: 257, length: 1, convRule: rule23 },
    { start: 258, length: 1, convRule: rule22 },
    { start: 259, length: 1, convRule: rule23 },
    { start: 260, length: 1, convRule: rule22 },
    { start: 261, length: 1, convRule: rule23 },
    { start: 262, length: 1, convRule: rule22 },
    { start: 263, length: 1, convRule: rule23 },
    { start: 264, length: 1, convRule: rule22 },
    { start: 265, length: 1, convRule: rule23 },
    { start: 266, length: 1, convRule: rule22 },
    { start: 267, length: 1, convRule: rule23 },
    { start: 268, length: 1, convRule: rule22 },
    { start: 269, length: 1, convRule: rule23 },
    { start: 270, length: 1, convRule: rule22 },
    { start: 271, length: 1, convRule: rule23 },
    { start: 272, length: 1, convRule: rule22 },
    { start: 273, length: 1, convRule: rule23 },
    { start: 274, length: 1, convRule: rule22 },
    { start: 275, length: 1, convRule: rule23 },
    { start: 276, length: 1, convRule: rule22 },
    { start: 277, length: 1, convRule: rule23 },
    { start: 278, length: 1, convRule: rule22 },
    { start: 279, length: 1, convRule: rule23 },
    { start: 280, length: 1, convRule: rule22 },
    { start: 281, length: 1, convRule: rule23 },
    { start: 282, length: 1, convRule: rule22 },
    { start: 283, length: 1, convRule: rule23 },
    { start: 284, length: 1, convRule: rule22 },
    { start: 285, length: 1, convRule: rule23 },
    { start: 286, length: 1, convRule: rule22 },
    { start: 287, length: 1, convRule: rule23 },
    { start: 288, length: 1, convRule: rule22 },
    { start: 289, length: 1, convRule: rule23 },
    { start: 290, length: 1, convRule: rule22 },
    { start: 291, length: 1, convRule: rule23 },
    { start: 292, length: 1, convRule: rule22 },
    { start: 293, length: 1, convRule: rule23 },
    { start: 294, length: 1, convRule: rule22 },
    { start: 295, length: 1, convRule: rule23 },
    { start: 296, length: 1, convRule: rule22 },
    { start: 297, length: 1, convRule: rule23 },
    { start: 298, length: 1, convRule: rule22 },
    { start: 299, length: 1, convRule: rule23 },
    { start: 300, length: 1, convRule: rule22 },
    { start: 301, length: 1, convRule: rule23 },
    { start: 302, length: 1, convRule: rule22 },
    { start: 303, length: 1, convRule: rule23 },
    { start: 304, length: 1, convRule: rule24 },
    { start: 305, length: 1, convRule: rule25 },
    { start: 306, length: 1, convRule: rule22 },
    { start: 307, length: 1, convRule: rule23 },
    { start: 308, length: 1, convRule: rule22 },
    { start: 309, length: 1, convRule: rule23 },
    { start: 310, length: 1, convRule: rule22 },
    { start: 311, length: 1, convRule: rule23 },
    { start: 312, length: 1, convRule: rule20 },
    { start: 313, length: 1, convRule: rule22 },
    { start: 314, length: 1, convRule: rule23 },
    { start: 315, length: 1, convRule: rule22 },
    { start: 316, length: 1, convRule: rule23 },
    { start: 317, length: 1, convRule: rule22 },
    { start: 318, length: 1, convRule: rule23 },
    { start: 319, length: 1, convRule: rule22 },
    { start: 320, length: 1, convRule: rule23 },
    { start: 321, length: 1, convRule: rule22 },
    { start: 322, length: 1, convRule: rule23 },
    { start: 323, length: 1, convRule: rule22 },
    { start: 324, length: 1, convRule: rule23 },
    { start: 325, length: 1, convRule: rule22 },
    { start: 326, length: 1, convRule: rule23 },
    { start: 327, length: 1, convRule: rule22 },
    { start: 328, length: 1, convRule: rule23 },
    { start: 329, length: 1, convRule: rule20 },
    { start: 330, length: 1, convRule: rule22 },
    { start: 331, length: 1, convRule: rule23 },
    { start: 332, length: 1, convRule: rule22 },
    { start: 333, length: 1, convRule: rule23 },
    { start: 334, length: 1, convRule: rule22 },
    { start: 335, length: 1, convRule: rule23 },
    { start: 336, length: 1, convRule: rule22 },
    { start: 337, length: 1, convRule: rule23 },
    { start: 338, length: 1, convRule: rule22 },
    { start: 339, length: 1, convRule: rule23 },
    { start: 340, length: 1, convRule: rule22 },
    { start: 341, length: 1, convRule: rule23 },
    { start: 342, length: 1, convRule: rule22 },
    { start: 343, length: 1, convRule: rule23 },
    { start: 344, length: 1, convRule: rule22 },
    { start: 345, length: 1, convRule: rule23 },
    { start: 346, length: 1, convRule: rule22 },
    { start: 347, length: 1, convRule: rule23 },
    { start: 348, length: 1, convRule: rule22 },
    { start: 349, length: 1, convRule: rule23 },
    { start: 350, length: 1, convRule: rule22 },
    { start: 351, length: 1, convRule: rule23 },
    { start: 352, length: 1, convRule: rule22 },
    { start: 353, length: 1, convRule: rule23 },
    { start: 354, length: 1, convRule: rule22 },
    { start: 355, length: 1, convRule: rule23 },
    { start: 356, length: 1, convRule: rule22 },
    { start: 357, length: 1, convRule: rule23 },
    { start: 358, length: 1, convRule: rule22 },
    { start: 359, length: 1, convRule: rule23 },
    { start: 360, length: 1, convRule: rule22 },
    { start: 361, length: 1, convRule: rule23 },
    { start: 362, length: 1, convRule: rule22 },
    { start: 363, length: 1, convRule: rule23 },
    { start: 364, length: 1, convRule: rule22 },
    { start: 365, length: 1, convRule: rule23 },
    { start: 366, length: 1, convRule: rule22 },
    { start: 367, length: 1, convRule: rule23 },
    { start: 368, length: 1, convRule: rule22 },
    { start: 369, length: 1, convRule: rule23 },
    { start: 370, length: 1, convRule: rule22 },
    { start: 371, length: 1, convRule: rule23 },
    { start: 372, length: 1, convRule: rule22 },
    { start: 373, length: 1, convRule: rule23 },
    { start: 374, length: 1, convRule: rule22 },
    { start: 375, length: 1, convRule: rule23 },
    { start: 376, length: 1, convRule: rule26 },
    { start: 377, length: 1, convRule: rule22 },
    { start: 378, length: 1, convRule: rule23 },
    { start: 379, length: 1, convRule: rule22 },
    { start: 380, length: 1, convRule: rule23 },
    { start: 381, length: 1, convRule: rule22 },
    { start: 382, length: 1, convRule: rule23 },
    { start: 383, length: 1, convRule: rule27 },
    { start: 384, length: 1, convRule: rule28 },
    { start: 385, length: 1, convRule: rule29 },
    { start: 386, length: 1, convRule: rule22 },
    { start: 387, length: 1, convRule: rule23 },
    { start: 388, length: 1, convRule: rule22 },
    { start: 389, length: 1, convRule: rule23 },
    { start: 390, length: 1, convRule: rule30 },
    { start: 391, length: 1, convRule: rule22 },
    { start: 392, length: 1, convRule: rule23 },
    { start: 393, length: 2, convRule: rule31 },
    { start: 395, length: 1, convRule: rule22 },
    { start: 396, length: 1, convRule: rule23 },
    { start: 397, length: 1, convRule: rule20 },
    { start: 398, length: 1, convRule: rule32 },
    { start: 399, length: 1, convRule: rule33 },
    { start: 400, length: 1, convRule: rule34 },
    { start: 401, length: 1, convRule: rule22 },
    { start: 402, length: 1, convRule: rule23 },
    { start: 403, length: 1, convRule: rule31 },
    { start: 404, length: 1, convRule: rule35 },
    { start: 405, length: 1, convRule: rule36 },
    { start: 406, length: 1, convRule: rule37 },
    { start: 407, length: 1, convRule: rule38 },
    { start: 408, length: 1, convRule: rule22 },
    { start: 409, length: 1, convRule: rule23 },
    { start: 410, length: 1, convRule: rule39 },
    { start: 411, length: 1, convRule: rule20 },
    { start: 412, length: 1, convRule: rule37 },
    { start: 413, length: 1, convRule: rule40 },
    { start: 414, length: 1, convRule: rule41 },
    { start: 415, length: 1, convRule: rule42 },
    { start: 416, length: 1, convRule: rule22 },
    { start: 417, length: 1, convRule: rule23 },
    { start: 418, length: 1, convRule: rule22 },
    { start: 419, length: 1, convRule: rule23 },
    { start: 420, length: 1, convRule: rule22 },
    { start: 421, length: 1, convRule: rule23 },
    { start: 422, length: 1, convRule: rule43 },
    { start: 423, length: 1, convRule: rule22 },
    { start: 424, length: 1, convRule: rule23 },
    { start: 425, length: 1, convRule: rule43 },
    { start: 426, length: 2, convRule: rule20 },
    { start: 428, length: 1, convRule: rule22 },
    { start: 429, length: 1, convRule: rule23 },
    { start: 430, length: 1, convRule: rule43 },
    { start: 431, length: 1, convRule: rule22 },
    { start: 432, length: 1, convRule: rule23 },
    { start: 433, length: 2, convRule: rule44 },
    { start: 435, length: 1, convRule: rule22 },
    { start: 436, length: 1, convRule: rule23 },
    { start: 437, length: 1, convRule: rule22 },
    { start: 438, length: 1, convRule: rule23 },
    { start: 439, length: 1, convRule: rule45 },
    { start: 440, length: 1, convRule: rule22 },
    { start: 441, length: 1, convRule: rule23 },
    { start: 442, length: 1, convRule: rule20 },
    { start: 443, length: 1, convRule: rule14 },
    { start: 444, length: 1, convRule: rule22 },
    { start: 445, length: 1, convRule: rule23 },
    { start: 446, length: 1, convRule: rule20 },
    { start: 447, length: 1, convRule: rule46 },
    { start: 448, length: 4, convRule: rule14 },
    { start: 452, length: 1, convRule: rule47 },
    { start: 453, length: 1, convRule: rule48 },
    { start: 454, length: 1, convRule: rule49 },
    { start: 455, length: 1, convRule: rule47 },
    { start: 456, length: 1, convRule: rule48 },
    { start: 457, length: 1, convRule: rule49 },
    { start: 458, length: 1, convRule: rule47 },
    { start: 459, length: 1, convRule: rule48 },
    { start: 460, length: 1, convRule: rule49 },
    { start: 461, length: 1, convRule: rule22 },
    { start: 462, length: 1, convRule: rule23 },
    { start: 463, length: 1, convRule: rule22 },
    { start: 464, length: 1, convRule: rule23 },
    { start: 465, length: 1, convRule: rule22 },
    { start: 466, length: 1, convRule: rule23 },
    { start: 467, length: 1, convRule: rule22 },
    { start: 468, length: 1, convRule: rule23 },
    { start: 469, length: 1, convRule: rule22 },
    { start: 470, length: 1, convRule: rule23 },
    { start: 471, length: 1, convRule: rule22 },
    { start: 472, length: 1, convRule: rule23 },
    { start: 473, length: 1, convRule: rule22 },
    { start: 474, length: 1, convRule: rule23 },
    { start: 475, length: 1, convRule: rule22 },
    { start: 476, length: 1, convRule: rule23 },
    { start: 477, length: 1, convRule: rule50 },
    { start: 478, length: 1, convRule: rule22 },
    { start: 479, length: 1, convRule: rule23 },
    { start: 480, length: 1, convRule: rule22 },
    { start: 481, length: 1, convRule: rule23 },
    { start: 482, length: 1, convRule: rule22 },
    { start: 483, length: 1, convRule: rule23 },
    { start: 484, length: 1, convRule: rule22 },
    { start: 485, length: 1, convRule: rule23 },
    { start: 486, length: 1, convRule: rule22 },
    { start: 487, length: 1, convRule: rule23 },
    { start: 488, length: 1, convRule: rule22 },
    { start: 489, length: 1, convRule: rule23 },
    { start: 490, length: 1, convRule: rule22 },
    { start: 491, length: 1, convRule: rule23 },
    { start: 492, length: 1, convRule: rule22 },
    { start: 493, length: 1, convRule: rule23 },
    { start: 494, length: 1, convRule: rule22 },
    { start: 495, length: 1, convRule: rule23 },
    { start: 496, length: 1, convRule: rule20 },
    { start: 497, length: 1, convRule: rule47 },
    { start: 498, length: 1, convRule: rule48 },
    { start: 499, length: 1, convRule: rule49 },
    { start: 500, length: 1, convRule: rule22 },
    { start: 501, length: 1, convRule: rule23 },
    { start: 502, length: 1, convRule: rule51 },
    { start: 503, length: 1, convRule: rule52 },
    { start: 504, length: 1, convRule: rule22 },
    { start: 505, length: 1, convRule: rule23 },
    { start: 506, length: 1, convRule: rule22 },
    { start: 507, length: 1, convRule: rule23 },
    { start: 508, length: 1, convRule: rule22 },
    { start: 509, length: 1, convRule: rule23 },
    { start: 510, length: 1, convRule: rule22 },
    { start: 511, length: 1, convRule: rule23 },
    { start: 512, length: 1, convRule: rule22 },
    { start: 513, length: 1, convRule: rule23 },
    { start: 514, length: 1, convRule: rule22 },
    { start: 515, length: 1, convRule: rule23 },
    { start: 516, length: 1, convRule: rule22 },
    { start: 517, length: 1, convRule: rule23 },
    { start: 518, length: 1, convRule: rule22 },
    { start: 519, length: 1, convRule: rule23 },
    { start: 520, length: 1, convRule: rule22 },
    { start: 521, length: 1, convRule: rule23 },
    { start: 522, length: 1, convRule: rule22 },
    { start: 523, length: 1, convRule: rule23 },
    { start: 524, length: 1, convRule: rule22 },
    { start: 525, length: 1, convRule: rule23 },
    { start: 526, length: 1, convRule: rule22 },
    { start: 527, length: 1, convRule: rule23 },
    { start: 528, length: 1, convRule: rule22 },
    { start: 529, length: 1, convRule: rule23 },
    { start: 530, length: 1, convRule: rule22 },
    { start: 531, length: 1, convRule: rule23 },
    { start: 532, length: 1, convRule: rule22 },
    { start: 533, length: 1, convRule: rule23 },
    { start: 534, length: 1, convRule: rule22 },
    { start: 535, length: 1, convRule: rule23 },
    { start: 536, length: 1, convRule: rule22 },
    { start: 537, length: 1, convRule: rule23 },
    { start: 538, length: 1, convRule: rule22 },
    { start: 539, length: 1, convRule: rule23 },
    { start: 540, length: 1, convRule: rule22 },
    { start: 541, length: 1, convRule: rule23 },
    { start: 542, length: 1, convRule: rule22 },
    { start: 543, length: 1, convRule: rule23 },
    { start: 544, length: 1, convRule: rule53 },
    { start: 545, length: 1, convRule: rule20 },
    { start: 546, length: 1, convRule: rule22 },
    { start: 547, length: 1, convRule: rule23 },
    { start: 548, length: 1, convRule: rule22 },
    { start: 549, length: 1, convRule: rule23 },
    { start: 550, length: 1, convRule: rule22 },
    { start: 551, length: 1, convRule: rule23 },
    { start: 552, length: 1, convRule: rule22 },
    { start: 553, length: 1, convRule: rule23 },
    { start: 554, length: 1, convRule: rule22 },
    { start: 555, length: 1, convRule: rule23 },
    { start: 556, length: 1, convRule: rule22 },
    { start: 557, length: 1, convRule: rule23 },
    { start: 558, length: 1, convRule: rule22 },
    { start: 559, length: 1, convRule: rule23 },
    { start: 560, length: 1, convRule: rule22 },
    { start: 561, length: 1, convRule: rule23 },
    { start: 562, length: 1, convRule: rule22 },
    { start: 563, length: 1, convRule: rule23 },
    { start: 564, length: 6, convRule: rule20 },
    { start: 570, length: 1, convRule: rule54 },
    { start: 571, length: 1, convRule: rule22 },
    { start: 572, length: 1, convRule: rule23 },
    { start: 573, length: 1, convRule: rule55 },
    { start: 574, length: 1, convRule: rule56 },
    { start: 575, length: 2, convRule: rule57 },
    { start: 577, length: 1, convRule: rule22 },
    { start: 578, length: 1, convRule: rule23 },
    { start: 579, length: 1, convRule: rule58 },
    { start: 580, length: 1, convRule: rule59 },
    { start: 581, length: 1, convRule: rule60 },
    { start: 582, length: 1, convRule: rule22 },
    { start: 583, length: 1, convRule: rule23 },
    { start: 584, length: 1, convRule: rule22 },
    { start: 585, length: 1, convRule: rule23 },
    { start: 586, length: 1, convRule: rule22 },
    { start: 587, length: 1, convRule: rule23 },
    { start: 588, length: 1, convRule: rule22 },
    { start: 589, length: 1, convRule: rule23 },
    { start: 590, length: 1, convRule: rule22 },
    { start: 591, length: 1, convRule: rule23 },
    { start: 592, length: 1, convRule: rule61 },
    { start: 593, length: 1, convRule: rule62 },
    { start: 594, length: 1, convRule: rule63 },
    { start: 595, length: 1, convRule: rule64 },
    { start: 596, length: 1, convRule: rule65 },
    { start: 597, length: 1, convRule: rule20 },
    { start: 598, length: 2, convRule: rule66 },
    { start: 600, length: 1, convRule: rule20 },
    { start: 601, length: 1, convRule: rule67 },
    { start: 602, length: 1, convRule: rule20 },
    { start: 603, length: 1, convRule: rule68 },
    { start: 604, length: 1, convRule: rule69 },
    { start: 605, length: 3, convRule: rule20 },
    { start: 608, length: 1, convRule: rule66 },
    { start: 609, length: 1, convRule: rule70 },
    { start: 610, length: 1, convRule: rule20 },
    { start: 611, length: 1, convRule: rule71 },
    { start: 612, length: 1, convRule: rule20 },
    { start: 613, length: 1, convRule: rule72 },
    { start: 614, length: 1, convRule: rule73 },
    { start: 615, length: 1, convRule: rule20 },
    { start: 616, length: 1, convRule: rule74 },
    { start: 617, length: 1, convRule: rule75 },
    { start: 618, length: 1, convRule: rule73 },
    { start: 619, length: 1, convRule: rule76 },
    { start: 620, length: 1, convRule: rule77 },
    { start: 621, length: 2, convRule: rule20 },
    { start: 623, length: 1, convRule: rule75 },
    { start: 624, length: 1, convRule: rule20 },
    { start: 625, length: 1, convRule: rule78 },
    { start: 626, length: 1, convRule: rule79 },
    { start: 627, length: 2, convRule: rule20 },
    { start: 629, length: 1, convRule: rule80 },
    { start: 630, length: 7, convRule: rule20 },
    { start: 637, length: 1, convRule: rule81 },
    { start: 638, length: 2, convRule: rule20 },
    { start: 640, length: 1, convRule: rule82 },
    { start: 641, length: 1, convRule: rule20 },
    { start: 642, length: 1, convRule: rule83 },
    { start: 643, length: 1, convRule: rule82 },
    { start: 644, length: 3, convRule: rule20 },
    { start: 647, length: 1, convRule: rule84 },
    { start: 648, length: 1, convRule: rule82 },
    { start: 649, length: 1, convRule: rule85 },
    { start: 650, length: 2, convRule: rule86 },
    { start: 652, length: 1, convRule: rule87 },
    { start: 653, length: 5, convRule: rule20 },
    { start: 658, length: 1, convRule: rule88 },
    { start: 659, length: 1, convRule: rule20 },
    { start: 660, length: 1, convRule: rule14 },
    { start: 661, length: 8, convRule: rule20 },
    { start: 669, length: 1, convRule: rule89 },
    { start: 670, length: 1, convRule: rule90 },
    { start: 671, length: 17, convRule: rule20 },
    { start: 688, length: 18, convRule: rule91 },
    { start: 706, length: 4, convRule: rule10 },
    { start: 710, length: 12, convRule: rule91 },
    { start: 722, length: 14, convRule: rule10 },
    { start: 736, length: 5, convRule: rule91 },
    { start: 741, length: 7, convRule: rule10 },
    { start: 748, length: 1, convRule: rule91 },
    { start: 749, length: 1, convRule: rule10 },
    { start: 750, length: 1, convRule: rule91 },
    { start: 751, length: 17, convRule: rule10 },
    { start: 768, length: 69, convRule: rule92 },
    { start: 837, length: 1, convRule: rule93 },
    { start: 838, length: 42, convRule: rule92 },
    { start: 880, length: 1, convRule: rule22 },
    { start: 881, length: 1, convRule: rule23 },
    { start: 882, length: 1, convRule: rule22 },
    { start: 883, length: 1, convRule: rule23 },
    { start: 884, length: 1, convRule: rule91 },
    { start: 885, length: 1, convRule: rule10 },
    { start: 886, length: 1, convRule: rule22 },
    { start: 887, length: 1, convRule: rule23 },
    { start: 890, length: 1, convRule: rule91 },
    { start: 891, length: 3, convRule: rule41 },
    { start: 894, length: 1, convRule: rule2 },
    { start: 895, length: 1, convRule: rule94 },
    { start: 900, length: 2, convRule: rule10 },
    { start: 902, length: 1, convRule: rule95 },
    { start: 903, length: 1, convRule: rule2 },
    { start: 904, length: 3, convRule: rule96 },
    { start: 908, length: 1, convRule: rule97 },
    { start: 910, length: 2, convRule: rule98 },
    { start: 912, length: 1, convRule: rule20 },
    { start: 913, length: 17, convRule: rule9 },
    { start: 931, length: 9, convRule: rule9 },
    { start: 940, length: 1, convRule: rule99 },
    { start: 941, length: 3, convRule: rule100 },
    { start: 944, length: 1, convRule: rule20 },
    { start: 945, length: 17, convRule: rule12 },
    { start: 962, length: 1, convRule: rule101 },
    { start: 963, length: 9, convRule: rule12 },
    { start: 972, length: 1, convRule: rule102 },
    { start: 973, length: 2, convRule: rule103 },
    { start: 975, length: 1, convRule: rule104 },
    { start: 976, length: 1, convRule: rule105 },
    { start: 977, length: 1, convRule: rule106 },
    { start: 978, length: 3, convRule: rule107 },
    { start: 981, length: 1, convRule: rule108 },
    { start: 982, length: 1, convRule: rule109 },
    { start: 983, length: 1, convRule: rule110 },
    { start: 984, length: 1, convRule: rule22 },
    { start: 985, length: 1, convRule: rule23 },
    { start: 986, length: 1, convRule: rule22 },
    { start: 987, length: 1, convRule: rule23 },
    { start: 988, length: 1, convRule: rule22 },
    { start: 989, length: 1, convRule: rule23 },
    { start: 990, length: 1, convRule: rule22 },
    { start: 991, length: 1, convRule: rule23 },
    { start: 992, length: 1, convRule: rule22 },
    { start: 993, length: 1, convRule: rule23 },
    { start: 994, length: 1, convRule: rule22 },
    { start: 995, length: 1, convRule: rule23 },
    { start: 996, length: 1, convRule: rule22 },
    { start: 997, length: 1, convRule: rule23 },
    { start: 998, length: 1, convRule: rule22 },
    { start: 999, length: 1, convRule: rule23 },
    { start: 1e3, length: 1, convRule: rule22 },
    { start: 1001, length: 1, convRule: rule23 },
    { start: 1002, length: 1, convRule: rule22 },
    { start: 1003, length: 1, convRule: rule23 },
    { start: 1004, length: 1, convRule: rule22 },
    { start: 1005, length: 1, convRule: rule23 },
    { start: 1006, length: 1, convRule: rule22 },
    { start: 1007, length: 1, convRule: rule23 },
    { start: 1008, length: 1, convRule: rule111 },
    { start: 1009, length: 1, convRule: rule112 },
    { start: 1010, length: 1, convRule: rule113 },
    { start: 1011, length: 1, convRule: rule114 },
    { start: 1012, length: 1, convRule: rule115 },
    { start: 1013, length: 1, convRule: rule116 },
    { start: 1014, length: 1, convRule: rule6 },
    { start: 1015, length: 1, convRule: rule22 },
    { start: 1016, length: 1, convRule: rule23 },
    { start: 1017, length: 1, convRule: rule117 },
    { start: 1018, length: 1, convRule: rule22 },
    { start: 1019, length: 1, convRule: rule23 },
    { start: 1020, length: 1, convRule: rule20 },
    { start: 1021, length: 3, convRule: rule53 },
    { start: 1024, length: 16, convRule: rule118 },
    { start: 1040, length: 32, convRule: rule9 },
    { start: 1072, length: 32, convRule: rule12 },
    { start: 1104, length: 16, convRule: rule112 },
    { start: 1120, length: 1, convRule: rule22 },
    { start: 1121, length: 1, convRule: rule23 },
    { start: 1122, length: 1, convRule: rule22 },
    { start: 1123, length: 1, convRule: rule23 },
    { start: 1124, length: 1, convRule: rule22 },
    { start: 1125, length: 1, convRule: rule23 },
    { start: 1126, length: 1, convRule: rule22 },
    { start: 1127, length: 1, convRule: rule23 },
    { start: 1128, length: 1, convRule: rule22 },
    { start: 1129, length: 1, convRule: rule23 },
    { start: 1130, length: 1, convRule: rule22 },
    { start: 1131, length: 1, convRule: rule23 },
    { start: 1132, length: 1, convRule: rule22 },
    { start: 1133, length: 1, convRule: rule23 },
    { start: 1134, length: 1, convRule: rule22 },
    { start: 1135, length: 1, convRule: rule23 },
    { start: 1136, length: 1, convRule: rule22 },
    { start: 1137, length: 1, convRule: rule23 },
    { start: 1138, length: 1, convRule: rule22 },
    { start: 1139, length: 1, convRule: rule23 },
    { start: 1140, length: 1, convRule: rule22 },
    { start: 1141, length: 1, convRule: rule23 },
    { start: 1142, length: 1, convRule: rule22 },
    { start: 1143, length: 1, convRule: rule23 },
    { start: 1144, length: 1, convRule: rule22 },
    { start: 1145, length: 1, convRule: rule23 },
    { start: 1146, length: 1, convRule: rule22 },
    { start: 1147, length: 1, convRule: rule23 },
    { start: 1148, length: 1, convRule: rule22 },
    { start: 1149, length: 1, convRule: rule23 },
    { start: 1150, length: 1, convRule: rule22 },
    { start: 1151, length: 1, convRule: rule23 },
    { start: 1152, length: 1, convRule: rule22 },
    { start: 1153, length: 1, convRule: rule23 },
    { start: 1154, length: 1, convRule: rule13 },
    { start: 1155, length: 5, convRule: rule92 },
    { start: 1160, length: 2, convRule: rule119 },
    { start: 1162, length: 1, convRule: rule22 },
    { start: 1163, length: 1, convRule: rule23 },
    { start: 1164, length: 1, convRule: rule22 },
    { start: 1165, length: 1, convRule: rule23 },
    { start: 1166, length: 1, convRule: rule22 },
    { start: 1167, length: 1, convRule: rule23 },
    { start: 1168, length: 1, convRule: rule22 },
    { start: 1169, length: 1, convRule: rule23 },
    { start: 1170, length: 1, convRule: rule22 },
    { start: 1171, length: 1, convRule: rule23 },
    { start: 1172, length: 1, convRule: rule22 },
    { start: 1173, length: 1, convRule: rule23 },
    { start: 1174, length: 1, convRule: rule22 },
    { start: 1175, length: 1, convRule: rule23 },
    { start: 1176, length: 1, convRule: rule22 },
    { start: 1177, length: 1, convRule: rule23 },
    { start: 1178, length: 1, convRule: rule22 },
    { start: 1179, length: 1, convRule: rule23 },
    { start: 1180, length: 1, convRule: rule22 },
    { start: 1181, length: 1, convRule: rule23 },
    { start: 1182, length: 1, convRule: rule22 },
    { start: 1183, length: 1, convRule: rule23 },
    { start: 1184, length: 1, convRule: rule22 },
    { start: 1185, length: 1, convRule: rule23 },
    { start: 1186, length: 1, convRule: rule22 },
    { start: 1187, length: 1, convRule: rule23 },
    { start: 1188, length: 1, convRule: rule22 },
    { start: 1189, length: 1, convRule: rule23 },
    { start: 1190, length: 1, convRule: rule22 },
    { start: 1191, length: 1, convRule: rule23 },
    { start: 1192, length: 1, convRule: rule22 },
    { start: 1193, length: 1, convRule: rule23 },
    { start: 1194, length: 1, convRule: rule22 },
    { start: 1195, length: 1, convRule: rule23 },
    { start: 1196, length: 1, convRule: rule22 },
    { start: 1197, length: 1, convRule: rule23 },
    { start: 1198, length: 1, convRule: rule22 },
    { start: 1199, length: 1, convRule: rule23 },
    { start: 1200, length: 1, convRule: rule22 },
    { start: 1201, length: 1, convRule: rule23 },
    { start: 1202, length: 1, convRule: rule22 },
    { start: 1203, length: 1, convRule: rule23 },
    { start: 1204, length: 1, convRule: rule22 },
    { start: 1205, length: 1, convRule: rule23 },
    { start: 1206, length: 1, convRule: rule22 },
    { start: 1207, length: 1, convRule: rule23 },
    { start: 1208, length: 1, convRule: rule22 },
    { start: 1209, length: 1, convRule: rule23 },
    { start: 1210, length: 1, convRule: rule22 },
    { start: 1211, length: 1, convRule: rule23 },
    { start: 1212, length: 1, convRule: rule22 },
    { start: 1213, length: 1, convRule: rule23 },
    { start: 1214, length: 1, convRule: rule22 },
    { start: 1215, length: 1, convRule: rule23 },
    { start: 1216, length: 1, convRule: rule120 },
    { start: 1217, length: 1, convRule: rule22 },
    { start: 1218, length: 1, convRule: rule23 },
    { start: 1219, length: 1, convRule: rule22 },
    { start: 1220, length: 1, convRule: rule23 },
    { start: 1221, length: 1, convRule: rule22 },
    { start: 1222, length: 1, convRule: rule23 },
    { start: 1223, length: 1, convRule: rule22 },
    { start: 1224, length: 1, convRule: rule23 },
    { start: 1225, length: 1, convRule: rule22 },
    { start: 1226, length: 1, convRule: rule23 },
    { start: 1227, length: 1, convRule: rule22 },
    { start: 1228, length: 1, convRule: rule23 },
    { start: 1229, length: 1, convRule: rule22 },
    { start: 1230, length: 1, convRule: rule23 },
    { start: 1231, length: 1, convRule: rule121 },
    { start: 1232, length: 1, convRule: rule22 },
    { start: 1233, length: 1, convRule: rule23 },
    { start: 1234, length: 1, convRule: rule22 },
    { start: 1235, length: 1, convRule: rule23 },
    { start: 1236, length: 1, convRule: rule22 },
    { start: 1237, length: 1, convRule: rule23 },
    { start: 1238, length: 1, convRule: rule22 },
    { start: 1239, length: 1, convRule: rule23 },
    { start: 1240, length: 1, convRule: rule22 },
    { start: 1241, length: 1, convRule: rule23 },
    { start: 1242, length: 1, convRule: rule22 },
    { start: 1243, length: 1, convRule: rule23 },
    { start: 1244, length: 1, convRule: rule22 },
    { start: 1245, length: 1, convRule: rule23 },
    { start: 1246, length: 1, convRule: rule22 },
    { start: 1247, length: 1, convRule: rule23 },
    { start: 1248, length: 1, convRule: rule22 },
    { start: 1249, length: 1, convRule: rule23 },
    { start: 1250, length: 1, convRule: rule22 },
    { start: 1251, length: 1, convRule: rule23 },
    { start: 1252, length: 1, convRule: rule22 },
    { start: 1253, length: 1, convRule: rule23 },
    { start: 1254, length: 1, convRule: rule22 },
    { start: 1255, length: 1, convRule: rule23 },
    { start: 1256, length: 1, convRule: rule22 },
    { start: 1257, length: 1, convRule: rule23 },
    { start: 1258, length: 1, convRule: rule22 },
    { start: 1259, length: 1, convRule: rule23 },
    { start: 1260, length: 1, convRule: rule22 },
    { start: 1261, length: 1, convRule: rule23 },
    { start: 1262, length: 1, convRule: rule22 },
    { start: 1263, length: 1, convRule: rule23 },
    { start: 1264, length: 1, convRule: rule22 },
    { start: 1265, length: 1, convRule: rule23 },
    { start: 1266, length: 1, convRule: rule22 },
    { start: 1267, length: 1, convRule: rule23 },
    { start: 1268, length: 1, convRule: rule22 },
    { start: 1269, length: 1, convRule: rule23 },
    { start: 1270, length: 1, convRule: rule22 },
    { start: 1271, length: 1, convRule: rule23 },
    { start: 1272, length: 1, convRule: rule22 },
    { start: 1273, length: 1, convRule: rule23 },
    { start: 1274, length: 1, convRule: rule22 },
    { start: 1275, length: 1, convRule: rule23 },
    { start: 1276, length: 1, convRule: rule22 },
    { start: 1277, length: 1, convRule: rule23 },
    { start: 1278, length: 1, convRule: rule22 },
    { start: 1279, length: 1, convRule: rule23 },
    { start: 1280, length: 1, convRule: rule22 },
    { start: 1281, length: 1, convRule: rule23 },
    { start: 1282, length: 1, convRule: rule22 },
    { start: 1283, length: 1, convRule: rule23 },
    { start: 1284, length: 1, convRule: rule22 },
    { start: 1285, length: 1, convRule: rule23 },
    { start: 1286, length: 1, convRule: rule22 },
    { start: 1287, length: 1, convRule: rule23 },
    { start: 1288, length: 1, convRule: rule22 },
    { start: 1289, length: 1, convRule: rule23 },
    { start: 1290, length: 1, convRule: rule22 },
    { start: 1291, length: 1, convRule: rule23 },
    { start: 1292, length: 1, convRule: rule22 },
    { start: 1293, length: 1, convRule: rule23 },
    { start: 1294, length: 1, convRule: rule22 },
    { start: 1295, length: 1, convRule: rule23 },
    { start: 1296, length: 1, convRule: rule22 },
    { start: 1297, length: 1, convRule: rule23 },
    { start: 1298, length: 1, convRule: rule22 },
    { start: 1299, length: 1, convRule: rule23 },
    { start: 1300, length: 1, convRule: rule22 },
    { start: 1301, length: 1, convRule: rule23 },
    { start: 1302, length: 1, convRule: rule22 },
    { start: 1303, length: 1, convRule: rule23 },
    { start: 1304, length: 1, convRule: rule22 },
    { start: 1305, length: 1, convRule: rule23 },
    { start: 1306, length: 1, convRule: rule22 },
    { start: 1307, length: 1, convRule: rule23 },
    { start: 1308, length: 1, convRule: rule22 },
    { start: 1309, length: 1, convRule: rule23 },
    { start: 1310, length: 1, convRule: rule22 },
    { start: 1311, length: 1, convRule: rule23 },
    { start: 1312, length: 1, convRule: rule22 },
    { start: 1313, length: 1, convRule: rule23 },
    { start: 1314, length: 1, convRule: rule22 },
    { start: 1315, length: 1, convRule: rule23 },
    { start: 1316, length: 1, convRule: rule22 },
    { start: 1317, length: 1, convRule: rule23 },
    { start: 1318, length: 1, convRule: rule22 },
    { start: 1319, length: 1, convRule: rule23 },
    { start: 1320, length: 1, convRule: rule22 },
    { start: 1321, length: 1, convRule: rule23 },
    { start: 1322, length: 1, convRule: rule22 },
    { start: 1323, length: 1, convRule: rule23 },
    { start: 1324, length: 1, convRule: rule22 },
    { start: 1325, length: 1, convRule: rule23 },
    { start: 1326, length: 1, convRule: rule22 },
    { start: 1327, length: 1, convRule: rule23 },
    { start: 1329, length: 38, convRule: rule122 },
    { start: 1369, length: 1, convRule: rule91 },
    { start: 1370, length: 6, convRule: rule2 },
    { start: 1376, length: 1, convRule: rule20 },
    { start: 1377, length: 38, convRule: rule123 },
    { start: 1415, length: 2, convRule: rule20 },
    { start: 1417, length: 1, convRule: rule2 },
    { start: 1418, length: 1, convRule: rule7 },
    { start: 1421, length: 2, convRule: rule13 },
    { start: 1423, length: 1, convRule: rule3 },
    { start: 1425, length: 45, convRule: rule92 },
    { start: 1470, length: 1, convRule: rule7 },
    { start: 1471, length: 1, convRule: rule92 },
    { start: 1472, length: 1, convRule: rule2 },
    { start: 1473, length: 2, convRule: rule92 },
    { start: 1475, length: 1, convRule: rule2 },
    { start: 1476, length: 2, convRule: rule92 },
    { start: 1478, length: 1, convRule: rule2 },
    { start: 1479, length: 1, convRule: rule92 },
    { start: 1488, length: 27, convRule: rule14 },
    { start: 1519, length: 4, convRule: rule14 },
    { start: 1523, length: 2, convRule: rule2 },
    { start: 1536, length: 6, convRule: rule16 },
    { start: 1542, length: 3, convRule: rule6 },
    { start: 1545, length: 2, convRule: rule2 },
    { start: 1547, length: 1, convRule: rule3 },
    { start: 1548, length: 2, convRule: rule2 },
    { start: 1550, length: 2, convRule: rule13 },
    { start: 1552, length: 11, convRule: rule92 },
    { start: 1563, length: 1, convRule: rule2 },
    { start: 1564, length: 1, convRule: rule16 },
    { start: 1566, length: 2, convRule: rule2 },
    { start: 1568, length: 32, convRule: rule14 },
    { start: 1600, length: 1, convRule: rule91 },
    { start: 1601, length: 10, convRule: rule14 },
    { start: 1611, length: 21, convRule: rule92 },
    { start: 1632, length: 10, convRule: rule8 },
    { start: 1642, length: 4, convRule: rule2 },
    { start: 1646, length: 2, convRule: rule14 },
    { start: 1648, length: 1, convRule: rule92 },
    { start: 1649, length: 99, convRule: rule14 },
    { start: 1748, length: 1, convRule: rule2 },
    { start: 1749, length: 1, convRule: rule14 },
    { start: 1750, length: 7, convRule: rule92 },
    { start: 1757, length: 1, convRule: rule16 },
    { start: 1758, length: 1, convRule: rule13 },
    { start: 1759, length: 6, convRule: rule92 },
    { start: 1765, length: 2, convRule: rule91 },
    { start: 1767, length: 2, convRule: rule92 },
    { start: 1769, length: 1, convRule: rule13 },
    { start: 1770, length: 4, convRule: rule92 },
    { start: 1774, length: 2, convRule: rule14 },
    { start: 1776, length: 10, convRule: rule8 },
    { start: 1786, length: 3, convRule: rule14 },
    { start: 1789, length: 2, convRule: rule13 },
    { start: 1791, length: 1, convRule: rule14 },
    { start: 1792, length: 14, convRule: rule2 },
    { start: 1807, length: 1, convRule: rule16 },
    { start: 1808, length: 1, convRule: rule14 },
    { start: 1809, length: 1, convRule: rule92 },
    { start: 1810, length: 30, convRule: rule14 },
    { start: 1840, length: 27, convRule: rule92 },
    { start: 1869, length: 89, convRule: rule14 },
    { start: 1958, length: 11, convRule: rule92 },
    { start: 1969, length: 1, convRule: rule14 },
    { start: 1984, length: 10, convRule: rule8 },
    { start: 1994, length: 33, convRule: rule14 },
    { start: 2027, length: 9, convRule: rule92 },
    { start: 2036, length: 2, convRule: rule91 },
    { start: 2038, length: 1, convRule: rule13 },
    { start: 2039, length: 3, convRule: rule2 },
    { start: 2042, length: 1, convRule: rule91 },
    { start: 2045, length: 1, convRule: rule92 },
    { start: 2046, length: 2, convRule: rule3 },
    { start: 2048, length: 22, convRule: rule14 },
    { start: 2070, length: 4, convRule: rule92 },
    { start: 2074, length: 1, convRule: rule91 },
    { start: 2075, length: 9, convRule: rule92 },
    { start: 2084, length: 1, convRule: rule91 },
    { start: 2085, length: 3, convRule: rule92 },
    { start: 2088, length: 1, convRule: rule91 },
    { start: 2089, length: 5, convRule: rule92 },
    { start: 2096, length: 15, convRule: rule2 },
    { start: 2112, length: 25, convRule: rule14 },
    { start: 2137, length: 3, convRule: rule92 },
    { start: 2142, length: 1, convRule: rule2 },
    { start: 2144, length: 11, convRule: rule14 },
    { start: 2208, length: 21, convRule: rule14 },
    { start: 2230, length: 18, convRule: rule14 },
    { start: 2259, length: 15, convRule: rule92 },
    { start: 2274, length: 1, convRule: rule16 },
    { start: 2275, length: 32, convRule: rule92 },
    { start: 2307, length: 1, convRule: rule124 },
    { start: 2308, length: 54, convRule: rule14 },
    { start: 2362, length: 1, convRule: rule92 },
    { start: 2363, length: 1, convRule: rule124 },
    { start: 2364, length: 1, convRule: rule92 },
    { start: 2365, length: 1, convRule: rule14 },
    { start: 2366, length: 3, convRule: rule124 },
    { start: 2369, length: 8, convRule: rule92 },
    { start: 2377, length: 4, convRule: rule124 },
    { start: 2381, length: 1, convRule: rule92 },
    { start: 2382, length: 2, convRule: rule124 },
    { start: 2384, length: 1, convRule: rule14 },
    { start: 2385, length: 7, convRule: rule92 },
    { start: 2392, length: 10, convRule: rule14 },
    { start: 2402, length: 2, convRule: rule92 },
    { start: 2404, length: 2, convRule: rule2 },
    { start: 2406, length: 10, convRule: rule8 },
    { start: 2416, length: 1, convRule: rule2 },
    { start: 2417, length: 1, convRule: rule91 },
    { start: 2418, length: 15, convRule: rule14 },
    { start: 2433, length: 1, convRule: rule92 },
    { start: 2434, length: 2, convRule: rule124 },
    { start: 2437, length: 8, convRule: rule14 },
    { start: 2447, length: 2, convRule: rule14 },
    { start: 2451, length: 22, convRule: rule14 },
    { start: 2474, length: 7, convRule: rule14 },
    { start: 2482, length: 1, convRule: rule14 },
    { start: 2486, length: 4, convRule: rule14 },
    { start: 2492, length: 1, convRule: rule92 },
    { start: 2493, length: 1, convRule: rule14 },
    { start: 2494, length: 3, convRule: rule124 },
    { start: 2497, length: 4, convRule: rule92 },
    { start: 2503, length: 2, convRule: rule124 },
    { start: 2507, length: 2, convRule: rule124 },
    { start: 2509, length: 1, convRule: rule92 },
    { start: 2510, length: 1, convRule: rule14 },
    { start: 2519, length: 1, convRule: rule124 },
    { start: 2524, length: 2, convRule: rule14 },
    { start: 2527, length: 3, convRule: rule14 },
    { start: 2530, length: 2, convRule: rule92 },
    { start: 2534, length: 10, convRule: rule8 },
    { start: 2544, length: 2, convRule: rule14 },
    { start: 2546, length: 2, convRule: rule3 },
    { start: 2548, length: 6, convRule: rule17 },
    { start: 2554, length: 1, convRule: rule13 },
    { start: 2555, length: 1, convRule: rule3 },
    { start: 2556, length: 1, convRule: rule14 },
    { start: 2557, length: 1, convRule: rule2 },
    { start: 2558, length: 1, convRule: rule92 },
    { start: 2561, length: 2, convRule: rule92 },
    { start: 2563, length: 1, convRule: rule124 },
    { start: 2565, length: 6, convRule: rule14 },
    { start: 2575, length: 2, convRule: rule14 },
    { start: 2579, length: 22, convRule: rule14 },
    { start: 2602, length: 7, convRule: rule14 },
    { start: 2610, length: 2, convRule: rule14 },
    { start: 2613, length: 2, convRule: rule14 },
    { start: 2616, length: 2, convRule: rule14 },
    { start: 2620, length: 1, convRule: rule92 },
    { start: 2622, length: 3, convRule: rule124 },
    { start: 2625, length: 2, convRule: rule92 },
    { start: 2631, length: 2, convRule: rule92 },
    { start: 2635, length: 3, convRule: rule92 },
    { start: 2641, length: 1, convRule: rule92 },
    { start: 2649, length: 4, convRule: rule14 },
    { start: 2654, length: 1, convRule: rule14 },
    { start: 2662, length: 10, convRule: rule8 },
    { start: 2672, length: 2, convRule: rule92 },
    { start: 2674, length: 3, convRule: rule14 },
    { start: 2677, length: 1, convRule: rule92 },
    { start: 2678, length: 1, convRule: rule2 },
    { start: 2689, length: 2, convRule: rule92 },
    { start: 2691, length: 1, convRule: rule124 },
    { start: 2693, length: 9, convRule: rule14 },
    { start: 2703, length: 3, convRule: rule14 },
    { start: 2707, length: 22, convRule: rule14 },
    { start: 2730, length: 7, convRule: rule14 },
    { start: 2738, length: 2, convRule: rule14 },
    { start: 2741, length: 5, convRule: rule14 },
    { start: 2748, length: 1, convRule: rule92 },
    { start: 2749, length: 1, convRule: rule14 },
    { start: 2750, length: 3, convRule: rule124 },
    { start: 2753, length: 5, convRule: rule92 },
    { start: 2759, length: 2, convRule: rule92 },
    { start: 2761, length: 1, convRule: rule124 },
    { start: 2763, length: 2, convRule: rule124 },
    { start: 2765, length: 1, convRule: rule92 },
    { start: 2768, length: 1, convRule: rule14 },
    { start: 2784, length: 2, convRule: rule14 },
    { start: 2786, length: 2, convRule: rule92 },
    { start: 2790, length: 10, convRule: rule8 },
    { start: 2800, length: 1, convRule: rule2 },
    { start: 2801, length: 1, convRule: rule3 },
    { start: 2809, length: 1, convRule: rule14 },
    { start: 2810, length: 6, convRule: rule92 },
    { start: 2817, length: 1, convRule: rule92 },
    { start: 2818, length: 2, convRule: rule124 },
    { start: 2821, length: 8, convRule: rule14 },
    { start: 2831, length: 2, convRule: rule14 },
    { start: 2835, length: 22, convRule: rule14 },
    { start: 2858, length: 7, convRule: rule14 },
    { start: 2866, length: 2, convRule: rule14 },
    { start: 2869, length: 5, convRule: rule14 },
    { start: 2876, length: 1, convRule: rule92 },
    { start: 2877, length: 1, convRule: rule14 },
    { start: 2878, length: 1, convRule: rule124 },
    { start: 2879, length: 1, convRule: rule92 },
    { start: 2880, length: 1, convRule: rule124 },
    { start: 2881, length: 4, convRule: rule92 },
    { start: 2887, length: 2, convRule: rule124 },
    { start: 2891, length: 2, convRule: rule124 },
    { start: 2893, length: 1, convRule: rule92 },
    { start: 2901, length: 2, convRule: rule92 },
    { start: 2903, length: 1, convRule: rule124 },
    { start: 2908, length: 2, convRule: rule14 },
    { start: 2911, length: 3, convRule: rule14 },
    { start: 2914, length: 2, convRule: rule92 },
    { start: 2918, length: 10, convRule: rule8 },
    { start: 2928, length: 1, convRule: rule13 },
    { start: 2929, length: 1, convRule: rule14 },
    { start: 2930, length: 6, convRule: rule17 },
    { start: 2946, length: 1, convRule: rule92 },
    { start: 2947, length: 1, convRule: rule14 },
    { start: 2949, length: 6, convRule: rule14 },
    { start: 2958, length: 3, convRule: rule14 },
    { start: 2962, length: 4, convRule: rule14 },
    { start: 2969, length: 2, convRule: rule14 },
    { start: 2972, length: 1, convRule: rule14 },
    { start: 2974, length: 2, convRule: rule14 },
    { start: 2979, length: 2, convRule: rule14 },
    { start: 2984, length: 3, convRule: rule14 },
    { start: 2990, length: 12, convRule: rule14 },
    { start: 3006, length: 2, convRule: rule124 },
    { start: 3008, length: 1, convRule: rule92 },
    { start: 3009, length: 2, convRule: rule124 },
    { start: 3014, length: 3, convRule: rule124 },
    { start: 3018, length: 3, convRule: rule124 },
    { start: 3021, length: 1, convRule: rule92 },
    { start: 3024, length: 1, convRule: rule14 },
    { start: 3031, length: 1, convRule: rule124 },
    { start: 3046, length: 10, convRule: rule8 },
    { start: 3056, length: 3, convRule: rule17 },
    { start: 3059, length: 6, convRule: rule13 },
    { start: 3065, length: 1, convRule: rule3 },
    { start: 3066, length: 1, convRule: rule13 },
    { start: 3072, length: 1, convRule: rule92 },
    { start: 3073, length: 3, convRule: rule124 },
    { start: 3076, length: 1, convRule: rule92 },
    { start: 3077, length: 8, convRule: rule14 },
    { start: 3086, length: 3, convRule: rule14 },
    { start: 3090, length: 23, convRule: rule14 },
    { start: 3114, length: 16, convRule: rule14 },
    { start: 3133, length: 1, convRule: rule14 },
    { start: 3134, length: 3, convRule: rule92 },
    { start: 3137, length: 4, convRule: rule124 },
    { start: 3142, length: 3, convRule: rule92 },
    { start: 3146, length: 4, convRule: rule92 },
    { start: 3157, length: 2, convRule: rule92 },
    { start: 3160, length: 3, convRule: rule14 },
    { start: 3168, length: 2, convRule: rule14 },
    { start: 3170, length: 2, convRule: rule92 },
    { start: 3174, length: 10, convRule: rule8 },
    { start: 3191, length: 1, convRule: rule2 },
    { start: 3192, length: 7, convRule: rule17 },
    { start: 3199, length: 1, convRule: rule13 },
    { start: 3200, length: 1, convRule: rule14 },
    { start: 3201, length: 1, convRule: rule92 },
    { start: 3202, length: 2, convRule: rule124 },
    { start: 3204, length: 1, convRule: rule2 },
    { start: 3205, length: 8, convRule: rule14 },
    { start: 3214, length: 3, convRule: rule14 },
    { start: 3218, length: 23, convRule: rule14 },
    { start: 3242, length: 10, convRule: rule14 },
    { start: 3253, length: 5, convRule: rule14 },
    { start: 3260, length: 1, convRule: rule92 },
    { start: 3261, length: 1, convRule: rule14 },
    { start: 3262, length: 1, convRule: rule124 },
    { start: 3263, length: 1, convRule: rule92 },
    { start: 3264, length: 5, convRule: rule124 },
    { start: 3270, length: 1, convRule: rule92 },
    { start: 3271, length: 2, convRule: rule124 },
    { start: 3274, length: 2, convRule: rule124 },
    { start: 3276, length: 2, convRule: rule92 },
    { start: 3285, length: 2, convRule: rule124 },
    { start: 3294, length: 1, convRule: rule14 },
    { start: 3296, length: 2, convRule: rule14 },
    { start: 3298, length: 2, convRule: rule92 },
    { start: 3302, length: 10, convRule: rule8 },
    { start: 3313, length: 2, convRule: rule14 },
    { start: 3328, length: 2, convRule: rule92 },
    { start: 3330, length: 2, convRule: rule124 },
    { start: 3332, length: 9, convRule: rule14 },
    { start: 3342, length: 3, convRule: rule14 },
    { start: 3346, length: 41, convRule: rule14 },
    { start: 3387, length: 2, convRule: rule92 },
    { start: 3389, length: 1, convRule: rule14 },
    { start: 3390, length: 3, convRule: rule124 },
    { start: 3393, length: 4, convRule: rule92 },
    { start: 3398, length: 3, convRule: rule124 },
    { start: 3402, length: 3, convRule: rule124 },
    { start: 3405, length: 1, convRule: rule92 },
    { start: 3406, length: 1, convRule: rule14 },
    { start: 3407, length: 1, convRule: rule13 },
    { start: 3412, length: 3, convRule: rule14 },
    { start: 3415, length: 1, convRule: rule124 },
    { start: 3416, length: 7, convRule: rule17 },
    { start: 3423, length: 3, convRule: rule14 },
    { start: 3426, length: 2, convRule: rule92 },
    { start: 3430, length: 10, convRule: rule8 },
    { start: 3440, length: 9, convRule: rule17 },
    { start: 3449, length: 1, convRule: rule13 },
    { start: 3450, length: 6, convRule: rule14 },
    { start: 3457, length: 1, convRule: rule92 },
    { start: 3458, length: 2, convRule: rule124 },
    { start: 3461, length: 18, convRule: rule14 },
    { start: 3482, length: 24, convRule: rule14 },
    { start: 3507, length: 9, convRule: rule14 },
    { start: 3517, length: 1, convRule: rule14 },
    { start: 3520, length: 7, convRule: rule14 },
    { start: 3530, length: 1, convRule: rule92 },
    { start: 3535, length: 3, convRule: rule124 },
    { start: 3538, length: 3, convRule: rule92 },
    { start: 3542, length: 1, convRule: rule92 },
    { start: 3544, length: 8, convRule: rule124 },
    { start: 3558, length: 10, convRule: rule8 },
    { start: 3570, length: 2, convRule: rule124 },
    { start: 3572, length: 1, convRule: rule2 },
    { start: 3585, length: 48, convRule: rule14 },
    { start: 3633, length: 1, convRule: rule92 },
    { start: 3634, length: 2, convRule: rule14 },
    { start: 3636, length: 7, convRule: rule92 },
    { start: 3647, length: 1, convRule: rule3 },
    { start: 3648, length: 6, convRule: rule14 },
    { start: 3654, length: 1, convRule: rule91 },
    { start: 3655, length: 8, convRule: rule92 },
    { start: 3663, length: 1, convRule: rule2 },
    { start: 3664, length: 10, convRule: rule8 },
    { start: 3674, length: 2, convRule: rule2 },
    { start: 3713, length: 2, convRule: rule14 },
    { start: 3716, length: 1, convRule: rule14 },
    { start: 3718, length: 5, convRule: rule14 },
    { start: 3724, length: 24, convRule: rule14 },
    { start: 3749, length: 1, convRule: rule14 },
    { start: 3751, length: 10, convRule: rule14 },
    { start: 3761, length: 1, convRule: rule92 },
    { start: 3762, length: 2, convRule: rule14 },
    { start: 3764, length: 9, convRule: rule92 },
    { start: 3773, length: 1, convRule: rule14 },
    { start: 3776, length: 5, convRule: rule14 },
    { start: 3782, length: 1, convRule: rule91 },
    { start: 3784, length: 6, convRule: rule92 },
    { start: 3792, length: 10, convRule: rule8 },
    { start: 3804, length: 4, convRule: rule14 },
    { start: 3840, length: 1, convRule: rule14 },
    { start: 3841, length: 3, convRule: rule13 },
    { start: 3844, length: 15, convRule: rule2 },
    { start: 3859, length: 1, convRule: rule13 },
    { start: 3860, length: 1, convRule: rule2 },
    { start: 3861, length: 3, convRule: rule13 },
    { start: 3864, length: 2, convRule: rule92 },
    { start: 3866, length: 6, convRule: rule13 },
    { start: 3872, length: 10, convRule: rule8 },
    { start: 3882, length: 10, convRule: rule17 },
    { start: 3892, length: 1, convRule: rule13 },
    { start: 3893, length: 1, convRule: rule92 },
    { start: 3894, length: 1, convRule: rule13 },
    { start: 3895, length: 1, convRule: rule92 },
    { start: 3896, length: 1, convRule: rule13 },
    { start: 3897, length: 1, convRule: rule92 },
    { start: 3898, length: 1, convRule: rule4 },
    { start: 3899, length: 1, convRule: rule5 },
    { start: 3900, length: 1, convRule: rule4 },
    { start: 3901, length: 1, convRule: rule5 },
    { start: 3902, length: 2, convRule: rule124 },
    { start: 3904, length: 8, convRule: rule14 },
    { start: 3913, length: 36, convRule: rule14 },
    { start: 3953, length: 14, convRule: rule92 },
    { start: 3967, length: 1, convRule: rule124 },
    { start: 3968, length: 5, convRule: rule92 },
    { start: 3973, length: 1, convRule: rule2 },
    { start: 3974, length: 2, convRule: rule92 },
    { start: 3976, length: 5, convRule: rule14 },
    { start: 3981, length: 11, convRule: rule92 },
    { start: 3993, length: 36, convRule: rule92 },
    { start: 4030, length: 8, convRule: rule13 },
    { start: 4038, length: 1, convRule: rule92 },
    { start: 4039, length: 6, convRule: rule13 },
    { start: 4046, length: 2, convRule: rule13 },
    { start: 4048, length: 5, convRule: rule2 },
    { start: 4053, length: 4, convRule: rule13 },
    { start: 4057, length: 2, convRule: rule2 },
    { start: 4096, length: 43, convRule: rule14 },
    { start: 4139, length: 2, convRule: rule124 },
    { start: 4141, length: 4, convRule: rule92 },
    { start: 4145, length: 1, convRule: rule124 },
    { start: 4146, length: 6, convRule: rule92 },
    { start: 4152, length: 1, convRule: rule124 },
    { start: 4153, length: 2, convRule: rule92 },
    { start: 4155, length: 2, convRule: rule124 },
    { start: 4157, length: 2, convRule: rule92 },
    { start: 4159, length: 1, convRule: rule14 },
    { start: 4160, length: 10, convRule: rule8 },
    { start: 4170, length: 6, convRule: rule2 },
    { start: 4176, length: 6, convRule: rule14 },
    { start: 4182, length: 2, convRule: rule124 },
    { start: 4184, length: 2, convRule: rule92 },
    { start: 4186, length: 4, convRule: rule14 },
    { start: 4190, length: 3, convRule: rule92 },
    { start: 4193, length: 1, convRule: rule14 },
    { start: 4194, length: 3, convRule: rule124 },
    { start: 4197, length: 2, convRule: rule14 },
    { start: 4199, length: 7, convRule: rule124 },
    { start: 4206, length: 3, convRule: rule14 },
    { start: 4209, length: 4, convRule: rule92 },
    { start: 4213, length: 13, convRule: rule14 },
    { start: 4226, length: 1, convRule: rule92 },
    { start: 4227, length: 2, convRule: rule124 },
    { start: 4229, length: 2, convRule: rule92 },
    { start: 4231, length: 6, convRule: rule124 },
    { start: 4237, length: 1, convRule: rule92 },
    { start: 4238, length: 1, convRule: rule14 },
    { start: 4239, length: 1, convRule: rule124 },
    { start: 4240, length: 10, convRule: rule8 },
    { start: 4250, length: 3, convRule: rule124 },
    { start: 4253, length: 1, convRule: rule92 },
    { start: 4254, length: 2, convRule: rule13 },
    { start: 4256, length: 38, convRule: rule125 },
    { start: 4295, length: 1, convRule: rule125 },
    { start: 4301, length: 1, convRule: rule125 },
    { start: 4304, length: 43, convRule: rule126 },
    { start: 4347, length: 1, convRule: rule2 },
    { start: 4348, length: 1, convRule: rule91 },
    { start: 4349, length: 3, convRule: rule126 },
    { start: 4352, length: 329, convRule: rule14 },
    { start: 4682, length: 4, convRule: rule14 },
    { start: 4688, length: 7, convRule: rule14 },
    { start: 4696, length: 1, convRule: rule14 },
    { start: 4698, length: 4, convRule: rule14 },
    { start: 4704, length: 41, convRule: rule14 },
    { start: 4746, length: 4, convRule: rule14 },
    { start: 4752, length: 33, convRule: rule14 },
    { start: 4786, length: 4, convRule: rule14 },
    { start: 4792, length: 7, convRule: rule14 },
    { start: 4800, length: 1, convRule: rule14 },
    { start: 4802, length: 4, convRule: rule14 },
    { start: 4808, length: 15, convRule: rule14 },
    { start: 4824, length: 57, convRule: rule14 },
    { start: 4882, length: 4, convRule: rule14 },
    { start: 4888, length: 67, convRule: rule14 },
    { start: 4957, length: 3, convRule: rule92 },
    { start: 4960, length: 9, convRule: rule2 },
    { start: 4969, length: 20, convRule: rule17 },
    { start: 4992, length: 16, convRule: rule14 },
    { start: 5008, length: 10, convRule: rule13 },
    { start: 5024, length: 80, convRule: rule127 },
    { start: 5104, length: 6, convRule: rule104 },
    { start: 5112, length: 6, convRule: rule110 },
    { start: 5120, length: 1, convRule: rule7 },
    { start: 5121, length: 620, convRule: rule14 },
    { start: 5741, length: 1, convRule: rule13 },
    { start: 5742, length: 1, convRule: rule2 },
    { start: 5743, length: 17, convRule: rule14 },
    { start: 5760, length: 1, convRule: rule1 },
    { start: 5761, length: 26, convRule: rule14 },
    { start: 5787, length: 1, convRule: rule4 },
    { start: 5788, length: 1, convRule: rule5 },
    { start: 5792, length: 75, convRule: rule14 },
    { start: 5867, length: 3, convRule: rule2 },
    { start: 5870, length: 3, convRule: rule128 },
    { start: 5873, length: 8, convRule: rule14 },
    { start: 5888, length: 13, convRule: rule14 },
    { start: 5902, length: 4, convRule: rule14 },
    { start: 5906, length: 3, convRule: rule92 },
    { start: 5920, length: 18, convRule: rule14 },
    { start: 5938, length: 3, convRule: rule92 },
    { start: 5941, length: 2, convRule: rule2 },
    { start: 5952, length: 18, convRule: rule14 },
    { start: 5970, length: 2, convRule: rule92 },
    { start: 5984, length: 13, convRule: rule14 },
    { start: 5998, length: 3, convRule: rule14 },
    { start: 6002, length: 2, convRule: rule92 },
    { start: 6016, length: 52, convRule: rule14 },
    { start: 6068, length: 2, convRule: rule92 },
    { start: 6070, length: 1, convRule: rule124 },
    { start: 6071, length: 7, convRule: rule92 },
    { start: 6078, length: 8, convRule: rule124 },
    { start: 6086, length: 1, convRule: rule92 },
    { start: 6087, length: 2, convRule: rule124 },
    { start: 6089, length: 11, convRule: rule92 },
    { start: 6100, length: 3, convRule: rule2 },
    { start: 6103, length: 1, convRule: rule91 },
    { start: 6104, length: 3, convRule: rule2 },
    { start: 6107, length: 1, convRule: rule3 },
    { start: 6108, length: 1, convRule: rule14 },
    { start: 6109, length: 1, convRule: rule92 },
    { start: 6112, length: 10, convRule: rule8 },
    { start: 6128, length: 10, convRule: rule17 },
    { start: 6144, length: 6, convRule: rule2 },
    { start: 6150, length: 1, convRule: rule7 },
    { start: 6151, length: 4, convRule: rule2 },
    { start: 6155, length: 3, convRule: rule92 },
    { start: 6158, length: 1, convRule: rule16 },
    { start: 6160, length: 10, convRule: rule8 },
    { start: 6176, length: 35, convRule: rule14 },
    { start: 6211, length: 1, convRule: rule91 },
    { start: 6212, length: 53, convRule: rule14 },
    { start: 6272, length: 5, convRule: rule14 },
    { start: 6277, length: 2, convRule: rule92 },
    { start: 6279, length: 34, convRule: rule14 },
    { start: 6313, length: 1, convRule: rule92 },
    { start: 6314, length: 1, convRule: rule14 },
    { start: 6320, length: 70, convRule: rule14 },
    { start: 6400, length: 31, convRule: rule14 },
    { start: 6432, length: 3, convRule: rule92 },
    { start: 6435, length: 4, convRule: rule124 },
    { start: 6439, length: 2, convRule: rule92 },
    { start: 6441, length: 3, convRule: rule124 },
    { start: 6448, length: 2, convRule: rule124 },
    { start: 6450, length: 1, convRule: rule92 },
    { start: 6451, length: 6, convRule: rule124 },
    { start: 6457, length: 3, convRule: rule92 },
    { start: 6464, length: 1, convRule: rule13 },
    { start: 6468, length: 2, convRule: rule2 },
    { start: 6470, length: 10, convRule: rule8 },
    { start: 6480, length: 30, convRule: rule14 },
    { start: 6512, length: 5, convRule: rule14 },
    { start: 6528, length: 44, convRule: rule14 },
    { start: 6576, length: 26, convRule: rule14 },
    { start: 6608, length: 10, convRule: rule8 },
    { start: 6618, length: 1, convRule: rule17 },
    { start: 6622, length: 34, convRule: rule13 },
    { start: 6656, length: 23, convRule: rule14 },
    { start: 6679, length: 2, convRule: rule92 },
    { start: 6681, length: 2, convRule: rule124 },
    { start: 6683, length: 1, convRule: rule92 },
    { start: 6686, length: 2, convRule: rule2 },
    { start: 6688, length: 53, convRule: rule14 },
    { start: 6741, length: 1, convRule: rule124 },
    { start: 6742, length: 1, convRule: rule92 },
    { start: 6743, length: 1, convRule: rule124 },
    { start: 6744, length: 7, convRule: rule92 },
    { start: 6752, length: 1, convRule: rule92 },
    { start: 6753, length: 1, convRule: rule124 },
    { start: 6754, length: 1, convRule: rule92 },
    { start: 6755, length: 2, convRule: rule124 },
    { start: 6757, length: 8, convRule: rule92 },
    { start: 6765, length: 6, convRule: rule124 },
    { start: 6771, length: 10, convRule: rule92 },
    { start: 6783, length: 1, convRule: rule92 },
    { start: 6784, length: 10, convRule: rule8 },
    { start: 6800, length: 10, convRule: rule8 },
    { start: 6816, length: 7, convRule: rule2 },
    { start: 6823, length: 1, convRule: rule91 },
    { start: 6824, length: 6, convRule: rule2 },
    { start: 6832, length: 14, convRule: rule92 },
    { start: 6846, length: 1, convRule: rule119 },
    { start: 6847, length: 2, convRule: rule92 },
    { start: 6912, length: 4, convRule: rule92 },
    { start: 6916, length: 1, convRule: rule124 },
    { start: 6917, length: 47, convRule: rule14 },
    { start: 6964, length: 1, convRule: rule92 },
    { start: 6965, length: 1, convRule: rule124 },
    { start: 6966, length: 5, convRule: rule92 },
    { start: 6971, length: 1, convRule: rule124 },
    { start: 6972, length: 1, convRule: rule92 },
    { start: 6973, length: 5, convRule: rule124 },
    { start: 6978, length: 1, convRule: rule92 },
    { start: 6979, length: 2, convRule: rule124 },
    { start: 6981, length: 7, convRule: rule14 },
    { start: 6992, length: 10, convRule: rule8 },
    { start: 7002, length: 7, convRule: rule2 },
    { start: 7009, length: 10, convRule: rule13 },
    { start: 7019, length: 9, convRule: rule92 },
    { start: 7028, length: 9, convRule: rule13 },
    { start: 7040, length: 2, convRule: rule92 },
    { start: 7042, length: 1, convRule: rule124 },
    { start: 7043, length: 30, convRule: rule14 },
    { start: 7073, length: 1, convRule: rule124 },
    { start: 7074, length: 4, convRule: rule92 },
    { start: 7078, length: 2, convRule: rule124 },
    { start: 7080, length: 2, convRule: rule92 },
    { start: 7082, length: 1, convRule: rule124 },
    { start: 7083, length: 3, convRule: rule92 },
    { start: 7086, length: 2, convRule: rule14 },
    { start: 7088, length: 10, convRule: rule8 },
    { start: 7098, length: 44, convRule: rule14 },
    { start: 7142, length: 1, convRule: rule92 },
    { start: 7143, length: 1, convRule: rule124 },
    { start: 7144, length: 2, convRule: rule92 },
    { start: 7146, length: 3, convRule: rule124 },
    { start: 7149, length: 1, convRule: rule92 },
    { start: 7150, length: 1, convRule: rule124 },
    { start: 7151, length: 3, convRule: rule92 },
    { start: 7154, length: 2, convRule: rule124 },
    { start: 7164, length: 4, convRule: rule2 },
    { start: 7168, length: 36, convRule: rule14 },
    { start: 7204, length: 8, convRule: rule124 },
    { start: 7212, length: 8, convRule: rule92 },
    { start: 7220, length: 2, convRule: rule124 },
    { start: 7222, length: 2, convRule: rule92 },
    { start: 7227, length: 5, convRule: rule2 },
    { start: 7232, length: 10, convRule: rule8 },
    { start: 7245, length: 3, convRule: rule14 },
    { start: 7248, length: 10, convRule: rule8 },
    { start: 7258, length: 30, convRule: rule14 },
    { start: 7288, length: 6, convRule: rule91 },
    { start: 7294, length: 2, convRule: rule2 },
    { start: 7296, length: 1, convRule: rule129 },
    { start: 7297, length: 1, convRule: rule130 },
    { start: 7298, length: 1, convRule: rule131 },
    { start: 7299, length: 2, convRule: rule132 },
    { start: 7301, length: 1, convRule: rule133 },
    { start: 7302, length: 1, convRule: rule134 },
    { start: 7303, length: 1, convRule: rule135 },
    { start: 7304, length: 1, convRule: rule136 },
    { start: 7312, length: 43, convRule: rule137 },
    { start: 7357, length: 3, convRule: rule137 },
    { start: 7360, length: 8, convRule: rule2 },
    { start: 7376, length: 3, convRule: rule92 },
    { start: 7379, length: 1, convRule: rule2 },
    { start: 7380, length: 13, convRule: rule92 },
    { start: 7393, length: 1, convRule: rule124 },
    { start: 7394, length: 7, convRule: rule92 },
    { start: 7401, length: 4, convRule: rule14 },
    { start: 7405, length: 1, convRule: rule92 },
    { start: 7406, length: 6, convRule: rule14 },
    { start: 7412, length: 1, convRule: rule92 },
    { start: 7413, length: 2, convRule: rule14 },
    { start: 7415, length: 1, convRule: rule124 },
    { start: 7416, length: 2, convRule: rule92 },
    { start: 7418, length: 1, convRule: rule14 },
    { start: 7424, length: 44, convRule: rule20 },
    { start: 7468, length: 63, convRule: rule91 },
    { start: 7531, length: 13, convRule: rule20 },
    { start: 7544, length: 1, convRule: rule91 },
    { start: 7545, length: 1, convRule: rule138 },
    { start: 7546, length: 3, convRule: rule20 },
    { start: 7549, length: 1, convRule: rule139 },
    { start: 7550, length: 16, convRule: rule20 },
    { start: 7566, length: 1, convRule: rule140 },
    { start: 7567, length: 12, convRule: rule20 },
    { start: 7579, length: 37, convRule: rule91 },
    { start: 7616, length: 58, convRule: rule92 },
    { start: 7675, length: 5, convRule: rule92 },
    { start: 7680, length: 1, convRule: rule22 },
    { start: 7681, length: 1, convRule: rule23 },
    { start: 7682, length: 1, convRule: rule22 },
    { start: 7683, length: 1, convRule: rule23 },
    { start: 7684, length: 1, convRule: rule22 },
    { start: 7685, length: 1, convRule: rule23 },
    { start: 7686, length: 1, convRule: rule22 },
    { start: 7687, length: 1, convRule: rule23 },
    { start: 7688, length: 1, convRule: rule22 },
    { start: 7689, length: 1, convRule: rule23 },
    { start: 7690, length: 1, convRule: rule22 },
    { start: 7691, length: 1, convRule: rule23 },
    { start: 7692, length: 1, convRule: rule22 },
    { start: 7693, length: 1, convRule: rule23 },
    { start: 7694, length: 1, convRule: rule22 },
    { start: 7695, length: 1, convRule: rule23 },
    { start: 7696, length: 1, convRule: rule22 },
    { start: 7697, length: 1, convRule: rule23 },
    { start: 7698, length: 1, convRule: rule22 },
    { start: 7699, length: 1, convRule: rule23 },
    { start: 7700, length: 1, convRule: rule22 },
    { start: 7701, length: 1, convRule: rule23 },
    { start: 7702, length: 1, convRule: rule22 },
    { start: 7703, length: 1, convRule: rule23 },
    { start: 7704, length: 1, convRule: rule22 },
    { start: 7705, length: 1, convRule: rule23 },
    { start: 7706, length: 1, convRule: rule22 },
    { start: 7707, length: 1, convRule: rule23 },
    { start: 7708, length: 1, convRule: rule22 },
    { start: 7709, length: 1, convRule: rule23 },
    { start: 7710, length: 1, convRule: rule22 },
    { start: 7711, length: 1, convRule: rule23 },
    { start: 7712, length: 1, convRule: rule22 },
    { start: 7713, length: 1, convRule: rule23 },
    { start: 7714, length: 1, convRule: rule22 },
    { start: 7715, length: 1, convRule: rule23 },
    { start: 7716, length: 1, convRule: rule22 },
    { start: 7717, length: 1, convRule: rule23 },
    { start: 7718, length: 1, convRule: rule22 },
    { start: 7719, length: 1, convRule: rule23 },
    { start: 7720, length: 1, convRule: rule22 },
    { start: 7721, length: 1, convRule: rule23 },
    { start: 7722, length: 1, convRule: rule22 },
    { start: 7723, length: 1, convRule: rule23 },
    { start: 7724, length: 1, convRule: rule22 },
    { start: 7725, length: 1, convRule: rule23 },
    { start: 7726, length: 1, convRule: rule22 },
    { start: 7727, length: 1, convRule: rule23 },
    { start: 7728, length: 1, convRule: rule22 },
    { start: 7729, length: 1, convRule: rule23 },
    { start: 7730, length: 1, convRule: rule22 },
    { start: 7731, length: 1, convRule: rule23 },
    { start: 7732, length: 1, convRule: rule22 },
    { start: 7733, length: 1, convRule: rule23 },
    { start: 7734, length: 1, convRule: rule22 },
    { start: 7735, length: 1, convRule: rule23 },
    { start: 7736, length: 1, convRule: rule22 },
    { start: 7737, length: 1, convRule: rule23 },
    { start: 7738, length: 1, convRule: rule22 },
    { start: 7739, length: 1, convRule: rule23 },
    { start: 7740, length: 1, convRule: rule22 },
    { start: 7741, length: 1, convRule: rule23 },
    { start: 7742, length: 1, convRule: rule22 },
    { start: 7743, length: 1, convRule: rule23 },
    { start: 7744, length: 1, convRule: rule22 },
    { start: 7745, length: 1, convRule: rule23 },
    { start: 7746, length: 1, convRule: rule22 },
    { start: 7747, length: 1, convRule: rule23 },
    { start: 7748, length: 1, convRule: rule22 },
    { start: 7749, length: 1, convRule: rule23 },
    { start: 7750, length: 1, convRule: rule22 },
    { start: 7751, length: 1, convRule: rule23 },
    { start: 7752, length: 1, convRule: rule22 },
    { start: 7753, length: 1, convRule: rule23 },
    { start: 7754, length: 1, convRule: rule22 },
    { start: 7755, length: 1, convRule: rule23 },
    { start: 7756, length: 1, convRule: rule22 },
    { start: 7757, length: 1, convRule: rule23 },
    { start: 7758, length: 1, convRule: rule22 },
    { start: 7759, length: 1, convRule: rule23 },
    { start: 7760, length: 1, convRule: rule22 },
    { start: 7761, length: 1, convRule: rule23 },
    { start: 7762, length: 1, convRule: rule22 },
    { start: 7763, length: 1, convRule: rule23 },
    { start: 7764, length: 1, convRule: rule22 },
    { start: 7765, length: 1, convRule: rule23 },
    { start: 7766, length: 1, convRule: rule22 },
    { start: 7767, length: 1, convRule: rule23 },
    { start: 7768, length: 1, convRule: rule22 },
    { start: 7769, length: 1, convRule: rule23 },
    { start: 7770, length: 1, convRule: rule22 },
    { start: 7771, length: 1, convRule: rule23 },
    { start: 7772, length: 1, convRule: rule22 },
    { start: 7773, length: 1, convRule: rule23 },
    { start: 7774, length: 1, convRule: rule22 },
    { start: 7775, length: 1, convRule: rule23 },
    { start: 7776, length: 1, convRule: rule22 },
    { start: 7777, length: 1, convRule: rule23 },
    { start: 7778, length: 1, convRule: rule22 },
    { start: 7779, length: 1, convRule: rule23 },
    { start: 7780, length: 1, convRule: rule22 },
    { start: 7781, length: 1, convRule: rule23 },
    { start: 7782, length: 1, convRule: rule22 },
    { start: 7783, length: 1, convRule: rule23 },
    { start: 7784, length: 1, convRule: rule22 },
    { start: 7785, length: 1, convRule: rule23 },
    { start: 7786, length: 1, convRule: rule22 },
    { start: 7787, length: 1, convRule: rule23 },
    { start: 7788, length: 1, convRule: rule22 },
    { start: 7789, length: 1, convRule: rule23 },
    { start: 7790, length: 1, convRule: rule22 },
    { start: 7791, length: 1, convRule: rule23 },
    { start: 7792, length: 1, convRule: rule22 },
    { start: 7793, length: 1, convRule: rule23 },
    { start: 7794, length: 1, convRule: rule22 },
    { start: 7795, length: 1, convRule: rule23 },
    { start: 7796, length: 1, convRule: rule22 },
    { start: 7797, length: 1, convRule: rule23 },
    { start: 7798, length: 1, convRule: rule22 },
    { start: 7799, length: 1, convRule: rule23 },
    { start: 7800, length: 1, convRule: rule22 },
    { start: 7801, length: 1, convRule: rule23 },
    { start: 7802, length: 1, convRule: rule22 },
    { start: 7803, length: 1, convRule: rule23 },
    { start: 7804, length: 1, convRule: rule22 },
    { start: 7805, length: 1, convRule: rule23 },
    { start: 7806, length: 1, convRule: rule22 },
    { start: 7807, length: 1, convRule: rule23 },
    { start: 7808, length: 1, convRule: rule22 },
    { start: 7809, length: 1, convRule: rule23 },
    { start: 7810, length: 1, convRule: rule22 },
    { start: 7811, length: 1, convRule: rule23 },
    { start: 7812, length: 1, convRule: rule22 },
    { start: 7813, length: 1, convRule: rule23 },
    { start: 7814, length: 1, convRule: rule22 },
    { start: 7815, length: 1, convRule: rule23 },
    { start: 7816, length: 1, convRule: rule22 },
    { start: 7817, length: 1, convRule: rule23 },
    { start: 7818, length: 1, convRule: rule22 },
    { start: 7819, length: 1, convRule: rule23 },
    { start: 7820, length: 1, convRule: rule22 },
    { start: 7821, length: 1, convRule: rule23 },
    { start: 7822, length: 1, convRule: rule22 },
    { start: 7823, length: 1, convRule: rule23 },
    { start: 7824, length: 1, convRule: rule22 },
    { start: 7825, length: 1, convRule: rule23 },
    { start: 7826, length: 1, convRule: rule22 },
    { start: 7827, length: 1, convRule: rule23 },
    { start: 7828, length: 1, convRule: rule22 },
    { start: 7829, length: 1, convRule: rule23 },
    { start: 7830, length: 5, convRule: rule20 },
    { start: 7835, length: 1, convRule: rule141 },
    { start: 7836, length: 2, convRule: rule20 },
    { start: 7838, length: 1, convRule: rule142 },
    { start: 7839, length: 1, convRule: rule20 },
    { start: 7840, length: 1, convRule: rule22 },
    { start: 7841, length: 1, convRule: rule23 },
    { start: 7842, length: 1, convRule: rule22 },
    { start: 7843, length: 1, convRule: rule23 },
    { start: 7844, length: 1, convRule: rule22 },
    { start: 7845, length: 1, convRule: rule23 },
    { start: 7846, length: 1, convRule: rule22 },
    { start: 7847, length: 1, convRule: rule23 },
    { start: 7848, length: 1, convRule: rule22 },
    { start: 7849, length: 1, convRule: rule23 },
    { start: 7850, length: 1, convRule: rule22 },
    { start: 7851, length: 1, convRule: rule23 },
    { start: 7852, length: 1, convRule: rule22 },
    { start: 7853, length: 1, convRule: rule23 },
    { start: 7854, length: 1, convRule: rule22 },
    { start: 7855, length: 1, convRule: rule23 },
    { start: 7856, length: 1, convRule: rule22 },
    { start: 7857, length: 1, convRule: rule23 },
    { start: 7858, length: 1, convRule: rule22 },
    { start: 7859, length: 1, convRule: rule23 },
    { start: 7860, length: 1, convRule: rule22 },
    { start: 7861, length: 1, convRule: rule23 },
    { start: 7862, length: 1, convRule: rule22 },
    { start: 7863, length: 1, convRule: rule23 },
    { start: 7864, length: 1, convRule: rule22 },
    { start: 7865, length: 1, convRule: rule23 },
    { start: 7866, length: 1, convRule: rule22 },
    { start: 7867, length: 1, convRule: rule23 },
    { start: 7868, length: 1, convRule: rule22 },
    { start: 7869, length: 1, convRule: rule23 },
    { start: 7870, length: 1, convRule: rule22 },
    { start: 7871, length: 1, convRule: rule23 },
    { start: 7872, length: 1, convRule: rule22 },
    { start: 7873, length: 1, convRule: rule23 },
    { start: 7874, length: 1, convRule: rule22 },
    { start: 7875, length: 1, convRule: rule23 },
    { start: 7876, length: 1, convRule: rule22 },
    { start: 7877, length: 1, convRule: rule23 },
    { start: 7878, length: 1, convRule: rule22 },
    { start: 7879, length: 1, convRule: rule23 },
    { start: 7880, length: 1, convRule: rule22 },
    { start: 7881, length: 1, convRule: rule23 },
    { start: 7882, length: 1, convRule: rule22 },
    { start: 7883, length: 1, convRule: rule23 },
    { start: 7884, length: 1, convRule: rule22 },
    { start: 7885, length: 1, convRule: rule23 },
    { start: 7886, length: 1, convRule: rule22 },
    { start: 7887, length: 1, convRule: rule23 },
    { start: 7888, length: 1, convRule: rule22 },
    { start: 7889, length: 1, convRule: rule23 },
    { start: 7890, length: 1, convRule: rule22 },
    { start: 7891, length: 1, convRule: rule23 },
    { start: 7892, length: 1, convRule: rule22 },
    { start: 7893, length: 1, convRule: rule23 },
    { start: 7894, length: 1, convRule: rule22 },
    { start: 7895, length: 1, convRule: rule23 },
    { start: 7896, length: 1, convRule: rule22 },
    { start: 7897, length: 1, convRule: rule23 },
    { start: 7898, length: 1, convRule: rule22 },
    { start: 7899, length: 1, convRule: rule23 },
    { start: 7900, length: 1, convRule: rule22 },
    { start: 7901, length: 1, convRule: rule23 },
    { start: 7902, length: 1, convRule: rule22 },
    { start: 7903, length: 1, convRule: rule23 },
    { start: 7904, length: 1, convRule: rule22 },
    { start: 7905, length: 1, convRule: rule23 },
    { start: 7906, length: 1, convRule: rule22 },
    { start: 7907, length: 1, convRule: rule23 },
    { start: 7908, length: 1, convRule: rule22 },
    { start: 7909, length: 1, convRule: rule23 },
    { start: 7910, length: 1, convRule: rule22 },
    { start: 7911, length: 1, convRule: rule23 },
    { start: 7912, length: 1, convRule: rule22 },
    { start: 7913, length: 1, convRule: rule23 },
    { start: 7914, length: 1, convRule: rule22 },
    { start: 7915, length: 1, convRule: rule23 },
    { start: 7916, length: 1, convRule: rule22 },
    { start: 7917, length: 1, convRule: rule23 },
    { start: 7918, length: 1, convRule: rule22 },
    { start: 7919, length: 1, convRule: rule23 },
    { start: 7920, length: 1, convRule: rule22 },
    { start: 7921, length: 1, convRule: rule23 },
    { start: 7922, length: 1, convRule: rule22 },
    { start: 7923, length: 1, convRule: rule23 },
    { start: 7924, length: 1, convRule: rule22 },
    { start: 7925, length: 1, convRule: rule23 },
    { start: 7926, length: 1, convRule: rule22 },
    { start: 7927, length: 1, convRule: rule23 },
    { start: 7928, length: 1, convRule: rule22 },
    { start: 7929, length: 1, convRule: rule23 },
    { start: 7930, length: 1, convRule: rule22 },
    { start: 7931, length: 1, convRule: rule23 },
    { start: 7932, length: 1, convRule: rule22 },
    { start: 7933, length: 1, convRule: rule23 },
    { start: 7934, length: 1, convRule: rule22 },
    { start: 7935, length: 1, convRule: rule23 },
    { start: 7936, length: 8, convRule: rule143 },
    { start: 7944, length: 8, convRule: rule144 },
    { start: 7952, length: 6, convRule: rule143 },
    { start: 7960, length: 6, convRule: rule144 },
    { start: 7968, length: 8, convRule: rule143 },
    { start: 7976, length: 8, convRule: rule144 },
    { start: 7984, length: 8, convRule: rule143 },
    { start: 7992, length: 8, convRule: rule144 },
    { start: 8e3, length: 6, convRule: rule143 },
    { start: 8008, length: 6, convRule: rule144 },
    { start: 8016, length: 1, convRule: rule20 },
    { start: 8017, length: 1, convRule: rule143 },
    { start: 8018, length: 1, convRule: rule20 },
    { start: 8019, length: 1, convRule: rule143 },
    { start: 8020, length: 1, convRule: rule20 },
    { start: 8021, length: 1, convRule: rule143 },
    { start: 8022, length: 1, convRule: rule20 },
    { start: 8023, length: 1, convRule: rule143 },
    { start: 8025, length: 1, convRule: rule144 },
    { start: 8027, length: 1, convRule: rule144 },
    { start: 8029, length: 1, convRule: rule144 },
    { start: 8031, length: 1, convRule: rule144 },
    { start: 8032, length: 8, convRule: rule143 },
    { start: 8040, length: 8, convRule: rule144 },
    { start: 8048, length: 2, convRule: rule145 },
    { start: 8050, length: 4, convRule: rule146 },
    { start: 8054, length: 2, convRule: rule147 },
    { start: 8056, length: 2, convRule: rule148 },
    { start: 8058, length: 2, convRule: rule149 },
    { start: 8060, length: 2, convRule: rule150 },
    { start: 8064, length: 8, convRule: rule143 },
    { start: 8072, length: 8, convRule: rule151 },
    { start: 8080, length: 8, convRule: rule143 },
    { start: 8088, length: 8, convRule: rule151 },
    { start: 8096, length: 8, convRule: rule143 },
    { start: 8104, length: 8, convRule: rule151 },
    { start: 8112, length: 2, convRule: rule143 },
    { start: 8114, length: 1, convRule: rule20 },
    { start: 8115, length: 1, convRule: rule152 },
    { start: 8116, length: 1, convRule: rule20 },
    { start: 8118, length: 2, convRule: rule20 },
    { start: 8120, length: 2, convRule: rule144 },
    { start: 8122, length: 2, convRule: rule153 },
    { start: 8124, length: 1, convRule: rule154 },
    { start: 8125, length: 1, convRule: rule10 },
    { start: 8126, length: 1, convRule: rule155 },
    { start: 8127, length: 3, convRule: rule10 },
    { start: 8130, length: 1, convRule: rule20 },
    { start: 8131, length: 1, convRule: rule152 },
    { start: 8132, length: 1, convRule: rule20 },
    { start: 8134, length: 2, convRule: rule20 },
    { start: 8136, length: 4, convRule: rule156 },
    { start: 8140, length: 1, convRule: rule154 },
    { start: 8141, length: 3, convRule: rule10 },
    { start: 8144, length: 2, convRule: rule143 },
    { start: 8146, length: 2, convRule: rule20 },
    { start: 8150, length: 2, convRule: rule20 },
    { start: 8152, length: 2, convRule: rule144 },
    { start: 8154, length: 2, convRule: rule157 },
    { start: 8157, length: 3, convRule: rule10 },
    { start: 8160, length: 2, convRule: rule143 },
    { start: 8162, length: 3, convRule: rule20 },
    { start: 8165, length: 1, convRule: rule113 },
    { start: 8166, length: 2, convRule: rule20 },
    { start: 8168, length: 2, convRule: rule144 },
    { start: 8170, length: 2, convRule: rule158 },
    { start: 8172, length: 1, convRule: rule117 },
    { start: 8173, length: 3, convRule: rule10 },
    { start: 8178, length: 1, convRule: rule20 },
    { start: 8179, length: 1, convRule: rule152 },
    { start: 8180, length: 1, convRule: rule20 },
    { start: 8182, length: 2, convRule: rule20 },
    { start: 8184, length: 2, convRule: rule159 },
    { start: 8186, length: 2, convRule: rule160 },
    { start: 8188, length: 1, convRule: rule154 },
    { start: 8189, length: 2, convRule: rule10 },
    { start: 8192, length: 11, convRule: rule1 },
    { start: 8203, length: 5, convRule: rule16 },
    { start: 8208, length: 6, convRule: rule7 },
    { start: 8214, length: 2, convRule: rule2 },
    { start: 8216, length: 1, convRule: rule15 },
    { start: 8217, length: 1, convRule: rule19 },
    { start: 8218, length: 1, convRule: rule4 },
    { start: 8219, length: 2, convRule: rule15 },
    { start: 8221, length: 1, convRule: rule19 },
    { start: 8222, length: 1, convRule: rule4 },
    { start: 8223, length: 1, convRule: rule15 },
    { start: 8224, length: 8, convRule: rule2 },
    { start: 8232, length: 1, convRule: rule161 },
    { start: 8233, length: 1, convRule: rule162 },
    { start: 8234, length: 5, convRule: rule16 },
    { start: 8239, length: 1, convRule: rule1 },
    { start: 8240, length: 9, convRule: rule2 },
    { start: 8249, length: 1, convRule: rule15 },
    { start: 8250, length: 1, convRule: rule19 },
    { start: 8251, length: 4, convRule: rule2 },
    { start: 8255, length: 2, convRule: rule11 },
    { start: 8257, length: 3, convRule: rule2 },
    { start: 8260, length: 1, convRule: rule6 },
    { start: 8261, length: 1, convRule: rule4 },
    { start: 8262, length: 1, convRule: rule5 },
    { start: 8263, length: 11, convRule: rule2 },
    { start: 8274, length: 1, convRule: rule6 },
    { start: 8275, length: 1, convRule: rule2 },
    { start: 8276, length: 1, convRule: rule11 },
    { start: 8277, length: 10, convRule: rule2 },
    { start: 8287, length: 1, convRule: rule1 },
    { start: 8288, length: 5, convRule: rule16 },
    { start: 8294, length: 10, convRule: rule16 },
    { start: 8304, length: 1, convRule: rule17 },
    { start: 8305, length: 1, convRule: rule91 },
    { start: 8308, length: 6, convRule: rule17 },
    { start: 8314, length: 3, convRule: rule6 },
    { start: 8317, length: 1, convRule: rule4 },
    { start: 8318, length: 1, convRule: rule5 },
    { start: 8319, length: 1, convRule: rule91 },
    { start: 8320, length: 10, convRule: rule17 },
    { start: 8330, length: 3, convRule: rule6 },
    { start: 8333, length: 1, convRule: rule4 },
    { start: 8334, length: 1, convRule: rule5 },
    { start: 8336, length: 13, convRule: rule91 },
    { start: 8352, length: 32, convRule: rule3 },
    { start: 8400, length: 13, convRule: rule92 },
    { start: 8413, length: 4, convRule: rule119 },
    { start: 8417, length: 1, convRule: rule92 },
    { start: 8418, length: 3, convRule: rule119 },
    { start: 8421, length: 12, convRule: rule92 },
    { start: 8448, length: 2, convRule: rule13 },
    { start: 8450, length: 1, convRule: rule107 },
    { start: 8451, length: 4, convRule: rule13 },
    { start: 8455, length: 1, convRule: rule107 },
    { start: 8456, length: 2, convRule: rule13 },
    { start: 8458, length: 1, convRule: rule20 },
    { start: 8459, length: 3, convRule: rule107 },
    { start: 8462, length: 2, convRule: rule20 },
    { start: 8464, length: 3, convRule: rule107 },
    { start: 8467, length: 1, convRule: rule20 },
    { start: 8468, length: 1, convRule: rule13 },
    { start: 8469, length: 1, convRule: rule107 },
    { start: 8470, length: 2, convRule: rule13 },
    { start: 8472, length: 1, convRule: rule6 },
    { start: 8473, length: 5, convRule: rule107 },
    { start: 8478, length: 6, convRule: rule13 },
    { start: 8484, length: 1, convRule: rule107 },
    { start: 8485, length: 1, convRule: rule13 },
    { start: 8486, length: 1, convRule: rule163 },
    { start: 8487, length: 1, convRule: rule13 },
    { start: 8488, length: 1, convRule: rule107 },
    { start: 8489, length: 1, convRule: rule13 },
    { start: 8490, length: 1, convRule: rule164 },
    { start: 8491, length: 1, convRule: rule165 },
    { start: 8492, length: 2, convRule: rule107 },
    { start: 8494, length: 1, convRule: rule13 },
    { start: 8495, length: 1, convRule: rule20 },
    { start: 8496, length: 2, convRule: rule107 },
    { start: 8498, length: 1, convRule: rule166 },
    { start: 8499, length: 1, convRule: rule107 },
    { start: 8500, length: 1, convRule: rule20 },
    { start: 8501, length: 4, convRule: rule14 },
    { start: 8505, length: 1, convRule: rule20 },
    { start: 8506, length: 2, convRule: rule13 },
    { start: 8508, length: 2, convRule: rule20 },
    { start: 8510, length: 2, convRule: rule107 },
    { start: 8512, length: 5, convRule: rule6 },
    { start: 8517, length: 1, convRule: rule107 },
    { start: 8518, length: 4, convRule: rule20 },
    { start: 8522, length: 1, convRule: rule13 },
    { start: 8523, length: 1, convRule: rule6 },
    { start: 8524, length: 2, convRule: rule13 },
    { start: 8526, length: 1, convRule: rule167 },
    { start: 8527, length: 1, convRule: rule13 },
    { start: 8528, length: 16, convRule: rule17 },
    { start: 8544, length: 16, convRule: rule168 },
    { start: 8560, length: 16, convRule: rule169 },
    { start: 8576, length: 3, convRule: rule128 },
    { start: 8579, length: 1, convRule: rule22 },
    { start: 8580, length: 1, convRule: rule23 },
    { start: 8581, length: 4, convRule: rule128 },
    { start: 8585, length: 1, convRule: rule17 },
    { start: 8586, length: 2, convRule: rule13 },
    { start: 8592, length: 5, convRule: rule6 },
    { start: 8597, length: 5, convRule: rule13 },
    { start: 8602, length: 2, convRule: rule6 },
    { start: 8604, length: 4, convRule: rule13 },
    { start: 8608, length: 1, convRule: rule6 },
    { start: 8609, length: 2, convRule: rule13 },
    { start: 8611, length: 1, convRule: rule6 },
    { start: 8612, length: 2, convRule: rule13 },
    { start: 8614, length: 1, convRule: rule6 },
    { start: 8615, length: 7, convRule: rule13 },
    { start: 8622, length: 1, convRule: rule6 },
    { start: 8623, length: 31, convRule: rule13 },
    { start: 8654, length: 2, convRule: rule6 },
    { start: 8656, length: 2, convRule: rule13 },
    { start: 8658, length: 1, convRule: rule6 },
    { start: 8659, length: 1, convRule: rule13 },
    { start: 8660, length: 1, convRule: rule6 },
    { start: 8661, length: 31, convRule: rule13 },
    { start: 8692, length: 268, convRule: rule6 },
    { start: 8960, length: 8, convRule: rule13 },
    { start: 8968, length: 1, convRule: rule4 },
    { start: 8969, length: 1, convRule: rule5 },
    { start: 8970, length: 1, convRule: rule4 },
    { start: 8971, length: 1, convRule: rule5 },
    { start: 8972, length: 20, convRule: rule13 },
    { start: 8992, length: 2, convRule: rule6 },
    { start: 8994, length: 7, convRule: rule13 },
    { start: 9001, length: 1, convRule: rule4 },
    { start: 9002, length: 1, convRule: rule5 },
    { start: 9003, length: 81, convRule: rule13 },
    { start: 9084, length: 1, convRule: rule6 },
    { start: 9085, length: 30, convRule: rule13 },
    { start: 9115, length: 25, convRule: rule6 },
    { start: 9140, length: 40, convRule: rule13 },
    { start: 9180, length: 6, convRule: rule6 },
    { start: 9186, length: 69, convRule: rule13 },
    { start: 9280, length: 11, convRule: rule13 },
    { start: 9312, length: 60, convRule: rule17 },
    { start: 9372, length: 26, convRule: rule13 },
    { start: 9398, length: 26, convRule: rule170 },
    { start: 9424, length: 26, convRule: rule171 },
    { start: 9450, length: 22, convRule: rule17 },
    { start: 9472, length: 183, convRule: rule13 },
    { start: 9655, length: 1, convRule: rule6 },
    { start: 9656, length: 9, convRule: rule13 },
    { start: 9665, length: 1, convRule: rule6 },
    { start: 9666, length: 54, convRule: rule13 },
    { start: 9720, length: 8, convRule: rule6 },
    { start: 9728, length: 111, convRule: rule13 },
    { start: 9839, length: 1, convRule: rule6 },
    { start: 9840, length: 248, convRule: rule13 },
    { start: 10088, length: 1, convRule: rule4 },
    { start: 10089, length: 1, convRule: rule5 },
    { start: 10090, length: 1, convRule: rule4 },
    { start: 10091, length: 1, convRule: rule5 },
    { start: 10092, length: 1, convRule: rule4 },
    { start: 10093, length: 1, convRule: rule5 },
    { start: 10094, length: 1, convRule: rule4 },
    { start: 10095, length: 1, convRule: rule5 },
    { start: 10096, length: 1, convRule: rule4 },
    { start: 10097, length: 1, convRule: rule5 },
    { start: 10098, length: 1, convRule: rule4 },
    { start: 10099, length: 1, convRule: rule5 },
    { start: 10100, length: 1, convRule: rule4 },
    { start: 10101, length: 1, convRule: rule5 },
    { start: 10102, length: 30, convRule: rule17 },
    { start: 10132, length: 44, convRule: rule13 },
    { start: 10176, length: 5, convRule: rule6 },
    { start: 10181, length: 1, convRule: rule4 },
    { start: 10182, length: 1, convRule: rule5 },
    { start: 10183, length: 31, convRule: rule6 },
    { start: 10214, length: 1, convRule: rule4 },
    { start: 10215, length: 1, convRule: rule5 },
    { start: 10216, length: 1, convRule: rule4 },
    { start: 10217, length: 1, convRule: rule5 },
    { start: 10218, length: 1, convRule: rule4 },
    { start: 10219, length: 1, convRule: rule5 },
    { start: 10220, length: 1, convRule: rule4 },
    { start: 10221, length: 1, convRule: rule5 },
    { start: 10222, length: 1, convRule: rule4 },
    { start: 10223, length: 1, convRule: rule5 },
    { start: 10224, length: 16, convRule: rule6 },
    { start: 10240, length: 256, convRule: rule13 },
    { start: 10496, length: 131, convRule: rule6 },
    { start: 10627, length: 1, convRule: rule4 },
    { start: 10628, length: 1, convRule: rule5 },
    { start: 10629, length: 1, convRule: rule4 },
    { start: 10630, length: 1, convRule: rule5 },
    { start: 10631, length: 1, convRule: rule4 },
    { start: 10632, length: 1, convRule: rule5 },
    { start: 10633, length: 1, convRule: rule4 },
    { start: 10634, length: 1, convRule: rule5 },
    { start: 10635, length: 1, convRule: rule4 },
    { start: 10636, length: 1, convRule: rule5 },
    { start: 10637, length: 1, convRule: rule4 },
    { start: 10638, length: 1, convRule: rule5 },
    { start: 10639, length: 1, convRule: rule4 },
    { start: 10640, length: 1, convRule: rule5 },
    { start: 10641, length: 1, convRule: rule4 },
    { start: 10642, length: 1, convRule: rule5 },
    { start: 10643, length: 1, convRule: rule4 },
    { start: 10644, length: 1, convRule: rule5 },
    { start: 10645, length: 1, convRule: rule4 },
    { start: 10646, length: 1, convRule: rule5 },
    { start: 10647, length: 1, convRule: rule4 },
    { start: 10648, length: 1, convRule: rule5 },
    { start: 10649, length: 63, convRule: rule6 },
    { start: 10712, length: 1, convRule: rule4 },
    { start: 10713, length: 1, convRule: rule5 },
    { start: 10714, length: 1, convRule: rule4 },
    { start: 10715, length: 1, convRule: rule5 },
    { start: 10716, length: 32, convRule: rule6 },
    { start: 10748, length: 1, convRule: rule4 },
    { start: 10749, length: 1, convRule: rule5 },
    { start: 10750, length: 258, convRule: rule6 },
    { start: 11008, length: 48, convRule: rule13 },
    { start: 11056, length: 21, convRule: rule6 },
    { start: 11077, length: 2, convRule: rule13 },
    { start: 11079, length: 6, convRule: rule6 },
    { start: 11085, length: 39, convRule: rule13 },
    { start: 11126, length: 32, convRule: rule13 },
    { start: 11159, length: 105, convRule: rule13 },
    { start: 11264, length: 47, convRule: rule122 },
    { start: 11312, length: 47, convRule: rule123 },
    { start: 11360, length: 1, convRule: rule22 },
    { start: 11361, length: 1, convRule: rule23 },
    { start: 11362, length: 1, convRule: rule172 },
    { start: 11363, length: 1, convRule: rule173 },
    { start: 11364, length: 1, convRule: rule174 },
    { start: 11365, length: 1, convRule: rule175 },
    { start: 11366, length: 1, convRule: rule176 },
    { start: 11367, length: 1, convRule: rule22 },
    { start: 11368, length: 1, convRule: rule23 },
    { start: 11369, length: 1, convRule: rule22 },
    { start: 11370, length: 1, convRule: rule23 },
    { start: 11371, length: 1, convRule: rule22 },
    { start: 11372, length: 1, convRule: rule23 },
    { start: 11373, length: 1, convRule: rule177 },
    { start: 11374, length: 1, convRule: rule178 },
    { start: 11375, length: 1, convRule: rule179 },
    { start: 11376, length: 1, convRule: rule180 },
    { start: 11377, length: 1, convRule: rule20 },
    { start: 11378, length: 1, convRule: rule22 },
    { start: 11379, length: 1, convRule: rule23 },
    { start: 11380, length: 1, convRule: rule20 },
    { start: 11381, length: 1, convRule: rule22 },
    { start: 11382, length: 1, convRule: rule23 },
    { start: 11383, length: 5, convRule: rule20 },
    { start: 11388, length: 2, convRule: rule91 },
    { start: 11390, length: 2, convRule: rule181 },
    { start: 11392, length: 1, convRule: rule22 },
    { start: 11393, length: 1, convRule: rule23 },
    { start: 11394, length: 1, convRule: rule22 },
    { start: 11395, length: 1, convRule: rule23 },
    { start: 11396, length: 1, convRule: rule22 },
    { start: 11397, length: 1, convRule: rule23 },
    { start: 11398, length: 1, convRule: rule22 },
    { start: 11399, length: 1, convRule: rule23 },
    { start: 11400, length: 1, convRule: rule22 },
    { start: 11401, length: 1, convRule: rule23 },
    { start: 11402, length: 1, convRule: rule22 },
    { start: 11403, length: 1, convRule: rule23 },
    { start: 11404, length: 1, convRule: rule22 },
    { start: 11405, length: 1, convRule: rule23 },
    { start: 11406, length: 1, convRule: rule22 },
    { start: 11407, length: 1, convRule: rule23 },
    { start: 11408, length: 1, convRule: rule22 },
    { start: 11409, length: 1, convRule: rule23 },
    { start: 11410, length: 1, convRule: rule22 },
    { start: 11411, length: 1, convRule: rule23 },
    { start: 11412, length: 1, convRule: rule22 },
    { start: 11413, length: 1, convRule: rule23 },
    { start: 11414, length: 1, convRule: rule22 },
    { start: 11415, length: 1, convRule: rule23 },
    { start: 11416, length: 1, convRule: rule22 },
    { start: 11417, length: 1, convRule: rule23 },
    { start: 11418, length: 1, convRule: rule22 },
    { start: 11419, length: 1, convRule: rule23 },
    { start: 11420, length: 1, convRule: rule22 },
    { start: 11421, length: 1, convRule: rule23 },
    { start: 11422, length: 1, convRule: rule22 },
    { start: 11423, length: 1, convRule: rule23 },
    { start: 11424, length: 1, convRule: rule22 },
    { start: 11425, length: 1, convRule: rule23 },
    { start: 11426, length: 1, convRule: rule22 },
    { start: 11427, length: 1, convRule: rule23 },
    { start: 11428, length: 1, convRule: rule22 },
    { start: 11429, length: 1, convRule: rule23 },
    { start: 11430, length: 1, convRule: rule22 },
    { start: 11431, length: 1, convRule: rule23 },
    { start: 11432, length: 1, convRule: rule22 },
    { start: 11433, length: 1, convRule: rule23 },
    { start: 11434, length: 1, convRule: rule22 },
    { start: 11435, length: 1, convRule: rule23 },
    { start: 11436, length: 1, convRule: rule22 },
    { start: 11437, length: 1, convRule: rule23 },
    { start: 11438, length: 1, convRule: rule22 },
    { start: 11439, length: 1, convRule: rule23 },
    { start: 11440, length: 1, convRule: rule22 },
    { start: 11441, length: 1, convRule: rule23 },
    { start: 11442, length: 1, convRule: rule22 },
    { start: 11443, length: 1, convRule: rule23 },
    { start: 11444, length: 1, convRule: rule22 },
    { start: 11445, length: 1, convRule: rule23 },
    { start: 11446, length: 1, convRule: rule22 },
    { start: 11447, length: 1, convRule: rule23 },
    { start: 11448, length: 1, convRule: rule22 },
    { start: 11449, length: 1, convRule: rule23 },
    { start: 11450, length: 1, convRule: rule22 },
    { start: 11451, length: 1, convRule: rule23 },
    { start: 11452, length: 1, convRule: rule22 },
    { start: 11453, length: 1, convRule: rule23 },
    { start: 11454, length: 1, convRule: rule22 },
    { start: 11455, length: 1, convRule: rule23 },
    { start: 11456, length: 1, convRule: rule22 },
    { start: 11457, length: 1, convRule: rule23 },
    { start: 11458, length: 1, convRule: rule22 },
    { start: 11459, length: 1, convRule: rule23 },
    { start: 11460, length: 1, convRule: rule22 },
    { start: 11461, length: 1, convRule: rule23 },
    { start: 11462, length: 1, convRule: rule22 },
    { start: 11463, length: 1, convRule: rule23 },
    { start: 11464, length: 1, convRule: rule22 },
    { start: 11465, length: 1, convRule: rule23 },
    { start: 11466, length: 1, convRule: rule22 },
    { start: 11467, length: 1, convRule: rule23 },
    { start: 11468, length: 1, convRule: rule22 },
    { start: 11469, length: 1, convRule: rule23 },
    { start: 11470, length: 1, convRule: rule22 },
    { start: 11471, length: 1, convRule: rule23 },
    { start: 11472, length: 1, convRule: rule22 },
    { start: 11473, length: 1, convRule: rule23 },
    { start: 11474, length: 1, convRule: rule22 },
    { start: 11475, length: 1, convRule: rule23 },
    { start: 11476, length: 1, convRule: rule22 },
    { start: 11477, length: 1, convRule: rule23 },
    { start: 11478, length: 1, convRule: rule22 },
    { start: 11479, length: 1, convRule: rule23 },
    { start: 11480, length: 1, convRule: rule22 },
    { start: 11481, length: 1, convRule: rule23 },
    { start: 11482, length: 1, convRule: rule22 },
    { start: 11483, length: 1, convRule: rule23 },
    { start: 11484, length: 1, convRule: rule22 },
    { start: 11485, length: 1, convRule: rule23 },
    { start: 11486, length: 1, convRule: rule22 },
    { start: 11487, length: 1, convRule: rule23 },
    { start: 11488, length: 1, convRule: rule22 },
    { start: 11489, length: 1, convRule: rule23 },
    { start: 11490, length: 1, convRule: rule22 },
    { start: 11491, length: 1, convRule: rule23 },
    { start: 11492, length: 1, convRule: rule20 },
    { start: 11493, length: 6, convRule: rule13 },
    { start: 11499, length: 1, convRule: rule22 },
    { start: 11500, length: 1, convRule: rule23 },
    { start: 11501, length: 1, convRule: rule22 },
    { start: 11502, length: 1, convRule: rule23 },
    { start: 11503, length: 3, convRule: rule92 },
    { start: 11506, length: 1, convRule: rule22 },
    { start: 11507, length: 1, convRule: rule23 },
    { start: 11513, length: 4, convRule: rule2 },
    { start: 11517, length: 1, convRule: rule17 },
    { start: 11518, length: 2, convRule: rule2 },
    { start: 11520, length: 38, convRule: rule182 },
    { start: 11559, length: 1, convRule: rule182 },
    { start: 11565, length: 1, convRule: rule182 },
    { start: 11568, length: 56, convRule: rule14 },
    { start: 11631, length: 1, convRule: rule91 },
    { start: 11632, length: 1, convRule: rule2 },
    { start: 11647, length: 1, convRule: rule92 },
    { start: 11648, length: 23, convRule: rule14 },
    { start: 11680, length: 7, convRule: rule14 },
    { start: 11688, length: 7, convRule: rule14 },
    { start: 11696, length: 7, convRule: rule14 },
    { start: 11704, length: 7, convRule: rule14 },
    { start: 11712, length: 7, convRule: rule14 },
    { start: 11720, length: 7, convRule: rule14 },
    { start: 11728, length: 7, convRule: rule14 },
    { start: 11736, length: 7, convRule: rule14 },
    { start: 11744, length: 32, convRule: rule92 },
    { start: 11776, length: 2, convRule: rule2 },
    { start: 11778, length: 1, convRule: rule15 },
    { start: 11779, length: 1, convRule: rule19 },
    { start: 11780, length: 1, convRule: rule15 },
    { start: 11781, length: 1, convRule: rule19 },
    { start: 11782, length: 3, convRule: rule2 },
    { start: 11785, length: 1, convRule: rule15 },
    { start: 11786, length: 1, convRule: rule19 },
    { start: 11787, length: 1, convRule: rule2 },
    { start: 11788, length: 1, convRule: rule15 },
    { start: 11789, length: 1, convRule: rule19 },
    { start: 11790, length: 9, convRule: rule2 },
    { start: 11799, length: 1, convRule: rule7 },
    { start: 11800, length: 2, convRule: rule2 },
    { start: 11802, length: 1, convRule: rule7 },
    { start: 11803, length: 1, convRule: rule2 },
    { start: 11804, length: 1, convRule: rule15 },
    { start: 11805, length: 1, convRule: rule19 },
    { start: 11806, length: 2, convRule: rule2 },
    { start: 11808, length: 1, convRule: rule15 },
    { start: 11809, length: 1, convRule: rule19 },
    { start: 11810, length: 1, convRule: rule4 },
    { start: 11811, length: 1, convRule: rule5 },
    { start: 11812, length: 1, convRule: rule4 },
    { start: 11813, length: 1, convRule: rule5 },
    { start: 11814, length: 1, convRule: rule4 },
    { start: 11815, length: 1, convRule: rule5 },
    { start: 11816, length: 1, convRule: rule4 },
    { start: 11817, length: 1, convRule: rule5 },
    { start: 11818, length: 5, convRule: rule2 },
    { start: 11823, length: 1, convRule: rule91 },
    { start: 11824, length: 10, convRule: rule2 },
    { start: 11834, length: 2, convRule: rule7 },
    { start: 11836, length: 4, convRule: rule2 },
    { start: 11840, length: 1, convRule: rule7 },
    { start: 11841, length: 1, convRule: rule2 },
    { start: 11842, length: 1, convRule: rule4 },
    { start: 11843, length: 13, convRule: rule2 },
    { start: 11856, length: 2, convRule: rule13 },
    { start: 11858, length: 1, convRule: rule2 },
    { start: 11904, length: 26, convRule: rule13 },
    { start: 11931, length: 89, convRule: rule13 },
    { start: 12032, length: 214, convRule: rule13 },
    { start: 12272, length: 12, convRule: rule13 },
    { start: 12288, length: 1, convRule: rule1 },
    { start: 12289, length: 3, convRule: rule2 },
    { start: 12292, length: 1, convRule: rule13 },
    { start: 12293, length: 1, convRule: rule91 },
    { start: 12294, length: 1, convRule: rule14 },
    { start: 12295, length: 1, convRule: rule128 },
    { start: 12296, length: 1, convRule: rule4 },
    { start: 12297, length: 1, convRule: rule5 },
    { start: 12298, length: 1, convRule: rule4 },
    { start: 12299, length: 1, convRule: rule5 },
    { start: 12300, length: 1, convRule: rule4 },
    { start: 12301, length: 1, convRule: rule5 },
    { start: 12302, length: 1, convRule: rule4 },
    { start: 12303, length: 1, convRule: rule5 },
    { start: 12304, length: 1, convRule: rule4 },
    { start: 12305, length: 1, convRule: rule5 },
    { start: 12306, length: 2, convRule: rule13 },
    { start: 12308, length: 1, convRule: rule4 },
    { start: 12309, length: 1, convRule: rule5 },
    { start: 12310, length: 1, convRule: rule4 },
    { start: 12311, length: 1, convRule: rule5 },
    { start: 12312, length: 1, convRule: rule4 },
    { start: 12313, length: 1, convRule: rule5 },
    { start: 12314, length: 1, convRule: rule4 },
    { start: 12315, length: 1, convRule: rule5 },
    { start: 12316, length: 1, convRule: rule7 },
    { start: 12317, length: 1, convRule: rule4 },
    { start: 12318, length: 2, convRule: rule5 },
    { start: 12320, length: 1, convRule: rule13 },
    { start: 12321, length: 9, convRule: rule128 },
    { start: 12330, length: 4, convRule: rule92 },
    { start: 12334, length: 2, convRule: rule124 },
    { start: 12336, length: 1, convRule: rule7 },
    { start: 12337, length: 5, convRule: rule91 },
    { start: 12342, length: 2, convRule: rule13 },
    { start: 12344, length: 3, convRule: rule128 },
    { start: 12347, length: 1, convRule: rule91 },
    { start: 12348, length: 1, convRule: rule14 },
    { start: 12349, length: 1, convRule: rule2 },
    { start: 12350, length: 2, convRule: rule13 },
    { start: 12353, length: 86, convRule: rule14 },
    { start: 12441, length: 2, convRule: rule92 },
    { start: 12443, length: 2, convRule: rule10 },
    { start: 12445, length: 2, convRule: rule91 },
    { start: 12447, length: 1, convRule: rule14 },
    { start: 12448, length: 1, convRule: rule7 },
    { start: 12449, length: 90, convRule: rule14 },
    { start: 12539, length: 1, convRule: rule2 },
    { start: 12540, length: 3, convRule: rule91 },
    { start: 12543, length: 1, convRule: rule14 },
    { start: 12549, length: 43, convRule: rule14 },
    { start: 12593, length: 94, convRule: rule14 },
    { start: 12688, length: 2, convRule: rule13 },
    { start: 12690, length: 4, convRule: rule17 },
    { start: 12694, length: 10, convRule: rule13 },
    { start: 12704, length: 32, convRule: rule14 },
    { start: 12736, length: 36, convRule: rule13 },
    { start: 12784, length: 16, convRule: rule14 },
    { start: 12800, length: 31, convRule: rule13 },
    { start: 12832, length: 10, convRule: rule17 },
    { start: 12842, length: 30, convRule: rule13 },
    { start: 12872, length: 8, convRule: rule17 },
    { start: 12880, length: 1, convRule: rule13 },
    { start: 12881, length: 15, convRule: rule17 },
    { start: 12896, length: 32, convRule: rule13 },
    { start: 12928, length: 10, convRule: rule17 },
    { start: 12938, length: 39, convRule: rule13 },
    { start: 12977, length: 15, convRule: rule17 },
    { start: 12992, length: 320, convRule: rule13 },
    { start: 13312, length: 6592, convRule: rule14 },
    { start: 19904, length: 64, convRule: rule13 },
    { start: 19968, length: 20989, convRule: rule14 },
    { start: 40960, length: 21, convRule: rule14 },
    { start: 40981, length: 1, convRule: rule91 },
    { start: 40982, length: 1143, convRule: rule14 },
    { start: 42128, length: 55, convRule: rule13 },
    { start: 42192, length: 40, convRule: rule14 },
    { start: 42232, length: 6, convRule: rule91 },
    { start: 42238, length: 2, convRule: rule2 },
    { start: 42240, length: 268, convRule: rule14 },
    { start: 42508, length: 1, convRule: rule91 },
    { start: 42509, length: 3, convRule: rule2 },
    { start: 42512, length: 16, convRule: rule14 },
    { start: 42528, length: 10, convRule: rule8 },
    { start: 42538, length: 2, convRule: rule14 },
    { start: 42560, length: 1, convRule: rule22 },
    { start: 42561, length: 1, convRule: rule23 },
    { start: 42562, length: 1, convRule: rule22 },
    { start: 42563, length: 1, convRule: rule23 },
    { start: 42564, length: 1, convRule: rule22 },
    { start: 42565, length: 1, convRule: rule23 },
    { start: 42566, length: 1, convRule: rule22 },
    { start: 42567, length: 1, convRule: rule23 },
    { start: 42568, length: 1, convRule: rule22 },
    { start: 42569, length: 1, convRule: rule23 },
    { start: 42570, length: 1, convRule: rule22 },
    { start: 42571, length: 1, convRule: rule23 },
    { start: 42572, length: 1, convRule: rule22 },
    { start: 42573, length: 1, convRule: rule23 },
    { start: 42574, length: 1, convRule: rule22 },
    { start: 42575, length: 1, convRule: rule23 },
    { start: 42576, length: 1, convRule: rule22 },
    { start: 42577, length: 1, convRule: rule23 },
    { start: 42578, length: 1, convRule: rule22 },
    { start: 42579, length: 1, convRule: rule23 },
    { start: 42580, length: 1, convRule: rule22 },
    { start: 42581, length: 1, convRule: rule23 },
    { start: 42582, length: 1, convRule: rule22 },
    { start: 42583, length: 1, convRule: rule23 },
    { start: 42584, length: 1, convRule: rule22 },
    { start: 42585, length: 1, convRule: rule23 },
    { start: 42586, length: 1, convRule: rule22 },
    { start: 42587, length: 1, convRule: rule23 },
    { start: 42588, length: 1, convRule: rule22 },
    { start: 42589, length: 1, convRule: rule23 },
    { start: 42590, length: 1, convRule: rule22 },
    { start: 42591, length: 1, convRule: rule23 },
    { start: 42592, length: 1, convRule: rule22 },
    { start: 42593, length: 1, convRule: rule23 },
    { start: 42594, length: 1, convRule: rule22 },
    { start: 42595, length: 1, convRule: rule23 },
    { start: 42596, length: 1, convRule: rule22 },
    { start: 42597, length: 1, convRule: rule23 },
    { start: 42598, length: 1, convRule: rule22 },
    { start: 42599, length: 1, convRule: rule23 },
    { start: 42600, length: 1, convRule: rule22 },
    { start: 42601, length: 1, convRule: rule23 },
    { start: 42602, length: 1, convRule: rule22 },
    { start: 42603, length: 1, convRule: rule23 },
    { start: 42604, length: 1, convRule: rule22 },
    { start: 42605, length: 1, convRule: rule23 },
    { start: 42606, length: 1, convRule: rule14 },
    { start: 42607, length: 1, convRule: rule92 },
    { start: 42608, length: 3, convRule: rule119 },
    { start: 42611, length: 1, convRule: rule2 },
    { start: 42612, length: 10, convRule: rule92 },
    { start: 42622, length: 1, convRule: rule2 },
    { start: 42623, length: 1, convRule: rule91 },
    { start: 42624, length: 1, convRule: rule22 },
    { start: 42625, length: 1, convRule: rule23 },
    { start: 42626, length: 1, convRule: rule22 },
    { start: 42627, length: 1, convRule: rule23 },
    { start: 42628, length: 1, convRule: rule22 },
    { start: 42629, length: 1, convRule: rule23 },
    { start: 42630, length: 1, convRule: rule22 },
    { start: 42631, length: 1, convRule: rule23 },
    { start: 42632, length: 1, convRule: rule22 },
    { start: 42633, length: 1, convRule: rule23 },
    { start: 42634, length: 1, convRule: rule22 },
    { start: 42635, length: 1, convRule: rule23 },
    { start: 42636, length: 1, convRule: rule22 },
    { start: 42637, length: 1, convRule: rule23 },
    { start: 42638, length: 1, convRule: rule22 },
    { start: 42639, length: 1, convRule: rule23 },
    { start: 42640, length: 1, convRule: rule22 },
    { start: 42641, length: 1, convRule: rule23 },
    { start: 42642, length: 1, convRule: rule22 },
    { start: 42643, length: 1, convRule: rule23 },
    { start: 42644, length: 1, convRule: rule22 },
    { start: 42645, length: 1, convRule: rule23 },
    { start: 42646, length: 1, convRule: rule22 },
    { start: 42647, length: 1, convRule: rule23 },
    { start: 42648, length: 1, convRule: rule22 },
    { start: 42649, length: 1, convRule: rule23 },
    { start: 42650, length: 1, convRule: rule22 },
    { start: 42651, length: 1, convRule: rule23 },
    { start: 42652, length: 2, convRule: rule91 },
    { start: 42654, length: 2, convRule: rule92 },
    { start: 42656, length: 70, convRule: rule14 },
    { start: 42726, length: 10, convRule: rule128 },
    { start: 42736, length: 2, convRule: rule92 },
    { start: 42738, length: 6, convRule: rule2 },
    { start: 42752, length: 23, convRule: rule10 },
    { start: 42775, length: 9, convRule: rule91 },
    { start: 42784, length: 2, convRule: rule10 },
    { start: 42786, length: 1, convRule: rule22 },
    { start: 42787, length: 1, convRule: rule23 },
    { start: 42788, length: 1, convRule: rule22 },
    { start: 42789, length: 1, convRule: rule23 },
    { start: 42790, length: 1, convRule: rule22 },
    { start: 42791, length: 1, convRule: rule23 },
    { start: 42792, length: 1, convRule: rule22 },
    { start: 42793, length: 1, convRule: rule23 },
    { start: 42794, length: 1, convRule: rule22 },
    { start: 42795, length: 1, convRule: rule23 },
    { start: 42796, length: 1, convRule: rule22 },
    { start: 42797, length: 1, convRule: rule23 },
    { start: 42798, length: 1, convRule: rule22 },
    { start: 42799, length: 1, convRule: rule23 },
    { start: 42800, length: 2, convRule: rule20 },
    { start: 42802, length: 1, convRule: rule22 },
    { start: 42803, length: 1, convRule: rule23 },
    { start: 42804, length: 1, convRule: rule22 },
    { start: 42805, length: 1, convRule: rule23 },
    { start: 42806, length: 1, convRule: rule22 },
    { start: 42807, length: 1, convRule: rule23 },
    { start: 42808, length: 1, convRule: rule22 },
    { start: 42809, length: 1, convRule: rule23 },
    { start: 42810, length: 1, convRule: rule22 },
    { start: 42811, length: 1, convRule: rule23 },
    { start: 42812, length: 1, convRule: rule22 },
    { start: 42813, length: 1, convRule: rule23 },
    { start: 42814, length: 1, convRule: rule22 },
    { start: 42815, length: 1, convRule: rule23 },
    { start: 42816, length: 1, convRule: rule22 },
    { start: 42817, length: 1, convRule: rule23 },
    { start: 42818, length: 1, convRule: rule22 },
    { start: 42819, length: 1, convRule: rule23 },
    { start: 42820, length: 1, convRule: rule22 },
    { start: 42821, length: 1, convRule: rule23 },
    { start: 42822, length: 1, convRule: rule22 },
    { start: 42823, length: 1, convRule: rule23 },
    { start: 42824, length: 1, convRule: rule22 },
    { start: 42825, length: 1, convRule: rule23 },
    { start: 42826, length: 1, convRule: rule22 },
    { start: 42827, length: 1, convRule: rule23 },
    { start: 42828, length: 1, convRule: rule22 },
    { start: 42829, length: 1, convRule: rule23 },
    { start: 42830, length: 1, convRule: rule22 },
    { start: 42831, length: 1, convRule: rule23 },
    { start: 42832, length: 1, convRule: rule22 },
    { start: 42833, length: 1, convRule: rule23 },
    { start: 42834, length: 1, convRule: rule22 },
    { start: 42835, length: 1, convRule: rule23 },
    { start: 42836, length: 1, convRule: rule22 },
    { start: 42837, length: 1, convRule: rule23 },
    { start: 42838, length: 1, convRule: rule22 },
    { start: 42839, length: 1, convRule: rule23 },
    { start: 42840, length: 1, convRule: rule22 },
    { start: 42841, length: 1, convRule: rule23 },
    { start: 42842, length: 1, convRule: rule22 },
    { start: 42843, length: 1, convRule: rule23 },
    { start: 42844, length: 1, convRule: rule22 },
    { start: 42845, length: 1, convRule: rule23 },
    { start: 42846, length: 1, convRule: rule22 },
    { start: 42847, length: 1, convRule: rule23 },
    { start: 42848, length: 1, convRule: rule22 },
    { start: 42849, length: 1, convRule: rule23 },
    { start: 42850, length: 1, convRule: rule22 },
    { start: 42851, length: 1, convRule: rule23 },
    { start: 42852, length: 1, convRule: rule22 },
    { start: 42853, length: 1, convRule: rule23 },
    { start: 42854, length: 1, convRule: rule22 },
    { start: 42855, length: 1, convRule: rule23 },
    { start: 42856, length: 1, convRule: rule22 },
    { start: 42857, length: 1, convRule: rule23 },
    { start: 42858, length: 1, convRule: rule22 },
    { start: 42859, length: 1, convRule: rule23 },
    { start: 42860, length: 1, convRule: rule22 },
    { start: 42861, length: 1, convRule: rule23 },
    { start: 42862, length: 1, convRule: rule22 },
    { start: 42863, length: 1, convRule: rule23 },
    { start: 42864, length: 1, convRule: rule91 },
    { start: 42865, length: 8, convRule: rule20 },
    { start: 42873, length: 1, convRule: rule22 },
    { start: 42874, length: 1, convRule: rule23 },
    { start: 42875, length: 1, convRule: rule22 },
    { start: 42876, length: 1, convRule: rule23 },
    { start: 42877, length: 1, convRule: rule183 },
    { start: 42878, length: 1, convRule: rule22 },
    { start: 42879, length: 1, convRule: rule23 },
    { start: 42880, length: 1, convRule: rule22 },
    { start: 42881, length: 1, convRule: rule23 },
    { start: 42882, length: 1, convRule: rule22 },
    { start: 42883, length: 1, convRule: rule23 },
    { start: 42884, length: 1, convRule: rule22 },
    { start: 42885, length: 1, convRule: rule23 },
    { start: 42886, length: 1, convRule: rule22 },
    { start: 42887, length: 1, convRule: rule23 },
    { start: 42888, length: 1, convRule: rule91 },
    { start: 42889, length: 2, convRule: rule10 },
    { start: 42891, length: 1, convRule: rule22 },
    { start: 42892, length: 1, convRule: rule23 },
    { start: 42893, length: 1, convRule: rule184 },
    { start: 42894, length: 1, convRule: rule20 },
    { start: 42895, length: 1, convRule: rule14 },
    { start: 42896, length: 1, convRule: rule22 },
    { start: 42897, length: 1, convRule: rule23 },
    { start: 42898, length: 1, convRule: rule22 },
    { start: 42899, length: 1, convRule: rule23 },
    { start: 42900, length: 1, convRule: rule185 },
    { start: 42901, length: 1, convRule: rule20 },
    { start: 42902, length: 1, convRule: rule22 },
    { start: 42903, length: 1, convRule: rule23 },
    { start: 42904, length: 1, convRule: rule22 },
    { start: 42905, length: 1, convRule: rule23 },
    { start: 42906, length: 1, convRule: rule22 },
    { start: 42907, length: 1, convRule: rule23 },
    { start: 42908, length: 1, convRule: rule22 },
    { start: 42909, length: 1, convRule: rule23 },
    { start: 42910, length: 1, convRule: rule22 },
    { start: 42911, length: 1, convRule: rule23 },
    { start: 42912, length: 1, convRule: rule22 },
    { start: 42913, length: 1, convRule: rule23 },
    { start: 42914, length: 1, convRule: rule22 },
    { start: 42915, length: 1, convRule: rule23 },
    { start: 42916, length: 1, convRule: rule22 },
    { start: 42917, length: 1, convRule: rule23 },
    { start: 42918, length: 1, convRule: rule22 },
    { start: 42919, length: 1, convRule: rule23 },
    { start: 42920, length: 1, convRule: rule22 },
    { start: 42921, length: 1, convRule: rule23 },
    { start: 42922, length: 1, convRule: rule186 },
    { start: 42923, length: 1, convRule: rule187 },
    { start: 42924, length: 1, convRule: rule188 },
    { start: 42925, length: 1, convRule: rule189 },
    { start: 42926, length: 1, convRule: rule186 },
    { start: 42927, length: 1, convRule: rule20 },
    { start: 42928, length: 1, convRule: rule190 },
    { start: 42929, length: 1, convRule: rule191 },
    { start: 42930, length: 1, convRule: rule192 },
    { start: 42931, length: 1, convRule: rule193 },
    { start: 42932, length: 1, convRule: rule22 },
    { start: 42933, length: 1, convRule: rule23 },
    { start: 42934, length: 1, convRule: rule22 },
    { start: 42935, length: 1, convRule: rule23 },
    { start: 42936, length: 1, convRule: rule22 },
    { start: 42937, length: 1, convRule: rule23 },
    { start: 42938, length: 1, convRule: rule22 },
    { start: 42939, length: 1, convRule: rule23 },
    { start: 42940, length: 1, convRule: rule22 },
    { start: 42941, length: 1, convRule: rule23 },
    { start: 42942, length: 1, convRule: rule22 },
    { start: 42943, length: 1, convRule: rule23 },
    { start: 42946, length: 1, convRule: rule22 },
    { start: 42947, length: 1, convRule: rule23 },
    { start: 42948, length: 1, convRule: rule194 },
    { start: 42949, length: 1, convRule: rule195 },
    { start: 42950, length: 1, convRule: rule196 },
    { start: 42951, length: 1, convRule: rule22 },
    { start: 42952, length: 1, convRule: rule23 },
    { start: 42953, length: 1, convRule: rule22 },
    { start: 42954, length: 1, convRule: rule23 },
    { start: 42997, length: 1, convRule: rule22 },
    { start: 42998, length: 1, convRule: rule23 },
    { start: 42999, length: 1, convRule: rule14 },
    { start: 43e3, length: 2, convRule: rule91 },
    { start: 43002, length: 1, convRule: rule20 },
    { start: 43003, length: 7, convRule: rule14 },
    { start: 43010, length: 1, convRule: rule92 },
    { start: 43011, length: 3, convRule: rule14 },
    { start: 43014, length: 1, convRule: rule92 },
    { start: 43015, length: 4, convRule: rule14 },
    { start: 43019, length: 1, convRule: rule92 },
    { start: 43020, length: 23, convRule: rule14 },
    { start: 43043, length: 2, convRule: rule124 },
    { start: 43045, length: 2, convRule: rule92 },
    { start: 43047, length: 1, convRule: rule124 },
    { start: 43048, length: 4, convRule: rule13 },
    { start: 43052, length: 1, convRule: rule92 },
    { start: 43056, length: 6, convRule: rule17 },
    { start: 43062, length: 2, convRule: rule13 },
    { start: 43064, length: 1, convRule: rule3 },
    { start: 43065, length: 1, convRule: rule13 },
    { start: 43072, length: 52, convRule: rule14 },
    { start: 43124, length: 4, convRule: rule2 },
    { start: 43136, length: 2, convRule: rule124 },
    { start: 43138, length: 50, convRule: rule14 },
    { start: 43188, length: 16, convRule: rule124 },
    { start: 43204, length: 2, convRule: rule92 },
    { start: 43214, length: 2, convRule: rule2 },
    { start: 43216, length: 10, convRule: rule8 },
    { start: 43232, length: 18, convRule: rule92 },
    { start: 43250, length: 6, convRule: rule14 },
    { start: 43256, length: 3, convRule: rule2 },
    { start: 43259, length: 1, convRule: rule14 },
    { start: 43260, length: 1, convRule: rule2 },
    { start: 43261, length: 2, convRule: rule14 },
    { start: 43263, length: 1, convRule: rule92 },
    { start: 43264, length: 10, convRule: rule8 },
    { start: 43274, length: 28, convRule: rule14 },
    { start: 43302, length: 8, convRule: rule92 },
    { start: 43310, length: 2, convRule: rule2 },
    { start: 43312, length: 23, convRule: rule14 },
    { start: 43335, length: 11, convRule: rule92 },
    { start: 43346, length: 2, convRule: rule124 },
    { start: 43359, length: 1, convRule: rule2 },
    { start: 43360, length: 29, convRule: rule14 },
    { start: 43392, length: 3, convRule: rule92 },
    { start: 43395, length: 1, convRule: rule124 },
    { start: 43396, length: 47, convRule: rule14 },
    { start: 43443, length: 1, convRule: rule92 },
    { start: 43444, length: 2, convRule: rule124 },
    { start: 43446, length: 4, convRule: rule92 },
    { start: 43450, length: 2, convRule: rule124 },
    { start: 43452, length: 2, convRule: rule92 },
    { start: 43454, length: 3, convRule: rule124 },
    { start: 43457, length: 13, convRule: rule2 },
    { start: 43471, length: 1, convRule: rule91 },
    { start: 43472, length: 10, convRule: rule8 },
    { start: 43486, length: 2, convRule: rule2 },
    { start: 43488, length: 5, convRule: rule14 },
    { start: 43493, length: 1, convRule: rule92 },
    { start: 43494, length: 1, convRule: rule91 },
    { start: 43495, length: 9, convRule: rule14 },
    { start: 43504, length: 10, convRule: rule8 },
    { start: 43514, length: 5, convRule: rule14 },
    { start: 43520, length: 41, convRule: rule14 },
    { start: 43561, length: 6, convRule: rule92 },
    { start: 43567, length: 2, convRule: rule124 },
    { start: 43569, length: 2, convRule: rule92 },
    { start: 43571, length: 2, convRule: rule124 },
    { start: 43573, length: 2, convRule: rule92 },
    { start: 43584, length: 3, convRule: rule14 },
    { start: 43587, length: 1, convRule: rule92 },
    { start: 43588, length: 8, convRule: rule14 },
    { start: 43596, length: 1, convRule: rule92 },
    { start: 43597, length: 1, convRule: rule124 },
    { start: 43600, length: 10, convRule: rule8 },
    { start: 43612, length: 4, convRule: rule2 },
    { start: 43616, length: 16, convRule: rule14 },
    { start: 43632, length: 1, convRule: rule91 },
    { start: 43633, length: 6, convRule: rule14 },
    { start: 43639, length: 3, convRule: rule13 },
    { start: 43642, length: 1, convRule: rule14 },
    { start: 43643, length: 1, convRule: rule124 },
    { start: 43644, length: 1, convRule: rule92 },
    { start: 43645, length: 1, convRule: rule124 },
    { start: 43646, length: 50, convRule: rule14 },
    { start: 43696, length: 1, convRule: rule92 },
    { start: 43697, length: 1, convRule: rule14 },
    { start: 43698, length: 3, convRule: rule92 },
    { start: 43701, length: 2, convRule: rule14 },
    { start: 43703, length: 2, convRule: rule92 },
    { start: 43705, length: 5, convRule: rule14 },
    { start: 43710, length: 2, convRule: rule92 },
    { start: 43712, length: 1, convRule: rule14 },
    { start: 43713, length: 1, convRule: rule92 },
    { start: 43714, length: 1, convRule: rule14 },
    { start: 43739, length: 2, convRule: rule14 },
    { start: 43741, length: 1, convRule: rule91 },
    { start: 43742, length: 2, convRule: rule2 },
    { start: 43744, length: 11, convRule: rule14 },
    { start: 43755, length: 1, convRule: rule124 },
    { start: 43756, length: 2, convRule: rule92 },
    { start: 43758, length: 2, convRule: rule124 },
    { start: 43760, length: 2, convRule: rule2 },
    { start: 43762, length: 1, convRule: rule14 },
    { start: 43763, length: 2, convRule: rule91 },
    { start: 43765, length: 1, convRule: rule124 },
    { start: 43766, length: 1, convRule: rule92 },
    { start: 43777, length: 6, convRule: rule14 },
    { start: 43785, length: 6, convRule: rule14 },
    { start: 43793, length: 6, convRule: rule14 },
    { start: 43808, length: 7, convRule: rule14 },
    { start: 43816, length: 7, convRule: rule14 },
    { start: 43824, length: 35, convRule: rule20 },
    { start: 43859, length: 1, convRule: rule197 },
    { start: 43860, length: 7, convRule: rule20 },
    { start: 43867, length: 1, convRule: rule10 },
    { start: 43868, length: 4, convRule: rule91 },
    { start: 43872, length: 9, convRule: rule20 },
    { start: 43881, length: 1, convRule: rule91 },
    { start: 43882, length: 2, convRule: rule10 },
    { start: 43888, length: 80, convRule: rule198 },
    { start: 43968, length: 35, convRule: rule14 },
    { start: 44003, length: 2, convRule: rule124 },
    { start: 44005, length: 1, convRule: rule92 },
    { start: 44006, length: 2, convRule: rule124 },
    { start: 44008, length: 1, convRule: rule92 },
    { start: 44009, length: 2, convRule: rule124 },
    { start: 44011, length: 1, convRule: rule2 },
    { start: 44012, length: 1, convRule: rule124 },
    { start: 44013, length: 1, convRule: rule92 },
    { start: 44016, length: 10, convRule: rule8 },
    { start: 44032, length: 11172, convRule: rule14 },
    { start: 55216, length: 23, convRule: rule14 },
    { start: 55243, length: 49, convRule: rule14 },
    { start: 55296, length: 896, convRule: rule199 },
    { start: 56192, length: 128, convRule: rule199 },
    { start: 56320, length: 1024, convRule: rule199 },
    { start: 57344, length: 6400, convRule: rule200 },
    { start: 63744, length: 366, convRule: rule14 },
    { start: 64112, length: 106, convRule: rule14 },
    { start: 64256, length: 7, convRule: rule20 },
    { start: 64275, length: 5, convRule: rule20 },
    { start: 64285, length: 1, convRule: rule14 },
    { start: 64286, length: 1, convRule: rule92 },
    { start: 64287, length: 10, convRule: rule14 },
    { start: 64297, length: 1, convRule: rule6 },
    { start: 64298, length: 13, convRule: rule14 },
    { start: 64312, length: 5, convRule: rule14 },
    { start: 64318, length: 1, convRule: rule14 },
    { start: 64320, length: 2, convRule: rule14 },
    { start: 64323, length: 2, convRule: rule14 },
    { start: 64326, length: 108, convRule: rule14 },
    { start: 64434, length: 16, convRule: rule10 },
    { start: 64467, length: 363, convRule: rule14 },
    { start: 64830, length: 1, convRule: rule5 },
    { start: 64831, length: 1, convRule: rule4 },
    { start: 64848, length: 64, convRule: rule14 },
    { start: 64914, length: 54, convRule: rule14 },
    { start: 65008, length: 12, convRule: rule14 },
    { start: 65020, length: 1, convRule: rule3 },
    { start: 65021, length: 1, convRule: rule13 },
    { start: 65024, length: 16, convRule: rule92 },
    { start: 65040, length: 7, convRule: rule2 },
    { start: 65047, length: 1, convRule: rule4 },
    { start: 65048, length: 1, convRule: rule5 },
    { start: 65049, length: 1, convRule: rule2 },
    { start: 65056, length: 16, convRule: rule92 },
    { start: 65072, length: 1, convRule: rule2 },
    { start: 65073, length: 2, convRule: rule7 },
    { start: 65075, length: 2, convRule: rule11 },
    { start: 65077, length: 1, convRule: rule4 },
    { start: 65078, length: 1, convRule: rule5 },
    { start: 65079, length: 1, convRule: rule4 },
    { start: 65080, length: 1, convRule: rule5 },
    { start: 65081, length: 1, convRule: rule4 },
    { start: 65082, length: 1, convRule: rule5 },
    { start: 65083, length: 1, convRule: rule4 },
    { start: 65084, length: 1, convRule: rule5 },
    { start: 65085, length: 1, convRule: rule4 },
    { start: 65086, length: 1, convRule: rule5 },
    { start: 65087, length: 1, convRule: rule4 },
    { start: 65088, length: 1, convRule: rule5 },
    { start: 65089, length: 1, convRule: rule4 },
    { start: 65090, length: 1, convRule: rule5 },
    { start: 65091, length: 1, convRule: rule4 },
    { start: 65092, length: 1, convRule: rule5 },
    { start: 65093, length: 2, convRule: rule2 },
    { start: 65095, length: 1, convRule: rule4 },
    { start: 65096, length: 1, convRule: rule5 },
    { start: 65097, length: 4, convRule: rule2 },
    { start: 65101, length: 3, convRule: rule11 },
    { start: 65104, length: 3, convRule: rule2 },
    { start: 65108, length: 4, convRule: rule2 },
    { start: 65112, length: 1, convRule: rule7 },
    { start: 65113, length: 1, convRule: rule4 },
    { start: 65114, length: 1, convRule: rule5 },
    { start: 65115, length: 1, convRule: rule4 },
    { start: 65116, length: 1, convRule: rule5 },
    { start: 65117, length: 1, convRule: rule4 },
    { start: 65118, length: 1, convRule: rule5 },
    { start: 65119, length: 3, convRule: rule2 },
    { start: 65122, length: 1, convRule: rule6 },
    { start: 65123, length: 1, convRule: rule7 },
    { start: 65124, length: 3, convRule: rule6 },
    { start: 65128, length: 1, convRule: rule2 },
    { start: 65129, length: 1, convRule: rule3 },
    { start: 65130, length: 2, convRule: rule2 },
    { start: 65136, length: 5, convRule: rule14 },
    { start: 65142, length: 135, convRule: rule14 },
    { start: 65279, length: 1, convRule: rule16 },
    { start: 65281, length: 3, convRule: rule2 },
    { start: 65284, length: 1, convRule: rule3 },
    { start: 65285, length: 3, convRule: rule2 },
    { start: 65288, length: 1, convRule: rule4 },
    { start: 65289, length: 1, convRule: rule5 },
    { start: 65290, length: 1, convRule: rule2 },
    { start: 65291, length: 1, convRule: rule6 },
    { start: 65292, length: 1, convRule: rule2 },
    { start: 65293, length: 1, convRule: rule7 },
    { start: 65294, length: 2, convRule: rule2 },
    { start: 65296, length: 10, convRule: rule8 },
    { start: 65306, length: 2, convRule: rule2 },
    { start: 65308, length: 3, convRule: rule6 },
    { start: 65311, length: 2, convRule: rule2 },
    { start: 65313, length: 26, convRule: rule9 },
    { start: 65339, length: 1, convRule: rule4 },
    { start: 65340, length: 1, convRule: rule2 },
    { start: 65341, length: 1, convRule: rule5 },
    { start: 65342, length: 1, convRule: rule10 },
    { start: 65343, length: 1, convRule: rule11 },
    { start: 65344, length: 1, convRule: rule10 },
    { start: 65345, length: 26, convRule: rule12 },
    { start: 65371, length: 1, convRule: rule4 },
    { start: 65372, length: 1, convRule: rule6 },
    { start: 65373, length: 1, convRule: rule5 },
    { start: 65374, length: 1, convRule: rule6 },
    { start: 65375, length: 1, convRule: rule4 },
    { start: 65376, length: 1, convRule: rule5 },
    { start: 65377, length: 1, convRule: rule2 },
    { start: 65378, length: 1, convRule: rule4 },
    { start: 65379, length: 1, convRule: rule5 },
    { start: 65380, length: 2, convRule: rule2 },
    { start: 65382, length: 10, convRule: rule14 },
    { start: 65392, length: 1, convRule: rule91 },
    { start: 65393, length: 45, convRule: rule14 },
    { start: 65438, length: 2, convRule: rule91 },
    { start: 65440, length: 31, convRule: rule14 },
    { start: 65474, length: 6, convRule: rule14 },
    { start: 65482, length: 6, convRule: rule14 },
    { start: 65490, length: 6, convRule: rule14 },
    { start: 65498, length: 3, convRule: rule14 },
    { start: 65504, length: 2, convRule: rule3 },
    { start: 65506, length: 1, convRule: rule6 },
    { start: 65507, length: 1, convRule: rule10 },
    { start: 65508, length: 1, convRule: rule13 },
    { start: 65509, length: 2, convRule: rule3 },
    { start: 65512, length: 1, convRule: rule13 },
    { start: 65513, length: 4, convRule: rule6 },
    { start: 65517, length: 2, convRule: rule13 },
    { start: 65529, length: 3, convRule: rule16 },
    { start: 65532, length: 2, convRule: rule13 },
    { start: 65536, length: 12, convRule: rule14 },
    { start: 65549, length: 26, convRule: rule14 },
    { start: 65576, length: 19, convRule: rule14 },
    { start: 65596, length: 2, convRule: rule14 },
    { start: 65599, length: 15, convRule: rule14 },
    { start: 65616, length: 14, convRule: rule14 },
    { start: 65664, length: 123, convRule: rule14 },
    { start: 65792, length: 3, convRule: rule2 },
    { start: 65799, length: 45, convRule: rule17 },
    { start: 65847, length: 9, convRule: rule13 },
    { start: 65856, length: 53, convRule: rule128 },
    { start: 65909, length: 4, convRule: rule17 },
    { start: 65913, length: 17, convRule: rule13 },
    { start: 65930, length: 2, convRule: rule17 },
    { start: 65932, length: 3, convRule: rule13 },
    { start: 65936, length: 13, convRule: rule13 },
    { start: 65952, length: 1, convRule: rule13 },
    { start: 66e3, length: 45, convRule: rule13 },
    { start: 66045, length: 1, convRule: rule92 },
    { start: 66176, length: 29, convRule: rule14 },
    { start: 66208, length: 49, convRule: rule14 },
    { start: 66272, length: 1, convRule: rule92 },
    { start: 66273, length: 27, convRule: rule17 },
    { start: 66304, length: 32, convRule: rule14 },
    { start: 66336, length: 4, convRule: rule17 },
    { start: 66349, length: 20, convRule: rule14 },
    { start: 66369, length: 1, convRule: rule128 },
    { start: 66370, length: 8, convRule: rule14 },
    { start: 66378, length: 1, convRule: rule128 },
    { start: 66384, length: 38, convRule: rule14 },
    { start: 66422, length: 5, convRule: rule92 },
    { start: 66432, length: 30, convRule: rule14 },
    { start: 66463, length: 1, convRule: rule2 },
    { start: 66464, length: 36, convRule: rule14 },
    { start: 66504, length: 8, convRule: rule14 },
    { start: 66512, length: 1, convRule: rule2 },
    { start: 66513, length: 5, convRule: rule128 },
    { start: 66560, length: 40, convRule: rule201 },
    { start: 66600, length: 40, convRule: rule202 },
    { start: 66640, length: 78, convRule: rule14 },
    { start: 66720, length: 10, convRule: rule8 },
    { start: 66736, length: 36, convRule: rule201 },
    { start: 66776, length: 36, convRule: rule202 },
    { start: 66816, length: 40, convRule: rule14 },
    { start: 66864, length: 52, convRule: rule14 },
    { start: 66927, length: 1, convRule: rule2 },
    { start: 67072, length: 311, convRule: rule14 },
    { start: 67392, length: 22, convRule: rule14 },
    { start: 67424, length: 8, convRule: rule14 },
    { start: 67584, length: 6, convRule: rule14 },
    { start: 67592, length: 1, convRule: rule14 },
    { start: 67594, length: 44, convRule: rule14 },
    { start: 67639, length: 2, convRule: rule14 },
    { start: 67644, length: 1, convRule: rule14 },
    { start: 67647, length: 23, convRule: rule14 },
    { start: 67671, length: 1, convRule: rule2 },
    { start: 67672, length: 8, convRule: rule17 },
    { start: 67680, length: 23, convRule: rule14 },
    { start: 67703, length: 2, convRule: rule13 },
    { start: 67705, length: 7, convRule: rule17 },
    { start: 67712, length: 31, convRule: rule14 },
    { start: 67751, length: 9, convRule: rule17 },
    { start: 67808, length: 19, convRule: rule14 },
    { start: 67828, length: 2, convRule: rule14 },
    { start: 67835, length: 5, convRule: rule17 },
    { start: 67840, length: 22, convRule: rule14 },
    { start: 67862, length: 6, convRule: rule17 },
    { start: 67871, length: 1, convRule: rule2 },
    { start: 67872, length: 26, convRule: rule14 },
    { start: 67903, length: 1, convRule: rule2 },
    { start: 67968, length: 56, convRule: rule14 },
    { start: 68028, length: 2, convRule: rule17 },
    { start: 68030, length: 2, convRule: rule14 },
    { start: 68032, length: 16, convRule: rule17 },
    { start: 68050, length: 46, convRule: rule17 },
    { start: 68096, length: 1, convRule: rule14 },
    { start: 68097, length: 3, convRule: rule92 },
    { start: 68101, length: 2, convRule: rule92 },
    { start: 68108, length: 4, convRule: rule92 },
    { start: 68112, length: 4, convRule: rule14 },
    { start: 68117, length: 3, convRule: rule14 },
    { start: 68121, length: 29, convRule: rule14 },
    { start: 68152, length: 3, convRule: rule92 },
    { start: 68159, length: 1, convRule: rule92 },
    { start: 68160, length: 9, convRule: rule17 },
    { start: 68176, length: 9, convRule: rule2 },
    { start: 68192, length: 29, convRule: rule14 },
    { start: 68221, length: 2, convRule: rule17 },
    { start: 68223, length: 1, convRule: rule2 },
    { start: 68224, length: 29, convRule: rule14 },
    { start: 68253, length: 3, convRule: rule17 },
    { start: 68288, length: 8, convRule: rule14 },
    { start: 68296, length: 1, convRule: rule13 },
    { start: 68297, length: 28, convRule: rule14 },
    { start: 68325, length: 2, convRule: rule92 },
    { start: 68331, length: 5, convRule: rule17 },
    { start: 68336, length: 7, convRule: rule2 },
    { start: 68352, length: 54, convRule: rule14 },
    { start: 68409, length: 7, convRule: rule2 },
    { start: 68416, length: 22, convRule: rule14 },
    { start: 68440, length: 8, convRule: rule17 },
    { start: 68448, length: 19, convRule: rule14 },
    { start: 68472, length: 8, convRule: rule17 },
    { start: 68480, length: 18, convRule: rule14 },
    { start: 68505, length: 4, convRule: rule2 },
    { start: 68521, length: 7, convRule: rule17 },
    { start: 68608, length: 73, convRule: rule14 },
    { start: 68736, length: 51, convRule: rule97 },
    { start: 68800, length: 51, convRule: rule102 },
    { start: 68858, length: 6, convRule: rule17 },
    { start: 68864, length: 36, convRule: rule14 },
    { start: 68900, length: 4, convRule: rule92 },
    { start: 68912, length: 10, convRule: rule8 },
    { start: 69216, length: 31, convRule: rule17 },
    { start: 69248, length: 42, convRule: rule14 },
    { start: 69291, length: 2, convRule: rule92 },
    { start: 69293, length: 1, convRule: rule7 },
    { start: 69296, length: 2, convRule: rule14 },
    { start: 69376, length: 29, convRule: rule14 },
    { start: 69405, length: 10, convRule: rule17 },
    { start: 69415, length: 1, convRule: rule14 },
    { start: 69424, length: 22, convRule: rule14 },
    { start: 69446, length: 11, convRule: rule92 },
    { start: 69457, length: 4, convRule: rule17 },
    { start: 69461, length: 5, convRule: rule2 },
    { start: 69552, length: 21, convRule: rule14 },
    { start: 69573, length: 7, convRule: rule17 },
    { start: 69600, length: 23, convRule: rule14 },
    { start: 69632, length: 1, convRule: rule124 },
    { start: 69633, length: 1, convRule: rule92 },
    { start: 69634, length: 1, convRule: rule124 },
    { start: 69635, length: 53, convRule: rule14 },
    { start: 69688, length: 15, convRule: rule92 },
    { start: 69703, length: 7, convRule: rule2 },
    { start: 69714, length: 20, convRule: rule17 },
    { start: 69734, length: 10, convRule: rule8 },
    { start: 69759, length: 3, convRule: rule92 },
    { start: 69762, length: 1, convRule: rule124 },
    { start: 69763, length: 45, convRule: rule14 },
    { start: 69808, length: 3, convRule: rule124 },
    { start: 69811, length: 4, convRule: rule92 },
    { start: 69815, length: 2, convRule: rule124 },
    { start: 69817, length: 2, convRule: rule92 },
    { start: 69819, length: 2, convRule: rule2 },
    { start: 69821, length: 1, convRule: rule16 },
    { start: 69822, length: 4, convRule: rule2 },
    { start: 69837, length: 1, convRule: rule16 },
    { start: 69840, length: 25, convRule: rule14 },
    { start: 69872, length: 10, convRule: rule8 },
    { start: 69888, length: 3, convRule: rule92 },
    { start: 69891, length: 36, convRule: rule14 },
    { start: 69927, length: 5, convRule: rule92 },
    { start: 69932, length: 1, convRule: rule124 },
    { start: 69933, length: 8, convRule: rule92 },
    { start: 69942, length: 10, convRule: rule8 },
    { start: 69952, length: 4, convRule: rule2 },
    { start: 69956, length: 1, convRule: rule14 },
    { start: 69957, length: 2, convRule: rule124 },
    { start: 69959, length: 1, convRule: rule14 },
    { start: 69968, length: 35, convRule: rule14 },
    { start: 70003, length: 1, convRule: rule92 },
    { start: 70004, length: 2, convRule: rule2 },
    { start: 70006, length: 1, convRule: rule14 },
    { start: 70016, length: 2, convRule: rule92 },
    { start: 70018, length: 1, convRule: rule124 },
    { start: 70019, length: 48, convRule: rule14 },
    { start: 70067, length: 3, convRule: rule124 },
    { start: 70070, length: 9, convRule: rule92 },
    { start: 70079, length: 2, convRule: rule124 },
    { start: 70081, length: 4, convRule: rule14 },
    { start: 70085, length: 4, convRule: rule2 },
    { start: 70089, length: 4, convRule: rule92 },
    { start: 70093, length: 1, convRule: rule2 },
    { start: 70094, length: 1, convRule: rule124 },
    { start: 70095, length: 1, convRule: rule92 },
    { start: 70096, length: 10, convRule: rule8 },
    { start: 70106, length: 1, convRule: rule14 },
    { start: 70107, length: 1, convRule: rule2 },
    { start: 70108, length: 1, convRule: rule14 },
    { start: 70109, length: 3, convRule: rule2 },
    { start: 70113, length: 20, convRule: rule17 },
    { start: 70144, length: 18, convRule: rule14 },
    { start: 70163, length: 25, convRule: rule14 },
    { start: 70188, length: 3, convRule: rule124 },
    { start: 70191, length: 3, convRule: rule92 },
    { start: 70194, length: 2, convRule: rule124 },
    { start: 70196, length: 1, convRule: rule92 },
    { start: 70197, length: 1, convRule: rule124 },
    { start: 70198, length: 2, convRule: rule92 },
    { start: 70200, length: 6, convRule: rule2 },
    { start: 70206, length: 1, convRule: rule92 },
    { start: 70272, length: 7, convRule: rule14 },
    { start: 70280, length: 1, convRule: rule14 },
    { start: 70282, length: 4, convRule: rule14 },
    { start: 70287, length: 15, convRule: rule14 },
    { start: 70303, length: 10, convRule: rule14 },
    { start: 70313, length: 1, convRule: rule2 },
    { start: 70320, length: 47, convRule: rule14 },
    { start: 70367, length: 1, convRule: rule92 },
    { start: 70368, length: 3, convRule: rule124 },
    { start: 70371, length: 8, convRule: rule92 },
    { start: 70384, length: 10, convRule: rule8 },
    { start: 70400, length: 2, convRule: rule92 },
    { start: 70402, length: 2, convRule: rule124 },
    { start: 70405, length: 8, convRule: rule14 },
    { start: 70415, length: 2, convRule: rule14 },
    { start: 70419, length: 22, convRule: rule14 },
    { start: 70442, length: 7, convRule: rule14 },
    { start: 70450, length: 2, convRule: rule14 },
    { start: 70453, length: 5, convRule: rule14 },
    { start: 70459, length: 2, convRule: rule92 },
    { start: 70461, length: 1, convRule: rule14 },
    { start: 70462, length: 2, convRule: rule124 },
    { start: 70464, length: 1, convRule: rule92 },
    { start: 70465, length: 4, convRule: rule124 },
    { start: 70471, length: 2, convRule: rule124 },
    { start: 70475, length: 3, convRule: rule124 },
    { start: 70480, length: 1, convRule: rule14 },
    { start: 70487, length: 1, convRule: rule124 },
    { start: 70493, length: 5, convRule: rule14 },
    { start: 70498, length: 2, convRule: rule124 },
    { start: 70502, length: 7, convRule: rule92 },
    { start: 70512, length: 5, convRule: rule92 },
    { start: 70656, length: 53, convRule: rule14 },
    { start: 70709, length: 3, convRule: rule124 },
    { start: 70712, length: 8, convRule: rule92 },
    { start: 70720, length: 2, convRule: rule124 },
    { start: 70722, length: 3, convRule: rule92 },
    { start: 70725, length: 1, convRule: rule124 },
    { start: 70726, length: 1, convRule: rule92 },
    { start: 70727, length: 4, convRule: rule14 },
    { start: 70731, length: 5, convRule: rule2 },
    { start: 70736, length: 10, convRule: rule8 },
    { start: 70746, length: 2, convRule: rule2 },
    { start: 70749, length: 1, convRule: rule2 },
    { start: 70750, length: 1, convRule: rule92 },
    { start: 70751, length: 3, convRule: rule14 },
    { start: 70784, length: 48, convRule: rule14 },
    { start: 70832, length: 3, convRule: rule124 },
    { start: 70835, length: 6, convRule: rule92 },
    { start: 70841, length: 1, convRule: rule124 },
    { start: 70842, length: 1, convRule: rule92 },
    { start: 70843, length: 4, convRule: rule124 },
    { start: 70847, length: 2, convRule: rule92 },
    { start: 70849, length: 1, convRule: rule124 },
    { start: 70850, length: 2, convRule: rule92 },
    { start: 70852, length: 2, convRule: rule14 },
    { start: 70854, length: 1, convRule: rule2 },
    { start: 70855, length: 1, convRule: rule14 },
    { start: 70864, length: 10, convRule: rule8 },
    { start: 71040, length: 47, convRule: rule14 },
    { start: 71087, length: 3, convRule: rule124 },
    { start: 71090, length: 4, convRule: rule92 },
    { start: 71096, length: 4, convRule: rule124 },
    { start: 71100, length: 2, convRule: rule92 },
    { start: 71102, length: 1, convRule: rule124 },
    { start: 71103, length: 2, convRule: rule92 },
    { start: 71105, length: 23, convRule: rule2 },
    { start: 71128, length: 4, convRule: rule14 },
    { start: 71132, length: 2, convRule: rule92 },
    { start: 71168, length: 48, convRule: rule14 },
    { start: 71216, length: 3, convRule: rule124 },
    { start: 71219, length: 8, convRule: rule92 },
    { start: 71227, length: 2, convRule: rule124 },
    { start: 71229, length: 1, convRule: rule92 },
    { start: 71230, length: 1, convRule: rule124 },
    { start: 71231, length: 2, convRule: rule92 },
    { start: 71233, length: 3, convRule: rule2 },
    { start: 71236, length: 1, convRule: rule14 },
    { start: 71248, length: 10, convRule: rule8 },
    { start: 71264, length: 13, convRule: rule2 },
    { start: 71296, length: 43, convRule: rule14 },
    { start: 71339, length: 1, convRule: rule92 },
    { start: 71340, length: 1, convRule: rule124 },
    { start: 71341, length: 1, convRule: rule92 },
    { start: 71342, length: 2, convRule: rule124 },
    { start: 71344, length: 6, convRule: rule92 },
    { start: 71350, length: 1, convRule: rule124 },
    { start: 71351, length: 1, convRule: rule92 },
    { start: 71352, length: 1, convRule: rule14 },
    { start: 71360, length: 10, convRule: rule8 },
    { start: 71424, length: 27, convRule: rule14 },
    { start: 71453, length: 3, convRule: rule92 },
    { start: 71456, length: 2, convRule: rule124 },
    { start: 71458, length: 4, convRule: rule92 },
    { start: 71462, length: 1, convRule: rule124 },
    { start: 71463, length: 5, convRule: rule92 },
    { start: 71472, length: 10, convRule: rule8 },
    { start: 71482, length: 2, convRule: rule17 },
    { start: 71484, length: 3, convRule: rule2 },
    { start: 71487, length: 1, convRule: rule13 },
    { start: 71680, length: 44, convRule: rule14 },
    { start: 71724, length: 3, convRule: rule124 },
    { start: 71727, length: 9, convRule: rule92 },
    { start: 71736, length: 1, convRule: rule124 },
    { start: 71737, length: 2, convRule: rule92 },
    { start: 71739, length: 1, convRule: rule2 },
    { start: 71840, length: 32, convRule: rule9 },
    { start: 71872, length: 32, convRule: rule12 },
    { start: 71904, length: 10, convRule: rule8 },
    { start: 71914, length: 9, convRule: rule17 },
    { start: 71935, length: 8, convRule: rule14 },
    { start: 71945, length: 1, convRule: rule14 },
    { start: 71948, length: 8, convRule: rule14 },
    { start: 71957, length: 2, convRule: rule14 },
    { start: 71960, length: 24, convRule: rule14 },
    { start: 71984, length: 6, convRule: rule124 },
    { start: 71991, length: 2, convRule: rule124 },
    { start: 71995, length: 2, convRule: rule92 },
    { start: 71997, length: 1, convRule: rule124 },
    { start: 71998, length: 1, convRule: rule92 },
    { start: 71999, length: 1, convRule: rule14 },
    { start: 72e3, length: 1, convRule: rule124 },
    { start: 72001, length: 1, convRule: rule14 },
    { start: 72002, length: 1, convRule: rule124 },
    { start: 72003, length: 1, convRule: rule92 },
    { start: 72004, length: 3, convRule: rule2 },
    { start: 72016, length: 10, convRule: rule8 },
    { start: 72096, length: 8, convRule: rule14 },
    { start: 72106, length: 39, convRule: rule14 },
    { start: 72145, length: 3, convRule: rule124 },
    { start: 72148, length: 4, convRule: rule92 },
    { start: 72154, length: 2, convRule: rule92 },
    { start: 72156, length: 4, convRule: rule124 },
    { start: 72160, length: 1, convRule: rule92 },
    { start: 72161, length: 1, convRule: rule14 },
    { start: 72162, length: 1, convRule: rule2 },
    { start: 72163, length: 1, convRule: rule14 },
    { start: 72164, length: 1, convRule: rule124 },
    { start: 72192, length: 1, convRule: rule14 },
    { start: 72193, length: 10, convRule: rule92 },
    { start: 72203, length: 40, convRule: rule14 },
    { start: 72243, length: 6, convRule: rule92 },
    { start: 72249, length: 1, convRule: rule124 },
    { start: 72250, length: 1, convRule: rule14 },
    { start: 72251, length: 4, convRule: rule92 },
    { start: 72255, length: 8, convRule: rule2 },
    { start: 72263, length: 1, convRule: rule92 },
    { start: 72272, length: 1, convRule: rule14 },
    { start: 72273, length: 6, convRule: rule92 },
    { start: 72279, length: 2, convRule: rule124 },
    { start: 72281, length: 3, convRule: rule92 },
    { start: 72284, length: 46, convRule: rule14 },
    { start: 72330, length: 13, convRule: rule92 },
    { start: 72343, length: 1, convRule: rule124 },
    { start: 72344, length: 2, convRule: rule92 },
    { start: 72346, length: 3, convRule: rule2 },
    { start: 72349, length: 1, convRule: rule14 },
    { start: 72350, length: 5, convRule: rule2 },
    { start: 72384, length: 57, convRule: rule14 },
    { start: 72704, length: 9, convRule: rule14 },
    { start: 72714, length: 37, convRule: rule14 },
    { start: 72751, length: 1, convRule: rule124 },
    { start: 72752, length: 7, convRule: rule92 },
    { start: 72760, length: 6, convRule: rule92 },
    { start: 72766, length: 1, convRule: rule124 },
    { start: 72767, length: 1, convRule: rule92 },
    { start: 72768, length: 1, convRule: rule14 },
    { start: 72769, length: 5, convRule: rule2 },
    { start: 72784, length: 10, convRule: rule8 },
    { start: 72794, length: 19, convRule: rule17 },
    { start: 72816, length: 2, convRule: rule2 },
    { start: 72818, length: 30, convRule: rule14 },
    { start: 72850, length: 22, convRule: rule92 },
    { start: 72873, length: 1, convRule: rule124 },
    { start: 72874, length: 7, convRule: rule92 },
    { start: 72881, length: 1, convRule: rule124 },
    { start: 72882, length: 2, convRule: rule92 },
    { start: 72884, length: 1, convRule: rule124 },
    { start: 72885, length: 2, convRule: rule92 },
    { start: 72960, length: 7, convRule: rule14 },
    { start: 72968, length: 2, convRule: rule14 },
    { start: 72971, length: 38, convRule: rule14 },
    { start: 73009, length: 6, convRule: rule92 },
    { start: 73018, length: 1, convRule: rule92 },
    { start: 73020, length: 2, convRule: rule92 },
    { start: 73023, length: 7, convRule: rule92 },
    { start: 73030, length: 1, convRule: rule14 },
    { start: 73031, length: 1, convRule: rule92 },
    { start: 73040, length: 10, convRule: rule8 },
    { start: 73056, length: 6, convRule: rule14 },
    { start: 73063, length: 2, convRule: rule14 },
    { start: 73066, length: 32, convRule: rule14 },
    { start: 73098, length: 5, convRule: rule124 },
    { start: 73104, length: 2, convRule: rule92 },
    { start: 73107, length: 2, convRule: rule124 },
    { start: 73109, length: 1, convRule: rule92 },
    { start: 73110, length: 1, convRule: rule124 },
    { start: 73111, length: 1, convRule: rule92 },
    { start: 73112, length: 1, convRule: rule14 },
    { start: 73120, length: 10, convRule: rule8 },
    { start: 73440, length: 19, convRule: rule14 },
    { start: 73459, length: 2, convRule: rule92 },
    { start: 73461, length: 2, convRule: rule124 },
    { start: 73463, length: 2, convRule: rule2 },
    { start: 73648, length: 1, convRule: rule14 },
    { start: 73664, length: 21, convRule: rule17 },
    { start: 73685, length: 8, convRule: rule13 },
    { start: 73693, length: 4, convRule: rule3 },
    { start: 73697, length: 17, convRule: rule13 },
    { start: 73727, length: 1, convRule: rule2 },
    { start: 73728, length: 922, convRule: rule14 },
    { start: 74752, length: 111, convRule: rule128 },
    { start: 74864, length: 5, convRule: rule2 },
    { start: 74880, length: 196, convRule: rule14 },
    { start: 77824, length: 1071, convRule: rule14 },
    { start: 78896, length: 9, convRule: rule16 },
    { start: 82944, length: 583, convRule: rule14 },
    { start: 92160, length: 569, convRule: rule14 },
    { start: 92736, length: 31, convRule: rule14 },
    { start: 92768, length: 10, convRule: rule8 },
    { start: 92782, length: 2, convRule: rule2 },
    { start: 92880, length: 30, convRule: rule14 },
    { start: 92912, length: 5, convRule: rule92 },
    { start: 92917, length: 1, convRule: rule2 },
    { start: 92928, length: 48, convRule: rule14 },
    { start: 92976, length: 7, convRule: rule92 },
    { start: 92983, length: 5, convRule: rule2 },
    { start: 92988, length: 4, convRule: rule13 },
    { start: 92992, length: 4, convRule: rule91 },
    { start: 92996, length: 1, convRule: rule2 },
    { start: 92997, length: 1, convRule: rule13 },
    { start: 93008, length: 10, convRule: rule8 },
    { start: 93019, length: 7, convRule: rule17 },
    { start: 93027, length: 21, convRule: rule14 },
    { start: 93053, length: 19, convRule: rule14 },
    { start: 93760, length: 32, convRule: rule9 },
    { start: 93792, length: 32, convRule: rule12 },
    { start: 93824, length: 23, convRule: rule17 },
    { start: 93847, length: 4, convRule: rule2 },
    { start: 93952, length: 75, convRule: rule14 },
    { start: 94031, length: 1, convRule: rule92 },
    { start: 94032, length: 1, convRule: rule14 },
    { start: 94033, length: 55, convRule: rule124 },
    { start: 94095, length: 4, convRule: rule92 },
    { start: 94099, length: 13, convRule: rule91 },
    { start: 94176, length: 2, convRule: rule91 },
    { start: 94178, length: 1, convRule: rule2 },
    { start: 94179, length: 1, convRule: rule91 },
    { start: 94180, length: 1, convRule: rule92 },
    { start: 94192, length: 2, convRule: rule124 },
    { start: 94208, length: 6136, convRule: rule14 },
    { start: 100352, length: 1238, convRule: rule14 },
    { start: 101632, length: 9, convRule: rule14 },
    { start: 110592, length: 287, convRule: rule14 },
    { start: 110928, length: 3, convRule: rule14 },
    { start: 110948, length: 4, convRule: rule14 },
    { start: 110960, length: 396, convRule: rule14 },
    { start: 113664, length: 107, convRule: rule14 },
    { start: 113776, length: 13, convRule: rule14 },
    { start: 113792, length: 9, convRule: rule14 },
    { start: 113808, length: 10, convRule: rule14 },
    { start: 113820, length: 1, convRule: rule13 },
    { start: 113821, length: 2, convRule: rule92 },
    { start: 113823, length: 1, convRule: rule2 },
    { start: 113824, length: 4, convRule: rule16 },
    { start: 118784, length: 246, convRule: rule13 },
    { start: 119040, length: 39, convRule: rule13 },
    { start: 119081, length: 60, convRule: rule13 },
    { start: 119141, length: 2, convRule: rule124 },
    { start: 119143, length: 3, convRule: rule92 },
    { start: 119146, length: 3, convRule: rule13 },
    { start: 119149, length: 6, convRule: rule124 },
    { start: 119155, length: 8, convRule: rule16 },
    { start: 119163, length: 8, convRule: rule92 },
    { start: 119171, length: 2, convRule: rule13 },
    { start: 119173, length: 7, convRule: rule92 },
    { start: 119180, length: 30, convRule: rule13 },
    { start: 119210, length: 4, convRule: rule92 },
    { start: 119214, length: 59, convRule: rule13 },
    { start: 119296, length: 66, convRule: rule13 },
    { start: 119362, length: 3, convRule: rule92 },
    { start: 119365, length: 1, convRule: rule13 },
    { start: 119520, length: 20, convRule: rule17 },
    { start: 119552, length: 87, convRule: rule13 },
    { start: 119648, length: 25, convRule: rule17 },
    { start: 119808, length: 26, convRule: rule107 },
    { start: 119834, length: 26, convRule: rule20 },
    { start: 119860, length: 26, convRule: rule107 },
    { start: 119886, length: 7, convRule: rule20 },
    { start: 119894, length: 18, convRule: rule20 },
    { start: 119912, length: 26, convRule: rule107 },
    { start: 119938, length: 26, convRule: rule20 },
    { start: 119964, length: 1, convRule: rule107 },
    { start: 119966, length: 2, convRule: rule107 },
    { start: 119970, length: 1, convRule: rule107 },
    { start: 119973, length: 2, convRule: rule107 },
    { start: 119977, length: 4, convRule: rule107 },
    { start: 119982, length: 8, convRule: rule107 },
    { start: 119990, length: 4, convRule: rule20 },
    { start: 119995, length: 1, convRule: rule20 },
    { start: 119997, length: 7, convRule: rule20 },
    { start: 120005, length: 11, convRule: rule20 },
    { start: 120016, length: 26, convRule: rule107 },
    { start: 120042, length: 26, convRule: rule20 },
    { start: 120068, length: 2, convRule: rule107 },
    { start: 120071, length: 4, convRule: rule107 },
    { start: 120077, length: 8, convRule: rule107 },
    { start: 120086, length: 7, convRule: rule107 },
    { start: 120094, length: 26, convRule: rule20 },
    { start: 120120, length: 2, convRule: rule107 },
    { start: 120123, length: 4, convRule: rule107 },
    { start: 120128, length: 5, convRule: rule107 },
    { start: 120134, length: 1, convRule: rule107 },
    { start: 120138, length: 7, convRule: rule107 },
    { start: 120146, length: 26, convRule: rule20 },
    { start: 120172, length: 26, convRule: rule107 },
    { start: 120198, length: 26, convRule: rule20 },
    { start: 120224, length: 26, convRule: rule107 },
    { start: 120250, length: 26, convRule: rule20 },
    { start: 120276, length: 26, convRule: rule107 },
    { start: 120302, length: 26, convRule: rule20 },
    { start: 120328, length: 26, convRule: rule107 },
    { start: 120354, length: 26, convRule: rule20 },
    { start: 120380, length: 26, convRule: rule107 },
    { start: 120406, length: 26, convRule: rule20 },
    { start: 120432, length: 26, convRule: rule107 },
    { start: 120458, length: 28, convRule: rule20 },
    { start: 120488, length: 25, convRule: rule107 },
    { start: 120513, length: 1, convRule: rule6 },
    { start: 120514, length: 25, convRule: rule20 },
    { start: 120539, length: 1, convRule: rule6 },
    { start: 120540, length: 6, convRule: rule20 },
    { start: 120546, length: 25, convRule: rule107 },
    { start: 120571, length: 1, convRule: rule6 },
    { start: 120572, length: 25, convRule: rule20 },
    { start: 120597, length: 1, convRule: rule6 },
    { start: 120598, length: 6, convRule: rule20 },
    { start: 120604, length: 25, convRule: rule107 },
    { start: 120629, length: 1, convRule: rule6 },
    { start: 120630, length: 25, convRule: rule20 },
    { start: 120655, length: 1, convRule: rule6 },
    { start: 120656, length: 6, convRule: rule20 },
    { start: 120662, length: 25, convRule: rule107 },
    { start: 120687, length: 1, convRule: rule6 },
    { start: 120688, length: 25, convRule: rule20 },
    { start: 120713, length: 1, convRule: rule6 },
    { start: 120714, length: 6, convRule: rule20 },
    { start: 120720, length: 25, convRule: rule107 },
    { start: 120745, length: 1, convRule: rule6 },
    { start: 120746, length: 25, convRule: rule20 },
    { start: 120771, length: 1, convRule: rule6 },
    { start: 120772, length: 6, convRule: rule20 },
    { start: 120778, length: 1, convRule: rule107 },
    { start: 120779, length: 1, convRule: rule20 },
    { start: 120782, length: 50, convRule: rule8 },
    { start: 120832, length: 512, convRule: rule13 },
    { start: 121344, length: 55, convRule: rule92 },
    { start: 121399, length: 4, convRule: rule13 },
    { start: 121403, length: 50, convRule: rule92 },
    { start: 121453, length: 8, convRule: rule13 },
    { start: 121461, length: 1, convRule: rule92 },
    { start: 121462, length: 14, convRule: rule13 },
    { start: 121476, length: 1, convRule: rule92 },
    { start: 121477, length: 2, convRule: rule13 },
    { start: 121479, length: 5, convRule: rule2 },
    { start: 121499, length: 5, convRule: rule92 },
    { start: 121505, length: 15, convRule: rule92 },
    { start: 122880, length: 7, convRule: rule92 },
    { start: 122888, length: 17, convRule: rule92 },
    { start: 122907, length: 7, convRule: rule92 },
    { start: 122915, length: 2, convRule: rule92 },
    { start: 122918, length: 5, convRule: rule92 },
    { start: 123136, length: 45, convRule: rule14 },
    { start: 123184, length: 7, convRule: rule92 },
    { start: 123191, length: 7, convRule: rule91 },
    { start: 123200, length: 10, convRule: rule8 },
    { start: 123214, length: 1, convRule: rule14 },
    { start: 123215, length: 1, convRule: rule13 },
    { start: 123584, length: 44, convRule: rule14 },
    { start: 123628, length: 4, convRule: rule92 },
    { start: 123632, length: 10, convRule: rule8 },
    { start: 123647, length: 1, convRule: rule3 },
    { start: 124928, length: 197, convRule: rule14 },
    { start: 125127, length: 9, convRule: rule17 },
    { start: 125136, length: 7, convRule: rule92 },
    { start: 125184, length: 34, convRule: rule203 },
    { start: 125218, length: 34, convRule: rule204 },
    { start: 125252, length: 7, convRule: rule92 },
    { start: 125259, length: 1, convRule: rule91 },
    { start: 125264, length: 10, convRule: rule8 },
    { start: 125278, length: 2, convRule: rule2 },
    { start: 126065, length: 59, convRule: rule17 },
    { start: 126124, length: 1, convRule: rule13 },
    { start: 126125, length: 3, convRule: rule17 },
    { start: 126128, length: 1, convRule: rule3 },
    { start: 126129, length: 4, convRule: rule17 },
    { start: 126209, length: 45, convRule: rule17 },
    { start: 126254, length: 1, convRule: rule13 },
    { start: 126255, length: 15, convRule: rule17 },
    { start: 126464, length: 4, convRule: rule14 },
    { start: 126469, length: 27, convRule: rule14 },
    { start: 126497, length: 2, convRule: rule14 },
    { start: 126500, length: 1, convRule: rule14 },
    { start: 126503, length: 1, convRule: rule14 },
    { start: 126505, length: 10, convRule: rule14 },
    { start: 126516, length: 4, convRule: rule14 },
    { start: 126521, length: 1, convRule: rule14 },
    { start: 126523, length: 1, convRule: rule14 },
    { start: 126530, length: 1, convRule: rule14 },
    { start: 126535, length: 1, convRule: rule14 },
    { start: 126537, length: 1, convRule: rule14 },
    { start: 126539, length: 1, convRule: rule14 },
    { start: 126541, length: 3, convRule: rule14 },
    { start: 126545, length: 2, convRule: rule14 },
    { start: 126548, length: 1, convRule: rule14 },
    { start: 126551, length: 1, convRule: rule14 },
    { start: 126553, length: 1, convRule: rule14 },
    { start: 126555, length: 1, convRule: rule14 },
    { start: 126557, length: 1, convRule: rule14 },
    { start: 126559, length: 1, convRule: rule14 },
    { start: 126561, length: 2, convRule: rule14 },
    { start: 126564, length: 1, convRule: rule14 },
    { start: 126567, length: 4, convRule: rule14 },
    { start: 126572, length: 7, convRule: rule14 },
    { start: 126580, length: 4, convRule: rule14 },
    { start: 126585, length: 4, convRule: rule14 },
    { start: 126590, length: 1, convRule: rule14 },
    { start: 126592, length: 10, convRule: rule14 },
    { start: 126603, length: 17, convRule: rule14 },
    { start: 126625, length: 3, convRule: rule14 },
    { start: 126629, length: 5, convRule: rule14 },
    { start: 126635, length: 17, convRule: rule14 },
    { start: 126704, length: 2, convRule: rule6 },
    { start: 126976, length: 44, convRule: rule13 },
    { start: 127024, length: 100, convRule: rule13 },
    { start: 127136, length: 15, convRule: rule13 },
    { start: 127153, length: 15, convRule: rule13 },
    { start: 127169, length: 15, convRule: rule13 },
    { start: 127185, length: 37, convRule: rule13 },
    { start: 127232, length: 13, convRule: rule17 },
    { start: 127245, length: 161, convRule: rule13 },
    { start: 127462, length: 29, convRule: rule13 },
    { start: 127504, length: 44, convRule: rule13 },
    { start: 127552, length: 9, convRule: rule13 },
    { start: 127568, length: 2, convRule: rule13 },
    { start: 127584, length: 6, convRule: rule13 },
    { start: 127744, length: 251, convRule: rule13 },
    { start: 127995, length: 5, convRule: rule10 },
    { start: 128e3, length: 728, convRule: rule13 },
    { start: 128736, length: 13, convRule: rule13 },
    { start: 128752, length: 13, convRule: rule13 },
    { start: 128768, length: 116, convRule: rule13 },
    { start: 128896, length: 89, convRule: rule13 },
    { start: 128992, length: 12, convRule: rule13 },
    { start: 129024, length: 12, convRule: rule13 },
    { start: 129040, length: 56, convRule: rule13 },
    { start: 129104, length: 10, convRule: rule13 },
    { start: 129120, length: 40, convRule: rule13 },
    { start: 129168, length: 30, convRule: rule13 },
    { start: 129200, length: 2, convRule: rule13 },
    { start: 129280, length: 121, convRule: rule13 },
    { start: 129402, length: 82, convRule: rule13 },
    { start: 129485, length: 135, convRule: rule13 },
    { start: 129632, length: 14, convRule: rule13 },
    { start: 129648, length: 5, convRule: rule13 },
    { start: 129656, length: 3, convRule: rule13 },
    { start: 129664, length: 7, convRule: rule13 },
    { start: 129680, length: 25, convRule: rule13 },
    { start: 129712, length: 7, convRule: rule13 },
    { start: 129728, length: 3, convRule: rule13 },
    { start: 129744, length: 7, convRule: rule13 },
    { start: 129792, length: 147, convRule: rule13 },
    { start: 129940, length: 55, convRule: rule13 },
    { start: 130032, length: 10, convRule: rule8 },
    { start: 131072, length: 42718, convRule: rule14 },
    { start: 173824, length: 4149, convRule: rule14 },
    { start: 177984, length: 222, convRule: rule14 },
    { start: 178208, length: 5762, convRule: rule14 },
    { start: 183984, length: 7473, convRule: rule14 },
    { start: 194560, length: 542, convRule: rule14 },
    { start: 196608, length: 4939, convRule: rule14 },
    { start: 917505, length: 1, convRule: rule16 },
    { start: 917536, length: 96, convRule: rule16 },
    { start: 917760, length: 240, convRule: rule92 },
    { start: 983040, length: 65534, convRule: rule200 },
    { start: 1048576, length: 65534, convRule: rule200 }
  ];
  var checkAttr = (categories) => ($$char2) => {
    const maybeConversionRule = getRule(allchars)($$char2)((() => {
      if ($$char2 < 256) {
        return 63;
      }
      return 3396;
    })());
    if (maybeConversionRule.tag === "Nothing") {
      return false;
    }
    if (maybeConversionRule.tag === "Just") {
      const $3 = findIndex((v) => v === maybeConversionRule._1.category)(categories);
      if ($3.tag === "Nothing") {
        return false;
      }
      if ($3.tag === "Just") {
        return true;
      }
      fail();
    }
    fail();
  };

  // output-es/Data.Bounded/foreign.js
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output-es/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output-es/DataType/index.js
  var $DataType$p = (_1, _2) => ({ tag: "DataType", _1, _2 });
  var fromFoldable3 = /* @__PURE__ */ fromFoldable2(foldableArray);
  var fromFoldable12 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)(unit)(m))(Leaf2))();
  var toUnfoldable6 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
  var show = /* @__PURE__ */ (() => showSet(showString).show)();
  var bind2Flipped2 = /* @__PURE__ */ bind2Flipped(monadEither);
  var DataType = (value0) => (value1) => $DataType$p(value0, value1);
  var typeName = (v) => v._1;
  var eqDataType$pInt = { eq: (x2) => (y2) => x2._1 === y2._1 };
  var showDataType$pInt = { show: typeName };
  var isCtrName = (str) => checkAttr([512, 524288])(toCharCode(definitely("absurd")(charAt2(0)(str))));
  var showCtr = (c) => {
    if (isCtrName(c)) {
      return c;
    }
    if (":" === definitely("absurd")(charAt2(0)(c))) {
      return "(" + (c + ")");
    }
    return unsafePerformEffect(throwException(error("absurd")));
  };
  var dataType = (name2) => {
    const $1 = arrayMap((v) => $Tuple(v._1, v._2));
    const $2 = DataType(name2);
    return (x2) => $2(fromFoldable3($1(x2)));
  };
  var dataTypes = /* @__PURE__ */ foldrArray(Cons)(Nil)([
    /* @__PURE__ */ dataType("Bool")([/* @__PURE__ */ $Tuple("True", 0), /* @__PURE__ */ $Tuple("False", 0)]),
    /* @__PURE__ */ dataType("List")([/* @__PURE__ */ $Tuple("Nil", 0), /* @__PURE__ */ $Tuple(":", 2)]),
    /* @__PURE__ */ dataType("Option")([/* @__PURE__ */ $Tuple("None", 0), /* @__PURE__ */ $Tuple("Some", 1)]),
    /* @__PURE__ */ dataType("Ordering")([/* @__PURE__ */ $Tuple("GT", 0), /* @__PURE__ */ $Tuple("LT", 0), /* @__PURE__ */ $Tuple("EQ", 0)]),
    /* @__PURE__ */ dataType("Pair")([/* @__PURE__ */ $Tuple("Pair", 2)]),
    /* @__PURE__ */ dataType("Tree")([/* @__PURE__ */ $Tuple("Empty", 0), /* @__PURE__ */ $Tuple("NonEmpty", 3)]),
    /* @__PURE__ */ dataType("Point")([/* @__PURE__ */ $Tuple("Point", 2)]),
    /* @__PURE__ */ dataType("Orient")([/* @__PURE__ */ $Tuple("Horiz", 0), /* @__PURE__ */ $Tuple("Vert", 0)]),
    /* @__PURE__ */ dataType("Plot")([
      /* @__PURE__ */ $Tuple("BarChart", 1),
      /* @__PURE__ */ $Tuple("LineChart", 1),
      /* @__PURE__ */ $Tuple("LinePlot", 1)
    ]),
    /* @__PURE__ */ dataType("GraphicsElement")([
      /* @__PURE__ */ $Tuple("Circle", 4),
      /* @__PURE__ */ $Tuple("Group", 1),
      /* @__PURE__ */ $Tuple("Line", 4),
      /* @__PURE__ */ $Tuple("Polyline", 3),
      /* @__PURE__ */ $Tuple("Polymarkers", 2),
      /* @__PURE__ */ $Tuple("Rect", 5),
      /* @__PURE__ */ $Tuple("Text", 5),
      /* @__PURE__ */ $Tuple("Viewport", 9)
    ]),
    /* @__PURE__ */ dataType("Transform")([/* @__PURE__ */ $Tuple("Scale", 2), /* @__PURE__ */ $Tuple("Translate", 2)]),
    /* @__PURE__ */ dataType("Marker")([/* @__PURE__ */ $Tuple("Arrowhead", 0)])
  ]);
  var ctrToDataType = /* @__PURE__ */ (() => fromFoldable2(foldableList)(bindList.bind(listMap((d) => listMap((v) => $Tuple(
    v,
    d
  ))(toUnfoldable6(fromFoldable12(keys2(d._2)))))(dataTypes))(identity4)))();
  var dataTypeForCtr = {
    dataTypeFor: (c) => {
      const $1 = "Unknown constructor " + showCtr(c);
      const $2 = _lookup(Nothing, Just, c, ctrToDataType);
      if ($2.tag === "Nothing") {
        return $Either("Left", $1);
      }
      if ($2.tag === "Just") {
        return $Either("Right", $2._1);
      }
      fail();
    }
  };
  var consistentWith = (cs) => (cs$p) => {
    const $2 = bindEither.bind((() => {
      const v = toUnfoldable6(cs$p);
      if (v.tag === "Cons") {
        return dataTypeForCtr.dataTypeFor(v._1);
      }
      fail();
    })())((d) => bindEither.bind((() => {
      const v = toUnfoldable6(cs$p);
      if (v.tag === "Cons") {
        return dataTypeForCtr.dataTypeFor(v._1);
      }
      fail();
    })())((d$p) => $$with("constructors of " + (d$p._1 + (" do not include " + show(map(ordString)(showCtr)(cs)))))(mayFailEq(showDataType$pInt)(eqDataType$pInt)(d)(d$p))));
    if ($2.tag === "Left") {
      return $Either("Left", $2._1);
    }
    if ($2.tag === "Right") {
      return $Either("Right", unit);
    }
    fail();
  };
  var arity = (c) => bindEither.bind(dataTypeForCtr.dataTypeFor(c))((v) => {
    const $2 = _lookup(Nothing, Just, c, v._2);
    if ($2.tag === "Nothing") {
      return $Either("Left", "absurd");
    }
    if ($2.tag === "Just") {
      return $Either("Right", $2._1);
    }
    fail();
  });
  var checkArity = (c) => (n) => {
    const $2 = $$with("Checking arity of " + showCtr(c))(bind2Flipped2(mayFailEq(showInt)(eqInt))(arity(c))($Either("Right", n)));
    if ($2.tag === "Left") {
      return $Either("Left", $2._1);
    }
    if ($2.tag === "Right") {
      return $Either("Right", unit);
    }
    fail();
  };

  // output-es/Data.Semiring/foreign.js
  var intAdd = function(x2) {
    return function(y2) {
      return x2 + y2 | 0;
    };
  };
  var intMul = function(x2) {
    return function(y2) {
      return x2 * y2 | 0;
    };
  };
  var numAdd = function(n1) {
    return function(n2) {
      return n1 + n2;
    };
  };
  var numMul = function(n1) {
    return function(n2) {
      return n1 * n2;
    };
  };

  // output-es/Data.Ring/foreign.js
  var intSub = function(x2) {
    return function(y2) {
      return x2 - y2 | 0;
    };
  };
  var numSub = function(n1) {
    return function(n2) {
      return n1 - n2;
    };
  };

  // output-es/Data.EuclideanRing/foreign.js
  var intDiv = function(x2) {
    return function(y2) {
      if (y2 === 0)
        return 0;
      return y2 > 0 ? Math.floor(x2 / y2) : -Math.floor(x2 / -y2);
    };
  };
  var intMod = function(x2) {
    return function(y2) {
      if (y2 === 0)
        return 0;
      var yy = Math.abs(y2);
      return (x2 % yy + yy) % yy;
    };
  };
  var numDiv = function(n1) {
    return function(n2) {
      return n1 / n2;
    };
  };

  // output-es/Data.String.Common/foreign.js
  var split = function(sep) {
    return function(s) {
      return s.split(sep);
    };
  };
  var toLower = function(s) {
    return s.toLowerCase();
  };
  var joinWith = function(s) {
    return function(xs) {
      return xs.join(s);
    };
  };

  // output-es/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _codePointAt = function(fallback) {
    return function(Just2) {
      return function(Nothing2) {
        return function(unsafeCodePointAt02) {
          return function(index3) {
            return function(str) {
              var length6 = str.length;
              if (index3 < 0 || index3 >= length6)
                return Nothing2;
              if (hasStringIterator) {
                var iter = str[Symbol.iterator]();
                for (var i = index3; ; --i) {
                  var o = iter.next();
                  if (o.done)
                    return Nothing2;
                  if (i === 0)
                    return Just2(unsafeCodePointAt02(o.value));
                }
              }
              return fallback(index3)(str);
            };
          };
        };
      };
    };
  };
  var _fromCodePointArray = function(singleton3) {
    return hasFromCodePoint ? function(cps) {
      if (cps.length < 1e4) {
        return String.fromCodePoint.apply(String, cps);
      }
      return cps.map(singleton3).join("");
    } : function(cps) {
      return cps.map(singleton3).join("");
    };
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output-es/Data.String.CodePoints/index.js
  var uncons3 = (s) => {
    const v = length3(s);
    if (v === 0) {
      return Nothing;
    }
    if (v === 1) {
      return $Maybe("Just", { head: toCharCode(charAt(0)(s)), tail: "" });
    }
    const cu1 = toCharCode(charAt(1)(s));
    const cu0 = toCharCode(charAt(0)(s));
    if (55296 <= cu0 && cu0 <= 56319 && (56320 <= cu1 && cu1 <= 57343)) {
      return $Maybe("Just", { head: (((cu0 - 55296 | 0) * 1024 | 0) + (cu1 - 56320 | 0) | 0) + 65536 | 0, tail: drop(2)(s) });
    }
    return $Maybe("Just", { head: cu0, tail: drop(1)(s) });
  };
  var unconsButWithTuple = (s) => {
    const $1 = uncons3(s);
    if ($1.tag === "Just") {
      return $Maybe("Just", $Tuple($1._1.head, $1._1.tail));
    }
    return Nothing;
  };
  var toCodePointArrayFallback = (s) => unfoldableArray.unfoldr(unconsButWithTuple)(s);
  var unsafeCodePointAt0Fallback = (s) => {
    const cu0 = toCharCode(charAt(0)(s));
    if (55296 <= cu0 && cu0 <= 56319 && length3(s) > 1) {
      const cu1 = toCharCode(charAt(1)(s));
      if (56320 <= cu1 && cu1 <= 57343) {
        return (((cu0 - 55296 | 0) * 1024 | 0) + (cu1 - 56320 | 0) | 0) + 65536 | 0;
      }
      return cu0;
    }
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length4 = (x2) => toCodePointArray(x2).length;
  var fromCharCode2 = (x2) => singleton((() => {
    if (x2 >= -2147483648 && x2 <= 2147483647) {
      return fromCharCode(x2);
    }
    if (x2 < 0) {
      return "\0";
    }
    return "\uFFFF";
  })());
  var singletonFallback = (v) => {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    return fromCharCode2(intDiv(v - 65536 | 0)(1024) + 55296 | 0) + fromCharCode2(intMod(v - 65536 | 0)(1024) + 56320 | 0);
  };
  var fromCodePointArray = /* @__PURE__ */ _fromCodePointArray(singletonFallback);
  var codePointAtFallback = (codePointAtFallback$a0$copy) => (codePointAtFallback$a1$copy) => {
    let codePointAtFallback$a0 = codePointAtFallback$a0$copy, codePointAtFallback$a1 = codePointAtFallback$a1$copy, codePointAtFallback$c = true, codePointAtFallback$r;
    while (codePointAtFallback$c) {
      const n = codePointAtFallback$a0, s = codePointAtFallback$a1;
      const v = uncons3(s);
      if (v.tag === "Just") {
        if (n === 0) {
          codePointAtFallback$c = false;
          codePointAtFallback$r = $Maybe("Just", v._1.head);
          continue;
        }
        codePointAtFallback$a0 = n - 1 | 0;
        codePointAtFallback$a1 = v._1.tail;
        continue;
      }
      codePointAtFallback$c = false;
      codePointAtFallback$r = Nothing;
      continue;
    }
    ;
    return codePointAtFallback$r;
  };
  var codePointAt = (v) => (v1) => {
    if (v < 0) {
      return Nothing;
    }
    if (v === 0) {
      if (v1 === "") {
        return Nothing;
      }
      return $Maybe("Just", unsafeCodePointAt0(v1));
    }
    return _codePointAt(codePointAtFallback)(Just)(Nothing)(unsafeCodePointAt0)(v)(v1);
  };

  // output-es/Text.Pretty/index.js
  var max2 = (x2) => (y2) => {
    const v = ordInt.compare(x2)(y2);
    if (v.tag === "LT") {
      return y2;
    }
    if (v.tag === "EQ") {
      return x2;
    }
    if (v.tag === "GT") {
      return x2;
    }
    fail();
  };
  var intercalate3 = (sep) => (xs) => foldlArray((v) => (x2) => {
    if (v.init) {
      return { init: false, acc: x2 };
    }
    return { init: false, acc: v.acc + (sep + x2) };
  })({ init: true, acc: "" })(xs).acc;
  var text = (s) => {
    const lines = split("\n")(s);
    return { width: foldlArray(max2)(0)(arrayMap(length4)(lines)), height: lines.length, lines };
  };
  var empty2 = (w) => (h) => ({
    width: w,
    height: h,
    lines: (() => {
      if (h === 0) {
        return [];
      }
      return arrayMap((v) => "")(range2(1)(h));
    })()
  });
  var beside = (v) => (v1) => {
    const height_ = max2(v.height)(v1.height);
    const adjust = (d) => concatArray(d.lines)(replicate(unfoldableArray)(height_ - d.height | 0)(fromCharArray(replicate(unfoldableArray)(d.width)(" "))));
    return {
      width: v.width + v1.width | 0,
      height: height_,
      lines: (() => {
        const $4 = zipWith2(concatString)(arrayMap((s) => s + fromCharArray(replicate(unfoldableArray)(v.width - toCodePointArray(s).length | 0)(" ")))(adjust(v)))(adjust(v1));
        if (height_ < 1) {
          return [];
        }
        return slice(0)(height_)($4);
      })()
    };
  };
  var semigroupColumns = { append: (v) => (v1) => beside(v)(v1) };
  var monoidColumns = { mempty: /* @__PURE__ */ empty2(0)(0), Semigroup0: () => semigroupColumns };
  var atop = (v) => (v1) => ({ width: max2(v.width)(v1.width), height: v.height + v1.height | 0, lines: concatArray(v.lines)(v1.lines) });

  // output-es/Expr/index.js
  var $Cont = (tag, _1) => ({ tag, _1 });
  var $Elim = (tag, _1, _2) => ({ tag, _1, _2 });
  var $Expr = (tag, _1, _2, _3, _4) => ({ tag, _1, _2, _3, _4 });
  var $Module = (_1) => ({ tag: "Module", _1 });
  var $VarDef = (_1, _2) => ({ tag: "VarDef", _1, _2 });
  var showSet2 = /* @__PURE__ */ showSet(showString);
  var eqSet = /* @__PURE__ */ (() => {
    const eq2 = eqMap(eqString)(eqUnit).eq;
    return { eq: (v) => (v1) => eq2(v)(v1) };
  })();
  var unions = /* @__PURE__ */ fold((z) => (v) => union2(ordString)(z))(Leaf2);
  var fromFoldable4 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)(unit)(m))(Leaf2))();
  var unions1 = /* @__PURE__ */ (() => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, v = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = unionWith(ordString)($$const)(b)(v._1);
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      ;
      return go$r;
    };
    return go(Leaf2);
  })();
  var ContNone = /* @__PURE__ */ $Cont("ContNone");
  var Int = (value0) => (value1) => $Expr("Int", value0, value1);
  var Float = (value0) => (value1) => $Expr("Float", value0, value1);
  var Str = (value0) => (value1) => $Expr("Str", value0, value1);
  var Record = (value0) => (value1) => $Expr("Record", value0, value1);
  var Dictionary = (value0) => (value1) => $Expr("Dictionary", value0, value1);
  var Constr = (value0) => (value1) => (value2) => $Expr("Constr", value0, value1, value2);
  var Matrix = (value0) => (value1) => (value2) => (value3) => $Expr("Matrix", value0, value1, value2, value3);
  var Project = (value0) => (value1) => $Expr("Project", value0, value1);
  var App2 = (value0) => (value1) => $Expr("App", value0, value1);
  var Let = (value0) => (value1) => $Expr("Let", value0, value1);
  var LetRec = (value0) => (value1) => $Expr("LetRec", value0, value1);
  var Sugar = (value0) => (value1) => $Expr("Sugar", value0, value1);
  var ElimVar = (value0) => (value1) => $Elim("ElimVar", value0, value1);
  var ElimRecord = (value0) => (value1) => $Elim("ElimRecord", value0, value1);
  var ElimSug = (value0) => (value1) => $Elim("ElimSug", value0, value1);
  var VarDef = (value0) => (value1) => $VarDef(value0, value1);
  var functorVarDef = { map: (f) => (m) => $VarDef(functorElim.map(f)(m._1), functorExpr.map(f)(m._2)) };
  var functorExpr = {
    map: (f) => (m) => {
      if (m.tag === "Var") {
        return $Expr("Var", m._1);
      }
      if (m.tag === "Op") {
        return $Expr("Op", m._1);
      }
      if (m.tag === "Int") {
        return $Expr("Int", f(m._1), m._2);
      }
      if (m.tag === "Float") {
        return $Expr("Float", f(m._1), m._2);
      }
      if (m.tag === "Str") {
        return $Expr("Str", f(m._1), m._2);
      }
      if (m.tag === "Record") {
        return $Expr("Record", f(m._1), _fmapObject(m._2, functorExpr.map(f)));
      }
      if (m.tag === "Dictionary") {
        return $Expr("Dictionary", f(m._1), listMap(functorPair.map(functorExpr.map(f)))(m._2));
      }
      if (m.tag === "Constr") {
        return $Expr("Constr", f(m._1), m._2, listMap(functorExpr.map(f))(m._3));
      }
      if (m.tag === "Matrix") {
        return $Expr("Matrix", f(m._1), functorExpr.map(f)(m._2), m._3, functorExpr.map(f)(m._4));
      }
      if (m.tag === "Lambda") {
        return $Expr("Lambda", functorElim.map(f)(m._1));
      }
      if (m.tag === "Project") {
        return $Expr("Project", functorExpr.map(f)(m._1), m._2);
      }
      if (m.tag === "App") {
        return $Expr("App", functorExpr.map(f)(m._1), functorExpr.map(f)(m._2));
      }
      if (m.tag === "Let") {
        return $Expr("Let", functorVarDef.map(f)(m._1), functorExpr.map(f)(m._2));
      }
      if (m.tag === "LetRec") {
        return $Expr("LetRec", _fmapObject(m._1, functorElim.map(f)), functorExpr.map(f)(m._2));
      }
      if (m.tag === "Sugar") {
        return $Expr("Sugar", m._1, functorExpr.map(f)(m._2));
      }
      fail();
    }
  };
  var functorElim = {
    map: (f) => (m) => {
      if (m.tag === "ElimVar") {
        return $Elim("ElimVar", m._1, functorCont.map(f)(m._2));
      }
      if (m.tag === "ElimConstr") {
        return $Elim("ElimConstr", _fmapObject(m._1, functorCont.map(f)));
      }
      if (m.tag === "ElimRecord") {
        return $Elim("ElimRecord", m._1, functorCont.map(f)(m._2));
      }
      if (m.tag === "ElimSug") {
        return $Elim("ElimSug", m._1, functorElim.map(f)(m._2));
      }
      fail();
    }
  };
  var functorCont = {
    map: (f) => (m) => {
      if (m.tag === "ContNone") {
        return ContNone;
      }
      if (m.tag === "ContExpr") {
        return $Cont("ContExpr", functorExpr.map(f)(m._1));
      }
      if (m.tag === "ContElim") {
        return $Cont("ContElim", functorElim.map(f)(m._1));
      }
      fail();
    }
  };
  var joinSemilatticeVarDef = (dictJoinSemilattice) => ({
    join: (def) => definedJoin(joinSemilatticeVarDef(dictJoinSemilattice))(def),
    maybeJoin: (v) => (v1) => applyEither.apply((() => {
      const $3 = joinSemilatticeElim(dictJoinSemilattice).maybeJoin(v._1)(v1._1);
      if ($3.tag === "Left") {
        return $Either("Left", $3._1);
      }
      if ($3.tag === "Right") {
        return $Either("Right", VarDef($3._1));
      }
      fail();
    })())(joinSemilatticeExpr(dictJoinSemilattice).maybeJoin(v._2)(v1._2)),
    neg: functorVarDef.map(dictJoinSemilattice.neg)
  });
  var joinSemilatticeExpr = (dictJoinSemilattice) => ({
    maybeJoin: (v) => (v1) => {
      if (v.tag === "Var") {
        if (v1.tag === "Var") {
          const $3 = mayFailEq(showString)(eqString)(v._1)(v1._1);
          if ($3.tag === "Left") {
            return $Either("Left", $3._1);
          }
          if ($3.tag === "Right") {
            return $Either("Right", $Expr("Var", $3._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "Op") {
        if (v1.tag === "Op") {
          const $3 = mayFailEq(showString)(eqString)(v._1)(v1._1);
          if ($3.tag === "Left") {
            return $Either("Left", $3._1);
          }
          if ($3.tag === "Right") {
            return $Either("Right", $Expr("Op", $3._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "Int") {
        if (v1.tag === "Int") {
          const $3 = Int(dictJoinSemilattice.join(v._1)(v1._1));
          const $4 = mayFailEq(showInt)(eqInt)(v._2)(v1._2);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "Str") {
        if (v1.tag === "Str") {
          const $3 = Str(dictJoinSemilattice.join(v._1)(v1._1));
          const $4 = mayFailEq(showString)(eqString)(v._2)(v1._2);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "Float") {
        if (v1.tag === "Float") {
          const $3 = Float(dictJoinSemilattice.join(v._1)(v1._1));
          const $4 = mayFailEq(showNumber)(eqNumber)(v._2)(v1._2);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "Record") {
        if (v1.tag === "Record") {
          const $3 = Record(dictJoinSemilattice.join(v._1)(v1._1));
          const $4 = joinSemilatticeDict(joinSemilatticeExpr(dictJoinSemilattice)).maybeJoin(v._2)(v1._2);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "Dictionary") {
        if (v1.tag === "Dictionary") {
          const $3 = Dictionary(dictJoinSemilattice.join(v._1)(v1._1));
          const $4 = joinSemilatticeList(joinSemilatticePair(joinSemilatticeExpr(dictJoinSemilattice))).maybeJoin(v._2)(v1._2);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "Constr") {
        if (v1.tag === "Constr") {
          return applyEither.apply((() => {
            const $3 = Constr(dictJoinSemilattice.join(v._1)(v1._1));
            const $4 = mayFailEq(showString)(eqString)(v._2)(v1._2);
            if ($4.tag === "Left") {
              return $Either("Left", $4._1);
            }
            if ($4.tag === "Right") {
              return $Either("Right", $3($4._1));
            }
            fail();
          })())(joinSemilatticeList(joinSemilatticeExpr(dictJoinSemilattice)).maybeJoin(v._3)(v1._3));
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "Matrix") {
        if (v1.tag === "Matrix") {
          return applyEither.apply(applyEither.apply((() => {
            const $3 = Matrix(dictJoinSemilattice.join(v._1)(v1._1));
            const $4 = joinSemilatticeExpr(dictJoinSemilattice).maybeJoin(v._2)(v1._2);
            if ($4.tag === "Left") {
              return $Either("Left", $4._1);
            }
            if ($4.tag === "Right") {
              return $Either("Right", $3($4._1));
            }
            fail();
          })())((() => {
            const $3 = mayFailEq(showString)(eqString)(v._3._1)(v1._3._1);
            return applyEither.apply((() => {
              if ($3.tag === "Left") {
                return $Either("Left", $3._1);
              }
              if ($3.tag === "Right") {
                return $Either("Right", Tuple($3._1));
              }
              fail();
            })())(mayFailEq(showString)(eqString)(v._3._2)(v1._3._2));
          })()))(joinSemilatticeExpr(dictJoinSemilattice).maybeJoin(v._4)(v1._4));
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "Lambda") {
        if (v1.tag === "Lambda") {
          const $3 = joinSemilatticeElim(dictJoinSemilattice).maybeJoin(v._1)(v1._1);
          if ($3.tag === "Left") {
            return $Either("Left", $3._1);
          }
          if ($3.tag === "Right") {
            return $Either("Right", $Expr("Lambda", $3._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "Project") {
        if (v1.tag === "Project") {
          return applyEither.apply((() => {
            const $3 = joinSemilatticeExpr(dictJoinSemilattice).maybeJoin(v._1)(v1._1);
            if ($3.tag === "Left") {
              return $Either("Left", $3._1);
            }
            if ($3.tag === "Right") {
              return $Either("Right", Project($3._1));
            }
            fail();
          })())(mayFailEq(showString)(eqString)(v._2)(v1._2));
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "App") {
        if (v1.tag === "App") {
          return applyEither.apply((() => {
            const $3 = joinSemilatticeExpr(dictJoinSemilattice).maybeJoin(v._1)(v1._1);
            if ($3.tag === "Left") {
              return $Either("Left", $3._1);
            }
            if ($3.tag === "Right") {
              return $Either("Right", App2($3._1));
            }
            fail();
          })())(joinSemilatticeExpr(dictJoinSemilattice).maybeJoin(v._2)(v1._2));
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "Let") {
        if (v1.tag === "Let") {
          return applyEither.apply((() => {
            const $3 = joinSemilatticeVarDef(dictJoinSemilattice).maybeJoin(v._1)(v1._1);
            if ($3.tag === "Left") {
              return $Either("Left", $3._1);
            }
            if ($3.tag === "Right") {
              return $Either("Right", Let($3._1));
            }
            fail();
          })())(joinSemilatticeExpr(dictJoinSemilattice).maybeJoin(v._2)(v1._2));
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "LetRec") {
        if (v1.tag === "LetRec") {
          return applyEither.apply((() => {
            const $3 = joinSemilatticeDict(joinSemilatticeElim(dictJoinSemilattice)).maybeJoin(v._1)(v1._1);
            if ($3.tag === "Left") {
              return $Either("Left", $3._1);
            }
            if ($3.tag === "Right") {
              return $Either("Right", LetRec($3._1));
            }
            fail();
          })())(joinSemilatticeExpr(dictJoinSemilattice).maybeJoin(v._2)(v1._2));
        }
        return $Either("Left", "Incompatible expressions");
      }
      if (v.tag === "Sugar") {
        if (v1.tag === "Sugar") {
          const $3 = Sugar(v._1);
          const $4 = joinSemilatticeExpr(dictJoinSemilattice).maybeJoin(v._2)(v1._2);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible expressions");
      }
      return $Either("Left", "Incompatible expressions");
    },
    join: (e) => definedJoin(joinSemilatticeExpr(dictJoinSemilattice))(e),
    neg: functorExpr.map(dictJoinSemilattice.neg)
  });
  var joinSemilatticeElim = (dictJoinSemilattice) => ({
    maybeJoin: (v) => (v1) => {
      if (v.tag === "ElimVar") {
        if (v1.tag === "ElimVar") {
          return applyEither.apply((() => {
            const $3 = mayFailEq(showString)(eqString)(v._1)(v1._1);
            if ($3.tag === "Left") {
              return $Either("Left", $3._1);
            }
            if ($3.tag === "Right") {
              return $Either("Right", ElimVar($3._1));
            }
            fail();
          })())(joinSemilatticeCont(dictJoinSemilattice).maybeJoin(v._2)(v1._2));
        }
        return $Either("Left", "Incompatible eliminators");
      }
      if (v.tag === "ElimConstr") {
        if (v1.tag === "ElimConstr") {
          const $3 = consistentWith(keys2(v._1))(keys2(v1._1));
          const $4 = applyEither.apply((() => {
            if ($3.tag === "Left") {
              return $Either("Left", $3._1);
            }
            if ($3.tag === "Right") {
              return $Either("Right", identity);
            }
            fail();
          })())(joinSemilatticeDict(joinSemilatticeCont(dictJoinSemilattice)).maybeJoin(v._1)(v1._1));
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $Elim("ElimConstr", $4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible eliminators");
      }
      if (v.tag === "ElimRecord") {
        if (v1.tag === "ElimRecord") {
          return applyEither.apply((() => {
            const $3 = mayFailEq(showSet2)(eqSet)(v._1)(v1._1);
            if ($3.tag === "Left") {
              return $Either("Left", $3._1);
            }
            if ($3.tag === "Right") {
              return $Either("Right", ElimRecord($3._1));
            }
            fail();
          })())(joinSemilatticeCont(dictJoinSemilattice).maybeJoin(v._2)(v1._2));
        }
        return $Either("Left", "Incompatible eliminators");
      }
      if (v.tag === "ElimSug") {
        if (v1.tag === "ElimSug") {
          const $3 = ElimSug(v._1);
          const $4 = joinSemilatticeElim(dictJoinSemilattice).maybeJoin(v._2)(v1._2);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible eliminators");
      }
      return $Either("Left", "Incompatible eliminators");
    },
    join: (\u03C3) => definedJoin(joinSemilatticeElim(dictJoinSemilattice))(\u03C3),
    neg: functorElim.map(dictJoinSemilattice.neg)
  });
  var joinSemilatticeCont = (dictJoinSemilattice) => ({
    maybeJoin: (v) => (v1) => {
      if (v.tag === "ContNone") {
        if (v1.tag === "ContNone") {
          return $Either("Right", ContNone);
        }
        return $Either("Left", "Incompatible continuations");
      }
      if (v.tag === "ContExpr") {
        if (v1.tag === "ContExpr") {
          const $3 = joinSemilatticeExpr(dictJoinSemilattice).maybeJoin(v._1)(v1._1);
          if ($3.tag === "Left") {
            return $Either("Left", $3._1);
          }
          if ($3.tag === "Right") {
            return $Either("Right", $Cont("ContExpr", $3._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible continuations");
      }
      if (v.tag === "ContElim") {
        if (v1.tag === "ContElim") {
          const $3 = joinSemilatticeElim(dictJoinSemilattice).maybeJoin(v._1)(v1._1);
          if ($3.tag === "Left") {
            return $Either("Left", $3._1);
          }
          if ($3.tag === "Right") {
            return $Either("Right", $Cont("ContElim", $3._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible continuations");
      }
      return $Either("Left", "Incompatible continuations");
    },
    join: (\u03BA) => definedJoin(joinSemilatticeCont(dictJoinSemilattice))(\u03BA),
    neg: functorCont.map(dictJoinSemilattice.neg)
  });
  var fromSugarExpr = {
    fromSug: Sugar,
    toSug: (v) => {
      if (v.tag === "Sugar") {
        return $Tuple(v._1, v._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
  };
  var expandableVarDefRawVarDef = (dictBoundedJoinSemilattice) => ({ expand: (v) => (v1) => $VarDef(expandableElimRawElim(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2)) });
  var expandableExprRawExpr = (dictBoundedJoinSemilattice) => ({
    expand: (v) => (v1) => {
      if (v.tag === "Var") {
        if (v1.tag === "Var") {
          return $Expr("Var", mustEq(eqString)(showString)(v._1)(v1._1));
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "Op") {
        if (v1.tag === "Op") {
          return $Expr("Op", mustEq(eqString)(showString)(v._1)(v1._1));
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "Int") {
        if (v1.tag === "Int") {
          return $Expr("Int", v._1, mustEq(eqInt)(showInt)(v._2)(v1._2));
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "Str") {
        if (v1.tag === "Str") {
          return $Expr("Str", v._1, mustEq(eqString)(showString)(v._2)(v1._2));
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "Float") {
        if (v1.tag === "Float") {
          return $Expr("Float", v._1, mustEq(eqNumber)(showNumber)(v._2)(v1._2));
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "Record") {
        if (v1.tag === "Record") {
          return $Expr(
            "Record",
            v._1,
            expandableDictDict({ botOf: functorExpr.map((v$1) => dictBoundedJoinSemilattice.bot) })(expandableExprRawExpr(dictBoundedJoinSemilattice)).expand(v._2)(v1._2)
          );
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "Dictionary") {
        if (v1.tag === "Dictionary") {
          return $Expr("Dictionary", v._1, zipWith(expandablePairPair(expandableExprRawExpr(dictBoundedJoinSemilattice)).expand)(v._2)(v1._2));
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "Constr") {
        if (v1.tag === "Constr") {
          return $Expr(
            "Constr",
            v._1,
            mustEq(eqString)(showString)(v._2)(v1._2),
            zipWith(expandableExprRawExpr(dictBoundedJoinSemilattice).expand)(v._3)(v1._3)
          );
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "Matrix") {
        if (v1.tag === "Matrix") {
          return $Expr(
            "Matrix",
            v._1,
            expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2),
            $Tuple(mustEq(eqString)(showString)(v._3._1)(v1._3._1), mustEq(eqString)(showString)(v._3._2)(v1._3._2)),
            expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._4)(v1._4)
          );
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "Lambda") {
        if (v1.tag === "Lambda") {
          return $Expr("Lambda", expandableElimRawElim(dictBoundedJoinSemilattice).expand(v._1)(v1._1));
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "Project") {
        if (v1.tag === "Project") {
          return $Expr("Project", expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._1)(v1._1), mustEq(eqString)(showString)(v._2)(v1._2));
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "App") {
        if (v1.tag === "App") {
          return $Expr("App", expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "Let") {
        if (v1.tag === "Let") {
          return $Expr("Let", expandableVarDefRawVarDef(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "LetRec") {
        if (v1.tag === "LetRec") {
          return $Expr(
            "LetRec",
            expandableDictDict({ botOf: functorElim.map((v$1) => dictBoundedJoinSemilattice.bot) })(expandableElimRawElim(dictBoundedJoinSemilattice)).expand(v._1)(v1._1),
            expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2)
          );
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      if (v.tag === "Sugar") {
        if (v1.tag === "Sugar") {
          return $Expr("Sugar", v._1, expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
        }
        return unsafePerformEffect(throwException(error("Incompatible expressions")));
      }
      return unsafePerformEffect(throwException(error("Incompatible expressions")));
    }
  });
  var expandableElimRawElim = (dictBoundedJoinSemilattice) => ({
    expand: (v) => (v1) => {
      if (v.tag === "ElimVar") {
        if (v1.tag === "ElimVar") {
          return $Elim("ElimVar", mustEq(eqString)(showString)(v._1)(v1._1), expandableContRawCont(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
        }
        return unsafePerformEffect(throwException(error("Incompatible eliminators")));
      }
      if (v.tag === "ElimConstr") {
        if (v1.tag === "ElimConstr") {
          return $Elim(
            "ElimConstr",
            expandableDictDict({ botOf: functorCont.map((v$1) => dictBoundedJoinSemilattice.bot) })(expandableContRawCont(dictBoundedJoinSemilattice)).expand(v._1)(v1._1)
          );
        }
        return unsafePerformEffect(throwException(error("Incompatible eliminators")));
      }
      if (v.tag === "ElimRecord") {
        if (v1.tag === "ElimRecord") {
          return $Elim("ElimRecord", mustEq(eqSet)(showSet2)(v._1)(v1._1), expandableContRawCont(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
        }
        return unsafePerformEffect(throwException(error("Incompatible eliminators")));
      }
      if (v.tag === "ElimSug") {
        if (v1.tag === "ElimSug") {
          return $Elim("ElimSug", v._1, expandableElimRawElim(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
        }
        return unsafePerformEffect(throwException(error("Incompatible eliminators")));
      }
      return unsafePerformEffect(throwException(error("Incompatible eliminators")));
    }
  });
  var expandableContRawCont = (dictBoundedJoinSemilattice) => ({
    expand: (v) => (v1) => {
      if (v.tag === "ContNone") {
        if (v1.tag === "ContNone") {
          return ContNone;
        }
        return unsafePerformEffect(throwException(error("Incompatible continuations")));
      }
      if (v.tag === "ContExpr") {
        if (v1.tag === "ContExpr") {
          return $Cont("ContExpr", expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._1)(v1._1));
        }
        return unsafePerformEffect(throwException(error("Incompatible continuations")));
      }
      if (v.tag === "ContElim") {
        if (v1.tag === "ContElim") {
          return $Cont("ContElim", expandableElimRawElim(dictBoundedJoinSemilattice).expand(v._1)(v1._1));
        }
        return unsafePerformEffect(throwException(error("Incompatible continuations")));
      }
      return unsafePerformEffect(throwException(error("Incompatible continuations")));
    }
  });
  var fVDict = (dictFV) => ({ fv: (\u03C1) => difference3(ordString)(unions(_fmapObject(\u03C1, dictFV.fv)))(fromFoldable4(keys2(\u03C1))) });
  var bVElim = {
    bv: (v) => {
      if (v.tag === "ElimVar") {
        return unionWith(ordString)($$const)($Map(
          "Two",
          Leaf2,
          v._1,
          unit,
          Leaf2
        ))(bVCont.bv(v._2));
      }
      if (v.tag === "ElimConstr") {
        return bVCont.bv(asSingletonMap(v._1)._2);
      }
      if (v.tag === "ElimRecord") {
        return bVCont.bv(v._2);
      }
      if (v.tag === "ElimSug") {
        return bVElim.bv(v._2);
      }
      fail();
    }
  };
  var bVCont = {
    bv: (v) => {
      if (v.tag === "ContNone") {
        return Leaf2;
      }
      if (v.tag === "ContElim") {
        return bVElim.bv(v._1);
      }
      if (v.tag === "ContExpr") {
        return Leaf2;
      }
      fail();
    }
  };
  var fVExpr = {
    fv: (v) => {
      if (v.tag === "Var") {
        return $Map("Two", Leaf2, v._1, unit, Leaf2);
      }
      if (v.tag === "Op") {
        return $Map("Two", Leaf2, v._1, unit, Leaf2);
      }
      if (v.tag === "Int") {
        return Leaf2;
      }
      if (v.tag === "Float") {
        return Leaf2;
      }
      if (v.tag === "Str") {
        return Leaf2;
      }
      if (v.tag === "Record") {
        return unions(_fmapObject(v._2, fVExpr.fv));
      }
      if (v.tag === "Dictionary") {
        return unions1(listMap((x2) => unionWith(ordString)($$const)(Leaf2)(fVExpr.fv(x2._2)))(v._2));
      }
      if (v.tag === "Constr") {
        return unions1(listMap(fVExpr.fv)(v._3));
      }
      if (v.tag === "Matrix") {
        return unionWith(ordString)($$const)(fVExpr.fv(v._2))(fVExpr.fv(v._4));
      }
      if (v.tag === "Lambda") {
        return fVElim.fv(v._1);
      }
      if (v.tag === "Project") {
        return fVExpr.fv(v._1);
      }
      if (v.tag === "App") {
        return unionWith(ordString)($$const)(fVExpr.fv(v._1))(fVExpr.fv(v._2));
      }
      if (v.tag === "Let") {
        return unionWith(ordString)($$const)(fVExpr.fv(v._1._2))(difference3(ordString)(fVExpr.fv(v._2))(bVElim.bv(v._1._1)));
      }
      if (v.tag === "LetRec") {
        return unionWith(ordString)($$const)(unions(_fmapObject(v._1, fVElim.fv)))(fVExpr.fv(v._2));
      }
      if (v.tag === "Sugar") {
        return fVExpr.fv(v._2);
      }
      fail();
    }
  };
  var fVElim = {
    fv: (v) => {
      if (v.tag === "ElimVar") {
        return difference3(ordString)(fVCont.fv(v._2))($Map("Two", Leaf2, v._1, unit, Leaf2));
      }
      if (v.tag === "ElimConstr") {
        return unions(_fmapObject(v._1, fVCont.fv));
      }
      if (v.tag === "ElimRecord") {
        return fVCont.fv(v._2);
      }
      if (v.tag === "ElimSug") {
        return fVElim.fv(v._2);
      }
      fail();
    }
  };
  var fVCont = {
    fv: (v) => {
      if (v.tag === "ContNone") {
        return Leaf2;
      }
      if (v.tag === "ContElim") {
        return fVElim.fv(v._1);
      }
      if (v.tag === "ContExpr") {
        return fVExpr.fv(v._1);
      }
      fail();
    }
  };

  // output-es/Val/index.js
  var $ForeignOp$p = (_1) => ({ tag: "ForeignOp'", _1 });
  var $Fun = (tag, _1, _2, _3, _4) => ({ tag, _1, _2, _3, _4 });
  var $Val = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
  var identity12 = (x2) => x2;
  var fromFoldable5 = /* @__PURE__ */ foldlArray((m) => (a) => insert(ordString)(a)(unit)(m))(Leaf2);
  var toUnfoldable7 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
  var intersection2 = /* @__PURE__ */ intersection(ordString);
  var Int2 = (value0) => (value1) => $Val("Int", value0, value1);
  var Float2 = (value0) => (value1) => $Val("Float", value0, value1);
  var Str2 = (value0) => (value1) => $Val("Str", value0, value1);
  var Constr2 = (value0) => (value1) => (value2) => $Val("Constr", value0, value1, value2);
  var Record2 = (value0) => (value1) => $Val("Record", value0, value1);
  var Dictionary2 = (value0) => (value1) => $Val("Dictionary", value0, value1);
  var Matrix2 = (value0) => (value1) => $Val("Matrix", value0, value1);
  var Closure = (value0) => (value1) => (value2) => (value3) => $Fun("Closure", value0, value1, value2, value3);
  var Foreign = (value0) => (value1) => $Fun("Foreign", value0, value1);
  var PartialConstr = (value0) => (value1) => (value2) => $Fun("PartialConstr", value0, value1, value2);
  var highlightableUnit = { highlightIf: (v) => identity12 };
  var highlightableBoolean = {
    highlightIf: (v) => {
      if (!v) {
        return identity12;
      }
      if (v) {
        return (doc) => beside(beside(text("_"))(doc))(text("_"));
      }
      fail();
    }
  };
  var functorVal = {
    map: (f) => (v) => {
      if (v.tag === "Int") {
        return $Val("Int", f(v._1), v._2);
      }
      if (v.tag === "Float") {
        return $Val("Float", f(v._1), v._2);
      }
      if (v.tag === "Str") {
        return $Val("Str", f(v._1), v._2);
      }
      if (v.tag === "Record") {
        return $Val("Record", f(v._1), _fmapObject(v._2, functorVal.map(f)));
      }
      if (v.tag === "Dictionary") {
        return $Val("Dictionary", f(v._1), _fmapObject(v._2, bifunctorTuple.bimap(f)(functorVal.map(f))));
      }
      if (v.tag === "Constr") {
        return $Val("Constr", f(v._1), v._2, listMap(functorVal.map(f))(v._3));
      }
      if (v.tag === "Matrix") {
        return $Val(
          "Matrix",
          f(v._1),
          $Tuple(
            arrayMap(arrayMap(functorVal.map(f)))(v._2._1),
            $Tuple($Tuple(v._2._2._1._1, f(v._2._2._1._2)), $Tuple(v._2._2._2._1, f(v._2._2._2._2)))
          )
        );
      }
      if (v.tag === "Fun") {
        return $Val("Fun", functorFun.map(f)(v._1));
      }
      fail();
    }
  };
  var functorFun = {
    map: (f) => (m) => {
      if (m.tag === "Closure") {
        return $Fun(
          "Closure",
          f(m._1),
          _fmapObject(m._2, functorVal.map(f)),
          _fmapObject(m._3, functorElim.map(f)),
          functorElim.map(f)(m._4)
        );
      }
      if (m.tag === "Foreign") {
        return $Fun("Foreign", m._1, listMap(functorVal.map(f))(m._2));
      }
      if (m.tag === "PartialConstr") {
        return $Fun("PartialConstr", f(m._1), m._2, listMap(functorVal.map(f))(m._3));
      }
      fail();
    }
  };
  var joinSemilatticeVal = (dictJoinSemilattice) => ({
    maybeJoin: (v) => (v1) => {
      if (v.tag === "Int") {
        if (v1.tag === "Int") {
          const $3 = Int2(dictJoinSemilattice.join(v._1)(v1._1));
          const $4 = mayFailEq(showInt)(eqInt)(v._2)(v1._2);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible values");
      }
      if (v.tag === "Float") {
        if (v1.tag === "Float") {
          const $3 = Float2(dictJoinSemilattice.join(v._1)(v1._1));
          const $4 = mayFailEq(showNumber)(eqNumber)(v._2)(v1._2);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible values");
      }
      if (v.tag === "Str") {
        if (v1.tag === "Str") {
          const $3 = Str2(dictJoinSemilattice.join(v._1)(v1._1));
          const $4 = mayFailEq(showString)(eqString)(v._2)(v1._2);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible values");
      }
      if (v.tag === "Record") {
        if (v1.tag === "Record") {
          const $3 = Record2(dictJoinSemilattice.join(v._1)(v1._1));
          const $4 = joinSemilatticeDict(joinSemilatticeVal(dictJoinSemilattice)).maybeJoin(v._2)(v1._2);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible values");
      }
      if (v.tag === "Dictionary") {
        if (v1.tag === "Dictionary") {
          const $3 = Dictionary2(dictJoinSemilattice.join(v._1)(v1._1));
          const $4 = joinSemilatticeDict(joinSemilattice$x215(dictJoinSemilattice)(joinSemilatticeVal(dictJoinSemilattice))).maybeJoin(v._2)(v1._2);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible values");
      }
      if (v.tag === "Constr") {
        if (v1.tag === "Constr") {
          return applyEither.apply((() => {
            const $3 = Constr2(dictJoinSemilattice.join(v._1)(v1._1));
            const $4 = mayFailEq(showString)(eqString)(v._2)(v1._2);
            if ($4.tag === "Left") {
              return $Either("Left", $4._1);
            }
            if ($4.tag === "Right") {
              return $Either("Right", $3($4._1));
            }
            fail();
          })())(joinSemilatticeList(joinSemilatticeVal(dictJoinSemilattice)).maybeJoin(v._3)(v1._3));
        }
        return $Either("Left", "Incompatible values");
      }
      if (v.tag === "Matrix") {
        if (v1.tag === "Matrix") {
          const $3 = Matrix2(dictJoinSemilattice.join(v._1)(v1._1));
          const $4 = joinSemilatticeArray(joinSemilatticeArray(joinSemilatticeVal(dictJoinSemilattice))).maybeJoin(v._2._1)(v1._2._1);
          const $5 = mayFailEq(showInt)(eqInt)(v._2._2._1._1)(v1._2._2._1._1);
          if ($5.tag === "Left") {
            const $6 = applyEither.apply((() => {
              if ($4.tag === "Left") {
                return $Either("Left", $4._1);
              }
              if ($4.tag === "Right") {
                return $Either("Right", Tuple($4._1));
              }
              fail();
            })())(applyEither.apply($Either("Left", $5._1))((() => {
              const $62 = mayFailEq(showInt)(eqInt)(v._2._2._2._1)(v1._2._2._2._1);
              if ($62.tag === "Left") {
                return $Either("Left", $62._1);
              }
              if ($62.tag === "Right") {
                return $Either("Right", $Tuple($62._1, dictJoinSemilattice.join(v._2._2._2._2)(v1._2._2._2._2)));
              }
              fail();
            })()));
            if ($6.tag === "Left") {
              return $Either("Left", $6._1);
            }
            if ($6.tag === "Right") {
              return $Either("Right", $3($6._1));
            }
            fail();
          }
          if ($5.tag === "Right") {
            const $6 = applyEither.apply((() => {
              if ($4.tag === "Left") {
                return $Either("Left", $4._1);
              }
              if ($4.tag === "Right") {
                return $Either("Right", Tuple($4._1));
              }
              fail();
            })())(applyEither.apply($Either(
              "Right",
              Tuple($Tuple($5._1, dictJoinSemilattice.join(v._2._2._1._2)(v1._2._2._1._2)))
            ))((() => {
              const $62 = mayFailEq(showInt)(eqInt)(v._2._2._2._1)(v1._2._2._2._1);
              if ($62.tag === "Left") {
                return $Either("Left", $62._1);
              }
              if ($62.tag === "Right") {
                return $Either("Right", $Tuple($62._1, dictJoinSemilattice.join(v._2._2._2._2)(v1._2._2._2._2)));
              }
              fail();
            })()));
            if ($6.tag === "Left") {
              return $Either("Left", $6._1);
            }
            if ($6.tag === "Right") {
              return $Either("Right", $3($6._1));
            }
            fail();
          }
          fail();
        }
        return $Either("Left", "Incompatible values");
      }
      if (v.tag === "Fun") {
        if (v1.tag === "Fun") {
          const $3 = joinSemilatticeFun(dictJoinSemilattice).maybeJoin(v._1)(v1._1);
          if ($3.tag === "Left") {
            return $Either("Left", $3._1);
          }
          if ($3.tag === "Right") {
            return $Either("Right", $Val("Fun", $3._1));
          }
          fail();
        }
        return $Either("Left", "Incompatible values");
      }
      return $Either("Left", "Incompatible values");
    },
    join: (v) => definedJoin(joinSemilatticeVal(dictJoinSemilattice))(v),
    neg: functorVal.map(dictJoinSemilattice.neg)
  });
  var joinSemilatticeFun = (dictJoinSemilattice) => {
    const joinSemilatticeElim2 = joinSemilatticeElim(dictJoinSemilattice);
    const maybeJoin = joinSemilatticeDict(joinSemilatticeElim2).maybeJoin;
    return {
      maybeJoin: (v) => (v1) => {
        if (v.tag === "Closure") {
          if (v1.tag === "Closure") {
            return applyEither.apply(applyEither.apply((() => {
              const $5 = Closure(dictJoinSemilattice.join(v._1)(v1._1));
              const $6 = joinSemilatticeDict(joinSemilatticeVal(dictJoinSemilattice)).maybeJoin(v._2)(v1._2);
              if ($6.tag === "Left") {
                return $Either("Left", $6._1);
              }
              if ($6.tag === "Right") {
                return $Either("Right", $5($6._1));
              }
              fail();
            })())(maybeJoin(v._3)(v1._3)))(joinSemilatticeElim2.maybeJoin(v._4)(v1._4));
          }
          return $Either("Left", "Incompatible functions");
        }
        if (v.tag === "Foreign") {
          if (v1.tag === "Foreign") {
            const $5 = Foreign(v._1);
            const $6 = joinSemilatticeList(joinSemilatticeVal(dictJoinSemilattice)).maybeJoin(v._2)(v1._2);
            if ($6.tag === "Left") {
              return $Either("Left", $6._1);
            }
            if ($6.tag === "Right") {
              return $Either("Right", $5($6._1));
            }
            fail();
          }
          return $Either("Left", "Incompatible functions");
        }
        if (v.tag === "PartialConstr") {
          if (v1.tag === "PartialConstr") {
            return applyEither.apply((() => {
              const $5 = PartialConstr(dictJoinSemilattice.join(v._1)(v1._1));
              const $6 = mayFailEq(showString)(eqString)(v._2)(v1._2);
              if ($6.tag === "Left") {
                return $Either("Left", $6._1);
              }
              if ($6.tag === "Right") {
                return $Either("Right", $5($6._1));
              }
              fail();
            })())(joinSemilatticeList(joinSemilatticeVal(dictJoinSemilattice)).maybeJoin(v._3)(v1._3));
          }
          return $Either("Left", "Incompatible functions");
        }
        return $Either("Left", "Incompatible functions");
      },
      join: (v) => definedJoin(joinSemilatticeFun(dictJoinSemilattice))(v),
      neg: functorFun.map(dictJoinSemilattice.neg)
    };
  };
  var expandableValRawVal = (dictBoundedJoinSemilattice) => {
    const expandableDictDict1 = expandableDictDict(botOfUnit$x215Raw$x215(functorVal)(dictBoundedJoinSemilattice));
    return {
      expand: (v) => (v1) => {
        if (v.tag === "Int") {
          if (v1.tag === "Int") {
            return $Val("Int", v._1, mustEq(eqInt)(showInt)(v._2)(v1._2));
          }
          return unsafePerformEffect(throwException(error("Incompatible values")));
        }
        if (v.tag === "Float") {
          if (v1.tag === "Float") {
            return $Val("Float", v._1, mustEq(eqNumber)(showNumber)(v._2)(v1._2));
          }
          return unsafePerformEffect(throwException(error("Incompatible values")));
        }
        if (v.tag === "Str") {
          if (v1.tag === "Str") {
            return $Val("Str", v._1, mustEq(eqString)(showString)(v._2)(v1._2));
          }
          return unsafePerformEffect(throwException(error("Incompatible values")));
        }
        if (v.tag === "Record") {
          if (v1.tag === "Record") {
            return $Val(
              "Record",
              v._1,
              expandableDictDict({ botOf: functorVal.map((v$1) => dictBoundedJoinSemilattice.bot) })(expandableValRawVal(dictBoundedJoinSemilattice)).expand(v._2)(v1._2)
            );
          }
          return unsafePerformEffect(throwException(error("Incompatible values")));
        }
        if (v.tag === "Dictionary") {
          if (v1.tag === "Dictionary") {
            return $Val(
              "Dictionary",
              v._1,
              expandableDictDict1((() => {
                const $4 = expandableValRawVal(dictBoundedJoinSemilattice);
                return { expand: (v$1) => (v1$1) => $Tuple(v$1._1, $4.expand(v$1._2)(v1$1._2)) };
              })()).expand(v._2)(v1._2)
            );
          }
          return unsafePerformEffect(throwException(error("Incompatible values")));
        }
        if (v.tag === "Constr") {
          if (v1.tag === "Constr") {
            return $Val(
              "Constr",
              v._1,
              mustEq(eqString)(showString)(v._2)(v1._2),
              zipWith(expandableValRawVal(dictBoundedJoinSemilattice).expand)(v._3)(v1._3)
            );
          }
          return unsafePerformEffect(throwException(error("Incompatible values")));
        }
        if (v.tag === "Matrix") {
          if (v1.tag === "Matrix") {
            return $Val(
              "Matrix",
              v._1,
              $Tuple(
                (() => {
                  const $4 = expandableValRawVal(dictBoundedJoinSemilattice);
                  return zipWith2((xs) => (ys) => zipWith2($4.expand)(xs)(ys))(v._2._1)(v1._2._1);
                })(),
                $Tuple(
                  $Tuple(mustEq(eqInt)(showInt)(v._2._2._1._1)(v1._2._2._1._1), v._2._2._1._2),
                  $Tuple(mustEq(eqInt)(showInt)(v._2._2._2._1)(v1._2._2._2._1), v._2._2._2._2)
                )
              )
            );
          }
          return unsafePerformEffect(throwException(error("Incompatible values")));
        }
        if (v.tag === "Fun") {
          if (v1.tag === "Fun") {
            return $Val("Fun", expandableFunRawFun(dictBoundedJoinSemilattice).expand(v._1)(v1._1));
          }
          return unsafePerformEffect(throwException(error("Incompatible values")));
        }
        return unsafePerformEffect(throwException(error("Incompatible values")));
      }
    };
  };
  var expandableFunRawFun = (dictBoundedJoinSemilattice) => {
    const expandableElimRawElim2 = expandableElimRawElim(dictBoundedJoinSemilattice);
    const expand = expandableDictDict({ botOf: functorElim.map((v) => dictBoundedJoinSemilattice.bot) })(expandableElimRawElim2).expand;
    return {
      expand: (v) => (v1) => {
        if (v.tag === "Closure") {
          if (v1.tag === "Closure") {
            return $Fun(
              "Closure",
              v._1,
              expandableDictDict({ botOf: functorVal.map((v$1) => dictBoundedJoinSemilattice.bot) })(expandableValRawVal(dictBoundedJoinSemilattice)).expand(v._2)(v1._2),
              expand(v._3)(v1._3),
              expandableElimRawElim2.expand(v._4)(v1._4)
            );
          }
          return unsafePerformEffect(throwException(error("Incompatible values")));
        }
        if (v.tag === "Foreign") {
          if (v1.tag === "Foreign") {
            return $Fun("Foreign", v._1, zipWith(expandableValRawVal(dictBoundedJoinSemilattice).expand)(v._2)(v1._2));
          }
          return unsafePerformEffect(throwException(error("Incompatible values")));
        }
        if (v.tag === "PartialConstr") {
          if (v1.tag === "PartialConstr") {
            return $Fun(
              "PartialConstr",
              v._1,
              mustEq(eqString)(showString)(v._2)(v1._2),
              zipWith(expandableValRawVal(dictBoundedJoinSemilattice).expand)(v._3)(v1._3)
            );
          }
          return unsafePerformEffect(throwException(error("Incompatible values")));
        }
        return unsafePerformEffect(throwException(error("Incompatible values")));
      }
    };
  };
  var annUnit = { Highlightable0: () => highlightableUnit, BoundedLattice1: () => boundedLatticeUnit };
  var annBoolean = { Highlightable0: () => highlightableBoolean, BoundedLattice1: () => boundedLatticeBoolean };
  var updateMatrix = (i) => (j) => (\u03B4v) => (v) => {
    const vs_i = definitely("index within bounds")(index2(v._1)(i - 1 | 0));
    return $Tuple(
      unsafeUpdateAt(i - 1 | 0)(unsafeUpdateAt(j - 1 | 0)(\u03B4v(definitely("index within bounds")(index2(vs_i)(j - 1 | 0))))(vs_i))(v._1),
      $Tuple(v._2._1, v._2._2)
    );
  };
  var restrict = (\u03B3) => (xs) => filterWithKey((x2) => {
    const $3 = lookup(ordString)(x2)(xs);
    const $4 = (() => {
      if ($3.tag === "Nothing") {
        return false;
      }
      if ($3.tag === "Just") {
        return true;
      }
      fail();
    })();
    return (v) => $4;
  })(\u03B3);
  var reaches = (\u03C1) => (xs) => {
    const dom_\u03C1 = fromFoldable5(keys(\u03C1));
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0, acc = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = acc;
          continue;
        }
        if (v.tag === "Cons") {
          if ((() => {
            const $6 = lookup(ordString)(v._1)(acc);
            if ($6.tag === "Nothing") {
              return false;
            }
            if ($6.tag === "Just") {
              return true;
            }
            fail();
          })()) {
            go$a0 = v._2;
            go$a1 = acc;
            continue;
          }
          go$a0 = foldableList.foldr(Cons)(v._2)(toUnfoldable7(intersection2(fVElim.fv($$get(v._1)(\u03C1)))(dom_\u03C1)));
          go$a1 = unionWith(ordString)($$const)($Map(
            "Two",
            Leaf2,
            v._1,
            unit,
            Leaf2
          ))(acc);
          continue;
        }
        fail();
      }
      ;
      return go$r;
    };
    return go(toUnfoldable7(xs))(Leaf2);
  };
  var lookup$p = (x2) => (\u03B3) => {
    const $2 = "variable " + (x2 + " not found");
    const $3 = _lookup(Nothing, Just, x2, \u03B3);
    if ($3.tag === "Nothing") {
      return $Either("Left", $2);
    }
    if ($3.tag === "Just") {
      return $Either("Right", $3._1);
    }
    fail();
  };
  var $$for = (\u03C1) => (\u03C3) => restrict(\u03C1)(reaches(\u03C1)(intersection2(fVElim.fv(\u03C3))(fromFoldable5(keys(\u03C1)))));
  var append_inv = (xs) => (\u03B3) => $Tuple(
    filterWithKey((x2) => {
      const $3 = lookup(ordString)(x2)(xs);
      const $4 = (() => {
        if ($3.tag === "Nothing") {
          return true;
        }
        if ($3.tag === "Just") {
          return false;
        }
        fail();
      })();
      return (v) => $4;
    })(\u03B3),
    restrict(\u03B3)(xs)
  );

  // output-es/Pretty/index.js
  var hcat = /* @__PURE__ */ (() => foldableList.foldMap(monoidColumns)(unsafeCoerce))();
  var toUnfoldable8 = /* @__PURE__ */ toAscUnfoldable(unfoldableList);
  var space = /* @__PURE__ */ text(" ");
  var prettyP = (dictPretty) => (x2) => intercalate3("\n")(dictPretty.pretty(x2).lines);
  var nil2 = /* @__PURE__ */ text("[]");
  var hspace = (dictFoldable) => {
    const $1 = dictFoldable.foldr(Cons)(Nil);
    return (x2) => hcat(intercalate2($List("Cons", space, Nil))(listMap(applicativeList.pure)($1(x2))));
  };
  var hspace1 = /* @__PURE__ */ hspace(foldableArray);
  var hspace2 = /* @__PURE__ */ hspace(foldableList);
  var emptyDoc = /* @__PURE__ */ empty2(0)(0);
  var vert = (dictFoldable) => {
    const fromFoldable10 = dictFoldable.foldr(Cons)(Nil);
    return (delim) => {
      const vert$p = (v) => {
        if (v.tag === "Nil") {
          return emptyDoc;
        }
        if (v.tag === "Cons") {
          if (v._2.tag === "Nil") {
            return v._1;
          }
          if (v._2.tag === "Cons") {
            return atop(beside(v._1)(delim))(vert$p($List("Cons", v._2._1, v._2._2)));
          }
          fail();
        }
        fail();
      };
      return (x2) => vert$p(fromFoldable10(x2));
    };
  };
  var vert2 = /* @__PURE__ */ vert(foldableArray);
  var comma = /* @__PURE__ */ text(",");
  var hcomma = (dictFoldable) => {
    const $1 = dictFoldable.foldr(Cons)(Nil);
    const $2 = intersperse(beside(comma)(space));
    return (x2) => hcat($2($1(x2)));
  };
  var hcomma1 = /* @__PURE__ */ hcomma(foldableList);
  var hcomma2 = /* @__PURE__ */ hcomma(foldableArray);
  var prettyRecordOrDict = (dictPretty) => (dictHighlightable) => (sep) => (bracify) => (prettyKey) => (\u03B1) => (xvs) => dictHighlightable.highlightIf(\u03B1)(bracify(hcomma1(listMap((v) => hspace1([
    beside(v._1)(sep),
    dictPretty.pretty(v._2)
  ]))(listMap(strongFn.first(prettyKey))(xvs)))));
  var between = (l) => (r) => (doc) => beside(beside(l)(doc))(r);
  var prettyParensOpt = (dictPretty) => (x2) => {
    const doc = dictPretty.pretty(x2);
    if (contains(" ")(intercalate3("\n")(doc.lines))) {
      return beside(beside(text("("))(doc))(text(")"));
    }
    return doc;
  };
  var prettyConstr = (dictPretty) => (dictHighlightable) => (\u03B1) => (c) => (v) => {
    const $5 = (c1, xs, \u03B11) => hspace2($List(
      "Cons",
      dictHighlightable.highlightIf(\u03B11)(text(showCtr(c1))),
      listMap(prettyParensOpt(dictPretty))(xs)
    ));
    if (v.tag === "Cons") {
      if (v._2.tag === "Cons") {
        if (c === "Pair") {
          return (() => {
            if (v._2._2.tag === "Nil") {
              return identity9;
            }
            return (v1) => unsafePerformEffect(throwException(error("Assertion failure")));
          })()(dictHighlightable.highlightIf(\u03B1)(beside(beside(text("("))(hcomma2([dictPretty.pretty(v._1), dictPretty.pretty(v._2._1)])))(text(")"))));
        }
        if (c === "Nil") {
          return (() => {
            if (v.tag === "Nil") {
              return identity9;
            }
            return (v1) => unsafePerformEffect(throwException(error("Assertion failure")));
          })()(dictHighlightable.highlightIf(\u03B1)(nil2));
        }
        if (c === ":") {
          return (() => {
            if (v._2._2.tag === "Nil") {
              return identity9;
            }
            return (v1) => unsafePerformEffect(throwException(error("Assertion failure")));
          })()(beside(beside(text("("))(hspace1([
            dictPretty.pretty(v._1),
            dictHighlightable.highlightIf(\u03B1)(text(":")),
            dictPretty.pretty(v._2._1)
          ])))(text(")")));
        }
        return $5(c, v, \u03B1);
      }
      if (c === "Nil") {
        return (() => {
          if (v.tag === "Nil") {
            return identity9;
          }
          return (v1) => unsafePerformEffect(throwException(error("Assertion failure")));
        })()(dictHighlightable.highlightIf(\u03B1)(nil2));
      }
      return $5(c, v, \u03B1);
    }
    if (c === "Nil") {
      return (() => {
        if (v.tag === "Nil") {
          return identity9;
        }
        return (v1) => unsafePerformEffect(throwException(error("Assertion failure")));
      })()(dictHighlightable.highlightIf(\u03B1)(nil2));
    }
    return $5(c, v, \u03B1);
  };
  var prettyDict = (dictPretty) => (dictHighlightable) => prettyRecordOrDict(dictPretty)(dictHighlightable)(text(":="))(between(text("{|"))(text("|}")));
  var prettyRecord = (dictPretty) => (dictHighlightable) => prettyRecordOrDict(dictPretty)(dictHighlightable)(text(":"))(between(text("{"))(text("}")));
  var prettyVal = (dictHighlightable) => ({
    pretty: (v) => {
      if (v.tag === "Int") {
        return dictHighlightable.highlightIf(v._1)(text(showIntImpl(v._2)));
      }
      if (v.tag === "Float") {
        return dictHighlightable.highlightIf(v._1)(text(showNumberImpl(v._2)));
      }
      if (v.tag === "Str") {
        return dictHighlightable.highlightIf(v._1)(text(showStringImpl(v._2)));
      }
      if (v.tag === "Record") {
        return prettyRecord(prettyVal(dictHighlightable))(dictHighlightable)(text)(v._1)(toUnfoldable8(v._2));
      }
      if (v.tag === "Dictionary") {
        return prettyDict(prettyVal(dictHighlightable))(dictHighlightable)((v1) => dictHighlightable.highlightIf(v1._2)(text(showStringImpl(v1._1))))(v._1)(listMap((v1) => $Tuple(
          $Tuple(v1._1, v1._2._1),
          v1._2._2
        ))(toUnfoldable8(v._2)));
      }
      if (v.tag === "Constr") {
        return prettyConstr(prettyVal(dictHighlightable))(dictHighlightable)(v._1)(v._2)(v._3);
      }
      if (v.tag === "Matrix") {
        return vert2(comma)(arrayMap((() => {
          const $2 = arrayMap(prettyVal(dictHighlightable).pretty);
          return (x2) => hcomma2($2(x2));
        })())(v._2._1));
      }
      if (v.tag === "Fun") {
        return prettyFun(dictHighlightable).pretty(v._1);
      }
      fail();
    }
  });
  var prettyFun = (dictHighlightable) => ({
    pretty: (v) => {
      if (v.tag === "Closure") {
        return text("<closure>");
      }
      if (v.tag === "Foreign") {
        return beside(beside(text("("))(text("<extern op>")))(text(")"));
      }
      if (v.tag === "PartialConstr") {
        return prettyConstr(prettyVal(dictHighlightable))(dictHighlightable)(v._1)(v._2)(v._3);
      }
      fail();
    }
  });

  // output-es/Primitive/index.js
  var fanin2 = /* @__PURE__ */ fanin(categoryFn)(choiceFn);
  var isZeroNumber = { isZero: ($0) => 0 === $0 };
  var isZeroInt = { isZero: ($0) => 0 === $0 };
  var val = {
    constr: (dictAnn) => fst,
    constr_bwd: (dictAnn) => {
      const bot = dictAnn.BoundedLattice1().BoundedJoinSemilattice0().bot;
      return (v) => $Tuple(v, bot);
    },
    match: (dictAnn) => {
      const top2 = dictAnn.BoundedLattice1().BoundedMeetSemilattice1().top;
      return (v) => $Tuple(v, top2);
    }
  };
  var unary = (op) => {
    const match10 = op.i.match(annUnit);
    return $Val(
      "Fun",
      $Fun(
        "Foreign",
        $ForeignOp$p({
          arity: 1,
          op: (dictAnn) => {
            const match13 = op.i.match(dictAnn);
            const constr = op.o.constr(dictAnn);
            return (v) => {
              if (v.tag === "Cons") {
                if (v._2.tag === "Nil") {
                  const v2 = match13(v._1);
                  return $Either("Right", $Tuple(functorVal.map((v$1) => unit)(v._1), constr($Tuple(op.fwd(v2._1), v2._2))));
                }
                fail();
              }
              fail();
            };
          },
          op_bwd: (dictAnn) => {
            const constr_bwd = op.o.constr_bwd(dictAnn);
            const constr = op.i.constr(dictAnn);
            return (v) => $List("Cons", constr($Tuple(match10(v._1)._1, constr_bwd(v._2)._2)), Nil);
          }
        }),
        Nil
      )
    );
  };
  var string = {
    constr: (dictAnn) => (v) => $Val("Str", v._2, v._1),
    constr_bwd: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Str") {
          return $Tuple(v._2, v._1);
        }
        return unsafePerformEffect(throwException(error("Str expected; got " + prettyP2(v))));
      };
    },
    match: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Str") {
          return $Tuple(v._2, v._1);
        }
        return unsafePerformEffect(throwException(error("Str expected; got " + prettyP2(v))));
      };
    }
  };
  var record = {
    constr: (dictAnn) => (v) => $Val("Record", v._2, v._1),
    constr_bwd: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Record") {
          return $Tuple(v._2, v._1);
        }
        return unsafePerformEffect(throwException(error("Record expected; got " + prettyP2(v))));
      };
    },
    match: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Record") {
          return $Tuple(v._2, v._1);
        }
        return unsafePerformEffect(throwException(error("Record expected; got " + prettyP2(v))));
      };
    }
  };
  var number = {
    constr: (dictAnn) => (v) => $Val("Float", v._2, v._1),
    constr_bwd: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Float") {
          return $Tuple(v._2, v._1);
        }
        return unsafePerformEffect(throwException(error("Float expected; got " + prettyP2(v))));
      };
    },
    match: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Float") {
          return $Tuple(v._2, v._1);
        }
        return unsafePerformEffect(throwException(error("Float expected; got " + prettyP2(v))));
      };
    }
  };
  var matrixRep = {
    constr: (dictAnn) => (v) => $Val("Matrix", v._2, v._1),
    constr_bwd: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Matrix") {
          return $Tuple(v._2, v._1);
        }
        return unsafePerformEffect(throwException(error("Matrix expected; got " + prettyP2(v))));
      };
    },
    match: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Matrix") {
          return $Tuple(v._2, v._1);
        }
        return unsafePerformEffect(throwException(error("Matrix expected; got " + prettyP2(v))));
      };
    }
  };
  var intOrNumberOrString = {
    constr: (dictAnn) => (v) => {
      if (v._1.tag === "Left") {
        return $Val("Int", v._2, v._1._1);
      }
      if (v._1.tag === "Right") {
        if (v._1._1.tag === "Left") {
          return $Val("Float", v._2, v._1._1._1);
        }
        if (v._1._1.tag === "Right") {
          return $Val("Str", v._2, v._1._1._1);
        }
        fail();
      }
      fail();
    },
    constr_bwd: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Int") {
          return $Tuple($Either("Left", v._2), v._1);
        }
        if (v.tag === "Float") {
          return $Tuple($Either("Right", $Either("Left", v._2)), v._1);
        }
        if (v.tag === "Str") {
          return $Tuple($Either("Right", $Either("Right", v._2)), v._1);
        }
        return unsafePerformEffect(throwException(error("Int, Float or Str expected; got " + prettyP2(v))));
      };
    },
    match: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Int") {
          return $Tuple($Either("Left", v._2), v._1);
        }
        if (v.tag === "Float") {
          return $Tuple($Either("Right", $Either("Left", v._2)), v._1);
        }
        if (v.tag === "Str") {
          return $Tuple($Either("Right", $Either("Right", v._2)), v._1);
        }
        return unsafePerformEffect(throwException(error("Int, Float or Str expected; got " + prettyP2(v))));
      };
    }
  };
  var intOrNumber = {
    constr: (dictAnn) => (v) => {
      if (v._1.tag === "Left") {
        return $Val("Int", v._2, v._1._1);
      }
      if (v._1.tag === "Right") {
        return $Val("Float", v._2, v._1._1);
      }
      fail();
    },
    constr_bwd: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Int") {
          return $Tuple($Either("Left", v._2), v._1);
        }
        if (v.tag === "Float") {
          return $Tuple($Either("Right", v._2), v._1);
        }
        return unsafePerformEffect(throwException(error("Int or Float expected; got " + prettyP2(v))));
      };
    },
    match: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Int") {
          return $Tuple($Either("Left", v._2), v._1);
        }
        if (v.tag === "Float") {
          return $Tuple($Either("Right", v._2), v._1);
        }
        return unsafePerformEffect(throwException(error("Int or Float expected; got " + prettyP2(v))));
      };
    }
  };
  var $$int = {
    constr: (dictAnn) => (v) => $Val("Int", v._2, v._1),
    constr_bwd: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Int") {
          return $Tuple(v._2, v._1);
        }
        return unsafePerformEffect(throwException(error("Int expected; got " + prettyP2(v))));
      };
    },
    match: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Int") {
          return $Tuple(v._2, v._1);
        }
        return unsafePerformEffect(throwException(error("Int expected; got " + prettyP2(v))));
      };
    }
  };
  var intPair = {
    constr: (dictAnn) => (v) => $Val(
      "Constr",
      v._2,
      "Pair",
      $List("Cons", $Val("Int", v._1._1._2, v._1._1._1), $List("Cons", $Val("Int", v._1._2._2, v._1._2._1), Nil))
    ),
    constr_bwd: (dictAnn) => {
      const match10 = $$int.match(dictAnn);
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Constr") {
          if (v._3.tag === "Cons") {
            if (v._3._2.tag === "Cons") {
              if (v._3._2._2.tag === "Nil") {
                if (v._2 === "Pair") {
                  return $Tuple($Tuple(match10(v._3._1), match10(v._3._2._1)), v._1);
                }
                return unsafePerformEffect(throwException(error("Pair expected; got " + prettyP2(v))));
              }
              return unsafePerformEffect(throwException(error("Pair expected; got " + prettyP2(v))));
            }
            return unsafePerformEffect(throwException(error("Pair expected; got " + prettyP2(v))));
          }
          return unsafePerformEffect(throwException(error("Pair expected; got " + prettyP2(v))));
        }
        return unsafePerformEffect(throwException(error("Pair expected; got " + prettyP2(v))));
      };
    },
    match: (dictAnn) => {
      const match10 = $$int.match(dictAnn);
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Constr") {
          if (v._3.tag === "Cons") {
            if (v._3._2.tag === "Cons") {
              if (v._3._2._2.tag === "Nil") {
                if (v._2 === "Pair") {
                  return $Tuple($Tuple(match10(v._3._1), match10(v._3._2._1)), v._1);
                }
                return unsafePerformEffect(throwException(error("Pair expected; got " + prettyP2(v))));
              }
              return unsafePerformEffect(throwException(error("Pair expected; got " + prettyP2(v))));
            }
            return unsafePerformEffect(throwException(error("Pair expected; got " + prettyP2(v))));
          }
          return unsafePerformEffect(throwException(error("Pair expected; got " + prettyP2(v))));
        }
        return unsafePerformEffect(throwException(error("Pair expected; got " + prettyP2(v))));
      };
    }
  };
  var $$boolean = {
    constr: (dictAnn) => (v) => {
      if (v._1) {
        return $Val("Constr", v._2, "True", Nil);
      }
      if (!v._1) {
        return $Val("Constr", v._2, "False", Nil);
      }
      fail();
    },
    constr_bwd: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Constr") {
          if (v._3.tag === "Nil") {
            if (v._2 === "True") {
              return $Tuple(true, v._1);
            }
            if (v._2 === "False") {
              return $Tuple(false, v._1);
            }
            return unsafePerformEffect(throwException(error("Boolean expected; got " + prettyP2(v))));
          }
          return unsafePerformEffect(throwException(error("Boolean expected; got " + prettyP2(v))));
        }
        return unsafePerformEffect(throwException(error("Boolean expected; got " + prettyP2(v))));
      };
    },
    match: (dictAnn) => {
      const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
      return (v) => {
        if (v.tag === "Constr") {
          if (v._3.tag === "Nil") {
            if (v._2 === "True") {
              return $Tuple(true, v._1);
            }
            if (v._2 === "False") {
              return $Tuple(false, v._1);
            }
            return unsafePerformEffect(throwException(error("Boolean expected; got " + prettyP2(v))));
          }
          return unsafePerformEffect(throwException(error("Boolean expected; got " + prettyP2(v))));
        }
        return unsafePerformEffect(throwException(error("Boolean expected; got " + prettyP2(v))));
      };
    }
  };
  var binaryZero = (dictIsZero) => (op) => {
    const match10 = op.i.match(annUnit);
    return $Val(
      "Fun",
      $Fun(
        "Foreign",
        $ForeignOp$p({
          arity: 2,
          op: (dictAnn) => {
            const match13 = op.i.match(dictAnn);
            const constr = op.o.constr(dictAnn);
            const meet = dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0().meet;
            return (v) => {
              if (v.tag === "Cons") {
                if (v._2.tag === "Cons") {
                  if (v._2._2.tag === "Nil") {
                    const $8 = match13(v._1);
                    const $9 = match13(v._2._1);
                    return $Either(
                      "Right",
                      $Tuple(
                        $Tuple(functorVal.map((v$1) => unit)(v._1), functorVal.map((v$1) => unit)(v._2._1)),
                        constr($Tuple(
                          op.fwd($8._1)($9._1),
                          (() => {
                            if (dictIsZero.isZero($8._1)) {
                              return $8._2;
                            }
                            if (dictIsZero.isZero($9._1)) {
                              return $9._2;
                            }
                            return meet($8._2)($9._2);
                          })()
                        ))
                      )
                    );
                  }
                  fail();
                }
                fail();
              }
              fail();
            };
          },
          op_bwd: (dictAnn) => {
            const constr_bwd = op.o.constr_bwd(dictAnn);
            const bot = dictAnn.BoundedLattice1().BoundedJoinSemilattice0().bot;
            const constr = op.i.constr(dictAnn);
            return (v) => {
              const $8 = constr_bwd(v._2)._2;
              const $9 = match10(v._1._1);
              const $10 = match10(v._1._2);
              if (dictIsZero.isZero($9._1)) {
                return $List(
                  "Cons",
                  constr($Tuple($9._1, $8)),
                  $List("Cons", constr($Tuple($10._1, bot)), Nil)
                );
              }
              if (dictIsZero.isZero($10._1)) {
                return $List(
                  "Cons",
                  constr($Tuple($9._1, bot)),
                  $List("Cons", constr($Tuple($10._1, $8)), Nil)
                );
              }
              return $List(
                "Cons",
                constr($Tuple($9._1, $8)),
                $List("Cons", constr($Tuple($10._1, $8)), Nil)
              );
            };
          }
        }),
        Nil
      )
    );
  };
  var binary = (op) => {
    const match10 = op.i1.match(annUnit);
    const match13 = op.i2.match(annUnit);
    return $Val(
      "Fun",
      $Fun(
        "Foreign",
        $ForeignOp$p({
          arity: 2,
          op: (dictAnn) => {
            const match22 = op.i1.match(dictAnn);
            const match32 = op.i2.match(dictAnn);
            const constr = op.o.constr(dictAnn);
            const meet = dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0().meet;
            return (v) => {
              if (v.tag === "Cons") {
                if (v._2.tag === "Cons") {
                  if (v._2._2.tag === "Nil") {
                    const $9 = match22(v._1);
                    const $10 = match32(v._2._1);
                    return $Either(
                      "Right",
                      $Tuple(
                        $Tuple(functorVal.map((v$1) => unit)(v._1), functorVal.map((v$1) => unit)(v._2._1)),
                        constr($Tuple(op.fwd($9._1)($10._1), meet($9._2)($10._2)))
                      )
                    );
                  }
                  fail();
                }
                fail();
              }
              fail();
            };
          },
          op_bwd: (dictAnn) => {
            const constr_bwd = op.o.constr_bwd(dictAnn);
            const constr = op.i1.constr(dictAnn);
            const constr1 = op.i2.constr(dictAnn);
            return (v) => {
              const $8 = constr_bwd(v._2)._2;
              return $List(
                "Cons",
                constr($Tuple(match10(v._1._1)._1, $8)),
                $List("Cons", constr1($Tuple(match13(v._1._2)._1, $8)), Nil)
              );
            };
          }
        }),
        Nil
      )
    );
  };
  var asNumberString = { as: (v) => unsafePerformEffect(throwException(error("Non-uniform argument types"))) };
  var asNumberIntOrNumber = { as: Right };
  var asIntNumberOrString = { as: (x2) => $Either("Left", toNumber(x2)) };
  var asIntNumber = { as: toNumber };
  var asIntIntOrNumber = { as: Left };
  var asBooleanBoolean = { as: (x2) => x2 };
  var union3 = (dictAs) => (dictAs1) => (dictAs2) => (dictAs3) => (v) => (v1) => (v2) => (v3) => {
    if (v2.tag === "Left") {
      if (v3.tag === "Left") {
        return dictAs.as(v(v2._1)(v3._1));
      }
      if (v3.tag === "Right") {
        return dictAs1.as(v1(dictAs2.as(v2._1))(v3._1));
      }
      fail();
    }
    if (v2.tag === "Right") {
      if (v3.tag === "Right") {
        return dictAs1.as(v1(v2._1)(v3._1));
      }
      if (v3.tag === "Left") {
        return dictAs1.as(v1(v2._1)(dictAs3.as(v3._1)));
      }
      fail();
    }
    fail();
  };
  var unionStr = (dictAs) => (dictAs1) => union3(dictAs)(dictAs)(dictAs1)(dictAs1);

  // output-es/App.Util/index.js
  var neg = /* @__PURE__ */ (() => joinSemilatticeVal(joinSemilatticeBoolean).neg)();
  var match = /* @__PURE__ */ (() => record.match(annBoolean))();
  var match1 = /* @__PURE__ */ (() => intOrNumber.match(annBoolean))();
  var toggleField = (v) => (v1) => (v2) => {
    if (v2.tag === "Record") {
      return $Val("Record", v2._1, update((x2) => $Maybe("Just", v1(x2)))(v)(v2._2));
    }
    return unsafePerformEffect(throwException(error("absurd")));
  };
  var toggleConstrArg = (v) => (v1) => (v2) => (v3) => {
    if (v3.tag === "Constr") {
      if (v === v3._2) {
        return definitely("absurd")((() => {
          const $4 = index(v3._3)(v1);
          if ($4.tag === "Just") {
            const $5 = updateAt(v1)(v2($4._1))(v3._3);
            if ($5.tag === "Just") {
              return $Maybe("Just", $Val("Constr", v3._1, v, $5._1));
            }
            if ($5.tag === "Nothing") {
              return Nothing;
            }
            fail();
          }
          if ($4.tag === "Nothing") {
            return Nothing;
          }
          fail();
        })());
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    return unsafePerformEffect(throwException(error("absurd")));
  };
  var toggleCell = (v) => (v1) => (v2) => {
    if (v2.tag === "Matrix") {
      return $Val(
        "Matrix",
        v2._1,
        updateMatrix(v)(v1)(neg)($Tuple(
          v2._2._1,
          $Tuple($Tuple(v2._2._2._1._1, v2._2._2._1._2), $Tuple(v2._2._2._2._1, v2._2._2._2._2))
        ))
      );
    }
    return unsafePerformEffect(throwException(error("absurd")));
  };
  var selectNth = (v) => (v1) => (v2) => {
    if (v2.tag === "Constr") {
      if (v2._3.tag === "Cons") {
        if (v2._3._2.tag === "Cons") {
          if (v2._3._2._2.tag === "Nil") {
            if (v === 0) {
              if (v2._2 === ":") {
                return $Val("Constr", v2._1, v2._2, $List("Cons", v1(v2._3._1), $List("Cons", v2._3._2._1, Nil)));
              }
              if (v2._2 === ":") {
                return $Val(
                  "Constr",
                  v2._1,
                  v2._2,
                  $List("Cons", v2._3._1, $List("Cons", selectNth(v - 1 | 0)(v1)(v2._3._2._1), Nil))
                );
              }
              return unsafePerformEffect(throwException(error("absurd")));
            }
            if (v2._2 === ":") {
              return $Val(
                "Constr",
                v2._1,
                v2._2,
                $List("Cons", v2._3._1, $List("Cons", selectNth(v - 1 | 0)(v1)(v2._3._2._1), Nil))
              );
            }
            return unsafePerformEffect(throwException(error("absurd")));
          }
          return unsafePerformEffect(throwException(error("absurd")));
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    return unsafePerformEffect(throwException(error("absurd")));
  };
  var record2 = (toRecord) => (u) => toRecord(match(u)._1);
  var get_intOrNumber = (x2) => (r) => {
    const $2 = match1($$get(x2)(r));
    return $Tuple(
      (() => {
        if ($2._1.tag === "Left") {
          return toNumber($2._1._1);
        }
        if ($2._1.tag === "Right") {
          return $2._1._1;
        }
        fail();
      })(),
      $2._2
    );
  };
  var reflectArray = {
    from: () => (v) => {
      if (v.tag === "Constr") {
        if (v._3.tag === "Nil") {
          if (v._2 === "Nil") {
            return [];
          }
          fail();
        }
        if (v._3.tag === "Cons") {
          if (v._3._2.tag === "Cons") {
            if (v._3._2._2.tag === "Nil") {
              if (v._2 === ":") {
                return concatArray([v._3._1])(reflectArray.from()(v._3._2._1));
              }
              fail();
            }
            fail();
          }
          fail();
        }
        fail();
      }
      fail();
    }
  };
  var doNothing = (v) => () => unit;

  // output-es/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }
  function notNull(x2) {
    return x2;
  }

  // output-es/Web.Event.Event/foreign.js
  function _target(e) {
    return e.target;
  }

  // node_modules/d3-array/src/ascending.js
  function ascending_default(a, b) {
    return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
  }

  // node_modules/d3-array/src/bisector.js
  function bisector_default(f) {
    let delta = f;
    let compare = f;
    if (f.length === 1) {
      delta = (d, x2) => f(d) - x2;
      compare = ascendingComparator(f);
    }
    function left2(a, x2, lo, hi) {
      if (lo == null)
        lo = 0;
      if (hi == null)
        hi = a.length;
      while (lo < hi) {
        const mid = lo + hi >>> 1;
        if (compare(a[mid], x2) < 0)
          lo = mid + 1;
        else
          hi = mid;
      }
      return lo;
    }
    function right2(a, x2, lo, hi) {
      if (lo == null)
        lo = 0;
      if (hi == null)
        hi = a.length;
      while (lo < hi) {
        const mid = lo + hi >>> 1;
        if (compare(a[mid], x2) > 0)
          hi = mid;
        else
          lo = mid + 1;
      }
      return lo;
    }
    function center2(a, x2, lo, hi) {
      if (lo == null)
        lo = 0;
      if (hi == null)
        hi = a.length;
      const i = left2(a, x2, lo, hi - 1);
      return i > lo && delta(a[i - 1], x2) > -delta(a[i], x2) ? i - 1 : i;
    }
    return { left: left2, center: center2, right: right2 };
  }
  function ascendingComparator(f) {
    return (d, x2) => ascending_default(f(d), x2);
  }

  // node_modules/d3-array/src/number.js
  function number_default(x2) {
    return x2 === null ? NaN : +x2;
  }

  // node_modules/d3-array/src/bisect.js
  var ascendingBisect = bisector_default(ascending_default);
  var bisectRight = ascendingBisect.right;
  var bisectLeft = ascendingBisect.left;
  var bisectCenter = bisector_default(number_default).center;
  var bisect_default = bisectRight;

  // node_modules/d3-array/src/ticks.js
  var e10 = Math.sqrt(50);
  var e5 = Math.sqrt(10);
  var e2 = Math.sqrt(2);
  function ticks_default(start2, stop, count) {
    var reverse3, i = -1, n, ticks, step;
    stop = +stop, start2 = +start2, count = +count;
    if (start2 === stop && count > 0)
      return [start2];
    if (reverse3 = stop < start2)
      n = start2, start2 = stop, stop = n;
    if ((step = tickIncrement(start2, stop, count)) === 0 || !isFinite(step))
      return [];
    if (step > 0) {
      let r0 = Math.round(start2 / step), r1 = Math.round(stop / step);
      if (r0 * step < start2)
        ++r0;
      if (r1 * step > stop)
        --r1;
      ticks = new Array(n = r1 - r0 + 1);
      while (++i < n)
        ticks[i] = (r0 + i) * step;
    } else {
      step = -step;
      let r0 = Math.round(start2 * step), r1 = Math.round(stop * step);
      if (r0 / step < start2)
        ++r0;
      if (r1 / step > stop)
        --r1;
      ticks = new Array(n = r1 - r0 + 1);
      while (++i < n)
        ticks[i] = (r0 + i) / step;
    }
    if (reverse3)
      ticks.reverse();
    return ticks;
  }
  function tickIncrement(start2, stop, count) {
    var step = (stop - start2) / Math.max(0, count), power = Math.floor(Math.log(step) / Math.LN10), error4 = step / Math.pow(10, power);
    return power >= 0 ? (error4 >= e10 ? 10 : error4 >= e5 ? 5 : error4 >= e2 ? 2 : 1) * Math.pow(10, power) : -Math.pow(10, -power) / (error4 >= e10 ? 10 : error4 >= e5 ? 5 : error4 >= e2 ? 2 : 1);
  }
  function tickStep(start2, stop, count) {
    var step0 = Math.abs(stop - start2) / Math.max(0, count), step1 = Math.pow(10, Math.floor(Math.log(step0) / Math.LN10)), error4 = step0 / step1;
    if (error4 >= e10)
      step1 *= 10;
    else if (error4 >= e5)
      step1 *= 5;
    else if (error4 >= e2)
      step1 *= 2;
    return stop < start2 ? -step1 : step1;
  }

  // node_modules/d3-array/src/range.js
  function range_default(start2, stop, step) {
    start2 = +start2, stop = +stop, step = (n = arguments.length) < 2 ? (stop = start2, start2 = 0, 1) : n < 3 ? 1 : +step;
    var i = -1, n = Math.max(0, Math.ceil((stop - start2) / step)) | 0, range3 = new Array(n);
    while (++i < n) {
      range3[i] = start2 + i * step;
    }
    return range3;
  }

  // node_modules/d3-axis/src/array.js
  var slice3 = Array.prototype.slice;

  // node_modules/d3-axis/src/identity.js
  function identity_default(x2) {
    return x2;
  }

  // node_modules/d3-axis/src/axis.js
  var top = 1;
  var right = 2;
  var bottom = 3;
  var left = 4;
  var epsilon = 1e-6;
  function translateX(x2) {
    return "translate(" + x2 + ",0)";
  }
  function translateY(y2) {
    return "translate(0," + y2 + ")";
  }
  function number2(scale) {
    return (d) => +scale(d);
  }
  function center(scale, offset) {
    offset = Math.max(0, scale.bandwidth() - offset * 2) / 2;
    if (scale.round())
      offset = Math.round(offset);
    return (d) => +scale(d) + offset;
  }
  function entering() {
    return !this.__axis;
  }
  function axis(orient, scale) {
    var tickArguments = [], tickValues = null, tickFormat2 = null, tickSizeInner = 6, tickSizeOuter = 6, tickPadding = 3, offset = typeof window !== "undefined" && window.devicePixelRatio > 1 ? 0 : 0.5, k = orient === top || orient === left ? -1 : 1, x2 = orient === left || orient === right ? "x" : "y", transform2 = orient === top || orient === bottom ? translateX : translateY;
    function axis2(context) {
      var values2 = tickValues == null ? scale.ticks ? scale.ticks.apply(scale, tickArguments) : scale.domain() : tickValues, format2 = tickFormat2 == null ? scale.tickFormat ? scale.tickFormat.apply(scale, tickArguments) : identity_default : tickFormat2, spacing = Math.max(tickSizeInner, 0) + tickPadding, range3 = scale.range(), range0 = +range3[0] + offset, range1 = +range3[range3.length - 1] + offset, position2 = (scale.bandwidth ? center : number2)(scale.copy(), offset), selection3 = context.selection ? context.selection() : context, path2 = selection3.selectAll(".domain").data([null]), tick = selection3.selectAll(".tick").data(values2, scale).order(), tickExit = tick.exit(), tickEnter = tick.enter().append("g").attr("class", "tick"), line = tick.select("line"), text2 = tick.select("text");
      path2 = path2.merge(path2.enter().insert("path", ".tick").attr("class", "domain").attr("stroke", "currentColor"));
      tick = tick.merge(tickEnter);
      line = line.merge(tickEnter.append("line").attr("stroke", "currentColor").attr(x2 + "2", k * tickSizeInner));
      text2 = text2.merge(tickEnter.append("text").attr("fill", "currentColor").attr(x2, k * spacing).attr("dy", orient === top ? "0em" : orient === bottom ? "0.71em" : "0.32em"));
      if (context !== selection3) {
        path2 = path2.transition(context);
        tick = tick.transition(context);
        line = line.transition(context);
        text2 = text2.transition(context);
        tickExit = tickExit.transition(context).attr("opacity", epsilon).attr("transform", function(d) {
          return isFinite(d = position2(d)) ? transform2(d + offset) : this.getAttribute("transform");
        });
        tickEnter.attr("opacity", epsilon).attr("transform", function(d) {
          var p = this.parentNode.__axis;
          return transform2((p && isFinite(p = p(d)) ? p : position2(d)) + offset);
        });
      }
      tickExit.remove();
      path2.attr("d", orient === left || orient === right ? tickSizeOuter ? "M" + k * tickSizeOuter + "," + range0 + "H" + offset + "V" + range1 + "H" + k * tickSizeOuter : "M" + offset + "," + range0 + "V" + range1 : tickSizeOuter ? "M" + range0 + "," + k * tickSizeOuter + "V" + offset + "H" + range1 + "V" + k * tickSizeOuter : "M" + range0 + "," + offset + "H" + range1);
      tick.attr("opacity", 1).attr("transform", function(d) {
        return transform2(position2(d) + offset);
      });
      line.attr(x2 + "2", k * tickSizeInner);
      text2.attr(x2, k * spacing).text(format2);
      selection3.filter(entering).attr("fill", "none").attr("font-size", 10).attr("font-family", "sans-serif").attr("text-anchor", orient === right ? "start" : orient === left ? "end" : "middle");
      selection3.each(function() {
        this.__axis = position2;
      });
    }
    axis2.scale = function(_) {
      return arguments.length ? (scale = _, axis2) : scale;
    };
    axis2.ticks = function() {
      return tickArguments = slice3.call(arguments), axis2;
    };
    axis2.tickArguments = function(_) {
      return arguments.length ? (tickArguments = _ == null ? [] : slice3.call(_), axis2) : tickArguments.slice();
    };
    axis2.tickValues = function(_) {
      return arguments.length ? (tickValues = _ == null ? null : slice3.call(_), axis2) : tickValues && tickValues.slice();
    };
    axis2.tickFormat = function(_) {
      return arguments.length ? (tickFormat2 = _, axis2) : tickFormat2;
    };
    axis2.tickSize = function(_) {
      return arguments.length ? (tickSizeInner = tickSizeOuter = +_, axis2) : tickSizeInner;
    };
    axis2.tickSizeInner = function(_) {
      return arguments.length ? (tickSizeInner = +_, axis2) : tickSizeInner;
    };
    axis2.tickSizeOuter = function(_) {
      return arguments.length ? (tickSizeOuter = +_, axis2) : tickSizeOuter;
    };
    axis2.tickPadding = function(_) {
      return arguments.length ? (tickPadding = +_, axis2) : tickPadding;
    };
    axis2.offset = function(_) {
      return arguments.length ? (offset = +_, axis2) : offset;
    };
    return axis2;
  }
  function axisBottom(scale) {
    return axis(bottom, scale);
  }
  function axisLeft(scale) {
    return axis(left, scale);
  }

  // node_modules/d3-dispatch/src/dispatch.js
  var noop = { value: () => {
  } };
  function dispatch() {
    for (var i = 0, n = arguments.length, _ = {}, t; i < n; ++i) {
      if (!(t = arguments[i] + "") || t in _ || /[\s.]/.test(t))
        throw new Error("illegal type: " + t);
      _[t] = [];
    }
    return new Dispatch(_);
  }
  function Dispatch(_) {
    this._ = _;
  }
  function parseTypenames(typenames, types) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name2 = "", i = t.indexOf(".");
      if (i >= 0)
        name2 = t.slice(i + 1), t = t.slice(0, i);
      if (t && !types.hasOwnProperty(t))
        throw new Error("unknown type: " + t);
      return { type: t, name: name2 };
    });
  }
  Dispatch.prototype = dispatch.prototype = {
    constructor: Dispatch,
    on: function(typename, callback) {
      var _ = this._, T = parseTypenames(typename + "", _), t, i = -1, n = T.length;
      if (arguments.length < 2) {
        while (++i < n)
          if ((t = (typename = T[i]).type) && (t = get(_[t], typename.name)))
            return t;
        return;
      }
      if (callback != null && typeof callback !== "function")
        throw new Error("invalid callback: " + callback);
      while (++i < n) {
        if (t = (typename = T[i]).type)
          _[t] = set(_[t], typename.name, callback);
        else if (callback == null)
          for (t in _)
            _[t] = set(_[t], typename.name, null);
      }
      return this;
    },
    copy: function() {
      var copy2 = {}, _ = this._;
      for (var t in _)
        copy2[t] = _[t].slice();
      return new Dispatch(copy2);
    },
    call: function(type2, that) {
      if ((n = arguments.length - 2) > 0)
        for (var args = new Array(n), i = 0, n, t; i < n; ++i)
          args[i] = arguments[i + 2];
      if (!this._.hasOwnProperty(type2))
        throw new Error("unknown type: " + type2);
      for (t = this._[type2], i = 0, n = t.length; i < n; ++i)
        t[i].value.apply(that, args);
    },
    apply: function(type2, that, args) {
      if (!this._.hasOwnProperty(type2))
        throw new Error("unknown type: " + type2);
      for (var t = this._[type2], i = 0, n = t.length; i < n; ++i)
        t[i].value.apply(that, args);
    }
  };
  function get(type2, name2) {
    for (var i = 0, n = type2.length, c; i < n; ++i) {
      if ((c = type2[i]).name === name2) {
        return c.value;
      }
    }
  }
  function set(type2, name2, callback) {
    for (var i = 0, n = type2.length; i < n; ++i) {
      if (type2[i].name === name2) {
        type2[i] = noop, type2 = type2.slice(0, i).concat(type2.slice(i + 1));
        break;
      }
    }
    if (callback != null)
      type2.push({ name: name2, value: callback });
    return type2;
  }
  var dispatch_default = dispatch;

  // node_modules/d3-selection/src/namespaces.js
  var xhtml = "http://www.w3.org/1999/xhtml";
  var namespaces_default = {
    svg: "http://www.w3.org/2000/svg",
    xhtml,
    xlink: "http://www.w3.org/1999/xlink",
    xml: "http://www.w3.org/XML/1998/namespace",
    xmlns: "http://www.w3.org/2000/xmlns/"
  };

  // node_modules/d3-selection/src/namespace.js
  function namespace_default(name2) {
    var prefix2 = name2 += "", i = prefix2.indexOf(":");
    if (i >= 0 && (prefix2 = name2.slice(0, i)) !== "xmlns")
      name2 = name2.slice(i + 1);
    return namespaces_default.hasOwnProperty(prefix2) ? { space: namespaces_default[prefix2], local: name2 } : name2;
  }

  // node_modules/d3-selection/src/creator.js
  function creatorInherit(name2) {
    return function() {
      var document2 = this.ownerDocument, uri = this.namespaceURI;
      return uri === xhtml && document2.documentElement.namespaceURI === xhtml ? document2.createElement(name2) : document2.createElementNS(uri, name2);
    };
  }
  function creatorFixed(fullname) {
    return function() {
      return this.ownerDocument.createElementNS(fullname.space, fullname.local);
    };
  }
  function creator_default(name2) {
    var fullname = namespace_default(name2);
    return (fullname.local ? creatorFixed : creatorInherit)(fullname);
  }

  // node_modules/d3-selection/src/selector.js
  function none() {
  }
  function selector_default(selector) {
    return selector == null ? none : function() {
      return this.querySelector(selector);
    };
  }

  // node_modules/d3-selection/src/selection/select.js
  function select_default(select) {
    if (typeof select !== "function")
      select = selector_default(select);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, subgroup = subgroups[j] = new Array(n), node, subnode, i = 0; i < n; ++i) {
        if ((node = group2[i]) && (subnode = select.call(node, node.__data__, i, group2))) {
          if ("__data__" in node)
            subnode.__data__ = node.__data__;
          subgroup[i] = subnode;
        }
      }
    }
    return new Selection(subgroups, this._parents);
  }

  // node_modules/d3-selection/src/array.js
  function array_default(x2) {
    return typeof x2 === "object" && "length" in x2 ? x2 : Array.from(x2);
  }

  // node_modules/d3-selection/src/selectorAll.js
  function empty3() {
    return [];
  }
  function selectorAll_default(selector) {
    return selector == null ? empty3 : function() {
      return this.querySelectorAll(selector);
    };
  }

  // node_modules/d3-selection/src/selection/selectAll.js
  function arrayAll(select) {
    return function() {
      var group2 = select.apply(this, arguments);
      return group2 == null ? [] : array_default(group2);
    };
  }
  function selectAll_default(select) {
    if (typeof select === "function")
      select = arrayAll(select);
    else
      select = selectorAll_default(select);
    for (var groups = this._groups, m = groups.length, subgroups = [], parents = [], j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, node, i = 0; i < n; ++i) {
        if (node = group2[i]) {
          subgroups.push(select.call(node, node.__data__, i, group2));
          parents.push(node);
        }
      }
    }
    return new Selection(subgroups, parents);
  }

  // node_modules/d3-selection/src/matcher.js
  function matcher_default(selector) {
    return function() {
      return this.matches(selector);
    };
  }
  function childMatcher(selector) {
    return function(node) {
      return node.matches(selector);
    };
  }

  // node_modules/d3-selection/src/selection/selectChild.js
  var find = Array.prototype.find;
  function childFind(match10) {
    return function() {
      return find.call(this.children, match10);
    };
  }
  function childFirst() {
    return this.firstElementChild;
  }
  function selectChild_default(match10) {
    return this.select(match10 == null ? childFirst : childFind(typeof match10 === "function" ? match10 : childMatcher(match10)));
  }

  // node_modules/d3-selection/src/selection/selectChildren.js
  var filter3 = Array.prototype.filter;
  function children() {
    return this.children;
  }
  function childrenFilter(match10) {
    return function() {
      return filter3.call(this.children, match10);
    };
  }
  function selectChildren_default(match10) {
    return this.selectAll(match10 == null ? children : childrenFilter(typeof match10 === "function" ? match10 : childMatcher(match10)));
  }

  // node_modules/d3-selection/src/selection/filter.js
  function filter_default(match10) {
    if (typeof match10 !== "function")
      match10 = matcher_default(match10);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, subgroup = subgroups[j] = [], node, i = 0; i < n; ++i) {
        if ((node = group2[i]) && match10.call(node, node.__data__, i, group2)) {
          subgroup.push(node);
        }
      }
    }
    return new Selection(subgroups, this._parents);
  }

  // node_modules/d3-selection/src/selection/sparse.js
  function sparse_default(update2) {
    return new Array(update2.length);
  }

  // node_modules/d3-selection/src/selection/enter.js
  function enter_default() {
    return new Selection(this._enter || this._groups.map(sparse_default), this._parents);
  }
  function EnterNode(parent, datum2) {
    this.ownerDocument = parent.ownerDocument;
    this.namespaceURI = parent.namespaceURI;
    this._next = null;
    this._parent = parent;
    this.__data__ = datum2;
  }
  EnterNode.prototype = {
    constructor: EnterNode,
    appendChild: function(child) {
      return this._parent.insertBefore(child, this._next);
    },
    insertBefore: function(child, next) {
      return this._parent.insertBefore(child, next);
    },
    querySelector: function(selector) {
      return this._parent.querySelector(selector);
    },
    querySelectorAll: function(selector) {
      return this._parent.querySelectorAll(selector);
    }
  };

  // node_modules/d3-selection/src/constant.js
  function constant_default(x2) {
    return function() {
      return x2;
    };
  }

  // node_modules/d3-selection/src/selection/data.js
  function bindIndex(parent, group2, enter, update2, exit, data) {
    var i = 0, node, groupLength = group2.length, dataLength = data.length;
    for (; i < dataLength; ++i) {
      if (node = group2[i]) {
        node.__data__ = data[i];
        update2[i] = node;
      } else {
        enter[i] = new EnterNode(parent, data[i]);
      }
    }
    for (; i < groupLength; ++i) {
      if (node = group2[i]) {
        exit[i] = node;
      }
    }
  }
  function bindKey(parent, group2, enter, update2, exit, data, key) {
    var i, node, nodeByKeyValue = /* @__PURE__ */ new Map(), groupLength = group2.length, dataLength = data.length, keyValues = new Array(groupLength), keyValue;
    for (i = 0; i < groupLength; ++i) {
      if (node = group2[i]) {
        keyValues[i] = keyValue = key.call(node, node.__data__, i, group2) + "";
        if (nodeByKeyValue.has(keyValue)) {
          exit[i] = node;
        } else {
          nodeByKeyValue.set(keyValue, node);
        }
      }
    }
    for (i = 0; i < dataLength; ++i) {
      keyValue = key.call(parent, data[i], i, data) + "";
      if (node = nodeByKeyValue.get(keyValue)) {
        update2[i] = node;
        node.__data__ = data[i];
        nodeByKeyValue.delete(keyValue);
      } else {
        enter[i] = new EnterNode(parent, data[i]);
      }
    }
    for (i = 0; i < groupLength; ++i) {
      if ((node = group2[i]) && nodeByKeyValue.get(keyValues[i]) === node) {
        exit[i] = node;
      }
    }
  }
  function datum(node) {
    return node.__data__;
  }
  function data_default(value, key) {
    if (!arguments.length)
      return Array.from(this, datum);
    var bind = key ? bindKey : bindIndex, parents = this._parents, groups = this._groups;
    if (typeof value !== "function")
      value = constant_default(value);
    for (var m = groups.length, update2 = new Array(m), enter = new Array(m), exit = new Array(m), j = 0; j < m; ++j) {
      var parent = parents[j], group2 = groups[j], groupLength = group2.length, data = array_default(value.call(parent, parent && parent.__data__, j, parents)), dataLength = data.length, enterGroup = enter[j] = new Array(dataLength), updateGroup = update2[j] = new Array(dataLength), exitGroup = exit[j] = new Array(groupLength);
      bind(parent, group2, enterGroup, updateGroup, exitGroup, data, key);
      for (var i0 = 0, i1 = 0, previous, next; i0 < dataLength; ++i0) {
        if (previous = enterGroup[i0]) {
          if (i0 >= i1)
            i1 = i0 + 1;
          while (!(next = updateGroup[i1]) && ++i1 < dataLength)
            ;
          previous._next = next || null;
        }
      }
    }
    update2 = new Selection(update2, parents);
    update2._enter = enter;
    update2._exit = exit;
    return update2;
  }

  // node_modules/d3-selection/src/selection/exit.js
  function exit_default() {
    return new Selection(this._exit || this._groups.map(sparse_default), this._parents);
  }

  // node_modules/d3-selection/src/selection/join.js
  function join_default(onenter, onupdate, onexit) {
    var enter = this.enter(), update2 = this, exit = this.exit();
    enter = typeof onenter === "function" ? onenter(enter) : enter.append(onenter + "");
    if (onupdate != null)
      update2 = onupdate(update2);
    if (onexit == null)
      exit.remove();
    else
      onexit(exit);
    return enter && update2 ? enter.merge(update2).order() : update2;
  }

  // node_modules/d3-selection/src/selection/merge.js
  function merge_default(selection3) {
    if (!(selection3 instanceof Selection))
      throw new Error("invalid merge");
    for (var groups0 = this._groups, groups1 = selection3._groups, m0 = groups0.length, m1 = groups1.length, m = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m; ++j) {
      for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group0[i] || group1[i]) {
          merge[i] = node;
        }
      }
    }
    for (; j < m0; ++j) {
      merges[j] = groups0[j];
    }
    return new Selection(merges, this._parents);
  }

  // node_modules/d3-selection/src/selection/order.js
  function order_default() {
    for (var groups = this._groups, j = -1, m = groups.length; ++j < m; ) {
      for (var group2 = groups[j], i = group2.length - 1, next = group2[i], node; --i >= 0; ) {
        if (node = group2[i]) {
          if (next && node.compareDocumentPosition(next) ^ 4)
            next.parentNode.insertBefore(node, next);
          next = node;
        }
      }
    }
    return this;
  }

  // node_modules/d3-selection/src/selection/sort.js
  function sort_default(compare) {
    if (!compare)
      compare = ascending;
    function compareNode(a, b) {
      return a && b ? compare(a.__data__, b.__data__) : !a - !b;
    }
    for (var groups = this._groups, m = groups.length, sortgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, sortgroup = sortgroups[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group2[i]) {
          sortgroup[i] = node;
        }
      }
      sortgroup.sort(compareNode);
    }
    return new Selection(sortgroups, this._parents).order();
  }
  function ascending(a, b) {
    return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
  }

  // node_modules/d3-selection/src/selection/call.js
  function call_default() {
    var callback = arguments[0];
    arguments[0] = this;
    callback.apply(null, arguments);
    return this;
  }

  // node_modules/d3-selection/src/selection/nodes.js
  function nodes_default() {
    return Array.from(this);
  }

  // node_modules/d3-selection/src/selection/node.js
  function node_default() {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group2 = groups[j], i = 0, n = group2.length; i < n; ++i) {
        var node = group2[i];
        if (node)
          return node;
      }
    }
    return null;
  }

  // node_modules/d3-selection/src/selection/size.js
  function size_default() {
    let size3 = 0;
    for (const node of this)
      ++size3;
    return size3;
  }

  // node_modules/d3-selection/src/selection/empty.js
  function empty_default() {
    return !this.node();
  }

  // node_modules/d3-selection/src/selection/each.js
  function each_default(callback) {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group2 = groups[j], i = 0, n = group2.length, node; i < n; ++i) {
        if (node = group2[i])
          callback.call(node, node.__data__, i, group2);
      }
    }
    return this;
  }

  // node_modules/d3-selection/src/selection/attr.js
  function attrRemove(name2) {
    return function() {
      this.removeAttribute(name2);
    };
  }
  function attrRemoveNS(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant(name2, value) {
    return function() {
      this.setAttribute(name2, value);
    };
  }
  function attrConstantNS(fullname, value) {
    return function() {
      this.setAttributeNS(fullname.space, fullname.local, value);
    };
  }
  function attrFunction(name2, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        this.removeAttribute(name2);
      else
        this.setAttribute(name2, v);
    };
  }
  function attrFunctionNS(fullname, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        this.removeAttributeNS(fullname.space, fullname.local);
      else
        this.setAttributeNS(fullname.space, fullname.local, v);
    };
  }
  function attr_default(name2, value) {
    var fullname = namespace_default(name2);
    if (arguments.length < 2) {
      var node = this.node();
      return fullname.local ? node.getAttributeNS(fullname.space, fullname.local) : node.getAttribute(fullname);
    }
    return this.each((value == null ? fullname.local ? attrRemoveNS : attrRemove : typeof value === "function" ? fullname.local ? attrFunctionNS : attrFunction : fullname.local ? attrConstantNS : attrConstant)(fullname, value));
  }

  // node_modules/d3-selection/src/window.js
  function window_default(node) {
    return node.ownerDocument && node.ownerDocument.defaultView || node.document && node || node.defaultView;
  }

  // node_modules/d3-selection/src/selection/style.js
  function styleRemove(name2) {
    return function() {
      this.style.removeProperty(name2);
    };
  }
  function styleConstant(name2, value, priority) {
    return function() {
      this.style.setProperty(name2, value, priority);
    };
  }
  function styleFunction(name2, value, priority) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        this.style.removeProperty(name2);
      else
        this.style.setProperty(name2, v, priority);
    };
  }
  function style_default(name2, value, priority) {
    return arguments.length > 1 ? this.each((value == null ? styleRemove : typeof value === "function" ? styleFunction : styleConstant)(name2, value, priority == null ? "" : priority)) : styleValue(this.node(), name2);
  }
  function styleValue(node, name2) {
    return node.style.getPropertyValue(name2) || window_default(node).getComputedStyle(node, null).getPropertyValue(name2);
  }

  // node_modules/d3-selection/src/selection/property.js
  function propertyRemove(name2) {
    return function() {
      delete this[name2];
    };
  }
  function propertyConstant(name2, value) {
    return function() {
      this[name2] = value;
    };
  }
  function propertyFunction(name2, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        delete this[name2];
      else
        this[name2] = v;
    };
  }
  function property_default(name2, value) {
    return arguments.length > 1 ? this.each((value == null ? propertyRemove : typeof value === "function" ? propertyFunction : propertyConstant)(name2, value)) : this.node()[name2];
  }

  // node_modules/d3-selection/src/selection/classed.js
  function classArray(string3) {
    return string3.trim().split(/^|\s+/);
  }
  function classList(node) {
    return node.classList || new ClassList(node);
  }
  function ClassList(node) {
    this._node = node;
    this._names = classArray(node.getAttribute("class") || "");
  }
  ClassList.prototype = {
    add: function(name2) {
      var i = this._names.indexOf(name2);
      if (i < 0) {
        this._names.push(name2);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    remove: function(name2) {
      var i = this._names.indexOf(name2);
      if (i >= 0) {
        this._names.splice(i, 1);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    contains: function(name2) {
      return this._names.indexOf(name2) >= 0;
    }
  };
  function classedAdd(node, names) {
    var list = classList(node), i = -1, n = names.length;
    while (++i < n)
      list.add(names[i]);
  }
  function classedRemove(node, names) {
    var list = classList(node), i = -1, n = names.length;
    while (++i < n)
      list.remove(names[i]);
  }
  function classedTrue(names) {
    return function() {
      classedAdd(this, names);
    };
  }
  function classedFalse(names) {
    return function() {
      classedRemove(this, names);
    };
  }
  function classedFunction(names, value) {
    return function() {
      (value.apply(this, arguments) ? classedAdd : classedRemove)(this, names);
    };
  }
  function classed_default(name2, value) {
    var names = classArray(name2 + "");
    if (arguments.length < 2) {
      var list = classList(this.node()), i = -1, n = names.length;
      while (++i < n)
        if (!list.contains(names[i]))
          return false;
      return true;
    }
    return this.each((typeof value === "function" ? classedFunction : value ? classedTrue : classedFalse)(names, value));
  }

  // node_modules/d3-selection/src/selection/text.js
  function textRemove() {
    this.textContent = "";
  }
  function textConstant(value) {
    return function() {
      this.textContent = value;
    };
  }
  function textFunction(value) {
    return function() {
      var v = value.apply(this, arguments);
      this.textContent = v == null ? "" : v;
    };
  }
  function text_default(value) {
    return arguments.length ? this.each(value == null ? textRemove : (typeof value === "function" ? textFunction : textConstant)(value)) : this.node().textContent;
  }

  // node_modules/d3-selection/src/selection/html.js
  function htmlRemove() {
    this.innerHTML = "";
  }
  function htmlConstant(value) {
    return function() {
      this.innerHTML = value;
    };
  }
  function htmlFunction(value) {
    return function() {
      var v = value.apply(this, arguments);
      this.innerHTML = v == null ? "" : v;
    };
  }
  function html_default(value) {
    return arguments.length ? this.each(value == null ? htmlRemove : (typeof value === "function" ? htmlFunction : htmlConstant)(value)) : this.node().innerHTML;
  }

  // node_modules/d3-selection/src/selection/raise.js
  function raise() {
    if (this.nextSibling)
      this.parentNode.appendChild(this);
  }
  function raise_default() {
    return this.each(raise);
  }

  // node_modules/d3-selection/src/selection/lower.js
  function lower() {
    if (this.previousSibling)
      this.parentNode.insertBefore(this, this.parentNode.firstChild);
  }
  function lower_default() {
    return this.each(lower);
  }

  // node_modules/d3-selection/src/selection/append.js
  function append_default(name2) {
    var create2 = typeof name2 === "function" ? name2 : creator_default(name2);
    return this.select(function() {
      return this.appendChild(create2.apply(this, arguments));
    });
  }

  // node_modules/d3-selection/src/selection/insert.js
  function constantNull() {
    return null;
  }
  function insert_default(name2, before) {
    var create2 = typeof name2 === "function" ? name2 : creator_default(name2), select = before == null ? constantNull : typeof before === "function" ? before : selector_default(before);
    return this.select(function() {
      return this.insertBefore(create2.apply(this, arguments), select.apply(this, arguments) || null);
    });
  }

  // node_modules/d3-selection/src/selection/remove.js
  function remove() {
    var parent = this.parentNode;
    if (parent)
      parent.removeChild(this);
  }
  function remove_default() {
    return this.each(remove);
  }

  // node_modules/d3-selection/src/selection/clone.js
  function selection_cloneShallow() {
    var clone = this.cloneNode(false), parent = this.parentNode;
    return parent ? parent.insertBefore(clone, this.nextSibling) : clone;
  }
  function selection_cloneDeep() {
    var clone = this.cloneNode(true), parent = this.parentNode;
    return parent ? parent.insertBefore(clone, this.nextSibling) : clone;
  }
  function clone_default(deep) {
    return this.select(deep ? selection_cloneDeep : selection_cloneShallow);
  }

  // node_modules/d3-selection/src/selection/datum.js
  function datum_default(value) {
    return arguments.length ? this.property("__data__", value) : this.node().__data__;
  }

  // node_modules/d3-selection/src/selection/on.js
  function contextListener(listener) {
    return function(event2) {
      listener.call(this, event2, this.__data__);
    };
  }
  function parseTypenames2(typenames) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name2 = "", i = t.indexOf(".");
      if (i >= 0)
        name2 = t.slice(i + 1), t = t.slice(0, i);
      return { type: t, name: name2 };
    });
  }
  function onRemove(typename) {
    return function() {
      var on = this.__on;
      if (!on)
        return;
      for (var j = 0, i = -1, m = on.length, o; j < m; ++j) {
        if (o = on[j], (!typename.type || o.type === typename.type) && o.name === typename.name) {
          this.removeEventListener(o.type, o.listener, o.options);
        } else {
          on[++i] = o;
        }
      }
      if (++i)
        on.length = i;
      else
        delete this.__on;
    };
  }
  function onAdd(typename, value, options) {
    return function() {
      var on = this.__on, o, listener = contextListener(value);
      if (on)
        for (var j = 0, m = on.length; j < m; ++j) {
          if ((o = on[j]).type === typename.type && o.name === typename.name) {
            this.removeEventListener(o.type, o.listener, o.options);
            this.addEventListener(o.type, o.listener = listener, o.options = options);
            o.value = value;
            return;
          }
        }
      this.addEventListener(typename.type, listener, options);
      o = { type: typename.type, name: typename.name, value, listener, options };
      if (!on)
        this.__on = [o];
      else
        on.push(o);
    };
  }
  function on_default(typename, value, options) {
    var typenames = parseTypenames2(typename + ""), i, n = typenames.length, t;
    if (arguments.length < 2) {
      var on = this.node().__on;
      if (on)
        for (var j = 0, m = on.length, o; j < m; ++j) {
          for (i = 0, o = on[j]; i < n; ++i) {
            if ((t = typenames[i]).type === o.type && t.name === o.name) {
              return o.value;
            }
          }
        }
      return;
    }
    on = value ? onAdd : onRemove;
    for (i = 0; i < n; ++i)
      this.each(on(typenames[i], value, options));
    return this;
  }

  // node_modules/d3-selection/src/selection/dispatch.js
  function dispatchEvent(node, type2, params) {
    var window2 = window_default(node), event2 = window2.CustomEvent;
    if (typeof event2 === "function") {
      event2 = new event2(type2, params);
    } else {
      event2 = window2.document.createEvent("Event");
      if (params)
        event2.initEvent(type2, params.bubbles, params.cancelable), event2.detail = params.detail;
      else
        event2.initEvent(type2, false, false);
    }
    node.dispatchEvent(event2);
  }
  function dispatchConstant(type2, params) {
    return function() {
      return dispatchEvent(this, type2, params);
    };
  }
  function dispatchFunction(type2, params) {
    return function() {
      return dispatchEvent(this, type2, params.apply(this, arguments));
    };
  }
  function dispatch_default2(type2, params) {
    return this.each((typeof params === "function" ? dispatchFunction : dispatchConstant)(type2, params));
  }

  // node_modules/d3-selection/src/selection/iterator.js
  function* iterator_default() {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group2 = groups[j], i = 0, n = group2.length, node; i < n; ++i) {
        if (node = group2[i])
          yield node;
      }
    }
  }

  // node_modules/d3-selection/src/selection/index.js
  var root = [null];
  function Selection(groups, parents) {
    this._groups = groups;
    this._parents = parents;
  }
  function selection() {
    return new Selection([[document.documentElement]], root);
  }
  function selection_selection() {
    return this;
  }
  Selection.prototype = selection.prototype = {
    constructor: Selection,
    select: select_default,
    selectAll: selectAll_default,
    selectChild: selectChild_default,
    selectChildren: selectChildren_default,
    filter: filter_default,
    data: data_default,
    enter: enter_default,
    exit: exit_default,
    join: join_default,
    merge: merge_default,
    selection: selection_selection,
    order: order_default,
    sort: sort_default,
    call: call_default,
    nodes: nodes_default,
    node: node_default,
    size: size_default,
    empty: empty_default,
    each: each_default,
    attr: attr_default,
    style: style_default,
    property: property_default,
    classed: classed_default,
    text: text_default,
    html: html_default,
    raise: raise_default,
    lower: lower_default,
    append: append_default,
    insert: insert_default,
    remove: remove_default,
    clone: clone_default,
    datum: datum_default,
    on: on_default,
    dispatch: dispatch_default2,
    [Symbol.iterator]: iterator_default
  };
  var selection_default = selection;

  // node_modules/d3-selection/src/select.js
  function select_default2(selector) {
    return typeof selector === "string" ? new Selection([[document.querySelector(selector)]], [document.documentElement]) : new Selection([[selector]], root);
  }

  // node_modules/d3-color/src/define.js
  function define_default(constructor, factory, prototype) {
    constructor.prototype = factory.prototype = prototype;
    prototype.constructor = constructor;
  }
  function extend(parent, definition) {
    var prototype = Object.create(parent.prototype);
    for (var key in definition)
      prototype[key] = definition[key];
    return prototype;
  }

  // node_modules/d3-color/src/color.js
  function Color() {
  }
  var darker = 0.7;
  var brighter = 1 / darker;
  var reI = "\\s*([+-]?\\d+)\\s*";
  var reN = "\\s*([+-]?\\d*\\.?\\d+(?:[eE][+-]?\\d+)?)\\s*";
  var reP = "\\s*([+-]?\\d*\\.?\\d+(?:[eE][+-]?\\d+)?)%\\s*";
  var reHex = /^#([0-9a-f]{3,8})$/;
  var reRgbInteger = new RegExp("^rgb\\(" + [reI, reI, reI] + "\\)$");
  var reRgbPercent = new RegExp("^rgb\\(" + [reP, reP, reP] + "\\)$");
  var reRgbaInteger = new RegExp("^rgba\\(" + [reI, reI, reI, reN] + "\\)$");
  var reRgbaPercent = new RegExp("^rgba\\(" + [reP, reP, reP, reN] + "\\)$");
  var reHslPercent = new RegExp("^hsl\\(" + [reN, reP, reP] + "\\)$");
  var reHslaPercent = new RegExp("^hsla\\(" + [reN, reP, reP, reN] + "\\)$");
  var named = {
    aliceblue: 15792383,
    antiquewhite: 16444375,
    aqua: 65535,
    aquamarine: 8388564,
    azure: 15794175,
    beige: 16119260,
    bisque: 16770244,
    black: 0,
    blanchedalmond: 16772045,
    blue: 255,
    blueviolet: 9055202,
    brown: 10824234,
    burlywood: 14596231,
    cadetblue: 6266528,
    chartreuse: 8388352,
    chocolate: 13789470,
    coral: 16744272,
    cornflowerblue: 6591981,
    cornsilk: 16775388,
    crimson: 14423100,
    cyan: 65535,
    darkblue: 139,
    darkcyan: 35723,
    darkgoldenrod: 12092939,
    darkgray: 11119017,
    darkgreen: 25600,
    darkgrey: 11119017,
    darkkhaki: 12433259,
    darkmagenta: 9109643,
    darkolivegreen: 5597999,
    darkorange: 16747520,
    darkorchid: 10040012,
    darkred: 9109504,
    darksalmon: 15308410,
    darkseagreen: 9419919,
    darkslateblue: 4734347,
    darkslategray: 3100495,
    darkslategrey: 3100495,
    darkturquoise: 52945,
    darkviolet: 9699539,
    deeppink: 16716947,
    deepskyblue: 49151,
    dimgray: 6908265,
    dimgrey: 6908265,
    dodgerblue: 2003199,
    firebrick: 11674146,
    floralwhite: 16775920,
    forestgreen: 2263842,
    fuchsia: 16711935,
    gainsboro: 14474460,
    ghostwhite: 16316671,
    gold: 16766720,
    goldenrod: 14329120,
    gray: 8421504,
    green: 32768,
    greenyellow: 11403055,
    grey: 8421504,
    honeydew: 15794160,
    hotpink: 16738740,
    indianred: 13458524,
    indigo: 4915330,
    ivory: 16777200,
    khaki: 15787660,
    lavender: 15132410,
    lavenderblush: 16773365,
    lawngreen: 8190976,
    lemonchiffon: 16775885,
    lightblue: 11393254,
    lightcoral: 15761536,
    lightcyan: 14745599,
    lightgoldenrodyellow: 16448210,
    lightgray: 13882323,
    lightgreen: 9498256,
    lightgrey: 13882323,
    lightpink: 16758465,
    lightsalmon: 16752762,
    lightseagreen: 2142890,
    lightskyblue: 8900346,
    lightslategray: 7833753,
    lightslategrey: 7833753,
    lightsteelblue: 11584734,
    lightyellow: 16777184,
    lime: 65280,
    limegreen: 3329330,
    linen: 16445670,
    magenta: 16711935,
    maroon: 8388608,
    mediumaquamarine: 6737322,
    mediumblue: 205,
    mediumorchid: 12211667,
    mediumpurple: 9662683,
    mediumseagreen: 3978097,
    mediumslateblue: 8087790,
    mediumspringgreen: 64154,
    mediumturquoise: 4772300,
    mediumvioletred: 13047173,
    midnightblue: 1644912,
    mintcream: 16121850,
    mistyrose: 16770273,
    moccasin: 16770229,
    navajowhite: 16768685,
    navy: 128,
    oldlace: 16643558,
    olive: 8421376,
    olivedrab: 7048739,
    orange: 16753920,
    orangered: 16729344,
    orchid: 14315734,
    palegoldenrod: 15657130,
    palegreen: 10025880,
    paleturquoise: 11529966,
    palevioletred: 14381203,
    papayawhip: 16773077,
    peachpuff: 16767673,
    peru: 13468991,
    pink: 16761035,
    plum: 14524637,
    powderblue: 11591910,
    purple: 8388736,
    rebeccapurple: 6697881,
    red: 16711680,
    rosybrown: 12357519,
    royalblue: 4286945,
    saddlebrown: 9127187,
    salmon: 16416882,
    sandybrown: 16032864,
    seagreen: 3050327,
    seashell: 16774638,
    sienna: 10506797,
    silver: 12632256,
    skyblue: 8900331,
    slateblue: 6970061,
    slategray: 7372944,
    slategrey: 7372944,
    snow: 16775930,
    springgreen: 65407,
    steelblue: 4620980,
    tan: 13808780,
    teal: 32896,
    thistle: 14204888,
    tomato: 16737095,
    turquoise: 4251856,
    violet: 15631086,
    wheat: 16113331,
    white: 16777215,
    whitesmoke: 16119285,
    yellow: 16776960,
    yellowgreen: 10145074
  };
  define_default(Color, color, {
    copy: function(channels) {
      return Object.assign(new this.constructor(), this, channels);
    },
    displayable: function() {
      return this.rgb().displayable();
    },
    hex: color_formatHex,
    formatHex: color_formatHex,
    formatHsl: color_formatHsl,
    formatRgb: color_formatRgb,
    toString: color_formatRgb
  });
  function color_formatHex() {
    return this.rgb().formatHex();
  }
  function color_formatHsl() {
    return hslConvert(this).formatHsl();
  }
  function color_formatRgb() {
    return this.rgb().formatRgb();
  }
  function color(format2) {
    var m, l;
    format2 = (format2 + "").trim().toLowerCase();
    return (m = reHex.exec(format2)) ? (l = m[1].length, m = parseInt(m[1], 16), l === 6 ? rgbn(m) : l === 3 ? new Rgb(m >> 8 & 15 | m >> 4 & 240, m >> 4 & 15 | m & 240, (m & 15) << 4 | m & 15, 1) : l === 8 ? rgba(m >> 24 & 255, m >> 16 & 255, m >> 8 & 255, (m & 255) / 255) : l === 4 ? rgba(m >> 12 & 15 | m >> 8 & 240, m >> 8 & 15 | m >> 4 & 240, m >> 4 & 15 | m & 240, ((m & 15) << 4 | m & 15) / 255) : null) : (m = reRgbInteger.exec(format2)) ? new Rgb(m[1], m[2], m[3], 1) : (m = reRgbPercent.exec(format2)) ? new Rgb(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, 1) : (m = reRgbaInteger.exec(format2)) ? rgba(m[1], m[2], m[3], m[4]) : (m = reRgbaPercent.exec(format2)) ? rgba(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, m[4]) : (m = reHslPercent.exec(format2)) ? hsla(m[1], m[2] / 100, m[3] / 100, 1) : (m = reHslaPercent.exec(format2)) ? hsla(m[1], m[2] / 100, m[3] / 100, m[4]) : named.hasOwnProperty(format2) ? rgbn(named[format2]) : format2 === "transparent" ? new Rgb(NaN, NaN, NaN, 0) : null;
  }
  function rgbn(n) {
    return new Rgb(n >> 16 & 255, n >> 8 & 255, n & 255, 1);
  }
  function rgba(r, g, b, a) {
    if (a <= 0)
      r = g = b = NaN;
    return new Rgb(r, g, b, a);
  }
  function rgbConvert(o) {
    if (!(o instanceof Color))
      o = color(o);
    if (!o)
      return new Rgb();
    o = o.rgb();
    return new Rgb(o.r, o.g, o.b, o.opacity);
  }
  function rgb(r, g, b, opacity) {
    return arguments.length === 1 ? rgbConvert(r) : new Rgb(r, g, b, opacity == null ? 1 : opacity);
  }
  function Rgb(r, g, b, opacity) {
    this.r = +r;
    this.g = +g;
    this.b = +b;
    this.opacity = +opacity;
  }
  define_default(Rgb, rgb, extend(Color, {
    brighter: function(k) {
      k = k == null ? brighter : Math.pow(brighter, k);
      return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
    },
    darker: function(k) {
      k = k == null ? darker : Math.pow(darker, k);
      return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
    },
    rgb: function() {
      return this;
    },
    displayable: function() {
      return -0.5 <= this.r && this.r < 255.5 && (-0.5 <= this.g && this.g < 255.5) && (-0.5 <= this.b && this.b < 255.5) && (0 <= this.opacity && this.opacity <= 1);
    },
    hex: rgb_formatHex,
    formatHex: rgb_formatHex,
    formatRgb: rgb_formatRgb,
    toString: rgb_formatRgb
  }));
  function rgb_formatHex() {
    return "#" + hex(this.r) + hex(this.g) + hex(this.b);
  }
  function rgb_formatRgb() {
    var a = this.opacity;
    a = isNaN(a) ? 1 : Math.max(0, Math.min(1, a));
    return (a === 1 ? "rgb(" : "rgba(") + Math.max(0, Math.min(255, Math.round(this.r) || 0)) + ", " + Math.max(0, Math.min(255, Math.round(this.g) || 0)) + ", " + Math.max(0, Math.min(255, Math.round(this.b) || 0)) + (a === 1 ? ")" : ", " + a + ")");
  }
  function hex(value) {
    value = Math.max(0, Math.min(255, Math.round(value) || 0));
    return (value < 16 ? "0" : "") + value.toString(16);
  }
  function hsla(h, s, l, a) {
    if (a <= 0)
      h = s = l = NaN;
    else if (l <= 0 || l >= 1)
      h = s = NaN;
    else if (s <= 0)
      h = NaN;
    return new Hsl(h, s, l, a);
  }
  function hslConvert(o) {
    if (o instanceof Hsl)
      return new Hsl(o.h, o.s, o.l, o.opacity);
    if (!(o instanceof Color))
      o = color(o);
    if (!o)
      return new Hsl();
    if (o instanceof Hsl)
      return o;
    o = o.rgb();
    var r = o.r / 255, g = o.g / 255, b = o.b / 255, min3 = Math.min(r, g, b), max4 = Math.max(r, g, b), h = NaN, s = max4 - min3, l = (max4 + min3) / 2;
    if (s) {
      if (r === max4)
        h = (g - b) / s + (g < b) * 6;
      else if (g === max4)
        h = (b - r) / s + 2;
      else
        h = (r - g) / s + 4;
      s /= l < 0.5 ? max4 + min3 : 2 - max4 - min3;
      h *= 60;
    } else {
      s = l > 0 && l < 1 ? 0 : h;
    }
    return new Hsl(h, s, l, o.opacity);
  }
  function hsl(h, s, l, opacity) {
    return arguments.length === 1 ? hslConvert(h) : new Hsl(h, s, l, opacity == null ? 1 : opacity);
  }
  function Hsl(h, s, l, opacity) {
    this.h = +h;
    this.s = +s;
    this.l = +l;
    this.opacity = +opacity;
  }
  define_default(Hsl, hsl, extend(Color, {
    brighter: function(k) {
      k = k == null ? brighter : Math.pow(brighter, k);
      return new Hsl(this.h, this.s, this.l * k, this.opacity);
    },
    darker: function(k) {
      k = k == null ? darker : Math.pow(darker, k);
      return new Hsl(this.h, this.s, this.l * k, this.opacity);
    },
    rgb: function() {
      var h = this.h % 360 + (this.h < 0) * 360, s = isNaN(h) || isNaN(this.s) ? 0 : this.s, l = this.l, m2 = l + (l < 0.5 ? l : 1 - l) * s, m1 = 2 * l - m2;
      return new Rgb(
        hsl2rgb(h >= 240 ? h - 240 : h + 120, m1, m2),
        hsl2rgb(h, m1, m2),
        hsl2rgb(h < 120 ? h + 240 : h - 120, m1, m2),
        this.opacity
      );
    },
    displayable: function() {
      return (0 <= this.s && this.s <= 1 || isNaN(this.s)) && (0 <= this.l && this.l <= 1) && (0 <= this.opacity && this.opacity <= 1);
    },
    formatHsl: function() {
      var a = this.opacity;
      a = isNaN(a) ? 1 : Math.max(0, Math.min(1, a));
      return (a === 1 ? "hsl(" : "hsla(") + (this.h || 0) + ", " + (this.s || 0) * 100 + "%, " + (this.l || 0) * 100 + "%" + (a === 1 ? ")" : ", " + a + ")");
    }
  }));
  function hsl2rgb(h, m1, m2) {
    return (h < 60 ? m1 + (m2 - m1) * h / 60 : h < 180 ? m2 : h < 240 ? m1 + (m2 - m1) * (240 - h) / 60 : m1) * 255;
  }

  // node_modules/d3-interpolate/src/basis.js
  function basis(t1, v0, v1, v2, v3) {
    var t2 = t1 * t1, t3 = t2 * t1;
    return ((1 - 3 * t1 + 3 * t2 - t3) * v0 + (4 - 6 * t2 + 3 * t3) * v1 + (1 + 3 * t1 + 3 * t2 - 3 * t3) * v2 + t3 * v3) / 6;
  }
  function basis_default(values2) {
    var n = values2.length - 1;
    return function(t) {
      var i = t <= 0 ? t = 0 : t >= 1 ? (t = 1, n - 1) : Math.floor(t * n), v1 = values2[i], v2 = values2[i + 1], v0 = i > 0 ? values2[i - 1] : 2 * v1 - v2, v3 = i < n - 1 ? values2[i + 2] : 2 * v2 - v1;
      return basis((t - i / n) * n, v0, v1, v2, v3);
    };
  }

  // node_modules/d3-interpolate/src/basisClosed.js
  function basisClosed_default(values2) {
    var n = values2.length;
    return function(t) {
      var i = Math.floor(((t %= 1) < 0 ? ++t : t) * n), v0 = values2[(i + n - 1) % n], v1 = values2[i % n], v2 = values2[(i + 1) % n], v3 = values2[(i + 2) % n];
      return basis((t - i / n) * n, v0, v1, v2, v3);
    };
  }

  // node_modules/d3-interpolate/src/constant.js
  var constant_default2 = (x2) => () => x2;

  // node_modules/d3-interpolate/src/color.js
  function linear(a, d) {
    return function(t) {
      return a + t * d;
    };
  }
  function exponential(a, b, y2) {
    return a = Math.pow(a, y2), b = Math.pow(b, y2) - a, y2 = 1 / y2, function(t) {
      return Math.pow(a + t * b, y2);
    };
  }
  function gamma(y2) {
    return (y2 = +y2) === 1 ? nogamma : function(a, b) {
      return b - a ? exponential(a, b, y2) : constant_default2(isNaN(a) ? b : a);
    };
  }
  function nogamma(a, b) {
    var d = b - a;
    return d ? linear(a, d) : constant_default2(isNaN(a) ? b : a);
  }

  // node_modules/d3-interpolate/src/rgb.js
  var rgb_default = function rgbGamma(y2) {
    var color2 = gamma(y2);
    function rgb2(start2, end) {
      var r = color2((start2 = rgb(start2)).r, (end = rgb(end)).r), g = color2(start2.g, end.g), b = color2(start2.b, end.b), opacity = nogamma(start2.opacity, end.opacity);
      return function(t) {
        start2.r = r(t);
        start2.g = g(t);
        start2.b = b(t);
        start2.opacity = opacity(t);
        return start2 + "";
      };
    }
    rgb2.gamma = rgbGamma;
    return rgb2;
  }(1);
  function rgbSpline(spline) {
    return function(colors) {
      var n = colors.length, r = new Array(n), g = new Array(n), b = new Array(n), i, color2;
      for (i = 0; i < n; ++i) {
        color2 = rgb(colors[i]);
        r[i] = color2.r || 0;
        g[i] = color2.g || 0;
        b[i] = color2.b || 0;
      }
      r = spline(r);
      g = spline(g);
      b = spline(b);
      color2.opacity = 1;
      return function(t) {
        color2.r = r(t);
        color2.g = g(t);
        color2.b = b(t);
        return color2 + "";
      };
    };
  }
  var rgbBasis = rgbSpline(basis_default);
  var rgbBasisClosed = rgbSpline(basisClosed_default);

  // node_modules/d3-interpolate/src/numberArray.js
  function numberArray_default(a, b) {
    if (!b)
      b = [];
    var n = a ? Math.min(b.length, a.length) : 0, c = b.slice(), i;
    return function(t) {
      for (i = 0; i < n; ++i)
        c[i] = a[i] * (1 - t) + b[i] * t;
      return c;
    };
  }
  function isNumberArray(x2) {
    return ArrayBuffer.isView(x2) && !(x2 instanceof DataView);
  }

  // node_modules/d3-interpolate/src/array.js
  function genericArray(a, b) {
    var nb = b ? b.length : 0, na = a ? Math.min(nb, a.length) : 0, x2 = new Array(na), c = new Array(nb), i;
    for (i = 0; i < na; ++i)
      x2[i] = value_default(a[i], b[i]);
    for (; i < nb; ++i)
      c[i] = b[i];
    return function(t) {
      for (i = 0; i < na; ++i)
        c[i] = x2[i](t);
      return c;
    };
  }

  // node_modules/d3-interpolate/src/date.js
  function date_default(a, b) {
    var d = new Date();
    return a = +a, b = +b, function(t) {
      return d.setTime(a * (1 - t) + b * t), d;
    };
  }

  // node_modules/d3-interpolate/src/number.js
  function number_default2(a, b) {
    return a = +a, b = +b, function(t) {
      return a * (1 - t) + b * t;
    };
  }

  // node_modules/d3-interpolate/src/object.js
  function object_default(a, b) {
    var i = {}, c = {}, k;
    if (a === null || typeof a !== "object")
      a = {};
    if (b === null || typeof b !== "object")
      b = {};
    for (k in b) {
      if (k in a) {
        i[k] = value_default(a[k], b[k]);
      } else {
        c[k] = b[k];
      }
    }
    return function(t) {
      for (k in i)
        c[k] = i[k](t);
      return c;
    };
  }

  // node_modules/d3-interpolate/src/string.js
  var reA = /[-+]?(?:\d+\.?\d*|\.?\d+)(?:[eE][-+]?\d+)?/g;
  var reB = new RegExp(reA.source, "g");
  function zero(b) {
    return function() {
      return b;
    };
  }
  function one(b) {
    return function(t) {
      return b(t) + "";
    };
  }
  function string_default(a, b) {
    var bi = reA.lastIndex = reB.lastIndex = 0, am, bm, bs, i = -1, s = [], q = [];
    a = a + "", b = b + "";
    while ((am = reA.exec(a)) && (bm = reB.exec(b))) {
      if ((bs = bm.index) > bi) {
        bs = b.slice(bi, bs);
        if (s[i])
          s[i] += bs;
        else
          s[++i] = bs;
      }
      if ((am = am[0]) === (bm = bm[0])) {
        if (s[i])
          s[i] += bm;
        else
          s[++i] = bm;
      } else {
        s[++i] = null;
        q.push({ i, x: number_default2(am, bm) });
      }
      bi = reB.lastIndex;
    }
    if (bi < b.length) {
      bs = b.slice(bi);
      if (s[i])
        s[i] += bs;
      else
        s[++i] = bs;
    }
    return s.length < 2 ? q[0] ? one(q[0].x) : zero(b) : (b = q.length, function(t) {
      for (var i2 = 0, o; i2 < b; ++i2)
        s[(o = q[i2]).i] = o.x(t);
      return s.join("");
    });
  }

  // node_modules/d3-interpolate/src/value.js
  function value_default(a, b) {
    var t = typeof b, c;
    return b == null || t === "boolean" ? constant_default2(b) : (t === "number" ? number_default2 : t === "string" ? (c = color(b)) ? (b = c, rgb_default) : string_default : b instanceof color ? rgb_default : b instanceof Date ? date_default : isNumberArray(b) ? numberArray_default : Array.isArray(b) ? genericArray : typeof b.valueOf !== "function" && typeof b.toString !== "function" || isNaN(b) ? object_default : number_default2)(a, b);
  }

  // node_modules/d3-interpolate/src/round.js
  function round_default(a, b) {
    return a = +a, b = +b, function(t) {
      return Math.round(a * (1 - t) + b * t);
    };
  }

  // node_modules/d3-interpolate/src/transform/decompose.js
  var degrees = 180 / Math.PI;
  var identity13 = {
    translateX: 0,
    translateY: 0,
    rotate: 0,
    skewX: 0,
    scaleX: 1,
    scaleY: 1
  };
  function decompose_default(a, b, c, d, e, f) {
    var scaleX, scaleY, skewX;
    if (scaleX = Math.sqrt(a * a + b * b))
      a /= scaleX, b /= scaleX;
    if (skewX = a * c + b * d)
      c -= a * skewX, d -= b * skewX;
    if (scaleY = Math.sqrt(c * c + d * d))
      c /= scaleY, d /= scaleY, skewX /= scaleY;
    if (a * d < b * c)
      a = -a, b = -b, skewX = -skewX, scaleX = -scaleX;
    return {
      translateX: e,
      translateY: f,
      rotate: Math.atan2(b, a) * degrees,
      skewX: Math.atan(skewX) * degrees,
      scaleX,
      scaleY
    };
  }

  // node_modules/d3-interpolate/src/transform/parse.js
  var svgNode;
  function parseCss(value) {
    const m = new (typeof DOMMatrix === "function" ? DOMMatrix : WebKitCSSMatrix)(value + "");
    return m.isIdentity ? identity13 : decompose_default(m.a, m.b, m.c, m.d, m.e, m.f);
  }
  function parseSvg(value) {
    if (value == null)
      return identity13;
    if (!svgNode)
      svgNode = document.createElementNS("http://www.w3.org/2000/svg", "g");
    svgNode.setAttribute("transform", value);
    if (!(value = svgNode.transform.baseVal.consolidate()))
      return identity13;
    value = value.matrix;
    return decompose_default(value.a, value.b, value.c, value.d, value.e, value.f);
  }

  // node_modules/d3-interpolate/src/transform/index.js
  function interpolateTransform(parse2, pxComma, pxParen, degParen) {
    function pop2(s) {
      return s.length ? s.pop() + " " : "";
    }
    function translate(xa, ya, xb, yb, s, q) {
      if (xa !== xb || ya !== yb) {
        var i = s.push("translate(", null, pxComma, null, pxParen);
        q.push({ i: i - 4, x: number_default2(xa, xb) }, { i: i - 2, x: number_default2(ya, yb) });
      } else if (xb || yb) {
        s.push("translate(" + xb + pxComma + yb + pxParen);
      }
    }
    function rotate(a, b, s, q) {
      if (a !== b) {
        if (a - b > 180)
          b += 360;
        else if (b - a > 180)
          a += 360;
        q.push({ i: s.push(pop2(s) + "rotate(", null, degParen) - 2, x: number_default2(a, b) });
      } else if (b) {
        s.push(pop2(s) + "rotate(" + b + degParen);
      }
    }
    function skewX(a, b, s, q) {
      if (a !== b) {
        q.push({ i: s.push(pop2(s) + "skewX(", null, degParen) - 2, x: number_default2(a, b) });
      } else if (b) {
        s.push(pop2(s) + "skewX(" + b + degParen);
      }
    }
    function scale(xa, ya, xb, yb, s, q) {
      if (xa !== xb || ya !== yb) {
        var i = s.push(pop2(s) + "scale(", null, ",", null, ")");
        q.push({ i: i - 4, x: number_default2(xa, xb) }, { i: i - 2, x: number_default2(ya, yb) });
      } else if (xb !== 1 || yb !== 1) {
        s.push(pop2(s) + "scale(" + xb + "," + yb + ")");
      }
    }
    return function(a, b) {
      var s = [], q = [];
      a = parse2(a), b = parse2(b);
      translate(a.translateX, a.translateY, b.translateX, b.translateY, s, q);
      rotate(a.rotate, b.rotate, s, q);
      skewX(a.skewX, b.skewX, s, q);
      scale(a.scaleX, a.scaleY, b.scaleX, b.scaleY, s, q);
      a = b = null;
      return function(t) {
        var i = -1, n = q.length, o;
        while (++i < n)
          s[(o = q[i]).i] = o.x(t);
        return s.join("");
      };
    };
  }
  var interpolateTransformCss = interpolateTransform(parseCss, "px, ", "px)", "deg)");
  var interpolateTransformSvg = interpolateTransform(parseSvg, ", ", ")", ")");

  // node_modules/d3-timer/src/timer.js
  var frame = 0;
  var timeout = 0;
  var interval = 0;
  var pokeDelay = 1e3;
  var taskHead;
  var taskTail;
  var clockLast = 0;
  var clockNow = 0;
  var clockSkew = 0;
  var clock = typeof performance === "object" && performance.now ? performance : Date;
  var setFrame = typeof window === "object" && window.requestAnimationFrame ? window.requestAnimationFrame.bind(window) : function(f) {
    setTimeout(f, 17);
  };
  function now() {
    return clockNow || (setFrame(clearNow), clockNow = clock.now() + clockSkew);
  }
  function clearNow() {
    clockNow = 0;
  }
  function Timer() {
    this._call = this._time = this._next = null;
  }
  Timer.prototype = timer.prototype = {
    constructor: Timer,
    restart: function(callback, delay, time2) {
      if (typeof callback !== "function")
        throw new TypeError("callback is not a function");
      time2 = (time2 == null ? now() : +time2) + (delay == null ? 0 : +delay);
      if (!this._next && taskTail !== this) {
        if (taskTail)
          taskTail._next = this;
        else
          taskHead = this;
        taskTail = this;
      }
      this._call = callback;
      this._time = time2;
      sleep();
    },
    stop: function() {
      if (this._call) {
        this._call = null;
        this._time = Infinity;
        sleep();
      }
    }
  };
  function timer(callback, delay, time2) {
    var t = new Timer();
    t.restart(callback, delay, time2);
    return t;
  }
  function timerFlush() {
    now();
    ++frame;
    var t = taskHead, e;
    while (t) {
      if ((e = clockNow - t._time) >= 0)
        t._call.call(null, e);
      t = t._next;
    }
    --frame;
  }
  function wake() {
    clockNow = (clockLast = clock.now()) + clockSkew;
    frame = timeout = 0;
    try {
      timerFlush();
    } finally {
      frame = 0;
      nap();
      clockNow = 0;
    }
  }
  function poke3() {
    var now3 = clock.now(), delay = now3 - clockLast;
    if (delay > pokeDelay)
      clockSkew -= delay, clockLast = now3;
  }
  function nap() {
    var t0, t1 = taskHead, t2, time2 = Infinity;
    while (t1) {
      if (t1._call) {
        if (time2 > t1._time)
          time2 = t1._time;
        t0 = t1, t1 = t1._next;
      } else {
        t2 = t1._next, t1._next = null;
        t1 = t0 ? t0._next = t2 : taskHead = t2;
      }
    }
    taskTail = t0;
    sleep(time2);
  }
  function sleep(time2) {
    if (frame)
      return;
    if (timeout)
      timeout = clearTimeout(timeout);
    var delay = time2 - clockNow;
    if (delay > 24) {
      if (time2 < Infinity)
        timeout = setTimeout(wake, time2 - clock.now() - clockSkew);
      if (interval)
        interval = clearInterval(interval);
    } else {
      if (!interval)
        clockLast = clock.now(), interval = setInterval(poke3, pokeDelay);
      frame = 1, setFrame(wake);
    }
  }

  // node_modules/d3-timer/src/timeout.js
  function timeout_default(callback, delay, time2) {
    var t = new Timer();
    delay = delay == null ? 0 : +delay;
    t.restart((elapsed) => {
      t.stop();
      callback(elapsed + delay);
    }, delay, time2);
    return t;
  }

  // node_modules/d3-transition/src/transition/schedule.js
  var emptyOn = dispatch_default("start", "end", "cancel", "interrupt");
  var emptyTween = [];
  var CREATED = 0;
  var SCHEDULED = 1;
  var STARTING = 2;
  var STARTED = 3;
  var RUNNING = 4;
  var ENDING = 5;
  var ENDED = 6;
  function schedule_default(node, name2, id3, index3, group2, timing) {
    var schedules = node.__transition;
    if (!schedules)
      node.__transition = {};
    else if (id3 in schedules)
      return;
    create(node, id3, {
      name: name2,
      index: index3,
      group: group2,
      on: emptyOn,
      tween: emptyTween,
      time: timing.time,
      delay: timing.delay,
      duration: timing.duration,
      ease: timing.ease,
      timer: null,
      state: CREATED
    });
  }
  function init(node, id3) {
    var schedule = get2(node, id3);
    if (schedule.state > CREATED)
      throw new Error("too late; already scheduled");
    return schedule;
  }
  function set2(node, id3) {
    var schedule = get2(node, id3);
    if (schedule.state > STARTED)
      throw new Error("too late; already running");
    return schedule;
  }
  function get2(node, id3) {
    var schedule = node.__transition;
    if (!schedule || !(schedule = schedule[id3]))
      throw new Error("transition not found");
    return schedule;
  }
  function create(node, id3, self) {
    var schedules = node.__transition, tween;
    schedules[id3] = self;
    self.timer = timer(schedule, 0, self.time);
    function schedule(elapsed) {
      self.state = SCHEDULED;
      self.timer.restart(start2, self.delay, self.time);
      if (self.delay <= elapsed)
        start2(elapsed - self.delay);
    }
    function start2(elapsed) {
      var i, j, n, o;
      if (self.state !== SCHEDULED)
        return stop();
      for (i in schedules) {
        o = schedules[i];
        if (o.name !== self.name)
          continue;
        if (o.state === STARTED)
          return timeout_default(start2);
        if (o.state === RUNNING) {
          o.state = ENDED;
          o.timer.stop();
          o.on.call("interrupt", node, node.__data__, o.index, o.group);
          delete schedules[i];
        } else if (+i < id3) {
          o.state = ENDED;
          o.timer.stop();
          o.on.call("cancel", node, node.__data__, o.index, o.group);
          delete schedules[i];
        }
      }
      timeout_default(function() {
        if (self.state === STARTED) {
          self.state = RUNNING;
          self.timer.restart(tick, self.delay, self.time);
          tick(elapsed);
        }
      });
      self.state = STARTING;
      self.on.call("start", node, node.__data__, self.index, self.group);
      if (self.state !== STARTING)
        return;
      self.state = STARTED;
      tween = new Array(n = self.tween.length);
      for (i = 0, j = -1; i < n; ++i) {
        if (o = self.tween[i].value.call(node, node.__data__, self.index, self.group)) {
          tween[++j] = o;
        }
      }
      tween.length = j + 1;
    }
    function tick(elapsed) {
      var t = elapsed < self.duration ? self.ease.call(null, elapsed / self.duration) : (self.timer.restart(stop), self.state = ENDING, 1), i = -1, n = tween.length;
      while (++i < n) {
        tween[i].call(node, t);
      }
      if (self.state === ENDING) {
        self.on.call("end", node, node.__data__, self.index, self.group);
        stop();
      }
    }
    function stop() {
      self.state = ENDED;
      self.timer.stop();
      delete schedules[id3];
      for (var i in schedules)
        return;
      delete node.__transition;
    }
  }

  // node_modules/d3-transition/src/interrupt.js
  function interrupt_default(node, name2) {
    var schedules = node.__transition, schedule, active, empty5 = true, i;
    if (!schedules)
      return;
    name2 = name2 == null ? null : name2 + "";
    for (i in schedules) {
      if ((schedule = schedules[i]).name !== name2) {
        empty5 = false;
        continue;
      }
      active = schedule.state > STARTING && schedule.state < ENDING;
      schedule.state = ENDED;
      schedule.timer.stop();
      schedule.on.call(active ? "interrupt" : "cancel", node, node.__data__, schedule.index, schedule.group);
      delete schedules[i];
    }
    if (empty5)
      delete node.__transition;
  }

  // node_modules/d3-transition/src/selection/interrupt.js
  function interrupt_default2(name2) {
    return this.each(function() {
      interrupt_default(this, name2);
    });
  }

  // node_modules/d3-transition/src/transition/tween.js
  function tweenRemove(id3, name2) {
    var tween0, tween1;
    return function() {
      var schedule = set2(this, id3), tween = schedule.tween;
      if (tween !== tween0) {
        tween1 = tween0 = tween;
        for (var i = 0, n = tween1.length; i < n; ++i) {
          if (tween1[i].name === name2) {
            tween1 = tween1.slice();
            tween1.splice(i, 1);
            break;
          }
        }
      }
      schedule.tween = tween1;
    };
  }
  function tweenFunction(id3, name2, value) {
    var tween0, tween1;
    if (typeof value !== "function")
      throw new Error();
    return function() {
      var schedule = set2(this, id3), tween = schedule.tween;
      if (tween !== tween0) {
        tween1 = (tween0 = tween).slice();
        for (var t = { name: name2, value }, i = 0, n = tween1.length; i < n; ++i) {
          if (tween1[i].name === name2) {
            tween1[i] = t;
            break;
          }
        }
        if (i === n)
          tween1.push(t);
      }
      schedule.tween = tween1;
    };
  }
  function tween_default(name2, value) {
    var id3 = this._id;
    name2 += "";
    if (arguments.length < 2) {
      var tween = get2(this.node(), id3).tween;
      for (var i = 0, n = tween.length, t; i < n; ++i) {
        if ((t = tween[i]).name === name2) {
          return t.value;
        }
      }
      return null;
    }
    return this.each((value == null ? tweenRemove : tweenFunction)(id3, name2, value));
  }
  function tweenValue(transition2, name2, value) {
    var id3 = transition2._id;
    transition2.each(function() {
      var schedule = set2(this, id3);
      (schedule.value || (schedule.value = {}))[name2] = value.apply(this, arguments);
    });
    return function(node) {
      return get2(node, id3).value[name2];
    };
  }

  // node_modules/d3-transition/src/transition/interpolate.js
  function interpolate_default(a, b) {
    var c;
    return (typeof b === "number" ? number_default2 : b instanceof color ? rgb_default : (c = color(b)) ? (b = c, rgb_default) : string_default)(a, b);
  }

  // node_modules/d3-transition/src/transition/attr.js
  function attrRemove2(name2) {
    return function() {
      this.removeAttribute(name2);
    };
  }
  function attrRemoveNS2(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant2(name2, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = this.getAttribute(name2);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function attrConstantNS2(fullname, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = this.getAttributeNS(fullname.space, fullname.local);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function attrFunction2(name2, interpolate, value) {
    var string00, string10, interpolate0;
    return function() {
      var string0, value1 = value(this), string1;
      if (value1 == null)
        return void this.removeAttribute(name2);
      string0 = this.getAttribute(name2);
      string1 = value1 + "";
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function attrFunctionNS2(fullname, interpolate, value) {
    var string00, string10, interpolate0;
    return function() {
      var string0, value1 = value(this), string1;
      if (value1 == null)
        return void this.removeAttributeNS(fullname.space, fullname.local);
      string0 = this.getAttributeNS(fullname.space, fullname.local);
      string1 = value1 + "";
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function attr_default2(name2, value) {
    var fullname = namespace_default(name2), i = fullname === "transform" ? interpolateTransformSvg : interpolate_default;
    return this.attrTween(name2, typeof value === "function" ? (fullname.local ? attrFunctionNS2 : attrFunction2)(fullname, i, tweenValue(this, "attr." + name2, value)) : value == null ? (fullname.local ? attrRemoveNS2 : attrRemove2)(fullname) : (fullname.local ? attrConstantNS2 : attrConstant2)(fullname, i, value));
  }

  // node_modules/d3-transition/src/transition/attrTween.js
  function attrInterpolate(name2, i) {
    return function(t) {
      this.setAttribute(name2, i.call(this, t));
    };
  }
  function attrInterpolateNS(fullname, i) {
    return function(t) {
      this.setAttributeNS(fullname.space, fullname.local, i.call(this, t));
    };
  }
  function attrTweenNS(fullname, value) {
    var t0, i0;
    function tween() {
      var i = value.apply(this, arguments);
      if (i !== i0)
        t0 = (i0 = i) && attrInterpolateNS(fullname, i);
      return t0;
    }
    tween._value = value;
    return tween;
  }
  function attrTween(name2, value) {
    var t0, i0;
    function tween() {
      var i = value.apply(this, arguments);
      if (i !== i0)
        t0 = (i0 = i) && attrInterpolate(name2, i);
      return t0;
    }
    tween._value = value;
    return tween;
  }
  function attrTween_default(name2, value) {
    var key = "attr." + name2;
    if (arguments.length < 2)
      return (key = this.tween(key)) && key._value;
    if (value == null)
      return this.tween(key, null);
    if (typeof value !== "function")
      throw new Error();
    var fullname = namespace_default(name2);
    return this.tween(key, (fullname.local ? attrTweenNS : attrTween)(fullname, value));
  }

  // node_modules/d3-transition/src/transition/delay.js
  function delayFunction(id3, value) {
    return function() {
      init(this, id3).delay = +value.apply(this, arguments);
    };
  }
  function delayConstant(id3, value) {
    return value = +value, function() {
      init(this, id3).delay = value;
    };
  }
  function delay_default(value) {
    var id3 = this._id;
    return arguments.length ? this.each((typeof value === "function" ? delayFunction : delayConstant)(id3, value)) : get2(this.node(), id3).delay;
  }

  // node_modules/d3-transition/src/transition/duration.js
  function durationFunction(id3, value) {
    return function() {
      set2(this, id3).duration = +value.apply(this, arguments);
    };
  }
  function durationConstant(id3, value) {
    return value = +value, function() {
      set2(this, id3).duration = value;
    };
  }
  function duration_default(value) {
    var id3 = this._id;
    return arguments.length ? this.each((typeof value === "function" ? durationFunction : durationConstant)(id3, value)) : get2(this.node(), id3).duration;
  }

  // node_modules/d3-transition/src/transition/ease.js
  function easeConstant(id3, value) {
    if (typeof value !== "function")
      throw new Error();
    return function() {
      set2(this, id3).ease = value;
    };
  }
  function ease_default(value) {
    var id3 = this._id;
    return arguments.length ? this.each(easeConstant(id3, value)) : get2(this.node(), id3).ease;
  }

  // node_modules/d3-transition/src/transition/easeVarying.js
  function easeVarying(id3, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (typeof v !== "function")
        throw new Error();
      set2(this, id3).ease = v;
    };
  }
  function easeVarying_default(value) {
    if (typeof value !== "function")
      throw new Error();
    return this.each(easeVarying(this._id, value));
  }

  // node_modules/d3-transition/src/transition/filter.js
  function filter_default2(match10) {
    if (typeof match10 !== "function")
      match10 = matcher_default(match10);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, subgroup = subgroups[j] = [], node, i = 0; i < n; ++i) {
        if ((node = group2[i]) && match10.call(node, node.__data__, i, group2)) {
          subgroup.push(node);
        }
      }
    }
    return new Transition(subgroups, this._parents, this._name, this._id);
  }

  // node_modules/d3-transition/src/transition/merge.js
  function merge_default2(transition2) {
    if (transition2._id !== this._id)
      throw new Error();
    for (var groups0 = this._groups, groups1 = transition2._groups, m0 = groups0.length, m1 = groups1.length, m = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m; ++j) {
      for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group0[i] || group1[i]) {
          merge[i] = node;
        }
      }
    }
    for (; j < m0; ++j) {
      merges[j] = groups0[j];
    }
    return new Transition(merges, this._parents, this._name, this._id);
  }

  // node_modules/d3-transition/src/transition/on.js
  function start(name2) {
    return (name2 + "").trim().split(/^|\s+/).every(function(t) {
      var i = t.indexOf(".");
      if (i >= 0)
        t = t.slice(0, i);
      return !t || t === "start";
    });
  }
  function onFunction(id3, name2, listener) {
    var on0, on1, sit = start(name2) ? init : set2;
    return function() {
      var schedule = sit(this, id3), on = schedule.on;
      if (on !== on0)
        (on1 = (on0 = on).copy()).on(name2, listener);
      schedule.on = on1;
    };
  }
  function on_default2(name2, listener) {
    var id3 = this._id;
    return arguments.length < 2 ? get2(this.node(), id3).on.on(name2) : this.each(onFunction(id3, name2, listener));
  }

  // node_modules/d3-transition/src/transition/remove.js
  function removeFunction(id3) {
    return function() {
      var parent = this.parentNode;
      for (var i in this.__transition)
        if (+i !== id3)
          return;
      if (parent)
        parent.removeChild(this);
    };
  }
  function remove_default2() {
    return this.on("end.remove", removeFunction(this._id));
  }

  // node_modules/d3-transition/src/transition/select.js
  function select_default3(select) {
    var name2 = this._name, id3 = this._id;
    if (typeof select !== "function")
      select = selector_default(select);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, subgroup = subgroups[j] = new Array(n), node, subnode, i = 0; i < n; ++i) {
        if ((node = group2[i]) && (subnode = select.call(node, node.__data__, i, group2))) {
          if ("__data__" in node)
            subnode.__data__ = node.__data__;
          subgroup[i] = subnode;
          schedule_default(subgroup[i], name2, id3, i, subgroup, get2(node, id3));
        }
      }
    }
    return new Transition(subgroups, this._parents, name2, id3);
  }

  // node_modules/d3-transition/src/transition/selectAll.js
  function selectAll_default2(select) {
    var name2 = this._name, id3 = this._id;
    if (typeof select !== "function")
      select = selectorAll_default(select);
    for (var groups = this._groups, m = groups.length, subgroups = [], parents = [], j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, node, i = 0; i < n; ++i) {
        if (node = group2[i]) {
          for (var children2 = select.call(node, node.__data__, i, group2), child, inherit2 = get2(node, id3), k = 0, l = children2.length; k < l; ++k) {
            if (child = children2[k]) {
              schedule_default(child, name2, id3, k, children2, inherit2);
            }
          }
          subgroups.push(children2);
          parents.push(node);
        }
      }
    }
    return new Transition(subgroups, parents, name2, id3);
  }

  // node_modules/d3-transition/src/transition/selection.js
  var Selection2 = selection_default.prototype.constructor;
  function selection_default2() {
    return new Selection2(this._groups, this._parents);
  }

  // node_modules/d3-transition/src/transition/style.js
  function styleNull(name2, interpolate) {
    var string00, string10, interpolate0;
    return function() {
      var string0 = styleValue(this, name2), string1 = (this.style.removeProperty(name2), styleValue(this, name2));
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : interpolate0 = interpolate(string00 = string0, string10 = string1);
    };
  }
  function styleRemove2(name2) {
    return function() {
      this.style.removeProperty(name2);
    };
  }
  function styleConstant2(name2, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = styleValue(this, name2);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function styleFunction2(name2, interpolate, value) {
    var string00, string10, interpolate0;
    return function() {
      var string0 = styleValue(this, name2), value1 = value(this), string1 = value1 + "";
      if (value1 == null)
        string1 = value1 = (this.style.removeProperty(name2), styleValue(this, name2));
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function styleMaybeRemove(id3, name2) {
    var on0, on1, listener0, key = "style." + name2, event2 = "end." + key, remove3;
    return function() {
      var schedule = set2(this, id3), on = schedule.on, listener = schedule.value[key] == null ? remove3 || (remove3 = styleRemove2(name2)) : void 0;
      if (on !== on0 || listener0 !== listener)
        (on1 = (on0 = on).copy()).on(event2, listener0 = listener);
      schedule.on = on1;
    };
  }
  function style_default2(name2, value, priority) {
    var i = (name2 += "") === "transform" ? interpolateTransformCss : interpolate_default;
    return value == null ? this.styleTween(name2, styleNull(name2, i)).on("end.style." + name2, styleRemove2(name2)) : typeof value === "function" ? this.styleTween(name2, styleFunction2(name2, i, tweenValue(this, "style." + name2, value))).each(styleMaybeRemove(this._id, name2)) : this.styleTween(name2, styleConstant2(name2, i, value), priority).on("end.style." + name2, null);
  }

  // node_modules/d3-transition/src/transition/styleTween.js
  function styleInterpolate(name2, i, priority) {
    return function(t) {
      this.style.setProperty(name2, i.call(this, t), priority);
    };
  }
  function styleTween(name2, value, priority) {
    var t, i0;
    function tween() {
      var i = value.apply(this, arguments);
      if (i !== i0)
        t = (i0 = i) && styleInterpolate(name2, i, priority);
      return t;
    }
    tween._value = value;
    return tween;
  }
  function styleTween_default(name2, value, priority) {
    var key = "style." + (name2 += "");
    if (arguments.length < 2)
      return (key = this.tween(key)) && key._value;
    if (value == null)
      return this.tween(key, null);
    if (typeof value !== "function")
      throw new Error();
    return this.tween(key, styleTween(name2, value, priority == null ? "" : priority));
  }

  // node_modules/d3-transition/src/transition/text.js
  function textConstant2(value) {
    return function() {
      this.textContent = value;
    };
  }
  function textFunction2(value) {
    return function() {
      var value1 = value(this);
      this.textContent = value1 == null ? "" : value1;
    };
  }
  function text_default2(value) {
    return this.tween("text", typeof value === "function" ? textFunction2(tweenValue(this, "text", value)) : textConstant2(value == null ? "" : value + ""));
  }

  // node_modules/d3-transition/src/transition/textTween.js
  function textInterpolate(i) {
    return function(t) {
      this.textContent = i.call(this, t);
    };
  }
  function textTween(value) {
    var t0, i0;
    function tween() {
      var i = value.apply(this, arguments);
      if (i !== i0)
        t0 = (i0 = i) && textInterpolate(i);
      return t0;
    }
    tween._value = value;
    return tween;
  }
  function textTween_default(value) {
    var key = "text";
    if (arguments.length < 1)
      return (key = this.tween(key)) && key._value;
    if (value == null)
      return this.tween(key, null);
    if (typeof value !== "function")
      throw new Error();
    return this.tween(key, textTween(value));
  }

  // node_modules/d3-transition/src/transition/transition.js
  function transition_default() {
    var name2 = this._name, id0 = this._id, id1 = newId();
    for (var groups = this._groups, m = groups.length, j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, node, i = 0; i < n; ++i) {
        if (node = group2[i]) {
          var inherit2 = get2(node, id0);
          schedule_default(node, name2, id1, i, group2, {
            time: inherit2.time + inherit2.delay + inherit2.duration,
            delay: 0,
            duration: inherit2.duration,
            ease: inherit2.ease
          });
        }
      }
    }
    return new Transition(groups, this._parents, name2, id1);
  }

  // node_modules/d3-transition/src/transition/end.js
  function end_default() {
    var on0, on1, that = this, id3 = that._id, size3 = that.size();
    return new Promise(function(resolve, reject) {
      var cancel = { value: reject }, end = { value: function() {
        if (--size3 === 0)
          resolve();
      } };
      that.each(function() {
        var schedule = set2(this, id3), on = schedule.on;
        if (on !== on0) {
          on1 = (on0 = on).copy();
          on1._.cancel.push(cancel);
          on1._.interrupt.push(cancel);
          on1._.end.push(end);
        }
        schedule.on = on1;
      });
      if (size3 === 0)
        resolve();
    });
  }

  // node_modules/d3-transition/src/transition/index.js
  var id = 0;
  function Transition(groups, parents, name2, id3) {
    this._groups = groups;
    this._parents = parents;
    this._name = name2;
    this._id = id3;
  }
  function transition(name2) {
    return selection_default().transition(name2);
  }
  function newId() {
    return ++id;
  }
  var selection_prototype = selection_default.prototype;
  Transition.prototype = transition.prototype = {
    constructor: Transition,
    select: select_default3,
    selectAll: selectAll_default2,
    filter: filter_default2,
    merge: merge_default2,
    selection: selection_default2,
    transition: transition_default,
    call: selection_prototype.call,
    nodes: selection_prototype.nodes,
    node: selection_prototype.node,
    size: selection_prototype.size,
    empty: selection_prototype.empty,
    each: selection_prototype.each,
    on: on_default2,
    attr: attr_default2,
    attrTween: attrTween_default,
    style: style_default2,
    styleTween: styleTween_default,
    text: text_default2,
    textTween: textTween_default,
    remove: remove_default2,
    tween: tween_default,
    delay: delay_default,
    duration: duration_default,
    ease: ease_default,
    easeVarying: easeVarying_default,
    end: end_default,
    [Symbol.iterator]: selection_prototype[Symbol.iterator]
  };

  // node_modules/d3-ease/src/cubic.js
  function cubicInOut(t) {
    return ((t *= 2) <= 1 ? t * t * t : (t -= 2) * t * t + 2) / 2;
  }

  // node_modules/d3-transition/src/selection/transition.js
  var defaultTiming = {
    time: null,
    delay: 0,
    duration: 250,
    ease: cubicInOut
  };
  function inherit(node, id3) {
    var timing;
    while (!(timing = node.__transition) || !(timing = timing[id3])) {
      if (!(node = node.parentNode)) {
        throw new Error(`transition ${id3} not found`);
      }
    }
    return timing;
  }
  function transition_default2(name2) {
    var id3, timing;
    if (name2 instanceof Transition) {
      id3 = name2._id, name2 = name2._name;
    } else {
      id3 = newId(), (timing = defaultTiming).time = now(), name2 = name2 == null ? null : name2 + "";
    }
    for (var groups = this._groups, m = groups.length, j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, node, i = 0; i < n; ++i) {
        if (node = group2[i]) {
          schedule_default(node, name2, id3, i, group2, timing || inherit(node, id3));
        }
      }
    }
    return new Transition(groups, this._parents, name2, id3);
  }

  // node_modules/d3-transition/src/selection/index.js
  selection_default.prototype.interrupt = interrupt_default2;
  selection_default.prototype.transition = transition_default2;

  // node_modules/d3-brush/src/brush.js
  var { abs: abs2, max: max3, min: min2 } = Math;
  function number1(e) {
    return [+e[0], +e[1]];
  }
  function number22(e) {
    return [number1(e[0]), number1(e[1])];
  }
  var X = {
    name: "x",
    handles: ["w", "e"].map(type),
    input: function(x2, e) {
      return x2 == null ? null : [[+x2[0], e[0][1]], [+x2[1], e[1][1]]];
    },
    output: function(xy) {
      return xy && [xy[0][0], xy[1][0]];
    }
  };
  var Y = {
    name: "y",
    handles: ["n", "s"].map(type),
    input: function(y2, e) {
      return y2 == null ? null : [[e[0][0], +y2[0]], [e[1][0], +y2[1]]];
    },
    output: function(xy) {
      return xy && [xy[0][1], xy[1][1]];
    }
  };
  var XY = {
    name: "xy",
    handles: ["n", "w", "e", "s", "nw", "ne", "sw", "se"].map(type),
    input: function(xy) {
      return xy == null ? null : number22(xy);
    },
    output: function(xy) {
      return xy;
    }
  };
  function type(t) {
    return { type: t };
  }

  // node_modules/d3-path/src/path.js
  var pi = Math.PI;
  var tau = 2 * pi;
  var epsilon2 = 1e-6;
  var tauEpsilon = tau - epsilon2;
  function Path() {
    this._x0 = this._y0 = this._x1 = this._y1 = null;
    this._ = "";
  }
  function path() {
    return new Path();
  }
  Path.prototype = path.prototype = {
    constructor: Path,
    moveTo: function(x2, y2) {
      this._ += "M" + (this._x0 = this._x1 = +x2) + "," + (this._y0 = this._y1 = +y2);
    },
    closePath: function() {
      if (this._x1 !== null) {
        this._x1 = this._x0, this._y1 = this._y0;
        this._ += "Z";
      }
    },
    lineTo: function(x2, y2) {
      this._ += "L" + (this._x1 = +x2) + "," + (this._y1 = +y2);
    },
    quadraticCurveTo: function(x1, y1, x2, y2) {
      this._ += "Q" + +x1 + "," + +y1 + "," + (this._x1 = +x2) + "," + (this._y1 = +y2);
    },
    bezierCurveTo: function(x1, y1, x2, y2, x3, y3) {
      this._ += "C" + +x1 + "," + +y1 + "," + +x2 + "," + +y2 + "," + (this._x1 = +x3) + "," + (this._y1 = +y3);
    },
    arcTo: function(x1, y1, x2, y2, r) {
      x1 = +x1, y1 = +y1, x2 = +x2, y2 = +y2, r = +r;
      var x0 = this._x1, y0 = this._y1, x21 = x2 - x1, y21 = y2 - y1, x01 = x0 - x1, y01 = y0 - y1, l01_2 = x01 * x01 + y01 * y01;
      if (r < 0)
        throw new Error("negative radius: " + r);
      if (this._x1 === null) {
        this._ += "M" + (this._x1 = x1) + "," + (this._y1 = y1);
      } else if (!(l01_2 > epsilon2))
        ;
      else if (!(Math.abs(y01 * x21 - y21 * x01) > epsilon2) || !r) {
        this._ += "L" + (this._x1 = x1) + "," + (this._y1 = y1);
      } else {
        var x20 = x2 - x0, y20 = y2 - y0, l21_2 = x21 * x21 + y21 * y21, l20_2 = x20 * x20 + y20 * y20, l21 = Math.sqrt(l21_2), l01 = Math.sqrt(l01_2), l = r * Math.tan((pi - Math.acos((l21_2 + l01_2 - l20_2) / (2 * l21 * l01))) / 2), t01 = l / l01, t21 = l / l21;
        if (Math.abs(t01 - 1) > epsilon2) {
          this._ += "L" + (x1 + t01 * x01) + "," + (y1 + t01 * y01);
        }
        this._ += "A" + r + "," + r + ",0,0," + +(y01 * x20 > x01 * y20) + "," + (this._x1 = x1 + t21 * x21) + "," + (this._y1 = y1 + t21 * y21);
      }
    },
    arc: function(x2, y2, r, a0, a1, ccw) {
      x2 = +x2, y2 = +y2, r = +r, ccw = !!ccw;
      var dx = r * Math.cos(a0), dy = r * Math.sin(a0), x0 = x2 + dx, y0 = y2 + dy, cw = 1 ^ ccw, da = ccw ? a0 - a1 : a1 - a0;
      if (r < 0)
        throw new Error("negative radius: " + r);
      if (this._x1 === null) {
        this._ += "M" + x0 + "," + y0;
      } else if (Math.abs(this._x1 - x0) > epsilon2 || Math.abs(this._y1 - y0) > epsilon2) {
        this._ += "L" + x0 + "," + y0;
      }
      if (!r)
        return;
      if (da < 0)
        da = da % tau + tau;
      if (da > tauEpsilon) {
        this._ += "A" + r + "," + r + ",0,1," + cw + "," + (x2 - dx) + "," + (y2 - dy) + "A" + r + "," + r + ",0,1," + cw + "," + (this._x1 = x0) + "," + (this._y1 = y0);
      } else if (da > epsilon2) {
        this._ += "A" + r + "," + r + ",0," + +(da >= pi) + "," + cw + "," + (this._x1 = x2 + r * Math.cos(a1)) + "," + (this._y1 = y2 + r * Math.sin(a1));
      }
    },
    rect: function(x2, y2, w, h) {
      this._ += "M" + (this._x0 = this._x1 = +x2) + "," + (this._y0 = this._y1 = +y2) + "h" + +w + "v" + +h + "h" + -w + "Z";
    },
    toString: function() {
      return this._;
    }
  };
  var path_default = path;

  // node_modules/d3-format/src/formatDecimal.js
  function formatDecimal_default(x2) {
    return Math.abs(x2 = Math.round(x2)) >= 1e21 ? x2.toLocaleString("en").replace(/,/g, "") : x2.toString(10);
  }
  function formatDecimalParts(x2, p) {
    if ((i = (x2 = p ? x2.toExponential(p - 1) : x2.toExponential()).indexOf("e")) < 0)
      return null;
    var i, coefficient = x2.slice(0, i);
    return [
      coefficient.length > 1 ? coefficient[0] + coefficient.slice(2) : coefficient,
      +x2.slice(i + 1)
    ];
  }

  // node_modules/d3-format/src/exponent.js
  function exponent_default(x2) {
    return x2 = formatDecimalParts(Math.abs(x2)), x2 ? x2[1] : NaN;
  }

  // node_modules/d3-format/src/formatGroup.js
  function formatGroup_default(grouping, thousands) {
    return function(value, width) {
      var i = value.length, t = [], j = 0, g = grouping[0], length6 = 0;
      while (i > 0 && g > 0) {
        if (length6 + g + 1 > width)
          g = Math.max(1, width - length6);
        t.push(value.substring(i -= g, i + g));
        if ((length6 += g + 1) > width)
          break;
        g = grouping[j = (j + 1) % grouping.length];
      }
      return t.reverse().join(thousands);
    };
  }

  // node_modules/d3-format/src/formatNumerals.js
  function formatNumerals_default(numerals) {
    return function(value) {
      return value.replace(/[0-9]/g, function(i) {
        return numerals[+i];
      });
    };
  }

  // node_modules/d3-format/src/formatSpecifier.js
  var re = /^(?:(.)?([<>=^]))?([+\-( ])?([$#])?(0)?(\d+)?(,)?(\.\d+)?(~)?([a-z%])?$/i;
  function formatSpecifier(specifier) {
    if (!(match10 = re.exec(specifier)))
      throw new Error("invalid format: " + specifier);
    var match10;
    return new FormatSpecifier({
      fill: match10[1],
      align: match10[2],
      sign: match10[3],
      symbol: match10[4],
      zero: match10[5],
      width: match10[6],
      comma: match10[7],
      precision: match10[8] && match10[8].slice(1),
      trim: match10[9],
      type: match10[10]
    });
  }
  formatSpecifier.prototype = FormatSpecifier.prototype;
  function FormatSpecifier(specifier) {
    this.fill = specifier.fill === void 0 ? " " : specifier.fill + "";
    this.align = specifier.align === void 0 ? ">" : specifier.align + "";
    this.sign = specifier.sign === void 0 ? "-" : specifier.sign + "";
    this.symbol = specifier.symbol === void 0 ? "" : specifier.symbol + "";
    this.zero = !!specifier.zero;
    this.width = specifier.width === void 0 ? void 0 : +specifier.width;
    this.comma = !!specifier.comma;
    this.precision = specifier.precision === void 0 ? void 0 : +specifier.precision;
    this.trim = !!specifier.trim;
    this.type = specifier.type === void 0 ? "" : specifier.type + "";
  }
  FormatSpecifier.prototype.toString = function() {
    return this.fill + this.align + this.sign + this.symbol + (this.zero ? "0" : "") + (this.width === void 0 ? "" : Math.max(1, this.width | 0)) + (this.comma ? "," : "") + (this.precision === void 0 ? "" : "." + Math.max(0, this.precision | 0)) + (this.trim ? "~" : "") + this.type;
  };

  // node_modules/d3-format/src/formatTrim.js
  function formatTrim_default(s) {
    out:
      for (var n = s.length, i = 1, i0 = -1, i1; i < n; ++i) {
        switch (s[i]) {
          case ".":
            i0 = i1 = i;
            break;
          case "0":
            if (i0 === 0)
              i0 = i;
            i1 = i;
            break;
          default:
            if (!+s[i])
              break out;
            if (i0 > 0)
              i0 = 0;
            break;
        }
      }
    return i0 > 0 ? s.slice(0, i0) + s.slice(i1 + 1) : s;
  }

  // node_modules/d3-format/src/formatPrefixAuto.js
  var prefixExponent;
  function formatPrefixAuto_default(x2, p) {
    var d = formatDecimalParts(x2, p);
    if (!d)
      return x2 + "";
    var coefficient = d[0], exponent = d[1], i = exponent - (prefixExponent = Math.max(-8, Math.min(8, Math.floor(exponent / 3))) * 3) + 1, n = coefficient.length;
    return i === n ? coefficient : i > n ? coefficient + new Array(i - n + 1).join("0") : i > 0 ? coefficient.slice(0, i) + "." + coefficient.slice(i) : "0." + new Array(1 - i).join("0") + formatDecimalParts(x2, Math.max(0, p + i - 1))[0];
  }

  // node_modules/d3-format/src/formatRounded.js
  function formatRounded_default(x2, p) {
    var d = formatDecimalParts(x2, p);
    if (!d)
      return x2 + "";
    var coefficient = d[0], exponent = d[1];
    return exponent < 0 ? "0." + new Array(-exponent).join("0") + coefficient : coefficient.length > exponent + 1 ? coefficient.slice(0, exponent + 1) + "." + coefficient.slice(exponent + 1) : coefficient + new Array(exponent - coefficient.length + 2).join("0");
  }

  // node_modules/d3-format/src/formatTypes.js
  var formatTypes_default = {
    "%": (x2, p) => (x2 * 100).toFixed(p),
    "b": (x2) => Math.round(x2).toString(2),
    "c": (x2) => x2 + "",
    "d": formatDecimal_default,
    "e": (x2, p) => x2.toExponential(p),
    "f": (x2, p) => x2.toFixed(p),
    "g": (x2, p) => x2.toPrecision(p),
    "o": (x2) => Math.round(x2).toString(8),
    "p": (x2, p) => formatRounded_default(x2 * 100, p),
    "r": formatRounded_default,
    "s": formatPrefixAuto_default,
    "X": (x2) => Math.round(x2).toString(16).toUpperCase(),
    "x": (x2) => Math.round(x2).toString(16)
  };

  // node_modules/d3-format/src/identity.js
  function identity_default2(x2) {
    return x2;
  }

  // node_modules/d3-format/src/locale.js
  var map2 = Array.prototype.map;
  var prefixes = ["y", "z", "a", "f", "p", "n", "\xB5", "m", "", "k", "M", "G", "T", "P", "E", "Z", "Y"];
  function locale_default(locale2) {
    var group2 = locale2.grouping === void 0 || locale2.thousands === void 0 ? identity_default2 : formatGroup_default(map2.call(locale2.grouping, Number), locale2.thousands + ""), currencyPrefix = locale2.currency === void 0 ? "" : locale2.currency[0] + "", currencySuffix = locale2.currency === void 0 ? "" : locale2.currency[1] + "", decimal = locale2.decimal === void 0 ? "." : locale2.decimal + "", numerals = locale2.numerals === void 0 ? identity_default2 : formatNumerals_default(map2.call(locale2.numerals, String)), percent = locale2.percent === void 0 ? "%" : locale2.percent + "", minus = locale2.minus === void 0 ? "\u2212" : locale2.minus + "", nan2 = locale2.nan === void 0 ? "NaN" : locale2.nan + "";
    function newFormat(specifier) {
      specifier = formatSpecifier(specifier);
      var fill = specifier.fill, align = specifier.align, sign2 = specifier.sign, symbol = specifier.symbol, zero2 = specifier.zero, width = specifier.width, comma2 = specifier.comma, precision = specifier.precision, trim2 = specifier.trim, type2 = specifier.type;
      if (type2 === "n")
        comma2 = true, type2 = "g";
      else if (!formatTypes_default[type2])
        precision === void 0 && (precision = 12), trim2 = true, type2 = "g";
      if (zero2 || fill === "0" && align === "=")
        zero2 = true, fill = "0", align = "=";
      var prefix2 = symbol === "$" ? currencyPrefix : symbol === "#" && /[boxX]/.test(type2) ? "0" + type2.toLowerCase() : "", suffix = symbol === "$" ? currencySuffix : /[%p]/.test(type2) ? percent : "";
      var formatType = formatTypes_default[type2], maybeSuffix = /[defgprs%]/.test(type2);
      precision = precision === void 0 ? 6 : /[gprs]/.test(type2) ? Math.max(1, Math.min(21, precision)) : Math.max(0, Math.min(20, precision));
      function format2(value) {
        var valuePrefix = prefix2, valueSuffix = suffix, i, n, c;
        if (type2 === "c") {
          valueSuffix = formatType(value) + valueSuffix;
          value = "";
        } else {
          value = +value;
          var valueNegative = value < 0 || 1 / value < 0;
          value = isNaN(value) ? nan2 : formatType(Math.abs(value), precision);
          if (trim2)
            value = formatTrim_default(value);
          if (valueNegative && +value === 0 && sign2 !== "+")
            valueNegative = false;
          valuePrefix = (valueNegative ? sign2 === "(" ? sign2 : minus : sign2 === "-" || sign2 === "(" ? "" : sign2) + valuePrefix;
          valueSuffix = (type2 === "s" ? prefixes[8 + prefixExponent / 3] : "") + valueSuffix + (valueNegative && sign2 === "(" ? ")" : "");
          if (maybeSuffix) {
            i = -1, n = value.length;
            while (++i < n) {
              if (c = value.charCodeAt(i), 48 > c || c > 57) {
                valueSuffix = (c === 46 ? decimal + value.slice(i + 1) : value.slice(i)) + valueSuffix;
                value = value.slice(0, i);
                break;
              }
            }
          }
        }
        if (comma2 && !zero2)
          value = group2(value, Infinity);
        var length6 = valuePrefix.length + value.length + valueSuffix.length, padding = length6 < width ? new Array(width - length6 + 1).join(fill) : "";
        if (comma2 && zero2)
          value = group2(padding + value, padding.length ? width - valueSuffix.length : Infinity), padding = "";
        switch (align) {
          case "<":
            value = valuePrefix + value + valueSuffix + padding;
            break;
          case "=":
            value = valuePrefix + padding + value + valueSuffix;
            break;
          case "^":
            value = padding.slice(0, length6 = padding.length >> 1) + valuePrefix + value + valueSuffix + padding.slice(length6);
            break;
          default:
            value = padding + valuePrefix + value + valueSuffix;
            break;
        }
        return numerals(value);
      }
      format2.toString = function() {
        return specifier + "";
      };
      return format2;
    }
    function formatPrefix2(specifier, value) {
      var f = newFormat((specifier = formatSpecifier(specifier), specifier.type = "f", specifier)), e = Math.max(-8, Math.min(8, Math.floor(exponent_default(value) / 3))) * 3, k = Math.pow(10, -e), prefix2 = prefixes[8 + e / 3];
      return function(value2) {
        return f(k * value2) + prefix2;
      };
    }
    return {
      format: newFormat,
      formatPrefix: formatPrefix2
    };
  }

  // node_modules/d3-format/src/defaultLocale.js
  var locale;
  var format;
  var formatPrefix;
  defaultLocale({
    thousands: ",",
    grouping: [3],
    currency: ["$", ""]
  });
  function defaultLocale(definition) {
    locale = locale_default(definition);
    format = locale.format;
    formatPrefix = locale.formatPrefix;
    return locale;
  }

  // node_modules/d3-format/src/precisionFixed.js
  function precisionFixed_default(step) {
    return Math.max(0, -exponent_default(Math.abs(step)));
  }

  // node_modules/d3-format/src/precisionPrefix.js
  function precisionPrefix_default(step, value) {
    return Math.max(0, Math.max(-8, Math.min(8, Math.floor(exponent_default(value) / 3))) * 3 - exponent_default(Math.abs(step)));
  }

  // node_modules/d3-format/src/precisionRound.js
  function precisionRound_default(step, max4) {
    step = Math.abs(step), max4 = Math.abs(max4) - step;
    return Math.max(0, exponent_default(max4) - exponent_default(step)) + 1;
  }

  // node_modules/d3-scale/src/init.js
  function initRange(domain, range3) {
    switch (arguments.length) {
      case 0:
        break;
      case 1:
        this.range(domain);
        break;
      default:
        this.range(range3).domain(domain);
        break;
    }
    return this;
  }

  // node_modules/d3-scale/src/ordinal.js
  var implicit = Symbol("implicit");
  function ordinal() {
    var index3 = /* @__PURE__ */ new Map(), domain = [], range3 = [], unknown = implicit;
    function scale(d) {
      var key = d + "", i = index3.get(key);
      if (!i) {
        if (unknown !== implicit)
          return unknown;
        index3.set(key, i = domain.push(d));
      }
      return range3[(i - 1) % range3.length];
    }
    scale.domain = function(_) {
      if (!arguments.length)
        return domain.slice();
      domain = [], index3 = /* @__PURE__ */ new Map();
      for (const value of _) {
        const key = value + "";
        if (index3.has(key))
          continue;
        index3.set(key, domain.push(value));
      }
      return scale;
    };
    scale.range = function(_) {
      return arguments.length ? (range3 = Array.from(_), scale) : range3.slice();
    };
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : unknown;
    };
    scale.copy = function() {
      return ordinal(domain, range3).unknown(unknown);
    };
    initRange.apply(scale, arguments);
    return scale;
  }

  // node_modules/d3-scale/src/band.js
  function band() {
    var scale = ordinal().unknown(void 0), domain = scale.domain, ordinalRange = scale.range, r0 = 0, r1 = 1, step, bandwidth, round2 = false, paddingInner = 0, paddingOuter = 0, align = 0.5;
    delete scale.unknown;
    function rescale() {
      var n = domain().length, reverse3 = r1 < r0, start2 = reverse3 ? r1 : r0, stop = reverse3 ? r0 : r1;
      step = (stop - start2) / Math.max(1, n - paddingInner + paddingOuter * 2);
      if (round2)
        step = Math.floor(step);
      start2 += (stop - start2 - step * (n - paddingInner)) * align;
      bandwidth = step * (1 - paddingInner);
      if (round2)
        start2 = Math.round(start2), bandwidth = Math.round(bandwidth);
      var values2 = range_default(n).map(function(i) {
        return start2 + step * i;
      });
      return ordinalRange(reverse3 ? values2.reverse() : values2);
    }
    scale.domain = function(_) {
      return arguments.length ? (domain(_), rescale()) : domain();
    };
    scale.range = function(_) {
      return arguments.length ? ([r0, r1] = _, r0 = +r0, r1 = +r1, rescale()) : [r0, r1];
    };
    scale.rangeRound = function(_) {
      return [r0, r1] = _, r0 = +r0, r1 = +r1, round2 = true, rescale();
    };
    scale.bandwidth = function() {
      return bandwidth;
    };
    scale.step = function() {
      return step;
    };
    scale.round = function(_) {
      return arguments.length ? (round2 = !!_, rescale()) : round2;
    };
    scale.padding = function(_) {
      return arguments.length ? (paddingInner = Math.min(1, paddingOuter = +_), rescale()) : paddingInner;
    };
    scale.paddingInner = function(_) {
      return arguments.length ? (paddingInner = Math.min(1, _), rescale()) : paddingInner;
    };
    scale.paddingOuter = function(_) {
      return arguments.length ? (paddingOuter = +_, rescale()) : paddingOuter;
    };
    scale.align = function(_) {
      return arguments.length ? (align = Math.max(0, Math.min(1, _)), rescale()) : align;
    };
    scale.copy = function() {
      return band(domain(), [r0, r1]).round(round2).paddingInner(paddingInner).paddingOuter(paddingOuter).align(align);
    };
    return initRange.apply(rescale(), arguments);
  }

  // node_modules/d3-scale/src/constant.js
  function constants(x2) {
    return function() {
      return x2;
    };
  }

  // node_modules/d3-scale/src/number.js
  function number3(x2) {
    return +x2;
  }

  // node_modules/d3-scale/src/continuous.js
  var unit2 = [0, 1];
  function identity14(x2) {
    return x2;
  }
  function normalize(a, b) {
    return (b -= a = +a) ? function(x2) {
      return (x2 - a) / b;
    } : constants(isNaN(b) ? NaN : 0.5);
  }
  function clamper(a, b) {
    var t;
    if (a > b)
      t = a, a = b, b = t;
    return function(x2) {
      return Math.max(a, Math.min(b, x2));
    };
  }
  function bimap(domain, range3, interpolate) {
    var d0 = domain[0], d1 = domain[1], r0 = range3[0], r1 = range3[1];
    if (d1 < d0)
      d0 = normalize(d1, d0), r0 = interpolate(r1, r0);
    else
      d0 = normalize(d0, d1), r0 = interpolate(r0, r1);
    return function(x2) {
      return r0(d0(x2));
    };
  }
  function polymap(domain, range3, interpolate) {
    var j = Math.min(domain.length, range3.length) - 1, d = new Array(j), r = new Array(j), i = -1;
    if (domain[j] < domain[0]) {
      domain = domain.slice().reverse();
      range3 = range3.slice().reverse();
    }
    while (++i < j) {
      d[i] = normalize(domain[i], domain[i + 1]);
      r[i] = interpolate(range3[i], range3[i + 1]);
    }
    return function(x2) {
      var i2 = bisect_default(domain, x2, 1, j) - 1;
      return r[i2](d[i2](x2));
    };
  }
  function copy(source2, target) {
    return target.domain(source2.domain()).range(source2.range()).interpolate(source2.interpolate()).clamp(source2.clamp()).unknown(source2.unknown());
  }
  function transformer() {
    var domain = unit2, range3 = unit2, interpolate = value_default, transform2, untransform, unknown, clamp = identity14, piecewise, output, input;
    function rescale() {
      var n = Math.min(domain.length, range3.length);
      if (clamp !== identity14)
        clamp = clamper(domain[0], domain[n - 1]);
      piecewise = n > 2 ? polymap : bimap;
      output = input = null;
      return scale;
    }
    function scale(x2) {
      return x2 == null || isNaN(x2 = +x2) ? unknown : (output || (output = piecewise(domain.map(transform2), range3, interpolate)))(transform2(clamp(x2)));
    }
    scale.invert = function(y2) {
      return clamp(untransform((input || (input = piecewise(range3, domain.map(transform2), number_default2)))(y2)));
    };
    scale.domain = function(_) {
      return arguments.length ? (domain = Array.from(_, number3), rescale()) : domain.slice();
    };
    scale.range = function(_) {
      return arguments.length ? (range3 = Array.from(_), rescale()) : range3.slice();
    };
    scale.rangeRound = function(_) {
      return range3 = Array.from(_), interpolate = round_default, rescale();
    };
    scale.clamp = function(_) {
      return arguments.length ? (clamp = _ ? true : identity14, rescale()) : clamp !== identity14;
    };
    scale.interpolate = function(_) {
      return arguments.length ? (interpolate = _, rescale()) : interpolate;
    };
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : unknown;
    };
    return function(t, u) {
      transform2 = t, untransform = u;
      return rescale();
    };
  }
  function continuous() {
    return transformer()(identity14, identity14);
  }

  // node_modules/d3-scale/src/tickFormat.js
  function tickFormat(start2, stop, count, specifier) {
    var step = tickStep(start2, stop, count), precision;
    specifier = formatSpecifier(specifier == null ? ",f" : specifier);
    switch (specifier.type) {
      case "s": {
        var value = Math.max(Math.abs(start2), Math.abs(stop));
        if (specifier.precision == null && !isNaN(precision = precisionPrefix_default(step, value)))
          specifier.precision = precision;
        return formatPrefix(specifier, value);
      }
      case "":
      case "e":
      case "g":
      case "p":
      case "r": {
        if (specifier.precision == null && !isNaN(precision = precisionRound_default(step, Math.max(Math.abs(start2), Math.abs(stop)))))
          specifier.precision = precision - (specifier.type === "e");
        break;
      }
      case "f":
      case "%": {
        if (specifier.precision == null && !isNaN(precision = precisionFixed_default(step)))
          specifier.precision = precision - (specifier.type === "%") * 2;
        break;
      }
    }
    return format(specifier);
  }

  // node_modules/d3-scale/src/linear.js
  function linearish(scale) {
    var domain = scale.domain;
    scale.ticks = function(count) {
      var d = domain();
      return ticks_default(d[0], d[d.length - 1], count == null ? 10 : count);
    };
    scale.tickFormat = function(count, specifier) {
      var d = domain();
      return tickFormat(d[0], d[d.length - 1], count == null ? 10 : count, specifier);
    };
    scale.nice = function(count) {
      if (count == null)
        count = 10;
      var d = domain();
      var i0 = 0;
      var i1 = d.length - 1;
      var start2 = d[i0];
      var stop = d[i1];
      var prestep;
      var step;
      var maxIter = 10;
      if (stop < start2) {
        step = start2, start2 = stop, stop = step;
        step = i0, i0 = i1, i1 = step;
      }
      while (maxIter-- > 0) {
        step = tickIncrement(start2, stop, count);
        if (step === prestep) {
          d[i0] = start2;
          d[i1] = stop;
          return domain(d);
        } else if (step > 0) {
          start2 = Math.floor(start2 / step) * step;
          stop = Math.ceil(stop / step) * step;
        } else if (step < 0) {
          start2 = Math.ceil(start2 * step) / step;
          stop = Math.floor(stop * step) / step;
        } else {
          break;
        }
        prestep = step;
      }
      return scale;
    };
    return scale;
  }
  function linear2() {
    var scale = continuous();
    scale.copy = function() {
      return copy(scale, linear2());
    };
    initRange.apply(scale, arguments);
    return linearish(scale);
  }

  // node_modules/d3-scale-chromatic/src/colors.js
  function colors_default(specifier) {
    var n = specifier.length / 6 | 0, colors = new Array(n), i = 0;
    while (i < n)
      colors[i] = "#" + specifier.slice(i * 6, ++i * 6);
    return colors;
  }

  // node_modules/d3-scale-chromatic/src/categorical/Pastel1.js
  var Pastel1_default = colors_default("fbb4aeb3cde3ccebc5decbe4fed9a6ffffcce5d8bdfddaecf2f2f2");

  // node_modules/d3-shape/src/constant.js
  function constant_default4(x2) {
    return function constant() {
      return x2;
    };
  }

  // node_modules/d3-shape/src/array.js
  var slice4 = Array.prototype.slice;
  function array_default2(x2) {
    return typeof x2 === "object" && "length" in x2 ? x2 : Array.from(x2);
  }

  // node_modules/d3-shape/src/curve/linear.js
  function Linear(context) {
    this._context = context;
  }
  Linear.prototype = {
    areaStart: function() {
      this._line = 0;
    },
    areaEnd: function() {
      this._line = NaN;
    },
    lineStart: function() {
      this._point = 0;
    },
    lineEnd: function() {
      if (this._line || this._line !== 0 && this._point === 1)
        this._context.closePath();
      this._line = 1 - this._line;
    },
    point: function(x2, y2) {
      x2 = +x2, y2 = +y2;
      switch (this._point) {
        case 0:
          this._point = 1;
          this._line ? this._context.lineTo(x2, y2) : this._context.moveTo(x2, y2);
          break;
        case 1:
          this._point = 2;
        default:
          this._context.lineTo(x2, y2);
          break;
      }
    }
  };
  function linear_default(context) {
    return new Linear(context);
  }

  // node_modules/d3-shape/src/point.js
  function x(p) {
    return p[0];
  }
  function y(p) {
    return p[1];
  }

  // node_modules/d3-shape/src/line.js
  function line_default(x2, y2) {
    var defined = constant_default4(true), context = null, curve = linear_default, output = null;
    x2 = typeof x2 === "function" ? x2 : x2 === void 0 ? x : constant_default4(x2);
    y2 = typeof y2 === "function" ? y2 : y2 === void 0 ? y : constant_default4(y2);
    function line(data) {
      var i, n = (data = array_default2(data)).length, d, defined0 = false, buffer;
      if (context == null)
        output = curve(buffer = path_default());
      for (i = 0; i <= n; ++i) {
        if (!(i < n && defined(d = data[i], i, data)) === defined0) {
          if (defined0 = !defined0)
            output.lineStart();
          else
            output.lineEnd();
        }
        if (defined0)
          output.point(+x2(d, i, data), +y2(d, i, data));
      }
      if (buffer)
        return output = null, buffer + "" || null;
    }
    line.x = function(_) {
      return arguments.length ? (x2 = typeof _ === "function" ? _ : constant_default4(+_), line) : x2;
    };
    line.y = function(_) {
      return arguments.length ? (y2 = typeof _ === "function" ? _ : constant_default4(+_), line) : y2;
    };
    line.defined = function(_) {
      return arguments.length ? (defined = typeof _ === "function" ? _ : constant_default4(!!_), line) : defined;
    };
    line.curve = function(_) {
      return arguments.length ? (curve = _, context != null && (output = curve(context)), line) : curve;
    };
    line.context = function(_) {
      return arguments.length ? (_ == null ? context = output = null : output = curve(context = _), line) : context;
    };
    return line;
  }

  // node_modules/d3-zoom/src/transform.js
  function Transform(k, x2, y2) {
    this.k = k;
    this.x = x2;
    this.y = y2;
  }
  Transform.prototype = {
    constructor: Transform,
    scale: function(k) {
      return k === 1 ? this : new Transform(this.k * k, this.x, this.y);
    },
    translate: function(x2, y2) {
      return x2 === 0 & y2 === 0 ? this : new Transform(this.k, this.x + this.k * x2, this.y + this.k * y2);
    },
    apply: function(point2) {
      return [point2[0] * this.k + this.x, point2[1] * this.k + this.y];
    },
    applyX: function(x2) {
      return x2 * this.k + this.x;
    },
    applyY: function(y2) {
      return y2 * this.k + this.y;
    },
    invert: function(location) {
      return [(location[0] - this.x) / this.k, (location[1] - this.y) / this.k];
    },
    invertX: function(x2) {
      return (x2 - this.x) / this.k;
    },
    invertY: function(y2) {
      return (y2 - this.y) / this.k;
    },
    rescaleX: function(x2) {
      return x2.copy().domain(x2.range().map(this.invertX, this).map(x2.invert, x2));
    },
    rescaleY: function(y2) {
      return y2.copy().domain(y2.range().map(this.invertY, this).map(y2.invert, y2));
    },
    toString: function() {
      return "translate(" + this.x + "," + this.y + ") scale(" + this.k + ")";
    }
  };
  var identity15 = new Transform(1, 0, 0);
  transform.prototype = Transform.prototype;
  function transform(node) {
    while (!node.__zoom)
      if (!(node = node.parentNode))
        return identity15;
    return node.__zoom;
  }

  // node_modules/d3-collection/src/map.js
  var prefix = "$";
  function Map2() {
  }
  Map2.prototype = map3.prototype = {
    constructor: Map2,
    has: function(key) {
      return prefix + key in this;
    },
    get: function(key) {
      return this[prefix + key];
    },
    set: function(key, value) {
      this[prefix + key] = value;
      return this;
    },
    remove: function(key) {
      var property = prefix + key;
      return property in this && delete this[property];
    },
    clear: function() {
      for (var property in this)
        if (property[0] === prefix)
          delete this[property];
    },
    keys: function() {
      var keys4 = [];
      for (var property in this)
        if (property[0] === prefix)
          keys4.push(property.slice(1));
      return keys4;
    },
    values: function() {
      var values2 = [];
      for (var property in this)
        if (property[0] === prefix)
          values2.push(this[property]);
      return values2;
    },
    entries: function() {
      var entries = [];
      for (var property in this)
        if (property[0] === prefix)
          entries.push({ key: property.slice(1), value: this[property] });
      return entries;
    },
    size: function() {
      var size3 = 0;
      for (var property in this)
        if (property[0] === prefix)
          ++size3;
      return size3;
    },
    empty: function() {
      for (var property in this)
        if (property[0] === prefix)
          return false;
      return true;
    },
    each: function(f) {
      for (var property in this)
        if (property[0] === prefix)
          f(this[property], property.slice(1), this);
    }
  };
  function map3(object, f) {
    var map5 = new Map2();
    if (object instanceof Map2)
      object.each(function(value, key2) {
        map5.set(key2, value);
      });
    else if (Array.isArray(object)) {
      var i = -1, n = object.length, o;
      if (f == null)
        while (++i < n)
          map5.set(i, object[i]);
      else
        while (++i < n)
          map5.set(f(o = object[i], i, object), o);
    } else if (object)
      for (var key in object)
        map5.set(key, object[key]);
    return map5;
  }
  var map_default = map3;

  // node_modules/d3-collection/src/set.js
  function Set() {
  }
  var proto = map_default.prototype;
  Set.prototype = set3.prototype = {
    constructor: Set,
    has: proto.has,
    add: function(value) {
      value += "";
      this[prefix + value] = value;
      return this;
    },
    remove: proto.remove,
    clear: proto.clear,
    values: proto.keys,
    size: proto.size,
    empty: proto.empty,
    each: proto.each
  };
  function set3(object, f) {
    var set4 = new Set();
    if (object instanceof Set)
      object.each(function(value) {
        set4.add(value);
      });
    else if (object) {
      var i = -1, n = object.length;
      if (f == null)
        while (++i < n)
          set4.add(object[i]);
      else
        while (++i < n)
          set4.add(f(object[i], i, object));
    }
    return set4;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/namespaces.js
  var xhtml2 = "http://www.w3.org/1999/xhtml";
  var namespaces_default2 = {
    svg: "http://www.w3.org/2000/svg",
    xhtml: xhtml2,
    xlink: "http://www.w3.org/1999/xlink",
    xml: "http://www.w3.org/XML/1998/namespace",
    xmlns: "http://www.w3.org/2000/xmlns/"
  };

  // node_modules/d3-tip/node_modules/d3-selection/src/namespace.js
  function namespace_default2(name2) {
    var prefix2 = name2 += "", i = prefix2.indexOf(":");
    if (i >= 0 && (prefix2 = name2.slice(0, i)) !== "xmlns")
      name2 = name2.slice(i + 1);
    return namespaces_default2.hasOwnProperty(prefix2) ? { space: namespaces_default2[prefix2], local: name2 } : name2;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/creator.js
  function creatorInherit2(name2) {
    return function() {
      var document2 = this.ownerDocument, uri = this.namespaceURI;
      return uri === xhtml2 && document2.documentElement.namespaceURI === xhtml2 ? document2.createElement(name2) : document2.createElementNS(uri, name2);
    };
  }
  function creatorFixed2(fullname) {
    return function() {
      return this.ownerDocument.createElementNS(fullname.space, fullname.local);
    };
  }
  function creator_default2(name2) {
    var fullname = namespace_default2(name2);
    return (fullname.local ? creatorFixed2 : creatorInherit2)(fullname);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selector.js
  function none2() {
  }
  function selector_default2(selector) {
    return selector == null ? none2 : function() {
      return this.querySelector(selector);
    };
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/select.js
  function select_default4(select) {
    if (typeof select !== "function")
      select = selector_default2(select);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, subgroup = subgroups[j] = new Array(n), node, subnode, i = 0; i < n; ++i) {
        if ((node = group2[i]) && (subnode = select.call(node, node.__data__, i, group2))) {
          if ("__data__" in node)
            subnode.__data__ = node.__data__;
          subgroup[i] = subnode;
        }
      }
    }
    return new Selection3(subgroups, this._parents);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selectorAll.js
  function empty4() {
    return [];
  }
  function selectorAll_default2(selector) {
    return selector == null ? empty4 : function() {
      return this.querySelectorAll(selector);
    };
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/selectAll.js
  function selectAll_default3(select) {
    if (typeof select !== "function")
      select = selectorAll_default2(select);
    for (var groups = this._groups, m = groups.length, subgroups = [], parents = [], j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, node, i = 0; i < n; ++i) {
        if (node = group2[i]) {
          subgroups.push(select.call(node, node.__data__, i, group2));
          parents.push(node);
        }
      }
    }
    return new Selection3(subgroups, parents);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/matcher.js
  function matcher_default2(selector) {
    return function() {
      return this.matches(selector);
    };
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/filter.js
  function filter_default3(match10) {
    if (typeof match10 !== "function")
      match10 = matcher_default2(match10);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, subgroup = subgroups[j] = [], node, i = 0; i < n; ++i) {
        if ((node = group2[i]) && match10.call(node, node.__data__, i, group2)) {
          subgroup.push(node);
        }
      }
    }
    return new Selection3(subgroups, this._parents);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/sparse.js
  function sparse_default2(update2) {
    return new Array(update2.length);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/enter.js
  function enter_default2() {
    return new Selection3(this._enter || this._groups.map(sparse_default2), this._parents);
  }
  function EnterNode2(parent, datum2) {
    this.ownerDocument = parent.ownerDocument;
    this.namespaceURI = parent.namespaceURI;
    this._next = null;
    this._parent = parent;
    this.__data__ = datum2;
  }
  EnterNode2.prototype = {
    constructor: EnterNode2,
    appendChild: function(child) {
      return this._parent.insertBefore(child, this._next);
    },
    insertBefore: function(child, next) {
      return this._parent.insertBefore(child, next);
    },
    querySelector: function(selector) {
      return this._parent.querySelector(selector);
    },
    querySelectorAll: function(selector) {
      return this._parent.querySelectorAll(selector);
    }
  };

  // node_modules/d3-tip/node_modules/d3-selection/src/constant.js
  function constant_default6(x2) {
    return function() {
      return x2;
    };
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/data.js
  var keyPrefix = "$";
  function bindIndex2(parent, group2, enter, update2, exit, data) {
    var i = 0, node, groupLength = group2.length, dataLength = data.length;
    for (; i < dataLength; ++i) {
      if (node = group2[i]) {
        node.__data__ = data[i];
        update2[i] = node;
      } else {
        enter[i] = new EnterNode2(parent, data[i]);
      }
    }
    for (; i < groupLength; ++i) {
      if (node = group2[i]) {
        exit[i] = node;
      }
    }
  }
  function bindKey2(parent, group2, enter, update2, exit, data, key) {
    var i, node, nodeByKeyValue = {}, groupLength = group2.length, dataLength = data.length, keyValues = new Array(groupLength), keyValue;
    for (i = 0; i < groupLength; ++i) {
      if (node = group2[i]) {
        keyValues[i] = keyValue = keyPrefix + key.call(node, node.__data__, i, group2);
        if (keyValue in nodeByKeyValue) {
          exit[i] = node;
        } else {
          nodeByKeyValue[keyValue] = node;
        }
      }
    }
    for (i = 0; i < dataLength; ++i) {
      keyValue = keyPrefix + key.call(parent, data[i], i, data);
      if (node = nodeByKeyValue[keyValue]) {
        update2[i] = node;
        node.__data__ = data[i];
        nodeByKeyValue[keyValue] = null;
      } else {
        enter[i] = new EnterNode2(parent, data[i]);
      }
    }
    for (i = 0; i < groupLength; ++i) {
      if ((node = group2[i]) && nodeByKeyValue[keyValues[i]] === node) {
        exit[i] = node;
      }
    }
  }
  function data_default2(value, key) {
    if (!value) {
      data = new Array(this.size()), j = -1;
      this.each(function(d) {
        data[++j] = d;
      });
      return data;
    }
    var bind = key ? bindKey2 : bindIndex2, parents = this._parents, groups = this._groups;
    if (typeof value !== "function")
      value = constant_default6(value);
    for (var m = groups.length, update2 = new Array(m), enter = new Array(m), exit = new Array(m), j = 0; j < m; ++j) {
      var parent = parents[j], group2 = groups[j], groupLength = group2.length, data = value.call(parent, parent && parent.__data__, j, parents), dataLength = data.length, enterGroup = enter[j] = new Array(dataLength), updateGroup = update2[j] = new Array(dataLength), exitGroup = exit[j] = new Array(groupLength);
      bind(parent, group2, enterGroup, updateGroup, exitGroup, data, key);
      for (var i0 = 0, i1 = 0, previous, next; i0 < dataLength; ++i0) {
        if (previous = enterGroup[i0]) {
          if (i0 >= i1)
            i1 = i0 + 1;
          while (!(next = updateGroup[i1]) && ++i1 < dataLength)
            ;
          previous._next = next || null;
        }
      }
    }
    update2 = new Selection3(update2, parents);
    update2._enter = enter;
    update2._exit = exit;
    return update2;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/exit.js
  function exit_default2() {
    return new Selection3(this._exit || this._groups.map(sparse_default2), this._parents);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/join.js
  function join_default2(onenter, onupdate, onexit) {
    var enter = this.enter(), update2 = this, exit = this.exit();
    enter = typeof onenter === "function" ? onenter(enter) : enter.append(onenter + "");
    if (onupdate != null)
      update2 = onupdate(update2);
    if (onexit == null)
      exit.remove();
    else
      onexit(exit);
    return enter && update2 ? enter.merge(update2).order() : update2;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/merge.js
  function merge_default3(selection3) {
    for (var groups0 = this._groups, groups1 = selection3._groups, m0 = groups0.length, m1 = groups1.length, m = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m; ++j) {
      for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group0[i] || group1[i]) {
          merge[i] = node;
        }
      }
    }
    for (; j < m0; ++j) {
      merges[j] = groups0[j];
    }
    return new Selection3(merges, this._parents);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/order.js
  function order_default2() {
    for (var groups = this._groups, j = -1, m = groups.length; ++j < m; ) {
      for (var group2 = groups[j], i = group2.length - 1, next = group2[i], node; --i >= 0; ) {
        if (node = group2[i]) {
          if (next && node.compareDocumentPosition(next) ^ 4)
            next.parentNode.insertBefore(node, next);
          next = node;
        }
      }
    }
    return this;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/sort.js
  function sort_default2(compare) {
    if (!compare)
      compare = ascending2;
    function compareNode(a, b) {
      return a && b ? compare(a.__data__, b.__data__) : !a - !b;
    }
    for (var groups = this._groups, m = groups.length, sortgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group2 = groups[j], n = group2.length, sortgroup = sortgroups[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group2[i]) {
          sortgroup[i] = node;
        }
      }
      sortgroup.sort(compareNode);
    }
    return new Selection3(sortgroups, this._parents).order();
  }
  function ascending2(a, b) {
    return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/call.js
  function call_default2() {
    var callback = arguments[0];
    arguments[0] = this;
    callback.apply(null, arguments);
    return this;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/nodes.js
  function nodes_default2() {
    var nodes = new Array(this.size()), i = -1;
    this.each(function() {
      nodes[++i] = this;
    });
    return nodes;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/node.js
  function node_default2() {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group2 = groups[j], i = 0, n = group2.length; i < n; ++i) {
        var node = group2[i];
        if (node)
          return node;
      }
    }
    return null;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/size.js
  function size_default2() {
    var size3 = 0;
    this.each(function() {
      ++size3;
    });
    return size3;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/empty.js
  function empty_default2() {
    return !this.node();
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/each.js
  function each_default2(callback) {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group2 = groups[j], i = 0, n = group2.length, node; i < n; ++i) {
        if (node = group2[i])
          callback.call(node, node.__data__, i, group2);
      }
    }
    return this;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/attr.js
  function attrRemove3(name2) {
    return function() {
      this.removeAttribute(name2);
    };
  }
  function attrRemoveNS3(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant3(name2, value) {
    return function() {
      this.setAttribute(name2, value);
    };
  }
  function attrConstantNS3(fullname, value) {
    return function() {
      this.setAttributeNS(fullname.space, fullname.local, value);
    };
  }
  function attrFunction3(name2, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        this.removeAttribute(name2);
      else
        this.setAttribute(name2, v);
    };
  }
  function attrFunctionNS3(fullname, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        this.removeAttributeNS(fullname.space, fullname.local);
      else
        this.setAttributeNS(fullname.space, fullname.local, v);
    };
  }
  function attr_default3(name2, value) {
    var fullname = namespace_default2(name2);
    if (arguments.length < 2) {
      var node = this.node();
      return fullname.local ? node.getAttributeNS(fullname.space, fullname.local) : node.getAttribute(fullname);
    }
    return this.each((value == null ? fullname.local ? attrRemoveNS3 : attrRemove3 : typeof value === "function" ? fullname.local ? attrFunctionNS3 : attrFunction3 : fullname.local ? attrConstantNS3 : attrConstant3)(fullname, value));
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/window.js
  function window_default2(node) {
    return node.ownerDocument && node.ownerDocument.defaultView || node.document && node || node.defaultView;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/style.js
  function styleRemove3(name2) {
    return function() {
      this.style.removeProperty(name2);
    };
  }
  function styleConstant3(name2, value, priority) {
    return function() {
      this.style.setProperty(name2, value, priority);
    };
  }
  function styleFunction3(name2, value, priority) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        this.style.removeProperty(name2);
      else
        this.style.setProperty(name2, v, priority);
    };
  }
  function style_default3(name2, value, priority) {
    return arguments.length > 1 ? this.each((value == null ? styleRemove3 : typeof value === "function" ? styleFunction3 : styleConstant3)(name2, value, priority == null ? "" : priority)) : styleValue2(this.node(), name2);
  }
  function styleValue2(node, name2) {
    return node.style.getPropertyValue(name2) || window_default2(node).getComputedStyle(node, null).getPropertyValue(name2);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/property.js
  function propertyRemove2(name2) {
    return function() {
      delete this[name2];
    };
  }
  function propertyConstant2(name2, value) {
    return function() {
      this[name2] = value;
    };
  }
  function propertyFunction2(name2, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        delete this[name2];
      else
        this[name2] = v;
    };
  }
  function property_default2(name2, value) {
    return arguments.length > 1 ? this.each((value == null ? propertyRemove2 : typeof value === "function" ? propertyFunction2 : propertyConstant2)(name2, value)) : this.node()[name2];
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/classed.js
  function classArray2(string3) {
    return string3.trim().split(/^|\s+/);
  }
  function classList2(node) {
    return node.classList || new ClassList2(node);
  }
  function ClassList2(node) {
    this._node = node;
    this._names = classArray2(node.getAttribute("class") || "");
  }
  ClassList2.prototype = {
    add: function(name2) {
      var i = this._names.indexOf(name2);
      if (i < 0) {
        this._names.push(name2);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    remove: function(name2) {
      var i = this._names.indexOf(name2);
      if (i >= 0) {
        this._names.splice(i, 1);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    contains: function(name2) {
      return this._names.indexOf(name2) >= 0;
    }
  };
  function classedAdd2(node, names) {
    var list = classList2(node), i = -1, n = names.length;
    while (++i < n)
      list.add(names[i]);
  }
  function classedRemove2(node, names) {
    var list = classList2(node), i = -1, n = names.length;
    while (++i < n)
      list.remove(names[i]);
  }
  function classedTrue2(names) {
    return function() {
      classedAdd2(this, names);
    };
  }
  function classedFalse2(names) {
    return function() {
      classedRemove2(this, names);
    };
  }
  function classedFunction2(names, value) {
    return function() {
      (value.apply(this, arguments) ? classedAdd2 : classedRemove2)(this, names);
    };
  }
  function classed_default2(name2, value) {
    var names = classArray2(name2 + "");
    if (arguments.length < 2) {
      var list = classList2(this.node()), i = -1, n = names.length;
      while (++i < n)
        if (!list.contains(names[i]))
          return false;
      return true;
    }
    return this.each((typeof value === "function" ? classedFunction2 : value ? classedTrue2 : classedFalse2)(names, value));
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/text.js
  function textRemove2() {
    this.textContent = "";
  }
  function textConstant3(value) {
    return function() {
      this.textContent = value;
    };
  }
  function textFunction3(value) {
    return function() {
      var v = value.apply(this, arguments);
      this.textContent = v == null ? "" : v;
    };
  }
  function text_default3(value) {
    return arguments.length ? this.each(value == null ? textRemove2 : (typeof value === "function" ? textFunction3 : textConstant3)(value)) : this.node().textContent;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/html.js
  function htmlRemove2() {
    this.innerHTML = "";
  }
  function htmlConstant2(value) {
    return function() {
      this.innerHTML = value;
    };
  }
  function htmlFunction2(value) {
    return function() {
      var v = value.apply(this, arguments);
      this.innerHTML = v == null ? "" : v;
    };
  }
  function html_default2(value) {
    return arguments.length ? this.each(value == null ? htmlRemove2 : (typeof value === "function" ? htmlFunction2 : htmlConstant2)(value)) : this.node().innerHTML;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/raise.js
  function raise2() {
    if (this.nextSibling)
      this.parentNode.appendChild(this);
  }
  function raise_default2() {
    return this.each(raise2);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/lower.js
  function lower2() {
    if (this.previousSibling)
      this.parentNode.insertBefore(this, this.parentNode.firstChild);
  }
  function lower_default2() {
    return this.each(lower2);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/append.js
  function append_default2(name2) {
    var create2 = typeof name2 === "function" ? name2 : creator_default2(name2);
    return this.select(function() {
      return this.appendChild(create2.apply(this, arguments));
    });
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/insert.js
  function constantNull2() {
    return null;
  }
  function insert_default2(name2, before) {
    var create2 = typeof name2 === "function" ? name2 : creator_default2(name2), select = before == null ? constantNull2 : typeof before === "function" ? before : selector_default2(before);
    return this.select(function() {
      return this.insertBefore(create2.apply(this, arguments), select.apply(this, arguments) || null);
    });
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/remove.js
  function remove2() {
    var parent = this.parentNode;
    if (parent)
      parent.removeChild(this);
  }
  function remove_default3() {
    return this.each(remove2);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/clone.js
  function selection_cloneShallow2() {
    var clone = this.cloneNode(false), parent = this.parentNode;
    return parent ? parent.insertBefore(clone, this.nextSibling) : clone;
  }
  function selection_cloneDeep2() {
    var clone = this.cloneNode(true), parent = this.parentNode;
    return parent ? parent.insertBefore(clone, this.nextSibling) : clone;
  }
  function clone_default2(deep) {
    return this.select(deep ? selection_cloneDeep2 : selection_cloneShallow2);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/datum.js
  function datum_default2(value) {
    return arguments.length ? this.property("__data__", value) : this.node().__data__;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/on.js
  var filterEvents = {};
  var event = null;
  if (typeof document !== "undefined") {
    element = document.documentElement;
    if (!("onmouseenter" in element)) {
      filterEvents = { mouseenter: "mouseover", mouseleave: "mouseout" };
    }
  }
  var element;
  function filterContextListener(listener, index3, group2) {
    listener = contextListener2(listener, index3, group2);
    return function(event2) {
      var related = event2.relatedTarget;
      if (!related || related !== this && !(related.compareDocumentPosition(this) & 8)) {
        listener.call(this, event2);
      }
    };
  }
  function contextListener2(listener, index3, group2) {
    return function(event1) {
      var event0 = event;
      event = event1;
      try {
        listener.call(this, this.__data__, index3, group2);
      } finally {
        event = event0;
      }
    };
  }
  function parseTypenames3(typenames) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name2 = "", i = t.indexOf(".");
      if (i >= 0)
        name2 = t.slice(i + 1), t = t.slice(0, i);
      return { type: t, name: name2 };
    });
  }
  function onRemove2(typename) {
    return function() {
      var on = this.__on;
      if (!on)
        return;
      for (var j = 0, i = -1, m = on.length, o; j < m; ++j) {
        if (o = on[j], (!typename.type || o.type === typename.type) && o.name === typename.name) {
          this.removeEventListener(o.type, o.listener, o.capture);
        } else {
          on[++i] = o;
        }
      }
      if (++i)
        on.length = i;
      else
        delete this.__on;
    };
  }
  function onAdd2(typename, value, capture) {
    var wrap = filterEvents.hasOwnProperty(typename.type) ? filterContextListener : contextListener2;
    return function(d, i, group2) {
      var on = this.__on, o, listener = wrap(value, i, group2);
      if (on)
        for (var j = 0, m = on.length; j < m; ++j) {
          if ((o = on[j]).type === typename.type && o.name === typename.name) {
            this.removeEventListener(o.type, o.listener, o.capture);
            this.addEventListener(o.type, o.listener = listener, o.capture = capture);
            o.value = value;
            return;
          }
        }
      this.addEventListener(typename.type, listener, capture);
      o = { type: typename.type, name: typename.name, value, listener, capture };
      if (!on)
        this.__on = [o];
      else
        on.push(o);
    };
  }
  function on_default3(typename, value, capture) {
    var typenames = parseTypenames3(typename + ""), i, n = typenames.length, t;
    if (arguments.length < 2) {
      var on = this.node().__on;
      if (on)
        for (var j = 0, m = on.length, o; j < m; ++j) {
          for (i = 0, o = on[j]; i < n; ++i) {
            if ((t = typenames[i]).type === o.type && t.name === o.name) {
              return o.value;
            }
          }
        }
      return;
    }
    on = value ? onAdd2 : onRemove2;
    if (capture == null)
      capture = false;
    for (i = 0; i < n; ++i)
      this.each(on(typenames[i], value, capture));
    return this;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/dispatch.js
  function dispatchEvent2(node, type2, params) {
    var window2 = window_default2(node), event2 = window2.CustomEvent;
    if (typeof event2 === "function") {
      event2 = new event2(type2, params);
    } else {
      event2 = window2.document.createEvent("Event");
      if (params)
        event2.initEvent(type2, params.bubbles, params.cancelable), event2.detail = params.detail;
      else
        event2.initEvent(type2, false, false);
    }
    node.dispatchEvent(event2);
  }
  function dispatchConstant2(type2, params) {
    return function() {
      return dispatchEvent2(this, type2, params);
    };
  }
  function dispatchFunction2(type2, params) {
    return function() {
      return dispatchEvent2(this, type2, params.apply(this, arguments));
    };
  }
  function dispatch_default3(type2, params) {
    return this.each((typeof params === "function" ? dispatchFunction2 : dispatchConstant2)(type2, params));
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/index.js
  var root2 = [null];
  function Selection3(groups, parents) {
    this._groups = groups;
    this._parents = parents;
  }
  function selection2() {
    return new Selection3([[document.documentElement]], root2);
  }
  Selection3.prototype = selection2.prototype = {
    constructor: Selection3,
    select: select_default4,
    selectAll: selectAll_default3,
    filter: filter_default3,
    data: data_default2,
    enter: enter_default2,
    exit: exit_default2,
    join: join_default2,
    merge: merge_default3,
    order: order_default2,
    sort: sort_default2,
    call: call_default2,
    nodes: nodes_default2,
    node: node_default2,
    size: size_default2,
    empty: empty_default2,
    each: each_default2,
    attr: attr_default3,
    style: style_default3,
    property: property_default2,
    classed: classed_default2,
    text: text_default3,
    html: html_default2,
    raise: raise_default2,
    lower: lower_default2,
    append: append_default2,
    insert: insert_default2,
    remove: remove_default3,
    clone: clone_default2,
    datum: datum_default2,
    on: on_default3,
    dispatch: dispatch_default3
  };
  var selection_default3 = selection2;

  // node_modules/d3-tip/node_modules/d3-selection/src/select.js
  function select_default5(selector) {
    return typeof selector === "string" ? new Selection3([[document.querySelector(selector)]], [document.documentElement]) : new Selection3([[selector]], root2);
  }

  // node_modules/d3-tip/index.js
  function d3_tip_default() {
    var direction = d3TipDirection, offset = d3TipOffset, html = d3TipHTML, rootElement = document.body, node = initNode(), svg = null, point2 = null, target = null;
    function tip(vis) {
      svg = getSVGNode(vis);
      if (!svg)
        return;
      point2 = svg.createSVGPoint();
      rootElement.appendChild(node);
    }
    tip.show = function() {
      var args = Array.prototype.slice.call(arguments);
      if (args[args.length - 1] instanceof SVGElement)
        target = args.pop();
      var content = html.apply(this, args), poffset = offset.apply(this, args), dir = direction.apply(this, args), nodel = getNodeEl(), i = directions.length, coords, scrollTop = document.documentElement.scrollTop || rootElement.scrollTop, scrollLeft = document.documentElement.scrollLeft || rootElement.scrollLeft;
      nodel.html(content).style("opacity", 1).style("pointer-events", "all");
      while (i--)
        nodel.classed(directions[i], false);
      coords = directionCallbacks.get(dir).apply(this);
      nodel.classed(dir, true).style("top", coords.top + poffset[0] + scrollTop + "px").style("left", coords.left + poffset[1] + scrollLeft + "px");
      return tip;
    };
    tip.hide = function() {
      var nodel = getNodeEl();
      nodel.style("opacity", 0).style("pointer-events", "none");
      return tip;
    };
    tip.attr = function(n, v) {
      if (arguments.length < 2 && typeof n === "string") {
        return getNodeEl().attr(n);
      }
      var args = Array.prototype.slice.call(arguments);
      selection_default3.prototype.attr.apply(getNodeEl(), args);
      return tip;
    };
    tip.style = function(n, v) {
      if (arguments.length < 2 && typeof n === "string") {
        return getNodeEl().style(n);
      }
      var args = Array.prototype.slice.call(arguments);
      selection_default3.prototype.style.apply(getNodeEl(), args);
      return tip;
    };
    tip.direction = function(v) {
      if (!arguments.length)
        return direction;
      direction = v == null ? v : functor(v);
      return tip;
    };
    tip.offset = function(v) {
      if (!arguments.length)
        return offset;
      offset = v == null ? v : functor(v);
      return tip;
    };
    tip.html = function(v) {
      if (!arguments.length)
        return html;
      html = v == null ? v : functor(v);
      return tip;
    };
    tip.rootElement = function(v) {
      if (!arguments.length)
        return rootElement;
      rootElement = v == null ? v : functor(v);
      return tip;
    };
    tip.destroy = function() {
      if (node) {
        getNodeEl().remove();
        node = null;
      }
      return tip;
    };
    function d3TipDirection() {
      return "n";
    }
    function d3TipOffset() {
      return [0, 0];
    }
    function d3TipHTML() {
      return " ";
    }
    var directionCallbacks = map_default({
      n: directionNorth,
      s: directionSouth,
      e: directionEast,
      w: directionWest,
      nw: directionNorthWest,
      ne: directionNorthEast,
      sw: directionSouthWest,
      se: directionSouthEast
    }), directions = directionCallbacks.keys();
    function directionNorth() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.n.y - node.offsetHeight,
        left: bbox.n.x - node.offsetWidth / 2
      };
    }
    function directionSouth() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.s.y,
        left: bbox.s.x - node.offsetWidth / 2
      };
    }
    function directionEast() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.e.y - node.offsetHeight / 2,
        left: bbox.e.x
      };
    }
    function directionWest() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.w.y - node.offsetHeight / 2,
        left: bbox.w.x - node.offsetWidth
      };
    }
    function directionNorthWest() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.nw.y - node.offsetHeight,
        left: bbox.nw.x - node.offsetWidth
      };
    }
    function directionNorthEast() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.ne.y - node.offsetHeight,
        left: bbox.ne.x
      };
    }
    function directionSouthWest() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.sw.y,
        left: bbox.sw.x - node.offsetWidth
      };
    }
    function directionSouthEast() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.se.y,
        left: bbox.se.x
      };
    }
    function initNode() {
      var div = select_default5(document.createElement("div"));
      div.style("position", "absolute").style("top", 0).style("opacity", 0).style("pointer-events", "none").style("box-sizing", "border-box");
      return div.node();
    }
    function getSVGNode(element) {
      var svgNode2 = element.node();
      if (!svgNode2)
        return null;
      if (svgNode2.tagName.toLowerCase() === "svg")
        return svgNode2;
      return svgNode2.ownerSVGElement;
    }
    function getNodeEl() {
      if (node == null) {
        node = initNode();
        rootElement.appendChild(node);
      }
      return select_default5(node);
    }
    function getScreenBBox(targetShape) {
      var targetel = target || targetShape;
      while (targetel.getScreenCTM == null && targetel.parentNode != null) {
        targetel = targetel.parentNode;
      }
      var bbox = {}, matrix = targetel.getScreenCTM(), tbbox = targetel.getBBox(), width = tbbox.width, height = tbbox.height, x2 = tbbox.x, y2 = tbbox.y;
      point2.x = x2;
      point2.y = y2;
      bbox.nw = point2.matrixTransform(matrix);
      point2.x += width;
      bbox.ne = point2.matrixTransform(matrix);
      point2.y += height;
      bbox.se = point2.matrixTransform(matrix);
      point2.x -= width;
      bbox.sw = point2.matrixTransform(matrix);
      point2.y -= height / 2;
      bbox.w = point2.matrixTransform(matrix);
      point2.x += width;
      bbox.e = point2.matrixTransform(matrix);
      point2.x -= width / 2;
      point2.y -= height / 2;
      bbox.n = point2.matrixTransform(matrix);
      point2.y += height;
      bbox.s = point2.matrixTransform(matrix);
      return bbox;
    }
    function functor(v) {
      return typeof v === "function" ? v : function() {
        return v;
      };
    }
    return tip;
  }

  // output-es/App.BarChart/foreign.js
  function curry4(f) {
    return (x1) => (x2) => (x3) => (x4) => f(x1, x2, x3, x4);
  }
  function colorShade(col, amt) {
    col = col.replace(/^#/, "");
    if (col.length === 3)
      col = col[0] + col[0] + col[1] + col[1] + col[2] + col[2];
    let [r, g, b] = col.match(/.{2}/g);
    [r, g, b] = [parseInt(r, 16) + amt, parseInt(g, 16) + amt, parseInt(b, 16) + amt];
    r = Math.max(Math.min(255, r), 0).toString(16);
    g = Math.max(Math.min(255, g), 0).toString(16);
    b = Math.max(Math.min(255, b), 0).toString(16);
    const rr = (r.length < 2 ? "0" : "") + r;
    const gg = (g.length < 2 ? "0" : "") + g;
    const bb = (b.length < 2 ? "0" : "") + b;
    return `#${rr}${gg}${bb}`;
  }
  function drawBarChart_(id3, childIndex, {
    caption,
    data
  }, listener) {
    return () => {
      const childId = id3 + "-" + childIndex;
      const margin = { top: 15, right: 0, bottom: 40, left: 30 }, width = 200 - margin.left - margin.right, height = 185 - margin.top - margin.bottom;
      const div = select_default2("#" + id3);
      div.selectAll("#" + childId).remove();
      const svg = div.append("svg").attr("width", width + margin.left + margin.right).attr("height", height + margin.top + margin.bottom).attr("id", childId).append("g").attr("transform", `translate(${margin.left}, ${margin.top})`);
      const tip = d3_tip_default().attr("class", "d3-tip").offset([0, 0]).html((_, d) => d.y._1);
      svg.call(tip);
      const x2 = band().range([0, width]).domain(data.map((d) => d.x._1)).padding(0.2);
      svg.append("g").attr("transform", "translate(0," + height + ")").call(axisBottom(x2)).selectAll("text").style("text-anchor", "middle");
      const nearest = 100, y_max = Math.ceil(Math.max(...data.map((d) => d.y._1)) / nearest) * nearest;
      const y2 = linear2().domain([0, y_max]).range([height, 0]);
      const tickEvery = nearest / 2, ticks = Array.from(Array(y_max / tickEvery + 1).keys()).map((n) => n * tickEvery);
      const yAxis = axisLeft(y2).tickValues(ticks);
      svg.append("g").call(yAxis);
      const barFill = "#dcdcdc";
      svg.selectAll("rect").data([...data.entries()]).enter().append("rect").attr("x", ([, d]) => x2(d.x._1)).attr("y", ([, d]) => y2(d.y._1 + 1)).attr("width", x2.bandwidth()).attr("height", ([, d]) => height - y2(d.y._1)).attr("fill", ([, d]) => d.y._2 ? colorShade(barFill, -40) : barFill).attr("class", ([, d]) => d.y._2 ? "bar-selected" : "bar-unselected").on("mousedown", (e, d) => {
        console.log(`mousedown ${d[0]}`);
        listener(e);
      });
      svg.append("text").text(caption._1).attr("x", width / 2).attr("y", height + 35).attr("class", "title-text").attr("dominant-baseline", "bottom").attr("text-anchor", "middle");
    };
  }
  var drawBarChart = curry4(drawBarChart_);

  // output-es/App.BarChart/index.js
  var match2 = /* @__PURE__ */ (() => string.match(annBoolean))();
  var neg2 = /* @__PURE__ */ (() => joinSemilatticeVal(joinSemilatticeBoolean).neg)();
  var reflectDictValBooleanBarC = { from: () => (r) => ({ x: match2($$get("x")(r)), y: get_intOrNumber("y")(r) }) };
  var reflectDictValBooleanBarC1 = {
    from: () => (r) => ({ caption: match2($$get("caption")(r)), data: arrayMap(record2(reflectDictValBooleanBarC.from()))(reflectArray.from()($$get("data")(r))) })
  };
  var barChartHandler = (ev) => toggleConstrArg("BarChart")(0)(toggleField("data")(selectNth(definitely("index within bounds")(index2(definitely("absurd")(nullable(
    _target(ev),
    Nothing,
    Just
  )).__data__)(0)))(neg2)));

  // output-es/App.LineChart/foreign.js
  function curry42(f) {
    return (x1) => (x2) => (x3) => (x4) => f(x1, x2, x3, x4);
  }
  function colorShade2(col, amt) {
    col = col.replace(/^#/, "");
    if (col.length === 3)
      col = col[0] + col[0] + col[1] + col[1] + col[2] + col[2];
    let [r, g, b] = col.match(/.{2}/g);
    [r, g, b] = [parseInt(r, 16) + amt, parseInt(g, 16) + amt, parseInt(b, 16) + amt];
    r = Math.max(Math.min(255, r), 0).toString(16);
    g = Math.max(Math.min(255, g), 0).toString(16);
    b = Math.max(Math.min(255, b), 0).toString(16);
    const rr = (r.length < 2 ? "0" : "") + r;
    const gg = (g.length < 2 ? "0" : "") + g;
    const bb = (b.length < 2 ? "0" : "") + b;
    return `#${rr}${gg}${bb}`;
  }
  function max_y(linePlot) {
    return Math.max(...linePlot.data.map((point2) => point2.y._1));
  }
  function min_x(linePlot) {
    return Math.min(...linePlot.data.map((point2) => point2.x._1));
  }
  function max_x(linePlot) {
    return Math.max(...linePlot.data.map((point2) => point2.x._1));
  }
  function drawLineChart_(id3, childIndex, {
    caption,
    plots
  }, listener) {
    return () => {
      const childId = id3 + "-" + childIndex;
      const margin = { top: 15, right: 65, bottom: 40, left: 30 }, width = 230 - margin.left - margin.right, height = 185 - margin.top - margin.bottom, y_max = Math.max(...plots.map(max_y)), x_min = Math.min(...plots.map(min_x)), x_max = Math.max(...plots.map(max_x)), names = plots.map((plot) => plot.name._1);
      const div = select_default2("#" + id3);
      div.selectAll("#" + childId).remove();
      const svg = div.append("svg").attr("width", width + margin.left + margin.right).attr("height", height + margin.top + margin.bottom).attr("id", childId).append("g").attr("transform", `translate(${margin.left}, ${margin.top})`);
      const x2 = linear2().domain([x_min, x_max]).range([0, width]), y2 = linear2().domain([0, y_max]).range([height, 0]);
      const line1 = line_default().x((d) => x2(d.x._1)).y((d) => y2(d.y._1));
      const color2 = ordinal(Pastel1_default);
      svg.selectAll("lines").data([...plots.entries()]).enter().append("g").append("path").attr("fill", "none").attr("stroke", ([, d]) => color2(names.indexOf(d.name._1))).attr("stroke-width", 1).attr("class", "line").attr("d", ([_, d]) => line1(d.data));
      const smallRadius = 2;
      for (const n_plot of plots.entries()) {
        const [i, plot] = n_plot, col = color2(names.indexOf(plot.name._1));
        svg.selectAll("markers").data([...plot.data.entries()].map(([j, ns]) => [[i, j], ns])).enter().append("g").append("circle").attr("r", ([, d]) => d.y._2 ? smallRadius * 2 : smallRadius).attr("cx", ([, d]) => x2(d.x._1)).attr("cy", ([, d]) => y2(d.y._1)).attr("fill", col).attr("stroke", ([, d]) => d.y._2 ? colorShade2(col, -30) : col).on("mousedown", (e, d) => {
          console.log(`mousedown ${d[0]}`);
          listener(e);
        });
      }
      svg.append("g").attr("transform", `translate(0, ${height})`).call(axisBottom(x2).ticks(x_max - x_min).tickFormat(format("d")));
      svg.append("g").call(axisLeft(y2).tickSizeOuter(0).ticks(3).tickFormat(format(".1f")));
      const legendLineHeight = 15, legendStart = width + margin.left / 2;
      svg.append("rect").attr("transform", `translate(${legendStart}, ${legendLineHeight * (names.length - 1) + 2})`).attr("x", 0).attr("y", 0).attr("stroke", "lightgray").attr("fill", "none").attr("height", legendLineHeight * names.length).attr("width", margin.right - 16);
      const legend = svg.selectAll("legend").data(names).enter().append("g").attr("class", "legend").attr(
        "transform",
        (d, i) => `translate(${legendStart}, ${height / 2 - margin.top + i * legendLineHeight})`
      );
      legend.append("text").text((d) => d).attr("font-size", 11).attr("transform", "translate(15, 9)");
      legend.append("circle").attr("fill", (d) => color2(names.indexOf(d))).attr("r", smallRadius).attr("cx", legendLineHeight / 2 - smallRadius / 2).attr("cy", legendLineHeight / 2 - smallRadius / 2);
      svg.append("text").text(caption._1).attr("x", width / 2).attr("y", height + 35).attr("class", "title-text").attr("dominant-baseline", "bottom").attr("text-anchor", "middle");
    };
  }
  var drawLineChart = curry42(drawLineChart_);

  // output-es/App.LineChart/index.js
  var match3 = /* @__PURE__ */ (() => string.match(annBoolean))();
  var neg3 = /* @__PURE__ */ (() => joinSemilatticeVal(joinSemilatticeBoolean).neg)();
  var reflectDictValBooleanPoin = { from: () => (r) => ({ x: get_intOrNumber("x")(r), y: get_intOrNumber("y")(r) }) };
  var reflectDictValBooleanLine = {
    from: () => (r) => ({ name: match3($$get("name")(r)), data: arrayMap(record2(reflectDictValBooleanPoin.from()))(reflectArray.from()($$get("data")(r))) })
  };
  var reflectValBooleanLinePlot = {
    from: () => (v) => {
      if (v.tag === "Constr") {
        if (v._3.tag === "Cons") {
          if (v._3._2.tag === "Nil") {
            if (v._2 === "LinePlot") {
              return reflectDictValBooleanLine.from()(match(v._3._1)._1);
            }
            fail();
          }
          fail();
        }
        fail();
      }
      fail();
    }
  };
  var reflectDictValBooleanLine1 = {
    from: () => (r) => ({ caption: match3($$get("caption")(r)), plots: arrayMap(reflectValBooleanLinePlot.from())(reflectArray.from()($$get("plots")(r))) })
  };
  var lineChartHandler = (ev) => {
    const xy = definitely("index within bounds")(index2(definitely("absurd")(nullable(
      _target(ev),
      Nothing,
      Just
    )).__data__)(0));
    return toggleConstrArg("LineChart")(0)(toggleField("plots")(selectNth(definitely("index within bounds")(index2(xy)(0)))(toggleConstrArg("LinePlot")(0)(toggleField("data")(selectNth(definitely("index within bounds")(index2(xy)(1)))(neg3))))));
  };

  // output-es/App.MatrixView/foreign.js
  function curry43(f) {
    return (x1) => (x2) => (x3) => (x4) => f(x1, x2, x3, x4);
  }
  function drawMatrix_(id3, childIndex, {
    title: title2,
    matrix: { _1: nss, _2: { _1: i_max, _2: j_max } }
  }, listener) {
    return () => {
      const childId = id3 + "-" + childIndex;
      const strokeWidth = 0.5;
      const w = 30, h = 30;
      const div = select_default2("#" + id3);
      const [width, height] = [w * j_max + strokeWidth, h * i_max + strokeWidth];
      const hMargin = w / 2;
      const vMargin = h / 2;
      div.selectAll("#" + childId).remove();
      const svg = div.append("svg").attr("id", childId).attr("width", width + hMargin).attr("height", height + vMargin);
      const grp = svg.selectAll("g").data([...nss.entries()].map(([i, ns]) => [i + 1, ns])).enter().append("g").attr(
        "transform",
        (_, i) => `translate(${strokeWidth / 2 + hMargin / 2}, ${h * i + strokeWidth / 2 + vMargin})`
      );
      const rect = grp.selectAll("rect").data(([i, ns]) => [...ns.entries()].map(([j, n]) => [[i, j + 1], n])).enter();
      rect.append("rect").attr("x", (_, j) => w * j).attr("width", w).attr("height", h).attr("class", ([, n]) => n._2 ? "matrix-cell-selected" : "matrix-cell-unselected").attr("stroke-width", strokeWidth);
      rect.append("text").text(([, n]) => n._1).attr("x", (_, j) => w * (j + 0.5)).attr("y", 0.5 * h).attr("class", "matrix-cell-text").attr("text-anchor", "middle").attr("dominant-baseline", "middle").attr("pointer-events", "none");
      svg.append("text").text(title2).attr("x", hMargin / 2).attr("y", vMargin / 2).attr("class", "title-text").attr("dominant-baseline", "middle").attr("text-anchor", "left");
      svg.selectAll("rect").on("mousedown", (e, d) => {
        console.log(`mousedown ${d[0]}`);
        listener(e);
      });
    };
  }
  var drawMatrix = curry43(drawMatrix_);

  // output-es/App.MatrixView/index.js
  var match4 = /* @__PURE__ */ (() => $$int.match(annBoolean))();
  var matrixViewHandler = (ev) => {
    const xy = definitely("index within bounds")(index2(definitely("absurd")(nullable(
      _target(ev),
      Nothing,
      Just
    )).__data__)(0));
    return toggleCell(definitely("index within bounds")(index2(xy)(0)))(definitely("index within bounds")(index2(xy)(1)));
  };
  var matrixRep2 = (v) => $Tuple(arrayMap(arrayMap((x2) => match4(x2)))(v._1), $Tuple(v._2._1._1, v._2._2._1));

  // output-es/App.TableView/foreign.js
  function curry44(f) {
    return (x1) => (x2) => (x3) => (x4) => f(x1, x2, x3, x4);
  }
  function colorShade3(col, amt) {
    col = col.replace(/^#/, "");
    if (col.length === 3)
      col = col[0] + col[0] + col[1] + col[1] + col[2] + col[2];
    let [r, g, b] = col.match(/.{2}/g);
    [r, g, b] = [parseInt(r, 16) + amt, parseInt(g, 16) + amt, parseInt(b, 16) + amt];
    r = Math.max(Math.min(255, r), 0).toString(16);
    g = Math.max(Math.min(255, g), 0).toString(16);
    b = Math.max(Math.min(255, b), 0).toString(16);
    const rr = (r.length < 2 ? "0" : "") + r;
    const gg = (g.length < 2 ? "0" : "") + g;
    const bb = (b.length < 2 ? "0" : "") + b;
    return `#${rr}${gg}${bb}`;
  }
  function isUsed(r) {
    return Object.keys(r).some((k) => r[k]._2);
  }
  function drawTable_(id3, childIndex, {
    title: title2,
    table
  }, listener) {
    return () => {
      const childId = id3 + "-" + childIndex;
      const cellFill = "#ffffff";
      const div = select_default2("#" + id3);
      div.selectAll("#" + childId).remove();
      table = table.filter((r) => isUsed(r));
      if (table.length > 0) {
        const HTMLtable = div.append("table").attr("id", childId);
        const colNames = Object.keys(table[0]);
        HTMLtable.append("thead").append("tr").selectAll("th").data(colNames).enter().append("th").text((d) => d);
        const rows = HTMLtable.append("tbody").selectAll("tr").data(table).enter().append("tr");
        rows.selectAll("td").data((d) => colNames.map((k) => {
          return { "value": d[k], "name": k };
        })).enter().append("td").attr("data-th", (d) => d.name).attr("class", (d) => d.value._2 ? "cell-selected" : null).attr("bgcolor", (d) => d.value._2 ? colorShade3(cellFill, -40) : cellFill).text((d) => d.value._1).on(
          "mouseover",
          (e, d) => listener(e)
        );
      }
    };
  }
  var drawTable = curry44(drawTable_);

  // output-es/App.TableView/index.js
  var match5 = /* @__PURE__ */ (() => $$int.match(annBoolean))();
  var match12 = /* @__PURE__ */ (() => string.match(annBoolean))();
  var energyRecord = (r) => ({ year: match5($$get("year")(r)), country: match12($$get("country")(r)), energyType: match12($$get("energyType")(r)), output: get_intOrNumber("output")(r) });

  // output-es/Desugarable/index.js
  var desugFwd$p = (dictJoinSemilattice) => (dictDesugarable) => {
    const fromSug1 = dictDesugarable.FromSugar2().fromSug;
    const erase3 = dictDesugarable.Functor0().map((v) => unit);
    const desugFwd1 = dictDesugarable.desugFwd(dictJoinSemilattice);
    return (s) => {
      const $6 = fromSug1((() => {
        const $62 = erase3(s);
        return (k) => k(dictDesugarable)($62);
      })());
      const $7 = desugFwd1(s);
      if ($7.tag === "Left") {
        return $Either("Left", $7._1);
      }
      if ($7.tag === "Right") {
        return $Either("Right", $6($7._1));
      }
      fail();
    };
  };
  var desugBwd$p = (dictBoundedJoinSemilattice) => (dictDesugarable) => {
    const toSug1 = dictDesugarable.FromSugar2().toSug;
    const desugBwd1 = dictDesugarable.desugBwd(dictBoundedJoinSemilattice);
    return (e) => {
      const v = toSug1(e);
      return desugBwd1(v._2)(v._1((dictDesugarable1) => unsafeCoerce));
    };
  };

  // output-es/Control.Monad.Error.Class/index.js
  var $$try = (dictMonadError) => {
    const Monad0 = dictMonadError.MonadThrow0().Monad0();
    const map5 = Monad0.Bind1().Apply0().Functor0().map;
    const pure2 = Monad0.Applicative0().pure;
    return (a) => dictMonadError.catchError(map5(Right)(a))((x2) => pure2($Either("Left", x2)));
  };

  // output-es/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error4) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error4) {
        setTimeout(function() {
          throw error4;
        }, 0);
      }
    }
    function runSync(left2, right2, eff) {
      try {
        return right2(eff());
      } catch (error4) {
        return left2(error4);
      }
    }
    function runAsync(left2, eff, k) {
      try {
        return eff(k)();
      } catch (error4) {
        k(left2(error4))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size3 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size3 !== 0) {
          size3--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i, tmp;
          if (size3 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size3) % limit] = cb;
          size3++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util2) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util2.isLeft(result) && util2.fromLeft(result)) {
                    setTimeout(function() {
                      throw util2.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error4) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util2, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step = aff;
      var fail4 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run2(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step = bhead(step);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail4 = util2.left(e);
                step = null;
              }
              break;
            case STEP_RESULT:
              if (util2.isLeft(step)) {
                status = RETURN;
                fail4 = step;
                step = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step = util2.fromRight(step);
              }
              break;
            case CONTINUE:
              switch (step.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step._2;
                  status = CONTINUE;
                  step = step._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step = util2.right(step._1);
                  } else {
                    status = STEP_BIND;
                    step = step._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step = runSync(util2.left, util2.right, step._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step = runAsync(util2.left, step._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step = result2;
                        run2(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail4 = util2.left(step._1);
                  step = null;
                  break;
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step = step._1;
                  break;
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step = step._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util2, supervisor, step._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step._1) {
                    tmp.run();
                  }
                  step = util2.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step = sequential(util2, supervisor, step._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step = interrupt || fail4 || step;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail4) {
                      status = CONTINUE;
                      step = attempt._2(util2.fromLeft(fail4));
                      fail4 = null;
                    }
                    break;
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail4) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step = util2.fromRight(step);
                    }
                    break;
                  case BRACKET:
                    bracketCount--;
                    if (fail4 === null) {
                      result = util2.fromRight(step);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step = attempt._3(result);
                      }
                    }
                    break;
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step, fail4), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step = attempt._1.killed(util2.fromLeft(interrupt))(attempt._2);
                    } else if (fail4) {
                      step = attempt._1.failed(util2.fromLeft(fail4))(attempt._2);
                    } else {
                      step = attempt._1.completed(util2.fromRight(step))(attempt._2);
                    }
                    fail4 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step, fail4), attempts, interrupt);
                    status = CONTINUE;
                    step = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step = attempt._1;
                    fail4 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step));
                }
              }
              joins = null;
              if (interrupt && fail4) {
                setTimeout(function() {
                  throw util2.fromLeft(fail4);
                }, 0);
              } else if (util2.isLeft(step) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util2.fromLeft(step);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join2) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join2.rethrow;
            join2.handler(step)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join2;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill(error4, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util2.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util2.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util2.left(error4);
              status = COMPLETED;
              step = interrupt;
              run2(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util2.left(error4);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step(error4)), attempts, interrupt);
                }
                status = RETURN;
                step = null;
                fail4 = null;
                run2(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util2.left(error4);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step = null;
                fail4 = null;
              }
          }
          return canceler;
        };
      }
      function join(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run2(runTick);
          }
          return canceler;
        };
      }
      return {
        kill,
        join,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run2(runTick);
              });
            } else {
              run2(runTick);
            }
          }
        }
      };
    }
    function runPar(util2, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root3 = EMPTY;
      function kill(error4, par2, cb2) {
        var step = par2;
        var head = null;
        var tail = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop:
          while (true) {
            tmp = null;
            switch (step.tag) {
              case FORKED:
                if (step._3 === EMPTY) {
                  tmp = fibers[step._1];
                  kills2[count++] = tmp.kill(error4, function(result) {
                    return function() {
                      count--;
                      if (count === 0) {
                        cb2(result)();
                      }
                    };
                  });
                }
                if (head === null) {
                  break loop;
                }
                step = head._2;
                if (tail === null) {
                  head = null;
                } else {
                  head = tail._1;
                  tail = tail._2;
                }
                break;
              case MAP:
                step = step._2;
                break;
              case APPLY:
              case ALT:
                if (head) {
                  tail = new Aff2(CONS, head, tail);
                }
                head = step;
                step = step._1;
                break;
            }
          }
        if (count === 0) {
          cb2(util2.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join(result, head, tail) {
        var fail4, step, lhs, rhs, tmp, kid;
        if (util2.isLeft(result)) {
          fail4 = result;
          step = null;
        } else {
          step = result;
          fail4 = null;
        }
        loop:
          while (true) {
            lhs = null;
            rhs = null;
            tmp = null;
            kid = null;
            if (interrupt !== null) {
              return;
            }
            if (head === null) {
              cb(fail4 || step)();
              return;
            }
            if (head._3 !== EMPTY) {
              return;
            }
            switch (head.tag) {
              case MAP:
                if (fail4 === null) {
                  head._3 = util2.right(head._1(util2.fromRight(step)));
                  step = head._3;
                } else {
                  head._3 = fail4;
                }
                break;
              case APPLY:
                lhs = head._1._3;
                rhs = head._2._3;
                if (fail4) {
                  head._3 = fail4;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill(early, fail4 === lhs ? head._2 : head._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail === null) {
                        join(fail4, null, null);
                      } else {
                        join(fail4, tail._1, tail._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                } else if (lhs === EMPTY || rhs === EMPTY) {
                  return;
                } else {
                  step = util2.right(util2.fromRight(lhs)(util2.fromRight(rhs)));
                  head._3 = step;
                }
                break;
              case ALT:
                lhs = head._1._3;
                rhs = head._2._3;
                if (lhs === EMPTY && util2.isLeft(rhs) || rhs === EMPTY && util2.isLeft(lhs)) {
                  return;
                }
                if (lhs !== EMPTY && util2.isLeft(lhs) && rhs !== EMPTY && util2.isLeft(rhs)) {
                  fail4 = step === lhs ? rhs : lhs;
                  step = null;
                  head._3 = fail4;
                } else {
                  head._3 = step;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill(early, step === lhs ? head._2 : head._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail === null) {
                        join(step, null, null);
                      } else {
                        join(step, tail._1, tail._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                }
                break;
            }
            if (tail === null) {
              head = null;
            } else {
              head = tail._1;
              tail = tail._2;
            }
          }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run2() {
        var status = CONTINUE;
        var step = par;
        var head = null;
        var tail = null;
        var tmp, fid;
        loop:
          while (true) {
            tmp = null;
            fid = null;
            switch (status) {
              case CONTINUE:
                switch (step.tag) {
                  case MAP:
                    if (head) {
                      tail = new Aff2(CONS, head, tail);
                    }
                    head = new Aff2(MAP, step._1, EMPTY, EMPTY);
                    step = step._2;
                    break;
                  case APPLY:
                    if (head) {
                      tail = new Aff2(CONS, head, tail);
                    }
                    head = new Aff2(APPLY, EMPTY, step._2, EMPTY);
                    step = step._1;
                    break;
                  case ALT:
                    if (head) {
                      tail = new Aff2(CONS, head, tail);
                    }
                    head = new Aff2(ALT, EMPTY, step._2, EMPTY);
                    step = step._1;
                    break;
                  default:
                    fid = fiberId++;
                    status = RETURN;
                    tmp = step;
                    step = new Aff2(FORKED, fid, new Aff2(CONS, head, tail), EMPTY);
                    tmp = Fiber(util2, supervisor, tmp);
                    tmp.onComplete({
                      rethrow: false,
                      handler: resolve(step)
                    })();
                    fibers[fid] = tmp;
                    if (supervisor) {
                      supervisor.register(tmp);
                    }
                }
                break;
              case RETURN:
                if (head === null) {
                  break loop;
                }
                if (head._1 === EMPTY) {
                  head._1 = step;
                  status = CONTINUE;
                  step = head._2;
                  head._2 = EMPTY;
                } else {
                  head._2 = step;
                  step = head;
                  if (tail === null) {
                    head = null;
                  } else {
                    head = tail._1;
                    tail = tail._2;
                  }
                }
            }
          }
        root3 = step;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error4, cb2) {
        interrupt = util2.left(error4);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill(error4, root3, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run2();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential(util2, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util2, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value) {
          return Aff.Pure(f(value));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  var _liftEffect = Aff.Sync;
  var makeAff = Aff.Async;
  function _makeFiber(util2, aff) {
    return function() {
      return Aff.Fiber(util2, null, aff);
    };
  }
  var _delay = function() {
    function setDelay(n, k) {
      if (n === 0 && typeof setImmediate !== "undefined") {
        return setImmediate(k);
      } else {
        return setTimeout(k, n);
      }
    }
    function clearDelay(n, t) {
      if (n === 0 && typeof clearImmediate !== "undefined") {
        return clearImmediate(t);
      } else {
        return clearTimeout(t);
      }
    }
    return function(right2, ms) {
      return Aff.Async(function(cb) {
        return function() {
          var timer2 = setDelay(ms, cb(right2()));
          return function() {
            return Aff.Sync(function() {
              return right2(clearDelay(ms, timer2));
            });
          };
        };
      });
    };
  }();
  var _sequential = Aff.Seq;

  // output-es/Effect.Aff/index.js
  var functorAff = { map: _map };
  var ffiUtil = {
    isLeft: (v) => {
      if (v.tag === "Left") {
        return true;
      }
      if (v.tag === "Right") {
        return false;
      }
      fail();
    },
    fromLeft: (v) => {
      if (v.tag === "Left") {
        return v._1;
      }
      if (v.tag === "Right") {
        return _crashWith("unsafeFromLeft: Right");
      }
      fail();
    },
    fromRight: (v) => {
      if (v.tag === "Right") {
        return v._1;
      }
      if (v.tag === "Left") {
        return _crashWith("unsafeFromRight: Left");
      }
      fail();
    },
    left: Left,
    right: Right
  };
  var monadAff = { Applicative0: () => applicativeAff, Bind1: () => bindAff };
  var bindAff = { bind: _bind, Apply0: () => applyAff };
  var applyAff = { apply: (f) => (a) => _bind(f)((f$p) => _bind(a)((a$p) => applicativeAff.pure(f$p(a$p)))), Functor0: () => functorAff };
  var applicativeAff = { pure: _pure, Apply0: () => applyAff };
  var monadThrowAff = { throwError: _throwError, Monad0: () => monadAff };
  var monadErrorAff = { catchError: _catchError, MonadThrow0: () => monadThrowAff };
  var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
  var runAff = (k) => (aff) => {
    const $2 = _makeFiber(ffiUtil, _bind($$try2(aff))((x2) => _liftEffect(k(x2))));
    return () => {
      const fiber = $2();
      fiber.run();
      return fiber;
    };
  };
  var nonCanceler = /* @__PURE__ */ (() => {
    const $0 = _pure(unit);
    return (v) => $0;
  })();

  // output-es/Effect.Console/foreign.js
  var log2 = function(s) {
    return function() {
      console.log(s);
    };
  };

  // output-es/Trace/index.js
  var $AppTrace = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
  var $ForeignTrace$p = (_1, _2) => ({ tag: "ForeignTrace'", _1, _2 });
  var $Match = (tag, _1, _2) => ({ tag, _1, _2 });
  var $Trace = (tag, _1, _2, _3, _4) => ({ tag, _1, _2, _3, _4 });
  var $VarDef2 = (_1, _2) => ({ tag: "VarDef", _1, _2 });
  var unions2 = /* @__PURE__ */ (() => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, v = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = unionWith(ordString)($$const)(b)(v._1);
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      ;
      return go$r;
    };
    return go(Leaf2);
  })();
  var unions12 = /* @__PURE__ */ fold((z) => (v) => union2(ordString)(z))(Leaf2);
  var Const = /* @__PURE__ */ $Trace("Const");
  var bVMatch = {
    bv: (v) => {
      if (v.tag === "MatchVar") {
        return $Map("Two", Leaf2, v._1, unit, Leaf2);
      }
      if (v.tag === "MatchVarAnon") {
        return Leaf2;
      }
      if (v.tag === "MatchConstr") {
        return unions2(listMap(bVMatch.bv)(v._2));
      }
      if (v.tag === "MatchRecord") {
        return unions12(_fmapObject(v._1, bVMatch.bv));
      }
      fail();
    }
  };

  // output-es/Eval/index.js
  var fromFoldable6 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)(unit)(m))(Leaf2))();
  var show2 = /* @__PURE__ */ (() => showSet(showString).show)();
  var toUnfoldable9 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
  var fromFoldable13 = /* @__PURE__ */ fromFoldable2(foldableList);
  var fv = /* @__PURE__ */ (() => fVDict(fVElim).fv)();
  var traverse = /* @__PURE__ */ (() => {
    const $0 = traversableWithIndexObject.traverseWithIndex(applicativeEither);
    return (x2) => $0((v) => x2);
  })();
  var traverse1 = /* @__PURE__ */ (() => traversableList.traverse(applicativeEither))();
  var traverse2 = /* @__PURE__ */ (() => traversablePair.traverse(applicativeEither))();
  var fromFoldable22 = /* @__PURE__ */ (() => fromFoldableImpl(foldableList.foldr))();
  var greaterThanOrEq1 = /* @__PURE__ */ (() => {
    const $0 = ordTuple(ordInt)(ordInt);
    return (a1) => (a2) => !($0.compare(a1)(a2).tag === "LT");
  })();
  var show3 = (v) => "(Tuple " + (showIntImpl(v._1) + (" " + (showIntImpl(v._2) + ")")));
  var sequence2 = /* @__PURE__ */ (() => traversableList.traverse(applicativeEither)(identity3))();
  var erase1 = /* @__PURE__ */ (() => functorElim.map((v) => unit))();
  var matchMany = (dictAnn) => {
    const BoundedMeetSemilattice1 = dictAnn.BoundedLattice1().BoundedMeetSemilattice1();
    const meet = BoundedMeetSemilattice1.MeetSemilattice0().meet;
    return (v) => (v1) => {
      if (v.tag === "Nil") {
        return $Either(
          "Right",
          $Tuple(empty, $Tuple(v1, $Tuple(BoundedMeetSemilattice1.top, Nil)))
        );
      }
      if (v.tag === "Cons") {
        if (v1.tag === "ContElim") {
          return bindEither.bind(match6(dictAnn)(v._1)(v1._1))((v3) => bindEither.bind(matchMany(dictAnn)(v._2)(v3._2._1))((v4) => $Either(
            "Right",
            $Tuple(
              unionWith2((v$1) => (v1$1) => unsafePerformEffect(throwException(error("not disjoint"))))(v3._1)(v4._1),
              $Tuple(v4._2._1, $Tuple(meet(v3._2._2._1)(v4._2._2._1), $List("Cons", v3._2._2._2, v4._2._2._2)))
            )
          )));
        }
        if (v1.tag === "ContExpr") {
          return $Either(
            "Left",
            showIntImpl((() => {
              const go = (go$a0$copy) => (go$a1$copy) => {
                let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
                while (go$c) {
                  const b = go$a0, v$1 = go$a1;
                  if (v$1.tag === "Nil") {
                    go$c = false;
                    go$r = b;
                    continue;
                  }
                  if (v$1.tag === "Cons") {
                    go$a0 = b + 1 | 0;
                    go$a1 = v$1._2;
                    continue;
                  }
                  fail();
                }
                ;
                return go$r;
              };
              return go(0)(v._2) + 1 | 0;
            })()) + " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"
          );
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      return unsafePerformEffect(throwException(error("absurd")));
    };
  };
  var match6 = (dictAnn) => {
    const BoundedMeetSemilattice1 = dictAnn.BoundedLattice1().BoundedMeetSemilattice1();
    const meet = BoundedMeetSemilattice1.MeetSemilattice0().meet;
    const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
    return (v) => (v1) => {
      if (v1.tag === "ElimVar") {
        if (v1._1 === "_") {
          return $Either(
            "Right",
            $Tuple(
              empty,
              $Tuple(v1._2, $Tuple(BoundedMeetSemilattice1.top, $Match("MatchVarAnon", functorVal.map((v$1) => unit)(v))))
            )
          );
        }
        return $Either(
          "Right",
          $Tuple(
            runST(bind_(newImpl)(poke2(v1._1)(v))),
            $Tuple(v1._2, $Tuple(BoundedMeetSemilattice1.top, $Match("MatchVar", v1._1, functorVal.map((v$1) => unit)(v))))
          )
        );
      }
      if (v1.tag === "ElimConstr") {
        if (v.tag === "Constr") {
          return bindEither.bind($$with("Pattern mismatch")(consistentWith($Map(
            "Two",
            Leaf2,
            v._2,
            unit,
            Leaf2
          ))(keys2(v1._1))))(() => bindEither.bind((() => {
            const $7 = "Incomplete patterns: no branch for " + showCtr(v._2);
            const $8 = _lookup(Nothing, Just, v._2, v1._1);
            if ($8.tag === "Nothing") {
              return $Either("Left", $7);
            }
            if ($8.tag === "Just") {
              return $Either("Right", $8._1);
            }
            fail();
          })())((\u03BA) => bindEither.bind(matchMany(dictAnn)(v._3)(\u03BA))((v2) => $Either(
            "Right",
            $Tuple(v2._1, $Tuple(v2._2._1, $Tuple(meet(v._1)(v2._2._2._1), $Match("MatchConstr", v._2, v2._2._2._2))))
          ))));
        }
        return bindEither.bind((() => {
          const v$1 = toUnfoldable6(keys2(v1._1));
          if (v$1.tag === "Cons") {
            return dataTypeForCtr.dataTypeFor(v$1._1);
          }
          fail();
        })())((d) => $Either("Left", "Pattern mismatch: found " + (prettyP2(v) + (", expected " + d._1))));
      }
      if (v1.tag === "ElimRecord") {
        if (v.tag === "Record") {
          return bindEither.bind((() => {
            const $6 = "Pattern mismatch: found " + (show2(keys2(v._2)) + (", expected " + show2(v1._1)));
            if (difference3(ordString)(v1._1)(fromFoldable6(keys2(v._2))).tag === "Leaf") {
              return $Either("Right", unit);
            }
            return $Either("Left", $6);
          })())(() => {
            const xs$p = toUnfoldable9(v1._1);
            return bindEither.bind(matchMany(dictAnn)(listMap((a) => $$get(a)(v._2))(xs$p))(v1._2))((v2) => $Either(
              "Right",
              $Tuple(
                v2._1,
                $Tuple(
                  v2._2._1,
                  $Tuple(meet(v._1)(v2._2._2._1), $Match("MatchRecord", fromFoldable13(zipWith(Tuple)(xs$p)(v2._2._2._2))))
                )
              )
            ));
          });
        }
        return $Either("Left", "Pattern mismatch: found " + (prettyP2(v) + (", expected " + show2(v1._1))));
      }
      if (v1.tag === "ElimSug") {
        return match6(dictAnn)(v)(v1._2);
      }
      fail();
    };
  };
  var closeDefs = (\u03B3) => (\u03C1) => (\u03B1) => _fmapObject(
    \u03C1,
    (\u03C3) => {
      const \u03C1$p = $$for(\u03C1)(\u03C3);
      return $Val("Fun", $Fun("Closure", \u03B1, restrict(\u03B3)(unionWith(ordString)($$const)(fv(\u03C1$p))(fVElim.fv(\u03C3))), \u03C1$p, \u03C3));
    }
  );
  var checkArity2 = (c) => (n) => bindEither.bind(arity(c))((n$p) => {
    const $3 = showCtr(c) + (" got " + (showIntImpl(n) + (" argument(s), expects at most " + showIntImpl(n$p))));
    if (n$p >= n) {
      return $Either("Right", unit);
    }
    return $Either("Left", $3);
  });
  var $$eval = (dictAnn) => {
    const meet = dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0().meet;
    const match13 = string.match(dictAnn);
    const match22 = intPair.match(dictAnn);
    const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
    const match32 = match6(dictAnn);
    return (v) => (v1) => (v2) => {
      if (v1.tag === "Var") {
        const $9 = lookup$p(v1._1)(v);
        if ($9.tag === "Left") {
          return $Either("Left", $9._1);
        }
        if ($9.tag === "Right") {
          return $Either("Right", $Tuple($Trace("Var", v1._1), $9._1));
        }
        fail();
      }
      if (v1.tag === "Op") {
        const $9 = lookup$p(v1._1)(v);
        if ($9.tag === "Left") {
          return $Either("Left", $9._1);
        }
        if ($9.tag === "Right") {
          return $Either("Right", $Tuple($Trace("Op", v1._1), $9._1));
        }
        fail();
      }
      if (v1.tag === "Int") {
        return $Either("Right", $Tuple(Const, $Val("Int", meet(v1._1)(v2), v1._2)));
      }
      if (v1.tag === "Float") {
        return $Either("Right", $Tuple(Const, $Val("Float", meet(v1._1)(v2), v1._2)));
      }
      if (v1.tag === "Str") {
        return $Either("Right", $Tuple(Const, $Val("Str", meet(v1._1)(v2), v1._2)));
      }
      if (v1.tag === "Record") {
        return bindEither.bind((() => {
          const $9 = traverse((() => {
            const $92 = $$eval(dictAnn)(v);
            return (a) => $92(a)(v2);
          })())(v1._2);
          if ($9.tag === "Left") {
            return $Either("Left", $9._1);
          }
          if ($9.tag === "Right") {
            return $Either("Right", $Tuple(_fmapObject($9._1, fst), _fmapObject($9._1, snd)));
          }
          fail();
        })())((v3) => $Either("Right", $Tuple($Trace("Record", v3._1), $Val("Record", meet(v1._1)(v2), v3._2))));
      }
      if (v1.tag === "Dictionary") {
        return bindEither.bind((() => {
          const $9 = traverse1(traverse2((() => {
            const $92 = $$eval(dictAnn)(v);
            return (a) => $92(a)(v2);
          })()))(v1._2);
          if ($9.tag === "Left") {
            return $Either("Left", $9._1);
          }
          if ($9.tag === "Right") {
            return $Either(
              "Right",
              (() => {
                const $10 = unzip(listMap(toTuple)($9._1));
                return $Tuple(unzip($10._1), unzip($10._2));
              })()
            );
          }
          fail();
        })())((v3) => {
          const v4 = unzip(listMap((u) => match13(u))(v3._1._2));
          const d = fromFoldable13(zipWith(Tuple)(v4._1)(zipWith(Tuple)(v4._2)(v3._2._2)));
          return $Either(
            "Right",
            $Tuple(
              $Trace(
                "Dictionary",
                zipWith(Tuple)(v4._1)(zipWith(Tuple)(v3._1._1)(v3._2._1)),
                _fmapObject(d, (x2) => functorVal.map((v$1) => unit)(x2._2))
              ),
              $Val("Dictionary", meet(v1._1)(v2), d)
            )
          );
        });
      }
      if (v1.tag === "Constr") {
        return bindEither.bind(checkArity2(v1._2)((() => {
          const go = (go$a0$copy) => (go$a1$copy) => {
            let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
            while (go$c) {
              const b = go$a0, v$1 = go$a1;
              if (v$1.tag === "Nil") {
                go$c = false;
                go$r = b;
                continue;
              }
              if (v$1.tag === "Cons") {
                go$a0 = b + 1 | 0;
                go$a1 = v$1._2;
                continue;
              }
              fail();
            }
            ;
            return go$r;
          };
          return go(0)(v1._3);
        })()))(() => bindEither.bind((() => {
          const $10 = traverse1((() => {
            const $102 = $$eval(dictAnn)(v);
            return (a) => $102(a)(v2);
          })())(v1._3);
          if ($10.tag === "Left") {
            return $Either("Left", $10._1);
          }
          if ($10.tag === "Right") {
            return $Either("Right", unzip($10._1));
          }
          fail();
        })())((v3) => $Either("Right", $Tuple($Trace("Constr", v1._2, v3._1), $Val("Constr", meet(v1._1)(v2), v1._2, v3._2)))));
      }
      if (v1.tag === "Matrix") {
        return bindEither.bind($$eval(dictAnn)(v)(v1._4)(v2))((v3) => {
          const v5 = match22(v3._2)._1;
          return bindEither.bind((() => {
            const $11 = "array must be at least (" + (show3($Tuple(1, 1)) + ("); got (" + (show3($Tuple(v5._1._1, v5._2._1)) + ")")));
            if (greaterThanOrEq1($Tuple(v5._1._1, v5._2._1))($Tuple(1, 1))) {
              return $Either("Right", unit);
            }
            return $Either("Left", $11);
          })())(() => bindEither.bind((() => {
            const $12 = listMap((x2) => {
              const $132 = unzip(x2);
              return $Tuple(fromFoldable22($132._1), fromFoldable22($132._2));
            });
            const $13 = sequence2(bindList.bind(range(1)(v5._1._1))((i) => $List(
              "Cons",
              sequence2(bindList.bind(range(1)(v5._2._1))((j) => $List(
                "Cons",
                $$eval(dictAnn)(unionWith2((v$1) => identity12)(v)(unionWith2((v$1) => (v1$1) => unsafePerformEffect(throwException(error("not disjoint"))))(runST(bind_(newImpl)(poke2(v1._3._1)($Val(
                  "Int",
                  v5._1._2,
                  i
                )))))(runST(bind_(newImpl)(poke2(v1._3._2)($Val("Int", v5._2._2, j)))))))(v1._2)(v2),
                Nil
              ))),
              Nil
            )));
            if ($13.tag === "Left") {
              return $Either("Left", $13._1);
            }
            if ($13.tag === "Right") {
              return $Either(
                "Right",
                (() => {
                  const $14 = unzip($12($13._1));
                  return $Tuple(fromFoldable22($14._1), fromFoldable22($14._2));
                })()
              );
            }
            fail();
          })())((v6) => $Either(
            "Right",
            $Tuple(
              $Trace("Matrix", v6._1, $Tuple(v1._3._1, v1._3._2), $Tuple(v5._1._1, v5._2._1), v3._1),
              $Val("Matrix", meet(v1._1)(v2), $Tuple(v6._2, $Tuple($Tuple(v5._1._1, v5._1._2), $Tuple(v5._2._1, v5._2._2))))
            )
          )));
        });
      }
      if (v1.tag === "Lambda") {
        return $Either(
          "Right",
          $Tuple(Const, $Val("Fun", $Fun("Closure", v2, restrict(v)(fVElim.fv(v1._1)), empty, v1._1)))
        );
      }
      if (v1.tag === "Project") {
        return bindEither.bind($$eval(dictAnn)(v)(v1._1)(v2))((v3) => {
          if (v3._2.tag === "Record") {
            const $10 = lookup$p(v1._2)(v3._2._2);
            if ($10.tag === "Left") {
              return $Either("Left", $10._1);
            }
            if ($10.tag === "Right") {
              return $Either("Right", $Tuple($Trace("Project", v3._1, v1._2), $10._1));
            }
            fail();
          }
          return $Either("Left", "Found " + (prettyP2(v3._2) + ", expected record"));
        });
      }
      if (v1.tag === "App") {
        return bindEither.bind($$eval(dictAnn)(v)(v1._1)(v2))((v3) => bindEither.bind($$eval(dictAnn)(v)(v1._2)(v2))((v5) => bindEither.bind(apply(dictAnn)($Tuple(
          v3._2,
          v5._2
        )))((v6) => $Either("Right", $Tuple($Trace("App", v3._1, v5._1, v6._1), v6._2)))));
      }
      if (v1.tag === "Let") {
        return bindEither.bind($$eval(dictAnn)(v)(v1._1._2)(v2))((v3) => bindEither.bind(match32(v3._2)(v1._1._1))((v5) => bindEither.bind($$eval(dictAnn)(unionWith2((v$1) => identity12)(v)(v5._1))(v1._2)(v5._2._2._1))((v6) => $Either(
          "Right",
          $Tuple($Trace("Let", $VarDef2(v5._2._2._2, v3._1), v6._1), v6._2)
        ))));
      }
      if (v1.tag === "LetRec") {
        return bindEither.bind($$eval(dictAnn)(unionWith2((v$1) => identity12)(v)(closeDefs(v)(v1._1)(v2)))(v1._2)(v2))((v3) => $Either(
          "Right",
          $Tuple($Trace("LetRec", _fmapObject(v1._1, erase1), v3._1), v3._2)
        ));
      }
      if (v1.tag === "Sugar") {
        return bindEither.bind($$eval(dictAnn)(v)(v1._2)(v2))((v3) => $Either("Right", $Tuple($Trace("Sugar", v1._1, v3._1), v3._2)));
      }
      fail();
    };
  };
  var apply = (dictAnn) => {
    const match13 = match6(dictAnn);
    const meet = dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0().meet;
    const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
    return (v) => {
      if (v._1.tag === "Fun") {
        if (v._1._1.tag === "Closure") {
          const \u03B32 = closeDefs(v._1._1._2)(v._1._1._3)(v._1._1._1);
          return bindEither.bind(match13(v._2)(v._1._1._4))((v2) => bindEither.bind($$eval(dictAnn)(unionWith2((v$1) => identity12)(unionWith2((v$1) => identity12)(v._1._1._2)(\u03B32))(v2._1))((() => {
            if (v2._2._1.tag === "ContExpr") {
              return v2._2._1._1;
            }
            return unsafePerformEffect(throwException(error("Expression expected")));
          })())(meet(v._1._1._1)(v2._2._2._1)))((v3) => $Either(
            "Right",
            $Tuple($AppTrace("AppClosure", fromFoldable6(keys2(v._1._1._3)), v2._2._2._2, v3._1), v3._2)
          )));
        }
        if (v._1._1.tag === "Foreign") {
          const vs$p = foldableList.foldr(Cons)($List("Cons", v._2, Nil))(v._1._1._2);
          return bindEither.bind(bindEither.bind((() => {
            if ((() => {
              const go = (go$a0$copy) => (go$a1$copy) => {
                let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
                while (go$c) {
                  const b = go$a0, v$1 = go$a1;
                  if (v$1.tag === "Nil") {
                    go$c = false;
                    go$r = b;
                    continue;
                  }
                  if (v$1.tag === "Cons") {
                    go$a0 = b + 1 | 0;
                    go$a1 = v$1._2;
                    continue;
                  }
                  fail();
                }
                ;
                return go$r;
              };
              return v._1._1._1._1.arity > go(0)(vs$p);
            })()) {
              return $Either("Right", $Tuple(Nothing, $Val("Fun", $Fun("Foreign", v._1._1._1, vs$p))));
            }
            const $6 = v._1._1._1._1.op(dictAnn)(vs$p);
            if ($6.tag === "Left") {
              return $Either("Left", $6._1);
            }
            if ($6.tag === "Right") {
              return $Either("Right", $Tuple($Maybe("Just", $6._1._1), $6._1._2));
            }
            fail();
          })())((v3) => $Either("Right", $Tuple($ForeignTrace$p($ForeignOp$p(v._1._1._1._1), v3._1), v3._2))))((v2) => $Either(
            "Right",
            $Tuple(
              $AppTrace(
                "AppForeign",
                (() => {
                  const go = (go$a0$copy) => (go$a1$copy) => {
                    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
                    while (go$c) {
                      const b = go$a0, v$1 = go$a1;
                      if (v$1.tag === "Nil") {
                        go$c = false;
                        go$r = b;
                        continue;
                      }
                      if (v$1.tag === "Cons") {
                        go$a0 = b + 1 | 0;
                        go$a1 = v$1._2;
                        continue;
                      }
                      fail();
                    }
                    ;
                    return go$r;
                  };
                  return go(0)(v._1._1._2) + 1 | 0;
                })(),
                v2._1
              ),
              v2._2
            )
          ));
        }
        if (v._1._1.tag === "PartialConstr") {
          const n = successful(arity(v._1._1._2));
          return bindEither.bind((() => {
            const $6 = "Too many arguments to " + showCtr(v._1._1._2);
            if ((() => {
              const go = (go$a0$copy) => (go$a1$copy) => {
                let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
                while (go$c) {
                  const b = go$a0, v$1 = go$a1;
                  if (v$1.tag === "Nil") {
                    go$c = false;
                    go$r = b;
                    continue;
                  }
                  if (v$1.tag === "Cons") {
                    go$a0 = b + 1 | 0;
                    go$a1 = v$1._2;
                    continue;
                  }
                  fail();
                }
                ;
                return go$r;
              };
              return go(0)(v._1._1._3) < n;
            })()) {
              return $Either("Right", unit);
            }
            return $Either("Left", $6);
          })())(() => $Either(
            "Right",
            $Tuple(
              $AppTrace("AppConstr", v._1._1._2),
              (() => {
                if ((() => {
                  const go = (go$a0$copy) => (go$a1$copy) => {
                    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
                    while (go$c) {
                      const b = go$a0, v$1 = go$a1;
                      if (v$1.tag === "Nil") {
                        go$c = false;
                        go$r = b;
                        continue;
                      }
                      if (v$1.tag === "Cons") {
                        go$a0 = b + 1 | 0;
                        go$a1 = v$1._2;
                        continue;
                      }
                      fail();
                    }
                    ;
                    return go$r;
                  };
                  return go(0)(v._1._1._3) < (n - 1 | 0);
                })()) {
                  return $Val(
                    "Fun",
                    $Fun(
                      "PartialConstr",
                      v._1._1._1,
                      v._1._1._2,
                      foldableList.foldr(Cons)($List("Cons", v._2, Nil))(v._1._1._3)
                    )
                  );
                }
                return $Val(
                  "Constr",
                  v._1._1._1,
                  v._1._1._2,
                  foldableList.foldr(Cons)($List("Cons", v._2, Nil))(v._1._1._3)
                );
              })()
            )
          ));
        }
        return $Either("Left", "Found " + (prettyP2(v._2) + ", expected function"));
      }
      return $Either("Left", "Found " + (prettyP2(v._2) + ", expected function"));
    };
  };
  var apply2 = (dictAnn) => {
    const apply1 = apply(dictAnn);
    return (v) => bindEither.bind(apply1($Tuple(v._1, v._2._1)))((v3) => bindEither.bind(apply1($Tuple(v3._2, v._2._2)))((v4) => $Either(
      "Right",
      $Tuple($Tuple(v3._1, v4._1), v4._2)
    )));
  };
  var eval_module = (dictAnn) => {
    const eval1 = $$eval(dictAnn);
    const match13 = match6(dictAnn);
    return (\u03B3) => {
      const go = (v) => (v1) => (v2) => {
        if (v1._1.tag === "Nil") {
          return $Either("Right", v);
        }
        if (v1._1.tag === "Cons") {
          if (v1._1._1.tag === "Left") {
            return bindEither.bind(eval1(unionWith2((v$1) => identity12)(\u03B3)(v))(v1._1._1._1._2)(v2))((v3) => bindEither.bind(match13(v3._2)(v1._1._1._1._1))((v5) => go(unionWith2((v$1) => identity12)(v)(v5._1))($Module(v1._1._2))(v5._2._2._1)));
          }
          if (v1._1._1.tag === "Right") {
            return go(unionWith2((v$1) => identity12)(v)(closeDefs(unionWith2((v$1) => identity12)(\u03B3)(v))(v1._1._1._1)(v2)))($Module(v1._1._2))(v2);
          }
          fail();
        }
        fail();
      };
      return go(empty);
    };
  };

  // output-es/EvalBwd/index.js
  var eq = /* @__PURE__ */ (() => eqMap(eqString)(eqUnit).eq)();
  var toUnfoldable10 = /* @__PURE__ */ toAscUnfoldable(unfoldableList);
  var fromFoldable7 = /* @__PURE__ */ fromFoldable2(foldableList);
  var fromFoldable14 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)(unit)(m))(Leaf2))();
  var foldl1 = /* @__PURE__ */ (() => foldable1NonEmpty(foldableList).foldl1)();
  var map4 = /* @__PURE__ */ (() => functorNonEmpty(functorList).map)();
  var matchManyBwd = (dictAnn) => (v) => (\u03BA) => (v1) => (v2) => {
    if (v2.tag === "Nil") {
      if (isEmpty(v)) {
        return $Tuple(Nil, \u03BA);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v2.tag === "Cons") {
      const v3 = disjointUnion_inv(bVMatch.bv(v2._1))(v);
      const v4 = matchBwd(dictAnn)(v3._1)(\u03BA)(v1)(v2._1);
      const v6 = matchManyBwd(dictAnn)(v3._2)($Cont("ContElim", v4._2))(v1)(v2._2);
      return $Tuple(foldableList.foldr(Cons)($List("Cons", v4._1, Nil))(v6._1), v6._2);
    }
    fail();
  };
  var matchBwd = (dictAnn) => {
    const $1 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
    const $2 = functorVal.map((v) => $1.bot);
    return (v) => (\u03BA) => (v1) => (v2) => {
      if (v2.tag === "MatchVar") {
        if (eq(keys2(v))($Map("Two", Leaf2, v2._1, unit, Leaf2))) {
          return $Tuple($$get(v2._1)(v), $Elim("ElimVar", v2._1, \u03BA));
        }
        return $Tuple($2(v2._2), $Elim("ElimVar", v2._1, \u03BA));
      }
      if (v2.tag === "MatchVarAnon") {
        if (isEmpty(v)) {
          return $Tuple($2(v2._1), $Elim("ElimVar", "_", \u03BA));
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v2.tag === "MatchConstr") {
        const v3 = matchManyBwd(dictAnn)(v)(\u03BA)(v1)(reverse(v2._2));
        return $Tuple(
          $Val("Constr", v1, v2._1, v3._1),
          $Elim("ElimConstr", runST(bind_(newImpl)(poke2(v2._1)(v3._2))))
        );
      }
      if (v2.tag === "MatchRecord") {
        const v3 = unzip(toUnfoldable10(v2._1));
        const v4 = matchManyBwd(dictAnn)(v)(\u03BA)(v1)(reverse(v3._2));
        return $Tuple(
          $Val("Record", v1, fromFoldable7(zipWith(Tuple)(v3._1)(v4._1))),
          $Elim("ElimRecord", fromFoldable14(keys2(v2._1)), v4._2)
        );
      }
      fail();
    };
  };
  var closeDefsBwd = (dictAnn) => {
    const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
    const JoinSemilattice0 = BoundedJoinSemilattice0.JoinSemilattice0();
    const join = joinSemilatticeDict(joinSemilatticeVal(JoinSemilattice0)).join;
    const join1 = joinSemilatticeDict(joinSemilatticeElim(JoinSemilattice0)).join;
    return (\u03B3) => {
      const v = foldableWithIndexObject.foldrWithIndex((f) => (v2) => (v1) => {
        const v22 = $$get(f)(\u03B3);
        if (v22.tag === "Fun") {
          if (v22._1.tag === "Closure") {
            return $Tuple(
              mutate(poke2(f)(v22._1._4))(v1._1),
              $Tuple(join(v1._2._1)(v22._1._2), $Tuple(join1(v1._2._2._1)(v22._1._3), JoinSemilattice0.join(v1._2._2._2)(v22._1._1)))
            );
          }
          return unsafePerformEffect(throwException(error("absurd")));
        }
        return unsafePerformEffect(throwException(error("absurd")));
      })($Tuple(empty, $Tuple(empty, $Tuple(empty, BoundedJoinSemilattice0.bot))))(\u03B3);
      return $Tuple(v._2._1, $Tuple(join1(v._2._2._1)(v._1), v._2._2._2));
    };
  };
  var evalBwd$p = (dictAnn) => {
    const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
    const JoinSemilattice0 = BoundedJoinSemilattice0.JoinSemilattice0();
    const join = joinSemilatticeDict(joinSemilatticeVal(JoinSemilattice0)).join;
    const expand = expandableDictDict(botOfUnit$x215Raw$x215(functorVal)(BoundedJoinSemilattice0))((() => {
      const $4 = expandableValRawVal(BoundedJoinSemilattice0);
      return { expand: (v) => (v1) => $Tuple(v._1, $4.expand(v._2)(v1._2)) };
    })()).expand;
    const join2 = joinSemilatticeExpr(JoinSemilattice0).join;
    const matchBwd1 = matchBwd(dictAnn);
    const closeDefsBwd1 = closeDefsBwd(dictAnn);
    return (v) => (v1) => {
      const $10 = (t, v2, x2) => {
        const v3 = evalBwd$p(dictAnn)($Val(
          "Record",
          BoundedJoinSemilattice0.bot,
          runST(bind_(newImpl)(poke2(x2)(v2)))
        ))(t);
        return { "\u03B3": v3["\u03B3"], e: $Expr("Project", v3.e, x2), "\u03B1": v3["\u03B1"] };
      };
      const $11 = (t1, t2, t3, v2) => {
        const v3 = applyBwd(dictAnn)($Tuple(t3, v2));
        const v4 = evalBwd$p(dictAnn)(v3._1)(t1);
        const v5 = evalBwd$p(dictAnn)(v3._2)(t2);
        return { "\u03B3": join(v4["\u03B3"])(v5["\u03B3"]), e: $Expr("App", v4.e, v5.e), "\u03B1": JoinSemilattice0.join(v4["\u03B1"])(v5["\u03B1"]) };
      };
      const $12 = (t1, t2, v2, w) => {
        const v3 = evalBwd$p(dictAnn)(v2)(t2);
        const v4 = append_inv(bVMatch.bv(w))(v3["\u03B3"]);
        const v5 = matchBwd1(v4._2)(ContNone)(v3["\u03B1"])(w);
        const v6 = evalBwd$p(dictAnn)(v5._1)(t1);
        return { "\u03B3": join(v4._1)(v6["\u03B3"]), e: $Expr("Let", $VarDef(v5._2, v6.e), v3.e), "\u03B1": JoinSemilattice0.join(v6["\u03B1"])(v3["\u03B1"]) };
      };
      const $13 = (t, v2, \u03C1) => {
        const v3 = evalBwd$p(dictAnn)(v2)(t);
        const v4 = append_inv(fromFoldable14(keys2(\u03C1)))(v3["\u03B3"]);
        const v5 = closeDefsBwd1(v4._2);
        return { "\u03B3": join(v4._1)(v5._1), e: $Expr("LetRec", v5._2._1, v3.e), "\u03B1": JoinSemilattice0.join(v3["\u03B1"])(v5._2._2) };
      };
      const $14 = (s, t, v2) => {
        const v3 = evalBwd$p(dictAnn)(v2)(t);
        return { "\u03B3": v3["\u03B3"], e: $Expr("Sugar", s, v3.e), "\u03B1": v3["\u03B1"] };
      };
      if (v1.tag === "Var") {
        return {
          "\u03B3": runST(bind_(newImpl)(poke2(v1._1)(v))),
          e: $Expr("Var", v1._1),
          "\u03B1": BoundedJoinSemilattice0.bot
        };
      }
      if (v1.tag === "Op") {
        return {
          "\u03B3": runST(bind_(newImpl)(poke2(v1._1)(v))),
          e: $Expr("Op", v1._1),
          "\u03B1": BoundedJoinSemilattice0.bot
        };
      }
      if (v1.tag === "Const") {
        if (v.tag === "Str") {
          return { "\u03B3": empty, e: $Expr("Str", v._1, v._2), "\u03B1": v._1 };
        }
        if (v.tag === "Int") {
          return { "\u03B3": empty, e: $Expr("Int", v._1, v._2), "\u03B1": v._1 };
        }
        if (v.tag === "Float") {
          return { "\u03B3": empty, e: $Expr("Float", v._1, v._2), "\u03B1": v._1 };
        }
        if (v.tag === "Fun") {
          if (v._1.tag === "Closure") {
            return { "\u03B3": v._1._2, e: $Expr("Lambda", v._1._4), "\u03B1": v._1._1 };
          }
          return unsafePerformEffect(throwException(error("absurd")));
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v.tag === "Record") {
        if (v1.tag === "Record") {
          const $15 = evalBwd$p(dictAnn);
          const x\u03B3e\u03B1s = _fmapObject(intersectionWith(Tuple)(v._2)(v1._1), (v$1) => $15(v$1._1)(v$1._2));
          return {
            "\u03B3": foldrArray(join)(empty)(values(_fmapObject(x\u03B3e\u03B1s, (v2) => v2["\u03B3"]))),
            e: $Expr("Record", v._1, _fmapObject(x\u03B3e\u03B1s, (v2) => v2.e)),
            "\u03B1": foldrArray(JoinSemilattice0.join)(v._1)(values(_fmapObject(x\u03B3e\u03B1s, (v2) => v2["\u03B1"])))
          };
        }
        if (v1.tag === "Project") {
          return $10(v1._1, v, v1._2);
        }
        if (v1.tag === "App") {
          return $11(v1._1, v1._2, v1._3, v);
        }
        if (v1.tag === "Let") {
          return $12(v1._1._2, v1._2, v, v1._1._1);
        }
        if (v1.tag === "LetRec") {
          return $13(v1._2, v, v1._1);
        }
        if (v1.tag === "Sugar") {
          return $14(v1._1, v1._2, v);
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v.tag === "Dictionary") {
        if (v1.tag === "Dictionary") {
          const s\u03B1vs$p = expand(v._2)(_fmapObject(v1._2, (v2) => $Tuple(unit, v2)));
          const \u03B3e\u03B1s = listMap((v2) => evalBwd$p(dictAnn)($Val("Str", $$get(v2._1)(s\u03B1vs$p)._1, v2._1))(v2._2._1))(v1._1);
          const \u03B3e\u03B1s$p = listMap((v2) => evalBwd$p(dictAnn)($$get(v2._1)(s\u03B1vs$p)._2)(v2._2._2))(v1._1);
          return {
            "\u03B3": foldableList.foldr(join)(empty)(foldableList.foldr(Cons)(listMap((v2) => v2["\u03B3"])(\u03B3e\u03B1s$p))(listMap((v2) => v2["\u03B3"])(\u03B3e\u03B1s))),
            e: $Expr(
              "Dictionary",
              v._1,
              listMap(fromTuple)(zipWith(Tuple)(listMap((v2) => v2.e)(\u03B3e\u03B1s))(listMap((v2) => v2.e)(\u03B3e\u03B1s$p)))
            ),
            "\u03B1": foldableList.foldr(JoinSemilattice0.join)(v._1)(foldableList.foldr(Cons)(listMap((v2) => v2["\u03B1"])(\u03B3e\u03B1s$p))(listMap((v2) => v2["\u03B1"])(\u03B3e\u03B1s)))
          };
        }
        if (v1.tag === "Project") {
          return $10(v1._1, v, v1._2);
        }
        if (v1.tag === "App") {
          return $11(v1._1, v1._2, v1._3, v);
        }
        if (v1.tag === "Let") {
          return $12(v1._1._2, v1._2, v, v1._1._1);
        }
        if (v1.tag === "LetRec") {
          return $13(v1._2, v, v1._1);
        }
        if (v1.tag === "Sugar") {
          return $14(v1._1, v1._2, v);
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v.tag === "Constr") {
        if (v1.tag === "Constr") {
          const v2 = foldableList.foldr((v22) => (v3) => {
            const v4 = evalBwd$p(dictAnn)(v22._1)(v22._2);
            return $Tuple(join(v3._1)(v4["\u03B3"]), $Tuple($List("Cons", v4.e, v3._2._1), JoinSemilattice0.join(v3._2._2)(v4["\u03B1"])));
          })($Tuple(empty, $Tuple(Nil, v._1)))(zipWith(Tuple)(v._3)(v1._2));
          return { "\u03B3": v2._1, e: $Expr("Constr", v._1, v1._1, v2._2._1), "\u03B1": v2._2._2 };
        }
        if (v1.tag === "Project") {
          return $10(v1._1, v, v1._2);
        }
        if (v1.tag === "App") {
          return $11(v1._1, v1._2, v1._3, v);
        }
        if (v1.tag === "Let") {
          return $12(v1._1._2, v1._2, v, v1._1._1);
        }
        if (v1.tag === "LetRec") {
          return $13(v1._2, v, v1._1);
        }
        if (v1.tag === "Sugar") {
          return $14(v1._1, v1._2, v);
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v.tag === "Matrix") {
        if (v1.tag === "Matrix") {
          const v3 = foldl1((v42) => (v5) => $Tuple(
            join(v42._1)(v5._1),
            $Tuple(
              join2(v42._2._1)(v5._2._1),
              $Tuple(
                JoinSemilattice0.join(v42._2._2._1)(v5._2._2._1),
                $Tuple(JoinSemilattice0.join(v42._2._2._2._1)(v5._2._2._2._1), JoinSemilattice0.join(v42._2._2._2._2)(v5._2._2._2._2))
              )
            )
          ))(map4((v32) => {
            const v42 = evalBwd$p(dictAnn)(definitely("index within bounds")(index2(definitely("index within bounds")(index2(v._2._1)(v32._1 - 1 | 0)))(v32._2 - 1 | 0)))(definitely("index within bounds")(index2(definitely("index within bounds")(index2(v1._1)(v32._1 - 1 | 0)))(v32._2 - 1 | 0)));
            const v5 = append_inv(unionWith(ordString)($$const)($Map(
              "Two",
              Leaf2,
              v1._2._1,
              unit,
              Leaf2
            ))($Map("Two", Leaf2, v1._2._2, unit, Leaf2)))(v42["\u03B3"]);
            const \u03B30 = unionWith2((v$1) => identity12)(unionWith2((v$1) => (v1$1) => unsafePerformEffect(throwException(error("not disjoint"))))(runST(bind_(newImpl)(poke2(v1._2._1)($Val(
              "Int",
              BoundedJoinSemilattice0.bot,
              v1._3._1
            )))))(runST(bind_(newImpl)(poke2(v1._2._2)($Val(
              "Int",
              BoundedJoinSemilattice0.bot,
              v1._3._2
            ))))))(v5._2);
            const $19 = $$get(v1._2._1)(\u03B30);
            const $20 = $$get(v1._2._1)(\u03B30);
            if ($19.tag === "Int") {
              if ($20.tag === "Int") {
                return $Tuple(v5._1, $Tuple(v42.e, $Tuple(v42["\u03B1"], $Tuple($19._1, $20._1))));
              }
              fail();
            }
            fail();
          })(nonEmpty(bindList.bind(range(1)(v1._3._1))((i) => bindList.bind(range(1)(v1._3._2))((j) => $List(
            "Cons",
            $Tuple(i, j),
            Nil
          ))))));
          const v4 = evalBwd$p(dictAnn)($Val(
            "Constr",
            BoundedJoinSemilattice0.bot,
            "Pair",
            $List(
              "Cons",
              $Val("Int", JoinSemilattice0.join(v3._2._2._2._1)(v._2._2._1._2), v1._3._1),
              $List("Cons", $Val("Int", JoinSemilattice0.join(v3._2._2._2._2)(v._2._2._2._2), v1._3._2), Nil)
            )
          ))(v1._4);
          return {
            "\u03B3": join(v3._1)(v4["\u03B3"]),
            e: $Expr("Matrix", v._1, v3._2._1, $Tuple(v1._2._1, v1._2._2), v4.e),
            "\u03B1": JoinSemilattice0.join(JoinSemilattice0.join(v._1)(v3._2._2._1))(v4["\u03B1"])
          };
        }
        if (v1.tag === "Project") {
          return $10(v1._1, v, v1._2);
        }
        if (v1.tag === "App") {
          return $11(v1._1, v1._2, v1._3, v);
        }
        if (v1.tag === "Let") {
          return $12(v1._1._2, v1._2, v, v1._1._1);
        }
        if (v1.tag === "LetRec") {
          return $13(v1._2, v, v1._1);
        }
        if (v1.tag === "Sugar") {
          return $14(v1._1, v1._2, v);
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v1.tag === "Project") {
        return $10(v1._1, v, v1._2);
      }
      if (v1.tag === "App") {
        return $11(v1._1, v1._2, v1._3, v);
      }
      if (v1.tag === "Let") {
        return $12(v1._1._2, v1._2, v, v1._1._1);
      }
      if (v1.tag === "LetRec") {
        return $13(v1._2, v, v1._1);
      }
      if (v1.tag === "Sugar") {
        return $14(v1._1, v1._2, v);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    };
  };
  var applyBwd = (dictAnn) => {
    const closeDefsBwd1 = closeDefsBwd(dictAnn);
    const matchBwd1 = matchBwd(dictAnn);
    const JoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0().JoinSemilattice0();
    const join1 = joinSemilatticeDict(joinSemilatticeVal(JoinSemilattice0)).join;
    return (v) => {
      if (v._1.tag === "AppClosure") {
        const v2 = evalBwd$p(dictAnn)(v._2)(v._1._3);
        const v3 = append_inv(bVMatch.bv(v._1._2))(v2["\u03B3"]);
        const v4 = append_inv(v._1._1)(v3._1);
        const v5 = closeDefsBwd1(v4._2);
        const v6 = matchBwd1(v3._2)($Cont("ContExpr", v2.e))(v2["\u03B1"])(v._1._2);
        return $Tuple($Val("Fun", $Fun("Closure", JoinSemilattice0.join(v2["\u03B1"])(v5._2._2), join1(v4._1)(v5._1), v5._2._1, v6._2)), v6._1);
      }
      if (v._1.tag === "AppForeign") {
        const $6 = definitely("absurd")(unsnoc((() => {
          if (v._1._2._1._1.arity > v._1._1) {
            if (v._2.tag === "Fun") {
              if (v._2._1.tag === "Foreign") {
                return v._2._1._2;
              }
              fail();
            }
            fail();
          }
          return v._1._2._1._1.op_bwd(dictAnn)($Tuple(definitely("absurd")(v._1._2._2), v._2));
        })()));
        return $Tuple($Val("Fun", $Fun("Foreign", $ForeignOp$p(v._1._2._1._1), $6.init)), $6.last);
      }
      if (v._1.tag === "AppConstr") {
        if (v._2.tag === "Constr") {
          if (v._2._2 === v._1._1) {
            const v33 = definitely("absurd")(unsnoc(v._2._3));
            return $Tuple($Val("Fun", $Fun("PartialConstr", v._2._1, v._1._1, v33.init)), v33.last);
          }
          const v32 = definitely("absurd")(unsnoc(unsafePerformEffect(throwException(error("absurd")))._1));
          return $Tuple(
            $Val("Fun", $Fun("PartialConstr", unsafePerformEffect(throwException(error("absurd")))._2, v._1._1, v32.init)),
            v32.last
          );
        }
        if (v._2.tag === "Fun") {
          if (v._2._1.tag === "PartialConstr") {
            if (v._2._1._2 === v._1._1) {
              const v34 = definitely("absurd")(unsnoc(v._2._1._3));
              return $Tuple($Val("Fun", $Fun("PartialConstr", v._2._1._1, v._1._1, v34.init)), v34.last);
            }
            const v33 = definitely("absurd")(unsnoc(unsafePerformEffect(throwException(error("absurd")))._1));
            return $Tuple(
              $Val("Fun", $Fun("PartialConstr", unsafePerformEffect(throwException(error("absurd")))._2, v._1._1, v33.init)),
              v33.last
            );
          }
          const v32 = definitely("absurd")(unsnoc(unsafePerformEffect(throwException(error("absurd")))._1));
          return $Tuple(
            $Val("Fun", $Fun("PartialConstr", unsafePerformEffect(throwException(error("absurd")))._2, v._1._1, v32.init)),
            v32.last
          );
        }
        const v3 = definitely("absurd")(unsnoc(unsafePerformEffect(throwException(error("absurd")))._1));
        return $Tuple(
          $Val("Fun", $Fun("PartialConstr", unsafePerformEffect(throwException(error("absurd")))._2, v._1._1, v3.init)),
          v3.last
        );
      }
      fail();
    };
  };
  var evalBwd = (dictAnn) => {
    const evalBwd$p1 = evalBwd$p(dictAnn);
    const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
    const expand = expandableDictDict({ botOf: functorVal.map((v) => BoundedJoinSemilattice0.bot) })(expandableValRawVal(BoundedJoinSemilattice0)).expand;
    const expand1 = expandableExprRawExpr(BoundedJoinSemilattice0).expand;
    return (\u03B3) => (e) => (v) => (t) => {
      const v1 = evalBwd$p1(v)(t);
      return { "\u03B3": expand(v1["\u03B3"])(\u03B3), e: expand1(v1.e)(e), "\u03B1": v1["\u03B1"] };
    };
  };
  var apply2Bwd = (dictAnn) => {
    const applyBwd1 = applyBwd(dictAnn);
    return (v) => {
      const v2 = applyBwd1($Tuple(v._1._2, v._2));
      const v3 = applyBwd1($Tuple(v._1._1, v2._1));
      return $Tuple(v3._1, $Tuple(v3._2, v2._2));
    };
  };

  // output-es/Affjax.RequestHeader/index.js
  var $RequestHeader = (tag, _1, _2) => ({ tag, _1, _2 });

  // output-es/Affjax.ResponseFormat/index.js
  var $ResponseFormat = (tag, _1) => ({ tag, _1 });
  var identity17 = (x2) => x2;

  // output-es/Affjax.ResponseHeader/index.js
  var $ResponseHeader = (_1, _2) => ({ tag: "ResponseHeader", _1, _2 });
  var ResponseHeader = (value0) => (value1) => $ResponseHeader(value0, value1);

  // output-es/Control.Monad.Except.Trans/index.js
  var bindExceptT = (dictMonad) => {
    const bind = dictMonad.Bind1().bind;
    const pure2 = dictMonad.Applicative0().pure;
    return {
      bind: (v) => (k) => bind(v)((v2) => {
        if (v2.tag === "Left") {
          return pure2($Either("Left", v2._1));
        }
        if (v2.tag === "Right") {
          return k(v2._1);
        }
        fail();
      }),
      Apply0: () => applyExceptT(dictMonad)
    };
  };
  var applyExceptT = (dictMonad) => {
    const $1 = dictMonad.Bind1().Apply0().Functor0();
    const functorExceptT1 = { map: (f) => $1.map(functorEither.map(f)) };
    return {
      apply: (() => {
        const bind = bindExceptT(dictMonad).bind;
        const pure2 = applicativeExceptT(dictMonad).pure;
        return (f) => (a) => bind(f)((f$p) => bind(a)((a$p) => pure2(f$p(a$p))));
      })(),
      Functor0: () => functorExceptT1
    };
  };
  var applicativeExceptT = (dictMonad) => ({
    pure: (() => {
      const $1 = dictMonad.Applicative0().pure;
      return (x2) => $1($Either("Right", x2));
    })(),
    Apply0: () => applyExceptT(dictMonad)
  });
  var monadThrowExceptT = (dictMonad) => {
    const monadExceptT1 = { Applicative0: () => applicativeExceptT(dictMonad), Bind1: () => bindExceptT(dictMonad) };
    return {
      throwError: (() => {
        const $2 = dictMonad.Applicative0().pure;
        return (x2) => $2($Either("Left", x2));
      })(),
      Monad0: () => monadExceptT1
    };
  };
  var altExceptT = (dictSemigroup) => (dictMonad) => {
    const Bind1 = dictMonad.Bind1();
    const pure2 = dictMonad.Applicative0().pure;
    const $4 = Bind1.Apply0().Functor0();
    const functorExceptT1 = { map: (f) => $4.map(functorEither.map(f)) };
    return {
      alt: (v) => (v1) => Bind1.bind(v)((rm) => {
        if (rm.tag === "Right") {
          return pure2($Either("Right", rm._1));
        }
        if (rm.tag === "Left") {
          return Bind1.bind(v1)((rn) => {
            if (rn.tag === "Right") {
              return pure2($Either("Right", rn._1));
            }
            if (rn.tag === "Left") {
              return pure2($Either("Left", dictSemigroup.append(rm._1)(rn._1)));
            }
            fail();
          });
        }
        fail();
      }),
      Functor0: () => functorExceptT1
    };
  };

  // output-es/Data.Argonaut.Core/foreign.js
  function id2(x2) {
    return x2;
  }
  function stringify(j) {
    return JSON.stringify(j);
  }

  // output-es/Data.Argonaut.Core/index.js
  var jsonEmptyObject = /* @__PURE__ */ id2(empty);

  // output-es/Data.Argonaut.Parser/foreign.js
  function _jsonParser(fail4, succ, s) {
    try {
      return succ(JSON.parse(s));
    } catch (e) {
      return fail4(e.message);
    }
  }

  // output-es/JSURI/foreign.js
  function toRFC3896(input) {
    return input.replace(/[!'()*]/g, function(c) {
      return "%" + c.charCodeAt(0).toString(16);
    });
  }
  var _encodeFormURLComponent = function encode(fail4, succeed, input) {
    try {
      return succeed(toRFC3896(encodeURIComponent(input)).replace(/%20/g, "+"));
    } catch (err) {
      return fail4(err);
    }
  };

  // output-es/Data.FormURLEncoded/index.js
  var traverse3 = /* @__PURE__ */ (() => traversableArray.traverse(applicativeMaybe))();
  var encode2 = /* @__PURE__ */ (() => {
    const $0 = functorMaybe.map(joinWith("&"));
    const $1 = traverse3((v) => {
      if (v._2.tag === "Nothing") {
        return _encodeFormURLComponent((v$1) => Nothing, Just, v._1);
      }
      if (v._2.tag === "Just") {
        return applyMaybe.apply((() => {
          const $2 = _encodeFormURLComponent((v$1) => Nothing, Just, v._1);
          if ($2.tag === "Just") {
            return $Maybe("Just", (val2) => $2._1 + ("=" + val2));
          }
          return Nothing;
        })())(_encodeFormURLComponent((v$1) => Nothing, Just, v._2._1));
      }
      fail();
    });
    return (x2) => $0($1(x2));
  })();

  // output-es/Data.HTTP.Method/index.js
  var $Method = (tag) => ({ tag });
  var GET = /* @__PURE__ */ $Method("GET");
  var print = (v2) => {
    if (v2.tag === "Left") {
      if (v2._1.tag === "OPTIONS") {
        return "OPTIONS";
      }
      if (v2._1.tag === "GET") {
        return "GET";
      }
      if (v2._1.tag === "HEAD") {
        return "HEAD";
      }
      if (v2._1.tag === "POST") {
        return "POST";
      }
      if (v2._1.tag === "PUT") {
        return "PUT";
      }
      if (v2._1.tag === "DELETE") {
        return "DELETE";
      }
      if (v2._1.tag === "TRACE") {
        return "TRACE";
      }
      if (v2._1.tag === "CONNECT") {
        return "CONNECT";
      }
      if (v2._1.tag === "PROPFIND") {
        return "PROPFIND";
      }
      if (v2._1.tag === "PROPPATCH") {
        return "PROPPATCH";
      }
      if (v2._1.tag === "MKCOL") {
        return "MKCOL";
      }
      if (v2._1.tag === "COPY") {
        return "COPY";
      }
      if (v2._1.tag === "MOVE") {
        return "MOVE";
      }
      if (v2._1.tag === "LOCK") {
        return "LOCK";
      }
      if (v2._1.tag === "UNLOCK") {
        return "UNLOCK";
      }
      if (v2._1.tag === "PATCH") {
        return "PATCH";
      }
      fail();
    }
    if (v2.tag === "Right") {
      return v2._1;
    }
    fail();
  };

  // output-es/Effect.Aff.Compat/index.js
  var fromEffectFnAff = (v) => makeAff((k) => () => {
    const v1 = v((x2) => k($Either("Left", x2))(), (x2) => k($Either("Right", x2))());
    return (e) => makeAff((k2) => () => {
      v1(e, (x2) => k2($Either("Left", x2))(), (x2) => k2($Either("Right", x2))());
      return nonCanceler;
    });
  });

  // output-es/Foreign/foreign.js
  function tagOf(value) {
    return Object.prototype.toString.call(value).slice(8, -1);
  }
  var isArray = Array.isArray || function(value) {
    return Object.prototype.toString.call(value) === "[object Array]";
  };

  // output-es/Foreign/index.js
  var $ForeignError = (tag, _1, _2) => ({ tag, _1, _2 });
  var renderForeignError = (v) => {
    if (v.tag === "ForeignError") {
      return v._1;
    }
    if (v.tag === "ErrorAtIndex") {
      return "Error at array index " + (showIntImpl(v._1) + (": " + renderForeignError(v._2)));
    }
    if (v.tag === "ErrorAtProperty") {
      return "Error at property " + (showStringImpl(v._1) + (": " + renderForeignError(v._2)));
    }
    if (v.tag === "TypeMismatch") {
      return "Type mismatch: expected " + (v._1 + (", found " + v._2));
    }
    fail();
  };
  var unsafeReadTagged = (dictMonad) => {
    const pure1 = applicativeExceptT(dictMonad).pure;
    const $2 = monadThrowExceptT(dictMonad).throwError;
    return (tag) => (value) => {
      if (tagOf(value) === tag) {
        return pure1(value);
      }
      return $2($NonEmpty($ForeignError("TypeMismatch", tag, tagOf(value)), Nil));
    };
  };

  // output-es/Affjax/foreign.js
  function _ajax(platformSpecificDriver, timeoutErrorMessageIdent, requestFailedMessageIdent, mkHeader, options) {
    return function(errback, callback) {
      var xhr = platformSpecificDriver.newXHR();
      var fixedUrl = platformSpecificDriver.fixupUrl(options.url, xhr);
      xhr.open(options.method || "GET", fixedUrl, true, options.username, options.password);
      if (options.headers) {
        try {
          for (var i = 0, header; (header = options.headers[i]) != null; i++) {
            xhr.setRequestHeader(header.field, header.value);
          }
        } catch (e) {
          errback(e);
        }
      }
      var onerror = function(msgIdent) {
        return function() {
          errback(new Error(msgIdent));
        };
      };
      xhr.onerror = onerror(requestFailedMessageIdent);
      xhr.ontimeout = onerror(timeoutErrorMessageIdent);
      xhr.onload = function() {
        callback({
          status: xhr.status,
          statusText: xhr.statusText,
          headers: xhr.getAllResponseHeaders().split("\r\n").filter(function(header2) {
            return header2.length > 0;
          }).map(function(header2) {
            var i2 = header2.indexOf(":");
            return mkHeader(header2.substring(0, i2))(header2.substring(i2 + 2));
          }),
          body: xhr.response
        });
      };
      xhr.responseType = options.responseType;
      xhr.withCredentials = options.withCredentials;
      xhr.timeout = options.timeout;
      xhr.send(options.content);
      return function(error4, cancelErrback, cancelCallback) {
        try {
          xhr.abort();
        } catch (e) {
          return cancelErrback(e);
        }
        return cancelCallback();
      };
    };
  }

  // output-es/Affjax/index.js
  var $Error = (tag, _1, _2) => ({ tag, _1, _2 });
  var pure = /* @__PURE__ */ (() => applicativeExceptT(monadIdentity).pure)();
  var fail2 = /* @__PURE__ */ (() => {
    const $0 = monadThrowExceptT(monadIdentity).throwError;
    return (x2) => $0($NonEmpty(x2, Nil));
  })();
  var unsafeReadTagged2 = /* @__PURE__ */ unsafeReadTagged(monadIdentity);
  var alt = /* @__PURE__ */ (() => altExceptT(semigroupNonEmptyList)(monadIdentity).alt)();
  var any2 = /* @__PURE__ */ (() => foldableArray.foldMap((() => {
    const semigroupDisj1 = { append: (v) => (v1) => v || v1 };
    return { mempty: false, Semigroup0: () => semigroupDisj1 };
  })()))();
  var $$try3 = /* @__PURE__ */ $$try(monadErrorAff);
  var TimeoutError = /* @__PURE__ */ $Error("TimeoutError");
  var RequestFailedError = /* @__PURE__ */ $Error("RequestFailedError");
  var request = (driver2) => (req2) => {
    const fromResponse = (() => {
      if (req2.responseFormat.tag === "ArrayBuffer") {
        return unsafeReadTagged2("ArrayBuffer");
      }
      if (req2.responseFormat.tag === "Blob") {
        return unsafeReadTagged2("Blob");
      }
      if (req2.responseFormat.tag === "Document") {
        return (x2) => alt(unsafeReadTagged2("Document")(x2))(alt(unsafeReadTagged2("XMLDocument")(x2))(unsafeReadTagged2("HTMLDocument")(x2)));
      }
      if (req2.responseFormat.tag === "Json") {
        const $2 = bindExceptT(monadIdentity);
        const $3 = unsafeReadTagged2("String");
        return (a) => $2.bind($3(a))((x2) => req2.responseFormat._1((() => {
          if (x2 === "") {
            return pure(jsonEmptyObject);
          }
          const $6 = _jsonParser(Left, Right, x2);
          if ($6.tag === "Left") {
            return fail2($ForeignError("ForeignError", $6._1));
          }
          if ($6.tag === "Right") {
            return pure($6._1);
          }
          fail();
        })()));
      }
      if (req2.responseFormat.tag === "String") {
        return unsafeReadTagged2("String");
      }
      if (req2.responseFormat.tag === "Ignore") {
        const $2 = req2.responseFormat._1(pure(unit));
        return (v) => $2;
      }
      fail();
    })();
    const send = (content) => _map((v) => {
      if (v.tag === "Right") {
        const v1 = fromResponse(v._1.body);
        if (v1.tag === "Left") {
          return $Either("Left", $Error("ResponseBodyError", v1._1._1, v._1));
        }
        if (v1.tag === "Right") {
          return $Either("Right", { body: v1._1, headers: v._1.headers, status: v._1.status, statusText: v._1.statusText });
        }
        fail();
      }
      if (v.tag === "Left") {
        return $Either(
          "Left",
          (() => {
            const message2 = message(v._1);
            if (message2 === "AffjaxTimeoutErrorMessageIdent") {
              return TimeoutError;
            }
            if (message2 === "AffjaxRequestFailedMessageIdent") {
              return RequestFailedError;
            }
            return $Error("XHROtherError", v._1);
          })()
        );
      }
      fail();
    })($$try3(fromEffectFnAff(_ajax(
      driver2,
      "AffjaxTimeoutErrorMessageIdent",
      "AffjaxRequestFailedMessageIdent",
      ResponseHeader,
      {
        method: print(req2.method),
        url: req2.url,
        headers: arrayMap((h) => ({
          field: (() => {
            if (h.tag === "Accept") {
              return "Accept";
            }
            if (h.tag === "ContentType") {
              return "Content-Type";
            }
            if (h.tag === "RequestHeader") {
              return h._1;
            }
            fail();
          })(),
          value: (() => {
            if (h.tag === "Accept") {
              return h._1;
            }
            if (h.tag === "ContentType") {
              return h._1;
            }
            if (h.tag === "RequestHeader") {
              return h._2;
            }
            fail();
          })()
        }))((() => {
          const $4 = (() => {
            if (req2.content.tag === "Just") {
              if (req2.content._1.tag === "FormURLEncoded") {
                return $Maybe("Just", "application/x-www-form-urlencoded");
              }
              if (req2.content._1.tag === "Json") {
                return $Maybe("Just", "application/json");
              }
              return Nothing;
            }
            if (req2.content.tag === "Nothing") {
              return Nothing;
            }
            fail();
          })();
          if ($4.tag === "Just") {
            if (req2.responseFormat.tag === "Json") {
              const $5 = (() => {
                if (!any2((y2) => {
                  if (y2.tag === "Accept") {
                    return true;
                  }
                  if (y2.tag === "ContentType") {
                    return false;
                  }
                  if (y2.tag === "RequestHeader") {
                    return "Accept" === y2._1;
                  }
                  fail();
                })(req2.headers)) {
                  return run(withArray(pushAll([$RequestHeader("Accept", "application/json")]))(req2.headers));
                }
                return req2.headers;
              })();
              if (!any2((y2) => {
                if (y2.tag === "Accept") {
                  return false;
                }
                if (y2.tag === "ContentType") {
                  return true;
                }
                if (y2.tag === "RequestHeader") {
                  return "Content-Type" === y2._1;
                }
                fail();
              })($5)) {
                return run(withArray(pushAll([$RequestHeader("ContentType", $4._1)]))($5));
              }
              return $5;
            }
            if (!any2((y2) => {
              if (y2.tag === "Accept") {
                return false;
              }
              if (y2.tag === "ContentType") {
                return true;
              }
              if (y2.tag === "RequestHeader") {
                return "Content-Type" === y2._1;
              }
              fail();
            })(req2.headers)) {
              return run(withArray(pushAll([$RequestHeader("ContentType", $4._1)]))(req2.headers));
            }
            return req2.headers;
          }
          if (req2.responseFormat.tag === "Json") {
            if (!any2((y2) => {
              if (y2.tag === "Accept") {
                return true;
              }
              if (y2.tag === "ContentType") {
                return false;
              }
              if (y2.tag === "RequestHeader") {
                return "Accept" === y2._1;
              }
              fail();
            })(req2.headers)) {
              return run(withArray(pushAll([$RequestHeader("Accept", "application/json")]))(req2.headers));
            }
            return req2.headers;
          }
          return req2.headers;
        })()),
        content,
        responseType: (() => {
          if (req2.responseFormat.tag === "ArrayBuffer") {
            return "arraybuffer";
          }
          if (req2.responseFormat.tag === "Blob") {
            return "blob";
          }
          if (req2.responseFormat.tag === "Document") {
            return "document";
          }
          if (req2.responseFormat.tag === "Json") {
            return "text";
          }
          if (req2.responseFormat.tag === "String") {
            return "text";
          }
          if (req2.responseFormat.tag === "Ignore") {
            return "";
          }
          fail();
        })(),
        username: (() => {
          if (req2.username.tag === "Nothing") {
            return nullImpl;
          }
          if (req2.username.tag === "Just") {
            return notNull(req2.username._1);
          }
          fail();
        })(),
        password: (() => {
          if (req2.password.tag === "Nothing") {
            return nullImpl;
          }
          if (req2.password.tag === "Just") {
            return notNull(req2.password._1);
          }
          fail();
        })(),
        withCredentials: req2.withCredentials,
        timeout: (() => {
          if (req2.timeout.tag === "Just") {
            return req2.timeout._1;
          }
          return 0;
        })()
      }
    ))));
    if (req2.content.tag === "Nothing") {
      return send(nullImpl);
    }
    if (req2.content.tag === "Just") {
      if (req2.content._1.tag === "ArrayView") {
        return send(notNull(req2.content._1._1(unsafeCoerce)));
      }
      if (req2.content._1.tag === "Blob") {
        return send(notNull(req2.content._1._1));
      }
      if (req2.content._1.tag === "Document") {
        return send(notNull(req2.content._1._1));
      }
      if (req2.content._1.tag === "String") {
        return send(notNull(req2.content._1._1));
      }
      if (req2.content._1.tag === "FormData") {
        return send(notNull(req2.content._1._1));
      }
      if (req2.content._1.tag === "FormURLEncoded") {
        const $4 = encode2(req2.content._1._1);
        if ($4.tag === "Just") {
          return send(notNull($4._1));
        }
        return _pure($Either("Left", $Error("RequestContentError", "Body contains values that cannot be encoded as application/x-www-form-urlencoded")));
      }
      if (req2.content._1.tag === "Json") {
        return send(notNull(stringify(req2.content._1._1)));
      }
      fail();
    }
    fail();
  };
  var printError = (v) => {
    if (v.tag === "RequestContentError") {
      return "There was a problem with the request content: " + v._1;
    }
    if (v.tag === "ResponseBodyError") {
      return "There was a problem with the response body: " + renderForeignError(v._1);
    }
    if (v.tag === "TimeoutError") {
      return "There was a problem making the request: timeout";
    }
    if (v.tag === "RequestFailedError") {
      return "There was a problem making the request: request failed";
    }
    if (v.tag === "XHROtherError") {
      return "There was a problem making the request: " + message(v._1);
    }
    fail();
  };

  // output-es/Affjax.Web/foreign.js
  var driver = {
    newXHR: function() {
      return new XMLHttpRequest();
    },
    fixupUrl: function(url) {
      return url || "/";
    }
  };

  // output-es/Data.CodePoint.Unicode/index.js
  var isUpper = (x2) => checkAttr([512, 524288])(x2);
  var isSpace = (c) => {
    if (c <= 823) {
      return c === 32 || (c >= 9 && c <= 13 || c === 160);
    }
    return checkAttrS([2])(c);
  };
  var isOctDigit = (c) => {
    const diff = c - 48 | 0;
    return diff <= 7 && diff >= 0;
  };
  var isDecDigit = (c) => {
    const diff = c - 48 | 0;
    return diff <= 9 && diff >= 0;
  };
  var isHexDigit = (c) => {
    const diff = c - 48 | 0;
    const diff$1 = c - 65 | 0;
    const diff$2 = c - 97 | 0;
    return diff <= 9 && diff >= 0 || (diff$1 <= 5 && diff$1 >= 0 || diff$2 <= 5 && diff$2 >= 0);
  };
  var isAlphaNum = (x2) => checkAttr([524288, 512, 4096, 1048576, 16384, 8388608, 4194304, 2097152, 131072, 256, 16777216])(x2);
  var isAlpha = (x2) => checkAttr([4096, 512, 524288, 1048576, 16384])(x2);
  var hexDigitToInt = (c) => {
    const hexUpper = c - 65 | 0;
    const hexLower = c - 97 | 0;
    const dec = c - 48 | 0;
    if (dec <= 9 && dec >= 0) {
      return $Maybe("Just", dec);
    }
    if (hexLower <= 5 && hexLower >= 0) {
      return $Maybe("Just", hexLower + 10 | 0);
    }
    if (hexUpper <= 5 && hexUpper >= 0) {
      return $Maybe("Just", hexUpper + 10 | 0);
    }
    return Nothing;
  };

  // output-es/Data.Show.Generic/foreign.js
  var intercalate4 = function(separator) {
    return function(xs) {
      return xs.join(separator);
    };
  };

  // output-es/Data.Show.Generic/index.js
  var genericShowConstructor = (dictGenericShowArgs) => (dictIsSymbol) => ({
    "genericShow'": (v) => {
      const ctor = dictIsSymbol.reflectSymbol($$Proxy);
      const v1 = dictGenericShowArgs.genericShowArgs(v);
      if (v1.length === 0) {
        return ctor;
      }
      return "(" + (intercalate4(" ")(concatArray([ctor])(v1)) + ")");
    }
  });

  // output-es/Parsing/index.js
  var $ParseError = (_1, _2) => ({ tag: "ParseError", _1, _2 });
  var $ParseState = (_1, _2, _3) => ({ tag: "ParseState", _1, _2, _3 });
  var $RunParser = (tag, _1, _2) => ({ tag, _1, _2 });
  var More = (value0) => $RunParser("More", value0);
  var Lift = (value0) => $RunParser("Lift", value0);
  var lazyParserT = {
    defer: (f) => {
      const m = defer(f);
      return (state1, more, lift1, $$throw, done) => force(m)(state1, more, lift1, $$throw, done);
    }
  };
  var genericShow = /* @__PURE__ */ (() => {
    const $0 = genericShowConstructor({
      genericShowArgs: (v) => [
        (() => {
          const v$1 = cons(intercalate(": ")(["column", showIntImpl(v.column)]))(cons(intercalate(": ")([
            "index",
            showIntImpl(v.index)
          ]))(cons(intercalate(": ")(["line", showIntImpl(v.line)]))([])));
          if (v$1.length === 0) {
            return "{}";
          }
          return intercalate(" ")(["{", intercalate(", ")(v$1), "}"]);
        })()
      ]
    })({ reflectSymbol: () => "Position" });
    return (x2) => $0["genericShow'"](x2);
  })();
  var functorParserT = { map: (f) => (v) => (state1, more, lift1, $$throw, done) => more((v1) => v(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, f(a))))) };
  var applyParserT = {
    apply: (v) => (v1) => (state1, more, lift1, $$throw, done) => more((v2) => v(
      state1,
      more,
      lift1,
      $$throw,
      (state2, f) => more((v3) => v1(state2, more, lift1, $$throw, (state3, a) => more((v4) => done(state3, f(a)))))
    )),
    Functor0: () => functorParserT
  };
  var bindParserT = {
    bind: (v) => (next) => (state1, more, lift1, $$throw, done) => more((v1) => v(state1, more, lift1, $$throw, (state2, a) => more((v2) => next(a)(state2, more, lift1, $$throw, done)))),
    Apply0: () => applyParserT
  };
  var applicativeParserT = { pure: (a) => (state1, v, v1, v2, done) => done(state1, a), Apply0: () => applyParserT };
  var monadParserT = { Applicative0: () => applicativeParserT, Bind1: () => bindParserT };
  var monadRecParserT = {
    tailRecM: (next) => (initArg) => (state1, more, lift1, $$throw, done) => {
      const loop = (state2, arg, gas) => next(arg)(
        state2,
        more,
        lift1,
        $$throw,
        (state3, step) => {
          if (step.tag === "Loop") {
            if (gas === 0) {
              return more((v1) => loop(state3, step._1, 30));
            }
            return loop(state3, step._1, gas - 1 | 0);
          }
          if (step.tag === "Done") {
            return done(state3, step._1);
          }
          fail();
        }
      );
      return loop(state1, initArg, 30);
    },
    Monad0: () => monadParserT
  };
  var altParserT = {
    alt: (v) => (v1) => (v2, $3, $4, $5, $6) => $3((v3) => v(
      $ParseState(v2._1, v2._2, false),
      $3,
      $4,
      (v4, $9) => $3((v5) => {
        if (v4._3) {
          return $5(v4, $9);
        }
        return v1(v2, $3, $4, $5, $6);
      }),
      $6
    )),
    Functor0: () => functorParserT
  };
  var showParseError = { show: (v) => "(ParseError " + (showStringImpl(v._1) + (" " + (genericShow(v._2) + ")"))) };
  var runParserT$p = (dictMonadRec) => {
    const Monad0 = dictMonadRec.Monad0();
    const map5 = Monad0.Bind1().Apply0().Functor0().map;
    const pure1 = Monad0.Applicative0().pure;
    return (state1) => (v) => {
      const go = (go$a0$copy) => {
        let go$a0 = go$a0$copy, go$c = true, go$r;
        while (go$c) {
          const step = go$a0;
          const v1 = step(unit);
          if (v1.tag === "More") {
            go$a0 = v1._1;
            continue;
          }
          if (v1.tag === "Lift") {
            go$c = false;
            go$r = map5(Loop)(v1._1);
            continue;
          }
          if (v1.tag === "Stop") {
            go$c = false;
            go$r = pure1($Step("Done", $Tuple(v1._2, v1._1)));
            continue;
          }
          fail();
        }
        ;
        return go$r;
      };
      return dictMonadRec.tailRecM(go)((v1) => v(
        state1,
        More,
        Lift,
        (state2, err) => $RunParser("Stop", state2, $Either("Left", err)),
        (state2, res) => $RunParser("Stop", state2, $Either("Right", res))
      ));
    };
  };
  var position = (state1, v, v1, v2, done) => done(state1, state1._2);
  var initialPos = { index: 0, line: 1, column: 1 };
  var runParserT = (dictMonadRec) => {
    const map5 = dictMonadRec.Monad0().Bind1().Apply0().Functor0().map;
    const runParserT$p1 = runParserT$p(dictMonadRec);
    return (s) => (p) => map5(fst)(runParserT$p1($ParseState(s, initialPos, false))(p));
  };
  var runParserT1 = /* @__PURE__ */ runParserT(monadRecIdentity);
  var failWithPosition = (message2) => (pos) => (state1, v, v1, $$throw, v2) => $$throw(state1, $ParseError(message2, pos));
  var fail3 = (message2) => (state1, more, lift1, $$throw, done) => more((v1) => position(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2) => failWithPosition(message2)(a)(state2, more, lift1, $$throw, done))
  ));
  var plusParserT = { empty: /* @__PURE__ */ fail3("No alternative"), Alt0: () => altParserT };
  var alternativeParserT = { Applicative0: () => applicativeParserT, Plus1: () => plusParserT };

  // output-es/Parsing.Combinators/index.js
  var manyRec2 = /* @__PURE__ */ manyRec(monadRecParserT)(alternativeParserT);
  var withLazyErrorMessage = (p) => (msg) => altParserT.alt(p)(lazyParserT.defer((v) => fail3("Expected " + msg(unit))));
  var withErrorMessage = (p) => (msg) => altParserT.alt(p)(fail3("Expected " + msg));
  var $$try4 = (v) => (v1, $2, $3, $4, $5) => v(v1, $2, $3, (v2, $7) => $4($ParseState(v2._1, v2._2, v1._3), $7), $5);
  var skipMany1 = (p) => (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => p(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2$1) => more((v3) => {
      const loop = (state2$1, arg, gas) => altParserT.alt((state1$1, more$1, lift1$1, throw$1, done$1) => more$1((v1$1) => p(
        state1$1,
        more$1,
        lift1$1,
        throw$1,
        (state2$2, a$1) => more$1((v2$2) => done$1(state2$2, $Step("Loop", unit)))
      )))((state1$1, v, v1$1, v2$2, done$1) => done$1(state1$1, $Step("Done", unit)))(
        state2$1,
        more,
        lift1,
        $$throw,
        (state3, step) => {
          if (step.tag === "Loop") {
            if (gas === 0) {
              return more((v1$1) => loop(state3, step._1, 30));
            }
            return loop(state3, step._1, gas - 1 | 0);
          }
          if (step.tag === "Done") {
            return more((v4) => done(state3, step._1));
          }
          fail();
        }
      );
      return loop(state2, unit, 30);
    }))
  )));
  var skipMany = (p) => altParserT.alt(skipMany1(p))((state1, v, v1, v2, done) => done(state1, unit));
  var sepBy1 = (p) => (sep) => (state1, more, lift1, $$throw, done) => more((v1) => p(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2) => {
      const $11 = manyRec2((state1$1, more$1, lift1$1, throw$1, done$1) => more$1((v2$1) => more$1((v1$1) => sep(
        state1$1,
        more$1,
        lift1$1,
        throw$1,
        (state2$1, a$1) => more$1((v2$2) => more$1((v3) => p(state2$1, more$1, lift1$1, throw$1, (state3, a$2) => more$1((v4) => done$1(state3, a$2)))))
      ))));
      return more((v1$1) => $11(state2, more, lift1, $$throw, (state2$1, a$1) => more((v2$1) => done(state2$1, $NonEmpty(a, a$1)))));
    })
  ));
  var sepBy = (p) => (sep) => altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => sepBy1(p)(sep)(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2) => done(state2, $List("Cons", a._1, a._2)))
  )))((state1, v, v1, v2, done) => done(state1, Nil));
  var option = (a) => (p) => altParserT.alt(p)((state1, v, v1, v2, done) => done(state1, a));
  var notFollowedBy = (p) => $$try4(altParserT.alt((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $$try4(p)(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2$1) => more((v3) => fail3("Negated parser succeeded")(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
  ))))((state1, v, v1, v2, done) => done(state1, unit)));
  var choice = (dictFoldable) => {
    const $1 = dictFoldable.foldr((p1) => (v) => {
      if (v.tag === "Nothing") {
        return $Maybe("Just", p1);
      }
      if (v.tag === "Just") {
        return $Maybe("Just", altParserT.alt(p1)(v._1));
      }
      fail();
    })(Nothing);
    return (x2) => {
      const $3 = $1(x2);
      if ($3.tag === "Nothing") {
        return fail3("No alternative");
      }
      if ($3.tag === "Just") {
        return $3._1;
      }
      fail();
    };
  };
  var between2 = (open) => (close) => (p) => (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => more((v2$1) => more((v1$1) => open(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2$2) => more((v3) => p(
      state2,
      more,
      lift1,
      $$throw,
      (state3, a$1) => more((v4) => more((v2$3) => more((v3$1) => close(state3, more, lift1, $$throw, (state3$1, a$2) => more((v4$1) => done(state3$1, a$1))))))
    )))
  )))));
  var asErrorMessage = (b) => (a) => withErrorMessage(a)(b);

  // output-es/Parsing.Expr/index.js
  var $Assoc = (tag) => ({ tag });
  var $Operator = (tag, _1, _2) => ({ tag, _1, _2 });
  var choice2 = /* @__PURE__ */ choice(foldableList);
  var identity18 = (x2) => x2;
  var AssocNone = /* @__PURE__ */ $Assoc("AssocNone");
  var AssocLeft = /* @__PURE__ */ $Assoc("AssocLeft");
  var AssocRight = /* @__PURE__ */ $Assoc("AssocRight");
  var splitOp = (v) => (accum) => {
    if (v.tag === "Infix") {
      if (v._2.tag === "AssocNone") {
        return { rassoc: accum.rassoc, lassoc: accum.lassoc, nassoc: $List("Cons", v._1, accum.nassoc), prefix: accum.prefix, postfix: accum.postfix };
      }
      if (v._2.tag === "AssocLeft") {
        return { rassoc: accum.rassoc, lassoc: $List("Cons", v._1, accum.lassoc), nassoc: accum.nassoc, prefix: accum.prefix, postfix: accum.postfix };
      }
      if (v._2.tag === "AssocRight") {
        return { rassoc: $List("Cons", v._1, accum.rassoc), lassoc: accum.lassoc, nassoc: accum.nassoc, prefix: accum.prefix, postfix: accum.postfix };
      }
      fail();
    }
    if (v.tag === "Prefix") {
      return { rassoc: accum.rassoc, lassoc: accum.lassoc, nassoc: accum.nassoc, prefix: $List("Cons", v._1, accum.prefix), postfix: accum.postfix };
    }
    if (v.tag === "Postfix") {
      return { rassoc: accum.rassoc, lassoc: accum.lassoc, nassoc: accum.nassoc, prefix: accum.prefix, postfix: $List("Cons", v._1, accum.postfix) };
    }
    fail();
  };
  var rassocP1 = (x2) => (rassocOp) => (prefixP) => (term) => (postfixP) => altParserT.alt(rassocP(x2)(rassocOp)(prefixP)(term)(postfixP))((state1, v, v1, v2, done) => done(state1, x2));
  var rassocP = (x2) => (rassocOp) => (prefixP) => (term) => (postfixP) => (state1, more, lift1, $$throw, done) => more((v1) => rassocOp(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2) => more((v1$1) => more((v1$2) => more((v1$3) => prefixP(
      state2,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more((v2$1) => more((v1$4) => term(
        state2$1,
        more,
        lift1,
        $$throw,
        (state2$2, a$2) => more((v2$2) => more((v1$5) => postfixP(
          state2$2,
          more,
          lift1,
          $$throw,
          (state2$3, a$3) => more((v2$3) => {
            const $28 = a$3(a$1(a$2));
            return more((v2$4) => rassocP1($28)(rassocOp)(prefixP)(term)(postfixP)(state2$3, more, lift1, $$throw, (state2$4, a$4) => more((v2$5) => done(state2$4, a(x2)(a$4)))));
          })
        )))
      )))
    )))))
  ));
  var nassocP = (x2) => (nassocOp) => (prefixP) => (term) => (postfixP) => (state1, more, lift1, $$throw, done) => more((v1) => nassocOp(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2) => more((v1$1) => more((v1$2) => prefixP(
      state2,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more((v2$1) => more((v1$3) => term(
        state2$1,
        more,
        lift1,
        $$throw,
        (state2$2, a$2) => more((v2$2) => more((v1$4) => postfixP(
          state2$2,
          more,
          lift1,
          $$throw,
          (state2$3, a$3) => more((v2$3) => {
            const $27 = a$3(a$1(a$2));
            return more((v2$4) => done(state2$3, a(x2)($27)));
          })
        )))
      )))
    ))))
  ));
  var lassocP1 = (x2) => (lassocOp) => (prefixP) => (term) => (postfixP) => altParserT.alt(lassocP(x2)(lassocOp)(prefixP)(term)(postfixP))((state1, v, v1, v2, done) => done(state1, x2));
  var lassocP = (x2) => (lassocOp) => (prefixP) => (term) => (postfixP) => (state1, more, lift1, $$throw, done) => more((v1) => lassocOp(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2) => more((v1$1) => more((v1$2) => prefixP(
      state2,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more((v2$1) => more((v1$3) => term(
        state2$1,
        more,
        lift1,
        $$throw,
        (state2$2, a$2) => more((v2$2) => more((v1$4) => postfixP(
          state2$2,
          more,
          lift1,
          $$throw,
          (state2$3, a$3) => more((v2$3) => {
            const $27 = a$3(a$1(a$2));
            return more((v2$4) => lassocP1(a(x2)($27))(lassocOp)(prefixP)(term)(postfixP)(state2$3, more, lift1, $$throw, done));
          })
        )))
      )))
    ))))
  ));
  var makeParser = (term) => (ops) => {
    const accum = foldrArray(splitOp)({
      rassoc: Nil,
      lassoc: Nil,
      nassoc: Nil,
      prefix: Nil,
      postfix: Nil
    })(ops);
    const lassocOp = choice2(accum.lassoc);
    const nassocOp = choice2(accum.nassoc);
    const postfixOp = withErrorMessage(choice2(accum.postfix))("");
    const prefixOp = withErrorMessage(choice2(accum.prefix))("");
    const rassocOp = choice2(accum.rassoc);
    const $8 = altParserT.alt(prefixOp)((state1, v, v1, v2, done) => done(state1, identity18));
    const $9 = altParserT.alt(postfixOp)((state1, v, v1, v2, done) => done(state1, identity18));
    return (state1, more, lift1, $$throw, done) => more((v1) => more((v1$1) => $8(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => more((v1$2) => term(
        state2,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more((v2$1) => more((v1$3) => $9(
          state2$1,
          more,
          lift1,
          $$throw,
          (state2$2, a$2) => more((v2$2) => {
            const $28 = a$2(a(a$1));
            return more((v2$3) => altParserT.alt(rassocP($28)(rassocOp)(altParserT.alt(prefixOp)((state1$1, v, v1$4, v2$4, done$1) => done$1(state1$1, identity18)))(term)(altParserT.alt(postfixOp)((state1$1, v, v1$4, v2$4, done$1) => done$1(state1$1, identity18))))(altParserT.alt(lassocP($28)(lassocOp)(altParserT.alt(prefixOp)((state1$1, v, v1$4, v2$4, done$1) => done$1(
              state1$1,
              identity18
            )))(term)(altParserT.alt(postfixOp)((state1$1, v, v1$4, v2$4, done$1) => done$1(state1$1, identity18))))(altParserT.alt(nassocP($28)(nassocOp)(altParserT.alt(prefixOp)((state1$1, v, v1$4, v2$4, done$1) => done$1(state1$1, identity18)))(term)(altParserT.alt(postfixOp)((state1$1, v, v1$4, v2$4, done$1) => done$1(state1$1, identity18))))(withErrorMessage((state1$1, v, v1$4, v2$4, done$1) => done$1(state1$1, $28))("operator"))))(state2$2, more, lift1, $$throw, done));
          })
        )))
      )))
    )));
  };
  var buildExprParser = (operators2) => (simpleExpr) => foldlArray(makeParser)(simpleExpr)(operators2);

  // output-es/Parsing.String/index.js
  var updatePosSingle = (v) => (cp) => (after) => {
    if (cp === 10) {
      return { index: v.index + 1 | 0, line: v.line + 1 | 0, column: 1 };
    }
    if (cp === 13) {
      const v2 = codePointAt(0)(after);
      if (v2.tag === "Just") {
        if (v2._1 === 10) {
          return { index: v.index + 1 | 0, line: v.line, column: v.column };
        }
        return { index: v.index + 1 | 0, line: v.line + 1 | 0, column: 1 };
      }
      return { index: v.index + 1 | 0, line: v.line + 1 | 0, column: 1 };
    }
    if (cp === 9) {
      return { index: v.index + 1 | 0, line: v.line, column: (v.column + 8 | 0) - intMod(v.column - 1 | 0)(8) | 0 };
    }
    return { index: v.index + 1 | 0, line: v.line, column: v.column + 1 | 0 };
  };
  var updatePosString = (updatePosString$a0$copy) => (updatePosString$a1$copy) => (updatePosString$a2$copy) => {
    let updatePosString$a0 = updatePosString$a0$copy;
    let updatePosString$a1 = updatePosString$a1$copy;
    let updatePosString$a2 = updatePosString$a2$copy;
    let updatePosString$c = true;
    let updatePosString$r;
    while (updatePosString$c) {
      const pos = updatePosString$a0, before = updatePosString$a1, after = updatePosString$a2;
      const v = uncons3(before);
      if (v.tag === "Nothing") {
        updatePosString$c = false;
        updatePosString$r = pos;
        continue;
      }
      if (v.tag === "Just") {
        updatePosString$a0 = (() => {
          if (v._1.tail === "") {
            return updatePosSingle(pos)(v._1.head)(after);
          }
          return updatePosSingle(pos)(v._1.head)(v._1.tail);
        })();
        updatePosString$a1 = v._1.tail;
        updatePosString$a2 = after;
        continue;
      }
      fail();
    }
    ;
    return updatePosString$r;
  };
  var satisfyCodePoint = (f) => (v, $2, $3, $4, $5) => {
    const v3 = uncons3(v._1);
    if (v3.tag === "Nothing") {
      return $4(v, $ParseError("Unexpected EOF", v._2));
    }
    if (v3.tag === "Just") {
      if (f(v3._1.head)) {
        return $5($ParseState(v3._1.tail, updatePosSingle(v._2)(v3._1.head)(v3._1.tail), true), v3._1.head);
      }
      return $4(v, $ParseError("Predicate unsatisfied", v._2));
    }
    fail();
  };
  var satisfy = (f) => (v, $2, $3, $4, $5) => {
    const v3 = uncons3(v._1);
    if (v3.tag === "Nothing") {
      return $4(v, $ParseError("Unexpected EOF", v._2));
    }
    if (v3.tag === "Just") {
      if (v3._1.head < 0 || v3._1.head > 65535) {
        return $4(v, $ParseError("Expected Char", v._2));
      }
      const ch = (() => {
        if (v3._1.head >= -2147483648 && v3._1.head <= 2147483647) {
          return fromCharCode(v3._1.head);
        }
        fail();
      })();
      if (f(ch)) {
        return $5($ParseState(v3._1.tail, updatePosSingle(v._2)(v3._1.head)(v3._1.tail), true), ch);
      }
      return $4(v, $ParseError("Predicate unsatisfied", v._2));
    }
    fail();
  };
  var eof = (v, $1, $2, $3, $4) => {
    if (v._1 === "") {
      return $4($ParseState(v._1, v._2, true), unit);
    }
    return $3(v, $ParseError("Expected EOF", v._2));
  };
  var consumeWith = (f) => (v, $2, $3, $4, $5) => {
    const v3 = f(v._1);
    if (v3.tag === "Left") {
      return $4(v, $ParseError(v3._1, v._2));
    }
    if (v3.tag === "Right") {
      return $5($ParseState(v3._1.remainder, updatePosString(v._2)(v3._1.consumed)(v3._1.remainder), v3._1.consumed !== ""), v3._1.value);
    }
    fail();
  };
  var string2 = (str) => consumeWith((input) => {
    const v = stripPrefix(str)(input);
    if (v.tag === "Just") {
      return $Either("Right", { value: str, consumed: str, remainder: v._1 });
    }
    return $Either("Left", "Expected " + showStringImpl(str));
  });
  var $$char = (c) => withErrorMessage(satisfy((v) => v === c))(showCharImpl(c));

  // output-es/Parsing.String.Basic/index.js
  var show1 = /* @__PURE__ */ showArrayImpl(showCharImpl);
  var satisfyCP = (p) => satisfy((x2) => p(toCharCode(x2)));
  var oneOf = (ss) => withLazyErrorMessage(satisfy((a) => elem(eqChar)(a)(ss)))((v) => "one of " + show1(ss));
  var noneOf = (ss) => withLazyErrorMessage(satisfy((a) => notElem(eqChar)(a)(ss)))((v) => "none of " + show1(ss));

  // output-es/Data.String.Unicode/index.js
  var convert = (f) => {
    const $1 = arrayMap(f);
    return (x2) => fromCodePointArray($1(toCodePointArray(x2)));
  };
  var toLowerSimple = /* @__PURE__ */ convert(uTowlower);
  var toUpperSimple = /* @__PURE__ */ convert(uTowupper);

  // output-es/Parsing.Token/index.js
  var identity19 = (x2) => x2;
  var many3 = /* @__PURE__ */ many2(alternativeParserT)(lazyParserT);
  var some3 = /* @__PURE__ */ some2(alternativeParserT)(lazyParserT);
  var choice3 = /* @__PURE__ */ choice(foldableArray);
  var many1 = /* @__PURE__ */ many(alternativeParserT)(lazyParserT);
  var toUnfoldable11 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
  var theReservedNames = (v) => {
    if (v.caseSensitive) {
      return sortBy2(ordString.compare)(v.reservedNames);
    }
    return sortBy2(ordString.compare)(arrayMap(toLower)(v.reservedNames));
  };
  var oneLineComment = (v) => {
    const $1 = skipMany(satisfy((v1) => v1 !== "\n"));
    return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $$try4(string2(v.commentLine))(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => $1(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
    )));
  };
  var isReserved = (isReserved$a0$copy) => (isReserved$a1$copy) => {
    let isReserved$a0 = isReserved$a0$copy, isReserved$a1 = isReserved$a1$copy, isReserved$c = true, isReserved$r;
    while (isReserved$c) {
      const names = isReserved$a0, name2 = isReserved$a1;
      const v = uncons(names);
      if (v.tag === "Nothing") {
        isReserved$c = false;
        isReserved$r = false;
        continue;
      }
      if (v.tag === "Just") {
        const v1 = ordString.compare(v._1.head)(name2);
        if (v1.tag === "LT") {
          isReserved$a0 = v._1.tail;
          isReserved$a1 = name2;
          continue;
        }
        if (v1.tag === "EQ") {
          isReserved$c = false;
          isReserved$r = true;
          continue;
        }
        if (v1.tag === "GT") {
          isReserved$c = false;
          isReserved$r = false;
          continue;
        }
        fail();
      }
      fail();
    }
    ;
    return isReserved$r;
  };
  var inCommentSingle = (v) => {
    const startEnd = concatArray(toCharArray(v.commentEnd))(toCharArray(v.commentStart));
    const go$lazy = binding(() => lazyParserT.defer((v$1) => altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => $$try4(string2(v.commentEnd))(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => done(state2, unit))
    )))(altParserT.alt((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => skipMany1(noneOf(startEnd))(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => go$lazy()(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
    ))))(withErrorMessage((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => oneOf(startEnd)(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => go$lazy()(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
    ))))("end of comment")))));
    const go = go$lazy();
    return go;
  };
  var multiLineComment = (v) => {
    const $1 = inComment(v);
    return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $$try4(string2(v.commentStart))(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => $1(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
    )));
  };
  var inCommentMulti = (v) => {
    const startEnd = concatArray(toCharArray(v.commentEnd))(toCharArray(v.commentStart));
    const go$lazy = binding(() => lazyParserT.defer((v$1) => altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => $$try4(string2(v.commentEnd))(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => done(state2, unit))
    )))(altParserT.alt((() => {
      const $4 = multiLineComment(v);
      return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $4(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more((v2$1) => more((v3) => go$lazy()(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
      )));
    })())(altParserT.alt((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => skipMany1(noneOf(startEnd))(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => go$lazy()(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
    ))))(withErrorMessage((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => oneOf(startEnd)(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => go$lazy()(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
    ))))("end of comment"))))));
    const go = go$lazy();
    return go;
  };
  var inComment = (v) => {
    if (v.nestedComments) {
      return inCommentMulti(v);
    }
    return inCommentSingle(v);
  };
  var whiteSpace$p = (v) => {
    if (v.commentLine === "" && v.commentStart === "") {
      return skipMany(withErrorMessage(skipMany1(satisfyCodePoint(isSpace)))(""));
    }
    if (v.commentLine === "") {
      return skipMany(altParserT.alt(skipMany1(satisfyCodePoint(isSpace)))(withErrorMessage(multiLineComment(v))("")));
    }
    if (v.commentStart === "") {
      return skipMany(altParserT.alt(skipMany1(satisfyCodePoint(isSpace)))(withErrorMessage(oneLineComment(v))("")));
    }
    return skipMany(altParserT.alt(skipMany1(satisfyCodePoint(isSpace)))(altParserT.alt(oneLineComment(v))(withErrorMessage(multiLineComment(v))(""))));
  };
  var makeTokenParser = (v) => {
    const sign1 = altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => $$char("-")(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => done(state2, (a$1) => -a$1))
    )))(altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => $$char("+")(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => done(state2, identity19))
    )))((state1, v$1, v1, v2, done) => done(state1, identity19)));
    const $2 = some3(withErrorMessage(satisfyCP(isOctDigit))("oct digit"));
    const octal = (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => oneOf(["o", "O"])(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => more((v1$1) => $2(
        state2,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more((v2$2) => {
          const $18 = foldlArray((v1$2) => (v2$3) => {
            if (v1$2.tag === "Nothing") {
              return Nothing;
            }
            if (v1$2.tag === "Just") {
              const $20 = hexDigitToInt(toCharCode(v2$3));
              if ($20.tag === "Just") {
                return $Maybe("Just", (8 * v1$2._1 | 0) + $20._1 | 0);
              }
              return Nothing;
            }
            fail();
          })($Maybe("Just", 0))(a$1);
          if ($18.tag === "Nothing") {
            return fail3("not digits")(state2$1, more, lift1, $$throw, (state3, a$2) => more((v4) => done(state3, a$2)));
          }
          if ($18.tag === "Just") {
            return more((v4) => done(state2$1, $18._1));
          }
          fail();
        })
      ))))
    )));
    const $4 = whiteSpace$p(v);
    const semi = (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2(";")(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => $4(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ";"))))))
    ))));
    const $6 = some3(withErrorMessage(satisfyCP(isHexDigit))("hex digit"));
    const hexadecimal = (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => oneOf(["x", "X"])(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => more((v1$1) => $6(
        state2,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more((v2$2) => {
          const $22 = foldlArray((v1$2) => (v2$3) => {
            if (v1$2.tag === "Nothing") {
              return Nothing;
            }
            if (v1$2.tag === "Just") {
              const $24 = hexDigitToInt(toCharCode(v2$3));
              if ($24.tag === "Just") {
                return $Maybe("Just", (16 * v1$2._1 | 0) + $24._1 | 0);
              }
              return Nothing;
            }
            fail();
          })($Maybe("Just", 0))(a$1);
          if ($22.tag === "Nothing") {
            return fail3("not digits")(state2$1, more, lift1, $$throw, (state3, a$2) => more((v4) => done(state3, a$2)));
          }
          if ($22.tag === "Just") {
            return more((v4) => done(state2$1, $22._1));
          }
          fail();
        })
      ))))
    )));
    const fraction = asErrorMessage("fraction")((state1, more, lift1, $$throw, done) => more((v1) => $$char(".")(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => {
        const $172 = withErrorMessage(some3(withErrorMessage(satisfyCP(isDecDigit))("digit")))("fraction");
        return more((v1$1) => $172(
          state2,
          more,
          lift1,
          $$throw,
          (state2$1, a$1) => more((v2$1) => {
            const $22 = foldrArray((v1$2) => (v2$2) => {
              if (v2$2.tag === "Nothing") {
                return Nothing;
              }
              if (v2$2.tag === "Just") {
                const $24 = hexDigitToInt(toCharCode(v1$2));
                if ($24.tag === "Just") {
                  return $Maybe("Just", (v2$2._1 + toNumber($24._1)) / 10);
                }
                if ($24.tag === "Nothing") {
                  return Nothing;
                }
                fail();
              }
              fail();
            })($Maybe("Just", 0))(a$1);
            if ($22.tag === "Nothing") {
              return fail3("not digit")(state2$1, more, lift1, $$throw, done);
            }
            if ($22.tag === "Just") {
              return done(state2$1, $22._1);
            }
            fail();
          })
        ));
      })
    )));
    const escapeGap = withErrorMessage((() => {
      const $9 = some3(withErrorMessage(satisfyCP(isSpace))("space"));
      return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $9(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more((v2$1) => more((v3) => $$char("\\")(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
      )));
    })())("end of string gap");
    const $10 = some3(withErrorMessage(satisfyCP(isDecDigit))("digit"));
    const decimal = (state1, more, lift1, $$throw, done) => more((v1) => $10(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => {
        const $20 = foldlArray((v1$1) => (v2$1) => {
          if (v1$1.tag === "Nothing") {
            return Nothing;
          }
          if (v1$1.tag === "Just") {
            const $22 = hexDigitToInt(toCharCode(v2$1));
            if ($22.tag === "Just") {
              return $Maybe("Just", (10 * v1$1._1 | 0) + $22._1 | 0);
            }
            return Nothing;
          }
          fail();
        })($Maybe("Just", 0))(a);
        if ($20.tag === "Nothing") {
          return fail3("not digits")(state2, more, lift1, $$throw, done);
        }
        if ($20.tag === "Just") {
          return done(state2, $20._1);
        }
        fail();
      })
    ));
    const exponent$p = (() => {
      const power = (e) => {
        if (e < 0) {
          return 1 / power(-e);
        }
        return pow(10)(toNumber(e));
      };
      return asErrorMessage("exponent")((state1, more, lift1, $$throw, done) => more((v1) => oneOf(["e", "E"])(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more((v2) => more((v1$1) => sign1(
          state2,
          more,
          lift1,
          $$throw,
          (state2$1, a$1) => more((v2$1) => more((v1$2) => withErrorMessage(decimal)("exponent")(
            state2$1,
            more,
            lift1,
            $$throw,
            (state2$2, a$2) => more((v2$2) => done(state2$2, power(a$1(a$2))))
          )))
        )))
      )));
    })();
    const fractExponent = (n) => altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => fraction(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => more((v1$1) => option(1)(exponent$p)(
        state2,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more((v2$1) => done(state2$1, (toNumber(n) + a) * a$1))
      )))
    )))((state1, more, lift1, $$throw, done) => more((v1) => exponent$p(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, toNumber(n) * a)))));
    const decimalFloat = (state1, more, lift1, $$throw, done) => more((v1) => decimal(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => option($Either("Left", a))((() => {
        const $23 = fractExponent(a);
        return (state1$1, more$1, lift1$1, throw$1, done$1) => more$1((v1$1) => $23(
          state1$1,
          more$1,
          lift1$1,
          throw$1,
          (state2$1, a$1) => more$1((v2$1) => done$1(state2$1, $Either("Right", a$1)))
        ));
      })())(state2, more, lift1, $$throw, done))
    ));
    const $15 = whiteSpace$p(v);
    const comma2 = (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2(",")(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => $15(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ","))))))
    ))));
    const $17 = altParserT.alt(decimal)(altParserT.alt((() => {
      const $172 = some3(withErrorMessage(satisfyCP(isOctDigit))("oct digit"));
      return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $$char("o")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more((v2$1) => more((v3) => more((v1$1) => $172(
          state2,
          more,
          lift1,
          $$throw,
          (state2$1, a$1) => more((v2$2) => {
            const $33 = foldlArray((v1$2) => (v2$3) => {
              if (v1$2.tag === "Nothing") {
                return Nothing;
              }
              if (v1$2.tag === "Just") {
                const $35 = hexDigitToInt(toCharCode(v2$3));
                if ($35.tag === "Just") {
                  return $Maybe("Just", (8 * v1$2._1 | 0) + $35._1 | 0);
                }
                return Nothing;
              }
              fail();
            })($Maybe("Just", 0))(a$1);
            if ($33.tag === "Nothing") {
              return fail3("not digits")(state2$1, more, lift1, $$throw, (state3, a$2) => more((v4) => done(state3, a$2)));
            }
            if ($33.tag === "Just") {
              return more((v4) => done(state2$1, $33._1));
            }
            fail();
          })
        ))))
      )));
    })())((() => {
      const $172 = some3(withErrorMessage(satisfyCP(isHexDigit))("hex digit"));
      return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $$char("x")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more((v2$1) => more((v3) => more((v1$1) => $172(
          state2,
          more,
          lift1,
          $$throw,
          (state2$1, a$1) => more((v2$2) => {
            const $33 = foldlArray((v1$2) => (v2$3) => {
              if (v1$2.tag === "Nothing") {
                return Nothing;
              }
              if (v1$2.tag === "Just") {
                const $35 = hexDigitToInt(toCharCode(v2$3));
                if ($35.tag === "Just") {
                  return $Maybe("Just", (16 * v1$2._1 | 0) + $35._1 | 0);
                }
                return Nothing;
              }
              fail();
            })($Maybe("Just", 0))(a$1);
            if ($33.tag === "Nothing") {
              return fail3("not digits")(state2$1, more, lift1, $$throw, (state3, a$2) => more((v4) => done(state3, a$2)));
            }
            if ($33.tag === "Just") {
              return more((v4) => done(state2$1, $33._1));
            }
            fail();
          })
        ))))
      )));
    })()));
    const charNum = (state1, more, lift1, $$throw, done) => more((v1) => $17(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => {
        if (a > 1114111) {
          return fail3("invalid escape sequence")(state2, more, lift1, $$throw, done);
        }
        if (a >= -2147483648 && a <= 2147483647) {
          return done(state2, fromCharCode(a));
        }
        return fail3("invalid character code (should not happen)")(state2, more, lift1, $$throw, done);
      })
    ));
    const charEsc = choice3(arrayMap((v1) => (state1, more, lift1, $$throw, done) => more((v1$1) => $$char(v1._1)(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => done(state2, v1._2))
    )))(zip(["a", "b", "f", "n", "r", "t", "v", "\\", '"', "'"])(["\x07", "\b", "\f", "\n", "\r", "	", "\v", "\\", '"', "'"])));
    const charAscii = choice3(arrayMap((v1) => $$try4((state1, more, lift1, $$throw, done) => more((v1$1) => string2(v1._1)(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => done(state2, v1._2))
    ))))(zip([
      "NUL",
      "SOH",
      "STX",
      "ETX",
      "EOT",
      "ENQ",
      "ACK",
      "BEL",
      "DLE",
      "DC1",
      "DC2",
      "DC3",
      "DC4",
      "NAK",
      "SYN",
      "ETB",
      "CAN",
      "SUB",
      "ESC",
      "DEL",
      "BS",
      "HT",
      "LF",
      "VT",
      "FF",
      "CR",
      "SO",
      "SI",
      "EM",
      "FS",
      "GS",
      "RS",
      "US",
      "SP"
    ])([
      "\0",
      "",
      "",
      "",
      "",
      "",
      "",
      "\x07",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "\x1B",
      "\x7F",
      "\b",
      "	",
      "\n",
      "\v",
      "\f",
      "\r",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      " "
    ])));
    return {
      identifier: (() => {
        const $21 = $$try4((() => {
          const $212 = withErrorMessage((state1, more, lift1, $$throw, done) => more((v1) => v.identStart(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2) => {
              const $30 = many3(v.identLetter);
              return more((v1$1) => $30(
                state2,
                more,
                lift1,
                $$throw,
                (state2$1, a$1) => more((v2$1) => done(state2$1, singleton(a) + fromCharArray(a$1)))
              ));
            })
          )))("identifier");
          return (state1, more, lift1, $$throw, done) => more((v1) => $212(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2) => {
              if (isReserved(theReservedNames(v))((() => {
                if (v.caseSensitive) {
                  return a;
                }
                return toLower(a);
              })())) {
                return fail3("reserved word " + showStringImpl(a))(state2, more, lift1, $$throw, done);
              }
              return done(state2, a);
            })
          ));
        })());
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $21(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a)))))
        )));
      })(),
      reserved: (name2) => {
        const $22 = $$try4((() => {
          const $222 = (() => {
            if (v.caseSensitive) {
              return (state1, more, lift1, $$throw, done) => more((v1) => string2(name2)(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, name2))));
            }
            const msg = showStringImpl(name2);
            const walk = (name$p) => {
              const v1 = uncons2(name$p);
              if (v1.tag === "Nothing") {
                return (state1, v$1, v1$1, v2, done) => done(state1, unit);
              }
              if (v1.tag === "Just") {
                const $26 = withErrorMessage((() => {
                  if (checkAttr([4096, 512, 524288, 1048576, 16384])(toCharCode(v1._1.head))) {
                    const $262 = toChar(toLowerSimple(singleton(v1._1.head)));
                    if ($262.tag === "Just") {
                      const $272 = toChar(toUpperSimple(singleton(v1._1.head)));
                      if ($272.tag === "Just") {
                        return altParserT.alt($$char($262._1))($$char($272._1));
                      }
                      return $$char(v1._1.head);
                    }
                    return $$char(v1._1.head);
                  }
                  return $$char(v1._1.head);
                })())(msg);
                const $27 = walk(v1._1.tail);
                return (state1, more, lift1, $$throw, done) => more((v2) => more((v1$1) => $26(
                  state1,
                  more,
                  lift1,
                  $$throw,
                  (state2, a) => more((v2$1) => more((v3) => $27(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
                )));
              }
              fail();
            };
            const $24 = walk(name2);
            return (state1, more, lift1, $$throw, done) => more((v1) => $24(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, name2))));
          })();
          const $232 = withErrorMessage(notFollowedBy(v.identLetter))("end of " + name2);
          return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $222(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2$1) => more((v3) => $232(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
          )));
        })());
        const $23 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $22(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $23(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a)))))
        )));
      },
      operator: (() => {
        const $21 = $$try4((() => {
          const $212 = withErrorMessage((state1, more, lift1, $$throw, done) => more((v1) => v.opStart(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2) => {
              const $30 = many3(v.opLetter);
              return more((v1$1) => $30(
                state2,
                more,
                lift1,
                $$throw,
                (state2$1, a$1) => more((v2$1) => done(state2$1, singleton(a) + fromCharArray(a$1)))
              ));
            })
          )))("operator");
          return (state1, more, lift1, $$throw, done) => more((v1) => $212(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2) => {
              if (isReserved(sortBy2(ordString.compare)(v.reservedOpNames))(a)) {
                return fail3("reserved operator " + a)(state2, more, lift1, $$throw, done);
              }
              return done(state2, a);
            })
          ));
        })());
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $21(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a)))))
        )));
      })(),
      reservedOp: (name2) => {
        const $22 = $$try4((state1, more, lift1, $$throw, done) => more((v1) => string2(name2)(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2) => withErrorMessage(notFollowedBy(v.opLetter))("end of " + name2)(state2, more, lift1, $$throw, done))
        )));
        const $23 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $22(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $23(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a)))))
        )));
      },
      charLiteral: withErrorMessage((() => {
        const $21 = between2($$char("'"))(withErrorMessage($$char("'"))("end of character"))(altParserT.alt(satisfy((c) => c !== "'" && (c !== "\\" && c > "")))(withErrorMessage((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $$char("\\")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => altParserT.alt(charEsc)(altParserT.alt(charNum)(altParserT.alt(charAscii)(withErrorMessage((state1$1, more$1, lift1$1, throw$1, done$1) => more$1((v1$1) => $$char("^")(
            state1$1,
            more$1,
            lift1$1,
            throw$1,
            (state2$1, a$1) => more$1((v2$2) => more$1((v1$2) => withErrorMessage(satisfyCP(isUpper))("uppercase letter")(
              state2$1,
              more$1,
              lift1$1,
              throw$1,
              (state2$2, a$2) => more$1((v2$3) => {
                const $45 = (toCharCode(a$2) - 65 | 0) + 1 | 0;
                if ($45 >= -2147483648 && $45 <= 2147483647) {
                  return done$1(state2$2, fromCharCode($45));
                }
                return fail3("invalid character code (should not happen)")(state2$2, more$1, lift1$1, throw$1, done$1);
              })
            )))
          )))("escape code"))))(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
        ))))("literal character")));
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $21(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a)))))
        )));
      })())("character"),
      stringLiteral: (() => {
        const $21 = withErrorMessage((() => {
          const $212 = between2($$char('"'))(withErrorMessage($$char('"'))("end of string"))(many1(altParserT.alt((() => {
            const $213 = satisfy((c) => c !== '"' && (c !== "\\" && c > ""));
            return (state1, more, lift1, $$throw, done) => more((v1) => $213(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, $Maybe("Just", a)))));
          })())(withErrorMessage((state1, more, lift1, $$throw, done) => more((v1) => $$char("\\")(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2) => altParserT.alt((state1$1, more$1, lift1$1, throw$1, done$1) => more$1((v1$1) => escapeGap(
              state1$1,
              more$1,
              lift1$1,
              throw$1,
              (state2$1, a$1) => more$1((v2$1) => done$1(state2$1, Nothing))
            )))(altParserT.alt((state1$1, more$1, lift1$1, throw$1, done$1) => more$1((v1$1) => $$char("&")(
              state1$1,
              more$1,
              lift1$1,
              throw$1,
              (state2$1, a$1) => more$1((v2$1) => done$1(state2$1, Nothing))
            )))((state1$1, more$1, lift1$1, throw$1, done$1) => more$1((v1$1) => altParserT.alt(charEsc)(altParserT.alt(charNum)(altParserT.alt(charAscii)(withErrorMessage((state1$2, more$2, lift1$2, throw$2, done$2) => more$2((v1$2) => $$char("^")(
              state1$2,
              more$2,
              lift1$2,
              throw$2,
              (state2$1, a$1) => more$2((v2$1) => more$2((v1$3) => withErrorMessage(satisfyCP(isUpper))("uppercase letter")(
                state2$1,
                more$2,
                lift1$2,
                throw$2,
                (state2$2, a$2) => more$2((v2$2) => {
                  const $49 = (toCharCode(a$2) - 65 | 0) + 1 | 0;
                  if ($49 >= -2147483648 && $49 <= 2147483647) {
                    return done$2(state2$2, fromCharCode($49));
                  }
                  return fail3("invalid character code (should not happen)")(state2$2, more$2, lift1$2, throw$2, done$2);
                })
              )))
            )))("escape code"))))(state1$1, more$1, lift1$1, throw$1, (state2$1, a$1) => more$1((v2$1) => done$1(state2$1, $Maybe("Just", a$1)))))))(
              state2,
              more,
              lift1,
              $$throw,
              done
            ))
          )))("string character"))));
          return (state1, more, lift1, $$throw, done) => more((v1) => $212(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2) => done(
              state2,
              fromCharArray(toUnfoldable11(foldableList.foldr((v1$1) => (chars) => {
                if (v1$1.tag === "Nothing") {
                  return chars;
                }
                if (v1$1.tag === "Just") {
                  return $List("Cons", v1$1._1, chars);
                }
                fail();
              })(Nil)(a)))
            ))
          ));
        })())("literal string");
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $21(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a)))))
        )));
      })(),
      natural: withErrorMessage((() => {
        const $21 = altParserT.alt(withErrorMessage((() => {
          const $212 = altParserT.alt(hexadecimal)(altParserT.alt(octal)(altParserT.alt(decimal)((state1, v$1, v1, v2, done) => done(state1, 0))));
          return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $$char("0")(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2$1) => more((v3) => $212(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
          )));
        })())(""))(decimal);
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $21(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a)))))
        )));
      })())("natural"),
      integer: withErrorMessage((() => {
        const $21 = whiteSpace$p(v);
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => more((v1$1) => more((v2$1) => more((v1$2) => sign1(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$2) => more((v3) => $21(
            state2,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more((v4) => more((v2$3) => {
              const $41 = altParserT.alt(withErrorMessage((() => {
                const $412 = altParserT.alt(hexadecimal)(altParserT.alt(octal)(altParserT.alt(decimal)((state1$1, v$1, v1$3, v2$4, done$1) => done$1(
                  state1$1,
                  0
                ))));
                return (state1$1, more$1, lift1$1, throw$1, done$1) => more$1((v2$4) => more$1((v1$3) => $$char("0")(
                  state1$1,
                  more$1,
                  lift1$1,
                  throw$1,
                  (state2$1, a$2) => more$1((v2$5) => more$1((v3$1) => $412(state2$1, more$1, lift1$1, throw$1, (state3$1, a$3) => more$1((v4$1) => done$1(state3$1, a$3)))))
                )));
              })())(""))(decimal);
              return more((v1$3) => $41(
                state3,
                more,
                lift1,
                $$throw,
                (state2$1, a$2) => more((v2$4) => {
                  const $46 = a(a$2);
                  return more((v2$5) => more((v3$1) => $22(state2$1, more, lift1, $$throw, (state3$1, a$3) => more((v4$1) => done(state3$1, $46)))));
                })
              ));
            }))
          )))
        ))))));
      })())("integer"),
      float: withErrorMessage((() => {
        const $21 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => more((v1$1) => decimal(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => fractExponent(a)(
            state2,
            more,
            lift1,
            $$throw,
            (state2$1, a$1) => more((v2$2) => more((v3) => $21(state2$1, more, lift1, $$throw, (state3, a$2) => more((v4) => done(state3, a$1)))))
          ))
        ))));
      })())("float"),
      naturalOrFloat: withErrorMessage((() => {
        const $21 = altParserT.alt((() => {
          const $212 = altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => altParserT.alt(hexadecimal)(octal)(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2) => done(state2, $Either("Left", a)))
          )))(altParserT.alt(decimalFloat)(altParserT.alt((() => {
            const $213 = fractExponent(0);
            return (state1, more, lift1, $$throw, done) => more((v1) => $213(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, $Either("Right", a)))));
          })())((state1, v$1, v1, v2, done) => done(state1, $Either("Left", 0)))));
          return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $$char("0")(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2$1) => more((v3) => $212(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
          )));
        })())(decimalFloat);
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $21(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a)))))
        )));
      })())("number"),
      decimal,
      hexadecimal,
      octal,
      symbol: (name2) => {
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2(name2)(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, name2))))))
        ))));
      },
      lexeme: (p) => {
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => p(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a)))))
        )));
      },
      whiteSpace: whiteSpace$p(v),
      parens: (p) => between2((() => {
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2("(")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "("))))))
        ))));
      })())((() => {
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2(")")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ")"))))))
        ))));
      })())(p),
      braces: (p) => between2((() => {
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2("{")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "{"))))))
        ))));
      })())((() => {
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2("}")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "}"))))))
        ))));
      })())(p),
      angles: (p) => between2((() => {
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2("<")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "<"))))))
        ))));
      })())((() => {
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2(">")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ">"))))))
        ))));
      })())(p),
      brackets: (p) => between2((() => {
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2("[")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "["))))))
        ))));
      })())((() => {
        const $22 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2("]")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "]"))))))
        ))));
      })())(p),
      semi,
      comma: comma2,
      colon: (() => {
        const $21 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2(":")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $21(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ":"))))))
        ))));
      })(),
      dot: (() => {
        const $21 = whiteSpace$p(v);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => string2(".")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => $21(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "."))))))
        ))));
      })(),
      semiSep: (p) => sepBy(p)(semi),
      semiSep1: (p) => sepBy1(p)(semi),
      commaSep: (p) => sepBy(p)(comma2),
      commaSep1: (p) => sepBy1(p)(comma2)
    };
  };

  // output-es/Primitive.Parse/index.js
  var opDefs = /* @__PURE__ */ fromFoldable(ordString)(foldableArray)([
    /* @__PURE__ */ $Tuple(".", { op: ".", prec: 8, assoc: AssocLeft }),
    /* @__PURE__ */ $Tuple("!", { op: "!", prec: 8, assoc: AssocLeft }),
    /* @__PURE__ */ $Tuple("**", { op: "**", prec: 8, assoc: AssocRight }),
    /* @__PURE__ */ $Tuple("*", { op: "*", prec: 7, assoc: AssocLeft }),
    /* @__PURE__ */ $Tuple("/", { op: "/", prec: 7, assoc: AssocLeft }),
    /* @__PURE__ */ $Tuple("+", { op: "+", prec: 6, assoc: AssocLeft }),
    /* @__PURE__ */ $Tuple("-", { op: "-", prec: 6, assoc: AssocLeft }),
    /* @__PURE__ */ $Tuple(":", { op: ":", prec: 6, assoc: AssocRight }),
    /* @__PURE__ */ $Tuple("++", { op: "++", prec: 5, assoc: AssocRight }),
    /* @__PURE__ */ $Tuple("==", { op: "==", prec: 4, assoc: AssocNone }),
    /* @__PURE__ */ $Tuple("/=", { op: "/=", prec: 4, assoc: AssocNone }),
    /* @__PURE__ */ $Tuple("<", { op: "<", prec: 4, assoc: AssocLeft }),
    /* @__PURE__ */ $Tuple(">", { op: ">", prec: 4, assoc: AssocLeft }),
    /* @__PURE__ */ $Tuple("<=", { op: "<=", prec: 4, assoc: AssocLeft }),
    /* @__PURE__ */ $Tuple(">=", { op: ">=", prec: 4, assoc: AssocLeft })
  ]);

  // output-es/Bindings/index.js
  var keys3 = (v) => {
    if (v.tag === "Nil") {
      return Leaf2;
    }
    if (v.tag === "Cons") {
      return unionWith(ordString)($$const)($Map(
        "Two",
        Leaf2,
        v._1._1,
        unit,
        Leaf2
      ))(keys3(v._2));
    }
    fail();
  };

  // output-es/Data.List.NonEmpty/index.js
  var wrappedOperation = (name2) => (f) => (v) => {
    const v1 = f($List("Cons", v._1, v._2));
    if (v1.tag === "Cons") {
      return $NonEmpty(v1._1, v1._2);
    }
    if (v1.tag === "Nil") {
      return _crashWith("Impossible: empty list in NonEmptyList " + name2);
    }
    fail();
  };

  // output-es/SExpr/index.js
  var $Expr2 = (tag, _1, _2, _3, _4) => ({ tag, _1, _2, _3, _4 });
  var $ListRest = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
  var $ListRestPattern = (tag, _1, _2) => ({ tag, _1, _2 });
  var $Module2 = (_1) => ({ tag: "Module", _1 });
  var $Pattern = (tag, _1, _2) => ({ tag, _1, _2 });
  var $Qualifier = (tag, _1, _2) => ({ tag, _1, _2 });
  var $VarDef3 = (_1, _2) => ({ tag: "VarDef", _1, _2 });
  var difference4 = /* @__PURE__ */ difference(eqString);
  var toUnfoldable13 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
  var fromFoldable8 = /* @__PURE__ */ fromFoldable2(foldableNonEmptyList);
  var fromFoldable15 = /* @__PURE__ */ fromFoldable2(foldableList);
  var fromFoldable23 = /* @__PURE__ */ fromFoldable2(foldableArray);
  var traverse4 = /* @__PURE__ */ (() => traversableNonEmptyList.traverse(applicativeEither))();
  var traverse12 = /* @__PURE__ */ (() => traversableList.traverse(applicativeEither))();
  var traverse22 = /* @__PURE__ */ (() => traversableTuple.traverse(applicativeEither))();
  var traverse32 = /* @__PURE__ */ (() => traversablePair.traverse(applicativeEither))();
  var foldM4 = (f) => (b0) => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, v = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = bindEither.bind(b)((a) => f(a)(v._1));
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      ;
      return go$r;
    };
    return go($Either("Right", b0));
  };
  var PEnd = /* @__PURE__ */ $ListRestPattern("PEnd");
  var PNext = (value0) => (value1) => $ListRestPattern("PNext", value0, value1);
  var PConstr = (value0) => (value1) => $Pattern("PConstr", value0, value1);
  var PListEmpty = /* @__PURE__ */ $Pattern("PListEmpty");
  var PListNonEmpty = (value0) => (value1) => $Pattern("PListNonEmpty", value0, value1);
  var Int3 = (value0) => (value1) => $Expr2("Int", value0, value1);
  var Float3 = (value0) => (value1) => $Expr2("Float", value0, value1);
  var Str3 = (value0) => (value1) => $Expr2("Str", value0, value1);
  var Constr3 = (value0) => (value1) => (value2) => $Expr2("Constr", value0, value1, value2);
  var Record3 = (value0) => (value1) => $Expr2("Record", value0, value1);
  var Dictionary3 = (value0) => (value1) => $Expr2("Dictionary", value0, value1);
  var Matrix3 = (value0) => (value1) => (value2) => (value3) => $Expr2("Matrix", value0, value1, value2, value3);
  var App3 = (value0) => (value1) => $Expr2("App", value0, value1);
  var MatchAs = (value0) => (value1) => $Expr2("MatchAs", value0, value1);
  var IfElse = (value0) => (value1) => (value2) => $Expr2("IfElse", value0, value1, value2);
  var ListNonEmpty = (value0) => (value1) => (value2) => $Expr2("ListNonEmpty", value0, value1, value2);
  var ListEnum = (value0) => (value1) => $Expr2("ListEnum", value0, value1);
  var ListComp = (value0) => (value1) => (value2) => $Expr2("ListComp", value0, value1, value2);
  var Let2 = (value0) => (value1) => $Expr2("Let", value0, value1);
  var LetRec2 = (value0) => (value1) => $Expr2("LetRec", value0, value1);
  var Next = (value0) => (value1) => (value2) => $ListRest("Next", value0, value1, value2);
  var $$Generator = (value0) => (value1) => $Qualifier("Generator", value0, value1);
  var VarDef2 = (value0) => (value1) => $VarDef3(value0, value1);
  var RecDef = (x2) => x2;
  var functorVarDef2 = { map: (f) => (m) => $VarDef3(m._1, functorExpr2.map(f)(m._2)) };
  var functorQualifier = {
    map: (f) => (m) => {
      if (m.tag === "Guard") {
        return $Qualifier("Guard", functorExpr2.map(f)(m._1));
      }
      if (m.tag === "Generator") {
        return $Qualifier("Generator", m._1, functorExpr2.map(f)(m._2));
      }
      if (m.tag === "Declaration") {
        return $Qualifier("Declaration", $VarDef3(m._1._1, functorExpr2.map(f)(m._1._2)));
      }
      fail();
    }
  };
  var functorListRest = {
    map: (f) => (m) => {
      if (m.tag === "End") {
        return $ListRest("End", f(m._1));
      }
      if (m.tag === "Next") {
        return $ListRest("Next", f(m._1), functorExpr2.map(f)(m._2), functorListRest.map(f)(m._3));
      }
      fail();
    }
  };
  var functorExpr2 = {
    map: (f) => (m) => {
      if (m.tag === "Var") {
        return $Expr2("Var", m._1);
      }
      if (m.tag === "Op") {
        return $Expr2("Op", m._1);
      }
      if (m.tag === "Int") {
        return $Expr2("Int", f(m._1), m._2);
      }
      if (m.tag === "Float") {
        return $Expr2("Float", f(m._1), m._2);
      }
      if (m.tag === "Str") {
        return $Expr2("Str", f(m._1), m._2);
      }
      if (m.tag === "Constr") {
        return $Expr2("Constr", f(m._1), m._2, listMap(functorExpr2.map(f))(m._3));
      }
      if (m.tag === "Record") {
        return $Expr2("Record", f(m._1), listMap(functorTuple.map(functorExpr2.map(f)))(m._2));
      }
      if (m.tag === "Dictionary") {
        return $Expr2("Dictionary", f(m._1), listMap(functorPair.map(functorExpr2.map(f)))(m._2));
      }
      if (m.tag === "Matrix") {
        return $Expr2("Matrix", f(m._1), functorExpr2.map(f)(m._2), m._3, functorExpr2.map(f)(m._4));
      }
      if (m.tag === "Lambda") {
        return $Expr2("Lambda", functorClauses.map(f)(m._1));
      }
      if (m.tag === "Project") {
        return $Expr2("Project", functorExpr2.map(f)(m._1), m._2);
      }
      if (m.tag === "App") {
        return $Expr2("App", functorExpr2.map(f)(m._1), functorExpr2.map(f)(m._2));
      }
      if (m.tag === "BinaryApp") {
        return $Expr2("BinaryApp", functorExpr2.map(f)(m._1), m._2, functorExpr2.map(f)(m._3));
      }
      if (m.tag === "MatchAs") {
        return $Expr2("MatchAs", functorExpr2.map(f)(m._1), functorNonEmptyList.map(functorTuple.map(functorExpr2.map(f)))(m._2));
      }
      if (m.tag === "IfElse") {
        return $Expr2("IfElse", functorExpr2.map(f)(m._1), functorExpr2.map(f)(m._2), functorExpr2.map(f)(m._3));
      }
      if (m.tag === "ListEmpty") {
        return $Expr2("ListEmpty", f(m._1));
      }
      if (m.tag === "ListNonEmpty") {
        return $Expr2("ListNonEmpty", f(m._1), functorExpr2.map(f)(m._2), functorListRest.map(f)(m._3));
      }
      if (m.tag === "ListEnum") {
        return $Expr2("ListEnum", functorExpr2.map(f)(m._1), functorExpr2.map(f)(m._2));
      }
      if (m.tag === "ListComp") {
        return $Expr2("ListComp", f(m._1), functorExpr2.map(f)(m._2), listMap(functorQualifier.map(f))(m._3));
      }
      if (m.tag === "Let") {
        return $Expr2("Let", functorNonEmptyList.map(functorVarDef2.map(f))(m._1), functorExpr2.map(f)(m._2));
      }
      if (m.tag === "LetRec") {
        return $Expr2("LetRec", functorNonEmptyList.map(functorTuple.map(functorClause.map(f)))(m._1), functorExpr2.map(f)(m._2));
      }
      fail();
    }
  };
  var functorClauses = { map: (f) => (m) => functorNonEmptyList.map(functorClause.map(f))(m) };
  var functorClause = { map: (f) => (m) => $Tuple(m._1, functorExpr2.map(f)(m._2)) };
  var functorModule = {
    map: (f) => (v) => $Module2(listMap((v1) => {
      if (v1.tag === "Left") {
        return $Either("Left", functorNonEmptyList.map(functorVarDef2.map(f))(v1._1));
      }
      if (v1.tag === "Right") {
        return $Either(
          "Right",
          functorNonEmptyList.map((v2) => $Tuple(v2._1, $Tuple(v2._2._1, functorExpr2.map(f)(v2._2._2))))(v1._1)
        );
      }
      fail();
    })(v._1))
  };
  var unlessBwd = (dictBoundedJoinSemilattice) => {
    const join2 = dictBoundedJoinSemilattice.JoinSemilattice0().join;
    return (m) => (c) => $Tuple(
      $$get(c)(m),
      (() => {
        const go = (go$a0$copy) => (go$a1$copy) => {
          let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
          while (go$c) {
            const b = go$a0, v = go$a1;
            if (v.tag === "Nil") {
              go$c = false;
              go$r = b;
              continue;
            }
            if (v.tag === "Cons") {
              go$a0 = join2(b)(v._1);
              go$a1 = v._2;
              continue;
            }
            fail();
          }
          ;
          return go$r;
        };
        return go(dictBoundedJoinSemilattice.bot)(listMap((x2) => {
          const $6 = (() => {
            const go$1 = (go$1$a0$copy) => (go$1$a1$copy) => {
              let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
              while (go$1$c) {
                const n = go$1$a0, acc = go$1$a1;
                if (n <= 0) {
                  go$1$c = false;
                  go$1$r = acc;
                  continue;
                }
                go$1$a0 = n - 1 | 0;
                go$1$a1 = (() => {
                  if (acc.tag === "ContElim") {
                    if (acc._1.tag === "ElimVar") {
                      return acc._1._2;
                    }
                    fail();
                  }
                  fail();
                })();
                continue;
              }
              ;
              return go$1$r;
            };
            return go$1(successful(arity(x2)))($$get(x2)(m));
          })();
          if ($6.tag === "ContExpr") {
            if ($6._1.tag === "Constr") {
              if ($6._1._3.tag === "Nil") {
                if ($6._1._2 === "Nil") {
                  return $6._1._1;
                }
                fail();
              }
              fail();
            }
            fail();
          }
          fail();
        })(difference4(toUnfoldable13(fromFoldable12(keys2(successful(dataTypeForCtr.dataTypeFor(c))._2))))($List(
          "Cons",
          c,
          Nil
        ))));
      })()
    );
  };
  var pattCont_ListRest_Fwd = (v) => (\u03BA) => {
    if (v.tag === "PEnd") {
      return $Either(
        "Right",
        $Elim("ElimConstr", runST(bind_(newImpl)(poke2("Nil")(\u03BA))))
      );
    }
    if (v.tag === "PNext") {
      const $2 = pattArgsFwd($List(
        "Cons",
        $Either("Left", v._1),
        $List("Cons", $Either("Right", v._2), Nil)
      ))(\u03BA);
      if ($2.tag === "Left") {
        return $Either("Left", $2._1);
      }
      if ($2.tag === "Right") {
        return $Either(
          "Right",
          $Elim("ElimConstr", runST(bind_(newImpl)(poke2(":")($2._1))))
        );
      }
      fail();
    }
    fail();
  };
  var pattContFwd = (v) => (\u03BA) => {
    if (v.tag === "PVar") {
      return $Either("Right", $Elim("ElimVar", v._1, \u03BA));
    }
    if (v.tag === "PConstr") {
      const $2 = checkArity(v._1)((() => {
        const go = (go$a0$copy) => (go$a1$copy) => {
          let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
          while (go$c) {
            const b = go$a0, v$1 = go$a1;
            if (v$1.tag === "Nil") {
              go$c = false;
              go$r = b;
              continue;
            }
            if (v$1.tag === "Cons") {
              go$a0 = b + 1 | 0;
              go$a1 = v$1._2;
              continue;
            }
            fail();
          }
          ;
          return go$r;
        };
        return go(0)(v._2);
      })());
      const $3 = pattArgsFwd(listMap(Left)(v._2))(\u03BA);
      return applyEither.apply((() => {
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", identity);
        }
        fail();
      })())((() => {
        if ($3.tag === "Left") {
          return $Either("Left", $3._1);
        }
        if ($3.tag === "Right") {
          return $Either(
            "Right",
            $Elim("ElimConstr", runST(bind_(newImpl)(poke2(v._1)($3._1))))
          );
        }
        fail();
      })());
    }
    if (v.tag === "PRecord") {
      const $2 = ElimRecord(keys3(v._1));
      const $3 = pattArgsFwd(listMap((x2) => $Either("Left", x2._2))(sortBy((x2) => (y2) => ordString.compare(x2._1)(y2._1))(v._1)))(\u03BA);
      if ($3.tag === "Left") {
        return $Either("Left", $3._1);
      }
      if ($3.tag === "Right") {
        return $Either("Right", $2($3._1));
      }
      fail();
    }
    if (v.tag === "PListEmpty") {
      return $Either(
        "Right",
        $Elim("ElimConstr", runST(bind_(newImpl)(poke2("Nil")(\u03BA))))
      );
    }
    if (v.tag === "PListNonEmpty") {
      const $2 = pattArgsFwd($List(
        "Cons",
        $Either("Left", v._1),
        $List("Cons", $Either("Right", v._2), Nil)
      ))(\u03BA);
      if ($2.tag === "Left") {
        return $Either("Left", $2._1);
      }
      if ($2.tag === "Right") {
        return $Either(
          "Right",
          $Elim("ElimConstr", runST(bind_(newImpl)(poke2(":")($2._1))))
        );
      }
      fail();
    }
    fail();
  };
  var pattArgsFwd = (v) => (\u03BA) => {
    if (v.tag === "Nil") {
      return $Either("Right", \u03BA);
    }
    if (v.tag === "Cons") {
      if (v._1.tag === "Left") {
        const $2 = bindEither.bind(pattArgsFwd(v._2)(\u03BA))(pattContFwd(v._1._1));
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", $Cont("ContElim", $2._1));
        }
        fail();
      }
      if (v._1.tag === "Right") {
        const $2 = bindEither.bind(pattArgsFwd(v._2)(\u03BA))(pattCont_ListRest_Fwd(v._1._1));
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", $Cont("ContElim", $2._1));
        }
        fail();
      }
      fail();
    }
    fail();
  };
  var $pattCpattCpattA = ($pattCpattCpattA$b$copy, $pattCpattCpattA$a0$copy, $pattCpattCpattA$a1$copy) => {
    let $pattCpattCpattA$b = $pattCpattCpattA$b$copy;
    let $pattCpattCpattA$a0 = $pattCpattCpattA$a0$copy;
    let $pattCpattCpattA$a1 = $pattCpattCpattA$a1$copy;
    let $pattCpattCpattA$c = true;
    let $pattCpattCpattA$r;
    while ($pattCpattCpattA$c) {
      if ($pattCpattCpattA$b === 0) {
        const v = $pattCpattCpattA$a0, v1 = $pattCpattCpattA$a1;
        if (v.tag === "ElimVar") {
          $pattCpattCpattA$c = false;
          $pattCpattCpattA$r = unsafePerformEffect(throwException(error("absurd")));
          continue;
        }
        if (v.tag === "ElimRecord") {
          $pattCpattCpattA$c = false;
          $pattCpattCpattA$r = unsafePerformEffect(throwException(error("absurd")));
          continue;
        }
        if (v.tag === "ElimConstr") {
          if (v1.tag === "PEnd") {
            $pattCpattCpattA$c = false;
            $pattCpattCpattA$r = $$get("Nil")(v._1);
            continue;
          }
          if (v1.tag === "PNext") {
            $pattCpattCpattA$b = 2;
            $pattCpattCpattA$a0 = $List(
              "Cons",
              $Either("Left", v1._1),
              $List("Cons", $Either("Right", v1._2), Nil)
            );
            $pattCpattCpattA$a1 = $$get(":")(v._1);
            continue;
          }
          fail();
        }
        if (v.tag === "ElimSug") {
          $pattCpattCpattA$b = 0;
          $pattCpattCpattA$a0 = v._2;
          $pattCpattCpattA$a1 = v1;
          continue;
        }
        fail();
      }
      if ($pattCpattCpattA$b === 1) {
        const v = $pattCpattCpattA$a0, v1 = $pattCpattCpattA$a1;
        if (v1.tag === "ElimVar") {
          if (v.tag === "PVar") {
            $pattCpattCpattA$c = false;
            $pattCpattCpattA$r = v1._2;
            continue;
          }
          $pattCpattCpattA$c = false;
          $pattCpattCpattA$r = unsafePerformEffect(throwException(error("absurd")));
          continue;
        }
        if (v1.tag === "ElimConstr") {
          if (v.tag === "PConstr") {
            $pattCpattCpattA$b = 2;
            $pattCpattCpattA$a0 = listMap(Left)(v._2);
            $pattCpattCpattA$a1 = $$get(v._1)(v1._1);
            continue;
          }
          if (v.tag === "PListEmpty") {
            $pattCpattCpattA$c = false;
            $pattCpattCpattA$r = $$get("Nil")(v1._1);
            continue;
          }
          if (v.tag === "PListNonEmpty") {
            $pattCpattCpattA$b = 2;
            $pattCpattCpattA$a0 = $List(
              "Cons",
              $Either("Left", v._1),
              $List("Cons", $Either("Right", v._2), Nil)
            );
            $pattCpattCpattA$a1 = $$get(":")(v1._1);
            continue;
          }
          $pattCpattCpattA$c = false;
          $pattCpattCpattA$r = unsafePerformEffect(throwException(error("absurd")));
          continue;
        }
        if (v.tag === "PRecord") {
          if (v1.tag === "ElimRecord") {
            $pattCpattCpattA$b = 2;
            $pattCpattCpattA$a0 = listMap((x2) => $Either("Left", x2._2))(sortBy((x2) => (y2) => ordString.compare(x2._1)(y2._1))(v._1));
            $pattCpattCpattA$a1 = v1._2;
            continue;
          }
          $pattCpattCpattA$c = false;
          $pattCpattCpattA$r = unsafePerformEffect(throwException(error("absurd")));
          continue;
        }
        $pattCpattCpattA$c = false;
        $pattCpattCpattA$r = unsafePerformEffect(throwException(error("absurd")));
        continue;
      }
      if ($pattCpattCpattA$b === 2) {
        const v = $pattCpattCpattA$a0, v1 = $pattCpattCpattA$a1;
        if (v.tag === "Nil") {
          $pattCpattCpattA$c = false;
          $pattCpattCpattA$r = v1;
          continue;
        }
        if (v.tag === "Cons") {
          if (v._1.tag === "Left") {
            $pattCpattCpattA$b = 2;
            $pattCpattCpattA$a0 = v._2;
            $pattCpattCpattA$a1 = pattContBwd(v._1._1)((() => {
              if (v1.tag === "ContElim") {
                return v1._1;
              }
              return unsafePerformEffect(throwException(error("Eliminator expected")));
            })());
            continue;
          }
          if (v._1.tag === "Right") {
            $pattCpattCpattA$b = 2;
            $pattCpattCpattA$a0 = v._2;
            $pattCpattCpattA$a1 = pattCont_ListRest_Bwd((() => {
              if (v1.tag === "ContElim") {
                return v1._1;
              }
              return unsafePerformEffect(throwException(error("Eliminator expected")));
            })())(v._1._1);
            continue;
          }
          fail();
        }
        fail();
      }
    }
    ;
    return $pattCpattCpattA$r;
  };
  var pattCont_ListRest_Bwd = (v) => (v1) => $pattCpattCpattA(0, v, v1);
  var pattContBwd = (v) => (v1) => $pattCpattCpattA(1, v, v1);
  var orElseBwd = (dictBoundedJoinSemilattice) => {
    const unlessBwd1 = unlessBwd(dictBoundedJoinSemilattice);
    const join2 = dictBoundedJoinSemilattice.JoinSemilattice0().join;
    return (v) => (v1) => {
      const $5 = (m, \u03C0, \u03C0s) => {
        const v2 = (() => {
          if (\u03C0.tag === "Left") {
            if (\u03C0._1.tag === "PVar") {
              return unsafePerformEffect(throwException(error("absurd")));
            }
            if (\u03C0._1.tag === "PRecord") {
              return unsafePerformEffect(throwException(error("absurd")));
            }
            if (\u03C0._1.tag === "PConstr") {
              return $Tuple(\u03C0._1._1, listMap(Left)(\u03C0._1._2));
            }
            if (\u03C0._1.tag === "PListEmpty") {
              return $Tuple("Nil", Nil);
            }
            if (\u03C0._1.tag === "PListNonEmpty") {
              return $Tuple(
                ":",
                $List("Cons", $Either("Left", \u03C0._1._1), $List("Cons", $Either("Right", \u03C0._1._2), Nil))
              );
            }
            fail();
          }
          if (\u03C0.tag === "Right") {
            if (\u03C0._1.tag === "PEnd") {
              return $Tuple("Nil", Nil);
            }
            if (\u03C0._1.tag === "PNext") {
              return $Tuple(
                ":",
                $List("Cons", $Either("Left", \u03C0._1._1), $List("Cons", $Either("Right", \u03C0._1._2), Nil))
              );
            }
            fail();
          }
          fail();
        })();
        const v3 = unlessBwd1(m)(v2._1);
        const $10 = orElseBwd(dictBoundedJoinSemilattice)(v3._1)(foldableList.foldr(Cons)(\u03C0s)(v2._2));
        return $Tuple(
          $Cont("ContElim", $Elim("ElimConstr", fromFoldable8($NonEmpty($Tuple(v2._1, $10._1), Nil)))),
          join2(v3._2)($10._2)
        );
      };
      if (v1.tag === "Nil") {
        return $Tuple(v, dictBoundedJoinSemilattice.bot);
      }
      if (v.tag === "ContNone") {
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v1.tag === "Cons") {
        if (v.tag === "ContElim") {
          if (v1._1.tag === "Left") {
            if (v1._1._1.tag === "PVar") {
              if (v._1.tag === "ElimVar") {
                const $6 = orElseBwd(dictBoundedJoinSemilattice)(v._1._2)(v1._2);
                return $Tuple($Cont("ContElim", $Elim("ElimVar", v1._1._1._1, $6._1)), $6._2);
              }
              if (v._1.tag === "ElimConstr") {
                return $5(v._1._1, v1._1, v1._2);
              }
              return unsafePerformEffect(throwException(error("absurd")));
            }
            if (v1._1._1.tag === "PRecord") {
              if (v._1.tag === "ElimRecord") {
                const $6 = orElseBwd(dictBoundedJoinSemilattice)(v._1._2)(foldableList.foldr(Cons)(v1._2)(listMap((x2) => $Either(
                  "Left",
                  x2._2
                ))(v1._1._1._1)));
                return $Tuple($Cont("ContElim", $Elim("ElimRecord", keys3(v1._1._1._1), $6._1)), $6._2);
              }
              if (v._1.tag === "ElimConstr") {
                return $5(v._1._1, v1._1, v1._2);
              }
              return unsafePerformEffect(throwException(error("absurd")));
            }
            if (v._1.tag === "ElimConstr") {
              return $5(v._1._1, v1._1, v1._2);
            }
            return unsafePerformEffect(throwException(error("absurd")));
          }
          if (v._1.tag === "ElimConstr") {
            return $5(v._1._1, v1._1, v1._2);
          }
          return unsafePerformEffect(throwException(error("absurd")));
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      return unsafePerformEffect(throwException(error("absurd")));
    };
  };
  var unlessFwd = (v) => (\u03B1) => fromFoldable15($List(
    "Cons",
    $Tuple(v._1, v._2),
    listMap((c$p) => $Tuple(
      c$p,
      (() => {
        const $3 = ElimVar("_");
        const go = (go$a0$copy) => (go$a1$copy) => {
          let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
          while (go$c) {
            const n = go$a0, acc = go$a1;
            if (n <= 0) {
              go$c = false;
              go$r = acc;
              continue;
            }
            go$a0 = n - 1 | 0;
            go$a1 = $Cont("ContElim", $3(acc));
            continue;
          }
          ;
          return go$r;
        };
        return go(successful(arity(c$p)))($Cont("ContExpr", $Expr("Constr", \u03B1, "Nil", Nil)));
      })()
    ))(difference4(toUnfoldable13(fromFoldable12(keys2(successful(dataTypeForCtr.dataTypeFor(v._1))._2))))($List(
      "Cons",
      v._1,
      Nil
    )))
  ));
  var orElseFwd = (v) => (v1) => {
    if (v.tag === "ContNone") {
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "ContExpr") {
      return $Cont("ContExpr", v._1);
    }
    if (v.tag === "ContElim") {
      if (v._1.tag === "ElimConstr") {
        const v2 = asSingletonMap(v._1._1);
        return $Cont("ContElim", $Elim("ElimConstr", unlessFwd($Tuple(v2._1, orElseFwd(v2._2)(v1)))(v1)));
      }
      if (v._1.tag === "ElimRecord") {
        return $Cont("ContElim", $Elim("ElimRecord", v._1._1, orElseFwd(v._1._2)(v1)));
      }
      if (v._1.tag === "ElimVar") {
        return $Cont("ContElim", $Elim("ElimVar", v._1._1, orElseFwd(v._1._2)(v1)));
      }
      if (v._1.tag === "ElimSug") {
        const v2 = orElseFwd($Cont("ContElim", v._1._2))(v1);
        if (v2.tag === "ContElim") {
          return $Cont("ContElim", $Elim("ElimSug", v._1._1, v2._1));
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      fail();
    }
    fail();
  };
  var elimBool = (\u03BA) => (\u03BA$p) => $Elim("ElimConstr", fromFoldable23([$Tuple("True", \u03BA), $Tuple("False", \u03BA$p)]));
  var econs = (\u03B1) => (e) => (e$p) => $Expr("Constr", \u03B1, ":", $List("Cons", e, $List("Cons", e$p, Nil)));
  var desugarableListRestExpr = {
    desugFwd: (dictJoinSemilattice) => listRestFwd(dictJoinSemilattice),
    desugBwd: (dictBoundedJoinSemilattice) => listRestBwd(dictBoundedJoinSemilattice),
    Functor0: () => functorListRest,
    Functor1: () => functorExpr,
    FromSugar2: () => fromSugarExpr
  };
  var desugarableExprExpr = {
    desugFwd: (dictJoinSemilattice) => exprFwd(dictJoinSemilattice),
    desugBwd: (dictBoundedJoinSemilattice) => exprBwd(dictBoundedJoinSemilattice),
    Functor0: () => functorExpr2,
    Functor1: () => functorExpr,
    FromSugar2: () => fromSugarExpr
  };
  var varDefsFwd = (dictJoinSemilattice) => (v) => {
    if (v._1._2.tag === "Nil") {
      return applyEither.apply((() => {
        const $2 = varDefFwd(dictJoinSemilattice)(v._1._1);
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", Let($2._1));
        }
        fail();
      })())(desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2));
    }
    if (v._1._2.tag === "Cons") {
      return applyEither.apply((() => {
        const $2 = varDefFwd(dictJoinSemilattice)(v._1._1);
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", Let($2._1));
        }
        fail();
      })())(varDefsFwd(dictJoinSemilattice)($Tuple($NonEmpty(v._1._2._1, v._1._2._2), v._2)));
    }
    fail();
  };
  var varDefsBwd = (dictBoundedJoinSemilattice) => (v) => (v1) => {
    if (v.tag === "Let") {
      if (v1._1._2.tag === "Nil") {
        return $Tuple(
          $NonEmpty($VarDef3(v1._1._1._1, desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._1._2)), Nil),
          desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._2)
        );
      }
      if (v1._1._2.tag === "Cons") {
        const v2 = varDefsBwd(dictBoundedJoinSemilattice)(v._2)($Tuple($NonEmpty(v1._1._2._1, v1._1._2._2), v1._2));
        return $Tuple(
          $NonEmpty(
            $VarDef3(v1._1._1._1, desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._1._2)),
            $List("Cons", v2._1._1, v2._1._2)
          ),
          v2._2
        );
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    return unsafePerformEffect(throwException(error("absurd")));
  };
  var varDefFwd = (dictJoinSemilattice) => (v) => applyEither.apply((() => {
    const $2 = pattContFwd(v._1)(ContNone);
    if ($2.tag === "Left") {
      return $Either("Left", $2._1);
    }
    if ($2.tag === "Right") {
      return $Either("Right", VarDef($2._1));
    }
    fail();
  })())(desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2));
  var recDefsFwd = (dictJoinSemilattice) => (xcs) => {
    const $2 = traverse4(recDefFwd(dictJoinSemilattice))(functorNonEmptyList.map(RecDef)(wrappedOperation("groupBy")(groupBy((x2) => (y2) => x2._1 === y2._1))(xcs)));
    if ($2.tag === "Left") {
      return $Either("Left", $2._1);
    }
    if ($2.tag === "Right") {
      return $Either("Right", fromFoldable8($2._1));
    }
    fail();
  };
  var recDefsBwd = (dictBoundedJoinSemilattice) => (\u03C1) => (xcs) => {
    const go = (v) => $NonEmpty(
      recDefBwd(dictBoundedJoinSemilattice)($Tuple(v._1._1._1, $$get(v._1._1._1)(\u03C1)))(v._1),
      (() => {
        if (v._2.tag === "Nil") {
          return Nil;
        }
        if (v._2.tag === "Cons") {
          const $5 = go($NonEmpty(v._2._1, v._2._2));
          return $List("Cons", $5._1, $5._2);
        }
        fail();
      })()
    );
    return bindNonEmptyList.bind(go(wrappedOperation("groupBy")(groupBy((x2) => (y2) => x2._1 === y2._1))(xcs)))(identity2);
  };
  var recDefFwd = (dictJoinSemilattice) => (xcs) => {
    const $2 = clausesFwd(dictJoinSemilattice)(functorNonEmptyList.map(snd)(xcs));
    if ($2.tag === "Left") {
      return $Either("Left", $2._1);
    }
    if ($2.tag === "Right") {
      return $Either("Right", $Tuple(xcs._1._1, $2._1));
    }
    fail();
  };
  var recDefBwd = (dictBoundedJoinSemilattice) => (v) => (v1) => functorNonEmptyList.map((v2) => $Tuple(v._1, v2))(clausesBwd(dictBoundedJoinSemilattice)(v._2)(functorNonEmptyList.map(snd)(v1)));
  var pattsExprFwd = (dictJoinSemilattice) => (v) => {
    if (v._1._2.tag === "Nil") {
      return bindEither.bind((() => {
        const $2 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2);
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", $Cont("ContExpr", $2._1));
        }
        fail();
      })())(pattContFwd(v._1._1));
    }
    if (v._1._2.tag === "Cons") {
      return bindEither.bind((() => {
        const $2 = pattsExprFwd(dictJoinSemilattice)($Tuple($NonEmpty(v._1._2._1, v._1._2._2), v._2));
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", $Cont("ContExpr", $Expr("Lambda", $2._1)));
        }
        fail();
      })())(pattContFwd(v._1._1));
    }
    fail();
  };
  var pattsExprBwd = (dictBoundedJoinSemilattice) => (v) => (\u03C3) => {
    if (v._2.tag === "Nil") {
      return desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)((() => {
        const $3 = pattContBwd(v._1)(\u03C3);
        if ($3.tag === "ContExpr") {
          return $3._1;
        }
        return unsafePerformEffect(throwException(error("Expression expected")));
      })());
    }
    if (v._2.tag === "Cons") {
      const $3 = pattContBwd(v._1)(\u03C3);
      const $4 = (() => {
        if ($3.tag === "ContExpr") {
          return $3._1;
        }
        return unsafePerformEffect(throwException(error("Expression expected")));
      })();
      if ($4.tag === "Lambda") {
        return pattsExprBwd(dictBoundedJoinSemilattice)($NonEmpty(v._2._1, v._2._2))($4._1);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    fail();
  };
  var listRestFwd = (dictJoinSemilattice) => (v) => {
    if (v.tag === "End") {
      return $Either("Right", $Expr("Constr", v._1, "Nil", Nil));
    }
    if (v.tag === "Next") {
      return applyEither.apply((() => {
        const $2 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2);
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", econs(v._1)($2._1));
        }
        fail();
      })())(desugFwd$p(dictJoinSemilattice)(desugarableListRestExpr)(v._3));
    }
    fail();
  };
  var listRestBwd = (dictBoundedJoinSemilattice) => (v) => (v1) => {
    if (v.tag === "Constr") {
      if (v1.tag === "End") {
        return $ListRest("End", v._1);
      }
      if (v._3.tag === "Cons") {
        if (v._3._2.tag === "Cons") {
          if (v._3._2._2.tag === "Nil") {
            if (v1.tag === "Next") {
              return $ListRest(
                "Next",
                v._1,
                desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._3._1),
                desugBwd$p(dictBoundedJoinSemilattice)(desugarableListRestExpr)(v._3._2._1)
              );
            }
            return unsafePerformEffect(throwException(error("absurd")));
          }
          return unsafePerformEffect(throwException(error("absurd")));
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    return unsafePerformEffect(throwException(error("absurd")));
  };
  var listCompFwd = (dictJoinSemilattice) => (v) => {
    if (v._2._1.tag === "Nil") {
      const $2 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2._2);
      if ($2.tag === "Left") {
        return $Either("Left", $2._1);
      }
      if ($2.tag === "Right") {
        return $Either(
          "Right",
          $Expr(
            "Constr",
            v._1,
            ":",
            $List("Cons", $2._1, $List("Cons", $Expr("Constr", v._1, "Nil", Nil), Nil))
          )
        );
      }
      fail();
    }
    if (v._2._1.tag === "Cons") {
      if (v._2._1._1.tag === "Guard") {
        return bindEither.bind(listCompFwd(dictJoinSemilattice)($Tuple(v._1, $Tuple(v._2._1._2, v._2._2))))((e) => {
          const $3 = App2($Expr(
            "Lambda",
            $Elim(
              "ElimConstr",
              fromFoldable23([
                $Tuple("True", $Cont("ContExpr", e)),
                $Tuple("False", $Cont("ContExpr", $Expr("Constr", v._1, "Nil", Nil)))
              ])
            )
          ));
          const $4 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2._1._1._1);
          if ($4.tag === "Left") {
            return $Either("Left", $4._1);
          }
          if ($4.tag === "Right") {
            return $Either("Right", $3($4._1));
          }
          fail();
        });
      }
      if (v._2._1._1.tag === "Declaration") {
        return bindEither.bind((() => {
          const $2 = listCompFwd(dictJoinSemilattice)($Tuple(v._1, $Tuple(v._2._1._2, v._2._2)));
          if ($2.tag === "Left") {
            return $Either("Left", $2._1);
          }
          if ($2.tag === "Right") {
            return $Either("Right", $Cont("ContExpr", $2._1));
          }
          fail();
        })())((e) => bindEither.bind(pattContFwd(v._2._1._1._1._1)(e))((\u03C3) => {
          const $4 = App2($Expr("Lambda", \u03C3));
          const $5 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2._1._1._1._2);
          if ($5.tag === "Left") {
            return $Either("Left", $5._1);
          }
          if ($5.tag === "Right") {
            return $Either("Right", $4($5._1));
          }
          fail();
        }));
      }
      if (v._2._1._1.tag === "Generator") {
        return bindEither.bind((() => {
          const $2 = listCompFwd(dictJoinSemilattice)($Tuple(v._1, $Tuple(v._2._1._2, v._2._2)));
          if ($2.tag === "Left") {
            return $Either("Left", $2._1);
          }
          if ($2.tag === "Right") {
            return $Either("Right", $Cont("ContExpr", $2._1));
          }
          fail();
        })())((e) => bindEither.bind(pattContFwd(v._2._1._1._1)(e))((\u03C3) => {
          const $4 = App2($Expr(
            "App",
            $Expr("Var", "concatMap"),
            $Expr(
              "Lambda",
              (() => {
                const $42 = orElseFwd($Cont("ContElim", \u03C3))(v._1);
                if ($42.tag === "ContElim") {
                  return $42._1;
                }
                return unsafePerformEffect(throwException(error("Eliminator expected")));
              })()
            )
          ));
          const $5 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2._1._1._2);
          if ($5.tag === "Left") {
            return $Either("Left", $5._1);
          }
          if ($5.tag === "Right") {
            return $Either("Right", $4($5._1));
          }
          fail();
        }));
      }
      fail();
    }
    fail();
  };
  var listCompBwd = (dictBoundedJoinSemilattice) => {
    const join2 = dictBoundedJoinSemilattice.JoinSemilattice0().join;
    const orElseBwd1 = orElseBwd(dictBoundedJoinSemilattice);
    return (v) => (v1) => {
      const $5 = (e, qs, s, \u03C0, \u03C3) => {
        const v2 = listCompBwd(dictBoundedJoinSemilattice)((() => {
          const $10 = pattContBwd(\u03C0)(\u03C3);
          if ($10.tag === "ContExpr") {
            return $10._1;
          }
          return unsafePerformEffect(throwException(error("Expression expected")));
        })())($Tuple(qs, s));
        return $Tuple(
          v2._1,
          $Tuple(
            $List("Cons", $Qualifier("Declaration", $VarDef3(\u03C0, desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(e))), v2._2._1),
            v2._2._2
          )
        );
      };
      if (v.tag === "Constr") {
        if (v._3.tag === "Cons") {
          if (v._3._2.tag === "Cons") {
            if (v._3._2._1.tag === "Constr") {
              if (v._3._2._1._3.tag === "Nil") {
                if (v._3._2._2.tag === "Nil") {
                  if (v1._1.tag === "Nil") {
                    if (v._2 === ":" && v._3._2._1._2 === "Nil") {
                      return $Tuple(
                        join2(v._3._2._1._1)(v._1),
                        $Tuple(Nil, desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._3._1))
                      );
                    }
                    return unsafePerformEffect(throwException(error("absurd")));
                  }
                  return unsafePerformEffect(throwException(error("absurd")));
                }
                return unsafePerformEffect(throwException(error("absurd")));
              }
              return unsafePerformEffect(throwException(error("absurd")));
            }
            return unsafePerformEffect(throwException(error("absurd")));
          }
          return unsafePerformEffect(throwException(error("absurd")));
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v.tag === "App") {
        if (v1._1.tag === "Cons") {
          if (v._1.tag === "Lambda") {
            if (v._1._1.tag === "ElimConstr") {
              if (v1._1._1.tag === "Guard") {
                const $6 = listCompBwd(dictBoundedJoinSemilattice)((() => {
                  const $62 = $$get("True")(v._1._1._1);
                  if ($62.tag === "ContExpr") {
                    return $62._1;
                  }
                  return unsafePerformEffect(throwException(error("Expression expected")));
                })())($Tuple(v1._1._2, v1._2));
                const $7 = $$get("False")(v._1._1._1);
                const $8 = (() => {
                  if ($7.tag === "ContExpr") {
                    return $7._1;
                  }
                  return unsafePerformEffect(throwException(error("Expression expected")));
                })();
                if ($8.tag === "Constr") {
                  if ($8._3.tag === "Nil") {
                    if ($8._2 === "Nil") {
                      return $Tuple(
                        join2($6._1)($8._1),
                        $Tuple(
                          $List("Cons", $Qualifier("Guard", desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._2)), $6._2._1),
                          $6._2._2
                        )
                      );
                    }
                    return unsafePerformEffect(throwException(error("absurd")));
                  }
                  return unsafePerformEffect(throwException(error("absurd")));
                }
                return unsafePerformEffect(throwException(error("absurd")));
              }
              if (v1._1._1.tag === "Declaration") {
                return $5(v._2, v1._1._2, v1._2, v1._1._1._1._1, v._1._1);
              }
              return unsafePerformEffect(throwException(error("absurd")));
            }
            if (v1._1._1.tag === "Declaration") {
              return $5(v._2, v1._1._2, v1._2, v1._1._1._1._1, v._1._1);
            }
            return unsafePerformEffect(throwException(error("absurd")));
          }
          if (v._1.tag === "App") {
            if (v._1._1.tag === "Var") {
              if (v._1._1._1 === "concatMap") {
                if (v._1._2.tag === "Lambda") {
                  if (v1._1._1.tag === "Generator") {
                    const v2 = orElseBwd1($Cont("ContElim", v._1._2._1))($List("Cons", $Either("Left", v1._1._1._1), Nil));
                    const v3 = listCompBwd(dictBoundedJoinSemilattice)((() => {
                      const $7 = pattContBwd(v1._1._1._1)((() => {
                        if (v2._1.tag === "ContElim") {
                          return v2._1._1;
                        }
                        return unsafePerformEffect(throwException(error("Eliminator expected")));
                      })());
                      if ($7.tag === "ContExpr") {
                        return $7._1;
                      }
                      return unsafePerformEffect(throwException(error("Expression expected")));
                    })())($Tuple(v1._1._2, v1._2));
                    return $Tuple(
                      join2(v3._1)(v2._2),
                      $Tuple(
                        $List("Cons", $Qualifier("Generator", v1._1._1._1, desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._2)), v3._2._1),
                        v3._2._2
                      )
                    );
                  }
                  return unsafePerformEffect(throwException(error("absurd")));
                }
                return unsafePerformEffect(throwException(error("absurd")));
              }
              return unsafePerformEffect(throwException(error("absurd")));
            }
            return unsafePerformEffect(throwException(error("absurd")));
          }
          return unsafePerformEffect(throwException(error("absurd")));
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      return unsafePerformEffect(throwException(error("absurd")));
    };
  };
  var exprFwd = (dictJoinSemilattice) => (v) => {
    if (v.tag === "Var") {
      return $Either("Right", $Expr("Var", v._1));
    }
    if (v.tag === "Op") {
      return $Either("Right", $Expr("Op", v._1));
    }
    if (v.tag === "Int") {
      return $Either("Right", $Expr("Int", v._1, v._2));
    }
    if (v.tag === "Float") {
      return $Either("Right", $Expr("Float", v._1, v._2));
    }
    if (v.tag === "Str") {
      return $Either("Right", $Expr("Str", v._1, v._2));
    }
    if (v.tag === "Constr") {
      const $2 = Constr(v._1)(v._2);
      const $3 = traverse12(desugFwd$p(dictJoinSemilattice)(desugarableExprExpr))(v._3);
      if ($3.tag === "Left") {
        return $Either("Left", $3._1);
      }
      if ($3.tag === "Right") {
        return $Either("Right", $2($3._1));
      }
      fail();
    }
    if (v.tag === "Record") {
      const $2 = Record(v._1);
      const $3 = traverse12(traverse22(desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)))(v._2);
      if ($3.tag === "Left") {
        return $Either("Left", $3._1);
      }
      if ($3.tag === "Right") {
        return $Either("Right", $2(fromFoldable15($3._1)));
      }
      fail();
    }
    if (v.tag === "Dictionary") {
      const $2 = Dictionary(v._1);
      const $3 = traverse12(traverse32(desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)))(v._2);
      if ($3.tag === "Left") {
        return $Either("Left", $3._1);
      }
      if ($3.tag === "Right") {
        return $Either("Right", $2($3._1));
      }
      fail();
    }
    if (v.tag === "Matrix") {
      return applyEither.apply((() => {
        const $2 = Matrix(v._1);
        const $3 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2);
        if ($3.tag === "Left") {
          return $Either("Left", $3._1);
        }
        if ($3.tag === "Right") {
          return $Either("Right", $2($3._1)($Tuple(v._3._1, v._3._2)));
        }
        fail();
      })())(desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._4));
    }
    if (v.tag === "Lambda") {
      const $2 = clausesFwd(dictJoinSemilattice)(v._1);
      if ($2.tag === "Left") {
        return $Either("Left", $2._1);
      }
      if ($2.tag === "Right") {
        return $Either("Right", $Expr("Lambda", $2._1));
      }
      fail();
    }
    if (v.tag === "Project") {
      const $2 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._1);
      if ($2.tag === "Left") {
        return $Either("Left", $2._1);
      }
      if ($2.tag === "Right") {
        return $Either("Right", $Expr("Project", $2._1, v._2));
      }
      fail();
    }
    if (v.tag === "App") {
      return applyEither.apply((() => {
        const $2 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._1);
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", App2($2._1));
        }
        fail();
      })())(desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2));
    }
    if (v.tag === "BinaryApp") {
      return applyEither.apply((() => {
        const $2 = App2($Expr("Op", v._2));
        const $3 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._1);
        if ($3.tag === "Left") {
          return $Either("Left", $3._1);
        }
        if ($3.tag === "Right") {
          return $Either("Right", App2($2($3._1)));
        }
        fail();
      })())(desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._3));
    }
    if (v.tag === "MatchAs") {
      return applyEither.apply((() => {
        const $2 = clausesFwd(dictJoinSemilattice)(functorNonEmptyList.map((x2) => $Tuple($NonEmpty(x2._1, Nil), x2._2))(v._2));
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", App2($Expr("Lambda", $2._1)));
        }
        fail();
      })())(desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._1));
    }
    if (v.tag === "IfElse") {
      return applyEither.apply((() => {
        const $2 = applyEither.apply((() => {
          const $22 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2);
          if ($22.tag === "Left") {
            return $Either("Left", $22._1);
          }
          if ($22.tag === "Right") {
            return $Either("Right", elimBool($Cont("ContExpr", $22._1)));
          }
          fail();
        })())((() => {
          const $22 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._3);
          if ($22.tag === "Left") {
            return $Either("Left", $22._1);
          }
          if ($22.tag === "Right") {
            return $Either("Right", $Cont("ContExpr", $22._1));
          }
          fail();
        })());
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", App2($Expr("Lambda", $2._1)));
        }
        fail();
      })())(desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._1));
    }
    if (v.tag === "ListEmpty") {
      return $Either("Right", $Expr("Constr", v._1, "Nil", Nil));
    }
    if (v.tag === "ListNonEmpty") {
      return applyEither.apply((() => {
        const $2 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2);
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", econs(v._1)($2._1));
        }
        fail();
      })())(desugFwd$p(dictJoinSemilattice)(desugarableListRestExpr)(v._3));
    }
    if (v.tag === "ListEnum") {
      return applyEither.apply((() => {
        const $2 = App2($Expr("Var", "enumFromTo"));
        const $3 = desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._1);
        if ($3.tag === "Left") {
          return $Either("Left", $3._1);
        }
        if ($3.tag === "Right") {
          return $Either("Right", App2($2($3._1)));
        }
        fail();
      })())(desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2));
    }
    if (v.tag === "ListComp") {
      return listCompFwd(dictJoinSemilattice)($Tuple(v._1, $Tuple(v._3, v._2)));
    }
    if (v.tag === "Let") {
      return varDefsFwd(dictJoinSemilattice)($Tuple(v._1, v._2));
    }
    if (v.tag === "LetRec") {
      return applyEither.apply((() => {
        const $2 = recDefsFwd(dictJoinSemilattice)(v._1);
        if ($2.tag === "Left") {
          return $Either("Left", $2._1);
        }
        if ($2.tag === "Right") {
          return $Either("Right", LetRec($2._1));
        }
        fail();
      })())(desugFwd$p(dictJoinSemilattice)(desugarableExprExpr)(v._2));
    }
    fail();
  };
  var exprBwd = (dictBoundedJoinSemilattice) => (v) => (v1) => {
    const $3 = (e, qs, s) => {
      const v2 = listCompBwd(dictBoundedJoinSemilattice)(e)($Tuple(qs, s));
      return $Expr2("ListComp", v2._1, v2._2._2, v2._2._1);
    };
    if (v.tag === "Var") {
      if (v1.tag === "Var") {
        return $Expr2("Var", v1._1);
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "Op") {
      if (v1.tag === "Op") {
        return $Expr2("Op", v1._1);
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "Int") {
      if (v1.tag === "Int") {
        return $Expr2("Int", v._1, v1._2);
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "Float") {
      if (v1.tag === "Float") {
        return $Expr2("Float", v._1, v1._2);
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "Str") {
      if (v1.tag === "Str") {
        return $Expr2("Str", v._1, v1._2);
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "Constr") {
      if (v1.tag === "Constr") {
        return $Expr2("Constr", v._1, v1._2, listMap(desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr))(v._3));
      }
      if (v._3.tag === "Nil") {
        if (v1.tag === "ListEmpty") {
          return $Expr2("ListEmpty", v._1);
        }
        if (v1.tag === "ListComp") {
          return $3(v, v1._3, v1._2);
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v._3.tag === "Cons") {
        if (v._3._2.tag === "Cons") {
          if (v._3._2._2.tag === "Nil") {
            if (v1.tag === "ListNonEmpty") {
              return $Expr2(
                "ListNonEmpty",
                v._1,
                desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._3._1),
                desugBwd$p(dictBoundedJoinSemilattice)(desugarableListRestExpr)(v._3._2._1)
              );
            }
            if (v1.tag === "ListComp") {
              return $3(v, v1._3, v1._2);
            }
            return unsafePerformEffect(throwException(error("absurd")));
          }
          if (v1.tag === "ListComp") {
            return $3(v, v1._3, v1._2);
          }
          return unsafePerformEffect(throwException(error("absurd")));
        }
        if (v1.tag === "ListComp") {
          return $3(v, v1._3, v1._2);
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "Record") {
      if (v1.tag === "Record") {
        return $Expr2(
          "Record",
          v._1,
          listMap((v2) => $Tuple(v2._1, desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)($$get(v2._1)(v._2))))(v1._2)
        );
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "Dictionary") {
      if (v1.tag === "Dictionary") {
        return $Expr2(
          "Dictionary",
          v._1,
          zipWith((v2) => (v3) => $Pair(
            desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v2._1),
            desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v2._2)
          ))(v._2)(v1._2)
        );
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "Matrix") {
      if (v1.tag === "Matrix") {
        return $Expr2(
          "Matrix",
          v._1,
          desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._2),
          $Tuple(v1._3._1, v1._3._2),
          desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._4)
        );
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "Lambda") {
      if (v1.tag === "Lambda") {
        return $Expr2("Lambda", clausesBwd(dictBoundedJoinSemilattice)(v._1)(v1._1));
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "Project") {
      if (v1.tag === "Project") {
        return $Expr2("Project", desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._1), v1._2);
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "App") {
      if (v1.tag === "App") {
        return $Expr2(
          "App",
          desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._1),
          desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._2)
        );
      }
      if (v._1.tag === "Lambda") {
        if (v1.tag === "MatchAs") {
          return $Expr2(
            "MatchAs",
            desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._2),
            functorNonEmptyList.map((x2) => $Tuple(x2._1._1, x2._2))(clausesBwd(dictBoundedJoinSemilattice)(v._1._1)(functorNonEmptyList.map((x2) => $Tuple(
              $NonEmpty(x2._1, Nil),
              x2._2
            ))(v1._2)))
          );
        }
        if (v._1._1.tag === "ElimConstr") {
          if (v1.tag === "IfElse") {
            return $Expr2(
              "IfElse",
              desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._2),
              desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)((() => {
                const $4 = $$get("True")(v._1._1._1);
                if ($4.tag === "ContExpr") {
                  return $4._1;
                }
                return unsafePerformEffect(throwException(error("Expression expected")));
              })()),
              desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)((() => {
                const $4 = $$get("False")(v._1._1._1);
                if ($4.tag === "ContExpr") {
                  return $4._1;
                }
                return unsafePerformEffect(throwException(error("Expression expected")));
              })())
            );
          }
          if (v1.tag === "ListComp") {
            return $3(v, v1._3, v1._2);
          }
          return unsafePerformEffect(throwException(error("absurd")));
        }
        if (v1.tag === "ListComp") {
          return $3(v, v1._3, v1._2);
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v._1.tag === "App") {
        if (v._1._1.tag === "Op") {
          if (v1.tag === "BinaryApp") {
            return $Expr2(
              "BinaryApp",
              desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._1._2),
              v1._2,
              desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._2)
            );
          }
          if (v1.tag === "ListComp") {
            return $3(v, v1._3, v1._2);
          }
          return unsafePerformEffect(throwException(error("absurd")));
        }
        if (v._1._1.tag === "Var") {
          if (v._1._1._1 === "enumFromTo") {
            if (v1.tag === "ListEnum") {
              return $Expr2(
                "ListEnum",
                desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._1._2),
                desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._2)
              );
            }
            if (v1.tag === "ListComp") {
              return $3(v, v1._3, v1._2);
            }
            return unsafePerformEffect(throwException(error("absurd")));
          }
          if (v1.tag === "ListComp") {
            return $3(v, v1._3, v1._2);
          }
          return unsafePerformEffect(throwException(error("absurd")));
        }
        if (v1.tag === "ListComp") {
          return $3(v, v1._3, v1._2);
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "Let") {
      if (v1.tag === "Let") {
        const $4 = varDefsBwd(dictBoundedJoinSemilattice)($Expr("Let", v._1, v._2))($Tuple(v1._1, v1._2));
        return $Expr2("Let", $4._1, $4._2);
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v.tag === "LetRec") {
      if (v1.tag === "LetRec") {
        return $Expr2("LetRec", recDefsBwd(dictBoundedJoinSemilattice)(v._1)(v1._1), desugBwd$p(dictBoundedJoinSemilattice)(desugarableExprExpr)(v._2));
      }
      if (v1.tag === "ListComp") {
        return $3(v, v1._3, v1._2);
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v1.tag === "ListComp") {
      return $3(v, v1._3, v1._2);
    }
    return unsafePerformEffect(throwException(error("absurd")));
  };
  var clausesFwd = (dictJoinSemilattice) => {
    const maybeJoin = joinSemilatticeElim(dictJoinSemilattice).maybeJoin;
    return (v) => bindEither.bind(traverse4(pattsExprFwd(dictJoinSemilattice))(functorNonEmptyList.map(unsafeCoerce)(v)))((v1) => foldM4(maybeJoin)(v1._1)(v1._2));
  };
  var clausesBwd = (dictBoundedJoinSemilattice) => (\u03C3) => (v) => functorNonEmptyList.map((v1) => $Tuple(
    v1._1,
    pattsExprBwd(dictBoundedJoinSemilattice)(v1._1)(\u03C3)
  ))(v);
  var moduleFwd = (dictJoinSemilattice) => (v) => {
    const $2 = traverse12((v1) => {
      if (v1.tag === "Left") {
        const $3 = varDefFwd(dictJoinSemilattice)(v1._1);
        if ($3.tag === "Left") {
          return $Either("Left", $3._1);
        }
        if ($3.tag === "Right") {
          return $Either("Right", $Either("Left", $3._1));
        }
        fail();
      }
      if (v1.tag === "Right") {
        const $3 = recDefsFwd(dictJoinSemilattice)(v1._1);
        if ($3.tag === "Left") {
          return $Either("Left", $3._1);
        }
        if ($3.tag === "Right") {
          return $Either("Right", $Either("Right", $3._1));
        }
        fail();
      }
      fail();
    })(bindList.bind(listMap((v1) => {
      if (v1.tag === "Left") {
        return listMap(Left)($List("Cons", v1._1._1, v1._1._2));
      }
      if (v1.tag === "Right") {
        return $List("Cons", $Either("Right", v1._1), Nil);
      }
      fail();
    })(v._1))(identity2));
    if ($2.tag === "Left") {
      return $Either("Left", $2._1);
    }
    if ($2.tag === "Right") {
      return $Either("Right", $Module($2._1));
    }
    fail();
  };
  var desugarModuleFwd = (dictJoinSemilattice) => moduleFwd(dictJoinSemilattice);

  // output-es/Util.Parse/index.js
  var some1 = /* @__PURE__ */ some(alternativeParserT)(lazyParserT);
  var many4 = /* @__PURE__ */ many(alternativeParserT)(lazyParserT);
  var some4 = (p) => {
    const $1 = some1(p);
    return (state1, more, lift1, $$throw, done) => more((v1) => $1(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => done(
        state2,
        definitely("absurd")((() => {
          if (a.tag === "Nil") {
            return Nothing;
          }
          if (a.tag === "Cons") {
            return $Maybe("Just", $NonEmpty(a._1, a._2));
          }
          fail();
        })())
      ))
    ));
  };
  var sepBy1_try = (p) => (sep) => {
    const $2 = many4($$try4((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => sep(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => p(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
    )))));
    return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => p(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => $2(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, $NonEmpty(a, a$1))))))
    )));
  };
  var sepBy_try = (p) => (sep) => altParserT.alt((() => {
    const $2 = sepBy1_try(p)(sep);
    return (state1, more, lift1, $$throw, done) => more((v1) => $2(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, $List("Cons", a._1, a._2)))));
  })())((state1, v, v1, v2, done) => done(state1, Nil));

  // output-es/Parse/index.js
  var fromFoldable9 = /* @__PURE__ */ (() => fromFoldableImpl(foldableList.foldr))();
  var fromFoldable16 = /* @__PURE__ */ (() => fromFoldableImpl(foldableNonEmptyList.foldr))();
  var choose2 = /* @__PURE__ */ choose(altParserT);
  var identity20 = (x2) => x2;
  var fanin3 = /* @__PURE__ */ fanin(categoryFn)(choiceFn);
  var operators = (binaryOp) => fromFoldable9(listMap(arrayMap((v) => $Operator("Infix", $$try4(binaryOp(v.op)), v.assoc)))(listMap(fromFoldable16)(groupBy((x2) => (y2) => x2.prec === y2.prec)(sortBy((x2) => (x$1) => {
    const $3 = ordInt.compare(x2.prec)(x$1.prec);
    if ($3.tag === "GT") {
      return LT;
    }
    if ($3.tag === "EQ") {
      return EQ;
    }
    if ($3.tag === "LT") {
      return GT;
    }
    fail();
  })(foldableMap.foldr(Cons)(Nil)(opDefs))))));
  var languageDef = /* @__PURE__ */ (() => {
    const opChar = oneOf([":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~"]);
    return {
      commentStart: "{-",
      commentEnd: "-}",
      commentLine: "--",
      nestedComments: true,
      identStart: altParserT.alt(withErrorMessage(satisfyCP(isAlpha))("letter"))($$char("_")),
      identLetter: altParserT.alt(withErrorMessage(satisfyCP(isAlphaNum))("letter or digit"))(oneOf([
        "_",
        "'"
      ])),
      opStart: opChar,
      opLetter: opChar,
      reservedNames: ["as", "else", "fun", "if", "in", "let", "match", "then"],
      reservedOpNames: ["|", "..", "=", "<-", "->"],
      caseSensitive: true
    };
  })();
  var token = /* @__PURE__ */ makeTokenParser(languageDef);
  var rArrow = /* @__PURE__ */ (() => token.reservedOp("->"))();
  var rBracket = /* @__PURE__ */ (() => {
    const $0 = token.symbol("]");
    return (state1, more, lift1, $$throw, done) => more((v1) => $0(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, unit))));
  })();
  var topLevel = (p) => (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => more((v2$1) => more((v1$1) => token.whiteSpace(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2$2) => more((v3) => p(
      state2,
      more,
      lift1,
      $$throw,
      (state3, a$1) => more((v4) => more((v2$3) => more((v3$1) => eof(state3, more, lift1, $$throw, (state3$1, a$2) => more((v4$1) => done(state3$1, a$1))))))
    )))
  )))));
  var lBracket = /* @__PURE__ */ (() => {
    const $0 = token.symbol("[");
    return (state1, more, lift1, $$throw, done) => more((v1) => $0(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, unit))));
  })();
  var lArrow = /* @__PURE__ */ (() => token.reservedOp("<-"))();
  var keyword = (str$p) => {
    if (elem(eqString)(str$p)(languageDef.reservedNames)) {
      return token.reserved(str$p);
    }
    return unsafePerformEffect(throwException(error(str$p + " is not a reserved word")));
  };
  var ident = (state1, more, lift1, $$throw, done) => more((v1) => token.identifier(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2) => {
      if (!isCtrName(a)) {
        return done(state2, a);
      }
      return fail3("No alternative")(state2, more, lift1, $$throw, done);
    })
  ));
  var field = (p) => (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => ident(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2$1) => {
      const $11 = Tuple(a);
      return more((v3) => more((v2$2) => more((v1$1) => token.colon(
        state2,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more((v2$3) => more((v3$1) => p(state2$1, more, lift1, $$throw, (state3, a$2) => more((v4) => more((v4$1) => done(state3, $11(a$2)))))))
      ))));
    })
  )));
  var equals = /* @__PURE__ */ (() => token.reservedOp("="))();
  var ellipsis = /* @__PURE__ */ (() => token.reservedOp(".."))();
  var ctr = (state1, more, lift1, $$throw, done) => more((v1) => token.identifier(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2) => {
      if (isCtrName(a)) {
        return done(state2, a);
      }
      return fail3("No alternative")(state2, more, lift1, $$throw, done);
    })
  ));
  var simplePattern = (pattern$p) => altParserT.alt($$try4(token.brackets((state1, v, v1, v2, done) => done(state1, PListEmpty))))(altParserT.alt((() => {
    const $1 = (() => {
      const go$lazy = binding(() => lazyParserT.defer((v) => altParserT.alt((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => rBracket(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more((v2$1) => more((v3) => more((v4) => done(state2, PEnd))))
      ))))((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => token.comma(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more((v2$1) => more((v3) => more((v2$2) => more((v1$1) => pattern$p(
          state2,
          more,
          lift1,
          $$throw,
          (state2$1, a$1) => more((v2$3) => {
            const $19 = PNext(a$1);
            return more((v3$1) => go$lazy()(
              state2$1,
              more,
              lift1,
              $$throw,
              (state3, a$2) => more((v4) => {
                const $24 = $19(a$2);
                return more((v4$1) => done(state3, $24));
              })
            ));
          })
        )))))
      ))))));
      const go = go$lazy();
      return go;
    })();
    return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => lBracket(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => more((v2$2) => more((v1$1) => pattern$p(
        state2,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more((v2$3) => {
          const $18 = PListNonEmpty(a$1);
          return more((v3$1) => $1(
            state2$1,
            more,
            lift1,
            $$throw,
            (state3, a$2) => more((v4) => {
              const $23 = $18(a$2);
              return more((v4$1) => done(state3, $23));
            })
          ));
        })
      )))))
    )));
  })())(altParserT.alt($$try4((state1, more, lift1, $$throw, done) => more((v1) => more((v1$1) => ctr(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2) => {
      const $11 = PConstr(a);
      return more((v2$1) => done(state2, $11(Nil)));
    })
  )))))(altParserT.alt($$try4(token.braces((state1, more, lift1, $$throw, done) => more((v1) => sepBy(field(pattern$p))(token.comma)(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2) => done(state2, $Pattern("PRecord", a)))
  )))))(altParserT.alt($$try4((state1, more, lift1, $$throw, done) => more((v1) => ident(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2) => done(state2, $Pattern("PVar", a)))
  ))))(altParserT.alt($$try4(token.parens(pattern$p)))(token.parens((state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => pattern$p(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2$1) => more((v3) => token.comma(
      state2,
      more,
      lift1,
      $$throw,
      (state3, a$1) => more((v4) => more((v2$2) => more((v1$2) => pattern$p(
        state3,
        more,
        lift1,
        $$throw,
        (state2$1, a$2) => more((v2$3) => done(
          state2$1,
          $Pattern("PConstr", "Pair", $List("Cons", a, $List("Cons", a$2, Nil)))
        ))
      ))))
    )))
  )))))))))));
  var pattern = /* @__PURE__ */ (() => {
    const $0 = buildExprParser(operators((op) => (state1, more, lift1, $$throw, done) => more((v1) => token.operator(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => (() => {
        if (":" === definitely("absurd")(charAt2(0)(a)) && op === a) {
          return applicativeParserT.pure;
        }
        return (v) => fail3("No alternative");
      })()((\u03C0) => (\u03C0$p) => $Pattern("PConstr", a, $List("Cons", \u03C0, $List("Cons", \u03C0$p, Nil))))(
        state2,
        more,
        lift1,
        $$throw,
        done
      ))
    ))));
    const go$lazy = binding(() => lazyParserT.defer((v) => $0((() => {
      const rest = (v$1) => {
        if (v$1.tag === "PConstr") {
          return altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => simplePattern(go$lazy())(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2) => rest($Pattern(
              "PConstr",
              v$1._1,
              foldableList.foldr(Cons)($List("Cons", a, Nil))(v$1._2)
            ))(state2, more, lift1, $$throw, done))
          )))((state1, v$2, v1, v2, done) => done(state1, v$1));
        }
        return (state1, v$2, v1, v2, done) => done(state1, v$1);
      };
      return (state1, more, lift1, $$throw, done) => more((v1) => simplePattern(go$lazy())(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more((v2) => rest(a)(state2, more, lift1, $$throw, done))
      ));
    })())));
    const go = go$lazy();
    return go;
  })();
  var varDefs = (expr$p) => {
    const $1 = keyword("let");
    const $2 = sepBy1_try((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => more((v2$1) => more((v1$1) => pattern(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$2) => more((v3) => equals(
        state2,
        more,
        lift1,
        $$throw,
        (state3, a$1) => more((v4) => more((v2$3) => {
          const $19 = VarDef2(a);
          return more((v3$1) => expr$p(state3, more, lift1, $$throw, (state3$1, a$2) => more((v4$1) => done(state3$1, $19(a$2)))));
        }))
      )))
    ))))))(token.semi);
    return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $1(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => $2(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
    )));
  };
  var colonEq = /* @__PURE__ */ (() => token.reservedOp(":="))();
  var clause_uncurried = (expr$p) => (delim) => (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => pattern(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2$1) => {
      const $12 = Tuple(a);
      return more((v3) => more((v2$2) => more((v1$1) => delim(
        state2,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more((v2$3) => more((v3$1) => expr$p(state2$1, more, lift1, $$throw, (state3, a$2) => more((v4) => more((v4$1) => done(state3, $12(a$2)))))))
      ))));
    })
  )));
  var clause_curried = (expr$p) => (delim) => {
    const $2 = some4(simplePattern(pattern));
    return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => $2(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => {
        const $14 = Tuple(a);
        return more((v3) => more((v2$2) => more((v1$2) => delim(
          state2,
          more,
          lift1,
          $$throw,
          (state2$1, a$1) => more((v2$3) => more((v3$1) => expr$p(
            state2$1,
            more,
            lift1,
            $$throw,
            (state3, a$2) => more((v4) => more((v4$1) => {
              const $26 = $14(a$2);
              return more((v2$4) => done(state3, $26));
            }))
          )))
        ))));
      })
    ))));
  };
  var recDefs = (expr$p) => {
    const $1 = keyword("let");
    const $2 = sepBy1_try((() => {
      const $22 = clause_curried(expr$p)(equals);
      return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => ident(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more((v2$1) => {
          const $13 = Tuple(a);
          return more((v3) => $22(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, $13(a$1)))));
        })
      )));
    })())(token.semi);
    return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $1(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2$1) => more((v3) => $2(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a$1)))))
    )));
  };
  var defs = (expr$p) => {
    const $1 = choose2($$try4(varDefs(expr$p)))(recDefs(expr$p));
    return (state1, more, lift1, $$throw, done) => more((v1) => $1(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => done(state2, $List("Cons", a, Nil)))
    ));
  };
  var branches = (expr$p) => (branch_) => altParserT.alt((() => {
    const $2 = branch_(expr$p)(altParserT.alt(rArrow)(equals));
    return (state1, more, lift1, $$throw, done) => more((v1) => $2(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => done(state2, $NonEmpty(a, Nil)))
    ));
  })())(token.braces(sepBy1(branch_(expr$p)(rArrow))(token.semi)));
  var bar = /* @__PURE__ */ (() => token.reservedOp("|"))();
  var backtick = /* @__PURE__ */ (() => {
    const $0 = token.symbol("`");
    return (state1, more, lift1, $$throw, done) => more((v1) => $0(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, unit))));
  })();
  var expr_ = /* @__PURE__ */ (() => {
    const $0 = buildExprParser(concatArray([
      [
        $Operator(
          "Infix",
          (state1, more, lift1, $$throw, done) => more((v1) => between2(backtick)(backtick)(ident)(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2) => done(state2, (e) => (e$p) => $Expr2("BinaryApp", e, a, e$p)))
          )),
          AssocLeft
        )
      ]
    ])(operators((op) => (state1, more, lift1, $$throw, done) => more((v1) => token.operator(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => (() => {
        if (op === a) {
          return applicativeParserT.pure;
        }
        return (v) => fail3("No alternative");
      })()((() => {
        if (op === ".") {
          return (e) => (e$p) => {
            if (e$p.tag === "Var") {
              return $Expr2("Project", e, e$p._1);
            }
            return unsafePerformEffect(throwException(error("Field names are not first class.")));
          };
        }
        if (":" === definitely("absurd")(charAt2(0)(a))) {
          return (e) => (e$p) => $Expr2("Constr", unit, a, $List("Cons", e, $List("Cons", e$p, Nil)));
        }
        return (e) => (e$p) => $Expr2("BinaryApp", e, op, e$p);
      })())(state2, more, lift1, $$throw, done))
    )))));
    const go$lazy = binding(() => lazyParserT.defer((v) => $0((() => {
      const simpleExpr = altParserT.alt(between2(token.symbol("[|"))(token.symbol("|]"))((() => {
        const $3 = Matrix3(unit);
        const $4 = token.parens((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => ident(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => {
            const $14 = Tuple(a);
            return more((v3) => more((v2$2) => more((v1$1) => token.comma(
              state2,
              more,
              lift1,
              $$throw,
              (state2$1, a$1) => more((v2$3) => more((v3$1) => ident(state2$1, more, lift1, $$throw, (state3, a$2) => more((v4) => more((v4$1) => done(state3, $14(a$2)))))))
            ))));
          })
        ))));
        const $5 = keyword("in");
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v2$1) => more((v1) => more((v2$2) => more((v1$1) => go$lazy()(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$3) => more((v3) => bar(
            state2,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more((v4) => more((v2$4) => {
              const $24 = $3(a);
              return more((v3$1) => $4(
                state3,
                more,
                lift1,
                $$throw,
                (state3$1, a$2) => more((v4$1) => {
                  const $29 = $24(a$2);
                  return more((v3$2) => more((v2$5) => more((v1$2) => $5(
                    state3$1,
                    more,
                    lift1,
                    $$throw,
                    (state2$1, a$3) => more((v2$6) => more((v3$3) => go$lazy()(state2$1, more, lift1, $$throw, (state3$2, a$4) => more((v4$2) => more((v4$3) => done(state3$2, $29(a$4)))))))
                  ))));
                })
              ));
            }))
          )))
        ))))));
      })()))(altParserT.alt($$try4(token.brackets((state1, v$1, v1, v2, done) => done(state1, $Expr2("ListEmpty", unit)))))(altParserT.alt((() => {
        const $3 = ListNonEmpty(unit);
        const $4 = (() => {
          const go$1$lazy = binding(() => lazyParserT.defer((v$1) => altParserT.alt((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => rBracket(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2$1) => more((v3) => more((v4) => done(state2, $ListRest("End", unit)))))
          ))))((() => {
            const $6 = Next(unit);
            return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => token.comma(
              state1,
              more,
              lift1,
              $$throw,
              (state2, a) => more((v2$1) => more((v3) => more((v2$2) => more((v1$1) => go$lazy()(
                state2,
                more,
                lift1,
                $$throw,
                (state2$1, a$1) => more((v2$3) => {
                  const $23 = $6(a$1);
                  return more((v3$1) => go$1$lazy()(
                    state2$1,
                    more,
                    lift1,
                    $$throw,
                    (state3, a$2) => more((v4) => {
                      const $28 = $23(a$2);
                      return more((v4$1) => done(state3, $28));
                    })
                  ));
                })
              )))))
            )));
          })())));
          const go$1 = go$1$lazy();
          return go$1;
        })();
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => lBracket(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => more((v2$2) => more((v1$1) => go$lazy()(
            state2,
            more,
            lift1,
            $$throw,
            (state2$1, a$1) => more((v2$3) => {
              const $21 = $3(a$1);
              return more((v3$1) => $4(
                state2$1,
                more,
                lift1,
                $$throw,
                (state3, a$2) => more((v4) => {
                  const $26 = $21(a$2);
                  return more((v4$1) => done(state3, $26));
                })
              ));
            })
          )))))
        )));
      })())(altParserT.alt(token.brackets((() => {
        const $3 = ListComp(unit);
        const $4 = sepBy1(altParserT.alt((state1, more, lift1, $$throw, done) => more((v2) => more((v2$1) => more((v1) => more((v1$1) => pattern(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$2) => {
            const $16 = $$Generator(a);
            return more((v2$3) => more((v3) => lArrow(
              state2,
              more,
              lift1,
              $$throw,
              (state3, a$1) => more((v4) => more((v3$1) => go$lazy()(state3, more, lift1, $$throw, (state3$1, a$2) => more((v4$1) => done(state3$1, $16(a$2))))))
            )));
          })
        ))))))(altParserT.alt((() => {
          const $42 = keyword("let");
          return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => more((v2$1) => more((v1$2) => more((v2$2) => more((v1$3) => $42(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2$3) => more((v3) => pattern(
              state2,
              more,
              lift1,
              $$throw,
              (state3, a$1) => more((v4) => more((v2$4) => more((v3$1) => equals(
                state3,
                more,
                lift1,
                $$throw,
                (state3$1, a$2) => more((v4$1) => more((v2$5) => {
                  const $30 = VarDef2(a$1);
                  return more((v3$2) => go$lazy()(
                    state3$1,
                    more,
                    lift1,
                    $$throw,
                    (state3$2, a$3) => more((v4$2) => {
                      const $35 = $30(a$3);
                      return more((v2$6) => done(state3$2, $Qualifier("Declaration", $35)));
                    })
                  ));
                }))
              ))))
            )))
          ))))))));
        })())((state1, more, lift1, $$throw, done) => more((v1) => go$lazy()(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, $Qualifier("Guard", a))))))))(token.comma);
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v2$1) => more((v1) => more((v2$2) => more((v3) => go$lazy()(
          state1,
          more,
          lift1,
          $$throw,
          (state3, a) => more((v4) => {
            const $18 = $3(a);
            return more((v2$3) => more((v3$1) => bar(
              state3,
              more,
              lift1,
              $$throw,
              (state3$1, a$1) => more((v4$1) => more((v3$2) => more((v1$1) => $4(
                state3$1,
                more,
                lift1,
                $$throw,
                (state2, a$2) => more((v2$4) => more((v4$2) => done(state2, $18($List("Cons", a$2._1, a$2._2)))))
              ))))
            )));
          })
        ))))));
      })()))(altParserT.alt(token.brackets((state1, more, lift1, $$throw, done) => more((v2) => more((v2$1) => more((v1) => more((v2$2) => more((v3) => go$lazy()(
        state1,
        more,
        lift1,
        $$throw,
        (state3, a) => more((v4) => {
          const $16 = ListEnum(a);
          return more((v2$3) => more((v3$1) => ellipsis(
            state3,
            more,
            lift1,
            $$throw,
            (state3$1, a$1) => more((v4$1) => more((v3$2) => go$lazy()(state3$1, more, lift1, $$throw, (state3$2, a$2) => more((v4$2) => done(state3$2, $16(a$2))))))
          )));
        })
      ))))))))(altParserT.alt($$try4((() => {
        const $3 = Constr3(unit);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v1$1) => ctr(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2) => {
            const $14 = $3(a);
            return more((v2$1) => done(state2, $14(Nil)));
          })
        )));
      })()))(altParserT.alt(between2(token.symbol("{|"))(token.symbol("|}"))((() => {
        const $3 = sepBy((state1, more, lift1, $$throw, done) => more((v2) => more((v1) => more((v2$1) => more((v1$1) => go$lazy()(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$2) => more((v3) => colonEq(
            state2,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more((v4) => more((v2$3) => {
              const $20 = Pair(a);
              return more((v3$1) => go$lazy()(state3, more, lift1, $$throw, (state3$1, a$2) => more((v4$1) => done(state3$1, $20(a$2)))));
            }))
          )))
        ))))))(token.comma);
        const $4 = Dictionary3(unit);
        return (state1, more, lift1, $$throw, done) => more((v1) => $3(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, $4(a)))));
      })()))(altParserT.alt(token.braces((() => {
        const $3 = Record3(unit);
        return (state1, more, lift1, $$throw, done) => more((v1) => sepBy(field(go$lazy()))(token.comma)(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2) => done(state2, $3(a)))
        ));
      })()))(altParserT.alt($$try4((state1, more, lift1, $$throw, done) => more((v1) => ident(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more((v2) => done(state2, $Expr2("Var", a)))
      ))))(altParserT.alt($$try4((() => {
        const $3 = altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => $$char("-")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2) => done(state2, (a$1) => -a$1))
        )))(altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => $$char("+")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2) => done(state2, identity20))
        )))((state1, v$1, v1, v2, done) => done(state1, identity20)));
        return (state1, more, lift1, $$throw, done) => more((v1) => $3(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2) => {
            const $13 = Float3(unit);
            return more((v1$1) => token.float(state2, more, lift1, $$throw, (state2$1, a$1) => more((v2$1) => done(state2$1, $13(a(a$1))))));
          })
        ));
      })()))(altParserT.alt($$try4((() => {
        const $3 = altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => $$char("-")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2) => done(state2, (a$1) => -a$1))
        )))(altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => $$char("+")(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2) => done(state2, identity20))
        )))((state1, v$1, v1, v2, done) => done(state1, identity20)));
        return (state1, more, lift1, $$throw, done) => more((v1) => $3(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2) => {
            const $13 = Int3(unit);
            return more((v1$1) => token.natural(state2, more, lift1, $$throw, (state2$1, a$1) => more((v2$1) => done(state2$1, $13(a(a$1))))));
          })
        ));
      })()))(altParserT.alt((() => {
        const $3 = Str3(unit);
        return (state1, more, lift1, $$throw, done) => more((v1) => token.stringLiteral(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, $3(a)))));
      })())(altParserT.alt((() => {
        const $3 = sepBy1(defs(go$lazy()))(token.semi);
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v1$1) => $3(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2) => {
            const $14 = bindList.bind($List("Cons", a._1, a._2))(identity4);
            return more((v2$1) => {
              const $16 = foldableList.foldr((def) => fanin3(Let2)(LetRec2)(def));
              const $17 = keyword("in");
              return more((v1$2) => more((v1$3) => more((v2$2) => more((v1$4) => $17(
                state2,
                more,
                lift1,
                $$throw,
                (state2$1, a$1) => more((v2$3) => more((v3) => go$lazy()(
                  state2$1,
                  more,
                  lift1,
                  $$throw,
                  (state3, a$2) => more((v4) => more((v2$4) => {
                    const $30 = $16(a$2);
                    return more((v2$5) => done(state3, $30($14)));
                  }))
                )))
              )))));
            });
          })
        )));
      })())(altParserT.alt((() => {
        const $3 = keyword("match");
        const $4 = keyword("as");
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => more((v2$1) => more((v1$1) => more((v2$2) => more((v1$2) => $3(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$3) => more((v3) => go$lazy()(
            state2,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more((v4) => more((v2$4) => more((v3$1) => $4(
              state3,
              more,
              lift1,
              $$throw,
              (state3$1, a$2) => more((v4$1) => more((v2$5) => {
                const $29 = MatchAs(a$1);
                return more((v3$2) => branches(go$lazy())(clause_uncurried)(state3$1, more, lift1, $$throw, (state3$2, a$3) => more((v4$2) => done(state3$2, $29(a$3)))));
              }))
            ))))
          )))
        )))))));
      })())(altParserT.alt($$try4(token.parens(go$lazy())))(altParserT.alt($$try4((() => {
        const $3 = token.parens(token.operator);
        return (state1, more, lift1, $$throw, done) => more((v1) => $3(state1, more, lift1, $$throw, (state2, a) => more((v2) => done(state2, $Expr2("Op", a)))));
      })()))(altParserT.alt(token.parens((state1, more, lift1, $$throw, done) => more((v2) => more((v2$1) => more((v3) => more((v2$2) => more((v1) => go$lazy()(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more((v2$3) => more((v3$1) => token.comma(
          state2,
          more,
          lift1,
          $$throw,
          (state3, a$1) => more((v4) => more((v4$1) => more((v3$2) => go$lazy()(
            state3,
            more,
            lift1,
            $$throw,
            (state3$1, a$2) => more((v4$2) => done(
              state3$1,
              $Expr2("Constr", unit, "Pair", $List("Cons", a, $List("Cons", a$2, Nil)))
            ))
          ))))
        )))
      ))))))))(altParserT.alt((() => {
        const $3 = keyword("fun");
        return (state1, more, lift1, $$throw, done) => more((v1) => more((v2) => more((v1$1) => $3(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$1) => more((v3) => branches(go$lazy())(clause_curried)(
            state2,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more((v4) => more((v2$2) => done(state3, $Expr2("Lambda", a$1))))
          )))
        ))));
      })())((() => {
        const $3 = keyword("if");
        const $4 = keyword("then");
        const $5 = keyword("else");
        return (state1, more, lift1, $$throw, done) => more((v2) => more((v2$1) => more((v1) => more((v2$2) => more((v2$3) => more((v1$1) => more((v2$4) => more((v3) => more((v2$5) => more((v1$2) => $3(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more((v2$6) => more((v3$1) => go$lazy()(
            state2,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more((v4) => more((v4$1) => {
              const $29 = IfElse(a$1);
              return more((v2$7) => more((v3$2) => $4(
                state3,
                more,
                lift1,
                $$throw,
                (state3$1, a$2) => more((v4$2) => more((v3$3) => go$lazy()(
                  state3$1,
                  more,
                  lift1,
                  $$throw,
                  (state3$2, a$3) => more((v4$3) => {
                    const $39 = $29(a$3);
                    return more((v2$8) => more((v3$4) => $5(
                      state3$2,
                      more,
                      lift1,
                      $$throw,
                      (state3$3, a$4) => more((v4$4) => more((v3$5) => go$lazy()(state3$3, more, lift1, $$throw, (state3$4, a$5) => more((v4$5) => done(state3$4, $39(a$5))))))
                    )));
                  })
                )))
              )));
            }))
          )))
        )))))))))));
      })()))))))))))))))))));
      const rest = (v$1) => {
        if (v$1.tag === "Constr") {
          return altParserT.alt((state1, more, lift1, $$throw, done) => more((v1) => simpleExpr(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2) => rest($Expr2(
              "Constr",
              v$1._1,
              v$1._2,
              foldableList.foldr(Cons)($List("Cons", a, Nil))(v$1._3)
            ))(state2, more, lift1, $$throw, done))
          )))((state1, v$2, v1, v2, done) => done(state1, v$1));
        }
        return altParserT.alt((() => {
          const $6 = App3(v$1);
          return (state1, more, lift1, $$throw, done) => more((v1) => more((v1$1) => simpleExpr(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more((v2) => {
              const $17 = $6(a);
              return more((v2$1) => rest($17)(state2, more, lift1, $$throw, done));
            })
          )));
        })())((state1, v$2, v1, v2, done) => done(state1, v$1));
      };
      return (state1, more, lift1, $$throw, done) => more((v1) => simpleExpr(state1, more, lift1, $$throw, (state2, a) => more((v2) => rest(a)(state2, more, lift1, $$throw, done))));
    })())));
    const go = go$lazy();
    return go;
  })();
  var module_ = /* @__PURE__ */ (() => {
    const $0 = topLevel((() => {
      const $02 = sepBy_try(defs(expr_))(token.semi);
      return (state1, more, lift1, $$throw, done) => more((v2) => more((v1) => $02(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more((v2$1) => more((v3) => token.semi(state2, more, lift1, $$throw, (state3, a$1) => more((v4) => done(state3, a)))))
      )));
    })());
    return (state1, more, lift1, $$throw, done) => more((v1) => $0(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => done(state2, $Module2(bindList.bind(a)(identity4))))
    ));
  })();

  // output-es/Debug/foreign.js
  var req = typeof module === "undefined" ? void 0 : module.require;
  var util = function() {
    try {
      return req === void 0 ? void 0 : req("util");
    } catch (e) {
      return void 0;
    }
  }();
  function _trace(x2, k) {
    if (util !== void 0) {
      console.log(util.inspect(x2, { depth: null, colors: true }));
    } else {
      console.log(x2);
    }
    return k({});
  }
  var now2 = function() {
    var perf;
    if (typeof performance !== "undefined") {
      perf = performance;
    } else if (req) {
      try {
        perf = req("perf_hooks").performance;
      } catch (e) {
      }
    }
    return function() {
      return (perf || Date).now();
    };
  }();

  // output-es/Primitive.Defs/index.js
  var erase = /* @__PURE__ */ (() => functorVal.map((v) => unit))();
  var traverse5 = /* @__PURE__ */ (() => {
    const $0 = traversableWithIndexObject.traverseWithIndex(applicativeEither);
    return (x2) => $0((v) => x2);
  })();
  var sequence3 = /* @__PURE__ */ (() => traversableWithIndexObject.traverseWithIndex(applicativeEither)((v) => identity8))();
  var foldWithIndexM = (f) => (a0) => fold((b) => (a) => (b$1) => bindEither.bind(b)((() => {
    const $5 = f(a);
    return (a$1) => $5(a$1)(b$1);
  })()))($Either("Right", a0));
  var pow3 = /* @__PURE__ */ union3(asNumberIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber)((x2) => (y2) => pow(toNumber(x2))(toNumber(y2)))(pow);
  var numToStr = (v2) => {
    if (v2.tag === "Left") {
      return showIntImpl(v2._1);
    }
    if (v2.tag === "Right") {
      return showNumberImpl(v2._1);
    }
    fail();
  };
  var notEquals = /* @__PURE__ */ union3(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((x2) => (y2) => x2 !== y2)(/* @__PURE__ */ union3(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((x2) => (y2) => x2 !== y2)((x2) => (y2) => x2 !== y2));
  var matrixLookup = /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    op: (dictAnn) => (v) => {
      if (v.tag === "Cons") {
        if (v._1.tag === "Matrix") {
          if (v._2.tag === "Cons") {
            if (v._2._1.tag === "Constr") {
              if (v._2._1._3.tag === "Cons") {
                if (v._2._1._3._1.tag === "Int") {
                  if (v._2._1._3._2.tag === "Cons") {
                    if (v._2._1._3._2._1.tag === "Int") {
                      if (v._2._1._3._2._2.tag === "Nil") {
                        if (v._2._2.tag === "Nil") {
                          if (v._2._1._2 === "Pair") {
                            return bindEither.bind((() => {
                              const $2 = index2(v._1._2._1)(v._2._1._3._1._2 - 1 | 0);
                              const $3 = (() => {
                                if ($2.tag === "Just") {
                                  return index2($2._1)(v._2._1._3._2._1._2 - 1 | 0);
                                }
                                if ($2.tag === "Nothing") {
                                  return Nothing;
                                }
                                fail();
                              })();
                              if ($3.tag === "Nothing") {
                                return $Either("Left", "Index out of bounds");
                              }
                              if ($3.tag === "Just") {
                                return $Either("Right", $3._1);
                              }
                              fail();
                            })())((v1) => $Either(
                              "Right",
                              $Tuple(
                                $Tuple(
                                  arrayMap(arrayMap(erase))(v._1._2._1),
                                  $Tuple($Tuple(v._1._2._2._1._1, v._1._2._2._2._1), $Tuple(v._2._1._3._1._2, v._2._1._3._2._1._2))
                                ),
                                v1
                              )
                            ));
                          }
                          return $Either("Left", "Matrix and pair of integers expected");
                        }
                        return $Either("Left", "Matrix and pair of integers expected");
                      }
                      return $Either("Left", "Matrix and pair of integers expected");
                    }
                    return $Either("Left", "Matrix and pair of integers expected");
                  }
                  return $Either("Left", "Matrix and pair of integers expected");
                }
                return $Either("Left", "Matrix and pair of integers expected");
              }
              return $Either("Left", "Matrix and pair of integers expected");
            }
            return $Either("Left", "Matrix and pair of integers expected");
          }
          return $Either("Left", "Matrix and pair of integers expected");
        }
        return $Either("Left", "Matrix and pair of integers expected");
      }
      return $Either("Left", "Matrix and pair of integers expected");
    },
    op_bwd: (dictAnn) => {
      const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
      return (v) => $List(
        "Cons",
        $Val(
          "Matrix",
          BoundedJoinSemilattice0.bot,
          updateMatrix(v._1._2._2._1)(v._1._2._2._2)((v$1) => v._2)($Tuple(
            arrayMap(arrayMap(functorVal.map((v$1) => BoundedJoinSemilattice0.bot)))(v._1._1),
            $Tuple($Tuple(v._1._2._1._1, BoundedJoinSemilattice0.bot), $Tuple(v._1._2._1._2, BoundedJoinSemilattice0.bot))
          ))
        ),
        $List(
          "Cons",
          $Val(
            "Constr",
            BoundedJoinSemilattice0.bot,
            "Pair",
            $List(
              "Cons",
              $Val("Int", BoundedJoinSemilattice0.bot, v._1._2._2._1),
              $List("Cons", $Val("Int", BoundedJoinSemilattice0.bot, v._1._2._2._2), Nil)
            )
          ),
          Nil
        )
      );
    }
  });
  var log3 = (v2) => {
    if (v2.tag === "Left") {
      return log(toNumber(v2._1));
    }
    if (v2.tag === "Right") {
      return log(v2._1);
    }
    fail();
  };
  var lessThanEquals = /* @__PURE__ */ union3(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((a1) => (a2) => a1 <= a2)(/* @__PURE__ */ union3(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((a1) => (a2) => a1 <= a2)((a1) => (a2) => a1 <= a2));
  var lessThan = /* @__PURE__ */ union3(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((a1) => (a2) => a1 < a2)(/* @__PURE__ */ union3(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((a1) => (a2) => a1 < a2)((a1) => (a2) => a1 < a2));
  var greaterThanEquals = /* @__PURE__ */ union3(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((a1) => (a2) => a1 >= a2)(/* @__PURE__ */ union3(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((a1) => (a2) => a1 >= a2)((a1) => (a2) => a1 >= a2));
  var greaterThan = /* @__PURE__ */ union3(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((a1) => (a2) => a1 > a2)(/* @__PURE__ */ union3(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((a1) => (a2) => a1 > a2)((a1) => (a2) => a1 > a2));
  var divide = /* @__PURE__ */ union3(asNumberIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber)((x2) => (y2) => toNumber(x2) / toNumber(y2))(numDiv);
  var dims = /* @__PURE__ */ $ForeignOp$p({
    arity: 1,
    op: (dictAnn) => (v) => {
      if (v.tag === "Cons") {
        if (v._1.tag === "Matrix") {
          if (v._2.tag === "Nil") {
            return $Either(
              "Right",
              $Tuple(
                arrayMap(arrayMap(erase))(v._1._2._1),
                $Val(
                  "Constr",
                  v._1._1,
                  "Pair",
                  $List(
                    "Cons",
                    $Val("Int", v._1._2._2._1._2, v._1._2._2._1._1),
                    $List("Cons", $Val("Int", v._1._2._2._2._2, v._1._2._2._2._1), Nil)
                  )
                )
              )
            );
          }
          return $Either("Left", "Matrix expected");
        }
        return $Either("Left", "Matrix expected");
      }
      return $Either("Left", "Matrix expected");
    },
    op_bwd: (dictAnn) => {
      const $1 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
      return (v) => {
        if (v._2.tag === "Constr") {
          if (v._2._3.tag === "Cons") {
            if (v._2._3._1.tag === "Int") {
              if (v._2._3._2.tag === "Cons") {
                if (v._2._3._2._1.tag === "Int") {
                  if (v._2._3._2._2.tag === "Nil") {
                    if (v._2._2 === "Pair") {
                      return $List(
                        "Cons",
                        $Val(
                          "Matrix",
                          v._2._1,
                          $Tuple(
                            arrayMap(arrayMap(functorVal.map((v$1) => $1.bot)))(v._1),
                            $Tuple($Tuple(v._2._3._1._2, v._2._3._1._1), $Tuple(v._2._3._2._1._2, v._2._3._2._1._1))
                          )
                        ),
                        Nil
                      );
                    }
                    fail();
                  }
                  fail();
                }
                fail();
              }
              fail();
            }
            fail();
          }
          fail();
        }
        fail();
      };
    }
  });
  var dict_map = /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    op: (dictAnn) => {
      const apply3 = apply(dictAnn);
      return (v) => {
        if (v.tag === "Cons") {
          if (v._2.tag === "Cons") {
            if (v._2._1.tag === "Dictionary") {
              if (v._2._2.tag === "Nil") {
                return bindEither.bind((() => {
                  const $3 = traverse5((v2) => {
                    const $4 = apply3($Tuple(v._1, v2._2));
                    if ($4.tag === "Left") {
                      return $Either("Left", $4._1);
                    }
                    if ($4.tag === "Right") {
                      return $Either("Right", $Tuple($4._1._1, $Tuple(v2._1, $4._1._2)));
                    }
                    fail();
                  })(v._2._1._2);
                  if ($3.tag === "Left") {
                    return $Either("Left", $3._1);
                  }
                  if ($3.tag === "Right") {
                    return $Either("Right", $Tuple(_fmapObject($3._1, fst), _fmapObject($3._1, snd)));
                  }
                  fail();
                })())((v2) => $Either(
                  "Right",
                  $Tuple($Tuple(functorVal.map((v$1) => unit)(v._1), v2._1), $Val("Dictionary", v._2._1._1, v2._2))
                ));
              }
              fail();
            }
            fail();
          }
          fail();
        }
        fail();
      };
    },
    op_bwd: (dictAnn) => {
      const applyBwd2 = applyBwd(dictAnn);
      const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
      const join = joinSemilatticeVal(BoundedJoinSemilattice0.JoinSemilattice0()).join;
      return (v) => {
        if (v._2.tag === "Dictionary") {
          const $5 = intersectionWith((t) => (v3) => {
            const $7 = applyBwd2($Tuple(t, v3._2));
            return $Tuple($7._1, $Tuple(v3._1, $7._2));
          })(v._1._2)(v._2._2);
          return $List(
            "Cons",
            fold((z) => (v$1) => join(z))(functorVal.map((v$1) => BoundedJoinSemilattice0.bot)(v._1._1))(_fmapObject($5, fst)),
            $List("Cons", $Val("Dictionary", v._2._1, _fmapObject($5, snd)), Nil)
          );
        }
        fail();
      };
    }
  });
  var dict_intersectionWith = /* @__PURE__ */ $ForeignOp$p({
    arity: 3,
    op: (dictAnn) => {
      const meet = dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0().meet;
      const apply22 = apply2(dictAnn);
      return (v) => {
        if (v.tag === "Cons") {
          if (v._2.tag === "Cons") {
            if (v._2._1.tag === "Dictionary") {
              if (v._2._2.tag === "Cons") {
                if (v._2._2._1.tag === "Dictionary") {
                  if (v._2._2._2.tag === "Nil") {
                    return bindEither.bind(sequence3(intersectionWith((v2) => (v3) => {
                      const $6 = apply22($Tuple(v._1, $Tuple(v2._2, v3._2)));
                      if ($6.tag === "Left") {
                        return $Either("Left", $6._1);
                      }
                      if ($6.tag === "Right") {
                        return $Either("Right", $Tuple(meet(v2._1)(v3._1), $6._1));
                      }
                      fail();
                    })(v._2._1._2)(v._2._2._1._2)))((d$p$p) => $Either(
                      "Right",
                      $Tuple(
                        $Tuple(functorVal.map((v$1) => unit)(v._1), _fmapObject(d$p$p, (x2) => x2._2._1)),
                        $Val("Dictionary", meet(v._2._1._1)(v._2._2._1._1), _fmapObject(d$p$p, functorTuple.map(snd)))
                      )
                    ));
                  }
                  return $Either("Left", "Function and two dictionaries expected");
                }
                return $Either("Left", "Function and two dictionaries expected");
              }
              return $Either("Left", "Function and two dictionaries expected");
            }
            return $Either("Left", "Function and two dictionaries expected");
          }
          return $Either("Left", "Function and two dictionaries expected");
        }
        return $Either("Left", "Function and two dictionaries expected");
      };
    },
    op_bwd: (dictAnn) => {
      const apply2Bwd2 = apply2Bwd(dictAnn);
      const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
      const join = joinSemilatticeVal(BoundedJoinSemilattice0.JoinSemilattice0()).join;
      return (v) => {
        if (v._2.tag === "Dictionary") {
          const d$p = intersectionWith((tt) => (v2) => $Tuple(v2._1, apply2Bwd2($Tuple(tt, v2._2))))(v._1._2)(v._2._2);
          return $List(
            "Cons",
            fold((z) => (v$1) => join(z))(functorVal.map((v$1) => BoundedJoinSemilattice0.bot)(v._1._1))(_fmapObject(d$p, (x2) => x2._2._1)),
            $List(
              "Cons",
              $Val("Dictionary", v._2._1, _fmapObject(d$p, (m) => $Tuple(m._1, m._2._2._1))),
              $List("Cons", $Val("Dictionary", v._2._1, _fmapObject(d$p, (m) => $Tuple(m._1, m._2._2._2))), Nil)
            )
          );
        }
        fail();
      };
    }
  });
  var dict_get = /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    op: (dictAnn) => (v) => {
      if (v.tag === "Cons") {
        if (v._1.tag === "Str") {
          if (v._2.tag === "Cons") {
            if (v._2._1.tag === "Dictionary") {
              if (v._2._2.tag === "Nil") {
                const $2 = 'Key "' + (v._1._2 + '" not found');
                const $3 = _lookup(Nothing, Just, v._1._2, v._2._1._2);
                if ($3.tag === "Just") {
                  return $Either("Right", $Tuple(v._1._2, $3._1._2));
                }
                return $Either("Left", $2);
              }
              return $Either("Left", "String and dictionary expected");
            }
            return $Either("Left", "String and dictionary expected");
          }
          return $Either("Left", "String and dictionary expected");
        }
        return $Either("Left", "String and dictionary expected");
      }
      return $Either("Left", "String and dictionary expected");
    },
    op_bwd: (dictAnn) => {
      const bot = dictAnn.BoundedLattice1().BoundedJoinSemilattice0().bot;
      return (v) => $List(
        "Cons",
        $Val("Str", bot, v._1),
        $List(
          "Cons",
          $Val(
            "Dictionary",
            bot,
            runST(bind_(newImpl)(poke2(v._1)($Tuple(bot, v._2))))
          ),
          Nil
        )
      );
    }
  });
  var dict_fromRecord = /* @__PURE__ */ $ForeignOp$p({
    arity: 1,
    op: (dictAnn) => (v) => {
      if (v.tag === "Cons") {
        if (v._1.tag === "Record") {
          if (v._2.tag === "Nil") {
            return $Either(
              "Right",
              $Tuple(unit, $Val("Dictionary", v._1._1, _fmapObject(v._1._2, (v1) => $Tuple(v._1._1, v1))))
            );
          }
          return $Either("Left", "Record expected.");
        }
        return $Either("Left", "Record expected.");
      }
      return $Either("Left", "Record expected.");
    },
    op_bwd: (dictAnn) => {
      const join = dictAnn.BoundedLattice1().BoundedJoinSemilattice0().JoinSemilattice0().join;
      return (v) => {
        if (v._2.tag === "Dictionary") {
          return $List(
            "Cons",
            $Val(
              "Record",
              fold((z) => (v$1) => join(z))(v._2._1)(_fmapObject(v._2._2, fst)),
              _fmapObject(v._2._2, snd)
            ),
            Nil
          );
        }
        fail();
      };
    }
  });
  var dict_foldl = /* @__PURE__ */ $ForeignOp$p({
    arity: 3,
    op: (dictAnn) => {
      const apply22 = apply2(dictAnn);
      return (v) => {
        if (v.tag === "Cons") {
          if (v._2.tag === "Cons") {
            if (v._2._2.tag === "Cons") {
              if (v._2._2._1.tag === "Dictionary") {
                if (v._2._2._2.tag === "Nil") {
                  return bindEither.bind(foldWithIndexM((s) => (v2) => (v3) => {
                    const $6 = apply22($Tuple(v._1, $Tuple(v2._2, v3._2)));
                    if ($6.tag === "Left") {
                      return $Either("Left", $6._1);
                    }
                    if ($6.tag === "Right") {
                      return $Either("Right", $Tuple($List("Cons", $Tuple(s, $6._1._1), v2._1), $6._1._2));
                    }
                    fail();
                  })($Tuple(Nil, v._2._1))(v._2._2._1._2))((v2) => $Either(
                    "Right",
                    $Tuple($Tuple(functorVal.map((v$1) => unit)(v._1), v2._1), v2._2)
                  ));
                }
                return $Either("Left", "Function, value and dictionary expected");
              }
              return $Either("Left", "Function, value and dictionary expected");
            }
            return $Either("Left", "Function, value and dictionary expected");
          }
          return $Either("Left", "Function, value and dictionary expected");
        }
        return $Either("Left", "Function, value and dictionary expected");
      };
    },
    op_bwd: (dictAnn) => {
      const apply2Bwd2 = apply2Bwd(dictAnn);
      const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
      const join = joinSemilatticeVal(BoundedJoinSemilattice0.JoinSemilattice0()).join;
      return (v) => {
        const v2 = (() => {
          const go = (go$a0$copy) => (go$a1$copy) => {
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
                  const v5 = apply2Bwd2($Tuple(v$1._1._2, b._2._1));
                  return $Tuple(
                    join(b._1)(v5._1),
                    $Tuple(v5._2._1, mutate(poke2(v$1._1._1)($Tuple(BoundedJoinSemilattice0.bot, v5._2._2)))(b._2._2))
                  );
                })();
                go$a1 = v$1._2;
                continue;
              }
              fail();
            }
            ;
            return go$r;
          };
          return go($Tuple(functorVal.map((v$1) => BoundedJoinSemilattice0.bot)(v._1._1), $Tuple(v._2, empty)))(v._1._2);
        })();
        return $List(
          "Cons",
          v2._1,
          $List("Cons", v2._2._1, $List("Cons", $Val("Dictionary", BoundedJoinSemilattice0.bot, v2._2._2), Nil))
        );
      };
    }
  });
  var dict_disjointUnion = /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    op: (dictAnn) => {
      const meet = dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0().meet;
      return (v) => {
        if (v.tag === "Cons") {
          if (v._1.tag === "Dictionary") {
            if (v._2.tag === "Cons") {
              if (v._2._1.tag === "Dictionary") {
                if (v._2._2.tag === "Nil") {
                  return $Either(
                    "Right",
                    $Tuple(
                      $Tuple(_fmapObject(v._1._2, (v$1) => unit), _fmapObject(v._2._1._2, (v$1) => unit)),
                      $Val(
                        "Dictionary",
                        meet(v._1._1)(v._2._1._1),
                        unionWith2((v$1) => (v1) => unsafePerformEffect(throwException(error("not disjoint"))))(v._1._2)(v._2._1._2)
                      )
                    )
                  );
                }
                return $Either("Left", "Dictionaries expected");
              }
              return $Either("Left", "Dictionaries expected");
            }
            return $Either("Left", "Dictionaries expected");
          }
          return $Either("Left", "Dictionaries expected");
        }
        return $Either("Left", "Dictionaries expected");
      };
    },
    op_bwd: (dictAnn) => (v) => {
      if (v._2.tag === "Dictionary") {
        return $List(
          "Cons",
          $Val("Dictionary", v._2._1, difference2(v._2._2)(v._1._2)),
          $List("Cons", $Val("Dictionary", v._2._1, difference2(v._2._2)(v._1._1)), Nil)
        );
      }
      fail();
    }
  });
  var dict_difference = /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    op: (dictAnn) => {
      const meet = dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0().meet;
      return (v) => {
        if (v.tag === "Cons") {
          if (v._1.tag === "Dictionary") {
            if (v._2.tag === "Cons") {
              if (v._2._1.tag === "Dictionary") {
                if (v._2._2.tag === "Nil") {
                  return $Either("Right", $Tuple(unit, $Val("Dictionary", meet(v._1._1)(v._2._1._1), difference2(v._1._2)(v._2._1._2))));
                }
                return $Either("Left", "Dictionaries expected.");
              }
              return $Either("Left", "Dictionaries expected.");
            }
            return $Either("Left", "Dictionaries expected.");
          }
          return $Either("Left", "Dictionaries expected.");
        }
        return $Either("Left", "Dictionaries expected.");
      };
    },
    op_bwd: (dictAnn) => (v) => {
      if (v._2.tag === "Dictionary") {
        return $List(
          "Cons",
          $Val("Dictionary", v._2._1, v._2._2),
          $List("Cons", $Val("Dictionary", v._2._1, empty), Nil)
        );
      }
      fail();
    }
  });
  var debugLog = (x2) => _trace(x2, (v) => x2);
  var primitives = /* @__PURE__ */ (() => fromFoldable2(foldableArray)([
    $Tuple(":", $Val("Fun", $Fun("PartialConstr", unit, ":", Nil))),
    $Tuple("ceiling", unary({ i: number, o: $$int, fwd: ceil2 })),
    $Tuple("debugLog", unary({ i: val, o: val, fwd: debugLog })),
    $Tuple("dims", $Val("Fun", $Fun("Foreign", dims, Nil))),
    $Tuple("error", unary({ i: string, o: val, fwd: error2 })),
    $Tuple("floor", unary({ i: number, o: $$int, fwd: floor2 })),
    $Tuple("log", unary({ i: intOrNumber, o: number, fwd: log3 })),
    $Tuple("numToStr", unary({ i: intOrNumber, o: string, fwd: numToStr })),
    $Tuple(
      "+",
      binary({
        i1: intOrNumber,
        i2: intOrNumber,
        o: intOrNumber,
        fwd: union3(asIntIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber)(intAdd)(numAdd)
      })
    ),
    $Tuple(
      "-",
      binary({
        i1: intOrNumber,
        i2: intOrNumber,
        o: intOrNumber,
        fwd: union3(asIntIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber)(intSub)(numSub)
      })
    ),
    $Tuple(
      "*",
      binaryZero({ isZero: fanin2(isZeroInt.isZero)(isZeroNumber.isZero) })({
        i: intOrNumber,
        o: intOrNumber,
        fwd: union3(asIntIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber)(intMul)(numMul)
      })
    ),
    $Tuple(
      "**",
      binaryZero({ isZero: fanin2(isZeroInt.isZero)(isZeroNumber.isZero) })({ i: intOrNumber, o: intOrNumber, fwd: pow3 })
    ),
    $Tuple(
      "/",
      binaryZero({ isZero: fanin2(isZeroInt.isZero)(isZeroNumber.isZero) })({ i: intOrNumber, o: intOrNumber, fwd: divide })
    ),
    $Tuple(
      "==",
      binary({
        i1: intOrNumberOrString,
        i2: intOrNumberOrString,
        o: $$boolean,
        fwd: union3(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)(eqIntImpl)(unionStr(asBooleanBoolean)(asNumberString)(eqNumberImpl)(eqStringImpl))
      })
    ),
    $Tuple("/=", binary({ i1: intOrNumberOrString, i2: intOrNumberOrString, o: $$boolean, fwd: notEquals })),
    $Tuple("<", binary({ i1: intOrNumberOrString, i2: intOrNumberOrString, o: $$boolean, fwd: lessThan })),
    $Tuple(">", binary({ i1: intOrNumberOrString, i2: intOrNumberOrString, o: $$boolean, fwd: greaterThan })),
    $Tuple("<=", binary({ i1: intOrNumberOrString, i2: intOrNumberOrString, o: $$boolean, fwd: lessThanEquals })),
    $Tuple(">=", binary({ i1: intOrNumberOrString, i2: intOrNumberOrString, o: $$boolean, fwd: greaterThanEquals })),
    $Tuple("++", binary({ i1: string, i2: string, o: string, fwd: concatString })),
    $Tuple("!", $Val("Fun", $Fun("Foreign", matrixLookup, Nil))),
    $Tuple("dict_difference", $Val("Fun", $Fun("Foreign", dict_difference, Nil))),
    $Tuple("dict_disjointUnion", $Val("Fun", $Fun("Foreign", dict_disjointUnion, Nil))),
    $Tuple("dict_foldl", $Val("Fun", $Fun("Foreign", dict_foldl, Nil))),
    $Tuple("dict_fromRecord", $Val("Fun", $Fun("Foreign", dict_fromRecord, Nil))),
    $Tuple("dict_get", $Val("Fun", $Fun("Foreign", dict_get, Nil))),
    $Tuple("dict_intersectionWith", $Val("Fun", $Fun("Foreign", dict_intersectionWith, Nil))),
    $Tuple("dict_map", $Val("Fun", $Fun("Foreign", dict_map, Nil))),
    $Tuple("div", binaryZero(isZeroInt)({ i: $$int, o: $$int, fwd: intDiv })),
    $Tuple("mod", binaryZero(isZeroInt)({ i: $$int, o: $$int, fwd: intMod })),
    $Tuple("quot", binaryZero(isZeroInt)({ i: $$int, o: $$int, fwd: quot })),
    $Tuple("rem", binaryZero(isZeroInt)({ i: $$int, o: $$int, fwd: rem }))
  ]))();

  // output-es/Module/index.js
  var identity21 = (x2) => x2;
  var eval_module2 = /* @__PURE__ */ eval_module(annBoolean);
  var desugFwd$p2 = /* @__PURE__ */ desugFwd$p(joinSemilatticeBoolean)(desugarableExprExpr);
  var $$eval2 = /* @__PURE__ */ $$eval(annBoolean);
  var parse = (src) => {
    const $1 = runParserT1(src);
    return (x2) => bifunctorEither.bimap(showParseError.show)(identity21)($1(x2));
  };
  var loadFile = (v) => (v1) => _bind(request(driver)({
    method: $Either("Left", GET),
    url: "./" + (v + ("/" + (v1 + ".fld"))),
    headers: [],
    content: Nothing,
    username: Nothing,
    password: Nothing,
    withCredentials: false,
    responseFormat: $ResponseFormat("String", identity17),
    timeout: Nothing
  }))((result) => {
    if (result.tag === "Left") {
      return unsafePerformEffect(throwException(error(printError(result._1))));
    }
    if (result.tag === "Right") {
      return _pure(result._1.body);
    }
    fail();
  });
  var loadModule = (file) => (\u03B3) => _bind(loadFile("fluid/lib")(file))((src) => _pure(successful((() => {
    const $3 = bindEither.bind(bindEither.bind(parse(src)((state1, more, lift1, $$throw, done) => more((v1) => module_(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more((v2) => done(state2, functorModule.map((v) => false)(a)))
    ))))(desugarModuleFwd(joinSemilatticeBoolean)))((() => {
      const $32 = eval_module2(\u03B3);
      return (a) => $32(a)(false);
    })());
    if ($3.tag === "Left") {
      return $Either("Left", $3._1);
    }
    if ($3.tag === "Right") {
      return $Either("Right", unionWith2((v) => identity12)(\u03B3)($3._1));
    }
    fail();
  })())));
  var parseProgram = (folder) => (file) => _bind(loadFile(folder)(file))((loaded) => _pure(successful(parse(loaded)((state1, more, lift1, $$throw, done) => more((v1) => topLevel(expr_)(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more((v2) => done(state2, functorExpr2.map((v) => false)(a)))
  ))))));
  var defaultImports = /* @__PURE__ */ (() => _bind(_bind(loadModule("prelude")(_fmapObject(
    primitives,
    functorVal.map((v) => false)
  )))(loadModule("graphics")))(loadModule("convolution")))();
  var openDatasetAs = (file) => (x2) => _bind(parseProgram("fluid")(file))((s) => _bind(defaultImports)((\u03B3) => _pure($Tuple(
    \u03B3,
    runST(bind_(newImpl)(poke2(x2)(successful(bindEither.bind(desugFwd$p2(s))((() => {
      const $4 = $$eval2(\u03B3);
      return (a) => $4(a)(false);
    })()))._2)))
  ))));

  // output-es/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event2) {
        return fn(event2)();
      };
    };
  }

  // output-es/App.Fig/index.js
  var $View = (tag, _1) => ({ tag, _1 });
  var match9 = /* @__PURE__ */ (() => matrixRep.match(annBoolean))();
  var sequence4 = /* @__PURE__ */ (() => traversableArray.traverse(applicativeEither)(identity7))();
  var eval_module3 = /* @__PURE__ */ eval_module(annBoolean);
  var desugFwd$p3 = /* @__PURE__ */ desugFwd$p(joinSemilatticeBoolean)(desugarableExprExpr);
  var $$eval3 = /* @__PURE__ */ $$eval(annBoolean);
  var evalBwd2 = /* @__PURE__ */ evalBwd(annBoolean);
  var erase2 = /* @__PURE__ */ (() => functorVal.map((v) => unit))();
  var joinSemilatticeVal2 = /* @__PURE__ */ joinSemilatticeVal(joinSemilatticeBoolean);
  var neg4 = /* @__PURE__ */ (() => joinSemilatticeDict(joinSemilatticeVal2).neg)();
  var botOf = /* @__PURE__ */ (() => functorVal.map((v) => false))();
  var identity22 = (x2) => x2;
  var sequence_ = /* @__PURE__ */ traverse_(applicativeEffect)(foldableArray)(identity5);
  var length5 = /* @__PURE__ */ foldlArray((c) => (v) => 1 + c | 0)(0);
  var view = (v) => (v1) => {
    if (v1.tag === "Constr") {
      if (v1._3.tag === "Cons") {
        if (v1._3._2.tag === "Nil") {
          if (v1._2 === "BarChart") {
            return $View("BarChartFig", reflectDictValBooleanBarC1.from()(match(v1._3._1)._1));
          }
          if (v1._2 === "LineChart") {
            return $View("LineChartFig", reflectDictValBooleanLine1.from()(match(v1._3._1)._1));
          }
          if (v1._2 === "Nil" || v1._2 === ":") {
            return $View("EnergyTableView", { title: v, table: arrayMap(record2(energyRecord))(reflectArray.from()(v1)) });
          }
          return unsafePerformEffect(throwException(error("absurd")));
        }
        if (v1._2 === "Nil" || v1._2 === ":") {
          return $View("EnergyTableView", { title: v, table: arrayMap(record2(energyRecord))(reflectArray.from()(v1)) });
        }
        return unsafePerformEffect(throwException(error("absurd")));
      }
      if (v1._2 === "Nil" || v1._2 === ":") {
        return $View("EnergyTableView", { title: v, table: arrayMap(record2(energyRecord))(reflectArray.from()(v1)) });
      }
      return unsafePerformEffect(throwException(error("absurd")));
    }
    if (v1.tag === "Matrix") {
      return $View("MatrixFig", { title: v, matrix: matrixRep2(match9(v1)._1) });
    }
    return unsafePerformEffect(throwException(error("absurd")));
  };
  var varView = (x2) => (\u03B3) => {
    const $2 = _lookup(Nothing, Just, x2, \u03B3);
    if ($2.tag === "Nothing") {
      return $Either("Left", "absurd");
    }
    if ($2.tag === "Just") {
      return $Either("Right", view(x2)($2._1));
    }
    fail();
  };
  var splitDefs = (\u03B30) => (s$p) => {
    if (s$p.tag === "LetRec") {
      return bindEither.bind(bindEither.bind(moduleFwd(joinSemilatticeBoolean)($Module2($List(
        "Cons",
        $Either("Right", s$p._1),
        Nil
      ))))((() => {
        const $2 = eval_module3(\u03B30);
        return (a) => $2(a)(false);
      })()))((\u03B3) => $Either("Right", { "\u03B3": \u03B3, s: s$p._2 }));
    }
    if (s$p.tag === "Let") {
      return bindEither.bind(bindEither.bind(moduleFwd(joinSemilatticeBoolean)($Module2($List(
        "Cons",
        $Either("Left", s$p._1),
        Nil
      ))))((() => {
        const $2 = eval_module3(\u03B30);
        return (a) => $2(a)(false);
      })()))((\u03B3) => $Either("Right", { "\u03B3": \u03B3, s: s$p._2 }));
    }
    fail();
  };
  var loadLinkFig = (v) => {
    const $1 = "linking/" + v.file1;
    const $2 = "linking/" + v.file2;
    return _bind(openDatasetAs("example/linking/" + v.dataFile)(v.x))((v2) => _bind(applyAff.apply(_map(Tuple)(parseProgram("fluid/example")($1)))(parseProgram("fluid/example")($2)))((v3) => _pure(successful(bindEither.bind(applyEither.apply((() => {
      const $5 = desugFwd$p3(v3._1);
      if ($5.tag === "Left") {
        return $Either("Left", $5._1);
      }
      if ($5.tag === "Right") {
        return $Either("Right", Tuple($5._1));
      }
      fail();
    })())(desugFwd$p3(v3._2)))((v4) => bindEither.bind($$eval3(unionWith2((v$1) => identity12)(v2._1)(v2._2))(v4._1)(false))((v5) => bindEither.bind($$eval3(unionWith2((v$1) => identity12)(v2._1)(v2._2))(v4._2)(false))((v6) => bindEither.bind((() => {
      const $8 = _lookup(Nothing, Just, v.x, v2._2);
      if ($8.tag === "Nothing") {
        return $Either("Left", "absurd");
      }
      if ($8.tag === "Just") {
        return $Either("Right", $8._1);
      }
      fail();
    })())((v0) => $Either("Right", { spec: v, "\u03B30": v2._1, "\u03B3": v2._2, s1: v3._1, s2: v3._2, e1: v4._1, e2: v4._2, t1: v5._1, t2: v6._1, v1: v5._2, v2: v6._2, v0 })))))))));
  };
  var loadFig = (v) => _bind(openDatasetAs("example/linking/renewables")("data"))((v1) => _map((s$p) => successful(bindEither.bind(splitDefs(unionWith2((v$1) => identity12)(v1._1)(v1._2))(s$p))((v2) => bindEither.bind(desugFwd$p3(v2.s))((e) => bindEither.bind($$eval3(unionWith2((v$1) => identity12)(unionWith2((v$1) => identity12)(v1._1)(v1._2))(v2["\u03B3"]))(e)(false))((v3) => $Either("Right", { spec: v, "\u03B30": v1._1, "\u03B3": unionWith2((v$1) => identity12)(v1._2)(v2["\u03B3"]), s: v2.s, e, t: v3._1, v: v3._2 }))))))(parseProgram("fluid/example")(v.file)));
  var linkResult = (x2) => (\u03B30) => (\u03B3) => (e1) => (e22) => (t1) => (v) => (v1) => {
    const $8 = append_inv($Map("Two", Leaf2, x2, unit, Leaf2))(evalBwd2(_fmapObject(
      unionWith2((v$1) => identity12)(\u03B30)(\u03B3),
      erase2
    ))(functorExpr.map((v$1) => unit)(e1))(v1)(t1)["\u03B3"])._2;
    return bindEither.bind((() => {
      const $9 = _lookup(Nothing, Just, x2, $8);
      if ($9.tag === "Nothing") {
        return $Either("Left", "absurd");
      }
      if ($9.tag === "Just") {
        return $Either("Right", $9._1);
      }
      fail();
    })())((v0$p) => bindEither.bind($$eval3(neg4(unionWith2((v$1) => identity12)(_fmapObject(\u03B30, botOf))($8)))(functorExpr.map((x$1) => true)(e22))(true))((v4) => $Either(
      "Right",
      { "v'": joinSemilatticeVal2.neg(v4._2), "v0'": v0$p }
    )));
  };
  var figViews = (v) => (\u03B4v) => {
    const v2 = evalBwd2(_fmapObject(unionWith2((v$1) => identity12)(v["\u03B30"])(v["\u03B3"]), erase2))(functorExpr.map((v$1) => unit)(v.e))(\u03B4v(v.v))(v.t);
    return bindEither.bind($$eval3(v2["\u03B3"])(v2.e)(v2["\u03B1"]))((v3) => bindEither.bind(sequence4(arrayMap((a) => varView(a)(v2["\u03B3"]))(v.spec.xs)))((views) => $Either(
      "Right",
      $Tuple(view("output")(v3._2), views)
    )));
  };
  var drawView = (divId) => (onSel) => (n) => (v) => {
    if (v.tag === "MatrixFig") {
      return bindE(eventListener((x2) => onSel(matrixViewHandler(x2))))(drawMatrix(divId)(n)(v._1));
    }
    if (v.tag === "EnergyTableView") {
      return bindE(eventListener((x2) => onSel((x$1) => x$1)))(drawTable(divId)(n)(v._1));
    }
    if (v.tag === "LineChartFig") {
      return bindE(eventListener((x2) => onSel(lineChartHandler(x2))))(drawLineChart(divId)(n)(v._1));
    }
    if (v.tag === "BarChartFig") {
      return bindE(eventListener((x2) => onSel(barChartHandler(x2))))(drawBarChart(divId)(n)(v._1));
    }
    fail();
  };
  var drawLinkFig = (v) => (\u03B4v) => {
    const $2 = log2("Redrawing " + v.spec.divId);
    return () => {
      $2();
      const v3 = successful((() => {
        if (\u03B4v.tag === "Left") {
          const v1$p = \u03B4v._1(v.v1);
          return bindEither.bind(linkResult(v.spec.x)(v["\u03B30"])(v["\u03B3"])(v.e1)(v.e2)(v.t1)(v.t2)(v1$p))((v4) => $Either(
            "Right",
            $Tuple(v1$p, $Tuple(v4["v'"], $Tuple((v$1) => v1$p, $Tuple(identity22, v4["v0'"]))))
          ));
        }
        if (\u03B4v.tag === "Right") {
          const v2$p = \u03B4v._1(v.v2);
          return bindEither.bind(linkResult(v.spec.x)(v["\u03B30"])(v["\u03B3"])(v.e2)(v.e1)(v.t2)(v.t1)(v2$p))((v4) => $Either(
            "Right",
            $Tuple(v4["v'"], $Tuple(v2$p, $Tuple(identity22, $Tuple((v$1) => v2$p, v4["v0'"]))))
          ));
        }
        fail();
      })());
      drawView(v.spec.divId)((selector) => drawLinkFig(v)($Either("Left", (x2) => selector(v3._2._2._1(x2)))))(2)(view("left view")(v3._1))();
      drawView(v.spec.divId)((selector) => drawLinkFig(v)($Either("Right", (x2) => selector(v3._2._2._2._1(x2)))))(0)(view("right view")(v3._2._1))();
      return drawView(v.spec.divId)(doNothing)(1)(view("common data")(v3._2._2._2._2))();
    };
  };
  var drawFig = (v) => (\u03B4v) => {
    const $2 = log2("Redrawing " + v.spec.divId);
    return () => {
      $2();
      const v1 = successful(figViews(v)(\u03B4v));
      sequence_(arrayMap((v$1) => drawView(v.spec.divId)(doNothing)(v$1._1)(v$1._2))(zip(range2(0)(length5(v1._2) - 1 | 0))(v1._2)))();
      return drawView(v.spec.divId)((selector) => drawFig(v)((x2) => selector(\u03B4v(x2))))(length5(v1._2))(v1._1)();
    };
  };

  // output-es/App.Main/index.js
  var sequence5 = /* @__PURE__ */ (() => traversableArray.traverse(applicativeAff)(identity7))();
  var sequence_2 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableArray)(identity5);
  var botOf2 = /* @__PURE__ */ (() => functorVal.map((v) => false))();
  var linkingFig1 = { divId: "fig-1", file1: "bar-chart", file2: "line-chart", dataFile: "renewables", x: "data" };
  var fig2 = { divId: "fig-conv-2", file: "slicing/convolution/emboss-wrap", xs: ["image", "filter"] };
  var fig1 = { divId: "fig-conv-1", file: "slicing/convolution/emboss", xs: ["image", "filter"] };
  var drawLinkFigs = (loadFigs) => {
    const $1 = runAff((v) => {
      if (v.tag === "Left") {
        return log2(showErrorImpl(v._1));
      }
      if (v.tag === "Right") {
        return sequence_2(arrayMap((a) => drawLinkFig(a)($Either("Left", botOf2)))(v._1));
      }
      fail();
    })(sequence5(loadFigs));
    return () => {
      $1();
      return unit;
    };
  };
  var drawFigs = (loadFigs) => {
    const $1 = runAff((v) => {
      if (v.tag === "Left") {
        return log2(showErrorImpl(v._1));
      }
      if (v.tag === "Right") {
        return sequence_2(arrayMap((a) => drawFig(a)(botOf2))(v._1));
      }
      fail();
    })(sequence5(loadFigs));
    return () => {
      $1();
      return unit;
    };
  };
  var main = /* @__PURE__ */ (() => {
    const $0 = drawFigs([loadFig(fig1), loadFig(fig2)]);
    return () => {
      $0();
      return drawLinkFigs([loadLinkFig(linkingFig1)])();
    };
  })();

  // <stdin>
  main();
})();
