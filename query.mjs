// output-es/runtime.js
function binding(init5) {
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
    value = init5();
    state = 2;
    return value;
  };
}
function fail() {
  throw new Error("Failed pattern match");
}
function intDiv(x, y) {
  if (y > 0)
    return Math.floor(x / y);
  if (y < 0)
    return -Math.floor(x / -y);
  return 0;
}

// output-es/Data.Function/index.js
var $$const = (a) => (v) => a;
var applyFlipped = (x) => (f) => f(x);
var apply = (f) => (x) => f(x);

// output-es/Control.Semigroupoid/index.js
var semigroupoidFn = { compose: (f) => (g) => (x) => f(g(x)) };

// output-es/Type.Proxy/index.js
var $$$Proxy = () => ({ tag: "Proxy" });
var $$Proxy = /* @__PURE__ */ $$$Proxy();

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

// output-es/Control.Bind/foreign.js
var arrayBind = function(arr) {
  return function(f) {
    var result = [];
    for (var i = 0, l = arr.length; i < l; i++) {
      Array.prototype.push.apply(result, f(arr[i]));
    }
    return result;
  };
};

// output-es/Control.Bind/index.js
var identity = (x) => x;

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
      var empty3 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
      return "\\" + c.charCodeAt(0).toString(10) + empty3;
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
  return function(tail4) {
    return [head].concat(tail4);
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

// output-es/Data.Generic.Rep/index.js
var $NoArguments = () => ({ tag: "NoArguments" });
var $Product = (_1, _2) => ({ tag: "Product", _1, _2 });
var $Sum = (tag, _1) => ({ tag, _1 });
var NoArguments = /* @__PURE__ */ $NoArguments();

// output-es/Data.Ordering/index.js
var $Ordering = (tag) => tag;
var LT = /* @__PURE__ */ $Ordering("LT");
var GT = /* @__PURE__ */ $Ordering("GT");
var EQ = /* @__PURE__ */ $Ordering("EQ");

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
var altMaybe = {
  alt: (v) => (v1) => {
    if (v.tag === "Nothing") {
      return v1;
    }
    return v;
  },
  Functor0: () => functorMaybe
};

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
  const $0 = dictAlt.Functor0();
  return (a) => (b) => dictAlt.alt($0.map(Left)(a))($0.map(Right)(b));
};

// output-es/Data.Identity/index.js
var Identity = (x) => x;
var functorIdentity = { map: (f) => (m) => f(m) };
var applyIdentity = { apply: (v) => (v1) => v(v1), Functor0: () => functorIdentity };
var bindIdentity = { bind: (v) => (f) => f(v), Apply0: () => applyIdentity };
var applicativeIdentity = { pure: Identity, Apply0: () => applyIdentity };
var monadIdentity = { Applicative0: () => applicativeIdentity, Bind1: () => bindIdentity };

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
      return go$r;
    };
    return (x) => go(f(x));
  },
  Monad0: () => monadIdentity
};

// output-es/Data.Tuple/index.js
var $Tuple = (_1, _2) => ({ tag: "Tuple", _1, _2 });
var Tuple = (value0) => (value1) => $Tuple(value0, value1);
var swap = (v) => $Tuple(v._2, v._1);
var snd = (v) => v._2;
var functorTuple = { map: (f) => (m) => $Tuple(m._1, f(m._2)) };
var fst = (v) => v._1;
var ordTuple = (dictOrd) => {
  const $0 = dictOrd.Eq0();
  return (dictOrd1) => {
    const $1 = dictOrd1.Eq0();
    const eqTuple2 = { eq: (x) => (y) => $0.eq(x._1)(y._1) && $1.eq(x._2)(y._2) };
    return {
      compare: (x) => (y) => {
        const v = dictOrd.compare(x._1)(y._1);
        if (v === "LT") {
          return LT;
        }
        if (v === "GT") {
          return GT;
        }
        return dictOrd1.compare(x._2)(y._2);
      },
      Eq0: () => eqTuple2
    };
  };
};

// output-es/Control.Monad.Except.Trans/index.js
var bindExceptT = (dictMonad) => ({
  bind: (v) => (k) => dictMonad.Bind1().bind(v)((v2) => {
    if (v2.tag === "Left") {
      return dictMonad.Applicative0().pure($Either("Left", v2._1));
    }
    if (v2.tag === "Right") {
      return k(v2._1);
    }
    fail();
  }),
  Apply0: () => applyExceptT(dictMonad)
});
var applyExceptT = (dictMonad) => {
  const $0 = dictMonad.Bind1().Apply0().Functor0();
  const functorExceptT1 = {
    map: (f) => $0.map((m) => {
      if (m.tag === "Left") {
        return $Either("Left", m._1);
      }
      if (m.tag === "Right") {
        return $Either("Right", f(m._1));
      }
      fail();
    })
  };
  return {
    apply: (() => {
      const $1 = bindExceptT(dictMonad);
      return (f) => (a) => $1.bind(f)((f$p) => $1.bind(a)((a$p) => applicativeExceptT(dictMonad).pure(f$p(a$p))));
    })(),
    Functor0: () => functorExceptT1
  };
};
var applicativeExceptT = (dictMonad) => ({ pure: (x) => dictMonad.Applicative0().pure($Either("Right", x)), Apply0: () => applyExceptT(dictMonad) });
var monadThrowExceptT = (dictMonad) => {
  const monadExceptT1 = { Applicative0: () => applicativeExceptT(dictMonad), Bind1: () => bindExceptT(dictMonad) };
  return { throwError: (x) => dictMonad.Applicative0().pure($Either("Left", x)), Monad0: () => monadExceptT1 };
};
var monadErrorExceptT = (dictMonad) => {
  const monadThrowExceptT1 = monadThrowExceptT(dictMonad);
  return {
    catchError: (v) => (k) => dictMonad.Bind1().bind(v)((v2) => {
      if (v2.tag === "Left") {
        return k(v2._1);
      }
      if (v2.tag === "Right") {
        return dictMonad.Applicative0().pure($Either("Right", v2._1));
      }
      fail();
    }),
    MonadThrow0: () => monadThrowExceptT1
  };
};

// output-es/Data.NonEmpty/index.js
var $NonEmpty = (_1, _2) => ({ tag: "NonEmpty", _1, _2 });
var NonEmpty = (value0) => (value1) => $NonEmpty(value0, value1);
var traversableNonEmpty = (dictTraversable) => {
  const $0 = dictTraversable.Functor0();
  const functorNonEmpty1 = { map: (f) => (m) => $NonEmpty(f(m._1), $0.map(f)(m._2)) };
  const $1 = dictTraversable.Foldable1();
  const foldableNonEmpty1 = {
    foldMap: (dictMonoid) => {
      const foldMap1 = $1.foldMap(dictMonoid);
      return (f) => (v) => dictMonoid.Semigroup0().append(f(v._1))(foldMap1(f)(v._2));
    },
    foldl: (f) => (b) => (v) => $1.foldl(f)(f(b)(v._1))(v._2),
    foldr: (f) => (b) => (v) => f(v._1)($1.foldr(f)(b)(v._2))
  };
  return {
    sequence: (dictApplicative) => {
      const Apply0 = dictApplicative.Apply0();
      const sequence1 = dictTraversable.sequence(dictApplicative);
      return (v) => Apply0.apply(Apply0.Functor0().map(NonEmpty)(v._1))(sequence1(v._2));
    },
    traverse: (dictApplicative) => {
      const Apply0 = dictApplicative.Apply0();
      const traverse1 = dictTraversable.traverse(dictApplicative);
      return (f) => (v) => Apply0.apply(Apply0.Functor0().map(NonEmpty)(f(v._1)))(traverse1(f)(v._2));
    },
    Functor0: () => functorNonEmpty1,
    Foldable1: () => foldableNonEmpty1
  };
};
var foldable1NonEmpty = (dictFoldable) => {
  const foldableNonEmpty1 = {
    foldMap: (dictMonoid) => {
      const foldMap1 = dictFoldable.foldMap(dictMonoid);
      return (f) => (v) => dictMonoid.Semigroup0().append(f(v._1))(foldMap1(f)(v._2));
    },
    foldl: (f) => (b) => (v) => dictFoldable.foldl(f)(f(b)(v._1))(v._2),
    foldr: (f) => (b) => (v) => f(v._1)(dictFoldable.foldr(f)(b)(v._2))
  };
  return {
    foldMap1: (dictSemigroup) => (f) => (v) => dictFoldable.foldl((s) => (a1) => dictSemigroup.append(s)(f(a1)))(f(v._1))(v._2),
    foldr1: (f) => (v) => {
      const $0 = f(v._1);
      const $1 = dictFoldable.foldr((a1) => {
        const $12 = f(a1);
        return (x) => $Maybe(
          "Just",
          (() => {
            if (x.tag === "Nothing") {
              return a1;
            }
            if (x.tag === "Just") {
              return $12(x._1);
            }
            fail();
          })()
        );
      })(Nothing)(v._2);
      if ($1.tag === "Nothing") {
        return v._1;
      }
      if ($1.tag === "Just") {
        return $0($1._1);
      }
      fail();
    },
    foldl1: (f) => (v) => dictFoldable.foldl(f)(v._1)(v._2),
    Foldable0: () => foldableNonEmpty1
  };
};

// output-es/Data.List.Types/index.js
var $List = (tag, _1, _2) => ({ tag, _1, _2 });
var identity2 = (x) => x;
var Nil = /* @__PURE__ */ $List("Nil");
var Cons = (value0) => (value1) => $List("Cons", value0, value1);
var listMap = (f) => {
  const chunkedRevMap = (chunkedRevMap$a0$copy) => (chunkedRevMap$a1$copy) => {
    let chunkedRevMap$a0 = chunkedRevMap$a0$copy, chunkedRevMap$a1 = chunkedRevMap$a1$copy, chunkedRevMap$c = true, chunkedRevMap$r;
    while (chunkedRevMap$c) {
      const v = chunkedRevMap$a0, v1 = chunkedRevMap$a1;
      if (v1.tag === "Cons" && v1._2.tag === "Cons" && v1._2._2.tag === "Cons") {
        chunkedRevMap$a0 = $List("Cons", v1, v);
        chunkedRevMap$a1 = v1._2._2._2;
        continue;
      }
      const reverseUnrolledMap = (reverseUnrolledMap$a0$copy) => (reverseUnrolledMap$a1$copy) => {
        let reverseUnrolledMap$a0 = reverseUnrolledMap$a0$copy, reverseUnrolledMap$a1 = reverseUnrolledMap$a1$copy, reverseUnrolledMap$c = true, reverseUnrolledMap$r;
        while (reverseUnrolledMap$c) {
          const v2 = reverseUnrolledMap$a0, v3 = reverseUnrolledMap$a1;
          if (v2.tag === "Cons" && v2._1.tag === "Cons" && v2._1._2.tag === "Cons" && v2._1._2._2.tag === "Cons") {
            reverseUnrolledMap$a0 = v2._2;
            reverseUnrolledMap$a1 = $List("Cons", f(v2._1._1), $List("Cons", f(v2._1._2._1), $List("Cons", f(v2._1._2._2._1), v3)));
            continue;
          }
          reverseUnrolledMap$c = false;
          reverseUnrolledMap$r = v3;
        }
        return reverseUnrolledMap$r;
      };
      chunkedRevMap$c = false;
      chunkedRevMap$r = reverseUnrolledMap(v)((() => {
        if (v1.tag === "Cons") {
          if (v1._2.tag === "Cons") {
            if (v1._2._2.tag === "Nil") {
              return $List("Cons", f(v1._1), $List("Cons", f(v1._2._1), Nil));
            }
            return Nil;
          }
          if (v1._2.tag === "Nil") {
            return $List("Cons", f(v1._1), Nil);
          }
        }
        return Nil;
      })());
    }
    return chunkedRevMap$r;
  };
  return chunkedRevMap(Nil);
};
var functorList = { map: listMap };
var functorNonEmptyList = { map: (f) => (m) => $NonEmpty(f(m._1), listMap(f)(m._2)) };
var foldableList = {
  foldr: (f) => (b) => {
    const $0 = foldableList.foldl((b$1) => (a) => f(a)(b$1))(b);
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0, v1 = go$a1;
        if (v1.tag === "Nil") {
          go$c = false;
          go$r = v;
          continue;
        }
        if (v1.tag === "Cons") {
          go$a0 = $List("Cons", v1._1, v);
          go$a1 = v1._2;
          continue;
        }
        fail();
      }
      return go$r;
    };
    const $1 = go(Nil);
    return (x) => $0($1(x));
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
      return go$r;
    };
    return go;
  },
  foldMap: (dictMonoid) => {
    const mempty = dictMonoid.mempty;
    return (f) => foldableList.foldl((acc) => {
      const $0 = dictMonoid.Semigroup0().append(acc);
      return (x) => $0(f(x));
    })(mempty);
  }
};
var foldableNonEmptyList = {
  foldMap: (dictMonoid) => {
    const foldMap1 = foldableList.foldMap(dictMonoid);
    return (f) => (v) => dictMonoid.Semigroup0().append(f(v._1))(foldMap1(f)(v._2));
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
      return go$r;
    };
    return go(f(b)(v._1))(v._2);
  },
  foldr: (f) => (b) => (v) => f(v._1)(foldableList.foldr(f)(b)(v._2))
};
var foldableWithIndexList = {
  foldrWithIndex: (f) => (b) => (xs) => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b$1 = go$a0, v2 = go$a1;
        if (v2.tag === "Nil") {
          go$c = false;
          go$r = b$1;
          continue;
        }
        if (v2.tag === "Cons") {
          go$a0 = $Tuple(b$1._1 + 1 | 0, $List("Cons", v2._1, b$1._2));
          go$a1 = v2._2;
          continue;
        }
        fail();
      }
      return go$r;
    };
    const v = go($Tuple(0, Nil))(xs);
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
          go$1$a0 = $Tuple(b$1._1 - 1 | 0, f(b$1._1 - 1 | 0)(v$1._1)(b$1._2));
          go$1$a1 = v$1._2;
          continue;
        }
        fail();
      }
      return go$1$r;
    };
    return go$1($Tuple(v._1, b))(v._2)._2;
  },
  foldlWithIndex: (f) => (acc) => {
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
          go$a0 = $Tuple(b._1 + 1 | 0, f(b._1)(b._2)(v._1));
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      return go$r;
    };
    const $0 = go($Tuple(0, acc));
    return (x) => $0(x)._2;
  },
  foldMapWithIndex: (dictMonoid) => {
    const mempty = dictMonoid.mempty;
    return (f) => foldableWithIndexList.foldlWithIndex((i) => (acc) => {
      const $0 = dictMonoid.Semigroup0().append(acc);
      const $1 = f(i);
      return (x) => $0($1(x));
    })(mempty);
  },
  Foldable0: () => foldableList
};
var functorWithIndexList = { mapWithIndex: (f) => foldableWithIndexList.foldrWithIndex((i) => (x) => (acc) => $List("Cons", f(i)(x), acc))(Nil), Functor0: () => functorList };
var showList = (dictShow) => {
  const show4 = dictShow.show;
  return {
    show: (v) => {
      if (v.tag === "Nil") {
        return "Nil";
      }
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
            go$a0 = b.init ? { init: false, acc: v$1._1 } : { init: false, acc: b.acc + " : " + v$1._1 };
            go$a1 = v$1._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      return "(" + go({ init: true, acc: "" })(listMap(show4)(v)).acc + " : Nil)";
    }
  };
};
var showNonEmptyList = (dictShow) => {
  const $0 = showList(dictShow);
  return { show: (v) => "(NonEmptyList (NonEmpty " + dictShow.show(v._1) + " " + $0.show(v._2) + "))" };
};
var traversableList = {
  traverse: (dictApplicative) => {
    const Apply0 = dictApplicative.Apply0();
    return (f) => {
      const $0 = Apply0.Functor0().map((() => {
        const go2 = (go$a0$copy) => (go$a1$copy) => {
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
          return go$r;
        };
        return go2(Nil);
      })());
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
            go$a0 = Apply0.apply(Apply0.Functor0().map((b$1) => (a) => $List("Cons", a, b$1))(b))(f(v._1));
            go$a1 = v._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      const $1 = go(dictApplicative.pure(Nil));
      return (x) => $0($1(x));
    };
  },
  sequence: (dictApplicative) => traversableList.traverse(dictApplicative)(identity2),
  Functor0: () => functorList,
  Foldable1: () => foldableList
};
var traversableNonEmptyList = /* @__PURE__ */ traversableNonEmpty(traversableList);
var traversableWithIndexList = {
  traverseWithIndex: (dictApplicative) => {
    const Apply0 = dictApplicative.Apply0();
    return (f) => {
      const $0 = Apply0.Functor0().map((() => {
        const go2 = (go$a0$copy) => (go$a1$copy) => {
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
          return go$r;
        };
        return go2(Nil);
      })());
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
            go$a0 = $Tuple(b._1 + 1 | 0, Apply0.apply(Apply0.Functor0().map((b$1) => (a) => $List("Cons", a, b$1))(b._2))(f(b._1)(v._1)));
            go$a1 = v._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      const $1 = go($Tuple(0, dictApplicative.pure(Nil)));
      return (x) => $0($1(x)._2);
    };
  },
  FunctorWithIndex0: () => functorWithIndexList,
  FoldableWithIndex1: () => foldableWithIndexList,
  Traversable2: () => traversableList
};
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
            return go$1$r;
          };
          go$c = false;
          go$r = go$1(Nil)($List("Cons", v._1, memo));
          continue;
        }
        fail();
      }
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
      return go$r;
    };
    return go(b)(Nil);
  },
  Unfoldable10: () => unfoldable1List
};
var eq1List = {
  eq1: (dictEq) => (xs) => (ys) => {
    const go = (v) => (v1) => (v2) => {
      if (!v2) {
        return false;
      }
      if (v.tag === "Nil") {
        return v1.tag === "Nil" && v2;
      }
      return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && dictEq.eq(v1._1)(v._1));
    };
    return go(xs)(ys)(true);
  }
};
var ord1List = {
  compare1: (dictOrd) => (xs) => (ys) => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0, v1 = go$a1;
        if (v.tag === "Nil") {
          if (v1.tag === "Nil") {
            go$c = false;
            go$r = EQ;
            continue;
          }
          go$c = false;
          go$r = LT;
          continue;
        }
        if (v1.tag === "Nil") {
          go$c = false;
          go$r = GT;
          continue;
        }
        if (v.tag === "Cons" && v1.tag === "Cons") {
          const v2 = dictOrd.compare(v._1)(v1._1);
          if (v2 === "EQ") {
            go$a0 = v._2;
            go$a1 = v1._2;
            continue;
          }
          go$c = false;
          go$r = v2;
          continue;
        }
        fail();
      }
      return go$r;
    };
    return go(xs)(ys);
  },
  Eq10: () => eq1List
};
var ordList = (dictOrd) => {
  const $0 = dictOrd.Eq0();
  const eqList1 = {
    eq: (xs) => (ys) => {
      const go = (v) => (v1) => (v2) => {
        if (!v2) {
          return false;
        }
        if (v.tag === "Nil") {
          return v1.tag === "Nil" && v2;
        }
        return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
      };
      return go(xs)(ys)(true);
    }
  };
  return { compare: ord1List.compare1(dictOrd), Eq0: () => eqList1 };
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
      foldableList.foldr(Cons)(bindList.bind(v._2)((x) => {
        const $0 = f(x);
        return $List("Cons", $0._1, $0._2);
      }))(v1._2)
    );
  },
  Apply0: () => applyNonEmptyList
};
var applicativeList = { pure: (a) => $List("Cons", a, Nil), Apply0: () => applyList };

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

// output-es/Control.Monad.Error.Class/index.js
var $$try = (dictMonadError) => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  return (a) => dictMonadError.catchError(Monad0.Bind1().Apply0().Functor0().map(Right)(a))((x) => Monad0.Applicative0().pure($Either("Left", x)));
};

// output-es/Data.Foldable/foreign.js
var foldrArray = function(f) {
  return function(init5) {
    return function(xs) {
      var acc = init5;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }
      return acc;
    };
  };
};
var foldlArray = function(f) {
  return function(init5) {
    return function(xs) {
      var acc = init5;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
    };
  };
};

// output-es/Data.Foldable/index.js
var monoidEndo = /* @__PURE__ */ (() => {
  const semigroupEndo1 = { append: (v) => (v1) => (x) => v(v1(x)) };
  return { mempty: (x) => x, Semigroup0: () => semigroupEndo1 };
})();
var foldableTuple = { foldr: (f) => (z) => (v) => f(v._2)(z), foldl: (f) => (z) => (v) => f(z)(v._2), foldMap: (dictMonoid) => (f) => (v) => f(v._2) };
var foldableArray = {
  foldr: foldrArray,
  foldl: foldlArray,
  foldMap: (dictMonoid) => {
    const mempty = dictMonoid.mempty;
    return (f) => foldableArray.foldr((x) => (acc) => dictMonoid.Semigroup0().append(f(x))(acc))(mempty);
  }
};
var foldrDefault = (dictFoldable) => {
  const foldMap22 = dictFoldable.foldMap(monoidEndo);
  return (c) => (u) => (xs) => foldMap22((x) => c(x))(xs)(u);
};

// output-es/Partial/foreign.js
var _crashWith = function(msg) {
  throw new Error(msg);
};

// output-es/Unsafe.Coerce/foreign.js
var unsafeCoerce = function(x) {
  return x;
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
  function nonCanceler2(error3) {
    return new Aff2(PURE, void 0);
  }
  function runEff(eff) {
    try {
      eff();
    } catch (error3) {
      setTimeout(function() {
        throw error3;
      }, 0);
    }
  }
  function runSync(left, right, eff) {
    try {
      return right(eff());
    } catch (error3) {
      return left(error3);
    }
  }
  function runAsync(left, eff, k) {
    try {
      return eff(k)();
    } catch (error3) {
      k(left(error3))();
      return nonCanceler2;
    }
  }
  var Scheduler = function() {
    var limit = 1024;
    var size6 = 0;
    var ix = 0;
    var queue = new Array(limit);
    var draining = false;
    function drain() {
      var thunk;
      draining = true;
      while (size6 !== 0) {
        size6--;
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
        if (size6 === limit) {
          tmp = draining;
          drain();
          draining = tmp;
        }
        queue[(ix + size6) % limit] = cb;
        size6++;
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
          return function(error3) {
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
    var fail3 = null;
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
              fail3 = util2.left(e);
              step = null;
            }
            break;
          case STEP_RESULT:
            if (util2.isLeft(step)) {
              status = RETURN;
              fail3 = step;
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
                fail3 = util2.left(step._1);
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
              step = interrupt || fail3 || step;
            } else {
              tmp = attempts._3;
              attempt = attempts._1;
              attempts = attempts._2;
              switch (attempt.tag) {
                case CATCH:
                  if (interrupt && interrupt !== tmp && bracketCount === 0) {
                    status = RETURN;
                  } else if (fail3) {
                    status = CONTINUE;
                    step = attempt._2(util2.fromLeft(fail3));
                    fail3 = null;
                  }
                  break;
                case RESUME:
                  if (interrupt && interrupt !== tmp && bracketCount === 0 || fail3) {
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
                  if (fail3 === null) {
                    result = util2.fromRight(step);
                    attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                    if (interrupt === tmp || bracketCount > 0) {
                      status = CONTINUE;
                      step = attempt._3(result);
                    }
                  }
                  break;
                case RELEASE:
                  attempts = new Aff2(CONS, new Aff2(FINALIZED, step, fail3), attempts, interrupt);
                  status = CONTINUE;
                  if (interrupt && interrupt !== tmp && bracketCount === 0) {
                    step = attempt._1.killed(util2.fromLeft(interrupt))(attempt._2);
                  } else if (fail3) {
                    step = attempt._1.failed(util2.fromLeft(fail3))(attempt._2);
                  } else {
                    step = attempt._1.completed(util2.fromRight(step))(attempt._2);
                  }
                  fail3 = null;
                  bracketCount++;
                  break;
                case FINALIZER:
                  bracketCount++;
                  attempts = new Aff2(CONS, new Aff2(FINALIZED, step, fail3), attempts, interrupt);
                  status = CONTINUE;
                  step = attempt._1;
                  break;
                case FINALIZED:
                  bracketCount--;
                  status = RETURN;
                  step = attempt._1;
                  fail3 = attempt._2;
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
            if (interrupt && fail3) {
              setTimeout(function() {
                throw util2.fromLeft(fail3);
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
    function kill(error3, cb) {
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
            interrupt = util2.left(error3);
            status = COMPLETED;
            step = interrupt;
            run2(runTick);
            break;
          case PENDING:
            if (interrupt === null) {
              interrupt = util2.left(error3);
            }
            if (bracketCount === 0) {
              if (status === PENDING) {
                attempts = new Aff2(CONS, new Aff2(FINALIZER, step(error3)), attempts, interrupt);
              }
              status = RETURN;
              step = null;
              fail3 = null;
              run2(++runTick);
            }
            break;
          default:
            if (interrupt === null) {
              interrupt = util2.left(error3);
            }
            if (bracketCount === 0) {
              status = RETURN;
              step = null;
              fail3 = null;
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
    var root = EMPTY;
    function kill(error3, par2, cb2) {
      var step = par2;
      var head = null;
      var tail4 = null;
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
                kills2[count++] = tmp.kill(error3, function(result) {
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
              if (tail4 === null) {
                head = null;
              } else {
                head = tail4._1;
                tail4 = tail4._2;
              }
              break;
            case MAP:
              step = step._2;
              break;
            case APPLY:
            case ALT:
              if (head) {
                tail4 = new Aff2(CONS, head, tail4);
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
    function join(result, head, tail4) {
      var fail3, step, lhs, rhs, tmp, kid;
      if (util2.isLeft(result)) {
        fail3 = result;
        step = null;
      } else {
        step = result;
        fail3 = null;
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
            cb(fail3 || step)();
            return;
          }
          if (head._3 !== EMPTY) {
            return;
          }
          switch (head.tag) {
            case MAP:
              if (fail3 === null) {
                head._3 = util2.right(head._1(util2.fromRight(step)));
                step = head._3;
              } else {
                head._3 = fail3;
              }
              break;
            case APPLY:
              lhs = head._1._3;
              rhs = head._2._3;
              if (fail3) {
                head._3 = fail3;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, fail3 === lhs ? head._2 : head._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail4 === null) {
                      join(fail3, null, null);
                    } else {
                      join(fail3, tail4._1, tail4._2);
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
                fail3 = step === lhs ? rhs : lhs;
                step = null;
                head._3 = fail3;
              } else {
                head._3 = step;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, step === lhs ? head._2 : head._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail4 === null) {
                      join(step, null, null);
                    } else {
                      join(step, tail4._1, tail4._2);
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
          if (tail4 === null) {
            head = null;
          } else {
            head = tail4._1;
            tail4 = tail4._2;
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
      var tail4 = null;
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
                    tail4 = new Aff2(CONS, head, tail4);
                  }
                  head = new Aff2(MAP, step._1, EMPTY, EMPTY);
                  step = step._2;
                  break;
                case APPLY:
                  if (head) {
                    tail4 = new Aff2(CONS, head, tail4);
                  }
                  head = new Aff2(APPLY, EMPTY, step._2, EMPTY);
                  step = step._1;
                  break;
                case ALT:
                  if (head) {
                    tail4 = new Aff2(CONS, head, tail4);
                  }
                  head = new Aff2(ALT, EMPTY, step._2, EMPTY);
                  step = step._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step;
                  step = new Aff2(FORKED, fid, new Aff2(CONS, head, tail4), EMPTY);
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
                if (tail4 === null) {
                  head = null;
                } else {
                  head = tail4._1;
                  tail4 = tail4._2;
                }
              }
          }
        }
      root = step;
      for (fid = 0; fid < fiberId; fid++) {
        fibers[fid].run();
      }
    }
    function cancel(error3, cb2) {
      interrupt = util2.left(error3);
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
      var newKills = kill(error3, root, cb2);
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
  return function(right, ms) {
    return Aff.Async(function(cb) {
      return function() {
        var timer = setDelay(ms, cb(right()));
        return function() {
          return Aff.Sync(function() {
            return right(clearDelay(ms, timer));
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
var monadEffectAff = { liftEffect: _liftEffect, Monad0: () => monadAff };
var monadThrowAff = { throwError: _throwError, Monad0: () => monadAff };
var monadErrorAff = { catchError: _catchError, MonadThrow0: () => monadThrowAff };
var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
var runAff = (k) => (aff) => {
  const $0 = _makeFiber(ffiUtil, _bind($$try2(aff))((x) => _liftEffect(k(x))));
  return () => {
    const fiber = $0();
    fiber.run();
    return fiber;
  };
};
var nonCanceler = /* @__PURE__ */ (() => {
  const $0 = _pure();
  return (v) => $0;
})();

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
var quot = function(x) {
  return function(y) {
    return x / y | 0;
  };
};
var rem = function(x) {
  return function(y) {
    return x % y;
  };
};

// output-es/Data.Int/index.js
var fromNumber = /* @__PURE__ */ fromNumberImpl(Just)(Nothing);
var unsafeClamp = (x) => {
  if (!isFiniteImpl(x)) {
    return 0;
  }
  if (x >= toNumber(2147483647)) {
    return 2147483647;
  }
  if (x <= toNumber(-2147483648)) {
    return -2147483648;
  }
  const $0 = fromNumber(x);
  if ($0.tag === "Nothing") {
    return 0;
  }
  if ($0.tag === "Just") {
    return $0._1;
  }
  fail();
};
var floor2 = (x) => unsafeClamp(floor(x));
var ceil2 = (x) => unsafeClamp(ceil(x));

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

// output-es/Data.Ord/foreign.js
var unsafeCompareImpl = function(lt) {
  return function(eq2) {
    return function(gt) {
      return function(x) {
        return function(y) {
          return x < y ? lt : x === y ? eq2 : gt;
        };
      };
    };
  };
};
var ordIntImpl = unsafeCompareImpl;
var ordNumberImpl = unsafeCompareImpl;
var ordStringImpl = unsafeCompareImpl;
var ordArrayImpl = function(f) {
  return function(xs) {
    return function(ys) {
      var i = 0;
      var xlen = xs.length;
      var ylen = ys.length;
      while (i < xlen && i < ylen) {
        var x = xs[i];
        var y = ys[i];
        var o = f(x)(y);
        if (o !== 0) {
          return o;
        }
        i++;
      }
      if (xlen === ylen) {
        return 0;
      } else if (xlen > ylen) {
        return -1;
      } else {
        return 1;
      }
    };
  };
};

// output-es/Data.Ord/index.js
var ordString = { compare: /* @__PURE__ */ ordStringImpl(LT)(EQ)(GT), Eq0: () => eqString };
var ordNumber = { compare: /* @__PURE__ */ ordNumberImpl(LT)(EQ)(GT), Eq0: () => eqNumber };
var ordInt = { compare: /* @__PURE__ */ ordIntImpl(LT)(EQ)(GT), Eq0: () => eqInt };
var ordArray = (dictOrd) => {
  const eqArray = { eq: eqArrayImpl(dictOrd.Eq0().eq) };
  return {
    compare: (xs) => (ys) => ordInt.compare(0)(ordArrayImpl((x) => (y) => {
      const v = dictOrd.compare(x)(y);
      if (v === "EQ") {
        return 0;
      }
      if (v === "LT") {
        return 1;
      }
      if (v === "GT") {
        return -1;
      }
      fail();
    })(xs)(ys)),
    Eq0: () => eqArray
  };
};

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
var length = function(s) {
  return s.length;
};
var _indexOf = function(just) {
  return function(nothing) {
    return function(x) {
      return function(s) {
        var i = s.indexOf(x);
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
var uncons = (v) => {
  if (v === "") {
    return Nothing;
  }
  return $Maybe("Just", { head: charAt(0)(v), tail: drop(1)(v) });
};
var toChar = /* @__PURE__ */ _toChar(Just)(Nothing);
var stripPrefix = (v) => (str) => {
  const v1 = splitAt(length(v))(str);
  if (v1.before === v) {
    return $Maybe("Just", v1.after);
  }
  return Nothing;
};
var indexOf = /* @__PURE__ */ _indexOf(Just)(Nothing);
var contains = (pat) => {
  const $0 = indexOf(pat);
  return (x) => {
    const $1 = $0(x);
    if ($1.tag === "Nothing") {
      return false;
    }
    if ($1.tag === "Just") {
      return true;
    }
    fail();
  };
};
var charAt2 = /* @__PURE__ */ _charAt(Just)(Nothing);

// output-es/Foreign/foreign.js
var isArray = Array.isArray || function(value) {
  return Object.prototype.toString.call(value) === "[object Array]";
};

// output-es/Control.Promise/foreign.js
function promise(f) {
  return function() {
    return new Promise(function(success, error3) {
      var succF = function(s) {
        return function() {
          return success(s);
        };
      };
      var failF = function(s) {
        return function() {
          return error3(s);
        };
      };
      try {
        f(succF)(failF)();
      } catch (e) {
        error3(e);
      }
    });
  };
}

// output-es/Control.Promise/index.js
var fromAff = (aff) => promise((succ) => (err) => {
  const $0 = runAff((v2) => {
    if (v2.tag === "Left") {
      return err(v2._1);
    }
    if (v2.tag === "Right") {
      return succ(v2._1);
    }
    fail();
  })(aff);
  return () => {
    $0();
  };
});

// output-es/Data.List/index.js
var identity5 = (x) => x;
var unzip = /* @__PURE__ */ (() => foldableList.foldr((v) => {
  const $0 = v._1;
  const $1 = v._2;
  return (v1) => $Tuple($List("Cons", $0, v1._1), $List("Cons", $1, v1._2));
})($Tuple(Nil, Nil)))();
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
  if (v1.tag === "Cons" && v(v1._1)) {
    const v2 = span(v)(v1._2);
    return { init: $List("Cons", v1._1, v2.init), rest: v2.rest };
  }
  return { init: Nil, rest: v1 };
};
var sortBy = (cmp) => {
  const merge = (v) => (v1) => {
    if (v.tag === "Cons") {
      if (v1.tag === "Cons") {
        if (cmp(v._1)(v1._1) === "GT") {
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
    if (v.tag === "Cons" && v._2.tag === "Cons") {
      return $List("Cons", merge(v._1)(v._2._1), mergePairs(v._2._2));
    }
    return v;
  };
  const mergeAll = (mergeAll$a0$copy) => {
    let mergeAll$a0 = mergeAll$a0$copy, mergeAll$c = true, mergeAll$r;
    while (mergeAll$c) {
      const v = mergeAll$a0;
      if (v.tag === "Cons" && v._2.tag === "Nil") {
        mergeAll$c = false;
        mergeAll$r = v._1;
        continue;
      }
      mergeAll$a0 = mergePairs(v);
    }
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
        if (v.tag === "Cons" && v._2.tag === "Cons") {
          if (cmp(v._1)(v._2._1) === "GT") {
            $sequedesceascen$b = 1;
            $sequedesceascen$a0 = v._2._1;
            $sequedesceascen$a1 = $List("Cons", v._1, Nil);
            $sequedesceascen$a2 = v._2._2;
            continue;
          }
          const $0 = v._1;
          $sequedesceascen$b = 2;
          $sequedesceascen$a0 = v._2._1;
          $sequedesceascen$a1 = (v1) => $List("Cons", $0, v1);
          $sequedesceascen$a2 = v._2._2;
          continue;
        }
        $sequedesceascen$c = false;
        $sequedesceascen$r = $List("Cons", v, Nil);
        continue;
      }
      if ($sequedesceascen$b === 1) {
        const v = $sequedesceascen$a0, v1 = $sequedesceascen$a1, v2 = $sequedesceascen$a2;
        if (v2.tag === "Cons" && cmp(v)(v2._1) === "GT") {
          $sequedesceascen$b = 1;
          $sequedesceascen$a0 = v2._1;
          $sequedesceascen$a1 = $List("Cons", v, v1);
          $sequedesceascen$a2 = v2._2;
          continue;
        }
        $sequedesceascen$c = false;
        $sequedesceascen$r = $List("Cons", $List("Cons", v, v1), sequences(v2));
        continue;
      }
      if ($sequedesceascen$b === 2) {
        const v = $sequedesceascen$a0, v1 = $sequedesceascen$a1, v2 = $sequedesceascen$a2;
        if (v2.tag === "Cons" && (() => {
          const $0 = cmp(v)(v2._1);
          return $0 === "LT" || $0 !== "GT";
        })()) {
          $sequedesceascen$b = 2;
          $sequedesceascen$a0 = v2._1;
          $sequedesceascen$a1 = (ys) => v1($List("Cons", v, ys));
          $sequedesceascen$a2 = v2._2;
          continue;
        }
        $sequedesceascen$c = false;
        $sequedesceascen$r = $List("Cons", v1($List("Cons", v, Nil)), sequences(v2));
      }
    }
    return $sequedesceascen$r;
  };
  const sequences = (v) => $sequedesceascen(0, v);
  const descending = (v) => (v1) => (v2) => $sequedesceascen(1, v, v1, v2);
  const ascending = (v) => (v1) => (v2) => $sequedesceascen(2, v, v1, v2);
  return (x) => mergeAll(sequences(x));
};
var reverse = /* @__PURE__ */ (() => {
  const go = (go$a0$copy) => (go$a1$copy) => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v1.tag === "Nil") {
        go$c = false;
        go$r = v;
        continue;
      }
      if (v1.tag === "Cons") {
        go$a0 = $List("Cons", v1._1, v);
        go$a1 = v1._2;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go(Nil);
})();
var take2 = /* @__PURE__ */ (() => {
  const go = (go$a0$copy) => (go$a1$copy) => (go$a2$copy) => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1, v2 = go$a2;
      if (v1 < 1) {
        go$c = false;
        go$r = reverse(v);
        continue;
      }
      if (v2.tag === "Nil") {
        go$c = false;
        go$r = reverse(v);
        continue;
      }
      if (v2.tag === "Cons") {
        go$a0 = $List("Cons", v2._1, v);
        go$a1 = v1 - 1 | 0;
        go$a2 = v2._2;
        continue;
      }
      fail();
    }
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
    return go$r;
  };
  const $0 = go(lst)(Nil);
  if ($0.tag === "Just") {
    return $Maybe("Just", { init: reverse($0._1.revInit), last: $0._1.last });
  }
  return Nothing;
};
var zipWith = (f) => (xs) => (ys) => {
  const go = (go$a0$copy) => (go$a1$copy) => (go$a2$copy) => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1, v2 = go$a2;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = v2;
        continue;
      }
      if (v1.tag === "Nil") {
        go$c = false;
        go$r = v2;
        continue;
      }
      if (v.tag === "Cons" && v1.tag === "Cons") {
        go$a0 = v._2;
        go$a1 = v1._2;
        go$a2 = $List("Cons", f(v._1)(v1._1), v2);
        continue;
      }
      fail();
    }
    return go$r;
  };
  return reverse(go(xs)(ys)(Nil));
};
var range = (start) => (end) => {
  if (start === end) {
    return $List("Cons", start, Nil);
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
    }
    return go$r;
  };
  return go(end)(start)(start > end ? 1 : -1)(Nil);
};
var mapMaybe = (f) => {
  const go = (go$a0$copy) => (go$a1$copy) => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v1.tag === "Nil") {
        go$c = false;
        go$r = reverse(v);
        continue;
      }
      if (v1.tag === "Cons") {
        const v2 = f(v1._1);
        if (v2.tag === "Nothing") {
          go$a0 = v;
          go$a1 = v1._2;
          continue;
        }
        if (v2.tag === "Just") {
          go$a0 = $List("Cons", v2._1, v);
          go$a1 = v1._2;
          continue;
        }
      }
      fail();
    }
    return go$r;
  };
  return go(Nil);
};
var manyRec = (dictMonadRec) => (dictAlternative) => {
  const Alt0 = dictAlternative.Plus1().Alt0();
  const $0 = dictAlternative.Applicative0();
  return (p) => dictMonadRec.tailRecM((acc) => dictMonadRec.Monad0().Bind1().bind(Alt0.alt(Alt0.Functor0().map(Loop)(p))($0.pure($Step(
    "Done",
    void 0
  ))))((aa) => $0.pure((() => {
    if (aa.tag === "Loop") {
      return $Step("Loop", $List("Cons", aa._1, acc));
    }
    if (aa.tag === "Done") {
      return $Step("Done", reverse(acc));
    }
    fail();
  })())))(Nil);
};
var some = (dictAlternative) => (dictLazy) => (v) => dictAlternative.Applicative0().Apply0().apply(dictAlternative.Plus1().Alt0().Functor0().map(Cons)(v))(dictLazy.defer((v1) => many(dictAlternative)(dictLazy)(v)));
var many = (dictAlternative) => (dictLazy) => (v) => dictAlternative.Plus1().Alt0().alt(some(dictAlternative)(dictLazy)(v))(dictAlternative.Applicative0().pure(Nil));
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
var drop2 = (drop$a0$copy) => (drop$a1$copy) => {
  let drop$a0 = drop$a0$copy, drop$a1 = drop$a1$copy, drop$c = true, drop$r;
  while (drop$c) {
    const v = drop$a0, v1 = drop$a1;
    if (v < 1) {
      drop$c = false;
      drop$r = v1;
      continue;
    }
    if (v1.tag === "Nil") {
      drop$c = false;
      drop$r = Nil;
      continue;
    }
    if (v1.tag === "Cons") {
      drop$a0 = v - 1 | 0;
      drop$a1 = v1._2;
      continue;
    }
    fail();
  }
  return drop$r;
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
    return go$r;
  };
  return go;
};

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

// output-es/Data.List.Lazy.Types/index.js
var $Step2 = (tag, _1, _2) => ({ tag, _1, _2 });
var Nil2 = /* @__PURE__ */ $Step2("Nil");
var nil = /* @__PURE__ */ defer((v) => Nil2);
var foldableList2 = {
  foldr: (op) => (z) => (xs) => foldableList2.foldl((b) => (a) => op(a)(b))(z)(foldableList2.foldl((b) => (a) => defer((v) => $Step2("Cons", a, b)))(nil)(xs)),
  foldl: (op) => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, xs = go$a1;
        const v = force(xs);
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = op(b)(v._1);
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      return go$r;
    };
    return go;
  },
  foldMap: (dictMonoid) => {
    const mempty = dictMonoid.mempty;
    return (f) => foldableList2.foldl((b) => (a) => dictMonoid.Semigroup0().append(b)(f(a)))(mempty);
  }
};
var unfoldable1List2 = {
  unfoldr1: /* @__PURE__ */ (() => {
    const go = (f) => (b) => defer((x) => force((() => {
      const v1 = f(b);
      if (v1._2.tag === "Just") {
        const $0 = v1._1;
        const $1 = go(f)(v1._2._1);
        return defer((v) => $Step2("Cons", $0, $1));
      }
      if (v1._2.tag === "Nothing") {
        const $0 = v1._1;
        return defer((v) => $Step2("Cons", $0, nil));
      }
      fail();
    })()));
    return go;
  })()
};
var unfoldableList2 = {
  unfoldr: /* @__PURE__ */ (() => {
    const go = (f) => (b) => defer((x) => force((() => {
      const v1 = f(b);
      if (v1.tag === "Nothing") {
        return nil;
      }
      if (v1.tag === "Just") {
        const $0 = v1._1._1;
        const $1 = go(f)(v1._1._2);
        return defer((v) => $Step2("Cons", $0, $1));
      }
      fail();
    })()));
    return go;
  })(),
  Unfoldable10: () => unfoldable1List2
};

// output-es/Data.List.Lazy/index.js
var filter = (p) => {
  const go = (go$a0$copy) => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = Nil2;
        continue;
      }
      if (v.tag === "Cons") {
        if (p(v._1)) {
          go$c = false;
          go$r = $Step2("Cons", v._1, filter(p)(v._2));
          continue;
        }
        go$a0 = force(v._2);
        continue;
      }
      fail();
    }
    return go$r;
  };
  return (x) => defer((v) => go(force(x)));
};

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

// output-es/Data.Map.Internal/index.js
var $KickUp = (_1, _2, _3, _4) => ({ tag: "KickUp", _1, _2, _3, _4 });
var $$$Map = (tag, _1, _2, _3, _4, _5, _6, _7) => ({ tag, _1, _2, _3, _4, _5, _6, _7 });
var $TreeContext = (tag, _1, _2, _3, _4, _5, _6) => ({ tag, _1, _2, _3, _4, _5, _6 });
var Leaf2 = /* @__PURE__ */ $$$Map("Leaf");
var size = (v) => {
  if (v.tag === "Leaf") {
    return 0;
  }
  if (v.tag === "Two") {
    return (1 + size(v._1) | 0) + size(v._4) | 0;
  }
  if (v.tag === "Three") {
    return ((2 + size(v._1) | 0) + size(v._4) | 0) + size(v._7) | 0;
  }
  fail();
};
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
            $List("Cons", $$$Map("Two", Leaf2, v._1._2, v._1._3, Leaf2), $List("Cons", v._1._4, v._2))
          );
          continue;
        }
        if (v._1.tag === "Three") {
          go$a0 = $List(
            "Cons",
            v._1._1,
            $List(
              "Cons",
              $$$Map("Two", Leaf2, v._1._2, v._1._3, Leaf2),
              $List("Cons", v._1._4, $List("Cons", $$$Map("Two", Leaf2, v._1._5, v._1._6, Leaf2), $List("Cons", v._1._7, v._2)))
            )
          );
          continue;
        }
      }
      fail();
    }
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
        if (v2 === "EQ") {
          go$c = false;
          go$r = $Maybe("Just", v._3);
          continue;
        }
        if (v2 === "LT") {
          go$a0 = v._1;
          continue;
        }
        go$a0 = v._4;
        continue;
      }
      if (v.tag === "Three") {
        const v3 = dictOrd.compare(k)(v._2);
        if (v3 === "EQ") {
          go$c = false;
          go$r = $Maybe("Just", v._3);
          continue;
        }
        const v4 = dictOrd.compare(k)(v._5);
        if (v4 === "EQ") {
          go$c = false;
          go$r = $Maybe("Just", v._6);
          continue;
        }
        if (v3 === "LT") {
          go$a0 = v._1;
          continue;
        }
        if (v4 === "GT") {
          go$a0 = v._7;
          continue;
        }
        go$a0 = v._4;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go;
};
var fromZipper = (fromZipper$a0$copy) => (fromZipper$a1$copy) => (fromZipper$a2$copy) => {
  let fromZipper$a0 = fromZipper$a0$copy, fromZipper$a1 = fromZipper$a1$copy, fromZipper$a2 = fromZipper$a2$copy, fromZipper$c = true, fromZipper$r;
  while (fromZipper$c) {
    const dictOrd = fromZipper$a0, v = fromZipper$a1, v1 = fromZipper$a2;
    if (v.tag === "Nil") {
      fromZipper$c = false;
      fromZipper$r = v1;
      continue;
    }
    if (v.tag === "Cons") {
      if (v._1.tag === "TwoLeft") {
        fromZipper$a0 = dictOrd;
        fromZipper$a1 = v._2;
        fromZipper$a2 = $$$Map("Two", v1, v._1._1, v._1._2, v._1._3);
        continue;
      }
      if (v._1.tag === "TwoRight") {
        fromZipper$a0 = dictOrd;
        fromZipper$a1 = v._2;
        fromZipper$a2 = $$$Map("Two", v._1._1, v._1._2, v._1._3, v1);
        continue;
      }
      if (v._1.tag === "ThreeLeft") {
        fromZipper$a0 = dictOrd;
        fromZipper$a1 = v._2;
        fromZipper$a2 = $$$Map("Three", v1, v._1._1, v._1._2, v._1._3, v._1._4, v._1._5, v._1._6);
        continue;
      }
      if (v._1.tag === "ThreeMiddle") {
        fromZipper$a0 = dictOrd;
        fromZipper$a1 = v._2;
        fromZipper$a2 = $$$Map("Three", v._1._1, v._1._2, v._1._3, v1, v._1._4, v._1._5, v._1._6);
        continue;
      }
      if (v._1.tag === "ThreeRight") {
        fromZipper$a0 = dictOrd;
        fromZipper$a1 = v._2;
        fromZipper$a2 = $$$Map("Three", v._1._1, v._1._2, v._1._3, v._1._4, v._1._5, v._1._6, v1);
        continue;
      }
    }
    fail();
  }
  return fromZipper$r;
};
var insert = (dictOrd) => (k) => (v) => {
  const up = (up$a0$copy) => (up$a1$copy) => {
    let up$a0 = up$a0$copy, up$a1 = up$a1$copy, up$c = true, up$r;
    while (up$c) {
      const v1 = up$a0, v2 = up$a1;
      if (v1.tag === "Nil") {
        up$c = false;
        up$r = $$$Map("Two", v2._1, v2._2, v2._3, v2._4);
        continue;
      }
      if (v1.tag === "Cons") {
        if (v1._1.tag === "TwoLeft") {
          up$c = false;
          up$r = fromZipper(dictOrd)(v1._2)($$$Map("Three", v2._1, v2._2, v2._3, v2._4, v1._1._1, v1._1._2, v1._1._3));
          continue;
        }
        if (v1._1.tag === "TwoRight") {
          up$c = false;
          up$r = fromZipper(dictOrd)(v1._2)($$$Map("Three", v1._1._1, v1._1._2, v1._1._3, v2._1, v2._2, v2._3, v2._4));
          continue;
        }
        if (v1._1.tag === "ThreeLeft") {
          up$a0 = v1._2;
          up$a1 = $KickUp($$$Map("Two", v2._1, v2._2, v2._3, v2._4), v1._1._1, v1._1._2, $$$Map("Two", v1._1._3, v1._1._4, v1._1._5, v1._1._6));
          continue;
        }
        if (v1._1.tag === "ThreeMiddle") {
          up$a0 = v1._2;
          up$a1 = $KickUp($$$Map("Two", v1._1._1, v1._1._2, v1._1._3, v2._1), v2._2, v2._3, $$$Map("Two", v2._4, v1._1._4, v1._1._5, v1._1._6));
          continue;
        }
        if (v1._1.tag === "ThreeRight") {
          up$a0 = v1._2;
          up$a1 = $KickUp($$$Map("Two", v1._1._1, v1._1._2, v1._1._3, v1._1._4), v1._1._5, v1._1._6, $$$Map("Two", v2._1, v2._2, v2._3, v2._4));
          continue;
        }
      }
      fail();
    }
    return up$r;
  };
  const down = (down$a0$copy) => (down$a1$copy) => {
    let down$a0 = down$a0$copy, down$a1 = down$a1$copy, down$c = true, down$r;
    while (down$c) {
      const v1 = down$a0, v2 = down$a1;
      if (v2.tag === "Leaf") {
        down$c = false;
        down$r = up(v1)($KickUp(Leaf2, k, v, Leaf2));
        continue;
      }
      if (v2.tag === "Two") {
        const v3 = dictOrd.compare(k)(v2._2);
        if (v3 === "EQ") {
          down$c = false;
          down$r = fromZipper(dictOrd)(v1)($$$Map("Two", v2._1, k, v, v2._4));
          continue;
        }
        if (v3 === "LT") {
          down$a0 = $List("Cons", $TreeContext("TwoLeft", v2._2, v2._3, v2._4), v1);
          down$a1 = v2._1;
          continue;
        }
        down$a0 = $List("Cons", $TreeContext("TwoRight", v2._1, v2._2, v2._3), v1);
        down$a1 = v2._4;
        continue;
      }
      if (v2.tag === "Three") {
        const v3 = dictOrd.compare(k)(v2._2);
        if (v3 === "EQ") {
          down$c = false;
          down$r = fromZipper(dictOrd)(v1)($$$Map("Three", v2._1, k, v, v2._4, v2._5, v2._6, v2._7));
          continue;
        }
        const v4 = dictOrd.compare(k)(v2._5);
        if (v4 === "EQ") {
          down$c = false;
          down$r = fromZipper(dictOrd)(v1)($$$Map("Three", v2._1, v2._2, v2._3, v2._4, k, v, v2._7));
          continue;
        }
        if (v3 === "LT") {
          down$a0 = $List("Cons", $TreeContext("ThreeLeft", v2._2, v2._3, v2._4, v2._5, v2._6, v2._7), v1);
          down$a1 = v2._1;
          continue;
        }
        if (v3 === "GT" && v4 === "LT") {
          down$a0 = $List("Cons", $TreeContext("ThreeMiddle", v2._1, v2._2, v2._3, v2._5, v2._6, v2._7), v1);
          down$a1 = v2._4;
          continue;
        }
        down$a0 = $List("Cons", $TreeContext("ThreeRight", v2._1, v2._2, v2._3, v2._4, v2._5, v2._6), v1);
        down$a1 = v2._7;
        continue;
      }
      fail();
    }
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
        const $0 = ctxs._2;
        const $1 = (a, b, c, d, k1, k2, k3, v1, v2, v3) => fromZipper(dictOrd)($0)($$$Map("Two", $$$Map("Two", a, k1, v1, b), k2, v2, $$$Map("Two", c, k3, v3, d)));
        const $2 = (a, b, c, d, k1, k2, k3, v1, v2, v3) => fromZipper(dictOrd)($0)($$$Map("Two", $$$Map("Two", a, k1, v1, b), k2, v2, $$$Map("Two", c, k3, v3, d)));
        const $3 = (a, b, c, d, k1, k2, k3, v1, v2, v3) => fromZipper(dictOrd)($0)($$$Map("Two", $$$Map("Three", a, k1, v1, b, k2, v2, c), k3, v3, d));
        const $4 = (a, b, c, d, k1, k2, k3, v1, v2, v3) => fromZipper(dictOrd)($0)($$$Map("Two", $$$Map("Three", a, k1, v1, b, k2, v2, c), k3, v3, d));
        const $5 = (a, b, c, d, k1, k2, k3, v1, v2, v3) => fromZipper(dictOrd)($0)($$$Map("Two", a, k1, v1, $$$Map("Three", b, k2, v2, c, k3, v3, d)));
        const $6 = (a, b, c, d, k1, k2, k3, v1, v2, v3) => fromZipper(dictOrd)($0)($$$Map("Two", a, k1, v1, $$$Map("Three", b, k2, v2, c, k3, v3, d)));
        const $7 = (a, b, c, d, e, k1, k2, k3, k4, v1, v2, v3, v4) => fromZipper(dictOrd)($0)($$$Map(
          "Three",
          $$$Map("Two", a, k1, v1, b),
          k2,
          v2,
          $$$Map("Two", c, k3, v3, d),
          k4,
          v4,
          e
        ));
        const $8 = (a, b, c, d, e, k1, k2, k3, k4, v1, v2, v3, v4) => fromZipper(dictOrd)($0)($$$Map(
          "Three",
          $$$Map("Two", a, k1, v1, b),
          k2,
          v2,
          $$$Map("Two", c, k3, v3, d),
          k4,
          v4,
          e
        ));
        const $9 = (a, b, c, d, e, k1, k2, k3, k4, v1, v2, v3, v4) => fromZipper(dictOrd)($0)($$$Map(
          "Three",
          a,
          k1,
          v1,
          $$$Map("Two", b, k2, v2, c),
          k3,
          v3,
          $$$Map("Two", d, k4, v4, e)
        ));
        const $10 = (a, b, c, d, e, k1, k2, k3, k4, v1, v2, v3, v4) => fromZipper(dictOrd)($0)($$$Map(
          "Three",
          a,
          k1,
          v1,
          $$$Map("Two", b, k2, v2, c),
          k3,
          v3,
          $$$Map("Two", d, k4, v4, e)
        ));
        if (tree.tag === "Leaf") {
          if (ctxs._1.tag === "TwoLeft") {
            if (ctxs._1._3.tag === "Leaf") {
              up$c = false;
              up$r = fromZipper(dictOrd)($0)($$$Map("Two", Leaf2, ctxs._1._1, ctxs._1._2, Leaf2));
              continue;
            }
            if (ctxs._1._3.tag === "Two") {
              up$a0 = $0;
              up$a1 = $$$Map("Three", tree, ctxs._1._1, ctxs._1._2, ctxs._1._3._1, ctxs._1._3._2, ctxs._1._3._3, ctxs._1._3._4);
              continue;
            }
            if (ctxs._1._3.tag === "Three") {
              up$c = false;
              up$r = $1(tree, ctxs._1._3._1, ctxs._1._3._4, ctxs._1._3._7, ctxs._1._1, ctxs._1._3._2, ctxs._1._3._5, ctxs._1._2, ctxs._1._3._3, ctxs._1._3._6);
              continue;
            }
            up$c = false;
            up$r = _crashWith("The impossible happened in partial function `up`.");
            continue;
          }
          if (ctxs._1.tag === "TwoRight") {
            if (ctxs._1._1.tag === "Leaf") {
              up$c = false;
              up$r = fromZipper(dictOrd)($0)($$$Map("Two", Leaf2, ctxs._1._2, ctxs._1._3, Leaf2));
              continue;
            }
            if (ctxs._1._1.tag === "Two") {
              up$a0 = $0;
              up$a1 = $$$Map("Three", ctxs._1._1._1, ctxs._1._1._2, ctxs._1._1._3, ctxs._1._1._4, ctxs._1._2, ctxs._1._3, tree);
              continue;
            }
            if (ctxs._1._1.tag === "Three") {
              up$c = false;
              up$r = $2(ctxs._1._1._1, ctxs._1._1._4, ctxs._1._1._7, tree, ctxs._1._1._2, ctxs._1._1._5, ctxs._1._2, ctxs._1._1._3, ctxs._1._1._6, ctxs._1._3);
              continue;
            }
            up$c = false;
            up$r = _crashWith("The impossible happened in partial function `up`.");
            continue;
          }
          if (ctxs._1.tag === "ThreeLeft") {
            if (ctxs._1._6.tag === "Leaf" && ctxs._1._3.tag === "Leaf") {
              up$c = false;
              up$r = fromZipper(dictOrd)($0)($$$Map("Three", Leaf2, ctxs._1._1, ctxs._1._2, Leaf2, ctxs._1._4, ctxs._1._5, Leaf2));
              continue;
            }
            if (ctxs._1._3.tag === "Two") {
              up$c = false;
              up$r = $3(tree, ctxs._1._3._1, ctxs._1._3._4, ctxs._1._6, ctxs._1._1, ctxs._1._3._2, ctxs._1._4, ctxs._1._2, ctxs._1._3._3, ctxs._1._5);
              continue;
            }
            if (ctxs._1._3.tag === "Three") {
              up$c = false;
              up$r = $7(
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
                up$r = fromZipper(dictOrd)($0)($$$Map("Three", Leaf2, ctxs._1._2, ctxs._1._3, Leaf2, ctxs._1._4, ctxs._1._5, Leaf2));
                continue;
              }
              if (ctxs._1._6.tag === "Two") {
                up$c = false;
                up$r = $5(ctxs._1._1, tree, ctxs._1._6._1, ctxs._1._6._4, ctxs._1._2, ctxs._1._4, ctxs._1._6._2, ctxs._1._3, ctxs._1._5, ctxs._1._6._3);
                continue;
              }
              if (ctxs._1._6.tag === "Three") {
                up$c = false;
                up$r = $9(
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
              up$r = $4(ctxs._1._1._1, ctxs._1._1._4, tree, ctxs._1._6, ctxs._1._1._2, ctxs._1._2, ctxs._1._4, ctxs._1._1._3, ctxs._1._3, ctxs._1._5);
              continue;
            }
            if (ctxs._1._6.tag === "Two") {
              up$c = false;
              up$r = $5(ctxs._1._1, tree, ctxs._1._6._1, ctxs._1._6._4, ctxs._1._2, ctxs._1._4, ctxs._1._6._2, ctxs._1._3, ctxs._1._5, ctxs._1._6._3);
              continue;
            }
            if (ctxs._1._1.tag === "Three") {
              up$c = false;
              up$r = $8(
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
              up$r = $9(
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
            if (ctxs._1._1.tag === "Leaf" && ctxs._1._4.tag === "Leaf") {
              up$c = false;
              up$r = fromZipper(dictOrd)($0)($$$Map("Three", Leaf2, ctxs._1._2, ctxs._1._3, Leaf2, ctxs._1._5, ctxs._1._6, Leaf2));
              continue;
            }
            if (ctxs._1._4.tag === "Two") {
              up$c = false;
              up$r = $6(ctxs._1._1, ctxs._1._4._1, ctxs._1._4._4, tree, ctxs._1._2, ctxs._1._4._2, ctxs._1._5, ctxs._1._3, ctxs._1._4._3, ctxs._1._6);
              continue;
            }
            if (ctxs._1._4.tag === "Three") {
              up$c = false;
              up$r = $10(
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
          }
          up$c = false;
          up$r = _crashWith("The impossible happened in partial function `up`.");
          continue;
        }
        if (ctxs._1.tag === "TwoLeft") {
          if (ctxs._1._3.tag === "Two") {
            up$a0 = $0;
            up$a1 = $$$Map("Three", tree, ctxs._1._1, ctxs._1._2, ctxs._1._3._1, ctxs._1._3._2, ctxs._1._3._3, ctxs._1._3._4);
            continue;
          }
          if (ctxs._1._3.tag === "Three") {
            up$c = false;
            up$r = $1(tree, ctxs._1._3._1, ctxs._1._3._4, ctxs._1._3._7, ctxs._1._1, ctxs._1._3._2, ctxs._1._3._5, ctxs._1._2, ctxs._1._3._3, ctxs._1._3._6);
            continue;
          }
          up$c = false;
          up$r = _crashWith("The impossible happened in partial function `up`.");
          continue;
        }
        if (ctxs._1.tag === "TwoRight") {
          if (ctxs._1._1.tag === "Two") {
            up$a0 = $0;
            up$a1 = $$$Map("Three", ctxs._1._1._1, ctxs._1._1._2, ctxs._1._1._3, ctxs._1._1._4, ctxs._1._2, ctxs._1._3, tree);
            continue;
          }
          if (ctxs._1._1.tag === "Three") {
            up$c = false;
            up$r = $2(ctxs._1._1._1, ctxs._1._1._4, ctxs._1._1._7, tree, ctxs._1._1._2, ctxs._1._1._5, ctxs._1._2, ctxs._1._1._3, ctxs._1._1._6, ctxs._1._3);
            continue;
          }
          up$c = false;
          up$r = _crashWith("The impossible happened in partial function `up`.");
          continue;
        }
        if (ctxs._1.tag === "ThreeLeft") {
          if (ctxs._1._3.tag === "Two") {
            up$c = false;
            up$r = $3(tree, ctxs._1._3._1, ctxs._1._3._4, ctxs._1._6, ctxs._1._1, ctxs._1._3._2, ctxs._1._4, ctxs._1._2, ctxs._1._3._3, ctxs._1._5);
            continue;
          }
          if (ctxs._1._3.tag === "Three") {
            up$c = false;
            up$r = $7(
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
            up$r = $4(ctxs._1._1._1, ctxs._1._1._4, tree, ctxs._1._6, ctxs._1._1._2, ctxs._1._2, ctxs._1._4, ctxs._1._1._3, ctxs._1._3, ctxs._1._5);
            continue;
          }
          if (ctxs._1._6.tag === "Two") {
            up$c = false;
            up$r = $5(ctxs._1._1, tree, ctxs._1._6._1, ctxs._1._6._4, ctxs._1._2, ctxs._1._4, ctxs._1._6._2, ctxs._1._3, ctxs._1._5, ctxs._1._6._3);
            continue;
          }
          if (ctxs._1._1.tag === "Three") {
            up$c = false;
            up$r = $8(
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
            up$r = $9(
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
            up$r = $6(ctxs._1._1, ctxs._1._4._1, ctxs._1._4._4, tree, ctxs._1._2, ctxs._1._4._2, ctxs._1._5, ctxs._1._3, ctxs._1._4._3, ctxs._1._6);
            continue;
          }
          if (ctxs._1._4.tag === "Three") {
            up$c = false;
            up$r = $10(
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
        }
        up$c = false;
        up$r = _crashWith("The impossible happened in partial function `up`.");
        continue;
      }
      fail();
    }
    return up$r;
  };
  const removeMaxNode = (removeMaxNode$a0$copy) => (removeMaxNode$a1$copy) => {
    let removeMaxNode$a0 = removeMaxNode$a0$copy, removeMaxNode$a1 = removeMaxNode$a1$copy, removeMaxNode$c = true, removeMaxNode$r;
    while (removeMaxNode$c) {
      const ctx = removeMaxNode$a0, m = removeMaxNode$a1;
      if (m.tag === "Two") {
        if (m._1.tag === "Leaf" && m._4.tag === "Leaf") {
          removeMaxNode$c = false;
          removeMaxNode$r = up(ctx)(Leaf2);
          continue;
        }
        removeMaxNode$a0 = $List("Cons", $TreeContext("TwoRight", m._1, m._2, m._3), ctx);
        removeMaxNode$a1 = m._4;
        continue;
      }
      if (m.tag === "Three") {
        if (m._1.tag === "Leaf" && m._4.tag === "Leaf" && m._7.tag === "Leaf") {
          removeMaxNode$c = false;
          removeMaxNode$r = up($List("Cons", $TreeContext("TwoRight", Leaf2, m._2, m._3), ctx))(Leaf2);
          continue;
        }
        removeMaxNode$a0 = $List("Cons", $TreeContext("ThreeRight", m._1, m._2, m._3, m._4, m._5, m._6), ctx);
        removeMaxNode$a1 = m._7;
        continue;
      }
      removeMaxNode$c = false;
      removeMaxNode$r = _crashWith("The impossible happened in partial function `removeMaxNode`.");
    }
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
    }
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
        if (v === "EQ") {
          if (m._4.tag === "Leaf") {
            down$c = false;
            down$r = $Maybe("Just", $Tuple(m._3, up(ctx)(Leaf2)));
            continue;
          }
          const max3 = maxNode(m._1);
          down$c = false;
          down$r = $Maybe(
            "Just",
            $Tuple(m._3, removeMaxNode($List("Cons", $TreeContext("TwoLeft", max3.key, max3.value, m._4), ctx))(m._1))
          );
          continue;
        }
        if (v === "LT") {
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
        if (m._1.tag === "Leaf" && m._4.tag === "Leaf" && m._7.tag === "Leaf") {
          if (v3 === "EQ") {
            down$c = false;
            down$r = $Maybe("Just", $Tuple(m._3, fromZipper(dictOrd)(ctx)($$$Map("Two", Leaf2, m._5, m._6, Leaf2))));
            continue;
          }
          if (v === "EQ") {
            down$c = false;
            down$r = $Maybe("Just", $Tuple(m._6, fromZipper(dictOrd)(ctx)($$$Map("Two", Leaf2, m._2, m._3, Leaf2))));
            continue;
          }
          if (v3 === "LT") {
            down$a0 = $List("Cons", $TreeContext("ThreeLeft", m._2, m._3, m._4, m._5, m._6, m._7), ctx);
            down$a1 = m._1;
            continue;
          }
          if (v3 === "GT" && v === "LT") {
            down$a0 = $List("Cons", $TreeContext("ThreeMiddle", m._1, m._2, m._3, m._5, m._6, m._7), ctx);
            down$a1 = m._4;
            continue;
          }
          down$a0 = $List("Cons", $TreeContext("ThreeRight", m._1, m._2, m._3, m._4, m._5, m._6), ctx);
          down$a1 = m._7;
          continue;
        }
        if (v3 === "EQ") {
          const max3 = maxNode(m._1);
          down$c = false;
          down$r = $Maybe(
            "Just",
            $Tuple(m._3, removeMaxNode($List("Cons", $TreeContext("ThreeLeft", max3.key, max3.value, m._4, m._5, m._6, m._7), ctx))(m._1))
          );
          continue;
        }
        if (v === "EQ") {
          const max3 = maxNode(m._4);
          down$c = false;
          down$r = $Maybe(
            "Just",
            $Tuple(m._6, removeMaxNode($List("Cons", $TreeContext("ThreeMiddle", m._1, m._2, m._3, max3.key, max3.value, m._7), ctx))(m._4))
          );
          continue;
        }
        if (v3 === "LT") {
          down$a0 = $List("Cons", $TreeContext("ThreeLeft", m._2, m._3, m._4, m._5, m._6, m._7), ctx);
          down$a1 = m._1;
          continue;
        }
        if (v3 === "GT" && v === "LT") {
          down$a0 = $List("Cons", $TreeContext("ThreeMiddle", m._1, m._2, m._3, m._5, m._6, m._7), ctx);
          down$a1 = m._4;
          continue;
        }
        down$a0 = $List("Cons", $TreeContext("ThreeRight", m._1, m._2, m._3, m._4, m._5, m._6), ctx);
        down$a1 = m._7;
        continue;
      }
      fail();
    }
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
    const mempty = dictMonoid.mempty;
    const $0 = dictMonoid.Semigroup0();
    return (f) => (m) => {
      if (m.tag === "Leaf") {
        return mempty;
      }
      if (m.tag === "Two") {
        return $0.append(foldableMap.foldMap(dictMonoid)(f)(m._1))($0.append(f(m._3))(foldableMap.foldMap(dictMonoid)(f)(m._4)));
      }
      if (m.tag === "Three") {
        return $0.append(foldableMap.foldMap(dictMonoid)(f)(m._1))($0.append(f(m._3))($0.append(foldableMap.foldMap(dictMonoid)(f)(m._4))($0.append(f(m._6))(foldableMap.foldMap(dictMonoid)(f)(m._7)))));
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
    const mempty = dictMonoid.mempty;
    const $0 = dictMonoid.Semigroup0();
    return (f) => (m) => {
      if (m.tag === "Leaf") {
        return mempty;
      }
      if (m.tag === "Two") {
        return $0.append(foldableWithIndexMap.foldMapWithIndex(dictMonoid)(f)(m._1))($0.append(f(m._2)(m._3))(foldableWithIndexMap.foldMapWithIndex(dictMonoid)(f)(m._4)));
      }
      if (m.tag === "Three") {
        return $0.append(foldableWithIndexMap.foldMapWithIndex(dictMonoid)(f)(m._1))($0.append(f(m._2)(m._3))($0.append(foldableWithIndexMap.foldMapWithIndex(dictMonoid)(f)(m._4))($0.append(f(m._5)(m._6))(foldableWithIndexMap.foldMapWithIndex(dictMonoid)(f)(m._7)))));
      }
      fail();
    };
  },
  Foldable0: () => foldableMap
};
var findMin = /* @__PURE__ */ (() => {
  const go = (go$a0$copy) => (go$a1$copy) => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v1.tag === "Leaf") {
        go$c = false;
        go$r = v;
        continue;
      }
      if (v1.tag === "Two") {
        go$a0 = $Maybe("Just", { key: v1._2, value: v1._3 });
        go$a1 = v1._1;
        continue;
      }
      if (v1.tag === "Three") {
        go$a0 = $Maybe("Just", { key: v1._2, value: v1._3 });
        go$a1 = v1._1;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go(Nothing);
})();
var eqMap = (dictEq) => (dictEq1) => {
  const eq1 = eqArrayImpl((x) => (y) => dictEq.eq(x._1)(y._1) && dictEq1.eq(x._2)(y._2));
  return { eq: (m1) => (m2) => eq1(toUnfoldable2(unfoldableArray)(m1))(toUnfoldable2(unfoldableArray)(m2)) };
};
var fromFoldable = (dictOrd) => (dictFoldable) => dictFoldable.foldl((m) => (v) => insert(dictOrd)(v._1)(v._2)(m))(Leaf2);
var filterWithKey = (dictOrd) => {
  const fromFoldable112 = fromFoldable(dictOrd)(foldableList2);
  return (predicate) => {
    const $0 = filter((v) => predicate(v._1)(v._2));
    return (x) => fromFoldable112($0(toUnfoldable2(unfoldableList2)(x)));
  };
};
var $$delete = (dictOrd) => (k) => (m) => {
  const $0 = pop(dictOrd)(k)(m);
  if ($0.tag === "Nothing") {
    return m;
  }
  if ($0.tag === "Just") {
    return $0._1._2;
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
  const $0 = f(v);
  return (x) => $Maybe(
    "Just",
    (() => {
      if (x.tag === "Nothing") {
        return v;
      }
      if (x.tag === "Just") {
        return $0(x._1);
      }
      fail();
    })()
  );
})())(k)(m))(m2)(m1);

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
    const $0 = f(a);
    return () => {
      const $1 = $0();
      let r = $1;
      while ((() => {
        const $22 = r;
        return $22.tag === "Loop";
      })()) {
        const v = r;
        if (v.tag === "Loop") {
          const e = f(v._1)();
          r = e;
          continue;
        }
        if (v.tag === "Done") {
          continue;
        }
        fail();
      }
      const $2 = r;
      if ($2.tag === "Done") {
        return $2._1;
      }
      fail();
    };
  },
  Monad0: () => monadST
};

// output-es/Data.Array.ST/foreign.js
var sortByImpl = function() {
  function mergeFromTo(compare, fromOrdering, xs1, xs2, from, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
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
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      } else {
        xs1[k++] = x;
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
  function concat22(xs) {
    return function(ys) {
      return xs.concat(ys);
    };
  }
  return function(apply4) {
    return function(map2) {
      return function(pure2) {
        return function(f) {
          return function(array) {
            function go(bot, top) {
              switch (top - bot) {
                case 0:
                  return pure2([]);
                case 1:
                  return map2(array1)(f(array[bot]));
                case 2:
                  return apply4(map2(array2)(f(array[bot])))(f(array[bot + 1]));
                case 3:
                  return apply4(apply4(map2(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                default:
                  var pivot = bot + Math.floor((top - bot) / 4) * 2;
                  return apply4(map2(concat22)(go(bot, pivot)))(go(pivot, top));
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
var identity7 = (x) => x;
var traversableTuple = {
  traverse: (dictApplicative) => (f) => (v) => dictApplicative.Apply0().Functor0().map(Tuple(v._1))(f(v._2)),
  sequence: (dictApplicative) => (v) => dictApplicative.Apply0().Functor0().map(Tuple(v._1))(v._2),
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
var range2 = function(start) {
  return function(end) {
    var step = start > end ? -1 : 1;
    var result = new Array(step * (end - start) + 1);
    var i = start, n = 0;
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
  function Cons2(head, tail4) {
    this.head = head;
    this.tail = tail4;
  }
  var emptyList = {};
  function curryCons(head) {
    return function(tail4) {
      return new Cons2(head, tail4);
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
  return function(foldr2) {
    return function(xs) {
      return listToArray(foldr2(curryCons)(emptyList)(xs));
    };
  };
}();
var unconsImpl = function(empty3) {
  return function(next) {
    return function(xs) {
      return xs.length === 0 ? empty3({}) : next(xs[0])(xs.slice(1));
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
var concat = function(xss) {
  if (xss.length <= 1e4) {
    return Array.prototype.concat.apply([], xss);
  }
  var result = [];
  for (var i = 0, l = xss.length; i < l; i++) {
    var xs = xss[i];
    for (var j = 0, m = xs.length; j < m; j++) {
      result.push(xs[j]);
    }
  }
  return result;
};
var filter2 = function(f) {
  return function(xs) {
    return xs.filter(f);
  };
};
var sortByImpl2 = function() {
  function mergeFromTo(compare, fromOrdering, xs1, xs2, from, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
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
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      } else {
        xs1[k++] = x;
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
var slice2 = function(s) {
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
var updateAt = /* @__PURE__ */ _updateAt(Just)(Nothing);
var uncons2 = /* @__PURE__ */ unconsImpl((v) => Nothing)((x) => (xs) => $Maybe("Just", { head: x, tail: xs }));
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
  if (v === "GT") {
    return 1;
  }
  if (v === "EQ") {
    return 0;
  }
  if (v === "LT") {
    return -1;
  }
  fail();
});
var sortWith = (dictOrd) => (f) => sortBy2((x) => (y) => dictOrd.compare(f(x))(f(y)));
var index = /* @__PURE__ */ indexImpl(Just)(Nothing);
var findIndex = /* @__PURE__ */ findIndexImpl(Just)(Nothing);
var notElem = (dictEq) => (a) => (arr) => {
  const $0 = findIndex((v) => dictEq.eq(v)(a))(arr);
  if ($0.tag === "Nothing") {
    return true;
  }
  if ($0.tag === "Just") {
    return false;
  }
  fail();
};
var elem = (dictEq) => (a) => (arr) => {
  const $0 = findIndex((v) => dictEq.eq(v)(a))(arr);
  if ($0.tag === "Nothing") {
    return false;
  }
  if ($0.tag === "Just") {
    return true;
  }
  fail();
};
var cons3 = (x) => (xs) => [x, ...xs];
var some2 = (dictAlternative) => (dictLazy) => (v) => dictAlternative.Applicative0().Apply0().apply(dictAlternative.Plus1().Alt0().Functor0().map(cons3)(v))(dictLazy.defer((v1) => many2(dictAlternative)(dictLazy)(v)));
var many2 = (dictAlternative) => (dictLazy) => (v) => dictAlternative.Plus1().Alt0().alt(some2(dictAlternative)(dictLazy)(v))(dictAlternative.Applicative0().pure([]));

// output-es/Data.Set/index.js
var fromFoldable1 = /* @__PURE__ */ (() => fromFoldableImpl(foldableList.foldr))();
var tailRecM2 = (f) => (a) => (b) => monadRecST.tailRecM((o) => f(o.a)(o.b))({ a, b });
var union = (dictOrd) => (v) => (v1) => unionWith(dictOrd)($$const)(v)(v1);
var toUnfoldable4 = (dictUnfoldable) => {
  const $0 = toUnfoldable(dictUnfoldable);
  return (x) => $0(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(x));
};
var toUnfoldable1 = /* @__PURE__ */ toUnfoldable4(unfoldableArray);
var size2 = (v) => size(v);
var showSet = (dictShow) => ({ show: (s) => "(fromFoldable " + showArrayImpl(dictShow.show)(toUnfoldable1(s)) + ")" });
var member = (dictOrd) => (a) => (v) => {
  const $0 = lookup(dictOrd)(a)(v);
  if ($0.tag === "Nothing") {
    return false;
  }
  if ($0.tag === "Just") {
    return true;
  }
  fail();
};
var isEmpty = (v) => v.tag === "Leaf";
var foldableSet = {
  foldMap: (dictMonoid) => {
    const foldMap1 = foldableList.foldMap(dictMonoid);
    return (f) => {
      const $0 = foldMap1(f);
      return (x) => $0(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(x));
    };
  },
  foldl: (f) => (x) => {
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
      return go$r;
    };
    const $0 = go(x);
    return (x$1) => $0(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(x$1));
  },
  foldr: (f) => (x) => {
    const $0 = foldableList.foldr(f)(x);
    return (x$1) => $0(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(x$1));
  }
};
var filter3 = (dictOrd) => {
  const filterWithKey3 = filterWithKey(dictOrd);
  return (f) => (v) => filterWithKey3((k) => (v1) => f(k))(v);
};
var ordSet = (dictOrd) => {
  const $0 = dictOrd.Eq0();
  const eqSet1 = { eq: (v) => (v1) => eqMap($0)(eqUnit).eq(v)(v1) };
  return {
    compare: (s1) => (s2) => ordList(dictOrd).compare(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(s1))(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List(
      "Cons",
      k,
      acc
    ))(Nil)(s2)),
    Eq0: () => eqSet1
  };
};
var intersection = (dictOrd) => {
  const fromFoldable33 = foldlArray((m) => (a) => insert(dictOrd)(a)()(m))(Leaf2);
  return (s1) => (s2) => {
    const rs = fromFoldable1(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(s2));
    const rl = rs.length;
    const ls = fromFoldable1(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(s1));
    const ll = ls.length;
    return fromFoldable33((() => {
      const acc = [];
      return tailRecM2((l) => (r) => {
        if (l < ll && r < rl) {
          const v = dictOrd.compare(ls[l])(rs[r]);
          if (v === "EQ") {
            const $0 = () => acc.push(ls[l]);
            return () => {
              $0();
              return $Step("Loop", { a: l + 1 | 0, b: r + 1 | 0 });
            };
          }
          if (v === "LT") {
            const $0 = $Step("Loop", { a: l + 1 | 0, b: r });
            return () => $0;
          }
          if (v === "GT") {
            const $0 = $Step("Loop", { a: l, b: r + 1 | 0 });
            return () => $0;
          }
          fail();
        }
        return () => $Step("Done", acc);
      })(0)(0)();
    })());
  };
};
var map = (dictOrd) => (f) => foldableSet.foldl((m) => (a) => insert(dictOrd)(f(a))()(m))(Leaf2);
var mapMaybe2 = (dictOrd) => (f) => foldableSet.foldr((a) => (acc) => {
  const $0 = f(a);
  if ($0.tag === "Nothing") {
    return acc;
  }
  if ($0.tag === "Just") {
    return insert(dictOrd)($0._1)()(acc);
  }
  fail();
})(Leaf2);
var difference2 = (dictOrd) => (s1) => (s2) => {
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
    return go$r;
  };
  return go(s1)(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(s2));
};

// output-es/Control.Monad.State.Trans/index.js
var bindStateT = (dictMonad) => ({ bind: (v) => (f) => (s) => dictMonad.Bind1().bind(v(s))((v1) => f(v1._1)(v1._2)), Apply0: () => applyStateT(dictMonad) });
var applyStateT = (dictMonad) => {
  const $0 = dictMonad.Bind1().Apply0().Functor0();
  const functorStateT1 = { map: (f) => (v) => (s) => $0.map((v1) => $Tuple(f(v1._1), v1._2))(v(s)) };
  return {
    apply: (() => {
      const $1 = bindStateT(dictMonad);
      return (f) => (a) => $1.bind(f)((f$p) => $1.bind(a)((a$p) => applicativeStateT(dictMonad).pure(f$p(a$p))));
    })(),
    Functor0: () => functorStateT1
  };
};
var applicativeStateT = (dictMonad) => ({ pure: (a) => (s) => dictMonad.Applicative0().pure($Tuple(a, s)), Apply0: () => applyStateT(dictMonad) });
var monadRecStateT = (dictMonadRec) => {
  const Monad0 = dictMonadRec.Monad0();
  const monadStateT1 = { Applicative0: () => applicativeStateT(Monad0), Bind1: () => bindStateT(Monad0) };
  return {
    tailRecM: (f) => (a) => (s) => dictMonadRec.tailRecM((v) => Monad0.Bind1().bind(f(v._1)(v._2))((v2) => Monad0.Applicative0().pure((() => {
      if (v2._1.tag === "Loop") {
        return $Step("Loop", $Tuple(v2._1._1, v2._2));
      }
      if (v2._1.tag === "Done") {
        return $Step("Done", $Tuple(v2._1._1, v2._2));
      }
      fail();
    })())))($Tuple(a, s)),
    Monad0: () => monadStateT1
  };
};
var monadStateStateT = (dictMonad) => {
  const monadStateT1 = { Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) };
  return { state: (f) => (x) => dictMonad.Applicative0().pure(f(x)), Monad0: () => monadStateT1 };
};
var monadThrowStateT = (dictMonadThrow) => {
  const Monad0 = dictMonadThrow.Monad0();
  const monadStateT1 = { Applicative0: () => applicativeStateT(Monad0), Bind1: () => bindStateT(Monad0) };
  return {
    throwError: (e) => {
      const $0 = dictMonadThrow.throwError(e);
      return (s) => Monad0.Bind1().bind($0)((x) => Monad0.Applicative0().pure($Tuple(x, s)));
    },
    Monad0: () => monadStateT1
  };
};
var monadErrorStateT = (dictMonadError) => {
  const monadThrowStateT1 = monadThrowStateT(dictMonadError.MonadThrow0());
  return { catchError: (v) => (h) => (s) => dictMonadError.catchError(v(s))((e) => h(e)(s)), MonadThrow0: () => monadThrowStateT1 };
};

// output-es/Effect.Aff.Class/index.js
var monadAffAff = { liftAff: (x) => x, MonadEffect0: () => monadEffectAff };

// output-es/Effect.Console/foreign.js
var log2 = function(s) {
  return function() {
    console.log(s);
  };
};

// output-es/Data.CodePoint.Unicode.Internal/index.js
var $UnicodeCategory = (tag) => tag;
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
var bsearch = (a) => (array) => (size6) => (compare) => {
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
      if (v === "EQ") {
        go$c = false;
        go$r = $Maybe("Just", b);
        continue;
      }
      if (v === "GT") {
        go$a0 = j + 1 | 0;
        go$a1 = k;
        continue;
      }
      go$a0 = i;
      go$a1 = j - 1 | 0;
    }
    return go$r;
  };
  return go(0)(size6);
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
var getRule = (blocks) => (unichar) => (size6) => {
  const maybeCharBlock = bsearch({ start: unichar, length: 1, convRule: nullrule })(blocks)(size6)(blkCmp);
  if (maybeCharBlock.tag === "Nothing") {
    return Nothing;
  }
  if (maybeCharBlock.tag === "Just") {
    return $Maybe("Just", maybeCharBlock._1.convRule);
  }
  fail();
};
var caseConv = (f) => ($$char) => {
  const maybeConversionRule = getRule(convchars)($$char)(1332);
  if (maybeConversionRule.tag === "Nothing") {
    return $$char;
  }
  if (maybeConversionRule.tag === "Just") {
    return $$char + f(maybeConversionRule._1) | 0;
  }
  fail();
};
var uTowlower = /* @__PURE__ */ caseConv((v) => v.lowdist);
var uTowupper = /* @__PURE__ */ caseConv((v) => v.updist);
var checkAttrS = (categories) => ($$char) => {
  const maybeConversionRule = getRule(spacechars)($$char)(7);
  if (maybeConversionRule.tag === "Nothing") {
    return false;
  }
  if (maybeConversionRule.tag === "Just") {
    const $0 = maybeConversionRule._1.category;
    const $1 = findIndex((v) => v === $0)(categories);
    if ($1.tag === "Nothing") {
      return false;
    }
    if ($1.tag === "Just") {
      return true;
    }
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
var checkAttr = (categories) => ($$char) => {
  const maybeConversionRule = getRule(allchars)($$char)($$char < 256 ? 63 : 3396);
  if (maybeConversionRule.tag === "Nothing") {
    return false;
  }
  if (maybeConversionRule.tag === "Just") {
    const $0 = maybeConversionRule._1.category;
    const $1 = findIndex((v) => v === $0)(categories);
    if ($1.tag === "Nothing") {
      return false;
    }
    if ($1.tag === "Just") {
      return true;
    }
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

// output-es/Foreign.Object/foreign.js
var empty = {};
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
function size3(m) {
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
var identity9 = (x) => x;
var values = /* @__PURE__ */ toArrayWithKey((v) => (v1) => v1);
var toAscUnfoldable = (dictUnfoldable) => {
  const $0 = sortWith(ordString)(fst);
  const $1 = toArrayWithKey(Tuple);
  return (x) => toUnfoldable3(dictUnfoldable)($0($1(x)));
};
var singleton2 = (k) => (v) => {
  const $0 = {};
  $0[k] = v;
  return $0;
};
var mutate = (f) => (m) => {
  const s = { ...m };
  f(s)();
  return s;
};
var member2 = ($0) => ($1) => _lookup(false, (v) => true, $0, $1);
var mapWithKey = (f) => (m) => _mapWithKey(m, f);
var lookup2 = ($0) => ($1) => _lookup(Nothing, Just, $0, $1);
var isSubmap = (dictEq) => (m1) => (m2) => all2((k) => (v) => _lookup(false, dictEq.eq(v), k, m2))(m1);
var isEmpty2 = /* @__PURE__ */ all2((v) => (v1) => false);
var insert2 = (k) => (v) => mutate(($0) => () => {
  $0[k] = v;
  return $0;
});
var functorObject = { map: (f) => (m) => _fmapObject(m, f) };
var functorWithIndexObject = { mapWithIndex: mapWithKey, Functor0: () => functorObject };
var fromFoldable2 = (dictFoldable) => {
  const fromFoldable112 = fromFoldableImpl(dictFoldable.foldr);
  return (l) => {
    const s = {};
    for (const v of fromFoldable112(l)) {
      s[v._1] = v._2;
    }
    return s;
  };
};
var foldM = (dictMonad) => {
  const bind1 = dictMonad.Bind1().bind;
  return (f) => (z) => _foldM(bind1)(f)(dictMonad.Applicative0().pure(z));
};
var foldM1 = /* @__PURE__ */ foldM(monadST);
var union2 = (m) => mutate((s) => foldM1((s$p) => (k) => (v) => () => {
  s$p[k] = v;
  return s$p;
})(s)(m));
var unionWith2 = (f) => (m1) => (m2) => mutate((s1) => foldM1((s2) => (k) => (v1) => {
  const $0 = _lookup(v1, (v2) => f(v1)(v2), k, m2);
  return () => {
    s2[k] = $0;
    return s2;
  };
})(s1)(m1))(m2);
var fold = /* @__PURE__ */ _foldM(applyFlipped);
var foldMap = (dictMonoid) => {
  const mempty = dictMonoid.mempty;
  return (f) => fold((acc) => (k) => (v) => dictMonoid.Semigroup0().append(acc)(f(k)(v)))(mempty);
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
    return (f) => (ms) => fold((acc) => (k) => (v) => Apply0.apply(Apply0.Functor0().map((b) => (a) => mutate(($0) => () => {
      $0[k] = a;
      return $0;
    })(b))(acc))(f(k)(v)))(dictApplicative.pure(empty))(ms);
  },
  FunctorWithIndex0: () => functorWithIndexObject,
  FoldableWithIndex1: () => foldableWithIndexObject,
  Traversable2: () => traversableObject
};
var traversableObject = {
  traverse: (dictApplicative) => {
    const $0 = traversableWithIndexObject.traverseWithIndex(dictApplicative);
    return (x) => $0((v) => x);
  },
  sequence: (dictApplicative) => traversableObject.traverse(dictApplicative)(identity9),
  Functor0: () => functorObject,
  Foldable1: () => foldableObject
};
var filterWithKey2 = (predicate) => (m) => {
  const m$p = {};
  return foldM1((acc) => (k) => (v) => {
    if (predicate(k)(v)) {
      return () => {
        acc[k] = v;
        return acc;
      };
    }
    return () => acc;
  })(m$p)(m)();
};
var filterKeys = (predicate) => filterWithKey2((x) => {
  const $0 = predicate(x);
  return (v) => $0;
});
var eqObject = (dictEq) => ({ eq: (m1) => (m2) => isSubmap(dictEq)(m1)(m2) && isSubmap(dictEq)(m2)(m1) });
var $$delete2 = (k) => mutate(($0) => () => {
  delete $0[k];
  return $0;
});

// output-es/Control.Category/index.js
var categoryFn = { identity: (x) => x, Semigroupoid0: () => semigroupoidFn };

// output-es/Data.Semigroup/foreign.js
var concatString = function(s1) {
  return function(s2) {
    return s1 + s2;
  };
};

// output-es/Data.FoldableWithIndex/index.js
var monoidEndo2 = /* @__PURE__ */ (() => {
  const semigroupEndo1 = { append: (v) => (v1) => (x) => v(v1(x)) };
  return { mempty: (x) => x, Semigroup0: () => semigroupEndo1 };
})();
var foldrWithIndexDefault = (dictFoldableWithIndex) => {
  const foldMapWithIndex1 = dictFoldableWithIndex.foldMapWithIndex(monoidEndo2);
  return (c) => (u) => (xs) => foldMapWithIndex1((i) => c(i))(xs)(u);
};

// output-es/Data.Array.NonEmpty.Internal/foreign.js
var traverse1Impl = function() {
  function Cont(fn) {
    this.fn = fn;
  }
  var emptyList = {};
  var ConsCell = function(head, tail4) {
    this.head = head;
    this.tail = tail4;
  };
  function finalCell(head) {
    return new ConsCell(head, emptyList);
  }
  function consList(x) {
    return function(xs) {
      return new ConsCell(x, xs);
    };
  }
  function listToArray(list) {
    var arr = [];
    var xs = list;
    while (xs !== emptyList) {
      arr.push(xs.head);
      xs = xs.tail;
    }
    return arr;
  }
  return function(apply4) {
    return function(map2) {
      return function(f) {
        var buildFrom = function(x, ys) {
          return apply4(map2(consList)(f(x)))(ys);
        };
        var go = function(acc, currentLen, xs) {
          if (currentLen === 0) {
            return acc;
          } else {
            var last2 = xs[currentLen - 1];
            return new Cont(function() {
              var built = go(buildFrom(last2, acc), currentLen - 1, xs);
              return built;
            });
          }
        };
        return function(array) {
          var acc = map2(finalCell)(f(array[array.length - 1]));
          var result = go(acc, array.length - 1, array);
          while (result instanceof Cont) {
            result = result.fn();
          }
          return map2(listToArray)(result);
        };
      };
    };
  };
}();

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
var unsnoc3 = (v) => {
  const v1 = unsnoc(v._2);
  if (v1.tag === "Nothing") {
    return { init: Nil, last: v._1 };
  }
  if (v1.tag === "Just") {
    return { init: $List("Cons", v._1, v1._1.init), last: v1._1.last };
  }
  fail();
};
var tail2 = (v) => v._2;
var init2 = (v) => {
  const $0 = unsnoc(v._2);
  if ($0.tag === "Just") {
    return $List("Cons", v._1, $0._1.init);
  }
  return Nil;
};

// output-es/Data.Profunctor/index.js
var profunctorFn = { dimap: (a2b) => (c2d) => (b2c) => (x) => c2d(b2c(a2b(x))) };

// output-es/Data.Profunctor.Strong/index.js
var identity12 = (x) => x;
var strongFn = /* @__PURE__ */ (() => ({ first: (a2b) => (v) => $Tuple(a2b(v._1), v._2), second: functorTuple.map, Profunctor0: () => profunctorFn }))();
var fanout = (dictCategory) => {
  const identity1 = dictCategory.identity;
  const $0 = dictCategory.Semigroupoid0();
  const $1 = dictCategory.Semigroupoid0();
  return (dictStrong) => (l) => (r) => $0.compose($1.compose(dictStrong.second(r))(dictStrong.first(l)))(dictStrong.Profunctor0().dimap(identity12)((a) => $Tuple(a, a))(identity1));
};

// output-es/Debug/foreign.js
var req = typeof module === "undefined" ? void 0 : module.require;
var util = function() {
  try {
    return req === void 0 ? void 0 : req("util");
  } catch (e) {
    return void 0;
  }
}();
function _trace(x, k) {
  if (util !== void 0) {
    console.log(util.inspect(x, { depth: null, colors: true }));
  } else {
    console.log(x);
  }
  return k({});
}
var now = function() {
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

// output-es/Util/index.js
var fanout2 = /* @__PURE__ */ fanout(categoryFn)(strongFn);
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
        go$a0 = b.init ? { init: false, acc: v._1 } : { init: false, acc: foldableList.foldr(Cons)(foldableList.foldr(Cons)(v._1)(sep))(b.acc) };
        go$a1 = v._2;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go({ init: true, acc: Nil })(xs).acc;
};
var identity13 = (x) => x;
var isEmptySet = { isEmpty };
var isEmptyObject = { isEmpty: isEmpty2 };
var unzip3 = (dictFunctor) => fanout2((v) => dictFunctor.map(fst)(v))((v) => dictFunctor.map(snd)(v));
var $$throw = (dictMonadThrow) => (x) => dictMonadThrow.throwError(error(x));
var withMsg = (dictMonadError) => {
  const throw2 = $$throw(dictMonadError.MonadThrow0());
  return (msg) => (m) => dictMonadError.catchError(m)((e) => throw2(message(e) + (msg === "" ? "" : "\n" + msg)));
};
var orElse = (dictMonadThrow) => (v) => (v1) => {
  if (v1.tag === "Nothing") {
    return dictMonadThrow.throwError(error(v));
  }
  if (v1.tag === "Just") {
    return dictMonadThrow.Monad0().Applicative0().pure(v1._1);
  }
  fail();
};
var mayFailEq = (dictMonadThrow) => (dictShow) => (dictEq) => (x) => (x$p) => orElse(dictMonadThrow)(dictShow.show(x) + " \u2260 " + dictShow.show(x$p))((() => {
  const $0 = dictEq.eq(x)(x$p);
  if (!$0) {
    return Nothing;
  }
  if ($0) {
    return $Maybe("Just", x);
  }
  fail();
})());
var definitely = (v) => (v1) => {
  if (v1.tag === "Just") {
    return v1._1;
  }
  if (v1.tag === "Nothing") {
    return throwException(error("definitely " + v))();
  }
  fail();
};
var mustEq = (dictEq) => (dictShow) => (x) => (x$p) => definitely(dictShow.show(x) + " equal to " + dictShow.show(x$p))((() => {
  const $0 = dictEq.eq(x)(x$p);
  if (!$0) {
    return Nothing;
  }
  if ($0) {
    return $Maybe("Just", x);
  }
  fail();
})());
var unsafeUpdateAt = (i) => (x) => {
  const $0 = updateAt(i)(x);
  return (x$1) => definitely("index within bounds")($0(x$1));
};
var nonEmptyListNonEmptyList = {
  nonEmpty: (x) => definitely("non-empty")((() => {
    if (x.tag === "Nil") {
      return Nothing;
    }
    if (x.tag === "Cons") {
      return $Maybe("Just", $NonEmpty(x._1, x._2));
    }
    fail();
  })()),
  init: init2,
  tail: tail2
};
var defined = (x) => {
  if (x.tag === "Right") {
    return x._1;
  }
  if (x.tag === "Left") {
    return throwException(error(showErrorImpl(x._1)))();
  }
  fail();
};
var spyWhen = (v) => (v1) => (v2) => (v3) => {
  if (v) {
    return _trace(v1 + ":", (v4) => _trace(v2(v3), (v$1) => v3));
  }
  return v3;
};
var spyFunWhenM = (dictFunctor) => (b) => (s) => (showIn) => (showOut) => (f) => (x) => dictFunctor.map(spyWhen(b)(s + " output")(showOut))(f(spyWhen(b)(s + " input")(showIn)(x)));
var spyFunWhen = (b) => (s) => (showIn) => (showOut) => (f) => spyFunWhenM(functorIdentity)(b)(s)(showIn)(showOut)((x) => f(x));
var check = (dictMonadThrow) => (v) => {
  if (!v) {
    return $$throw(dictMonadThrow);
  }
  if (v) {
    const $0 = dictMonadThrow.Monad0().Applicative0().pure();
    return (v$1) => $0;
  }
  fail();
};
var bind2Flipped = (dictMonad) => {
  const Bind1 = dictMonad.Bind1();
  const $0 = Bind1.Apply0();
  return (f) => (x) => (y) => Bind1.bind($0.apply($0.Functor0().map(f)(x))(y))(identity);
};
var assertWith = (v) => (v1) => {
  if (v1) {
    return identity13;
  }
  return (v2) => throwException(error("Assertion failure: " + v))();
};
var assertWhen = (v) => (v1) => {
  if (!v) {
    return (v$1) => identity13;
  }
  if (v) {
    return (x) => assertWith(v1)(x());
  }
  fail();
};

// output-es/Util.Set/index.js
var setSet = (dictOrd) => ({
  empty: Leaf2,
  filter: filter3(dictOrd),
  size: size2,
  difference: difference2(dictOrd),
  member: member(dictOrd),
  union: union(dictOrd),
  IsEmpty0: () => isEmptySet
});
var setObjectString = {
  empty,
  filter: filterKeys,
  size: size3,
  difference: (x) => (y) => foldlArray((b) => (a) => mutate(($0) => () => {
    delete $0[a];
    return $0;
  })(b))(x)(Object.keys(y)),
  member: member2,
  union: union2,
  IsEmpty0: () => isEmptyObject
};

// output-es/Util.Map/foreign.js
function intersectionWith_Object(f) {
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

// output-es/Util.Map/index.js
var identity14 = (x) => x;
var mapObjectString = {
  maplet: singleton2,
  keys: /* @__PURE__ */ (() => {
    const $0 = foldlArray((m) => (a) => insert(ordString)(a)()(m))(Leaf2);
    return (x) => $0(Object.keys(x));
  })(),
  values: /* @__PURE__ */ (() => {
    const $0 = foldrArray(Cons)(Nil);
    return (x) => $0(values(x));
  })(),
  filterKeys,
  unionWith: unionWith2,
  lookup: lookup2,
  delete: $$delete2,
  insert: insert2,
  toUnfoldable: (dictUnfoldable) => toAscUnfoldable(dictUnfoldable),
  Set0: () => setObjectString
};
var lookup$p = (dictMonadThrow) => (dictShow) => (dictMap) => (k) => (\u03B3) => orElse(dictMonadThrow)("Key " + dictShow.show(k) + " exists in map")(dictMap.lookup(k)(\u03B3));
var $$get = (dictShow) => (dictMap) => (k) => {
  const $0 = dictMap.lookup(k);
  const $1 = definitely("Key " + dictShow.show(k) + " exists in map");
  return (x) => $1($0(x));
};
var disjointUnion_inv = (dictOrd) => {
  const $0 = setSet(dictOrd);
  return (dictMap) => (ks) => (m) => $Tuple(dictMap.filterKeys((v) => $0.member(v)(ks))(m), dictMap.filterKeys((v) => !$0.member(v)(ks))(m));
};
var disjointUnion = (dictMap) => dictMap.unionWith((v) => (v1) => throwException(error("not disjoint"))());
var mapFObjectString = {
  intersectionWith: intersectionWith_Object,
  difference: (m1) => (m2) => foldlArray((b) => (a) => mutate(($0) => () => {
    delete $0[a];
    return $0;
  })(b))(m1)(Object.keys(m2)),
  mapWithKey
};
var asMaplet = (dictMap) => {
  const toUnfoldable16 = dictMap.toUnfoldable(unfoldableList);
  return (m) => assertWith("")(dictMap.Set0().size(m) === 1)(definitely("singleton map")((() => {
    const $0 = toUnfoldable16(m);
    if ($0.tag === "Nil") {
      return Nothing;
    }
    if ($0.tag === "Cons") {
      return $Maybe("Just", $0._1);
    }
    fail();
  })()));
};
var append_inv = (dictOrd) => (dictMap) => (xs) => (\u03B3) => $Tuple(
  dictMap.filterKeys((v) => !setSet(dictOrd).member(v)(xs))(\u03B3),
  dictMap.filterKeys((v) => setSet(dictOrd).member(v)(xs))(\u03B3)
);

// output-es/DataType/index.js
var $DataType = (_1, _2) => ({ tag: "DataType", _1, _2 });
var fromFoldable3 = /* @__PURE__ */ fromFoldable2(foldableArray);
var fromFoldable12 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)()(m))(Leaf2))();
var toUnfoldable5 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
var show = /* @__PURE__ */ (() => showSet(showString).show)();
var DataType = (value0) => (value1) => $DataType(value0, value1);
var typeName = (v) => v._1;
var eqDataType = { eq: (x) => (y) => x._1 === y._1 };
var showDataType = { show: typeName };
var isCtrName = (str) => checkAttr([512, 524288])(toCharCode(definitely("absurd")(charAt2(0)(str))));
var showCtr = (c) => {
  if (isCtrName(c)) {
    return c;
  }
  if (":" === definitely("absurd")(charAt2(0)(c))) {
    return "(" + c + ")";
  }
  return throwException(error("absurd"))();
};
var dataType = (name2) => {
  const $0 = arrayMap((v) => $Tuple(v._1, v._2));
  const $1 = DataType(name2);
  return (x) => $1(fromFoldable3($0(x)));
};
var dataTypes = /* @__PURE__ */ foldrArray(Cons)(Nil)([
  /* @__PURE__ */ dataType("Bool")([/* @__PURE__ */ $Tuple("True", 0), /* @__PURE__ */ $Tuple("False", 0)]),
  /* @__PURE__ */ dataType("InfNum")([/* @__PURE__ */ $Tuple("FNum", 1), /* @__PURE__ */ $Tuple("Infty", 0)]),
  /* @__PURE__ */ dataType("List")([/* @__PURE__ */ $Tuple("Nil", 0), /* @__PURE__ */ $Tuple(":", 2)]),
  /* @__PURE__ */ dataType("Option")([/* @__PURE__ */ $Tuple("None", 0), /* @__PURE__ */ $Tuple("Some", 1)]),
  /* @__PURE__ */ dataType("Ordering")([/* @__PURE__ */ $Tuple("GT", 0), /* @__PURE__ */ $Tuple("LT", 0), /* @__PURE__ */ $Tuple("EQ", 0)]),
  /* @__PURE__ */ dataType("Pair")([/* @__PURE__ */ $Tuple("Pair", 2)]),
  /* @__PURE__ */ dataType("Tree")([/* @__PURE__ */ $Tuple("Empty", 0), /* @__PURE__ */ $Tuple("NonEmpty", 3)]),
  /* @__PURE__ */ dataType("LinePlot")([/* @__PURE__ */ $Tuple("LinePlot", 1)]),
  /* @__PURE__ */ dataType("Orientation")([/* @__PURE__ */ $Tuple("Default", 0), /* @__PURE__ */ $Tuple("Rotated", 0)]),
  /* @__PURE__ */ dataType("View")([
    /* @__PURE__ */ $Tuple("BarChart", 1),
    /* @__PURE__ */ $Tuple("LineChart", 1),
    /* @__PURE__ */ $Tuple("Paragraph", 1),
    /* @__PURE__ */ $Tuple("MultiView", 1),
    /* @__PURE__ */ $Tuple("ScatterPlot", 1)
  ]),
  /* @__PURE__ */ dataType("Point")([/* @__PURE__ */ $Tuple("Point", 2)]),
  /* @__PURE__ */ dataType("Orient")([/* @__PURE__ */ $Tuple("Horiz", 0), /* @__PURE__ */ $Tuple("Vert", 0)]),
  /* @__PURE__ */ dataType("GraphicsElement")([
    /* @__PURE__ */ $Tuple("Circle", 4),
    /* @__PURE__ */ $Tuple("Group", 1),
    /* @__PURE__ */ $Tuple("Line", 4),
    /* @__PURE__ */ $Tuple("Polyline", 3),
    /* @__PURE__ */ $Tuple("Polymarkers", 2),
    /* @__PURE__ */ $Tuple("Rect", 5),
    /* @__PURE__ */ $Tuple("String", 5),
    /* @__PURE__ */ $Tuple("Viewport", 9)
  ]),
  /* @__PURE__ */ dataType("Transform")([/* @__PURE__ */ $Tuple("Scale", 2), /* @__PURE__ */ $Tuple("Translate", 2)]),
  /* @__PURE__ */ dataType("Marker")([/* @__PURE__ */ $Tuple("Arrowhead", 0)]),
  /* @__PURE__ */ dataType("TextFragment")([/* @__PURE__ */ $Tuple("Text", 1), /* @__PURE__ */ $Tuple("Link", 2)])
]);
var ctrToDataType = /* @__PURE__ */ (() => fromFoldable2(foldableList)(bindList.bind(listMap((d) => listMap((v) => $Tuple(
  v,
  d
))(toUnfoldable5(fromFoldable12(mapObjectString.keys(d._2)))))(dataTypes))(identity5)))();
var dataTypeForCtr = {
  dataTypeFor: (dictMonadThrow) => (c) => orElse(dictMonadThrow)("Unknown constructor " + showCtr(c))(_lookup(
    Nothing,
    Just,
    c,
    ctrToDataType
  ))
};
var dataTypeForSetCtr = {
  dataTypeFor: (dictMonadThrow) => (cs) => {
    const v = toUnfoldable5(cs);
    if (v.tag === "Cons") {
      return dataTypeForCtr.dataTypeFor(dictMonadThrow)(v._1);
    }
    fail();
  }
};
var consistentWith = (dictMonadError) => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Bind1 = MonadThrow0.Monad0().Bind1();
  const $$void = Bind1.Apply0().Functor0().map((v) => {
  });
  const withMsg2 = withMsg(dictMonadError);
  return (cs) => (cs$p) => $$void(Bind1.bind(dataTypeForSetCtr.dataTypeFor(MonadThrow0)(cs$p))((d) => Bind1.bind(dataTypeForSetCtr.dataTypeFor(MonadThrow0)(cs$p))((d$p) => withMsg2("constructors of " + d$p._1 + " do not include " + show(map(ordString)(showCtr)(cs)))(mayFailEq(MonadThrow0)(showDataType)(eqDataType)(d)(d$p)))));
};
var arity = (dictMonadThrow) => (c) => dictMonadThrow.Monad0().Bind1().bind(dataTypeForCtr.dataTypeFor(dictMonadThrow)(c))((v) => orElse(dictMonadThrow)("absurd")(_lookup(
  Nothing,
  Just,
  c,
  v._2
)));
var checkArity = (dictMonadError) => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  const $$void = Monad0.Bind1().Apply0().Functor0().map((v) => {
  });
  const withMsg2 = withMsg(dictMonadError);
  const bind2Flipped2 = bind2Flipped(Monad0);
  return (c) => (n) => $$void(withMsg2("Checking arity of " + showCtr(c))(bind2Flipped2(mayFailEq(MonadThrow0)(showInt)(eqInt))(arity(MonadThrow0)(c))(Monad0.Applicative0().pure(n))));
};

// output-es/Dict/index.js
var identity15 = (x) => x;
var isEmptyDict = { isEmpty: (v) => isEmpty2(v) };
var setDictString = {
  empty,
  filter: (p) => (v) => filterWithKey2((x) => {
    const $0 = p(x);
    return (v$1) => $0;
  })(v),
  size: (v) => size3(v),
  member: (x) => (v) => Object.hasOwn(v, x),
  difference: (v) => (v1) => setObjectString.difference(v)(v1),
  union: (v) => (v1) => union2(v)(v1),
  IsEmpty0: () => isEmptyDict
};
var mapDictString = {
  maplet: (k) => (v) => {
    const $0 = {};
    $0[k] = v;
    return $0;
  },
  keys: (v) => mapObjectString.keys(v),
  values: (v) => mapObjectString.values(v),
  filterKeys: (p) => (v) => filterWithKey2((x) => {
    const $0 = p(x);
    return (v$1) => $0;
  })(v),
  unionWith: (f) => (v) => (v1) => unionWith2(f)(v)(v1),
  lookup: (k) => (v) => _lookup(Nothing, Just, k, v),
  delete: (k) => (v) => mutate(($0) => () => {
    delete $0[k];
    return $0;
  })(v),
  insert: (k) => (v) => (v1) => mutate(($0) => () => {
    $0[k] = v;
    return $0;
  })(v1),
  toUnfoldable: (dictUnfoldable) => toAscUnfoldable(dictUnfoldable),
  Set0: () => setDictString
};
var functorDict = { map: (f) => (m) => _fmapObject(m, f) };
var foldableDict = {
  foldl: (f) => (z) => (m) => fold((z$1) => (v) => f(z$1))(z)(m),
  foldr: (f) => (z) => (m) => foldrArray(f)(z)(values(m)),
  foldMap: (dictMonoid) => {
    const foldMap1 = foldMap(dictMonoid);
    return (f) => foldMap1((v) => f);
  }
};
var foldableWithIndexStringDi = {
  foldlWithIndex: (f) => (z) => (v) => fold((b) => (a) => f(a)(b))(z)(v),
  foldrWithIndex: (f) => foldrWithIndexDefault(foldableWithIndexStringDi)(f),
  foldMapWithIndex: (dictMonoid) => (f) => foldableWithIndexStringDi.foldlWithIndex((i) => (acc) => (x) => dictMonoid.Semigroup0().append(acc)(f(i)(x)))(dictMonoid.mempty),
  Foldable0: () => foldableDict
};
var traversableDict = {
  traverse: (dictApplicative) => {
    const $0 = traversableWithIndexObject.traverseWithIndex(dictApplicative);
    return (f) => (m) => dictApplicative.Apply0().Functor0().map((v1) => v1)($0((v) => f)(m));
  },
  sequence: (dictApplicative) => (v) => traversableDict.traverse(dictApplicative)(identity15)(v),
  Functor0: () => functorDict,
  Foldable1: () => foldableDict
};
var ordDict = (dictOrd) => {
  const Eq0 = dictOrd.Eq0();
  const eqDict1 = eqObject(Eq0);
  return {
    compare: (v) => (v1) => {
      if (isSubmap(Eq0)(v)(v1)) {
        if (isSubmap(Eq0)(v1)(v)) {
          return EQ;
        }
        return LT;
      }
      return GT;
    },
    Eq0: () => eqDict1
  };
};

// output-es/Data.String.Common/foreign.js
var replaceAll = function(s1) {
  return function(s2) {
    return function(s3) {
      return s3.replace(new RegExp(s1.replace(/[-\/\\^$*+?.()|[\]{}]/g, "\\$&"), "g"), s2);
    };
  };
};
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

// output-es/Graph/index.js
var fromFoldable4 = /* @__PURE__ */ (() => fromFoldableImpl(foldableSet.foldr))();
var fromFoldable13 = /* @__PURE__ */ (() => fromFoldableImpl(foldableList.foldr))();
var fromFoldable22 = /* @__PURE__ */ (() => foldableSet.foldr(Cons)(Nil))();
var Vertex = (x) => x;
var eqVertex = { eq: (x) => (y) => x === y };
var ordVertex = { compare: (x) => (y) => ordString.compare(x)(y), Eq0: () => eqVertex };
var eqDVertex = { eq: (v) => (v1) => v._1 === v1._1 };
var ordDVertex = { compare: (v) => (v1) => ordString.compare(v._1)(v1._1), Eq0: () => eqDVertex };
var unions1 = /* @__PURE__ */ foldlArray(/* @__PURE__ */ union(ordDVertex))(Leaf2);
var verticesDict = (dictVertices) => {
  const vertices1 = dictVertices.vertices;
  return { vertices: (d) => unions1(arrayMap(vertices1)(values(d))) };
};
var showVertices = (\u03B1s) => "{" + joinWith(", ")(fromFoldable4(map(ordString)(unsafeCoerce)(\u03B1s))) + "}";
var showEdgeList = (es) => joinWith("\n")([
  "digraph G {",
  ...arrayMap((v) => "   " + v)([
    "rankdir = RL",
    ...arrayMap((v) => v._1._1 + " -> {" + joinWith(", ")(fromFoldable4(map(ordString)(unsafeCoerce)(v._2))) + "}")(fromFoldable13(reverse(es)))
  ]),
  "}"
]);
var runQuery = (dictOrd) => (dictGraph) => (query) => (g) => mapMaybe2(dictOrd)((x) => query(x._2))(dictGraph.Vertices1().vertices(g));
var toEdgeList = (dictGraph) => (g) => {
  const $0 = (v) => {
    if (v._1.tag === "Nil") {
      return $Step("Done", v._2);
    }
    if (v._1.tag === "Cons") {
      return $Step(
        "Loop",
        $Tuple(
          v._1._2,
          $List("Cons", $Tuple($Tuple(v._1._1, dictGraph.vertexData(g)(v._1._1)), dictGraph.outN(g)(v._1._1)), v._2)
        )
      );
    }
    fail();
  };
  const go = (go$a0$copy) => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Loop") {
        go$a0 = $0(v._1);
        continue;
      }
      if (v.tag === "Done") {
        go$c = false;
        go$r = v._1;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go($0($Tuple(dictGraph.topologicalSort(g), Nil)));
};
var showGraph = (dictGraph) => (x) => showEdgeList(toEdgeList(dictGraph)(x));
var inEdges$p = (dictGraph) => (g) => (\u03B1) => fromFoldable22(map(ordTuple(ordVertex)(ordVertex))((v) => $Tuple(v, \u03B1))(dictGraph.inN(g)(\u03B1)));
var inEdges = (dictGraph) => (g) => (\u03B1s) => {
  const $0 = (v) => {
    if (v._1.tag === "Nil") {
      return $Step("Done", v._2);
    }
    if (v._1.tag === "Cons") {
      return $Step(
        "Loop",
        $Tuple(v._1._2, foldableList.foldr(Cons)(v._2)(inEdges$p(dictGraph)(g)(v._1._1)))
      );
    }
    fail();
  };
  const go = (go$a0$copy) => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Loop") {
        go$a0 = $0(v._1);
        continue;
      }
      if (v.tag === "Done") {
        go$c = false;
        go$r = v._1;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go($0($Tuple(fromFoldable22(\u03B1s), Nil)));
};
var addresses = /* @__PURE__ */ map(ordVertex)((x) => x._1);
var showVertices$p = (x) => showVertices(addresses(x));

// output-es/Util.Pair/index.js
var $Pair = (_1, _2) => ({ tag: "Pair", _1, _2 });
var Pair = (value0) => (value1) => $Pair(value0, value1);
var functorPair = { map: (f) => (v) => $Pair(f(v._1), f(v._2)) };
var foldablePair = {
  foldl: (f) => (z) => (v) => f(f(z)(v._1))(v._2),
  foldr: (f) => foldrDefault(foldablePair)(f),
  foldMap: (dictMonoid) => (f) => foldablePair.foldl((acc) => (x) => dictMonoid.Semigroup0().append(acc)(f(x)))(dictMonoid.mempty)
};
var traversablePair = {
  traverse: (dictApplicative) => {
    const Apply0 = dictApplicative.Apply0();
    return (f) => (v) => Apply0.apply(Apply0.Functor0().map(Pair)(f(v._1)))(f(v._2));
  },
  sequence: (dictApplicative) => traversablePair.traverse(dictApplicative)(identity7),
  Functor0: () => functorPair,
  Foldable1: () => foldablePair
};
var ordPair = (dictOrd) => {
  const $0 = dictOrd.Eq0();
  const eqPair1 = { eq: (x) => (y) => $0.eq(x._1)(y._1) && $0.eq(x._2)(y._2) };
  return {
    compare: (x) => (y) => {
      const v = dictOrd.compare(x._1)(y._1);
      if (v === "LT") {
        return LT;
      }
      if (v === "GT") {
        return GT;
      }
      return dictOrd.compare(x._2)(y._2);
    },
    Eq0: () => eqPair1
  };
};
var toTuple = (v) => $Tuple(v._1, v._2);
var unzip4 = (xys) => unzip(listMap(toTuple)(xys));
var fromTuple = (v) => $Pair(v._1, v._2);

// output-es/Lattice/index.js
var identity16 = (x) => x;
var length4 = /* @__PURE__ */ foldlArray((c) => (v) => 1 + c | 0)(0);
var meetSemilatticeUnit = { meet: (v) => identity16 };
var joinSemilatticeUnit = { join: (v) => identity16 };
var boundedMeetSemilatticeUni = { top: void 0, MeetSemilattice0: () => meetSemilatticeUnit };
var boundedJoinSemilatticeUni = { bot: void 0, JoinSemilattice0: () => joinSemilatticeUnit };
var joinSemilatticeArray = (dictJoinSemilattice) => {
  const join1 = dictJoinSemilattice.join;
  return {
    join: (xs) => (ys) => {
      if (length4(xs) === length4(ys)) {
        return zipWith2(join1)(xs)(ys);
      }
      return throwException(error("Shape mismatch"))();
    }
  };
};
var joinSemilatticeList = (dictJoinSemilattice) => {
  const join1 = dictJoinSemilattice.join;
  return {
    join: (xs) => (ys) => {
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
          return go$1$r;
        };
        return go(0)(xs) === go$1(0)(ys);
      })()) {
        return zipWith(join1)(xs)(ys);
      }
      return throwException(error("Shape mismatch"))();
    }
  };
};
var expandableDictDict = (dictBotOf) => {
  const botOf3 = dictBotOf.botOf;
  return (dictExpandable) => {
    const expand1 = dictExpandable.expand;
    return {
      expand: (kvs) => (kvs$p) => assertWith("")(difference2(ordString)(mapObjectString.keys(kvs))(mapObjectString.keys(kvs$p)).tag === "Leaf")(union2(intersectionWith_Object(expand1)(kvs)(kvs$p))(_fmapObject(
        mapFObjectString.difference(kvs$p)(kvs),
        botOf3
      )))
    };
  };
};

// output-es/Expr/index.js
var $Cont = (tag, _1) => ({ tag, _1 });
var $Elim = (tag, _1, _2) => ({ tag, _1, _2 });
var $Expr = (tag, _1, _2, _3, _4) => ({ tag, _1, _2, _3, _4 });
var $RecDefs = (_1, _2) => ({ tag: "RecDefs", _1, _2 });
var $VarDef = (_1, _2) => ({ tag: "VarDef", _1, _2 });
var union4 = /* @__PURE__ */ (() => setSet(ordDVertex).union)();
var unions12 = /* @__PURE__ */ (() => {
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
        go$a0 = unionWith(ordDVertex)($$const)(b)(v._1);
        go$a1 = v._2;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go(Leaf2);
})();
var eqSet = { eq: (v) => (v1) => eqMap(eqString)(eqUnit).eq(v)(v1) };
var identity17 = (x) => x;
var compare3 = /* @__PURE__ */ (() => ordTuple(ordString)(ordString).compare)();
var compare4 = /* @__PURE__ */ (() => ordSet(ordString).compare)();
var setSet2 = /* @__PURE__ */ setSet(ordString);
var fromFoldable5 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)()(m))(Leaf2))();
var asMaplet2 = /* @__PURE__ */ asMaplet(mapDictString);
var unions3 = /* @__PURE__ */ (() => {
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
    return go$r;
  };
  return go(Leaf2);
})();
var ContExpr = (value0) => $Cont("ContExpr", value0);
var Dictionary = (value0) => (value1) => $Expr("Dictionary", value0, value1);
var Constr = (value0) => (value1) => (value2) => $Expr("Constr", value0, value1, value2);
var Matrix = (value0) => (value1) => (value2) => (value3) => $Expr("Matrix", value0, value1, value2, value3);
var Lambda = (value0) => (value1) => $Expr("Lambda", value0, value1);
var Project = (value0) => (value1) => $Expr("Project", value0, value1);
var DProject = (value0) => (value1) => $Expr("DProject", value0, value1);
var App2 = (value0) => (value1) => $Expr("App", value0, value1);
var Let = (value0) => (value1) => $Expr("Let", value0, value1);
var LetRec = (value0) => (value1) => $Expr("LetRec", value0, value1);
var ElimVar = (value0) => (value1) => $Elim("ElimVar", value0, value1);
var ElimDict = (value0) => (value1) => $Elim("ElimDict", value0, value1);
var VarDef = (value0) => (value1) => $VarDef(value0, value1);
var RecDefs = (value0) => (value1) => $RecDefs(value0, value1);
var Module = (x) => x;
var typeNameRecDefs = { typeName: (v) => "RecDefs" };
var pack = (x) => (k) => k(typeNameRecDefs)(x);
var typeNameExpr = { typeName: (v) => "Expr" };
var pack1 = (x) => (k) => k(typeNameExpr)(x);
var verticesVarDefVertex = { vertices: (v) => union4(verticesElimVertex.vertices(v._1))(verticesExprVertex.vertices(v._2)) };
var verticesRecDefsVertex = {
  vertices: (v) => union4($$$Map("Two", Leaf2, $Tuple(v._1, pack(v)), void 0, Leaf2))(verticesDict(verticesElimVertex).vertices(v._2))
};
var verticesExprVertex = {
  vertices: (v) => {
    if (v.tag === "Var") {
      return Leaf2;
    }
    if (v.tag === "Op") {
      return Leaf2;
    }
    if (v.tag === "Int") {
      return $$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2);
    }
    if (v.tag === "Float") {
      return $$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2);
    }
    if (v.tag === "Str") {
      return $$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2);
    }
    if (v.tag === "Dictionary") {
      return union4($$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2))(unions12(listMap((v1) => union4(verticesExprVertex.vertices(v1._1))(verticesExprVertex.vertices(v1._2)))(v._2)));
    }
    if (v.tag === "Constr") {
      return union4($$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2))(unions12(listMap(verticesExprVertex.vertices)(v._3)));
    }
    if (v.tag === "Matrix") {
      return union4($$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2))(union4(verticesExprVertex.vertices(v._2))(verticesExprVertex.vertices(v._4)));
    }
    if (v.tag === "Lambda") {
      return union4($$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2))(verticesElimVertex.vertices(v._2));
    }
    if (v.tag === "Project") {
      return verticesExprVertex.vertices(v._1);
    }
    if (v.tag === "DProject") {
      return union4(verticesExprVertex.vertices(v._1))(verticesExprVertex.vertices(v._2));
    }
    if (v.tag === "App") {
      return union4(verticesExprVertex.vertices(v._1))(verticesExprVertex.vertices(v._2));
    }
    if (v.tag === "Let") {
      return union4(verticesVarDefVertex.vertices(v._1))(verticesExprVertex.vertices(v._2));
    }
    if (v.tag === "LetRec") {
      return union4(verticesRecDefsVertex.vertices(v._1))(verticesExprVertex.vertices(v._2));
    }
    fail();
  }
};
var verticesElimVertex = {
  vertices: (v) => {
    if (v.tag === "ElimVar") {
      return verticesContVertex.vertices(v._2);
    }
    if (v.tag === "ElimConstr") {
      return verticesDict(verticesContVertex).vertices(v._1);
    }
    if (v.tag === "ElimDict") {
      return verticesContVertex.vertices(v._2);
    }
    fail();
  }
};
var verticesContVertex = {
  vertices: (v) => {
    if (v.tag === "ContExpr") {
      return verticesExprVertex.vertices(v._1);
    }
    if (v.tag === "ContElim") {
      return verticesElimVertex.vertices(v._1);
    }
    fail();
  }
};
var verticesModuleVertex = {
  vertices: (v) => unions12(listMap((v1) => {
    if (v1.tag === "Left") {
      return verticesVarDefVertex.vertices(v1._1);
    }
    if (v1.tag === "Right") {
      return verticesRecDefsVertex.vertices(v1._1);
    }
    fail();
  })(v))
};
var joinSemilatticeVarDef = (dictJoinSemilattice) => ({ join: (v) => (v1) => $VarDef(joinSemilatticeElim(dictJoinSemilattice).join(v._1)(v1._1), joinSemilatticeExpr(dictJoinSemilattice).join(v._2)(v1._2)) });
var joinSemilatticeRecDefs = (dictJoinSemilattice) => ({ join: (v) => (v1) => $RecDefs(dictJoinSemilattice.join(v._1)(v1._1), unionWith2(joinSemilatticeElim(dictJoinSemilattice).join)(v._2)(v1._2)) });
var joinSemilatticeExpr = (dictJoinSemilattice) => ({
  join: (v) => (v1) => {
    if (v.tag === "Var") {
      if (v1.tag === "Var") {
        return $Expr("Var", mustEq(eqString)(showString)(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Op") {
      if (v1.tag === "Op") {
        return $Expr("Op", mustEq(eqString)(showString)(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Int") {
      if (v1.tag === "Int") {
        return $Expr("Int", dictJoinSemilattice.join(v._1)(v1._1), mustEq(eqInt)(showInt)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Str") {
      if (v1.tag === "Str") {
        return $Expr("Str", dictJoinSemilattice.join(v._1)(v1._1), mustEq(eqString)(showString)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Float") {
      if (v1.tag === "Float") {
        return $Expr("Float", dictJoinSemilattice.join(v._1)(v1._1), mustEq(eqNumber)(showNumber)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Dictionary") {
      if (v1.tag === "Dictionary") {
        return $Expr(
          "Dictionary",
          dictJoinSemilattice.join(v._1)(v1._1),
          joinSemilatticeList((() => {
            const $0 = joinSemilatticeExpr(dictJoinSemilattice);
            return { join: (v$1) => (v1$1) => $Pair($0.join(v$1._1)(v1$1._1), $0.join(v$1._2)(v1$1._2)) };
          })()).join(v._2)(v1._2)
        );
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Constr") {
      if (v1.tag === "Constr") {
        return $Expr(
          "Constr",
          dictJoinSemilattice.join(v._1)(v1._1),
          mustEq(eqString)(showString)(v._2)(v1._2),
          joinSemilatticeList(joinSemilatticeExpr(dictJoinSemilattice)).join(v._3)(v1._3)
        );
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Matrix") {
      if (v1.tag === "Matrix") {
        return $Expr(
          "Matrix",
          dictJoinSemilattice.join(v._1)(v1._1),
          joinSemilatticeExpr(dictJoinSemilattice).join(v._2)(v1._2),
          $Tuple(mustEq(eqString)(showString)(v._3._1)(v1._3._1), mustEq(eqString)(showString)(v._3._2)(v1._3._2)),
          joinSemilatticeExpr(dictJoinSemilattice).join(v._4)(v1._4)
        );
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Lambda") {
      if (v1.tag === "Lambda") {
        return $Expr("Lambda", dictJoinSemilattice.join(v._1)(v1._1), joinSemilatticeElim(dictJoinSemilattice).join(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Project") {
      if (v1.tag === "Project") {
        return $Expr("Project", joinSemilatticeExpr(dictJoinSemilattice).join(v._1)(v1._1), mustEq(eqString)(showString)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "DProject") {
      if (v1.tag === "DProject") {
        return $Expr("DProject", joinSemilatticeExpr(dictJoinSemilattice).join(v._1)(v1._1), joinSemilatticeExpr(dictJoinSemilattice).join(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "App") {
      if (v1.tag === "App") {
        return $Expr("App", joinSemilatticeExpr(dictJoinSemilattice).join(v._1)(v1._1), joinSemilatticeExpr(dictJoinSemilattice).join(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Let") {
      if (v1.tag === "Let") {
        return $Expr("Let", joinSemilatticeVarDef(dictJoinSemilattice).join(v._1)(v1._1), joinSemilatticeExpr(dictJoinSemilattice).join(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "LetRec" && v1.tag === "LetRec") {
      return $Expr("LetRec", joinSemilatticeRecDefs(dictJoinSemilattice).join(v._1)(v1._1), joinSemilatticeExpr(dictJoinSemilattice).join(v._2)(v1._2));
    }
    return throwException(error("Shape mismatch"))();
  }
});
var joinSemilatticeElim = (dictJoinSemilattice) => ({
  join: (v) => (v1) => {
    if (v.tag === "ElimVar") {
      if (v1.tag === "ElimVar") {
        return $Elim("ElimVar", mustEq(eqString)(showString)(v._1)(v1._1), joinSemilatticeCont(dictJoinSemilattice).join(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "ElimConstr") {
      if (v1.tag === "ElimConstr") {
        return $Elim("ElimConstr", unionWith2(joinSemilatticeCont(dictJoinSemilattice).join)(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "ElimDict" && v1.tag === "ElimDict") {
      return $Elim("ElimDict", mustEq(eqSet)(showSet(showString))(v._1)(v1._1), joinSemilatticeCont(dictJoinSemilattice).join(v._2)(v1._2));
    }
    return throwException(error("Shape mismatch"))();
  }
});
var joinSemilatticeCont = (dictJoinSemilattice) => ({
  join: (v) => (v1) => {
    if (v.tag === "ContExpr") {
      if (v1.tag === "ContExpr") {
        return $Cont("ContExpr", joinSemilatticeExpr(dictJoinSemilattice).join(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "ContElim" && v1.tag === "ContElim") {
      return $Cont("ContElim", joinSemilatticeElim(dictJoinSemilattice).join(v._1)(v1._1));
    }
    return throwException(error("Shape mismatch"))();
  }
});
var functorVarDef = { map: (f) => (m) => $VarDef(functorElim.map(f)(m._1), functorExpr.map(f)(m._2)) };
var functorRecDefs = { map: (f) => (m) => $RecDefs(f(m._1), _fmapObject(m._2, functorElim.map(f))) };
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
    if (m.tag === "Dictionary") {
      return $Expr(
        "Dictionary",
        f(m._1),
        listMap((() => {
          const $0 = functorExpr.map(f);
          return (v) => $Pair($0(v._1), $0(v._2));
        })())(m._2)
      );
    }
    if (m.tag === "Constr") {
      return $Expr("Constr", f(m._1), m._2, listMap(functorExpr.map(f))(m._3));
    }
    if (m.tag === "Matrix") {
      return $Expr("Matrix", f(m._1), functorExpr.map(f)(m._2), m._3, functorExpr.map(f)(m._4));
    }
    if (m.tag === "Lambda") {
      return $Expr("Lambda", f(m._1), functorElim.map(f)(m._2));
    }
    if (m.tag === "Project") {
      return $Expr("Project", functorExpr.map(f)(m._1), m._2);
    }
    if (m.tag === "DProject") {
      return $Expr("DProject", functorExpr.map(f)(m._1), functorExpr.map(f)(m._2));
    }
    if (m.tag === "App") {
      return $Expr("App", functorExpr.map(f)(m._1), functorExpr.map(f)(m._2));
    }
    if (m.tag === "Let") {
      return $Expr("Let", functorVarDef.map(f)(m._1), functorExpr.map(f)(m._2));
    }
    if (m.tag === "LetRec") {
      return $Expr("LetRec", functorRecDefs.map(f)(m._1), functorExpr.map(f)(m._2));
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
    if (m.tag === "ElimDict") {
      return $Elim("ElimDict", m._1, functorCont.map(f)(m._2));
    }
    fail();
  }
};
var functorCont = {
  map: (f) => (m) => {
    if (m.tag === "ContExpr") {
      return $Cont("ContExpr", functorExpr.map(f)(m._1));
    }
    if (m.tag === "ContElim") {
      return $Cont("ContElim", functorElim.map(f)(m._1));
    }
    fail();
  }
};
var functorModule = {
  map: (f) => (m) => listMap((v2) => {
    if (v2.tag === "Left") {
      return $Either("Left", functorVarDef.map(f)(v2._1));
    }
    if (v2.tag === "Right") {
      return $Either("Right", functorRecDefs.map(f)(v2._1));
    }
    fail();
  })(m)
};
var foldableVarDef = {
  foldl: (f) => (z) => (m) => foldableExpr.foldl(f)(foldableElim.foldl(f)(z)(m._1))(m._2),
  foldr: (f) => (z) => (m) => foldableElim.foldr(f)(foldableExpr.foldr(f)(z)(m._2))(m._1),
  foldMap: (dictMonoid) => (f) => (m) => dictMonoid.Semigroup0().append(foldableElim.foldMap(dictMonoid)(f)(m._1))(foldableExpr.foldMap(dictMonoid)(f)(m._2))
};
var foldableRecDefs = {
  foldl: (f) => (z) => (m) => {
    const $0 = foldableElim.foldl(f);
    return fold((z$1) => (v) => $0(z$1))(f(z)(m._1))(m._2);
  },
  foldr: (f) => (z) => (m) => f(m._1)((() => {
    const $0 = foldableElim.foldr(f);
    return foldrArray((b) => (a) => $0(a)(b))(z)(values(m._2));
  })()),
  foldMap: (dictMonoid) => {
    const foldMap1 = foldMap(dictMonoid);
    return (f) => (m) => dictMonoid.Semigroup0().append(f(m._1))((() => {
      const $0 = foldableElim.foldMap(dictMonoid)(f);
      return foldMap1((v) => $0)(m._2);
    })());
  }
};
var foldableExpr = {
  foldl: (f) => (z) => (m) => {
    if (m.tag === "Var") {
      return z;
    }
    if (m.tag === "Op") {
      return z;
    }
    if (m.tag === "Int") {
      return f(z)(m._1);
    }
    if (m.tag === "Float") {
      return f(z)(m._1);
    }
    if (m.tag === "Str") {
      return f(z)(m._1);
    }
    if (m.tag === "Dictionary") {
      const $0 = foldableExpr.foldl(f);
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
            go$a0 = $0($0(b)(v._1._1))(v._1._2);
            go$a1 = v._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      return go(f(z)(m._1))(m._2);
    }
    if (m.tag === "Constr") {
      const $0 = foldableExpr.foldl(f);
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
            go$a0 = $0(b)(v._1);
            go$a1 = v._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      return go(f(z)(m._1))(m._3);
    }
    if (m.tag === "Matrix") {
      return foldableExpr.foldl(f)(foldableExpr.foldl(f)(f(z)(m._1))(m._2))(m._4);
    }
    if (m.tag === "Lambda") {
      return foldableElim.foldl(f)(f(z)(m._1))(m._2);
    }
    if (m.tag === "Project") {
      return foldableExpr.foldl(f)(z)(m._1);
    }
    if (m.tag === "DProject") {
      return foldableExpr.foldl(f)(foldableExpr.foldl(f)(z)(m._1))(m._2);
    }
    if (m.tag === "App") {
      return foldableExpr.foldl(f)(foldableExpr.foldl(f)(z)(m._1))(m._2);
    }
    if (m.tag === "Let") {
      return foldableExpr.foldl(f)(foldableVarDef.foldl(f)(z)(m._1))(m._2);
    }
    if (m.tag === "LetRec") {
      return foldableExpr.foldl(f)(foldableRecDefs.foldl(f)(z)(m._1))(m._2);
    }
    fail();
  },
  foldr: (f) => (z) => (m) => {
    if (m.tag === "Var") {
      return z;
    }
    if (m.tag === "Op") {
      return z;
    }
    if (m.tag === "Int") {
      return f(m._1)(z);
    }
    if (m.tag === "Float") {
      return f(m._1)(z);
    }
    if (m.tag === "Str") {
      return f(m._1)(z);
    }
    if (m.tag === "Dictionary") {
      return f(m._1)(foldableList.foldr((() => {
        const $0 = foldableExpr.foldr(f);
        const $1 = foldrDefault(foldablePair)((b) => (a) => $0(a)(b));
        return (b) => (a) => $1(a)(b);
      })())(z)(m._2));
    }
    if (m.tag === "Constr") {
      return f(m._1)(foldableList.foldr((() => {
        const $0 = foldableExpr.foldr(f);
        return (b) => (a) => $0(a)(b);
      })())(z)(m._3));
    }
    if (m.tag === "Matrix") {
      return f(m._1)(foldableExpr.foldr(f)(foldableExpr.foldr(f)(z)(m._4))(m._2));
    }
    if (m.tag === "Lambda") {
      return f(m._1)(foldableElim.foldr(f)(z)(m._2));
    }
    if (m.tag === "Project") {
      return foldableExpr.foldr(f)(z)(m._1);
    }
    if (m.tag === "DProject") {
      return foldableExpr.foldr(f)(foldableExpr.foldr(f)(z)(m._2))(m._1);
    }
    if (m.tag === "App") {
      return foldableExpr.foldr(f)(foldableExpr.foldr(f)(z)(m._2))(m._1);
    }
    if (m.tag === "Let") {
      return foldableVarDef.foldr(f)(foldableExpr.foldr(f)(z)(m._2))(m._1);
    }
    if (m.tag === "LetRec") {
      return foldableRecDefs.foldr(f)(foldableExpr.foldr(f)(z)(m._2))(m._1);
    }
    fail();
  },
  foldMap: (dictMonoid) => {
    const mempty = dictMonoid.mempty;
    const $0 = dictMonoid.Semigroup0();
    const foldMap3 = foldableList.foldMap(dictMonoid);
    return (f) => (m) => {
      if (m.tag === "Var") {
        return mempty;
      }
      if (m.tag === "Op") {
        return mempty;
      }
      if (m.tag === "Int") {
        return f(m._1);
      }
      if (m.tag === "Float") {
        return f(m._1);
      }
      if (m.tag === "Str") {
        return f(m._1);
      }
      if (m.tag === "Dictionary") {
        return $0.append(f(m._1))(foldMap3(foldablePair.foldMap(dictMonoid)(foldableExpr.foldMap(dictMonoid)(f)))(m._2));
      }
      if (m.tag === "Constr") {
        return $0.append(f(m._1))(foldMap3(foldableExpr.foldMap(dictMonoid)(f))(m._3));
      }
      if (m.tag === "Matrix") {
        return $0.append(f(m._1))($0.append(foldableExpr.foldMap(dictMonoid)(f)(m._2))(foldableExpr.foldMap(dictMonoid)(f)(m._4)));
      }
      if (m.tag === "Lambda") {
        return $0.append(f(m._1))(foldableElim.foldMap(dictMonoid)(f)(m._2));
      }
      if (m.tag === "Project") {
        return foldableExpr.foldMap(dictMonoid)(f)(m._1);
      }
      if (m.tag === "DProject") {
        return $0.append(foldableExpr.foldMap(dictMonoid)(f)(m._1))(foldableExpr.foldMap(dictMonoid)(f)(m._2));
      }
      if (m.tag === "App") {
        return $0.append(foldableExpr.foldMap(dictMonoid)(f)(m._1))(foldableExpr.foldMap(dictMonoid)(f)(m._2));
      }
      if (m.tag === "Let") {
        return $0.append(foldableVarDef.foldMap(dictMonoid)(f)(m._1))(foldableExpr.foldMap(dictMonoid)(f)(m._2));
      }
      if (m.tag === "LetRec") {
        return $0.append(foldableRecDefs.foldMap(dictMonoid)(f)(m._1))(foldableExpr.foldMap(dictMonoid)(f)(m._2));
      }
      fail();
    };
  }
};
var foldableElim = {
  foldl: (f) => (z) => (m) => {
    if (m.tag === "ElimVar") {
      return foldableCont.foldl(f)(z)(m._2);
    }
    if (m.tag === "ElimConstr") {
      const $0 = foldableCont.foldl(f);
      return fold((z$1) => (v) => $0(z$1))(z)(m._1);
    }
    if (m.tag === "ElimDict") {
      return foldableCont.foldl(f)(z)(m._2);
    }
    fail();
  },
  foldr: (f) => (z) => (m) => {
    if (m.tag === "ElimVar") {
      return foldableCont.foldr(f)(z)(m._2);
    }
    if (m.tag === "ElimConstr") {
      const $0 = foldableCont.foldr(f);
      return foldrArray((b) => (a) => $0(a)(b))(z)(values(m._1));
    }
    if (m.tag === "ElimDict") {
      return foldableCont.foldr(f)(z)(m._2);
    }
    fail();
  },
  foldMap: (dictMonoid) => {
    const foldMap1 = foldMap(dictMonoid);
    return (f) => (m) => {
      if (m.tag === "ElimVar") {
        return foldableCont.foldMap(dictMonoid)(f)(m._2);
      }
      if (m.tag === "ElimConstr") {
        const $0 = foldableCont.foldMap(dictMonoid)(f);
        return foldMap1((v) => $0)(m._1);
      }
      if (m.tag === "ElimDict") {
        return foldableCont.foldMap(dictMonoid)(f)(m._2);
      }
      fail();
    };
  }
};
var foldableCont = {
  foldl: (f) => (z) => (m) => {
    if (m.tag === "ContExpr") {
      return foldableExpr.foldl(f)(z)(m._1);
    }
    if (m.tag === "ContElim") {
      return foldableElim.foldl(f)(z)(m._1);
    }
    fail();
  },
  foldr: (f) => (z) => (m) => {
    if (m.tag === "ContExpr") {
      return foldableExpr.foldr(f)(z)(m._1);
    }
    if (m.tag === "ContElim") {
      return foldableElim.foldr(f)(z)(m._1);
    }
    fail();
  },
  foldMap: (dictMonoid) => (f) => (m) => {
    if (m.tag === "ContExpr") {
      return foldableExpr.foldMap(dictMonoid)(f)(m._1);
    }
    if (m.tag === "ContElim") {
      return foldableElim.foldMap(dictMonoid)(f)(m._1);
    }
    fail();
  }
};
var traversableVarDef = {
  traverse: (dictApplicative) => {
    const Apply0 = dictApplicative.Apply0();
    return (f) => (m) => Apply0.apply(Apply0.Functor0().map((v2) => (v3) => $VarDef(v2, v3))(traversableElim.traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2));
  },
  sequence: (dictApplicative) => (v) => traversableVarDef.traverse(dictApplicative)(identity17)(v),
  Functor0: () => functorVarDef,
  Foldable1: () => foldableVarDef
};
var traversableRecDefs = {
  traverse: (dictApplicative) => {
    const Apply0 = dictApplicative.Apply0();
    const traverse5 = traversableDict.traverse(dictApplicative);
    return (f) => (m) => Apply0.apply(Apply0.Functor0().map((v2) => (v3) => $RecDefs(v2, v3))(f(m._1)))(traverse5(traversableElim.traverse(dictApplicative)(f))(m._2));
  },
  sequence: (dictApplicative) => (v) => traversableRecDefs.traverse(dictApplicative)(identity17)(v),
  Functor0: () => functorRecDefs,
  Foldable1: () => foldableRecDefs
};
var traversableExpr = {
  traverse: (dictApplicative) => {
    const Apply0 = dictApplicative.Apply0();
    const $0 = Apply0.Functor0();
    const traverse5 = traversableList.traverse(dictApplicative);
    const traverse6 = traversablePair.traverse(dictApplicative);
    return (f) => (m) => {
      if (m.tag === "Var") {
        return dictApplicative.pure($Expr("Var", m._1));
      }
      if (m.tag === "Op") {
        return dictApplicative.pure($Expr("Op", m._1));
      }
      if (m.tag === "Int") {
        const $1 = m._2;
        return $0.map((v2) => $Expr("Int", v2, $1))(f(m._1));
      }
      if (m.tag === "Float") {
        const $1 = m._2;
        return $0.map((v2) => $Expr("Float", v2, $1))(f(m._1));
      }
      if (m.tag === "Str") {
        const $1 = m._2;
        return $0.map((v2) => $Expr("Str", v2, $1))(f(m._1));
      }
      if (m.tag === "Dictionary") {
        return Apply0.apply($0.map((v2) => (v3) => $Expr("Dictionary", v2, v3))(f(m._1)))(traverse5(traverse6(traversableExpr.traverse(dictApplicative)(f)))(m._2));
      }
      if (m.tag === "Constr") {
        const $1 = m._2;
        return Apply0.apply($0.map((v3) => (v4) => $Expr("Constr", v3, $1, v4))(f(m._1)))(traverse5(traversableExpr.traverse(dictApplicative)(f))(m._3));
      }
      if (m.tag === "Matrix") {
        const $1 = m._3;
        return Apply0.apply(Apply0.apply($0.map((v4) => (v5) => (v6) => $Expr("Matrix", v4, v5, $1, v6))(f(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2)))(traversableExpr.traverse(dictApplicative)(f)(m._4));
      }
      if (m.tag === "Lambda") {
        return Apply0.apply($0.map((v2) => (v3) => $Expr("Lambda", v2, v3))(f(m._1)))(traversableElim.traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "Project") {
        const $1 = m._2;
        return $0.map((v2) => $Expr("Project", v2, $1))(traversableExpr.traverse(dictApplicative)(f)(m._1));
      }
      if (m.tag === "DProject") {
        return Apply0.apply($0.map((v2) => (v3) => $Expr("DProject", v2, v3))(traversableExpr.traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "App") {
        return Apply0.apply($0.map((v2) => (v3) => $Expr("App", v2, v3))(traversableExpr.traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "Let") {
        return Apply0.apply($0.map((v2) => (v3) => $Expr("Let", v2, v3))(traversableVarDef.traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "LetRec") {
        return Apply0.apply($0.map((v2) => (v3) => $Expr("LetRec", v2, v3))(traversableRecDefs.traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2));
      }
      fail();
    };
  },
  sequence: (dictApplicative) => (v) => traversableExpr.traverse(dictApplicative)(identity17)(v),
  Functor0: () => functorExpr,
  Foldable1: () => foldableExpr
};
var traversableElim = {
  traverse: (dictApplicative) => {
    const $0 = dictApplicative.Apply0().Functor0();
    const traverse5 = traversableDict.traverse(dictApplicative);
    return (f) => (m) => {
      if (m.tag === "ElimVar") {
        const $1 = m._1;
        return $0.map((v2) => $Elim("ElimVar", $1, v2))(traversableCont.traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "ElimConstr") {
        return $0.map((v1) => $Elim("ElimConstr", v1))(traverse5(traversableCont.traverse(dictApplicative)(f))(m._1));
      }
      if (m.tag === "ElimDict") {
        const $1 = m._1;
        return $0.map((v2) => $Elim("ElimDict", $1, v2))(traversableCont.traverse(dictApplicative)(f)(m._2));
      }
      fail();
    };
  },
  sequence: (dictApplicative) => (v) => traversableElim.traverse(dictApplicative)(identity17)(v),
  Functor0: () => functorElim,
  Foldable1: () => foldableElim
};
var traversableCont = {
  traverse: (dictApplicative) => {
    const $0 = dictApplicative.Apply0().Functor0();
    return (f) => (m) => {
      if (m.tag === "ContExpr") {
        return $0.map((v1) => $Cont("ContExpr", v1))(traversableExpr.traverse(dictApplicative)(f)(m._1));
      }
      if (m.tag === "ContElim") {
        return $0.map((v1) => $Cont("ContElim", v1))(traversableElim.traverse(dictApplicative)(f)(m._1));
      }
      fail();
    };
  },
  sequence: (dictApplicative) => (v) => traversableCont.traverse(dictApplicative)(identity17)(v),
  Functor0: () => functorCont,
  Foldable1: () => foldableCont
};
var expandableVarDefRawVarDef = (dictBoundedJoinSemilattice) => ({ expand: (v) => (v1) => $VarDef(expandableElimRawElim(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2)) });
var expandableRecDefsRawRecDe = (dictBoundedJoinSemilattice) => {
  const expandableDictDict2 = expandableDictDict({
    botOf: functorElim.map((() => {
      const $0 = dictBoundedJoinSemilattice.bot;
      return (v) => $0;
    })())
  });
  return { expand: (v) => (v1) => $RecDefs(v._1, expandableDictDict2(expandableElimRawElim(dictBoundedJoinSemilattice)).expand(v._2)(v1._2)) };
};
var expandableExprRawExpr = (dictBoundedJoinSemilattice) => ({
  expand: (v) => (v1) => {
    if (v.tag === "Var") {
      if (v1.tag === "Var") {
        return $Expr("Var", mustEq(eqString)(showString)(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Op") {
      if (v1.tag === "Op") {
        return $Expr("Op", mustEq(eqString)(showString)(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Int") {
      if (v1.tag === "Int") {
        return $Expr("Int", v._1, mustEq(eqInt)(showInt)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Str") {
      if (v1.tag === "Str") {
        return $Expr("Str", v._1, mustEq(eqString)(showString)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Float") {
      if (v1.tag === "Float") {
        return $Expr("Float", v._1, mustEq(eqNumber)(showNumber)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Dictionary") {
      if (v1.tag === "Dictionary") {
        return $Expr(
          "Dictionary",
          v._1,
          (() => {
            const $0 = expandableExprRawExpr(dictBoundedJoinSemilattice);
            return zipWith((v$1) => (v1$1) => $Pair($0.expand(v$1._1)(v1$1._1), $0.expand(v$1._2)(v1$1._2)))(v._2)(v1._2);
          })()
        );
      }
      return throwException(error("Shape mismatch"))();
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
      return throwException(error("Shape mismatch"))();
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
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Lambda") {
      if (v1.tag === "Lambda") {
        return $Expr("Lambda", v._1, expandableElimRawElim(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Project") {
      if (v1.tag === "Project") {
        return $Expr("Project", expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._1)(v1._1), mustEq(eqString)(showString)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "DProject") {
      if (v1.tag === "DProject") {
        return $Expr("DProject", expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "App") {
      if (v1.tag === "App") {
        return $Expr("App", expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Let") {
      if (v1.tag === "Let") {
        return $Expr("Let", expandableVarDefRawVarDef(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "LetRec" && v1.tag === "LetRec") {
      return $Expr("LetRec", expandableRecDefsRawRecDe(dictBoundedJoinSemilattice).expand(v._1)(v1._1), expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
    }
    return throwException(error("Shape mismatch"))();
  }
});
var expandableElimRawElim = (dictBoundedJoinSemilattice) => {
  const expandableDictDict2 = expandableDictDict({
    botOf: functorCont.map((() => {
      const $0 = dictBoundedJoinSemilattice.bot;
      return (v) => $0;
    })())
  });
  return {
    expand: (v) => (v1) => {
      if (v.tag === "ElimVar") {
        if (v1.tag === "ElimVar") {
          return $Elim("ElimVar", mustEq(eqString)(showString)(v._1)(v1._1), expandableContRawCont(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
        }
        return throwException(error("Shape mismatch"))();
      }
      if (v.tag === "ElimConstr") {
        if (v1.tag === "ElimConstr") {
          return $Elim("ElimConstr", expandableDictDict2(expandableContRawCont(dictBoundedJoinSemilattice)).expand(v._1)(v1._1));
        }
        return throwException(error("Shape mismatch"))();
      }
      if (v.tag === "ElimDict" && v1.tag === "ElimDict") {
        return $Elim("ElimDict", mustEq(eqSet)(showSet(showString))(v._1)(v1._1), expandableContRawCont(dictBoundedJoinSemilattice).expand(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
  };
};
var expandableContRawCont = (dictBoundedJoinSemilattice) => ({
  expand: (v) => (v1) => {
    if (v.tag === "ContExpr") {
      if (v1.tag === "ContExpr") {
        return $Cont("ContExpr", expandableExprRawExpr(dictBoundedJoinSemilattice).expand(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "ContElim" && v1.tag === "ContElim") {
      return $Cont("ContElim", expandableElimRawElim(dictBoundedJoinSemilattice).expand(v._1)(v1._1));
    }
    return throwException(error("Shape mismatch"))();
  }
});
var eqVarDef = (dictEq) => ({ eq: (x) => (y) => eqElim(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2) });
var eqRecDefs = (dictEq) => ({ eq: (x) => (y) => dictEq.eq(x._1)(y._1) && eqObject(eqElim(dictEq)).eq(x._2)(y._2) });
var eqExpr = (dictEq) => ({
  eq: (x) => (y) => {
    if (x.tag === "Var") {
      return y.tag === "Var" && x._1 === y._1;
    }
    if (x.tag === "Op") {
      return y.tag === "Op" && x._1 === y._1;
    }
    if (x.tag === "Int") {
      return y.tag === "Int" && dictEq.eq(x._1)(y._1) && x._2 === y._2;
    }
    if (x.tag === "Float") {
      return y.tag === "Float" && dictEq.eq(x._1)(y._1) && x._2 === y._2;
    }
    if (x.tag === "Str") {
      return y.tag === "Str" && dictEq.eq(x._1)(y._1) && x._2 === y._2;
    }
    if (x.tag === "Dictionary") {
      return y.tag === "Dictionary" && (() => {
        const $0 = eqExpr(dictEq);
        return dictEq.eq(x._1)(y._1) && (() => {
          const go = (v) => (v1) => (v2) => {
            if (!v2) {
              return false;
            }
            if (v.tag === "Nil") {
              return v1.tag === "Nil" && v2;
            }
            return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1._1)(v._1._1) && $0.eq(v1._1._2)(v._1._2));
          };
          return go(x._2)(y._2)(true);
        })();
      })();
    }
    if (x.tag === "Constr") {
      return y.tag === "Constr" && (() => {
        const $0 = eqExpr(dictEq);
        return dictEq.eq(x._1)(y._1) && x._2 === y._2 && (() => {
          const go = (v) => (v1) => (v2) => {
            if (!v2) {
              return false;
            }
            if (v.tag === "Nil") {
              return v1.tag === "Nil" && v2;
            }
            return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
          };
          return go(x._3)(y._3)(true);
        })();
      })();
    }
    if (x.tag === "Matrix") {
      return y.tag === "Matrix" && dictEq.eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2) && x._3._1 === y._3._1 && x._3._2 === y._3._2 && eqExpr(dictEq).eq(x._4)(y._4);
    }
    if (x.tag === "Lambda") {
      return y.tag === "Lambda" && dictEq.eq(x._1)(y._1) && eqElim(dictEq).eq(x._2)(y._2);
    }
    if (x.tag === "Project") {
      return y.tag === "Project" && eqExpr(dictEq).eq(x._1)(y._1) && x._2 === y._2;
    }
    if (x.tag === "DProject") {
      return y.tag === "DProject" && eqExpr(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2);
    }
    if (x.tag === "App") {
      return y.tag === "App" && eqExpr(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2);
    }
    if (x.tag === "Let") {
      return y.tag === "Let" && eqVarDef(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2);
    }
    return x.tag === "LetRec" && y.tag === "LetRec" && eqRecDefs(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2);
  }
});
var eqElim = (dictEq) => ({
  eq: (x) => (y) => {
    if (x.tag === "ElimVar") {
      return y.tag === "ElimVar" && x._1 === y._1 && eqCont(dictEq).eq(x._2)(y._2);
    }
    if (x.tag === "ElimConstr") {
      return y.tag === "ElimConstr" && eqObject(eqCont(dictEq)).eq(x._1)(y._1);
    }
    return x.tag === "ElimDict" && y.tag === "ElimDict" && eqMap(eqString)(eqUnit).eq(x._1)(y._1) && eqCont(dictEq).eq(x._2)(y._2);
  }
});
var eqCont = (dictEq) => ({
  eq: (x) => (y) => {
    if (x.tag === "ContExpr") {
      return y.tag === "ContExpr" && eqExpr(dictEq).eq(x._1)(y._1);
    }
    return x.tag === "ContElim" && y.tag === "ContElim" && eqElim(dictEq).eq(x._1)(y._1);
  }
});
var ordVarDef = (dictOrd) => {
  const eqVarDef1 = eqVarDef(dictOrd.Eq0());
  return {
    compare: (x) => (y) => {
      const v = ordElim(dictOrd).compare(x._1)(y._1);
      if (v === "LT") {
        return LT;
      }
      if (v === "GT") {
        return GT;
      }
      return ordExpr(dictOrd).compare(x._2)(y._2);
    },
    Eq0: () => eqVarDef1
  };
};
var ordRecDefs = (dictOrd) => {
  const eqRecDefs1 = eqRecDefs(dictOrd.Eq0());
  return {
    compare: (x) => (y) => {
      const v = dictOrd.compare(x._1)(y._1);
      if (v === "LT") {
        return LT;
      }
      if (v === "GT") {
        return GT;
      }
      return ordDict(ordElim(dictOrd)).compare(x._2)(y._2);
    },
    Eq0: () => eqRecDefs1
  };
};
var ordExpr = (dictOrd) => {
  const eqExpr1 = eqExpr(dictOrd.Eq0());
  return {
    compare: (x) => (y) => {
      if (x.tag === "Var") {
        if (y.tag === "Var") {
          return ordString.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "Var") {
        return GT;
      }
      if (x.tag === "Op") {
        if (y.tag === "Op") {
          return ordString.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "Op") {
        return GT;
      }
      if (x.tag === "Int") {
        if (y.tag === "Int") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordInt.compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "Int") {
        return GT;
      }
      if (x.tag === "Float") {
        if (y.tag === "Float") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordNumber.compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "Float") {
        return GT;
      }
      if (x.tag === "Str") {
        if (y.tag === "Str") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordString.compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "Str") {
        return GT;
      }
      if (x.tag === "Dictionary") {
        if (y.tag === "Dictionary") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordList(ordPair(ordExpr(dictOrd))).compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "Dictionary") {
        return GT;
      }
      if (x.tag === "Constr") {
        if (y.tag === "Constr") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          const v1 = ordString.compare(x._2)(y._2);
          if (v1 === "LT") {
            return LT;
          }
          if (v1 === "GT") {
            return GT;
          }
          return ordList(ordExpr(dictOrd)).compare(x._3)(y._3);
        }
        return LT;
      }
      if (y.tag === "Constr") {
        return GT;
      }
      if (x.tag === "Matrix") {
        if (y.tag === "Matrix") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          const v1 = ordExpr(dictOrd).compare(x._2)(y._2);
          if (v1 === "LT") {
            return LT;
          }
          if (v1 === "GT") {
            return GT;
          }
          const v2 = compare3(x._3)(y._3);
          if (v2 === "LT") {
            return LT;
          }
          if (v2 === "GT") {
            return GT;
          }
          return ordExpr(dictOrd).compare(x._4)(y._4);
        }
        return LT;
      }
      if (y.tag === "Matrix") {
        return GT;
      }
      if (x.tag === "Lambda") {
        if (y.tag === "Lambda") {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordElim(dictOrd).compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "Lambda") {
        return GT;
      }
      if (x.tag === "Project") {
        if (y.tag === "Project") {
          const v = ordExpr(dictOrd).compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordString.compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "Project") {
        return GT;
      }
      if (x.tag === "DProject") {
        if (y.tag === "DProject") {
          const v = ordExpr(dictOrd).compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordExpr(dictOrd).compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "DProject") {
        return GT;
      }
      if (x.tag === "App") {
        if (y.tag === "App") {
          const v = ordExpr(dictOrd).compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordExpr(dictOrd).compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "App") {
        return GT;
      }
      if (x.tag === "Let") {
        if (y.tag === "Let") {
          const v = ordVarDef(dictOrd).compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordExpr(dictOrd).compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "Let") {
        return GT;
      }
      if (x.tag === "LetRec" && y.tag === "LetRec") {
        const v = ordRecDefs(dictOrd).compare(x._1)(y._1);
        if (v === "LT") {
          return LT;
        }
        if (v === "GT") {
          return GT;
        }
        return ordExpr(dictOrd).compare(x._2)(y._2);
      }
      fail();
    },
    Eq0: () => eqExpr1
  };
};
var ordElim = (dictOrd) => {
  const eqElim1 = eqElim(dictOrd.Eq0());
  return {
    compare: (x) => (y) => {
      if (x.tag === "ElimVar") {
        if (y.tag === "ElimVar") {
          const v = ordString.compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordCont(dictOrd).compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "ElimVar") {
        return GT;
      }
      if (x.tag === "ElimConstr") {
        if (y.tag === "ElimConstr") {
          return ordDict(ordCont(dictOrd)).compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "ElimConstr") {
        return GT;
      }
      if (x.tag === "ElimDict" && y.tag === "ElimDict") {
        const v = compare4(x._1)(y._1);
        if (v === "LT") {
          return LT;
        }
        if (v === "GT") {
          return GT;
        }
        return ordCont(dictOrd).compare(x._2)(y._2);
      }
      fail();
    },
    Eq0: () => eqElim1
  };
};
var ordCont = (dictOrd) => {
  const eqCont1 = eqCont(dictOrd.Eq0());
  return {
    compare: (x) => (y) => {
      if (x.tag === "ContExpr") {
        if (y.tag === "ContExpr") {
          return ordExpr(dictOrd).compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "ContExpr") {
        return GT;
      }
      if (x.tag === "ContElim" && y.tag === "ContElim") {
        return ordElim(dictOrd).compare(x._1)(y._1);
      }
      fail();
    },
    Eq0: () => eqCont1
  };
};
var applyRecDefs = {
  apply: (v) => (v1) => $RecDefs(v._1(v1._1), intersectionWith_Object(apply)(_fmapObject(v._2, applyElim.apply))(v1._2)),
  Functor0: () => functorRecDefs
};
var applyExpr = {
  apply: (v) => (v1) => {
    if (v.tag === "Var") {
      if (v1.tag === "Var") {
        return $Expr("Var", mustEq(eqString)(showString)(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Op") {
      if (v1.tag === "Op") {
        return $Expr("Op", v._1);
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Int") {
      if (v1.tag === "Int") {
        return $Expr("Int", v._1(v1._1), mustEq(eqInt)(showInt)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Float") {
      if (v1.tag === "Float") {
        return $Expr("Float", v._1(v1._1), mustEq(eqNumber)(showNumber)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Str") {
      if (v1.tag === "Str") {
        return $Expr("Str", v._1(v1._1), mustEq(eqString)(showString)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Dictionary") {
      if (v1.tag === "Dictionary") {
        return $Expr("Dictionary", v._1(v1._1), zipWith((a) => (b) => $Pair(applyExpr.apply(a._1)(b._1), applyExpr.apply(a._2)(b._2)))(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Constr") {
      if (v1.tag === "Constr") {
        return $Expr("Constr", v._1(v1._1), mustEq(eqString)(showString)(v._2)(v1._2), zipWith(applyExpr.apply)(v._3)(v1._3));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Matrix") {
      if (v1.tag === "Matrix") {
        return $Expr(
          "Matrix",
          v._1(v1._1),
          applyExpr.apply(v._2)(v1._2),
          $Tuple(mustEq(eqString)(showString)(v._3._1)(v1._3._1), mustEq(eqString)(showString)(v._3._2)(v1._3._2)),
          applyExpr.apply(v._4)(v1._4)
        );
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Lambda") {
      if (v1.tag === "Lambda") {
        return $Expr("Lambda", v._1(v1._1), applyElim.apply(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Project") {
      if (v1.tag === "Project") {
        return $Expr("Project", applyExpr.apply(v._1)(v1._1), v._2);
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "App") {
      if (v1.tag === "App") {
        return $Expr("App", applyExpr.apply(v._1)(v1._1), applyExpr.apply(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Let") {
      if (v1.tag === "Let") {
        return $Expr("Let", $VarDef(applyElim.apply(v._1._1)(v1._1._1), applyExpr.apply(v._1._2)(v1._1._2)), applyExpr.apply(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "LetRec") {
      if (v1.tag === "LetRec") {
        return $Expr("LetRec", applyRecDefs.apply(v._1)(v1._1), applyExpr.apply(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "DProject" && v1.tag === "DProject") {
      return $Expr("DProject", applyExpr.apply(v._1)(v1._1), applyExpr.apply(v._2)(v1._2));
    }
    return throwException(error("Shape mismatch"))();
  },
  Functor0: () => functorExpr
};
var applyElim = {
  apply: (v) => (v1) => {
    if (v.tag === "ElimVar") {
      if (v1.tag === "ElimVar") {
        return $Elim("ElimVar", v._1, applyCont.apply(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "ElimConstr") {
      if (v1.tag === "ElimConstr") {
        return $Elim("ElimConstr", intersectionWith_Object(apply)(_fmapObject(v._1, applyCont.apply))(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "ElimDict" && v1.tag === "ElimDict") {
      return $Elim("ElimDict", v._1, applyCont.apply(v._2)(v1._2));
    }
    return throwException(error("Shape mismatch"))();
  },
  Functor0: () => functorElim
};
var applyCont = {
  apply: (v) => (v1) => {
    if (v.tag === "ContExpr") {
      if (v1.tag === "ContExpr") {
        return $Cont("ContExpr", applyExpr.apply(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "ContElim" && v1.tag === "ContElim") {
      return $Cont("ContElim", applyElim.apply(v._1)(v1._1));
    }
    return throwException(error("Shape mismatch"))();
  },
  Functor0: () => functorCont
};
var fVDict = (dictFV) => {
  const fv1 = dictFV.fv;
  return {
    fv: (\u03C1) => setSet2.difference(fold((z) => (v) => union(ordString)(z))(Leaf2)(_fmapObject(\u03C1, fv1)))(fromFoldable5(mapObjectString.keys(\u03C1)))
  };
};
var foldlModuleDef = (v) => (v1) => (v2) => {
  if (v2.tag === "Left") {
    return foldableVarDef.foldl(v)(v1)(v2._1);
  }
  if (v2.tag === "Right") {
    return foldableRecDefs.foldl(v)(v1)(v2._1);
  }
  fail();
};
var foldableModule = {
  foldl: (v) => (v1) => (v2) => {
    if (v2.tag === "Nil") {
      return v1;
    }
    if (v2.tag === "Cons") {
      if (v2._1.tag === "Left") {
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
              go$a0 = foldlModuleDef(v)(b)(v$1._1);
              go$a1 = v$1._2;
              continue;
            }
            fail();
          }
          return go$r;
        };
        return go(foldableVarDef.foldl(v)(v1)(v2._1._1))(v2._2);
      }
      if (v2._1.tag === "Right") {
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
              go$a0 = foldlModuleDef(v)(b)(v$1._1);
              go$a1 = v$1._2;
              continue;
            }
            fail();
          }
          return go$r;
        };
        return go(foldableRecDefs.foldl(v)(v1)(v2._1._1))(v2._2);
      }
    }
    fail();
  },
  foldr: (f) => foldrDefault(foldableModule)(f),
  foldMap: (dictMonoid) => (f) => foldableModule.foldl((acc) => (x) => dictMonoid.Semigroup0().append(acc)(f(x)))(dictMonoid.mempty)
};
var traversableModule = {
  traverse: (dictApplicative) => {
    const Apply0 = dictApplicative.Apply0();
    const $0 = Apply0.Functor0();
    const traverse5 = traversableVarDef.traverse(dictApplicative);
    const traverse6 = traversableRecDefs.traverse(dictApplicative);
    return (v) => (v1) => {
      if (v1.tag === "Nil") {
        return dictApplicative.pure(Nil);
      }
      if (v1.tag === "Cons") {
        if (v1._1.tag === "Left") {
          return $0.map(Module)(Apply0.apply(Apply0.Functor0().map(Cons)($0.map(Left)(traverse5(v)(v1._1._1))))($0.map(unsafeCoerce)(traversableModule.traverse(dictApplicative)(v)(v1._2))));
        }
        if (v1._1.tag === "Right") {
          return $0.map(Module)(Apply0.apply(Apply0.Functor0().map(Cons)($0.map(Right)(traverse6(v)(v1._1._1))))($0.map(unsafeCoerce)(traversableModule.traverse(dictApplicative)(v)(v1._2))));
        }
      }
      fail();
    };
  },
  sequence: (dictApplicative) => traversableModule.traverse(dictApplicative)(identity7),
  Functor0: () => functorModule,
  Foldable1: () => foldableModule
};
var bVElim = {
  bv: (v) => {
    if (v.tag === "ElimVar") {
      return setSet2.union($$$Map("Two", Leaf2, v._1, void 0, Leaf2))(bVCont.bv(v._2));
    }
    if (v.tag === "ElimConstr") {
      return bVCont.bv(asMaplet2(v._1)._2);
    }
    if (v.tag === "ElimDict") {
      return bVCont.bv(v._2);
    }
    fail();
  }
};
var bVCont = {
  bv: (v) => {
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
      return $$$Map("Two", Leaf2, v._1, void 0, Leaf2);
    }
    if (v.tag === "Op") {
      return $$$Map("Two", Leaf2, v._1, void 0, Leaf2);
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
    if (v.tag === "Dictionary") {
      return unions3(listMap((v1) => setSet2.union(fVExpr.fv(v1._1))(fVExpr.fv(v1._2)))(v._2));
    }
    if (v.tag === "Constr") {
      return unions3(listMap(fVExpr.fv)(v._3));
    }
    if (v.tag === "Matrix") {
      return setSet2.union(fVExpr.fv(v._2))(fVExpr.fv(v._4));
    }
    if (v.tag === "Lambda") {
      return fVElim.fv(v._2);
    }
    if (v.tag === "Project") {
      return fVExpr.fv(v._1);
    }
    if (v.tag === "DProject") {
      return setSet2.union(fVExpr.fv(v._1))(fVExpr.fv(v._2));
    }
    if (v.tag === "App") {
      return setSet2.union(fVExpr.fv(v._1))(fVExpr.fv(v._2));
    }
    if (v.tag === "Let") {
      return setSet2.union(fVExpr.fv(v._1._2))(setSet2.difference(fVExpr.fv(v._2))(bVElim.bv(v._1._1)));
    }
    if (v.tag === "LetRec") {
      return setSet2.union(fVDict(fVElim).fv(v._1._2))(fVExpr.fv(v._2));
    }
    fail();
  }
};
var fVElim = {
  fv: (v) => {
    if (v.tag === "ElimVar") {
      return setSet2.difference(fVCont.fv(v._2))($$$Map("Two", Leaf2, v._1, void 0, Leaf2));
    }
    if (v.tag === "ElimConstr") {
      return fold((z) => (v$1) => union(ordString)(z))(Leaf2)(_fmapObject(v._1, fVCont.fv));
    }
    if (v.tag === "ElimDict") {
      return fVCont.fv(v._2);
    }
    fail();
  }
};
var fVCont = {
  fv: (v) => {
    if (v.tag === "ContElim") {
      return fVElim.fv(v._1);
    }
    if (v.tag === "ContExpr") {
      return fVExpr.fv(v._1);
    }
    fail();
  }
};
var asElim = (v) => {
  if (v.tag === "ContElim") {
    return v._1;
  }
  return throwException(error("Eliminator expected"))();
};

// output-es/Data.CatQueue/index.js
var $CatQueue = (_1, _2) => ({ tag: "CatQueue", _1, _2 });
var uncons3 = (uncons$a0$copy) => {
  let uncons$a0 = uncons$a0$copy, uncons$c = true, uncons$r;
  while (uncons$c) {
    const v = uncons$a0;
    if (v._1.tag === "Nil") {
      if (v._2.tag === "Nil") {
        uncons$c = false;
        uncons$r = Nothing;
        continue;
      }
      uncons$a0 = $CatQueue(reverse(v._2), Nil);
      continue;
    }
    if (v._1.tag === "Cons") {
      uncons$c = false;
      uncons$r = $Maybe("Just", $Tuple(v._1._1, $CatQueue(v._1._2, v._2)));
      continue;
    }
    fail();
  }
  return uncons$r;
};

// output-es/Data.CatList/index.js
var $CatList = (tag, _1, _2) => ({ tag, _1, _2 });
var CatNil = /* @__PURE__ */ $CatList("CatNil");
var link = (v) => (v1) => {
  if (v.tag === "CatNil") {
    return v1;
  }
  if (v1.tag === "CatNil") {
    return v;
  }
  if (v.tag === "CatCons") {
    return $CatList("CatCons", v._1, $CatQueue(v._2._1, $List("Cons", v1, v._2._2)));
  }
  fail();
};
var foldr = (k) => (b) => (q) => {
  const foldl = (foldl$a0$copy) => (foldl$a1$copy) => (foldl$a2$copy) => {
    let foldl$a0 = foldl$a0$copy, foldl$a1 = foldl$a1$copy, foldl$a2 = foldl$a2$copy, foldl$c = true, foldl$r;
    while (foldl$c) {
      const v = foldl$a0, v1 = foldl$a1, v2 = foldl$a2;
      if (v2.tag === "Nil") {
        foldl$c = false;
        foldl$r = v1;
        continue;
      }
      if (v2.tag === "Cons") {
        foldl$a0 = v;
        foldl$a1 = v(v1)(v2._1);
        foldl$a2 = v2._2;
        continue;
      }
      fail();
    }
    return foldl$r;
  };
  const go = (go$a0$copy) => (go$a1$copy) => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const xs = go$a0, ys = go$a1;
      const v = uncons3(xs);
      if (v.tag === "Nothing") {
        go$c = false;
        go$r = foldl((x) => (i) => i(x))(b)(ys);
        continue;
      }
      if (v.tag === "Just") {
        go$a0 = v._1._2;
        go$a1 = $List("Cons", k(v._1._1), ys);
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go(q)(Nil);
};
var uncons4 = (v) => {
  if (v.tag === "CatNil") {
    return Nothing;
  }
  if (v.tag === "CatCons") {
    return $Maybe("Just", $Tuple(v._1, v._2._1.tag === "Nil" && v._2._2.tag === "Nil" ? CatNil : foldr(link)(CatNil)(v._2)));
  }
  fail();
};
var singleton5 = (a) => $CatList("CatCons", a, $CatQueue(Nil, Nil));
var semigroupCatList = { append: link };
var monoidCatList = { mempty: CatNil, Semigroup0: () => semigroupCatList };

// output-es/Data.Graph/index.js
var $SortStep = (tag, _1) => ({ tag, _1 });
var fromFoldable6 = /* @__PURE__ */ (() => {
  const foldMap1 = foldableList.foldMap(monoidCatList);
  return (f) => foldMap1(singleton5)(f);
})();
var fromFoldable14 = /* @__PURE__ */ (() => {
  const foldMap1 = foldableArray.foldMap(monoidCatList);
  return (f) => foldMap1(singleton5)(f);
})();
var Visit = (value0) => $SortStep("Visit", value0);
var topologicalSort = (dictOrd) => (v) => {
  const visit = (visit$a0$copy) => (visit$a1$copy) => {
    let visit$a0 = visit$a0$copy, visit$a1 = visit$a1$copy, visit$c = true, visit$r;
    while (visit$c) {
      const state = visit$a0, stack = visit$a1;
      const v1 = uncons4(stack);
      if (v1.tag === "Nothing") {
        visit$c = false;
        visit$r = state;
        continue;
      }
      if (v1.tag === "Just") {
        if (v1._1._1.tag === "Emit") {
          visit$a0 = { result: $List("Cons", v1._1._1._1, state.result), unvisited: state.unvisited };
          visit$a1 = v1._1._2;
          continue;
        }
        if (v1._1._1.tag === "Visit") {
          if ((() => {
            const $0 = lookup(dictOrd)(v1._1._1._1)(state.unvisited);
            if ($0.tag === "Nothing") {
              return false;
            }
            if ($0.tag === "Just") {
              return true;
            }
            fail();
          })()) {
            visit$a0 = { result: state.result, unvisited: $$delete(dictOrd)(v1._1._1._1)(state.unvisited) };
            visit$a1 = (() => {
              const $0 = fromFoldable6(listMap(Visit)((() => {
                const $02 = lookup(dictOrd)(v1._1._1._1)(v);
                if ($02.tag === "Nothing") {
                  return Nil;
                }
                if ($02.tag === "Just") {
                  return $02._1._2;
                }
                fail();
              })()));
              const $1 = v1._1._2.tag === "CatNil" ? $CatList("CatCons", $SortStep("Emit", v1._1._1._1), $CatQueue(Nil, Nil)) : $CatList(
                "CatCons",
                $SortStep("Emit", v1._1._1._1),
                $CatQueue(Nil, $List("Cons", v1._1._2, Nil))
              );
              if ($0.tag === "CatNil") {
                return $1;
              }
              if ($1.tag === "CatNil") {
                return $0;
              }
              if ($0.tag === "CatCons") {
                return $CatList("CatCons", $0._1, $CatQueue($0._2._1, $List("Cons", $1, $0._2._2)));
              }
              fail();
            })();
            continue;
          }
          visit$a0 = state;
          visit$a1 = v1._1._2;
          continue;
        }
      }
      fail();
    }
    return visit$r;
  };
  const go = (go$a0$copy) => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v1 = go$a0;
      const v2 = findMin(v1.unvisited);
      if (v2.tag === "Just") {
        go$a0 = visit(v1)(fromFoldable14([$SortStep("Visit", v2._1.key)]));
        continue;
      }
      if (v2.tag === "Nothing") {
        go$c = false;
        go$r = v1.result;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go({ unvisited: v, result: Nil });
};

// output-es/Foreign.Object.ST/foreign.js
function peekImpl2(just) {
  return function(nothing) {
    return function(k) {
      return function(m) {
        return function() {
          return {}.hasOwnProperty.call(m, k) ? just(m[k]) : nothing;
        };
      };
    };
  };
}

// output-es/Foreign.Object.ST/index.js
var peek = /* @__PURE__ */ peekImpl2(Just)(Nothing);

// output-es/Graph.GraphImpl/index.js
var $GraphImpl = (_1) => ({ tag: "GraphImpl", _1 });
var eqSet2 = { eq: (v) => (v1) => eqMap(eqVertex)(eqUnit).eq(v)(v1) };
var eq = /* @__PURE__ */ (() => eqObject(eqSet2).eq)();
var fromFoldable15 = /* @__PURE__ */ foldlArray((m) => (a) => insert(ordVertex)(a)()(m))(Leaf2);
var toUnfoldable6 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
var fromFoldable23 = /* @__PURE__ */ (() => foldableSet.foldr(Cons)(Nil))();
var fromFoldable32 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordVertex)(a)()(m))(Leaf2))();
var toUnfoldable12 = /* @__PURE__ */ toAscUnfoldable(unfoldableArray);
var fromFoldable42 = /* @__PURE__ */ fromFoldable(ordVertex)(foldableArray);
var verticesGraphImpl = {
  vertices: (v) => fold((z) => (v$1) => (a) => insert(ordDVertex)(a)()(z))(Leaf2)(_mapWithKey(
    v._1.out,
    (k) => (v1) => $Tuple(k, v1._2)
  ))
};
var eqGraphImpl = { eq: (v) => (v1) => eq(_fmapObject(v._1.out, fst))(_fmapObject(v1._1.out, fst)) };
var sinks$p = (m) => fromFoldable15(arrayMap((x) => x._1)(filter2((x) => x._2._1.tag === "Leaf")(toArrayWithKey(Tuple)(m))));
var init4 = (\u03B1s) => () => {
  const obj = {};
  return monadRecST.tailRecM((v) => {
    if (v._1.tag === "Nil") {
      return () => $Step("Done", v._2);
    }
    if (v._1.tag === "Cons") {
      const $0 = v._1._1._1;
      const $1 = v._1._2;
      const $2 = v._2;
      return () => {
        $2[$0] = $Tuple(Leaf2, v._1._1._2);
        return $Step("Loop", $Tuple($1, $2));
      };
    }
    fail();
  })($Tuple(\u03B1s, obj))();
};
var assertPresent = (v) => (v1) => {
  if (v1.tag === "Nil") {
    return () => $Step("Done", void 0);
  }
  if (v1.tag === "Cons") {
    const $0 = v1._1;
    const $1 = v1._2;
    const $2 = peek($0)(v);
    return () => {
      const $3 = $2();
      const present = (() => {
        if ($3.tag === "Nothing") {
          return false;
        }
        if ($3.tag === "Just") {
          return true;
        }
        fail();
      })();
      return assertWhen(true)($0 + " is an existing vertex")((v2) => present)(() => $Step("Loop", $1))();
    };
  }
  fail();
};
var outMap = (\u03B1s) => (es) => {
  const $0 = init4(\u03B1s);
  return () => {
    const out = $0();
    return monadRecST.tailRecM((v) => {
      if (v._1.tag === "Nil") {
        return () => $Step("Done", v._2);
      }
      if (v._1.tag === "Cons") {
        const $1 = v._2;
        const $2 = v._1._2;
        const $3 = v._1._1._1._2;
        const $4 = v._1._1._1._1;
        const $5 = v._1._1._2;
        const $6 = peek($4)($1);
        return () => {
          const $7 = $6();
          if ((() => {
            if ($7.tag === "Nothing") {
              return true;
            }
            if ($7.tag === "Just") {
              return eqMap(eqVertex)(eqUnit).eq($7._1._1)(Leaf2);
            }
            fail();
          })()) {
            monadRecST.tailRecM(assertPresent($1))(toUnfoldable6($5))();
            $1[$4] = $Tuple($5, $3);
            return $Step("Loop", $Tuple($2, $1));
          }
          return throwException(error("Duplicate edge list entry for " + showStringImpl($4)))()();
        };
      }
      fail();
    })($Tuple(es, out))();
  };
};
var addIfMissing = (acc) => (v) => {
  const $0 = v._2;
  const $1 = v._1;
  const $2 = peek($1)(acc);
  return () => {
    const v1 = $2();
    if (v1.tag === "Nothing") {
      acc[$1] = $Tuple(Leaf2, $0);
      return acc;
    }
    if (v1.tag === "Just") {
      return acc;
    }
    fail();
  };
};
var inMap = (\u03B1s) => (es) => {
  const $0 = init4(\u03B1s);
  return () => {
    const in_ = $0();
    return monadRecST.tailRecM((v) => {
      if (v._1.tag === "Nil") {
        return () => $Step("Done", v._2);
      }
      if (v._1.tag === "Cons") {
        const $1 = v._1._2;
        const $2 = v._1._1._1._2;
        const $3 = v._1._1._1._1;
        const $4 = monadRecST.tailRecM((v2) => {
          if (v2._1.tag === "Nil") {
            return () => $Step("Done", v2._2);
          }
          if (v2._1.tag === "Cons") {
            const $42 = v2._2;
            const $5 = v2._1._1;
            const $6 = v2._1._2;
            const $7 = peek($5)($42);
            return () => {
              const v1 = $7();
              const acc$p = (() => {
                if (v1.tag === "Nothing") {
                  $42[$5] = $Tuple($$$Map("Two", Leaf2, $3, void 0, Leaf2), $2);
                  return $42;
                }
                if (v1.tag === "Just") {
                  $42[$5] = $Tuple(insert(ordVertex)($3)()(v1._1._1), $2);
                  return $42;
                }
                fail();
              })();
              return $Step("Loop", $Tuple($6, acc$p));
            };
          }
          fail();
        })($Tuple(toUnfoldable6(v._1._1._2), v._2));
        return () => {
          const $5 = $4();
          const acc$p = addIfMissing($5)($Tuple($3, $2))();
          return $Step("Loop", $Tuple($1, acc$p));
        };
      }
      fail();
    })($Tuple(es, in_))();
  };
};
var graphGraphImpl = {
  outN: (v) => (\u03B1) => definitely("in graph")(_lookup(Nothing, Just, \u03B1, v._1.out))._1,
  vertexData: (v) => (\u03B1) => definitely("in graph")(_lookup(Nothing, Just, \u03B1, v._1.out))._2,
  inN: (g) => graphGraphImpl.outN(graphGraphImpl.op(g)),
  elem: (\u03B1) => (v) => {
    const $0 = _lookup(Nothing, Just, \u03B1, v._1.out);
    if ($0.tag === "Nothing") {
      return false;
    }
    if ($0.tag === "Just") {
      return true;
    }
    fail();
  },
  size: (v) => size3(v._1.out),
  sinks: (v) => v._1.sinks,
  sources: (v) => v._1.sources,
  op: (v) => $GraphImpl({ out: v._1.in_, in_: v._1.out, sinks: v._1.sources, sources: v._1.sinks, vertices: v._1.vertices }),
  empty: /* @__PURE__ */ $GraphImpl({
    out: empty,
    in_: empty,
    sinks: Leaf2,
    sources: Leaf2,
    vertices: Leaf2
  }),
  fromEdgeList: (\u03B1s) => (es) => {
    const \u03B1s$p = fromFoldable23(\u03B1s);
    const es$p = reverse(es);
    const in_ = inMap(\u03B1s$p)(es$p)();
    const out = outMap(\u03B1s$p)(es$p)();
    return $GraphImpl({
      out,
      in_,
      sinks: sinks$p(out),
      sources: sinks$p(in_),
      vertices: fromFoldable32(map(ordVertex)(Vertex)(mapObjectString.keys(out)))
    });
  },
  topologicalSort: (v) => reverse(topologicalSort(ordVertex)(fromFoldable42(arrayMap((x) => $Tuple(
    x._1,
    $Tuple(void 0, x._2)
  ))(toUnfoldable12(_fmapObject(_fmapObject(v._1.out, fst), toUnfoldable6)))))),
  Eq0: () => eqGraphImpl,
  Vertices1: () => verticesGraphImpl
};

// output-es/Graph.WithGraph/index.js
var fromFoldable7 = /* @__PURE__ */ (() => {
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
        go$a0 = insert(ordVertex)(v._1)()(b);
        go$a1 = v._2;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go(Leaf2);
})();
var monadWithGraphWithGraphT = (dictMonad) => {
  const $0 = dictMonad.Bind1().Apply0().Functor0();
  const $1 = monadStateStateT(dictMonad);
  const monadStateT = { Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) };
  return {
    extend: (\u03B1) => (\u03B1s) => {
      const $2 = Cons($Tuple(\u03B1, \u03B1s));
      const $3 = $1.state((s) => $Tuple(void 0, $2(s)));
      return (s) => $0.map((v1) => $Tuple(void 0, v1._2))($3(s));
    },
    Monad0: () => monadStateT
  };
};
var monadAllocAllocT = (dictMonad) => {
  const monadStateT = { Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) };
  return {
    fresh: bindStateT(dictMonad).bind(monadStateStateT(dictMonad).state((s) => {
      const s$p = 1 + s | 0;
      return $Tuple(s$p, s$p);
    }))((n) => applicativeStateT(dictMonad).pure(showIntImpl(n))),
    Monad0: () => monadStateT
  };
};
var runAllocT = (dictMonad) => (m) => (n) => dictMonad.Bind1().bind(m(n))((v) => dictMonad.Applicative0().pure($Tuple(
  v._2,
  $Tuple(
    fromFoldable7(listMap((x) => showIntImpl(x))((() => {
      const $0 = n + 1 | 0;
      if (v._2 < $0) {
        return Nil;
      }
      return range($0)(v._2);
    })())),
    v._1
  )
)));
var monadAllocWithGraphAllocT = (dictMonad) => {
  const monadStateT = { Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) };
  const monadStateT1 = { Applicative0: () => applicativeStateT(monadStateT), Bind1: () => bindStateT(monadStateT) };
  return {
    fresh: (() => {
      const $0 = monadAllocAllocT(dictMonad).fresh;
      return (s) => monadStateT.Bind1().bind($0)((x) => monadStateT.Applicative0().pure($Tuple(x, s)));
    })(),
    Monad0: () => monadStateT1
  };
};
var freezeGraph = (dictMonad) => {
  const $0 = dictMonad.Bind1().Apply0().Functor0();
  return (dictGraph) => (m) => (\u03B1s) => {
    const $1 = dictGraph.fromEdgeList(\u03B1s);
    return $0.map((v) => $Tuple($1(spyWhen(false)("runWithGraphT edge list")(showEdgeList)(v._1)), v._2))($0.map(swap)(m(Nil)));
  };
};
var runWithGraphT = (dictMonad) => {
  const freezeGraph1 = freezeGraph(dictMonad);
  return (dictGraph) => {
    const freezeGraph2 = freezeGraph1(dictGraph);
    return (m) => (\u03B1s) => dictMonad.Bind1().bind(freezeGraph2(m)(\u03B1s))((v) => {
      const $0 = v._1;
      return assertWhen(true)("edgeListGC")((v1) => dictGraph.Eq0().eq($0)(dictGraph.fromEdgeList(Leaf2)(toEdgeList(dictGraph)($0))))(dictMonad.Applicative0().pure($Tuple(
        $0,
        v._2
      )));
    });
  };
};
var runWithGraphT_spy = (dictMonad) => {
  const runWithGraphT2 = runWithGraphT(dictMonad);
  const spyFunWhenM2 = spyFunWhenM(dictMonad.Bind1().Apply0().Functor0());
  return (dictGraph) => {
    const runWithGraphT3 = runWithGraphT2(dictGraph);
    return (wg) => (\u03B1s) => spyFunWhenM2(false)("runWithGraphT")(showVertices$p)((x) => showEdgeList(toEdgeList(dictGraph)(x._1)))(runWithGraphT3(wg))(\u03B1s);
  };
};
var runWithGraphT_spy1 = /* @__PURE__ */ runWithGraphT_spy(monadIdentity);
var monadWithGraphAllocWithGr = (dictMonadError) => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  const monadStateT = { Applicative0: () => applicativeStateT(Monad0), Bind1: () => bindStateT(Monad0) };
  const bindStateT2 = bindStateT(monadStateT);
  const monadAllocWithGraphAllocT1 = monadAllocWithGraphAllocT(Monad0);
  const fresh1 = monadAllocWithGraphAllocT1.fresh;
  const monadWithGraphWithGraphT1 = monadWithGraphWithGraphT(monadStateT);
  const monadErrorStateT2 = monadErrorStateT(monadErrorStateT(dictMonadError));
  return {
    new: (dictTypeName) => (constr) => (\u03B1s) => (vd) => bindStateT2.bind(fresh1)((\u03B1) => bindStateT2.bind(monadWithGraphWithGraphT1.extend($Tuple(\u03B1, (k) => k(dictTypeName)(vd)))(\u03B1s))(() => applicativeStateT(monadStateT).pure(constr(\u03B1)(vd)))),
    MonadAlloc0: () => monadAllocWithGraphAllocT1,
    MonadError1: () => monadErrorStateT2,
    MonadWithGraph2: () => monadWithGraphWithGraphT1
  };
};

// output-es/Data.Bifoldable/index.js
var bifoldableTuple = {
  bifoldMap: (dictMonoid) => (f) => (g) => (v) => dictMonoid.Semigroup0().append(f(v._1))(g(v._2)),
  bifoldr: (f) => (g) => (z) => (v) => f(v._1)(g(v._2)(z)),
  bifoldl: (f) => (g) => (z) => (v) => g(f(z)(v._1))(v._2)
};

// output-es/Data.Bifunctor/index.js
var bifunctorTuple = { bimap: (f) => (g) => (v) => $Tuple(f(v._1), g(v._2)) };

// output-es/Data.Bitraversable/index.js
var bitraversableTuple = {
  bitraverse: (dictApplicative) => {
    const Apply0 = dictApplicative.Apply0();
    return (f) => (g) => (v) => Apply0.apply(Apply0.Functor0().map(Tuple)(f(v._1)))(g(v._2));
  },
  bisequence: (dictApplicative) => {
    const Apply0 = dictApplicative.Apply0();
    return (v) => Apply0.apply(Apply0.Functor0().map(Tuple)(v._1))(v._2);
  },
  Bifunctor0: () => bifunctorTuple,
  Bifoldable1: () => bifoldableTuple
};

// output-es/Data.Semiring/foreign.js
var intAdd = function(x) {
  return function(y) {
    return x + y | 0;
  };
};
var intMul = function(x) {
  return function(y) {
    return x * y | 0;
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
var intSub = function(x) {
  return function(y) {
    return x - y | 0;
  };
};
var numSub = function(n1) {
  return function(n2) {
    return n1 - n2;
  };
};

// output-es/Data.EuclideanRing/foreign.js
var intDiv2 = function(x) {
  return function(y) {
    if (y === 0)
      return 0;
    return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
  };
};
var intMod = function(x) {
  return function(y) {
    if (y === 0)
      return 0;
    var yy = Math.abs(y);
    return (x % yy + yy) % yy;
  };
};
var numDiv = function(n1) {
  return function(n2) {
    return n1 / n2;
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
            var length5 = str.length;
            if (index3 < 0 || index3 >= length5)
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
var _fromCodePointArray = function(singleton7) {
  return hasFromCodePoint ? function(cps) {
    if (cps.length < 1e4) {
      return String.fromCodePoint.apply(String, cps);
    }
    return cps.map(singleton7).join("");
  } : function(cps) {
    return cps.map(singleton7).join("");
  };
};
var _singleton = function(fallback) {
  return hasFromCodePoint ? String.fromCodePoint : fallback;
};
var _take = function(fallback) {
  return function(n) {
    if (hasStringIterator) {
      return function(str) {
        var accum = "";
        var iter = str[Symbol.iterator]();
        for (var i = 0; i < n; ++i) {
          var o = iter.next();
          if (o.done)
            return accum;
          accum += o.value;
        }
        return accum;
      };
    }
    return fallback(n);
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
var uncons5 = (s) => {
  const v = length(s);
  if (v === 0) {
    return Nothing;
  }
  if (v === 1) {
    return $Maybe("Just", { head: toCharCode(charAt(0)(s)), tail: "" });
  }
  const cu1 = toCharCode(charAt(1)(s));
  const cu0 = toCharCode(charAt(0)(s));
  if (55296 <= cu0 && cu0 <= 56319 && 56320 <= cu1 && cu1 <= 57343) {
    return $Maybe("Just", { head: (((cu0 - 55296 | 0) * 1024 | 0) + (cu1 - 56320 | 0) | 0) + 65536 | 0, tail: drop(2)(s) });
  }
  return $Maybe("Just", { head: cu0, tail: drop(1)(s) });
};
var unconsButWithTuple = (s) => {
  const $0 = uncons5(s);
  if ($0.tag === "Just") {
    return $Maybe("Just", $Tuple($0._1.head, $0._1.tail));
  }
  return Nothing;
};
var toCodePointArrayFallback = (s) => unfoldableArray.unfoldr(unconsButWithTuple)(s);
var unsafeCodePointAt0Fallback = (s) => {
  const cu0 = toCharCode(charAt(0)(s));
  if (55296 <= cu0 && cu0 <= 56319 && length(s) > 1) {
    const cu1 = toCharCode(charAt(1)(s));
    if (56320 <= cu1 && cu1 <= 57343) {
      return (((cu0 - 55296 | 0) * 1024 | 0) + (cu1 - 56320 | 0) | 0) + 65536 | 0;
    }
  }
  return cu0;
};
var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
var fromCharCode2 = (x) => singleton((() => {
  if (x >= -2147483648 && x <= 2147483647) {
    return fromCharCode(x);
  }
  if (x < 0) {
    return "\0";
  }
  return "\uFFFF";
})());
var singletonFallback = (v) => {
  if (v <= 65535) {
    return fromCharCode2(v);
  }
  return fromCharCode2(intDiv(v - 65536 | 0, 1024) + 55296 | 0) + fromCharCode2(intMod(v - 65536 | 0)(1024) + 56320 | 0);
};
var fromCodePointArray = /* @__PURE__ */ _fromCodePointArray(singletonFallback);
var singleton6 = /* @__PURE__ */ _singleton(singletonFallback);
var takeFallback = (v) => (v1) => {
  if (v < 1) {
    return "";
  }
  const v2 = uncons5(v1);
  if (v2.tag === "Just") {
    return singleton6(v2._1.head) + takeFallback(v - 1 | 0)(v2._1.tail);
  }
  return v1;
};
var take3 = /* @__PURE__ */ _take(takeFallback);
var codePointAtFallback = (codePointAtFallback$a0$copy) => (codePointAtFallback$a1$copy) => {
  let codePointAtFallback$a0 = codePointAtFallback$a0$copy, codePointAtFallback$a1 = codePointAtFallback$a1$copy, codePointAtFallback$c = true, codePointAtFallback$r;
  while (codePointAtFallback$c) {
    const n = codePointAtFallback$a0, s = codePointAtFallback$a1;
    const v = uncons5(s);
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
  }
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

// output-es/Util.Pretty/index.js
var intercalate3 = (sep) => (xs) => foldlArray((v) => (v1) => {
  if (v.init) {
    return { init: false, acc: v1 };
  }
  return { init: false, acc: v.acc + sep + v1 };
})({ init: true, acc: "" })(xs).acc;
var max2 = (x) => (y) => {
  const v = ordInt.compare(x)(y);
  if (v === "LT") {
    return y;
  }
  if (v === "EQ") {
    return x;
  }
  if (v === "GT") {
    return x;
  }
  fail();
};
var lastLine = (v) => {
  const $0 = index(v.lines)(v.lines.length - 1 | 0);
  if ($0.tag === "Just") {
    return $0._1;
  }
  if ($0.tag === "Nothing") {
    return "";
  }
  fail();
};
var firstLine = (v) => {
  const $0 = index(v.lines)(0);
  if ($0.tag === "Just") {
    return $0._1;
  }
  if ($0.tag === "Nothing") {
    return "";
  }
  fail();
};
var empty2 = { width: 0, height: 1, lines: [""] };
var checkOneLine = (xs) => {
  const v = uncons2(xs);
  if (v.tag === "Just") {
    if (v._1.tail.length === 0) {
      return { width: toCodePointArray(v._1.head).length, height: 1, lines: [v._1.head] };
    }
    return throwException(error("absurd"))();
  }
  if (v.tag === "Nothing") {
    return throwException(error("absurd"))();
  }
  fail();
};
var text = (s) => checkOneLine(split("\n")(" " + s));
var atop = (v) => (v1) => ({ width: max2(v.width)(v1.width), height: v.height + v1.height | 0, lines: [...v.lines, ...v1.lines] });
var allButLast = (v) => {
  const $0 = v.lines.length - 1 | 0;
  if ($0 < 1) {
    return [];
  }
  return slice2(0)($0)(v.lines);
};
var indentedExpression = (v) => (v1) => zipWith2(concatString)(replicate2(slice2(1)(v1.lines.length)(v1.lines).length)(foldlArray(concatString)("")(replicate2(toCodePointArray(lastLine(v)).length)(" "))))(slice2(1)(v1.lines.length)(v1.lines));
var beside = (v) => (v1) => ({ width: v.width + v1.width | 0, height: v.height + v1.height | 0, lines: [...allButLast(v), lastLine(v) + "" + firstLine(v1), ...indentedExpression(v)(v1)] });
var semigroupColumns = { append: (v) => (v1) => beside(v)(v1) };
var monoidColumns = { mempty: empty2, Semigroup0: () => semigroupColumns };

// output-es/Val/index.js
var $BaseVal = (tag, _1, _2) => ({ tag, _1, _2 });
var $EnvExpr = (_1, _2) => ({ tag: "EnvExpr", _1, _2 });
var $ForeignOp$p = (_1) => ({ tag: "ForeignOp'", _1 });
var $Fun = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
var $Val = (_1, _2) => ({ tag: "Val", _1, _2 });
var setSet3 = /* @__PURE__ */ setSet(ordDVertex);
var unions = /* @__PURE__ */ foldlArray(/* @__PURE__ */ union(ordDVertex))(Leaf2);
var vertices = /* @__PURE__ */ (() => verticesDict(verticesElimVertex).vertices)();
var unions13 = /* @__PURE__ */ (() => {
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
        go$a0 = unionWith(ordDVertex)($$const)(b)(v._1);
        go$a1 = v._2;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go(Leaf2);
})();
var foldMap2 = /* @__PURE__ */ foldMap({ mempty: Leaf2, Semigroup0: () => ({ append: union(ordDVertex) }) });
var identity20 = (x) => x;
var ordTuple2 = /* @__PURE__ */ ordTuple(ordInt);
var boundedLattice = { BoundedJoinSemilattice0: () => boundedJoinSemilatticeUni, BoundedMeetSemilattice1: () => boundedMeetSemilatticeUni };
var fromFoldable8 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)()(m))(Leaf2))();
var setSet1 = /* @__PURE__ */ setSet(ordString);
var toUnfoldable13 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
var intersection2 = /* @__PURE__ */ intersection(ordString);
var Val = (value0) => (value1) => $Val(value0, value1);
var Int = (value0) => $BaseVal("Int", value0);
var Float = (value0) => $BaseVal("Float", value0);
var Str = (value0) => $BaseVal("Str", value0);
var Dictionary2 = (value0) => $BaseVal("Dictionary", value0);
var DictRep = (x) => x;
var MatrixRep = (x) => x;
var Env = (x) => x;
var typeNameMatrixDim = { typeName: (v) => "MatrixDim" };
var pack2 = (x) => (k) => k(typeNameMatrixDim)(x);
var typeNameDictKey = { typeName: (v) => "DictKey" };
var pack12 = (x) => (k) => k(typeNameDictKey)(x);
var typeNameBaseVal = { typeName: (v) => "BaseVal" };
var pack22 = (x) => (k) => k(typeNameBaseVal)(x);
var verticesValVertex = {
  vertices: (v) => setSet3.union($$$Map("Two", Leaf2, $Tuple(v._1, pack22(v._2)), void 0, Leaf2))(verticesBaseValVertex.vertices(v._2))
};
var verticesMatrixRepVertex = {
  vertices: (v) => setSet3.union(unions(concat(arrayMap(arrayMap(verticesValVertex.vertices))(v._1))))(setSet3.union($$$Map(
    "Two",
    Leaf2,
    $Tuple(v._2._1._2, pack2(v._2._1)),
    void 0,
    Leaf2
  ))($$$Map("Two", Leaf2, $Tuple(v._2._2._2, pack2(v._2._2)), void 0, Leaf2)))
};
var verticesFunVertex = {
  vertices: (v) => {
    if (v.tag === "Closure") {
      return setSet3.union(verticesEnvVertex.vertices(v._1))(setSet3.union(vertices(v._2))(verticesElimVertex.vertices(v._3)));
    }
    if (v.tag === "Foreign") {
      return unions13(listMap(verticesValVertex.vertices)(v._2));
    }
    if (v.tag === "PartialConstr") {
      return unions13(listMap(verticesValVertex.vertices)(v._2));
    }
    fail();
  }
};
var verticesEnvVertex = { vertices: (v) => unions13(listMap(verticesValVertex.vertices)(mapObjectString.values(v))) };
var verticesDictRepVertex = {
  vertices: (v) => foldMap2((k) => (v1) => setSet3.union($$$Map(
    "Two",
    Leaf2,
    $Tuple(v1._1, pack12($Tuple(k, v1._1))),
    void 0,
    Leaf2
  ))(verticesValVertex.vertices(v1._2)))(v)
};
var verticesBaseValVertex = {
  vertices: (v) => {
    if (v.tag === "Int") {
      return setSet3.empty;
    }
    if (v.tag === "Float") {
      return setSet3.empty;
    }
    if (v.tag === "Str") {
      return setSet3.empty;
    }
    if (v.tag === "Constr") {
      return unions13(listMap(verticesValVertex.vertices)(v._2));
    }
    if (v.tag === "Dictionary") {
      return verticesDictRepVertex.vertices(v._1);
    }
    if (v.tag === "Matrix") {
      return verticesMatrixRepVertex.vertices(v._1);
    }
    if (v.tag === "Fun") {
      return verticesFunVertex.vertices(v._1);
    }
    fail();
  }
};
var verticesEnvExprVertex = {
  vertices: (v) => setSet3.union(unions13(listMap(verticesValVertex.vertices)(mapObjectString.values(v._1))))(verticesExprVertex.vertices(v._2))
};
var joinSemilatticeMatrixDim = (dictJoinSemilattice) => ({ join: (v) => (v1) => $Tuple(mustEq(eqInt)(showInt)(v._1)(v1._1), dictJoinSemilattice.join(v._2)(v1._2)) });
var isEmptyEnv = { isEmpty: (v) => isEmpty2(v) };
var setEnvString = {
  empty,
  filter: (p) => (v) => filterWithKey2((x) => {
    const $0 = p(x);
    return (v$1) => $0;
  })(v),
  size: (v) => size3(v),
  member: (x) => (v) => Object.hasOwn(v, x),
  difference: (v) => (v1) => setObjectString.difference(v)(v1),
  union: (v) => (v1) => union2(v)(v1),
  IsEmpty0: () => isEmptyEnv
};
var mapEnvStringVal = {
  maplet: (k) => (v) => {
    const $0 = {};
    $0[k] = v;
    return $0;
  },
  keys: (v) => mapObjectString.keys(v),
  values: (v) => mapObjectString.values(v),
  filterKeys: (p) => (v) => filterWithKey2((x) => {
    const $0 = p(x);
    return (v$1) => $0;
  })(v),
  unionWith: (f) => (v) => (v1) => unionWith2(f)(v)(v1),
  lookup: (k) => (v) => _lookup(Nothing, Just, k, v),
  delete: (k) => (v) => mutate(($0) => () => {
    delete $0[k];
    return $0;
  })(v),
  insert: (k) => (v) => (v1) => mutate(($0) => () => {
    $0[k] = v;
    return $0;
  })(v1),
  toUnfoldable: (dictUnfoldable) => toAscUnfoldable(dictUnfoldable),
  Set0: () => setEnvString
};
var highlightableVertex = {
  highlightIf: (v) => (doc) => beside(beside(doc)(checkOneLine(split("\n")(" _"))))(checkOneLine(split("\n")(" \u27E8" + v + "\u27E9")))
};
var highlightableUnit = { highlightIf: (v) => identity20 };
var functorMatrixDim = { map: (f) => (m) => $Tuple(m._1, f(m._2)) };
var functorVal = { map: (f) => (m) => $Val(f(m._1), functorBaseVal.map(f)(m._2)) };
var functorMatrixRep = {
  map: (f) => (m) => $Tuple(
    arrayMap(arrayMap(functorVal.map(f)))(m._1),
    $Tuple($Tuple(m._2._1._1, f(m._2._1._2)), $Tuple(m._2._2._1, f(m._2._2._2)))
  )
};
var functorFun = {
  map: (f) => (m) => {
    if (m.tag === "Closure") {
      return $Fun("Closure", functorEnv.map(f)(m._1), _fmapObject(m._2, functorElim.map(f)), functorElim.map(f)(m._3));
    }
    if (m.tag === "Foreign") {
      return $Fun("Foreign", m._1, listMap(functorVal.map(f))(m._2));
    }
    if (m.tag === "PartialConstr") {
      return $Fun("PartialConstr", m._1, listMap(functorVal.map(f))(m._2));
    }
    fail();
  }
};
var functorEnv = { map: (f) => (m) => _fmapObject(m, functorVal.map(f)) };
var functorDictRep = { map: (f) => (m) => _fmapObject(m, (v) => $Tuple(f(v._1), $Val(f(v._2._1), functorBaseVal.map(f)(v._2._2)))) };
var functorBaseVal = {
  map: (f) => (m) => {
    if (m.tag === "Int") {
      return $BaseVal("Int", m._1);
    }
    if (m.tag === "Float") {
      return $BaseVal("Float", m._1);
    }
    if (m.tag === "Str") {
      return $BaseVal("Str", m._1);
    }
    if (m.tag === "Constr") {
      return $BaseVal("Constr", m._1, listMap(functorVal.map(f))(m._2));
    }
    if (m.tag === "Dictionary") {
      return $BaseVal("Dictionary", functorDictRep.map(f)(m._1));
    }
    if (m.tag === "Matrix") {
      return $BaseVal("Matrix", functorMatrixRep.map(f)(m._1));
    }
    if (m.tag === "Fun") {
      return $BaseVal("Fun", functorFun.map(f)(m._1));
    }
    fail();
  }
};
var botOfUnit$x215Raw$x215 = (dictBoundedJoinSemilattice) => ({
  botOf: (() => {
    const $0 = dictBoundedJoinSemilattice.bot;
    const $1 = functorVal.map((() => {
      const $12 = dictBoundedJoinSemilattice.bot;
      return (v) => $12;
    })());
    return (x) => $Tuple($0, $1(x._2));
  })()
});
var foldableMatrixDim = { foldl: (f) => (z) => (m) => f(z)(m._2), foldr: (f) => (z) => (m) => f(m._2)(z), foldMap: (dictMonoid) => (f) => (m) => f(m._2) };
var traversableMatrixDim = {
  traverse: (dictApplicative) => (f) => (m) => dictApplicative.Apply0().Functor0().map((v1) => v1)(traversableTuple.traverse(dictApplicative)(f)(m)),
  sequence: (dictApplicative) => (v) => traversableMatrixDim.traverse(dictApplicative)(identity20)(v),
  Functor0: () => functorMatrixDim,
  Foldable1: () => foldableMatrixDim
};
var foldableVal = {
  foldl: (f) => (z) => (m) => foldableBaseVal.foldl(f)(f(z)(m._1))(m._2),
  foldr: (f) => (z) => (m) => f(m._1)(foldableBaseVal.foldr(f)(z)(m._2)),
  foldMap: (dictMonoid) => (f) => (m) => dictMonoid.Semigroup0().append(f(m._1))(foldableBaseVal.foldMap(dictMonoid)(f)(m._2))
};
var foldableMatrixRep = {
  foldl: (f) => (acc) => (v) => foldlArray(foldlArray(foldableVal.foldl(f)))(f(f(acc)(v._2._1._2))(v._2._2._2))(v._1),
  foldr: (f) => foldrDefault(foldableMatrixRep)(f),
  foldMap: (dictMonoid) => (f) => foldableMatrixRep.foldl((acc) => (x) => dictMonoid.Semigroup0().append(acc)(f(x)))(dictMonoid.mempty)
};
var foldableFun = {
  foldl: (f) => (z) => (m) => {
    if (m.tag === "Closure") {
      return foldableElim.foldl(f)(fold((z$1) => (v) => foldableElim.foldl(f)(z$1))(foldableEnv.foldl(f)(z)(m._1))(m._2))(m._3);
    }
    if (m.tag === "Foreign") {
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
            go$a0 = foldableBaseVal.foldl(f)(f(b)(v._1._1))(v._1._2);
            go$a1 = v._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      return go(z)(m._2);
    }
    if (m.tag === "PartialConstr") {
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
            go$a0 = foldableBaseVal.foldl(f)(f(b)(v._1._1))(v._1._2);
            go$a1 = v._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      return go(z)(m._2);
    }
    fail();
  },
  foldr: (f) => (z) => (m) => {
    if (m.tag === "Closure") {
      return foldableEnv.foldr(f)(foldrArray((b) => (a) => foldableElim.foldr(f)(a)(b))(foldableElim.foldr(f)(z)(m._3))(values(m._2)))(m._1);
    }
    if (m.tag === "Foreign") {
      return foldableList.foldr((b) => (a) => f(b._1)(foldableBaseVal.foldr(f)(a)(b._2)))(z)(m._2);
    }
    if (m.tag === "PartialConstr") {
      return foldableList.foldr((b) => (a) => f(b._1)(foldableBaseVal.foldr(f)(a)(b._2)))(z)(m._2);
    }
    fail();
  },
  foldMap: (dictMonoid) => {
    const $0 = dictMonoid.Semigroup0();
    const foldMap1 = foldMap(dictMonoid);
    const foldMap8 = foldableElim.foldMap(dictMonoid);
    const foldMap9 = foldableList.foldMap(dictMonoid);
    return (f) => (m) => {
      if (m.tag === "Closure") {
        return $0.append(foldableEnv.foldMap(dictMonoid)(f)(m._1))($0.append((() => {
          const $1 = foldMap8(f);
          return foldMap1((v) => $1)(m._2);
        })())(foldMap8(f)(m._3)));
      }
      if (m.tag === "Foreign") {
        return foldMap9(foldableVal.foldMap(dictMonoid)(f))(m._2);
      }
      if (m.tag === "PartialConstr") {
        return foldMap9(foldableVal.foldMap(dictMonoid)(f))(m._2);
      }
      fail();
    };
  }
};
var foldableEnv = {
  foldl: (f) => (z) => (m) => fold((z$1) => (v) => foldableVal.foldl(f)(z$1))(z)(m),
  foldr: (f) => (z) => (m) => foldrArray((b) => (a) => f(b._1)(foldableBaseVal.foldr(f)(a)(b._2)))(z)(values(m)),
  foldMap: (dictMonoid) => {
    const foldMap1 = foldMap(dictMonoid);
    return (f) => (m) => foldMap1((v) => foldableVal.foldMap(dictMonoid)(f))(m);
  }
};
var foldableDictRep = {
  foldl: (f) => (acc) => (v) => fold((z) => (v$1) => (v1) => foldableBaseVal.foldl(f)(f(f(z)(v1._1))(v1._2._1))(v1._2._2))(acc)(v),
  foldr: (f) => foldrDefault(foldableDictRep)(f),
  foldMap: (dictMonoid) => (f) => foldableDictRep.foldl((acc) => (x) => dictMonoid.Semigroup0().append(acc)(f(x)))(dictMonoid.mempty)
};
var foldableBaseVal = {
  foldl: (f) => (z) => (m) => {
    if (m.tag === "Int") {
      return z;
    }
    if (m.tag === "Float") {
      return z;
    }
    if (m.tag === "Str") {
      return z;
    }
    if (m.tag === "Constr") {
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
            go$a0 = foldableBaseVal.foldl(f)(f(b)(v._1._1))(v._1._2);
            go$a1 = v._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      return go(z)(m._2);
    }
    if (m.tag === "Dictionary") {
      return foldableDictRep.foldl(f)(z)(m._1);
    }
    if (m.tag === "Matrix") {
      return foldableMatrixRep.foldl(f)(z)(m._1);
    }
    if (m.tag === "Fun") {
      return foldableFun.foldl(f)(z)(m._1);
    }
    fail();
  },
  foldr: (f) => (z) => (m) => {
    if (m.tag === "Int") {
      return z;
    }
    if (m.tag === "Float") {
      return z;
    }
    if (m.tag === "Str") {
      return z;
    }
    if (m.tag === "Constr") {
      return foldableList.foldr((b) => (a) => f(b._1)(foldableBaseVal.foldr(f)(a)(b._2)))(z)(m._2);
    }
    if (m.tag === "Dictionary") {
      return foldrDefault(foldableDictRep)(f)(z)(m._1);
    }
    if (m.tag === "Matrix") {
      return foldrDefault(foldableMatrixRep)(f)(z)(m._1);
    }
    if (m.tag === "Fun") {
      return foldableFun.foldr(f)(z)(m._1);
    }
    fail();
  },
  foldMap: (dictMonoid) => {
    const mempty = dictMonoid.mempty;
    const foldMap7 = foldableList.foldMap(dictMonoid);
    return (f) => (m) => {
      if (m.tag === "Int") {
        return mempty;
      }
      if (m.tag === "Float") {
        return mempty;
      }
      if (m.tag === "Str") {
        return mempty;
      }
      if (m.tag === "Constr") {
        return foldMap7(foldableVal.foldMap(dictMonoid)(f))(m._2);
      }
      if (m.tag === "Dictionary") {
        return foldableDictRep.foldMap(dictMonoid)(f)(m._1);
      }
      if (m.tag === "Matrix") {
        return foldableMatrixRep.foldMap(dictMonoid)(f)(m._1);
      }
      if (m.tag === "Fun") {
        return foldableFun.foldMap(dictMonoid)(f)(m._1);
      }
      fail();
    };
  }
};
var traversableVal = {
  traverse: (dictApplicative) => {
    const Apply0 = dictApplicative.Apply0();
    return (f) => (m) => Apply0.apply(Apply0.Functor0().map((v2) => (v3) => $Val(v2, v3))(f(m._1)))(traversableBaseVal.traverse(dictApplicative)(f)(m._2));
  },
  sequence: (dictApplicative) => (v) => traversableVal.traverse(dictApplicative)(identity20)(v),
  Functor0: () => functorVal,
  Foldable1: () => foldableVal
};
var traversableMatrixRep = {
  traverse: (dictApplicative) => {
    const bitraverse1 = bitraversableTuple.bitraverse(dictApplicative);
    const traverse8 = traversableArray.traverse(dictApplicative);
    return (f) => (v) => dictApplicative.Apply0().Functor0().map(MatrixRep)(bitraverse1(traverse8(traverse8(traversableVal.traverse(dictApplicative)(f))))(bitraverse1(traversableMatrixDim.traverse(dictApplicative)(f))(traversableMatrixDim.traverse(dictApplicative)(f)))(v));
  },
  sequence: (dictApplicative) => traversableMatrixRep.traverse(dictApplicative)(identity7),
  Functor0: () => functorMatrixRep,
  Foldable1: () => foldableMatrixRep
};
var traversableFun = {
  traverse: (dictApplicative) => {
    const Apply0 = dictApplicative.Apply0();
    const $0 = Apply0.Functor0();
    const traverse8 = traversableDict.traverse(dictApplicative);
    const traverse9 = traversableElim.traverse(dictApplicative);
    const traverse10 = traversableList.traverse(dictApplicative);
    return (f) => (m) => {
      if (m.tag === "Closure") {
        return Apply0.apply(Apply0.apply($0.map((v3) => (v4) => (v5) => $Fun("Closure", v3, v4, v5))(traversableEnv.traverse(dictApplicative)(f)(m._1)))(traverse8(traverse9(f))(m._2)))(traverse9(f)(m._3));
      }
      if (m.tag === "Foreign") {
        const $1 = m._1;
        return $0.map((v2) => $Fun("Foreign", $1, v2))(traverse10(traversableVal.traverse(dictApplicative)(f))(m._2));
      }
      if (m.tag === "PartialConstr") {
        const $1 = m._1;
        return $0.map((v2) => $Fun("PartialConstr", $1, v2))(traverse10(traversableVal.traverse(dictApplicative)(f))(m._2));
      }
      fail();
    };
  },
  sequence: (dictApplicative) => (v) => traversableFun.traverse(dictApplicative)(identity20)(v),
  Functor0: () => functorFun,
  Foldable1: () => foldableFun
};
var traversableEnv = {
  traverse: (dictApplicative) => {
    const traverse8 = traversableDict.traverse(dictApplicative);
    return (f) => (m) => dictApplicative.Apply0().Functor0().map((v1) => v1)(traverse8(traversableVal.traverse(dictApplicative)(f))(m));
  },
  sequence: (dictApplicative) => (v) => traversableEnv.traverse(dictApplicative)(identity20)(v),
  Functor0: () => functorEnv,
  Foldable1: () => foldableEnv
};
var traversableDictRep = {
  traverse: (dictApplicative) => {
    const traverse8 = traversableDict.traverse(dictApplicative);
    const bitraverse1 = bitraversableTuple.bitraverse(dictApplicative);
    return (f) => (v) => dictApplicative.Apply0().Functor0().map(DictRep)(traverse8(bitraverse1(f)(traversableVal.traverse(dictApplicative)(f)))(v));
  },
  sequence: (dictApplicative) => traversableDictRep.traverse(dictApplicative)(identity7),
  Functor0: () => functorDictRep,
  Foldable1: () => foldableDictRep
};
var traversableBaseVal = {
  traverse: (dictApplicative) => {
    const $0 = dictApplicative.Apply0().Functor0();
    const traverse8 = traversableList.traverse(dictApplicative);
    return (f) => (m) => {
      if (m.tag === "Int") {
        return dictApplicative.pure($BaseVal("Int", m._1));
      }
      if (m.tag === "Float") {
        return dictApplicative.pure($BaseVal("Float", m._1));
      }
      if (m.tag === "Str") {
        return dictApplicative.pure($BaseVal("Str", m._1));
      }
      if (m.tag === "Constr") {
        const $1 = m._1;
        return $0.map((v2) => $BaseVal("Constr", $1, v2))(traverse8(traversableVal.traverse(dictApplicative)(f))(m._2));
      }
      if (m.tag === "Dictionary") {
        return $0.map((v1) => $BaseVal("Dictionary", v1))(traversableDictRep.traverse(dictApplicative)(f)(m._1));
      }
      if (m.tag === "Matrix") {
        return $0.map((v1) => $BaseVal("Matrix", v1))(traversableMatrixRep.traverse(dictApplicative)(f)(m._1));
      }
      if (m.tag === "Fun") {
        return $0.map((v1) => $BaseVal("Fun", v1))(traversableFun.traverse(dictApplicative)(f)(m._1));
      }
      fail();
    };
  },
  sequence: (dictApplicative) => (v) => traversableBaseVal.traverse(dictApplicative)(identity20)(v),
  Functor0: () => functorBaseVal,
  Foldable1: () => foldableBaseVal
};
var expandableValRawVal = (dictBoundedJoinSemilattice) => ({ expand: (v) => (v1) => $Val(v._1, expandableBaseValRawBaseV(dictBoundedJoinSemilattice).expand(v._2)(v1._2)) });
var expandableMatrixRepRawMat = (dictBoundedJoinSemilattice) => ({
  expand: (v) => (v1) => $Tuple(
    (() => {
      const expand1 = expandableValRawVal(dictBoundedJoinSemilattice).expand;
      return zipWith2((xs) => zipWith2(expand1)(xs))(v._1)(v1._1);
    })(),
    $Tuple(
      $Tuple(mustEq(eqInt)(showInt)(v._2._1._1)(v1._2._1._1), v._2._1._2),
      $Tuple(mustEq(eqInt)(showInt)(v._2._2._1)(v1._2._2._1), v._2._2._2)
    )
  )
});
var expandableFunRawFun = (dictBoundedJoinSemilattice) => {
  const expandableElimRawElim2 = expandableElimRawElim(dictBoundedJoinSemilattice);
  return {
    expand: (v) => (v1) => {
      if (v.tag === "Closure") {
        if (v1.tag === "Closure") {
          return $Fun(
            "Closure",
            expandableEnvRawEnv(dictBoundedJoinSemilattice).expand(v._1)(v1._1),
            expandableDictDict({
              botOf: functorElim.map((() => {
                const $0 = dictBoundedJoinSemilattice.bot;
                return (v$1) => $0;
              })())
            })(expandableElimRawElim2).expand(v._2)(v1._2),
            expandableElimRawElim2.expand(v._3)(v1._3)
          );
        }
        return throwException(error("Shape mismatch"))();
      }
      if (v.tag === "Foreign") {
        if (v1.tag === "Foreign") {
          return $Fun("Foreign", v._1, zipWith(expandableValRawVal(dictBoundedJoinSemilattice).expand)(v._2)(v1._2));
        }
        return throwException(error("Shape mismatch"))();
      }
      if (v.tag === "PartialConstr" && v1.tag === "PartialConstr") {
        return $Fun(
          "PartialConstr",
          mustEq(eqString)(showString)(v._1)(v1._1),
          zipWith(expandableValRawVal(dictBoundedJoinSemilattice).expand)(v._2)(v1._2)
        );
      }
      return throwException(error("Shape mismatch"))();
    }
  };
};
var expandableEnvRawEnv = (dictBoundedJoinSemilattice) => {
  const expandableDictDict2 = expandableDictDict({
    botOf: functorVal.map((() => {
      const $0 = dictBoundedJoinSemilattice.bot;
      return (v) => $0;
    })())
  });
  return { expand: (v) => (v1) => expandableDictDict2(expandableValRawVal(dictBoundedJoinSemilattice)).expand(v)(v1) };
};
var expandableDictRepRawDictR = (dictBoundedJoinSemilattice) => {
  const expandableDictDict2 = expandableDictDict(botOfUnit$x215Raw$x215(dictBoundedJoinSemilattice));
  return {
    expand: (v) => (v1) => expandableDictDict2((() => {
      const $0 = expandableValRawVal(dictBoundedJoinSemilattice);
      return { expand: (v$1) => (v1$1) => $Tuple(v$1._1, $0.expand(v$1._2)(v1$1._2)) };
    })()).expand(v)(v1)
  };
};
var expandableBaseValRawBaseV = (dictBoundedJoinSemilattice) => ({
  expand: (v) => (v1) => {
    if (v.tag === "Int") {
      if (v1.tag === "Int") {
        return $BaseVal("Int", mustEq(eqInt)(showInt)(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Float") {
      if (v1.tag === "Float") {
        return $BaseVal("Float", mustEq(eqNumber)(showNumber)(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Str") {
      if (v1.tag === "Str") {
        return $BaseVal("Str", mustEq(eqString)(showString)(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Dictionary") {
      if (v1.tag === "Dictionary") {
        return $BaseVal("Dictionary", expandableDictRepRawDictR(dictBoundedJoinSemilattice).expand(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Constr") {
      if (v1.tag === "Constr") {
        return $BaseVal(
          "Constr",
          mustEq(eqString)(showString)(v._1)(v1._1),
          zipWith(expandableValRawVal(dictBoundedJoinSemilattice).expand)(v._2)(v1._2)
        );
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Matrix") {
      if (v1.tag === "Matrix") {
        return $BaseVal("Matrix", expandableMatrixRepRawMat(dictBoundedJoinSemilattice).expand(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Fun" && v1.tag === "Fun") {
      return $BaseVal("Fun", expandableFunRawFun(dictBoundedJoinSemilattice).expand(v._1)(v1._1));
    }
    return throwException(error("Shape mismatch"))();
  }
});
var ordMatrixDim = (dictOrd) => {
  const $0 = dictOrd.Eq0();
  const eqMatrixDim1 = { eq: (x) => (y) => x._1 === y._1 && $0.eq(x._2)(y._2) };
  return { compare: (x) => (y) => ordTuple2(dictOrd).compare(x)(y), Eq0: () => eqMatrixDim1 };
};
var eqVal = (dictEq) => ({ eq: (x) => (y) => dictEq.eq(x._1)(y._1) && eqBaseVal(dictEq).eq(x._2)(y._2) });
var eqMatrixRep = (dictEq) => ({
  eq: (x) => (y) => eqArrayImpl(eqArrayImpl(eqVal(dictEq).eq))(x._1)(y._1) && x._2._1._1 === y._2._1._1 && dictEq.eq(x._2._1._2)(y._2._1._2) && x._2._2._1 === y._2._2._1 && dictEq.eq(x._2._2._2)(y._2._2._2)
});
var eqFun = (dictEq) => {
  const eqElim2 = eqElim(dictEq);
  return {
    eq: (x) => (y) => {
      if (x.tag === "Closure") {
        return y.tag === "Closure" && eqEnv(dictEq).eq(x._1)(y._1) && eqObject(eqElim2).eq(x._2)(y._2) && eqElim2.eq(x._3)(y._3);
      }
      if (x.tag === "Foreign") {
        return y.tag === "Foreign" && (() => {
          const $0 = eqVal(dictEq);
          return x._1._1 === y._1._1 && (() => {
            const go = (v) => (v1) => (v2) => {
              if (!v2) {
                return false;
              }
              if (v.tag === "Nil") {
                return v1.tag === "Nil" && v2;
              }
              return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
            };
            return go(x._2)(y._2)(true);
          })();
        })();
      }
      return x.tag === "PartialConstr" && y.tag === "PartialConstr" && (() => {
        const $0 = eqVal(dictEq);
        return x._1 === y._1 && (() => {
          const go = (v) => (v1) => (v2) => {
            if (!v2) {
              return false;
            }
            if (v.tag === "Nil") {
              return v1.tag === "Nil" && v2;
            }
            return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
          };
          return go(x._2)(y._2)(true);
        })();
      })();
    }
  };
};
var eqEnv = (dictEq) => ({ eq: (x) => (y) => eqObject(eqVal(dictEq)).eq(x)(y) });
var eqDictRep = (dictEq) => ({
  eq: (x) => (y) => {
    const $0 = eqVal(dictEq);
    return eqObject({ eq: (x$1) => (y$1) => dictEq.eq(x$1._1)(y$1._1) && $0.eq(x$1._2)(y$1._2) }).eq(x)(y);
  }
});
var eqBaseVal = (dictEq) => ({
  eq: (x) => (y) => {
    if (x.tag === "Int") {
      return y.tag === "Int" && x._1 === y._1;
    }
    if (x.tag === "Float") {
      return y.tag === "Float" && x._1 === y._1;
    }
    if (x.tag === "Str") {
      return y.tag === "Str" && x._1 === y._1;
    }
    if (x.tag === "Constr") {
      return y.tag === "Constr" && (() => {
        const $0 = eqVal(dictEq);
        return x._1 === y._1 && (() => {
          const go = (v) => (v1) => (v2) => {
            if (!v2) {
              return false;
            }
            if (v.tag === "Nil") {
              return v1.tag === "Nil" && v2;
            }
            return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
          };
          return go(x._2)(y._2)(true);
        })();
      })();
    }
    if (x.tag === "Dictionary") {
      return y.tag === "Dictionary" && eqDictRep(dictEq).eq(x._1)(y._1);
    }
    if (x.tag === "Matrix") {
      return y.tag === "Matrix" && eqMatrixRep(dictEq).eq(x._1)(y._1);
    }
    return x.tag === "Fun" && y.tag === "Fun" && eqFun(dictEq).eq(x._1)(y._1);
  }
});
var ordVal = (dictOrd) => {
  const eqVal1 = eqVal(dictOrd.Eq0());
  return {
    compare: (x) => (y) => {
      const v = dictOrd.compare(x._1)(y._1);
      if (v === "LT") {
        return LT;
      }
      if (v === "GT") {
        return GT;
      }
      return ordBaseVal(dictOrd).compare(x._2)(y._2);
    },
    Eq0: () => eqVal1
  };
};
var ordMatrixRep = (dictOrd) => {
  const ordMatrixDim1 = ordMatrixDim(dictOrd);
  const ordTuple1 = ordTuple(ordMatrixDim1)(ordMatrixDim1);
  const eqMatrixRep1 = eqMatrixRep(dictOrd.Eq0());
  return { compare: (x) => (y) => ordTuple(ordArray(ordArray(ordVal(dictOrd))))(ordTuple1).compare(x)(y), Eq0: () => eqMatrixRep1 };
};
var ordFun = (dictOrd) => {
  const ordElim2 = ordElim(dictOrd);
  const eqFun1 = eqFun(dictOrd.Eq0());
  return {
    compare: (x) => (y) => {
      if (x.tag === "Closure") {
        if (y.tag === "Closure") {
          const v = ordEnv(dictOrd).compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          const v1 = ordDict(ordElim2).compare(x._2)(y._2);
          if (v1 === "LT") {
            return LT;
          }
          if (v1 === "GT") {
            return GT;
          }
          return ordElim2.compare(x._3)(y._3);
        }
        return LT;
      }
      if (y.tag === "Closure") {
        return GT;
      }
      if (x.tag === "Foreign") {
        if (y.tag === "Foreign") {
          const v = ordString.compare(x._1._1)(y._1._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordList(ordVal(dictOrd)).compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "Foreign") {
        return GT;
      }
      if (x.tag === "PartialConstr" && y.tag === "PartialConstr") {
        const v = ordString.compare(x._1)(y._1);
        if (v === "LT") {
          return LT;
        }
        if (v === "GT") {
          return GT;
        }
        return ordList(ordVal(dictOrd)).compare(x._2)(y._2);
      }
      fail();
    },
    Eq0: () => eqFun1
  };
};
var ordEnv = (dictOrd) => {
  const $0 = dictOrd.Eq0();
  const eqEnv1 = { eq: (x) => (y) => eqObject(eqVal($0)).eq(x)(y) };
  return { compare: (x) => (y) => ordDict(ordVal(dictOrd)).compare(x)(y), Eq0: () => eqEnv1 };
};
var ordDictRep = (dictOrd) => {
  const ordTuple1 = ordTuple(dictOrd);
  const eqDictRep1 = eqDictRep(dictOrd.Eq0());
  return { compare: (x) => (y) => ordDict(ordTuple1(ordVal(dictOrd))).compare(x)(y), Eq0: () => eqDictRep1 };
};
var ordBaseVal = (dictOrd) => {
  const eqBaseVal1 = eqBaseVal(dictOrd.Eq0());
  return {
    compare: (x) => (y) => {
      if (x.tag === "Int") {
        if (y.tag === "Int") {
          return ordInt.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "Int") {
        return GT;
      }
      if (x.tag === "Float") {
        if (y.tag === "Float") {
          return ordNumber.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "Float") {
        return GT;
      }
      if (x.tag === "Str") {
        if (y.tag === "Str") {
          return ordString.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "Str") {
        return GT;
      }
      if (x.tag === "Constr") {
        if (y.tag === "Constr") {
          const v = ordString.compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordList(ordVal(dictOrd)).compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "Constr") {
        return GT;
      }
      if (x.tag === "Dictionary") {
        if (y.tag === "Dictionary") {
          return ordDictRep(dictOrd).compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "Dictionary") {
        return GT;
      }
      if (x.tag === "Matrix") {
        if (y.tag === "Matrix") {
          return ordMatrixRep(dictOrd).compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "Matrix") {
        return GT;
      }
      if (x.tag === "Fun" && y.tag === "Fun") {
        return ordFun(dictOrd).compare(x._1)(y._1);
      }
      fail();
    },
    Eq0: () => eqBaseVal1
  };
};
var applyMatrixDim = { apply: (v) => (v1) => $Tuple(mustEq(eqInt)(showInt)(v._1)(v1._1), v._2(v1._2)), Functor0: () => functorMatrixDim };
var applyVal = { apply: (v) => (v1) => $Val(v._1(v1._1), applyBaseVal.apply(v._2)(v1._2)), Functor0: () => functorVal };
var applyMatrixRep = {
  apply: (v) => (v1) => $Tuple(
    zipWith2(zipWith2(applyVal.apply))(v._1)(v1._1),
    $Tuple(applyMatrixDim.apply(v._2._1)(v1._2._1), applyMatrixDim.apply(v._2._2)(v1._2._2))
  ),
  Functor0: () => functorMatrixRep
};
var applyFun = {
  apply: (v) => (v1) => {
    if (v.tag === "Closure") {
      if (v1.tag === "Closure") {
        return $Fun(
          "Closure",
          applyEnv.apply(v._1)(v1._1),
          intersectionWith_Object(apply)(_fmapObject(v._2, applyElim.apply))(v1._2),
          applyElim.apply(v._3)(v1._3)
        );
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Foreign") {
      if (v1.tag === "Foreign") {
        return $Fun("Foreign", v._1, zipWith(applyVal.apply)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "PartialConstr" && v1.tag === "PartialConstr") {
      return $Fun("PartialConstr", mustEq(eqString)(showString)(v._1)(v1._1), zipWith(applyVal.apply)(v._2)(v1._2));
    }
    return throwException(error("Shape mismatch"))();
  },
  Functor0: () => functorFun
};
var applyEnv = { apply: (v) => (v1) => intersectionWith_Object(apply)(_fmapObject(v, applyVal.apply))(v1), Functor0: () => functorEnv };
var applyDictRep = {
  apply: (v) => (v1) => intersectionWith_Object((v2) => {
    const $0 = v2._2;
    return (v3) => $Tuple(v2._1(v3._1), applyVal.apply($0)(v3._2));
  })(v)(v1),
  Functor0: () => functorDictRep
};
var applyBaseVal = {
  apply: (v) => (v1) => {
    if (v.tag === "Int") {
      if (v1.tag === "Int") {
        return $BaseVal("Int", mustEq(eqInt)(showInt)(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Float") {
      if (v1.tag === "Float") {
        return $BaseVal("Float", mustEq(eqNumber)(showNumber)(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Str") {
      if (v1.tag === "Str") {
        return $BaseVal("Str", mustEq(eqString)(showString)(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Constr") {
      if (v1.tag === "Constr") {
        return $BaseVal("Constr", mustEq(eqString)(showString)(v._1)(v1._1), zipWith(applyVal.apply)(v._2)(v1._2));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Dictionary") {
      if (v1.tag === "Dictionary") {
        return $BaseVal("Dictionary", applyDictRep.apply(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Matrix") {
      if (v1.tag === "Matrix") {
        return $BaseVal("Matrix", applyMatrixRep.apply(v._1)(v1._1));
      }
      return throwException(error("Shape mismatch"))();
    }
    if (v.tag === "Fun" && v1.tag === "Fun") {
      return $BaseVal("Fun", applyFun.apply(v._1)(v1._1));
    }
    return throwException(error("Shape mismatch"))();
  },
  Functor0: () => functorBaseVal
};
var joinSemilatticeVal = (dictJoinSemilattice) => ({ join: (v) => (v1) => $Val(dictJoinSemilattice.join(v._1)(v1._1), joinSemilatticeBaseVal(dictJoinSemilattice).join(v._2)(v1._2)) });
var joinSemilatticeMatrixRep = (dictJoinSemilattice) => {
  const $0 = joinSemilatticeMatrixDim(dictJoinSemilattice);
  return {
    join: (v) => (v1) => $Tuple(
      joinSemilatticeArray(joinSemilatticeArray(joinSemilatticeVal(dictJoinSemilattice))).join(v._1)(v1._1),
      $Tuple($0.join(v._2._1)(v1._2._1), $0.join(v._2._2)(v1._2._2))
    )
  };
};
var joinSemilatticeFun = (dictJoinSemilattice) => {
  const joinSemilatticeElim2 = joinSemilatticeElim(dictJoinSemilattice);
  return {
    join: (v) => (v1) => {
      if (v.tag === "Closure") {
        if (v1.tag === "Closure") {
          return $Fun(
            "Closure",
            joinSemilatticeEnv(dictJoinSemilattice).join(v._1)(v1._1),
            unionWith2(joinSemilatticeElim2.join)(v._2)(v1._2),
            joinSemilatticeElim2.join(v._3)(v1._3)
          );
        }
        return throwException(error("Shape mismatch"))();
      }
      if (v.tag === "Foreign") {
        if (v1.tag === "Foreign") {
          return $Fun("Foreign", v._1, joinSemilatticeList(joinSemilatticeVal(dictJoinSemilattice)).join(v._2)(v1._2));
        }
        return throwException(error("Shape mismatch"))();
      }
      if (v.tag === "PartialConstr" && v1.tag === "PartialConstr") {
        return $Fun(
          "PartialConstr",
          mustEq(eqString)(showString)(v._1)(v1._1),
          joinSemilatticeList(joinSemilatticeVal(dictJoinSemilattice)).join(v._2)(v1._2)
        );
      }
      return throwException(error("Shape mismatch"))();
    }
  };
};
var joinSemilatticeEnv = (dictJoinSemilattice) => ({ join: (v) => (v1) => unionWith2(joinSemilatticeVal(dictJoinSemilattice).join)(v)(v1) });
var joinSemilatticeDictRep = (dictJoinSemilattice) => ({
  join: (v) => (v1) => {
    const $0 = joinSemilatticeVal(dictJoinSemilattice);
    return unionWith2((v$1) => (v1$1) => $Tuple(dictJoinSemilattice.join(v$1._1)(v1$1._1), $0.join(v$1._2)(v1$1._2)))(v)(v1);
  }
});
var joinSemilatticeBaseVal = (dictJoinSemilattice) => {
  const join = dictJoinSemilattice.join;
  return {
    join: (v) => (v1) => {
      if (v.tag === "Int") {
        if (v1.tag === "Int") {
          return $BaseVal("Int", mustEq(eqInt)(showInt)(v._1)(v1._1));
        }
        return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
      }
      if (v.tag === "Float") {
        if (v1.tag === "Float") {
          return $BaseVal("Float", mustEq(eqNumber)(showNumber)(v._1)(v1._1));
        }
        return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
      }
      if (v.tag === "Str") {
        if (v1.tag === "Str") {
          return $BaseVal("Str", mustEq(eqString)(showString)(v._1)(v1._1));
        }
        return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
      }
      if (v.tag === "Dictionary") {
        if (v1.tag === "Dictionary") {
          return $BaseVal("Dictionary", joinSemilatticeDictRep(dictJoinSemilattice).join(v._1)(v1._1));
        }
        return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
      }
      if (v.tag === "Constr") {
        if (v1.tag === "Constr") {
          return $BaseVal(
            "Constr",
            mustEq(eqString)(showString)(v._1)(v1._1),
            joinSemilatticeList(joinSemilatticeVal(dictJoinSemilattice)).join(v._2)(v1._2)
          );
        }
        return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
      }
      if (v.tag === "Matrix") {
        if (v1.tag === "Matrix") {
          return $BaseVal("Matrix", joinSemilatticeMatrixRep(dictJoinSemilattice).join(v._1)(v1._1));
        }
        return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
      }
      if (v.tag === "Fun" && v1.tag === "Fun") {
        return $BaseVal("Fun", joinSemilatticeFun(dictJoinSemilattice).join(v._1)(v1._1));
      }
      return applyBaseVal.apply(functorBaseVal.map(join)(v))(v1);
    }
  };
};
var annUnit = { Highlightable0: () => highlightableUnit, BoundedLattice1: () => boundedLattice };
var reaches = (\u03C1) => (xs) => {
  const dom_\u03C1 = fromFoldable8(mapObjectString.keys(\u03C1));
  const go = (go$a0$copy) => (go$a1$copy) => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = v1;
        continue;
      }
      if (v.tag === "Cons") {
        if (setSet1.member(v._1)(v1)) {
          go$a0 = v._2;
          go$a1 = v1;
          continue;
        }
        go$a0 = foldableList.foldr(Cons)(v._2)(toUnfoldable13(intersection2(fVElim.fv($$get(showString)(mapDictString)(v._1)(\u03C1)))(dom_\u03C1)));
        go$a1 = setSet1.union($$$Map("Two", Leaf2, v._1, void 0, Leaf2))(v1);
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go(toUnfoldable13(xs))(setSet1.empty);
};
var matrixPut = (i) => (j) => (\u03B4v) => (v) => {
  const vs_i = definitely("index within bounds")(index(v._1)(i - 1 | 0));
  return $Tuple(
    unsafeUpdateAt(i - 1 | 0)(unsafeUpdateAt(j - 1 | 0)(\u03B4v(definitely("index within bounds")(index(vs_i)(j - 1 | 0))))(vs_i))(v._1),
    $Tuple(v._2._1, v._2._2)
  );
};
var matrixGet = (i) => (j) => (v) => definitely("index out of bounds!")((() => {
  const $0 = index(v._1)(i - 1 | 0);
  if ($0.tag === "Just") {
    return index($0._1)(j - 1 | 0);
  }
  if ($0.tag === "Nothing") {
    return Nothing;
  }
  fail();
})());
var forDefs = (\u03C1) => (\u03C3) => {
  const $0 = reaches(\u03C1)(intersection2(fVElim.fv(\u03C3))(fromFoldable8(mapObjectString.keys(\u03C1))));
  return filterWithKey2((x) => {
    const $1 = setSet(ordString).member(x)($0);
    return (v) => $1;
  })(\u03C1);
};
var asVal = (e) => {
  const type$p = e((dictTypeName) => dictTypeName.typeName);
  if (type$p === "BaseVal") {
    return $Either("Left", e((dictTypeName) => unsafeCoerce));
  }
  return $Either("Right", type$p);
};

// output-es/Graph.Slice/index.js
var pure = /* @__PURE__ */ (() => applicativeStateT(monadIdentity).pure)();
var extend = /* @__PURE__ */ (() => monadWithGraphWithGraphT(monadIdentity).extend)();
var tailRecM = /* @__PURE__ */ (() => monadRecStateT(monadRecIdentity).tailRecM)();
var member3 = /* @__PURE__ */ (() => setSet(ordVertex).member)();
var fromFoldable9 = /* @__PURE__ */ (() => foldableSet.foldr(Cons)(Nil))();
var intersection3 = /* @__PURE__ */ intersection(ordVertex);
var fwdSlice = (dictGraph) => {
  const runWithGraph_spy = runWithGraphT_spy1(dictGraph);
  return (v) => {
    const $0 = v._2;
    return runWithGraph_spy(tailRecM((v1) => {
      if (v1.es.tag === "Nil") {
        return pure($Step("Done", void 0));
      }
      if (v1.es.tag === "Cons") {
        const $1 = lookup(ordVertex)(v1.es._1._1)(v1.pending);
        const \u03B2s = (() => {
          if ($1.tag === "Nothing") {
            return $$$Map("Two", Leaf2, v1.es._1._2, void 0, Leaf2);
          }
          if ($1.tag === "Just") {
            return insert(ordVertex)(v1.es._1._2)()($1._1);
          }
          fail();
        })();
        if (eqMap(eqVertex)(eqUnit).eq(\u03B2s)(dictGraph.outN($0)(v1.es._1._1))) {
          return bindStateT(monadIdentity).bind(extend($Tuple(v1.es._1._1, dictGraph.vertexData($0)(v1.es._1._1)))(\u03B2s))(() => pure($Step(
            "Loop",
            {
              pending: $$delete(ordVertex)(v1.es._1._1)(v1.pending),
              es: foldableList.foldr(Cons)(v1.es._2)(inEdges$p(dictGraph)($0)(v1.es._1._1))
            }
          )));
        }
        return pure($Step("Loop", { pending: insert(ordVertex)(v1.es._1._1)(\u03B2s)(v1.pending), es: v1.es._2 }));
      }
      fail();
    })({ pending: Leaf2, es: inEdges(dictGraph)($0)(v._1) }))((() => {
      const $1 = map(ordDVertex)((\u03B1) => $Tuple(\u03B1, dictGraph.vertexData($0)(\u03B1)))(v._1);
      return assertWhen(true)("inputs are sinks")((v$1) => difference2(ordVertex)(addresses($1))(dictGraph.sinks($0)).tag === "Leaf")($1);
    })())._1;
  };
};
var bwdSlice = (dictGraph) => {
  const runWithGraph_spy = runWithGraphT_spy1(dictGraph);
  return (v) => {
    const $0 = v._2;
    const $1 = map(ordDVertex)((\u03B1) => $Tuple(
      \u03B1,
      spyWhen(true)("Value found at " + showStringImpl(\u03B1))((x) => {
        const $12 = asVal(x);
        if ($12.tag === "Left") {
          if ($12._1.tag === "Int") {
            return "BV: Int";
          }
          if ($12._1.tag === "Float") {
            return "BV: Float";
          }
          if ($12._1.tag === "Str") {
            return "BV: Str";
          }
          if ($12._1.tag === "Constr") {
            return "BV: Constr";
          }
          if ($12._1.tag === "Dictionary") {
            return "BV: Dictionary";
          }
          if ($12._1.tag === "Matrix") {
            return "BV: Matrix";
          }
          if ($12._1.tag === "Fun") {
            return "BV: Fun";
          }
          fail();
        }
        if ($12.tag === "Right") {
          return $12._1;
        }
        fail();
      })(dictGraph.vertexData($0)(\u03B1))
    ))(v._1);
    return runWithGraph_spy(tailRecM((v1) => {
      if (v1["\u03B1s"].tag === "Nil") {
        if (v1.pending.tag === "Nil") {
          return pure($Step("Done", void 0));
        }
        if (v1.pending.tag === "Cons") {
          const $2 = v1.pending._1._1._2;
          const $3 = v1.pending._1._1._1;
          if (member3($3)(v1.visited)) {
            return pure($Step("Loop", { visited: v1.visited, "\u03B1s": Nil, pending: v1.pending._2 }));
          }
          return bindStateT(monadIdentity).bind(extend($Tuple(
            $3,
            spyWhen(true)("Value found at " + showStringImpl($3))((x) => {
              const $4 = asVal(x);
              if ($4.tag === "Left") {
                if ($4._1.tag === "Int") {
                  return "BV: Int";
                }
                if ($4._1.tag === "Float") {
                  return "BV: Float";
                }
                if ($4._1.tag === "Str") {
                  return "BV: Str";
                }
                if ($4._1.tag === "Constr") {
                  return "BV: Constr";
                }
                if ($4._1.tag === "Dictionary") {
                  return "BV: Dictionary";
                }
                if ($4._1.tag === "Matrix") {
                  return "BV: Matrix";
                }
                if ($4._1.tag === "Fun") {
                  return "BV: Fun";
                }
                fail();
              }
              if ($4.tag === "Right") {
                return $4._1;
              }
              fail();
            })($2)
          ))(v1.pending._1._2))(() => pure($Step(
            "Loop",
            { visited: insert(ordVertex)($3)()(v1.visited), "\u03B1s": Nil, pending: v1.pending._2 }
          )));
        }
        fail();
      }
      if (v1["\u03B1s"].tag === "Cons") {
        const \u03B2s = dictGraph.outN($0)(v1["\u03B1s"]._1);
        return pure($Step(
          "Loop",
          {
            visited: v1.visited,
            "\u03B1s": foldableList.foldr(Cons)(v1["\u03B1s"]._2)(fromFoldable9(\u03B2s)),
            pending: $List("Cons", $Tuple($Tuple(v1["\u03B1s"]._1, dictGraph.vertexData($0)(v1["\u03B1s"]._1)), \u03B2s), v1.pending)
          }
        ));
      }
      fail();
    })({
      visited: Leaf2,
      "\u03B1s": fromFoldable9(intersection3(addresses(assertWhen(true)("inputs are sinks")((v$1) => difference2(ordDVertex)($1)(dictGraph.Vertices1().vertices($0)).tag === "Leaf")($1)))(dictGraph.sources($0))),
      pending: Nil
    }))(Leaf2)._1;
  };
};

// output-es/Data.Profunctor.Choice/index.js
var identity21 = (x) => x;
var fanin = (dictCategory) => {
  const identity1 = dictCategory.identity;
  const $0 = dictCategory.Semigroupoid0();
  const $1 = dictCategory.Semigroupoid0();
  return (dictChoice) => (l) => (r) => $0.compose(dictChoice.Profunctor0().dimap((v2) => {
    if (v2.tag === "Left") {
      return v2._1;
    }
    if (v2.tag === "Right") {
      return v2._1;
    }
    fail();
  })(identity21)(identity1))($1.compose(dictChoice.right(r))(dictChoice.left(l)));
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

// output-es/Data.Show.Generic/foreign.js
var intercalate4 = function(separator) {
  return function(xs) {
    return xs.join(separator);
  };
};

// output-es/Data.Show.Generic/index.js
var genericShowArgsNoArguments = { genericShowArgs: (v) => [] };
var genericShowArgsProduct = (dictGenericShowArgs) => (dictGenericShowArgs1) => ({ genericShowArgs: (v) => [...dictGenericShowArgs.genericShowArgs(v._1), ...dictGenericShowArgs1.genericShowArgs(v._2)] });
var genericShowConstructor = (dictGenericShowArgs) => (dictIsSymbol) => ({
  "genericShow'": (v) => {
    const ctor = dictIsSymbol.reflectSymbol($$Proxy);
    const v1 = dictGenericShowArgs.genericShowArgs(v);
    if (v1.length === 0) {
      return ctor;
    }
    return "(" + intercalate4(" ")([ctor, ...v1]) + ")";
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
    return (state1, more, lift1, $$throw2, done) => force(m)(state1, more, lift1, $$throw2, done);
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
  return (x) => $0["genericShow'"](x);
})();
var functorParserT = { map: (f) => (v) => (state1, more, lift1, $$throw2, done) => more((v1) => v(state1, more, lift1, $$throw2, (state2, a) => more((v2) => done(state2, f(a))))) };
var applyParserT = {
  apply: (v) => (v1) => (state1, more, lift1, $$throw2, done) => more((v2) => v(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, f) => more((v3) => v1(state2, more, lift1, $$throw2, (state3, a) => more((v4) => done(state3, f(a)))))
  )),
  Functor0: () => functorParserT
};
var bindParserT = {
  bind: (v) => (next) => (state1, more, lift1, $$throw2, done) => more((v1) => v(state1, more, lift1, $$throw2, (state2, a) => more((v2) => next(a)(state2, more, lift1, $$throw2, done)))),
  Apply0: () => applyParserT
};
var applicativeParserT = { pure: (a) => (state1, v, v1, v2, done) => done(state1, a), Apply0: () => applyParserT };
var monadParserT = { Applicative0: () => applicativeParserT, Bind1: () => bindParserT };
var monadRecParserT = {
  tailRecM: (next) => (initArg) => (state1, more, lift1, $$throw2, done) => {
    const loop = (state2, arg, gas) => next(arg)(
      state2,
      more,
      lift1,
      $$throw2,
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
  alt: (v) => (v1) => (v2, $0, $1, $2, $3) => {
    const $4 = v2._1;
    const $5 = v2._2;
    return $0((v3) => v(
      $ParseState($4, $5, false),
      $0,
      $1,
      (v4, $6) => {
        const $7 = v4._3;
        return $0((v5) => {
          if ($7) {
            return $2(v4, $6);
          }
          return v1(v2, $0, $1, $2, $3);
        });
      },
      $3
    ));
  },
  Functor0: () => functorParserT
};
var showParseError = { show: (v) => "(ParseError " + showStringImpl(v._1) + " " + genericShow(v._2) + ")" };
var runParserT$p = (dictMonadRec) => {
  const Monad0 = dictMonadRec.Monad0();
  return (state1) => (v) => {
    const go = (go$a0$copy) => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const step = go$a0;
        const v1 = step();
        if (v1.tag === "More") {
          go$a0 = v1._1;
          continue;
        }
        if (v1.tag === "Lift") {
          go$c = false;
          go$r = Monad0.Bind1().Apply0().Functor0().map(Loop)(v1._1);
          continue;
        }
        if (v1.tag === "Stop") {
          go$c = false;
          go$r = Monad0.Applicative0().pure($Step("Done", $Tuple(v1._2, v1._1)));
          continue;
        }
        fail();
      }
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
  const runParserT$p1 = runParserT$p(dictMonadRec);
  return (s) => (p) => dictMonadRec.Monad0().Bind1().Apply0().Functor0().map(fst)(runParserT$p1($ParseState(s, initialPos, false))(p));
};
var runParserT1 = /* @__PURE__ */ runParserT(monadRecIdentity);
var fail2 = (message2) => (state1, more, lift1, $$throw2, done) => more((v1) => position(
  state1,
  more,
  lift1,
  $$throw2,
  (state2, a) => more((v2) => $$throw2(state2, $ParseError(message2, a)))
));
var plusParserT = { empty: /* @__PURE__ */ fail2("No alternative"), Alt0: () => altParserT };
var alternativeParserT = { Applicative0: () => applicativeParserT, Plus1: () => plusParserT };

// output-es/Parsing.Combinators/index.js
var manyRec2 = /* @__PURE__ */ manyRec(monadRecParserT)(alternativeParserT);
var withLazyErrorMessage = (p) => (msg) => {
  const $0 = lazyParserT.defer((v) => fail2("Expected " + msg()));
  return (v2, $1, $2, $3, $4) => {
    const $5 = v2._1;
    const $6 = v2._2;
    return $1((v3) => p(
      $ParseState($5, $6, false),
      $1,
      $2,
      (v4, $7) => {
        const $8 = v4._3;
        return $1((v5) => {
          if ($8) {
            return $3(v4, $7);
          }
          return $0(v2, $1, $2, $3, $4);
        });
      },
      $4
    ));
  };
};
var withErrorMessage = (p) => (msg) => {
  const $0 = fail2("Expected " + msg);
  return (v2, $1, $2, $3, $4) => {
    const $5 = v2._1;
    const $6 = v2._2;
    return $1((v3) => p(
      $ParseState($5, $6, false),
      $1,
      $2,
      (v4, $7) => {
        const $8 = v4._3;
        return $1((v5) => {
          if ($8) {
            return $3(v4, $7);
          }
          return $0(v2, $1, $2, $3, $4);
        });
      },
      $4
    ));
  };
};
var skipMany1 = (p) => (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => p(
  state1,
  more,
  lift1,
  $$throw2,
  (state2, a) => more((v2$1) => more((v3) => {
    const loop = (state2$1, arg, gas) => {
      const $0 = (state3, step) => {
        if (step.tag === "Loop") {
          if (gas === 0) {
            return more((v1$1) => loop(state3, step._1, 30));
          }
          return loop(state3, step._1, gas - 1 | 0);
        }
        if (step.tag === "Done") {
          const $02 = step._1;
          return more((v4) => done(state3, $02));
        }
        fail();
      };
      const $1 = state2$1._1;
      const $2 = state2$1._2;
      return more((v3$1) => more((v1$1) => p(
        $ParseState($1, $2, false),
        more,
        lift1,
        (v4, $3) => {
          const $4 = v4._3;
          return more((v5) => {
            if ($4) {
              return $$throw2(v4, $3);
            }
            return $0(state2$1, $Step("Done", void 0));
          });
        },
        (state2$2, a$1) => more((v2$2) => $0(state2$2, $Step("Loop", void 0)))
      )));
    };
    return loop(state2, void 0, 30);
  }))
)));
var skipMany = (p) => (v2, $0, $1, $2, $3) => {
  const $4 = v2._1;
  const $5 = v2._2;
  return $0((v3) => skipMany1(p)(
    $ParseState($4, $5, false),
    $0,
    $1,
    (v4, $6) => {
      const $7 = v4._3;
      return $0((v5) => {
        if ($7) {
          return $2(v4, $6);
        }
        return $3(v2, void 0);
      });
    },
    $3
  ));
};
var sepBy1 = (p) => (sep) => (state1, more, lift1, $$throw2, done) => more((v1) => p(
  state1,
  more,
  lift1,
  $$throw2,
  (state2, a) => more((v2) => {
    const $0 = manyRec2((state1$1, more$1, lift1$1, throw$1, done$1) => more$1((v2$1) => more$1((v1$1) => sep(
      state1$1,
      more$1,
      lift1$1,
      throw$1,
      (state2$1, a$1) => more$1((v2$2) => more$1((v3) => p(state2$1, more$1, lift1$1, throw$1, (state3, a$2) => more$1((v4) => done$1(state3, a$2)))))
    ))));
    return more((v1$1) => $0(state2, more, lift1, $$throw2, (state2$1, a$1) => more((v2$1) => done(state2$1, $NonEmpty(a, a$1)))));
  })
));
var sepBy = (p) => (sep) => (v2, $0, $1, $2, $3) => {
  const $4 = v2._1;
  const $5 = v2._2;
  return $0((v3) => $0((v1) => sepBy1(p)(sep)(
    $ParseState($4, $5, false),
    $0,
    $1,
    (v4, $6) => {
      const $7 = v4._3;
      return $0((v5) => {
        if ($7) {
          return $2(v4, $6);
        }
        return $3(v2, Nil);
      });
    },
    (state2, a) => $0((v2$1) => $3(state2, $List("Cons", a._1, a._2)))
  )));
};
var notFollowedBy = (p) => (v1, $0, $1, $2, $3) => {
  const $4 = v1._3;
  const $5 = v1._1;
  const $6 = v1._2;
  return $0((v3) => {
    const $7 = (v4, $72) => {
      const $8 = v4._3;
      return $0((v5) => {
        if ($8) {
          return $2($ParseState(v4._1, v4._2, $4), $72);
        }
        return $3(v1, void 0);
      });
    };
    return $0((v2) => $0((v1$1) => p(
      $ParseState($5, $6, false),
      $0,
      $1,
      (v2$1, $8) => $7($ParseState(v2$1._1, v2$1._2, false), $8),
      (state2, a) => $0((v2$1) => $0((v3$1) => fail2("Negated parser succeeded")(state2, $0, $1, $7, (state3, a$1) => $0((v4) => $3(state3, a$1)))))
    )));
  });
};
var choice = (dictFoldable) => {
  const $0 = dictFoldable.foldr((p1) => (v) => {
    if (v.tag === "Nothing") {
      return $Maybe("Just", p1);
    }
    if (v.tag === "Just") {
      return $Maybe(
        "Just",
        (v2, $02, $1, $2, $3) => {
          const $4 = v2._1;
          const $5 = v2._2;
          return $02((v3) => p1(
            $ParseState($4, $5, false),
            $02,
            $1,
            (v4, $6) => {
              const $7 = v4._3;
              return $02((v5) => {
                if ($7) {
                  return $2(v4, $6);
                }
                return v._1(v2, $02, $1, $2, $3);
              });
            },
            $3
          ));
        }
      );
    }
    fail();
  })(Nothing);
  return (x) => {
    const $1 = $0(x);
    if ($1.tag === "Nothing") {
      return fail2("No alternative");
    }
    if ($1.tag === "Just") {
      return $1._1;
    }
    fail();
  };
};
var between = (open2) => (close2) => (p) => (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => more((v2$1) => more((v1$1) => open2(
  state1,
  more,
  lift1,
  $$throw2,
  (state2, a) => more((v2$2) => more((v3) => p(
    state2,
    more,
    lift1,
    $$throw2,
    (state3, a$1) => more((v4) => more((v2$3) => more((v3$1) => close2(state3, more, lift1, $$throw2, (state3$1, a$2) => more((v4$1) => done(state3$1, a$1))))))
  )))
)))));

// output-es/Parsing.Expr/index.js
var $Assoc = (tag) => tag;
var $Operator = (tag, _1, _2) => ({ tag, _1, _2 });
var choice2 = /* @__PURE__ */ choice(foldableList);
var identity22 = (x) => x;
var AssocNone = /* @__PURE__ */ $Assoc("AssocNone");
var AssocLeft = /* @__PURE__ */ $Assoc("AssocLeft");
var AssocRight = /* @__PURE__ */ $Assoc("AssocRight");
var splitOp = (v) => (v1) => {
  if (v.tag === "Infix") {
    if (v._2 === "AssocNone") {
      return { rassoc: v1.rassoc, lassoc: v1.lassoc, nassoc: $List("Cons", v._1, v1.nassoc), prefix: v1.prefix, postfix: v1.postfix };
    }
    if (v._2 === "AssocLeft") {
      return { rassoc: v1.rassoc, lassoc: $List("Cons", v._1, v1.lassoc), nassoc: v1.nassoc, prefix: v1.prefix, postfix: v1.postfix };
    }
    if (v._2 === "AssocRight") {
      return { rassoc: $List("Cons", v._1, v1.rassoc), lassoc: v1.lassoc, nassoc: v1.nassoc, prefix: v1.prefix, postfix: v1.postfix };
    }
    fail();
  }
  if (v.tag === "Prefix") {
    return { rassoc: v1.rassoc, lassoc: v1.lassoc, nassoc: v1.nassoc, prefix: $List("Cons", v._1, v1.prefix), postfix: v1.postfix };
  }
  if (v.tag === "Postfix") {
    return { rassoc: v1.rassoc, lassoc: v1.lassoc, nassoc: v1.nassoc, prefix: v1.prefix, postfix: $List("Cons", v._1, v1.postfix) };
  }
  fail();
};
var rassocP1 = (x) => (rassocOp) => (prefixP) => (term) => (postfixP) => {
  const $0 = rassocP(x)(rassocOp)(prefixP)(term)(postfixP);
  return (v2, $1, $2, $3, $4) => {
    const $5 = v2._1;
    const $6 = v2._2;
    return $1((v3) => $0(
      $ParseState($5, $6, false),
      $1,
      $2,
      (v4, $7) => {
        const $8 = v4._3;
        return $1((v5) => {
          if ($8) {
            return $3(v4, $7);
          }
          return $4(v2, x);
        });
      },
      $4
    ));
  };
};
var rassocP = (x) => (rassocOp) => (prefixP) => (term) => (postfixP) => (state1, more, lift1, $$throw2, done) => more((v1) => rassocOp(
  state1,
  more,
  lift1,
  $$throw2,
  (state2, a) => more((v2) => more((v1$1) => more((v1$2) => more((v1$3) => prefixP(
    state2,
    more,
    lift1,
    $$throw2,
    (state2$1, a$1) => more((v2$1) => more((v1$4) => term(
      state2$1,
      more,
      lift1,
      $$throw2,
      (state2$2, a$2) => more((v2$2) => more((v1$5) => postfixP(
        state2$2,
        more,
        lift1,
        $$throw2,
        (state2$3, a$3) => more((v2$3) => {
          const $0 = a$3(a$1(a$2));
          return more((v2$4) => rassocP1($0)(rassocOp)(prefixP)(term)(postfixP)(state2$3, more, lift1, $$throw2, (state2$4, a$4) => more((v2$5) => done(state2$4, a(x)(a$4)))));
        })
      )))
    )))
  )))))
));
var nassocP = (x) => (nassocOp) => (prefixP) => (term) => (postfixP) => (state1, more, lift1, $$throw2, done) => more((v1) => nassocOp(
  state1,
  more,
  lift1,
  $$throw2,
  (state2, a) => more((v2) => more((v1$1) => more((v1$2) => prefixP(
    state2,
    more,
    lift1,
    $$throw2,
    (state2$1, a$1) => more((v2$1) => more((v1$3) => term(
      state2$1,
      more,
      lift1,
      $$throw2,
      (state2$2, a$2) => more((v2$2) => more((v1$4) => postfixP(
        state2$2,
        more,
        lift1,
        $$throw2,
        (state2$3, a$3) => more((v2$3) => {
          const $0 = a$3(a$1(a$2));
          return more((v2$4) => done(state2$3, a(x)($0)));
        })
      )))
    )))
  ))))
));
var lassocP1 = (x) => (lassocOp) => (prefixP) => (term) => (postfixP) => {
  const $0 = lassocP(x)(lassocOp)(prefixP)(term)(postfixP);
  return (v2, $1, $2, $3, $4) => {
    const $5 = v2._1;
    const $6 = v2._2;
    return $1((v3) => $0(
      $ParseState($5, $6, false),
      $1,
      $2,
      (v4, $7) => {
        const $8 = v4._3;
        return $1((v5) => {
          if ($8) {
            return $3(v4, $7);
          }
          return $4(v2, x);
        });
      },
      $4
    ));
  };
};
var lassocP = (x) => (lassocOp) => (prefixP) => (term) => (postfixP) => (state1, more, lift1, $$throw2, done) => more((v1) => lassocOp(
  state1,
  more,
  lift1,
  $$throw2,
  (state2, a) => more((v2) => more((v1$1) => more((v1$2) => prefixP(
    state2,
    more,
    lift1,
    $$throw2,
    (state2$1, a$1) => more((v2$1) => more((v1$3) => term(
      state2$1,
      more,
      lift1,
      $$throw2,
      (state2$2, a$2) => more((v2$2) => more((v1$4) => postfixP(
        state2$2,
        more,
        lift1,
        $$throw2,
        (state2$3, a$3) => more((v2$3) => {
          const $0 = a$3(a$1(a$2));
          return more((v2$4) => lassocP1(a(x)($0))(lassocOp)(prefixP)(term)(postfixP)(state2$3, more, lift1, $$throw2, done));
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
  return (state1, more, lift1, $$throw2, done) => more((v1) => {
    const $0 = (state2, a) => more((v2) => {
      const $02 = rassocP(a)(rassocOp)((v2$1, $03, $12, $22, $32) => {
        const $42 = v2$1._1;
        const $52 = v2$1._2;
        return $03((v3) => prefixOp(
          $ParseState($42, $52, false),
          $03,
          $12,
          (v4, $6) => {
            const $7 = v4._3;
            return $03((v5) => {
              if ($7) {
                return $22(v4, $6);
              }
              return $32(v2$1, identity22);
            });
          },
          $32
        ));
      })(term)((v2$1, $03, $12, $22, $32) => {
        const $42 = v2$1._1;
        const $52 = v2$1._2;
        return $03((v3) => postfixOp(
          $ParseState($42, $52, false),
          $03,
          $12,
          (v4, $6) => {
            const $7 = v4._3;
            return $03((v5) => {
              if ($7) {
                return $22(v4, $6);
              }
              return $32(v2$1, identity22);
            });
          },
          $32
        ));
      });
      const $1 = lassocP(a)(lassocOp)((v2$1, $12, $22, $32, $42) => {
        const $52 = v2$1._1;
        const $6 = v2$1._2;
        return $12((v3) => prefixOp(
          $ParseState($52, $6, false),
          $12,
          $22,
          (v4, $7) => {
            const $8 = v4._3;
            return $12((v5) => {
              if ($8) {
                return $32(v4, $7);
              }
              return $42(v2$1, identity22);
            });
          },
          $42
        ));
      })(term)((v2$1, $12, $22, $32, $42) => {
        const $52 = v2$1._1;
        const $6 = v2$1._2;
        return $12((v3) => postfixOp(
          $ParseState($52, $6, false),
          $12,
          $22,
          (v4, $7) => {
            const $8 = v4._3;
            return $12((v5) => {
              if ($8) {
                return $32(v4, $7);
              }
              return $42(v2$1, identity22);
            });
          },
          $42
        ));
      });
      const $2 = nassocP(a)(nassocOp)((v2$1, $22, $32, $42, $52) => {
        const $6 = v2$1._1;
        const $7 = v2$1._2;
        return $22((v3) => prefixOp(
          $ParseState($6, $7, false),
          $22,
          $32,
          (v4, $8) => {
            const $9 = v4._3;
            return $22((v5) => {
              if ($9) {
                return $42(v4, $8);
              }
              return $52(v2$1, identity22);
            });
          },
          $52
        ));
      })(term)((v2$1, $22, $32, $42, $52) => {
        const $6 = v2$1._1;
        const $7 = v2$1._2;
        return $22((v3) => postfixOp(
          $ParseState($6, $7, false),
          $22,
          $32,
          (v4, $8) => {
            const $9 = v4._3;
            return $22((v5) => {
              if ($9) {
                return $42(v4, $8);
              }
              return $52(v2$1, identity22);
            });
          },
          $52
        ));
      });
      const $3 = withErrorMessage((state1$1, v, v1$1, v2$1, done$1) => done$1(state1$1, a))("operator");
      const $4 = state2._1;
      const $5 = state2._2;
      return more((v3) => $02(
        $ParseState($4, $5, false),
        more,
        lift1,
        (v4, $6) => {
          const $7 = v4._3;
          return more((v5) => {
            if ($7) {
              return $$throw2(v4, $6);
            }
            const $8 = state2._1;
            const $9 = state2._2;
            return more((v3$1) => $1(
              $ParseState($8, $9, false),
              more,
              lift1,
              (v4$1, $10) => {
                const $11 = v4$1._3;
                return more((v5$1) => {
                  if ($11) {
                    return $$throw2(v4$1, $10);
                  }
                  const $12 = state2._1;
                  const $13 = state2._2;
                  return more((v3$2) => $2(
                    $ParseState($12, $13, false),
                    more,
                    lift1,
                    (v4$2, $14) => {
                      const $15 = v4$2._3;
                      return more((v5$2) => {
                        if ($15) {
                          return $$throw2(v4$2, $14);
                        }
                        return $3(state2, more, lift1, $$throw2, done);
                      });
                    },
                    done
                  ));
                });
              },
              done
            ));
          });
        },
        done
      ));
    });
    return more((v1$1) => {
      const $1 = (state2, a) => more((v2) => more((v1$2) => term(
        state2,
        more,
        lift1,
        $$throw2,
        (state2$1, a$1) => more((v2$1) => more((v1$3) => {
          const $12 = state2$1._1;
          const $22 = state2$1._2;
          return more((v3) => postfixOp(
            $ParseState($12, $22, false),
            more,
            lift1,
            (v4, $32) => {
              const $4 = v4._3;
              return more((v5) => {
                if ($4) {
                  return $$throw2(v4, $32);
                }
                return more((v2$2) => $0(state2$1, a(a$1)));
              });
            },
            (state2$2, a$2) => more((v2$2) => $0(state2$2, a$2(a(a$1))))
          ));
        }))
      )));
      const $2 = state1._1;
      const $3 = state1._2;
      return more((v3) => prefixOp(
        $ParseState($2, $3, false),
        more,
        lift1,
        (v4, $4) => {
          const $5 = v4._3;
          return more((v5) => {
            if ($5) {
              return $$throw2(v4, $4);
            }
            return $1(state1, identity22);
          });
        },
        $1
      ));
    });
  });
};
var buildExprParser = (operators2) => (simpleExpr) => foldlArray(makeParser)(simpleExpr)(operators2);

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

// output-es/Bind/index.js
var union5 = /* @__PURE__ */ (() => setSet(ordString).union)();
var keys2 = (v) => {
  if (v.tag === "Nil") {
    return Leaf2;
  }
  if (v.tag === "Cons") {
    return union5($$$Map("Two", Leaf2, v._1._1, void 0, Leaf2))(keys2(v._2));
  }
  fail();
};

// output-es/Data.Unit/index.js
var showUnit = { show: (v) => "unit" };

// output-es/SExpr/index.js
var $DictEntry = (tag, _1, _2) => ({ tag, _1, _2 });
var $Expr2 = (tag, _1, _2, _3, _4) => ({ tag, _1, _2, _3, _4 });
var $ListRest = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
var $ListRestPattern = (tag, _1, _2) => ({ tag, _1, _2 });
var $Module = (_1) => ({ tag: "Module", _1 });
var $Pattern = (tag, _1, _2) => ({ tag, _1, _2 });
var $Qualifier = (tag, _1, _2) => ({ tag, _1, _2 });
var $VarDef2 = (_1, _2) => ({ tag: "VarDef", _1, _2 });
var genericShowArgsArgument = { genericShowArgs: (v) => [showStringImpl(v)] };
var genericShowSum = /* @__PURE__ */ (() => {
  const $0 = genericShowConstructor(genericShowArgsArgument)({ reflectSymbol: () => "PVar" });
  return (dictGenericShow1) => ({
    "genericShow'": (v) => {
      if (v.tag === "Inl") {
        return $0["genericShow'"](v._1);
      }
      if (v.tag === "Inr") {
        return dictGenericShow1["genericShow'"](v._1);
      }
      fail();
    }
  });
})();
var PConstrIsSymbol = { reflectSymbol: () => "PConstr" };
var showTuple = (dictShow1) => ({ show: (v) => "(Tuple " + showStringImpl(v._1) + " " + dictShow1.show(v._2) + ")" });
var PRecordIsSymbol = { reflectSymbol: () => "PRecord" };
var genericShowSum1 = /* @__PURE__ */ (() => {
  const $0 = genericShowConstructor(genericShowArgsNoArguments)({ reflectSymbol: () => "PListEmpty" });
  return (dictGenericShow1) => ({
    "genericShow'": (v) => {
      if (v.tag === "Inl") {
        return $0["genericShow'"](v._1);
      }
      if (v.tag === "Inr") {
        return dictGenericShow1["genericShow'"](v._1);
      }
      fail();
    }
  });
})();
var PListNonEmptyIsSymbol = { reflectSymbol: () => "PListNonEmpty" };
var genericShowSum2 = /* @__PURE__ */ (() => {
  const $0 = genericShowConstructor(genericShowArgsArgument)({ reflectSymbol: () => "PListVar" });
  return (dictGenericShow1) => ({
    "genericShow'": (v) => {
      if (v.tag === "Inl") {
        return $0["genericShow'"](v._1);
      }
      if (v.tag === "Inr") {
        return dictGenericShow1["genericShow'"](v._1);
      }
      fail();
    }
  });
})();
var genericShowSum3 = /* @__PURE__ */ (() => {
  const $0 = genericShowConstructor(genericShowArgsNoArguments)({ reflectSymbol: () => "PListEnd" });
  return (dictGenericShow1) => ({
    "genericShow'": (v) => {
      if (v.tag === "Inl") {
        return $0["genericShow'"](v._1);
      }
      if (v.tag === "Inr") {
        return dictGenericShow1["genericShow'"](v._1);
      }
      fail();
    }
  });
})();
var PListNextIsSymbol = { reflectSymbol: () => "PListNext" };
var VarDefIsSymbol = { reflectSymbol: () => "VarDef" };
var ListCompGuardIsSymbol = { reflectSymbol: () => "ListCompGuard" };
var ListCompGenIsSymbol = { reflectSymbol: () => "ListCompGen" };
var ListCompDeclIsSymbol = { reflectSymbol: () => "ListCompDecl" };
var EndIsSymbol = { reflectSymbol: () => "End" };
var NextIsSymbol = { reflectSymbol: () => "Next" };
var genericShowSum4 = /* @__PURE__ */ (() => {
  const $0 = genericShowConstructor(genericShowArgsArgument)({ reflectSymbol: () => "Var" });
  return (dictGenericShow1) => ({
    "genericShow'": (v) => {
      if (v.tag === "Inl") {
        return $0["genericShow'"](v._1);
      }
      if (v.tag === "Inr") {
        return dictGenericShow1["genericShow'"](v._1);
      }
      fail();
    }
  });
})();
var genericShowSum5 = /* @__PURE__ */ (() => {
  const $0 = genericShowConstructor(genericShowArgsArgument)({ reflectSymbol: () => "Op" });
  return (dictGenericShow1) => ({
    "genericShow'": (v) => {
      if (v.tag === "Inl") {
        return $0["genericShow'"](v._1);
      }
      if (v.tag === "Inr") {
        return dictGenericShow1["genericShow'"](v._1);
      }
      fail();
    }
  });
})();
var genericShowArgsArgument1 = { genericShowArgs: (v) => [showIntImpl(v)] };
var IntIsSymbol = { reflectSymbol: () => "Int" };
var genericShowArgsArgument2 = { genericShowArgs: (v) => [showNumberImpl(v)] };
var FloatIsSymbol = { reflectSymbol: () => "Float" };
var StrIsSymbol = { reflectSymbol: () => "Str" };
var ConstrIsSymbol = { reflectSymbol: () => "Constr" };
var DictionaryIsSymbol = { reflectSymbol: () => "Dictionary" };
var genericShowArgsProduct1 = /* @__PURE__ */ genericShowArgsProduct(/* @__PURE__ */ (() => {
  const $0 = showTuple(showString);
  return { genericShowArgs: (v) => [$0.show(v)] };
})());
var MatrixIsSymbol = { reflectSymbol: () => "Matrix" };
var LambdaIsSymbol = { reflectSymbol: () => "Lambda" };
var ProjectIsSymbol = { reflectSymbol: () => "Project" };
var DProjectIsSymbol = { reflectSymbol: () => "DProject" };
var AppIsSymbol = { reflectSymbol: () => "App" };
var BinaryAppIsSymbol = { reflectSymbol: () => "BinaryApp" };
var MatchAsIsSymbol = { reflectSymbol: () => "MatchAs" };
var IfElseIsSymbol = { reflectSymbol: () => "IfElse" };
var ListEmptyIsSymbol = { reflectSymbol: () => "ListEmpty" };
var ListNonEmptyIsSymbol = { reflectSymbol: () => "ListNonEmpty" };
var ListEnumIsSymbol = { reflectSymbol: () => "ListEnum" };
var ListCompIsSymbol = { reflectSymbol: () => "ListComp" };
var LetIsSymbol = { reflectSymbol: () => "Let" };
var LetRecIsSymbol = { reflectSymbol: () => "LetRec" };
var ExprKeyIsSymbol = { reflectSymbol: () => "ExprKey" };
var VarKeyIsSymbol = { reflectSymbol: () => "VarKey" };
var ClausesIsSymbol = { reflectSymbol: () => "Clauses" };
var ClauseIsSymbol = { reflectSymbol: () => "Clause" };
var difference3 = /* @__PURE__ */ difference(eqString);
var toUnfoldable7 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
var monadThrowExceptT2 = /* @__PURE__ */ monadThrowExceptT(monadIdentity);
var fromFoldable10 = /* @__PURE__ */ fromFoldable2(foldableArray);
var fromFoldable16 = /* @__PURE__ */ fromFoldable2(foldableNonEmptyList);
var fromFoldable24 = /* @__PURE__ */ fromFoldable2(foldableList);
var monadErrorExceptT2 = /* @__PURE__ */ monadErrorExceptT(monadIdentity);
var PListEnd = /* @__PURE__ */ $ListRestPattern("PListEnd");
var PListNext = (value0) => (value1) => $ListRestPattern("PListNext", value0, value1);
var PConstr = (value0) => (value1) => $Pattern("PConstr", value0, value1);
var PListEmpty = /* @__PURE__ */ $Pattern("PListEmpty");
var PListNonEmpty = (value0) => (value1) => $Pattern("PListNonEmpty", value0, value1);
var Clause = (x) => x;
var Int2 = (value0) => (value1) => $Expr2("Int", value0, value1);
var Float2 = (value0) => (value1) => $Expr2("Float", value0, value1);
var Str2 = (value0) => (value1) => $Expr2("Str", value0, value1);
var Constr2 = (value0) => (value1) => (value2) => $Expr2("Constr", value0, value1, value2);
var Dictionary3 = (value0) => (value1) => $Expr2("Dictionary", value0, value1);
var Matrix2 = (value0) => (value1) => (value2) => (value3) => $Expr2("Matrix", value0, value1, value2, value3);
var Project2 = (value0) => (value1) => $Expr2("Project", value0, value1);
var DProject2 = (value0) => (value1) => $Expr2("DProject", value0, value1);
var App3 = (value0) => (value1) => $Expr2("App", value0, value1);
var MatchAs = (value0) => (value1) => $Expr2("MatchAs", value0, value1);
var IfElse = (value0) => (value1) => (value2) => $Expr2("IfElse", value0, value1, value2);
var ListNonEmpty = (value0) => (value1) => (value2) => $Expr2("ListNonEmpty", value0, value1, value2);
var ListEnum = (value0) => (value1) => $Expr2("ListEnum", value0, value1);
var ListComp = (value0) => (value1) => (value2) => $Expr2("ListComp", value0, value1, value2);
var Let2 = (value0) => (value1) => $Expr2("Let", value0, value1);
var LetRec2 = (value0) => (value1) => $Expr2("LetRec", value0, value1);
var VarKey = (value0) => (value1) => $DictEntry("VarKey", value0, value1);
var Next = (value0) => (value1) => (value2) => $ListRest("Next", value0, value1, value2);
var ListCompGen = (value0) => (value1) => $Qualifier("ListCompGen", value0, value1);
var VarDef2 = (value0) => (value1) => $VarDef2(value0, value1);
var RecDef = (x) => x;
var genericPattern_ = {
  to: (x) => {
    if (x.tag === "Inl") {
      return $Pattern("PVar", x._1);
    }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") {
        return $Pattern("PConstr", x._1._1._1, x._1._1._2);
      }
      if (x._1.tag === "Inr") {
        if (x._1._1.tag === "Inl") {
          return $Pattern("PRecord", x._1._1._1);
        }
        if (x._1._1.tag === "Inr") {
          if (x._1._1._1.tag === "Inl") {
            return PListEmpty;
          }
          if (x._1._1._1.tag === "Inr") {
            return $Pattern("PListNonEmpty", x._1._1._1._1._1, x._1._1._1._1._2);
          }
        }
      }
    }
    fail();
  },
  from: (x) => {
    if (x.tag === "PVar") {
      return $Sum("Inl", x._1);
    }
    if (x.tag === "PConstr") {
      return $Sum("Inr", $Sum("Inl", $Product(x._1, x._2)));
    }
    if (x.tag === "PRecord") {
      return $Sum("Inr", $Sum("Inr", $Sum("Inl", x._1)));
    }
    if (x.tag === "PListEmpty") {
      return $Sum("Inr", $Sum("Inr", $Sum("Inr", $Sum("Inl", NoArguments))));
    }
    if (x.tag === "PListNonEmpty") {
      return $Sum("Inr", $Sum("Inr", $Sum("Inr", $Sum("Inr", $Product(x._1, x._2)))));
    }
    fail();
  }
};
var genericListRestPattern_ = {
  to: (x) => {
    if (x.tag === "Inl") {
      return $ListRestPattern("PListVar", x._1);
    }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") {
        return PListEnd;
      }
      if (x._1.tag === "Inr") {
        return $ListRestPattern("PListNext", x._1._1._1, x._1._1._2);
      }
    }
    fail();
  },
  from: (x) => {
    if (x.tag === "PListVar") {
      return $Sum("Inl", x._1);
    }
    if (x.tag === "PListEnd") {
      return $Sum("Inr", $Sum("Inl", NoArguments));
    }
    if (x.tag === "PListNext") {
      return $Sum("Inr", $Sum("Inr", $Product(x._1, x._2)));
    }
    fail();
  }
};
var showPattern1 = {
  show: (c) => genericShowSum((() => {
    const $0 = genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument)((() => {
      const $02 = showList(showPattern1);
      return { genericShowArgs: (v) => [$02.show(v)] };
    })()))(PConstrIsSymbol);
    const $1 = genericShowConstructor((() => {
      const $12 = showList(showTuple(showPattern1));
      return { genericShowArgs: (v) => [$12.show(v)] };
    })())(PRecordIsSymbol);
    const $2 = (() => {
      const $22 = genericShowSum1(genericShowConstructor(genericShowArgsProduct({ genericShowArgs: (v) => [showPattern1.show(v)] })({
        genericShowArgs: (v) => [showListRestPattern.show(v)]
      }))(PListNonEmptyIsSymbol));
      return {
        "genericShow'": (v) => {
          if (v.tag === "Inl") {
            return $1["genericShow'"](v._1);
          }
          if (v.tag === "Inr") {
            return $22["genericShow'"](v._1);
          }
          fail();
        }
      };
    })();
    return {
      "genericShow'": (v) => {
        if (v.tag === "Inl") {
          return $0["genericShow'"](v._1);
        }
        if (v.tag === "Inr") {
          return $2["genericShow'"](v._1);
        }
        fail();
      }
    };
  })())["genericShow'"](genericPattern_.from(c))
};
var showListRestPattern = {
  show: (c) => genericShowSum2(genericShowSum3(genericShowConstructor(genericShowArgsProduct({ genericShowArgs: (v) => [showPattern1.show(v)] })({
    genericShowArgs: (v) => [showListRestPattern.show(v)]
  }))(PListNextIsSymbol)))["genericShow'"](genericListRestPattern_.from(c))
};
var showTuple1 = (dictShow1) => ({ show: (v) => "(Tuple " + showPattern1.show(v._1) + " " + dictShow1.show(v._2) + ")" });
var showTuple2 = /* @__PURE__ */ (() => {
  const $0 = showNonEmptyList(showPattern1);
  return (dictShow1) => ({ show: (v) => "(Tuple " + $0.show(v._1) + " " + dictShow1.show(v._2) + ")" });
})();
var genericExpr_ = {
  to: (x) => {
    if (x.tag === "Inl") {
      return $Expr2("Var", x._1);
    }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") {
        return $Expr2("Op", x._1._1);
      }
      if (x._1.tag === "Inr") {
        if (x._1._1.tag === "Inl") {
          return $Expr2("Int", x._1._1._1._1, x._1._1._1._2);
        }
        if (x._1._1.tag === "Inr") {
          if (x._1._1._1.tag === "Inl") {
            return $Expr2("Float", x._1._1._1._1._1, x._1._1._1._1._2);
          }
          if (x._1._1._1.tag === "Inr") {
            if (x._1._1._1._1.tag === "Inl") {
              return $Expr2("Str", x._1._1._1._1._1._1, x._1._1._1._1._1._2);
            }
            if (x._1._1._1._1.tag === "Inr") {
              if (x._1._1._1._1._1.tag === "Inl") {
                return $Expr2("Constr", x._1._1._1._1._1._1._1, x._1._1._1._1._1._1._2._1, x._1._1._1._1._1._1._2._2);
              }
              if (x._1._1._1._1._1.tag === "Inr") {
                if (x._1._1._1._1._1._1.tag === "Inl") {
                  return $Expr2("Dictionary", x._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._2);
                }
                if (x._1._1._1._1._1._1.tag === "Inr") {
                  if (x._1._1._1._1._1._1._1.tag === "Inl") {
                    return $Expr2("Matrix", x._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._2._1, x._1._1._1._1._1._1._1._1._2._2._1, x._1._1._1._1._1._1._1._1._2._2._2);
                  }
                  if (x._1._1._1._1._1._1._1.tag === "Inr") {
                    if (x._1._1._1._1._1._1._1._1.tag === "Inl") {
                      return $Expr2("Lambda", x._1._1._1._1._1._1._1._1._1);
                    }
                    if (x._1._1._1._1._1._1._1._1.tag === "Inr") {
                      if (x._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                        return $Expr2("Project", x._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._2);
                      }
                      if (x._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                        if (x._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                          return $Expr2("DProject", x._1._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._1._2);
                        }
                        if (x._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                          if (x._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                            return $Expr2("App", x._1._1._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._1._1._2);
                          }
                          if (x._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                            if (x._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                              return $Expr2(
                                "BinaryApp",
                                x._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                x._1._1._1._1._1._1._1._1._1._1._1._1._1._2._1,
                                x._1._1._1._1._1._1._1._1._1._1._1._1._1._2._2
                              );
                            }
                            if (x._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                              if (x._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                return $Expr2("MatchAs", x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2);
                              }
                              if (x._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                  return $Expr2(
                                    "IfElse",
                                    x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                    x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._1,
                                    x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._2
                                  );
                                }
                                if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                  if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                    return $Expr2("ListEmpty", x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                  }
                                  if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                    if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                      return $Expr2(
                                        "ListNonEmpty",
                                        x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                        x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._1,
                                        x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._2
                                      );
                                    }
                                    if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                      if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                        return $Expr2(
                                          "ListEnum",
                                          x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                          x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2
                                        );
                                      }
                                      if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                        if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                          return $Expr2(
                                            "ListComp",
                                            x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                            x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._1,
                                            x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._2
                                          );
                                        }
                                        if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                          if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                            return $Expr2(
                                              "Let",
                                              x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                              x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2
                                            );
                                          }
                                          if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                            return $Expr2(
                                              "LetRec",
                                              x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                              x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2
                                            );
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    fail();
  },
  from: (x) => {
    if (x.tag === "Var") {
      return $Sum("Inl", x._1);
    }
    if (x.tag === "Op") {
      return $Sum("Inr", $Sum("Inl", x._1));
    }
    if (x.tag === "Int") {
      return $Sum("Inr", $Sum("Inr", $Sum("Inl", $Product(x._1, x._2))));
    }
    if (x.tag === "Float") {
      return $Sum("Inr", $Sum("Inr", $Sum("Inr", $Sum("Inl", $Product(x._1, x._2)))));
    }
    if (x.tag === "Str") {
      return $Sum(
        "Inr",
        $Sum("Inr", $Sum("Inr", $Sum("Inr", $Sum("Inl", $Product(x._1, x._2)))))
      );
    }
    if (x.tag === "Constr") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum("Inr", $Sum("Inl", $Product(x._1, $Product(x._2, x._3))))
            )
          )
        )
      );
    }
    if (x.tag === "Dictionary") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum("Inr", $Sum("Inr", $Sum("Inr", $Sum("Inl", $Product(x._1, x._2)))))
          )
        )
      );
    }
    if (x.tag === "Matrix") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum("Inl", $Product(x._1, $Product(x._2, $Product(x._3, x._4))))
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "Lambda") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum("Inr", $Sum("Inr", $Sum("Inr", $Sum("Inr", $Sum("Inl", x._1)))))
            )
          )
        )
      );
    }
    if (x.tag === "Project") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum("Inr", $Sum("Inr", $Sum("Inl", $Product(x._1, x._2))))
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "DProject") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum(
                      "Inr",
                      $Sum("Inr", $Sum("Inr", $Sum("Inl", $Product(x._1, x._2))))
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "App") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum(
                      "Inr",
                      $Sum(
                        "Inr",
                        $Sum("Inr", $Sum("Inr", $Sum("Inl", $Product(x._1, x._2))))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "BinaryApp") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum(
                      "Inr",
                      $Sum(
                        "Inr",
                        $Sum(
                          "Inr",
                          $Sum(
                            "Inr",
                            $Sum("Inr", $Sum("Inl", $Product(x._1, $Product(x._2, x._3))))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "MatchAs") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum(
                      "Inr",
                      $Sum(
                        "Inr",
                        $Sum(
                          "Inr",
                          $Sum(
                            "Inr",
                            $Sum("Inr", $Sum("Inr", $Sum("Inl", $Product(x._1, x._2))))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "IfElse") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum(
                      "Inr",
                      $Sum(
                        "Inr",
                        $Sum(
                          "Inr",
                          $Sum(
                            "Inr",
                            $Sum(
                              "Inr",
                              $Sum(
                                "Inr",
                                $Sum("Inr", $Sum("Inl", $Product(x._1, $Product(x._2, x._3))))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "ListEmpty") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum(
                      "Inr",
                      $Sum(
                        "Inr",
                        $Sum(
                          "Inr",
                          $Sum(
                            "Inr",
                            $Sum(
                              "Inr",
                              $Sum("Inr", $Sum("Inr", $Sum("Inr", $Sum("Inl", x._1))))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "ListNonEmpty") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum(
                      "Inr",
                      $Sum(
                        "Inr",
                        $Sum(
                          "Inr",
                          $Sum(
                            "Inr",
                            $Sum(
                              "Inr",
                              $Sum(
                                "Inr",
                                $Sum(
                                  "Inr",
                                  $Sum(
                                    "Inr",
                                    $Sum("Inr", $Sum("Inl", $Product(x._1, $Product(x._2, x._3))))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "ListEnum") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum(
                      "Inr",
                      $Sum(
                        "Inr",
                        $Sum(
                          "Inr",
                          $Sum(
                            "Inr",
                            $Sum(
                              "Inr",
                              $Sum(
                                "Inr",
                                $Sum(
                                  "Inr",
                                  $Sum(
                                    "Inr",
                                    $Sum("Inr", $Sum("Inr", $Sum("Inl", $Product(x._1, x._2))))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "ListComp") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum(
                      "Inr",
                      $Sum(
                        "Inr",
                        $Sum(
                          "Inr",
                          $Sum(
                            "Inr",
                            $Sum(
                              "Inr",
                              $Sum(
                                "Inr",
                                $Sum(
                                  "Inr",
                                  $Sum(
                                    "Inr",
                                    $Sum(
                                      "Inr",
                                      $Sum(
                                        "Inr",
                                        $Sum("Inr", $Sum("Inl", $Product(x._1, $Product(x._2, x._3))))
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "Let") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum(
                      "Inr",
                      $Sum(
                        "Inr",
                        $Sum(
                          "Inr",
                          $Sum(
                            "Inr",
                            $Sum(
                              "Inr",
                              $Sum(
                                "Inr",
                                $Sum(
                                  "Inr",
                                  $Sum(
                                    "Inr",
                                    $Sum(
                                      "Inr",
                                      $Sum(
                                        "Inr",
                                        $Sum("Inr", $Sum("Inr", $Sum("Inl", $Product(x._1, x._2))))
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "LetRec") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum(
            "Inr",
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum(
                  "Inr",
                  $Sum(
                    "Inr",
                    $Sum(
                      "Inr",
                      $Sum(
                        "Inr",
                        $Sum(
                          "Inr",
                          $Sum(
                            "Inr",
                            $Sum(
                              "Inr",
                              $Sum(
                                "Inr",
                                $Sum(
                                  "Inr",
                                  $Sum(
                                    "Inr",
                                    $Sum(
                                      "Inr",
                                      $Sum(
                                        "Inr",
                                        $Sum("Inr", $Sum("Inr", $Sum("Inr", $Product(x._1, x._2))))
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    fail();
  }
};
var showVarDef = (dictShow) => ({
  show: (c) => genericShowConstructor(genericShowArgsProduct({ genericShowArgs: (v) => [showPattern1.show(v)] })((() => {
    const $0 = showExpr(dictShow);
    return { genericShowArgs: (v) => [$0.show(v)] };
  })()))(VarDefIsSymbol)["genericShow'"]($Product(c._1, c._2))
});
var showQualifier = (dictShow) => ({
  show: (c) => {
    const $0 = genericShowConstructor((() => {
      const $02 = showExpr(dictShow);
      return { genericShowArgs: (v) => [$02.show(v)] };
    })())(ListCompGuardIsSymbol);
    const $1 = genericShowConstructor(genericShowArgsProduct({ genericShowArgs: (v) => [showPattern1.show(v)] })((() => {
      const $12 = showExpr(dictShow);
      return { genericShowArgs: (v) => [$12.show(v)] };
    })()))(ListCompGenIsSymbol);
    const $2 = genericShowConstructor((() => {
      const $22 = showVarDef(dictShow);
      return { genericShowArgs: (v) => [$22.show(v)] };
    })())(ListCompDeclIsSymbol);
    if (c.tag === "ListCompGuard") {
      return $0["genericShow'"](c._1);
    }
    if (c.tag === "ListCompGen") {
      return $1["genericShow'"]($Product(c._1, c._2));
    }
    if (c.tag === "ListCompDecl") {
      return $2["genericShow'"](c._1);
    }
    fail();
  }
});
var showListRest = (dictShow) => {
  const genericShowArgsArgument3 = { genericShowArgs: (v) => [dictShow.show(v)] };
  const $0 = genericShowConstructor(genericShowArgsArgument3)(EndIsSymbol);
  return {
    show: (c) => {
      const $1 = genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument3)(genericShowArgsProduct((() => {
        const $12 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$12.show(v)] };
      })())((() => {
        const $12 = showListRest(dictShow);
        return { genericShowArgs: (v) => [$12.show(v)] };
      })())))(NextIsSymbol);
      if (c.tag === "End") {
        return $0["genericShow'"](c._1);
      }
      if (c.tag === "Next") {
        return $1["genericShow'"]($Product(c._1, $Product(c._2, c._3)));
      }
      fail();
    }
  };
};
var showExpr = (dictShow) => {
  const genericShowArgsArgument3 = { genericShowArgs: (v) => [dictShow.show(v)] };
  const $0 = genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument3)(genericShowArgsArgument1))(IntIsSymbol);
  const $1 = genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument3)(genericShowArgsArgument2))(FloatIsSymbol);
  const $2 = genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument3)(genericShowArgsArgument))(StrIsSymbol);
  const $3 = genericShowConstructor(genericShowArgsArgument3)(ListEmptyIsSymbol);
  return {
    show: (c) => genericShowSum4(genericShowSum5((() => {
      const $4 = genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument3)(genericShowArgsProduct(genericShowArgsArgument)((() => {
        const $42 = showList(showExpr(dictShow));
        return { genericShowArgs: (v) => [$42.show(v)] };
      })())))(ConstrIsSymbol);
      const $5 = genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument3)((() => {
        const $52 = showList((() => {
          const $53 = showDictEntry(dictShow);
          const $62 = showExpr(dictShow);
          return { show: (v) => "(Tuple " + $53.show(v._1) + " " + $62.show(v._2) + ")" };
        })());
        return { genericShowArgs: (v) => [$52.show(v)] };
      })()))(DictionaryIsSymbol);
      const $6 = genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument3)(genericShowArgsProduct((() => {
        const $62 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$62.show(v)] };
      })())(genericShowArgsProduct1((() => {
        const $62 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$62.show(v)] };
      })()))))(MatrixIsSymbol);
      const $7 = genericShowConstructor((() => {
        const $72 = showClauses(dictShow);
        return { genericShowArgs: (v) => [$72.show(v)] };
      })())(LambdaIsSymbol);
      const $8 = genericShowConstructor(genericShowArgsProduct((() => {
        const $82 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$82.show(v)] };
      })())(genericShowArgsArgument))(ProjectIsSymbol);
      const $9 = genericShowConstructor(genericShowArgsProduct((() => {
        const $92 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$92.show(v)] };
      })())((() => {
        const $92 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$92.show(v)] };
      })()))(DProjectIsSymbol);
      const $10 = genericShowConstructor(genericShowArgsProduct((() => {
        const $102 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$102.show(v)] };
      })())((() => {
        const $102 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$102.show(v)] };
      })()))(AppIsSymbol);
      const $11 = genericShowConstructor(genericShowArgsProduct((() => {
        const $112 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$112.show(v)] };
      })())(genericShowArgsProduct(genericShowArgsArgument)((() => {
        const $112 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$112.show(v)] };
      })())))(BinaryAppIsSymbol);
      const $12 = genericShowConstructor(genericShowArgsProduct((() => {
        const $122 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$122.show(v)] };
      })())((() => {
        const $122 = showNonEmptyList(showTuple1(showExpr(dictShow)));
        return { genericShowArgs: (v) => [$122.show(v)] };
      })()))(MatchAsIsSymbol);
      const $13 = genericShowConstructor(genericShowArgsProduct((() => {
        const $132 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$132.show(v)] };
      })())(genericShowArgsProduct((() => {
        const $132 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$132.show(v)] };
      })())((() => {
        const $132 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$132.show(v)] };
      })())))(IfElseIsSymbol);
      const $14 = genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument3)(genericShowArgsProduct((() => {
        const $142 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$142.show(v)] };
      })())((() => {
        const $142 = showListRest(dictShow);
        return { genericShowArgs: (v) => [$142.show(v)] };
      })())))(ListNonEmptyIsSymbol);
      const $15 = genericShowConstructor(genericShowArgsProduct((() => {
        const $152 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$152.show(v)] };
      })())((() => {
        const $152 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$152.show(v)] };
      })()))(ListEnumIsSymbol);
      const $16 = genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument3)(genericShowArgsProduct((() => {
        const $162 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$162.show(v)] };
      })())((() => {
        const $162 = showList(showQualifier(dictShow));
        return { genericShowArgs: (v) => [$162.show(v)] };
      })())))(ListCompIsSymbol);
      const $17 = genericShowConstructor(genericShowArgsProduct((() => {
        const $172 = showNonEmptyList(showVarDef(dictShow));
        return { genericShowArgs: (v) => [$172.show(v)] };
      })())((() => {
        const $172 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$172.show(v)] };
      })()))(LetIsSymbol);
      const $18 = genericShowConstructor(genericShowArgsProduct((() => {
        const $182 = showNonEmptyList(showTuple(showClause(dictShow)));
        return { genericShowArgs: (v) => [$182.show(v)] };
      })())((() => {
        const $182 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$182.show(v)] };
      })()))(LetRecIsSymbol);
      return {
        "genericShow'": (v) => {
          if (v.tag === "Inl") {
            return $0["genericShow'"](v._1);
          }
          if (v.tag === "Inr") {
            if (v._1.tag === "Inl") {
              return $1["genericShow'"](v._1._1);
            }
            if (v._1.tag === "Inr") {
              if (v._1._1.tag === "Inl") {
                return $2["genericShow'"](v._1._1._1);
              }
              if (v._1._1.tag === "Inr") {
                if (v._1._1._1.tag === "Inl") {
                  return $4["genericShow'"](v._1._1._1._1);
                }
                if (v._1._1._1.tag === "Inr") {
                  if (v._1._1._1._1.tag === "Inl") {
                    return $5["genericShow'"](v._1._1._1._1._1);
                  }
                  if (v._1._1._1._1.tag === "Inr") {
                    if (v._1._1._1._1._1.tag === "Inl") {
                      return $6["genericShow'"](v._1._1._1._1._1._1);
                    }
                    if (v._1._1._1._1._1.tag === "Inr") {
                      if (v._1._1._1._1._1._1.tag === "Inl") {
                        return $7["genericShow'"](v._1._1._1._1._1._1._1);
                      }
                      if (v._1._1._1._1._1._1.tag === "Inr") {
                        if (v._1._1._1._1._1._1._1.tag === "Inl") {
                          return $8["genericShow'"](v._1._1._1._1._1._1._1._1);
                        }
                        if (v._1._1._1._1._1._1._1.tag === "Inr") {
                          if (v._1._1._1._1._1._1._1._1.tag === "Inl") {
                            return $9["genericShow'"](v._1._1._1._1._1._1._1._1._1);
                          }
                          if (v._1._1._1._1._1._1._1._1.tag === "Inr") {
                            if (v._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                              return $10["genericShow'"](v._1._1._1._1._1._1._1._1._1._1);
                            }
                            if (v._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                              if (v._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                return $11["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1);
                              }
                              if (v._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                if (v._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                  return $12["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1);
                                }
                                if (v._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                  if (v._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                    return $13["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                  }
                                  if (v._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                    if (v._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                      return $3["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                    }
                                    if (v._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                      if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                        return $14["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                      }
                                      if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                        if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                          return $15["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                        }
                                        if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                          if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                            return $16["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                          }
                                          if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                            if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                              return $17["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                            }
                                            if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                              return $18["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          fail();
        }
      };
    })()))["genericShow'"](genericExpr_.from(c))
  };
};
var showDictEntry = (dictShow) => {
  const genericShowConstructor2 = genericShowConstructor(genericShowArgsProduct({ genericShowArgs: (v) => [dictShow.show(v)] })(genericShowArgsArgument))(VarKeyIsSymbol);
  return {
    show: (c) => {
      const $0 = genericShowConstructor((() => {
        const $02 = showExpr(dictShow);
        return { genericShowArgs: (v) => [$02.show(v)] };
      })())(ExprKeyIsSymbol);
      if (c.tag === "ExprKey") {
        return $0["genericShow'"](c._1);
      }
      if (c.tag === "VarKey") {
        return genericShowConstructor2["genericShow'"]($Product(c._1, c._2));
      }
      fail();
    }
  };
};
var showClauses = (dictShow) => ({
  show: (c) => genericShowConstructor((() => {
    const $0 = showNonEmptyList(showClause(dictShow));
    return { genericShowArgs: (v) => [$0.show(v)] };
  })())(ClausesIsSymbol)["genericShow'"](c)
});
var showClause = (dictShow) => ({
  show: (c) => genericShowConstructor((() => {
    const $0 = showTuple2(showExpr(dictShow));
    return { genericShowArgs: (v) => [$0.show(v)] };
  })())(ClauseIsSymbol)["genericShow'"](c)
});
var show2 = /* @__PURE__ */ (() => showExpr(showUnit).show)();
var functorVarDef2 = { map: (f) => (m) => $VarDef2(m._1, functorExpr2.map(f)(m._2)) };
var functorQualifier = {
  map: (f) => (m) => {
    if (m.tag === "ListCompGuard") {
      return $Qualifier("ListCompGuard", functorExpr2.map(f)(m._1));
    }
    if (m.tag === "ListCompGen") {
      return $Qualifier("ListCompGen", m._1, functorExpr2.map(f)(m._2));
    }
    if (m.tag === "ListCompDecl") {
      return $Qualifier("ListCompDecl", $VarDef2(m._1._1, functorExpr2.map(f)(m._1._2)));
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
    if (m.tag === "Dictionary") {
      return $Expr2(
        "Dictionary",
        f(m._1),
        listMap((() => {
          const $0 = functorDictEntry.map(f);
          const $1 = functorExpr2.map(f);
          return (v) => $Tuple($0(v._1), $1(v._2));
        })())(m._2)
      );
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
    if (m.tag === "DProject") {
      return $Expr2("DProject", functorExpr2.map(f)(m._1), functorExpr2.map(f)(m._2));
    }
    if (m.tag === "App") {
      return $Expr2("App", functorExpr2.map(f)(m._1), functorExpr2.map(f)(m._2));
    }
    if (m.tag === "BinaryApp") {
      return $Expr2("BinaryApp", functorExpr2.map(f)(m._1), m._2, functorExpr2.map(f)(m._3));
    }
    if (m.tag === "MatchAs") {
      return $Expr2(
        "MatchAs",
        functorExpr2.map(f)(m._1),
        (() => {
          const $0 = functorExpr2.map(f);
          return $NonEmpty($Tuple(m._2._1._1, $0(m._2._1._2)), listMap((m$1) => $Tuple(m$1._1, $0(m$1._2)))(m._2._2));
        })()
      );
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
      return $Expr2(
        "Let",
        $NonEmpty($VarDef2(m._1._1._1, functorExpr2.map(f)(m._1._1._2)), listMap(functorVarDef2.map(f))(m._1._2)),
        functorExpr2.map(f)(m._2)
      );
    }
    if (m.tag === "LetRec") {
      return $Expr2(
        "LetRec",
        (() => {
          const $0 = functorClause.map(f);
          return $NonEmpty($Tuple(m._1._1._1, $0(m._1._1._2)), listMap((m$1) => $Tuple(m$1._1, $0(m$1._2)))(m._1._2));
        })(),
        functorExpr2.map(f)(m._2)
      );
    }
    fail();
  }
};
var functorDictEntry = {
  map: (f) => (m) => {
    if (m.tag === "ExprKey") {
      return $DictEntry("ExprKey", functorExpr2.map(f)(m._1));
    }
    if (m.tag === "VarKey") {
      return $DictEntry("VarKey", f(m._1), m._2);
    }
    fail();
  }
};
var functorClauses = {
  map: (f) => (m) => {
    const $0 = functorClause.map(f);
    return $NonEmpty($0(m._1), listMap($0)(m._2));
  }
};
var functorClause = { map: (f) => (m) => $Tuple(m._1, functorExpr2.map(f)(m._2)) };
var eqPattern = {
  eq: (x) => (y) => {
    if (x.tag === "PVar") {
      return y.tag === "PVar" && x._1 === y._1;
    }
    if (x.tag === "PConstr") {
      return y.tag === "PConstr" && x._1 === y._1 && (() => {
        const go = (v) => (v1) => (v2) => {
          if (!v2) {
            return false;
          }
          if (v.tag === "Nil") {
            return v1.tag === "Nil" && v2;
          }
          return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && eqPattern.eq(v1._1)(v._1));
        };
        return go(x._2)(y._2)(true);
      })();
    }
    if (x.tag === "PRecord") {
      return y.tag === "PRecord" && (() => {
        const go = (v) => (v1) => (v2) => {
          if (!v2) {
            return false;
          }
          if (v.tag === "Nil") {
            return v1.tag === "Nil" && v2;
          }
          return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && v1._1._1 === v._1._1 && eqPattern.eq(v1._1._2)(v._1._2));
        };
        return go(x._1)(y._1)(true);
      })();
    }
    if (x.tag === "PListEmpty") {
      return y.tag === "PListEmpty";
    }
    return x.tag === "PListNonEmpty" && y.tag === "PListNonEmpty" && eqPattern.eq(x._1)(y._1) && eqListRestPattern.eq(x._2)(y._2);
  }
};
var eqListRestPattern = {
  eq: (x) => (y) => {
    if (x.tag === "PListVar") {
      return y.tag === "PListVar" && x._1 === y._1;
    }
    if (x.tag === "PListEnd") {
      return y.tag === "PListEnd";
    }
    return x.tag === "PListNext" && y.tag === "PListNext" && eqPattern.eq(x._1)(y._1) && eqListRestPattern.eq(x._2)(y._2);
  }
};
var eqList = {
  eq: (xs) => (ys) => {
    const go = (v) => (v1) => (v2) => {
      if (!v2) {
        return false;
      }
      if (v.tag === "Nil") {
        return v1.tag === "Nil" && v2;
      }
      return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v1._1.tag === "Left" ? v2 && v._1.tag === "Left" && eqPattern.eq(v1._1._1)(v._1._1) : v2 && v1._1.tag === "Right" && v._1.tag === "Right" && eqListRestPattern.eq(v1._1._1)(v._1._1));
    };
    return go(xs)(ys)(true);
  }
};
var eq8 = (xs) => (ys) => {
  const go = (v) => (v1) => (v2) => {
    if (!v2) {
      return false;
    }
    if (v.tag === "Nil") {
      return v1.tag === "Nil" && v2;
    }
    return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && eqPattern.eq(v1._1)(v._1));
  };
  return go(xs)(ys)(true);
};
var varKeyBwd = (v) => (v1) => {
  if (v.tag === "Str" && v1.tag === "VarKey") {
    return $DictEntry("VarKey", v._1, v1._2);
  }
  return throwException(error("absurd"))();
};
var toClausesStateFwd = (v) => listMap((v1) => $Tuple(
  $List("Cons", $Either("Left", v1._1._1), Nil),
  $Tuple(v1._1._2, v1._2)
))($List("Cons", v._1, v._2));
var toClausesStateBwd = (v) => {
  if (v.tag === "Nil") {
    return throwException(error(throwException(error("Shape mismatch"))()))();
  }
  if (v.tag === "Cons") {
    return $NonEmpty(
      v._1._1.tag === "Cons" && v._1._1._1.tag === "Left" && v._1._1._2.tag === "Nil" ? $Tuple($NonEmpty(v._1._1._1._1, v._1._2._1), v._1._2._2) : throwException(error(throwException(error("Shape mismatch"))()))(),
      listMap((v1) => {
        if (v1._1.tag === "Cons" && v1._1._1.tag === "Left" && v1._1._2.tag === "Nil") {
          return $Tuple($NonEmpty(v1._1._1._1, v1._2._1), v1._2._2);
        }
        return throwException(error(throwException(error("Shape mismatch"))()))();
      })(v._2)
    );
  }
  fail();
};
var subpatts = (v) => {
  if (v.tag === "Left") {
    if (v._1.tag === "PVar") {
      return Nil;
    }
    if (v._1.tag === "PConstr") {
      return listMap(Left)(v._1._2);
    }
    if (v._1.tag === "PRecord") {
      return listMap(Left)(listMap(snd)(v._1._1));
    }
    if (v._1.tag === "PListEmpty") {
      return Nil;
    }
    if (v._1.tag === "PListNonEmpty") {
      return $List("Cons", $Either("Left", v._1._1), $List("Cons", $Either("Right", v._1._2), Nil));
    }
    fail();
  }
  if (v.tag === "Right") {
    if (v._1.tag === "PListVar") {
      return Nil;
    }
    if (v._1.tag === "PListEnd") {
      return Nil;
    }
    if (v._1.tag === "PListNext") {
      return $List("Cons", $Either("Left", v._1._1), $List("Cons", $Either("Right", v._1._2), Nil));
    }
  }
  fail();
};
var showPattern = (v) => {
  if (v.tag === "Left") {
    return showPattern1.show(v._1);
  }
  if (v.tag === "Right") {
    return showListRestPattern.show(v._1);
  }
  fail();
};
var popVarFwd = (dictMonadError) => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  return (v) => (v1) => {
    if (v1.tag === "Cons") {
      if (v1._1._1.tag === "Cons" && v1._1._1._1.tag === "Left" && v1._1._1._1._1.tag === "PVar") {
        const $0 = v1._1._2._2;
        const $1 = v1._1._1._2;
        const $2 = v1._1._2._1;
        return Monad0.Bind1().Apply0().Functor0().map((v2) => $List("Cons", $Tuple($1, $Tuple($2, $0)), v2))(popVarFwd(dictMonadError)(mustEq(eqString)(showString)(v)(v1._1._1._1._1._1))(v1._2));
      }
      return MonadThrow0.throwError(error(throwException(error("Shape mismatch"))()));
    }
    if (v1.tag === "Nil") {
      return Monad0.Applicative0().pure(Nil);
    }
    return MonadThrow0.throwError(error(throwException(error("Shape mismatch"))()));
  };
};
var popVarFwd1 = /* @__PURE__ */ popVarFwd(monadErrorExceptT2);
var popVarBwd = (v) => (v1) => {
  if (v1.tag === "Cons") {
    return $List(
      "Cons",
      $Tuple($List("Cons", $Either("Left", $Pattern("PVar", v)), v1._1._1), $Tuple(v1._1._2._1, v1._1._2._2)),
      popVarBwd(v)(v1._2)
    );
  }
  if (v1.tag === "Nil") {
    return Nil;
  }
  fail();
};
var popRecordFwd = (dictMonadError) => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  return (v) => (v1) => {
    if (v1.tag === "Cons") {
      if (v1._1._1.tag === "Cons" && v1._1._1._1.tag === "Left" && v1._1._1._1._1.tag === "PRecord") {
        const $0 = v1._1._2._2;
        const $1 = v1._1._1._1._1._1;
        const $2 = v1._1._1._2;
        const $3 = v1._1._2._1;
        return assertWith("")((() => {
          const go = (v$1) => (v1$1) => (v2) => {
            if (!v2) {
              return false;
            }
            if (v$1.tag === "Nil") {
              return v1$1.tag === "Nil" && v2;
            }
            return v$1.tag === "Cons" && v1$1.tag === "Cons" && go(v$1._2)(v1$1._2)(v2 && v1$1._1 === v$1._1);
          };
          return go(listMap(fst)($1))(v)(true);
        })())(Monad0.Bind1().Apply0().Functor0().map((v2) => $List(
          "Cons",
          $Tuple(
            foldableList.foldr(Cons)($2)(listMap((x) => $Either("Left", x._2))($1)),
            $Tuple($3, $0)
          ),
          v2
        ))(popRecordFwd(dictMonadError)(v)(v1._2)));
      }
      return MonadThrow0.throwError(error(throwException(error("Shape mismatch"))()));
    }
    if (v1.tag === "Nil") {
      return Monad0.Applicative0().pure(Nil);
    }
    return MonadThrow0.throwError(error(throwException(error("Shape mismatch"))()));
  };
};
var popRecordFwd1 = /* @__PURE__ */ popRecordFwd(monadErrorExceptT2);
var popRecordBwd = (v) => (v1) => {
  if (v1.tag === "Cons") {
    return $List(
      "Cons",
      $Tuple(
        $List(
          "Cons",
          $Either(
            "Left",
            $Pattern(
              "PRecord",
              zipWith(Tuple)(v)(listMap((v2) => {
                if (v2.tag === "Left") {
                  return v2._1;
                }
                fail();
              })(take2((() => {
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
                      go$a0 = 1 + b | 0;
                      go$a1 = v$1._2;
                      continue;
                    }
                    fail();
                  }
                  return go$r;
                };
                return go(0)(v);
              })())(v1._1._1)))
            )
          ),
          drop2((() => {
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
                  go$a0 = 1 + b | 0;
                  go$a1 = v$1._2;
                  continue;
                }
                fail();
              }
              return go$r;
            };
            return go(0)(v);
          })())(v1._1._1)
        ),
        $Tuple(v1._1._2._1, v1._1._2._2)
      ),
      popRecordBwd(v)(v1._2)
    );
  }
  if (v1.tag === "Nil") {
    return Nil;
  }
  fail();
};
var popListVarFwd = (dictMonadError) => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  return (v) => (v1) => {
    if (v1.tag === "Cons") {
      if (v1._1._1.tag === "Cons" && v1._1._1._1.tag === "Right" && v1._1._1._1._1.tag === "PListVar") {
        const $0 = v1._1._2._2;
        const $1 = v1._1._1._2;
        const $2 = v1._1._2._1;
        return Monad0.Bind1().Apply0().Functor0().map((v2) => $List("Cons", $Tuple($1, $Tuple($2, $0)), v2))(popListVarFwd(dictMonadError)(mustEq(eqString)(showString)(v)(v1._1._1._1._1._1))(v1._2));
      }
      return MonadThrow0.throwError(error(throwException(error("Shape mismatch"))()));
    }
    if (v1.tag === "Nil") {
      return Monad0.Applicative0().pure(Nil);
    }
    return MonadThrow0.throwError(error(throwException(error("Shape mismatch"))()));
  };
};
var popListVarFwd1 = /* @__PURE__ */ popListVarFwd(monadErrorExceptT2);
var popListVarBwd = (v) => (v1) => {
  if (v1.tag === "Cons") {
    return $List(
      "Cons",
      $Tuple($List("Cons", $Either("Left", $Pattern("PVar", v)), v1._1._1), $Tuple(v1._1._2._1, v1._1._2._2)),
      popListVarBwd(v)(v1._2)
    );
  }
  if (v1.tag === "Nil") {
    return Nil;
  }
  fail();
};
var popArgFwd = (dictMonadError) => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  return (v) => {
    if (v.tag === "Cons") {
      if (v._1._1.tag === "Nil" && v._1._2._1.tag === "Cons") {
        const $0 = v._1._2._1._1;
        const $1 = v._1._2._2;
        const $2 = v._1._2._1._2;
        return Monad0.Bind1().Apply0().Functor0().map((v1) => $List(
          "Cons",
          $Tuple($List("Cons", $Either("Left", $0), Nil), $Tuple($2, $1)),
          v1
        ))(popArgFwd(dictMonadError)(v._2));
      }
      return MonadThrow0.throwError(error(throwException(error("Shape mismatch"))()));
    }
    if (v.tag === "Nil") {
      return Monad0.Applicative0().pure(Nil);
    }
    return MonadThrow0.throwError(error(throwException(error("Shape mismatch"))()));
  };
};
var popArgFwd1 = /* @__PURE__ */ popArgFwd(monadErrorExceptT2);
var popArgBwd = (v) => {
  if (v.tag === "Cons") {
    if (v._1._1.tag === "Cons" && v._1._1._1.tag === "Left" && v._1._1._2.tag === "Nil") {
      return $List(
        "Cons",
        $Tuple(Nil, $Tuple($List("Cons", v._1._1._1._1, v._1._2._1), v._1._2._2)),
        popArgBwd(v._2)
      );
    }
    return throwException(error("absurd"))();
  }
  if (v.tag === "Nil") {
    return Nil;
  }
  return throwException(error("absurd"))();
};
var unless = (v) => {
  if (v.tag === "Left") {
    if (v._1.tag === "PVar") {
      return Nil;
    }
    if (v._1.tag === "PRecord") {
      return Nil;
    }
    if (v._1.tag === "PConstr") {
      return listMap((c$p) => $Either(
        "Left",
        $Pattern("PConstr", c$p, replicate(unfoldableList)(defined(arity(monadThrowExceptT2)(c$p)))($Pattern("PVar", "_")))
      ))(difference3(toUnfoldable7(fromFoldable12(mapObjectString.keys(defined(dataTypeForCtr.dataTypeFor(monadThrowExceptT2)(v._1._1))._2))))($List(
        "Cons",
        v._1._1,
        Nil
      )));
    }
    if (v._1.tag === "PListEmpty") {
      return $List(
        "Cons",
        $Either("Left", $Pattern("PConstr", ":", replicate(unfoldableList)(2)($Pattern("PVar", "_")))),
        Nil
      );
    }
    if (v._1.tag === "PListNonEmpty") {
      return $List("Cons", $Either("Left", PListEmpty), Nil);
    }
    fail();
  }
  if (v.tag === "Right") {
    if (v._1.tag === "PListVar") {
      return Nil;
    }
    if (v._1.tag === "PListNext") {
      return $List("Cons", $Either("Right", PListEnd), Nil);
    }
    if (v._1.tag === "PListEnd") {
      return $List(
        "Cons",
        $Either("Right", $ListRestPattern("PListNext", $Pattern("PVar", "_"), $ListRestPattern("PListVar", "_"))),
        Nil
      );
    }
  }
  fail();
};
var forConstrFwd = (v) => (v1) => (v2) => {
  if (v2.tag === "Nil") {
    return $List("Cons", $Tuple(v, $List("Cons", v1, Nil)), Nil);
  }
  if (v2.tag === "Cons") {
    if (v === v2._1._1) {
      return $List("Cons", $Tuple(v2._1._1, $List("Cons", v1, v2._1._2)), v2._2);
    }
    return $List("Cons", $Tuple(v2._1._1, v2._1._2), forConstrFwd(v)(v1)(v2._2));
  }
  fail();
};
var forConstrBwd = (v) => (v1) => {
  if (v1.tag === "Nil") {
    return Nothing;
  }
  if (v1.tag === "Cons") {
    if (v === v1._1._1) {
      if (v1._1._2.tag === "Nil") {
        return Nothing;
      }
      if (v1._1._2.tag === "Cons") {
        return $Maybe("Just", $Tuple(v1._1._2._1, $List("Cons", $Tuple(v1._1._1, v1._1._2._2), v1._2)));
      }
      fail();
    }
    const $0 = forConstrBwd(v)(v1._2);
    if ($0.tag === "Just") {
      return $Maybe("Just", $Tuple($0._1._1, $List("Cons", $Tuple(v1._1._1, v1._1._2), $0._1._2)));
    }
    return Nothing;
  }
  fail();
};
var elimBool = (\u03BA) => (\u03BA$p) => $Elim("ElimConstr", fromFoldable10([$Tuple("True", \u03BA), $Tuple("False", \u03BA$p)]));
var econs = (\u03B1) => (e) => (e$p) => $Expr("Constr", \u03B1, ":", $List("Cons", e, $List("Cons", e$p, Nil)));
var ctrFor = (v) => {
  if (v.tag === "Left") {
    if (v._1.tag === "PVar") {
      return Nothing;
    }
    if (v._1.tag === "PConstr") {
      return $Maybe("Just", v._1._1);
    }
    if (v._1.tag === "PRecord") {
      return Nothing;
    }
    if (v._1.tag === "PListEmpty") {
      return $Maybe("Just", "Nil");
    }
    if (v._1.tag === "PListNonEmpty") {
      return $Maybe("Just", ":");
    }
    fail();
  }
  if (v.tag === "Right") {
    if (v._1.tag === "PListVar") {
      return Nothing;
    }
    if (v._1.tag === "PListEnd") {
      return $Maybe("Just", "Nil");
    }
    if (v._1.tag === "PListNext") {
      return $Maybe("Just", ":");
    }
  }
  fail();
};
var popConstrBwd = (v) => (v1) => {
  if (v1.tag === "Cons") {
    if (v1._1._1.tag === "Nil") {
      return throwException(error("absurd"))();
    }
    if (v1._1._1.tag === "Cons") {
      const v2 = forConstrBwd(definitely("absurd")(ctrFor(v1._1._1._1)))(v);
      if (v2.tag === "Nothing") {
        return popConstrBwd(v)(v1._2);
      }
      if (v2.tag === "Just") {
        if (eqList.eq(v2._1._1._1)(foldableList.foldr(Cons)(v1._1._1._2)(subpatts(v1._1._1._1))) && eq8(v2._1._1._2._1)(v1._1._2._1)) {
          return $List(
            "Cons",
            $Tuple($List("Cons", v1._1._1._1, v1._1._1._2), $Tuple(v1._1._2._1, v2._1._1._2._2)),
            popConstrBwd(v2._1._2)(v1._2)
          );
        }
        return popConstrBwd(v)(v1._2);
      }
    }
    fail();
  }
  if (v1.tag === "Nil") {
    return Nil;
  }
  fail();
};
var popConstrFwd = (dictMonadError) => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  return (v) => (v1) => {
    if (v1.tag === "Cons") {
      if (v1._1._1.tag === "Nil") {
        return throwException(error("absurd"))();
      }
      if (v1._1._1.tag === "Cons") {
        const \u03C0 = subpatts(v1._1._1._1);
        const c = definitely("Failed to distinguish constructor: " + showPattern(v1._1._1._1))(ctrFor(v1._1._1._1));
        return assertWith("")((() => {
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
                go$a0 = 1 + b | 0;
                go$a1 = v$1._2;
                continue;
              }
              fail();
            }
            return go$r;
          };
          return go(0)(\u03C0) === defined(arity(monadThrowExceptT2)(c)) && defined(dataTypeForCtr.dataTypeFor(monadThrowExceptT2)(c))._1 === v._1;
        })())(Monad0.Bind1().Apply0().Functor0().map(forConstrFwd(c)($Tuple(
          foldableList.foldr(Cons)(v1._1._1._2)(\u03C0),
          $Tuple(v1._1._2._1, v1._1._2._2)
        )))(popConstrFwd(dictMonadError)(v)(v1._2)));
      }
      fail();
    }
    if (v1.tag === "Nil") {
      return Monad0.Applicative0().pure(Nil);
    }
    fail();
  };
};
var popConstrFwd1 = /* @__PURE__ */ popConstrFwd(monadErrorExceptT2);
var anon = (v) => {
  if (v.tag === "Left") {
    return $Either("Left", $Pattern("PVar", "_"));
  }
  if (v.tag === "Right") {
    return $Either("Right", $ListRestPattern("PListVar", "_"));
  }
  fail();
};
var orElseBwd = (dictBoundedJoinSemilattice) => {
  const bot = dictBoundedJoinSemilattice.bot;
  const $0 = dictBoundedJoinSemilattice.JoinSemilattice0();
  return (v) => (ks) => {
    if (v._1.tag === "Nil") {
      if (ks._1._1.tag === "Nil" && ks._2.tag === "Nil") {
        return $Tuple(bot, ks._1._2);
      }
      fail();
    }
    if (v._1.tag === "Cons") {
      const $1 = v._1._2;
      const popIfPresent = (v1) => (v2) => {
        if (v1.tag === "Nil") {
          return $Tuple(bot, v2);
        }
        const v3 = unsnoc3(v2);
        const v4 = unsnoc3(nonEmptyListNonEmptyList.nonEmpty(v1));
        if (!eqList.eq($List("Cons", v4.last, listMap(anon)($1)))(v3.last._1)) {
          return popIfPresent(v4.init)(v2);
        }
        const $22 = popIfPresent(v4.init)(nonEmptyListNonEmptyList.nonEmpty(v3.init));
        return $Tuple(
          $0.join($22._1)((() => {
            if (v3.last._2.tag === "ListEmpty") {
              return v3.last._2._1;
            }
            fail();
          })()),
          $22._2
        );
      };
      const $2 = popIfPresent(unless(v._1._1))(ks);
      const $3 = orElseBwd(dictBoundedJoinSemilattice)($Tuple(foldableList.foldr(Cons)($1)(subpatts(v._1._1)), v._2))($NonEmpty(
        (() => {
          if ($2._2._1._1.tag === "Cons") {
            return $Tuple(foldableList.foldr(Cons)($2._2._1._1._2)(subpatts($2._2._1._1._1)), $2._2._1._2);
          }
          fail();
        })(),
        listMap((v2) => {
          if (v2._1.tag === "Cons") {
            return $Tuple(foldableList.foldr(Cons)(v2._1._2)(subpatts(v2._1._1)), v2._2);
          }
          fail();
        })($2._2._2)
      ));
      return $Tuple($0.join($3._1)($2._1), $3._2);
    }
    fail();
  };
};
var orElseFwd = (\u03B1) => (v) => {
  if (v._1.tag === "Nil") {
    return $NonEmpty($Tuple(Nil, v._2), Nil);
  }
  if (v._1.tag === "Cons") {
    const $0 = v._1._2;
    const \u03C0$p = subpatts(v._1._1);
    const $1 = orElseFwd(\u03B1)($Tuple(foldableList.foldr(Cons)($0)(\u03C0$p), v._2));
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
          go$a0 = 1 + b | 0;
          go$a1 = v$1._2;
          continue;
        }
        fail();
      }
      return go$r;
    };
    const $2 = go(0)(\u03C0$p);
    const $3 = (v$1) => $Tuple(take2($2)(v$1._1), $Tuple(drop2($2)(v$1._1), v$1._2));
    const $4 = (() => {
      if (v._1._1.tag === "Left") {
        if (v._1._1._1.tag === "PVar") {
          const $42 = v._1._1._1._1;
          return (v1) => $Tuple($List("Cons", $Either("Left", $Pattern("PVar", $42)), v1._2._1), v1._2._2);
        }
        if (v._1._1._1.tag === "PRecord") {
          const $42 = v._1._1._1._1;
          return (v1) => $Tuple(
            $List(
              "Cons",
              $Either(
                "Left",
                $Pattern(
                  "PRecord",
                  zipWith(Tuple)(listMap(fst)($42))(listMap((v2) => {
                    if (v2.tag === "Left") {
                      return v2._1;
                    }
                    fail();
                  })(v1._1))
                )
              ),
              v1._2._1
            ),
            v1._2._2
          );
        }
        if (v._1._1._1.tag === "PConstr") {
          const $42 = v._1._1._1._1;
          return (v1) => $Tuple(
            $List(
              "Cons",
              $Either(
                "Left",
                $Pattern(
                  "PConstr",
                  $42,
                  listMap((v2) => {
                    if (v2.tag === "Left") {
                      return v2._1;
                    }
                    fail();
                  })(v1._1)
                )
              ),
              v1._2._1
            ),
            v1._2._2
          );
        }
        if (v._1._1._1.tag === "PListEmpty") {
          return (v1) => $Tuple($List("Cons", $Either("Left", PListEmpty), v1._2._1), v1._2._2);
        }
        if (v._1._1._1.tag === "PListNonEmpty") {
          return (v1) => {
            if (v1._1.tag === "Cons" && v1._1._1.tag === "Left" && v1._1._2.tag === "Cons" && v1._1._2._1.tag === "Right" && v1._1._2._2.tag === "Nil") {
              return $Tuple($List("Cons", $Either("Left", $Pattern("PListNonEmpty", v1._1._1._1, v1._1._2._1._1)), v1._2._1), v1._2._2);
            }
            fail();
          };
        }
        fail();
      }
      if (v._1._1.tag === "Right") {
        if (v._1._1._1.tag === "PListVar") {
          const $42 = v._1._1._1._1;
          return (v1) => $Tuple($List("Cons", $Either("Right", $ListRestPattern("PListVar", $42)), v1._2._1), v1._2._2);
        }
        if (v._1._1._1.tag === "PListNext") {
          return (v1) => {
            if (v1._1.tag === "Cons" && v1._1._1.tag === "Left" && v1._1._2.tag === "Cons" && v1._1._2._1.tag === "Right" && v1._1._2._2.tag === "Nil") {
              return $Tuple(
                $List("Cons", $Either("Right", $ListRestPattern("PListNext", v1._1._1._1, v1._1._2._1._1)), v1._2._1),
                v1._2._2
              );
            }
            fail();
          };
        }
        if (v._1._1._1.tag === "PListEnd") {
          return (v1) => $Tuple($List("Cons", $Either("Right", PListEnd), v1._2._1), v1._2._2);
        }
      }
      fail();
    })();
    return $NonEmpty(
      $4($3($1._1)),
      foldableList.foldr(Cons)(listMap((p$p) => $Tuple(
        $List("Cons", p$p, listMap(anon)($0)),
        $Expr2("ListEmpty", \u03B1)
      ))(unless(v._1._1)))(listMap($4)(listMap(($5) => $3($5))($1._2)))
    );
  }
  fail();
};
var desugarableListRestExpr = {
  desug: (dictMonadError) => (dictBoundedLattice) => {
    const Monad0 = dictMonadError.MonadThrow0().Monad0();
    const Apply0 = Monad0.Bind1().Apply0();
    return (v) => {
      if (v.tag === "End") {
        return Monad0.Applicative0().pure($Expr("Constr", v._1, "Nil", Nil));
      }
      if (v.tag === "Next") {
        return Apply0.apply(Apply0.Functor0().map(econs(v._1))(desugarableExprExpr.desug(dictMonadError)(dictBoundedLattice)(v._2)))(desugarableListRestExpr.desug(dictMonadError)(dictBoundedLattice)(v._3));
      }
      fail();
    };
  },
  desugBwd: (dictBoundedJoinSemilattice) => (v) => (v1) => {
    if (v.tag === "Constr") {
      if (v1.tag === "End") {
        return $ListRest("End", v._1);
      }
      if (v._3.tag === "Cons" && v._3._2.tag === "Cons" && v._3._2._2.tag === "Nil" && v1.tag === "Next") {
        return $ListRest(
          "Next",
          v._1,
          desugarableExprExpr.desugBwd(dictBoundedJoinSemilattice)(v._3._1)(v1._2),
          desugarableListRestExpr.desugBwd(dictBoundedJoinSemilattice)(v._3._2._1)(v1._3)
        );
      }
    }
    return throwException(error("absurd"))();
  },
  Functor0: () => functorListRest,
  Functor1: () => functorExpr
};
var desugarableExprExpr = {
  desug: (dictMonadError) => (dictBoundedLattice) => exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0()),
  desugBwd: (dictBoundedJoinSemilattice) => exprBwd(dictBoundedJoinSemilattice),
  Functor0: () => functorExpr2,
  Functor1: () => functorExpr
};
var desugarableDictEntryExpr = {
  desug: (dictMonadError) => (dictBoundedLattice) => (v) => {
    if (v.tag === "ExprKey") {
      return exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1);
    }
    if (v.tag === "VarKey") {
      return dictMonadError.MonadThrow0().Monad0().Applicative0().pure($Expr("Str", v._1, v._2));
    }
    fail();
  },
  desugBwd: (dictBoundedJoinSemilattice) => (v) => (v1) => {
    if (v1.tag === "ExprKey") {
      return $DictEntry("ExprKey", exprBwd(dictBoundedJoinSemilattice)(v)(v1._1));
    }
    return varKeyBwd(v)(v1);
  },
  Functor0: () => functorDictEntry,
  Functor1: () => functorExpr
};
var desugarableClausesElim = {
  desug: (dictMonadError) => (dictBoundedLattice) => {
    const $0 = dictMonadError.MonadThrow0().Monad0().Bind1().Apply0().Functor0();
    return (\u03BC) => $0.map(asElim)(clausesStateFwd(dictBoundedLattice)(dictMonadError)(toClausesStateFwd(\u03BC)));
  },
  desugBwd: (dictBoundedJoinSemilattice) => (\u03C3) => (\u03BC) => toClausesStateBwd(clausesStateBwd(dictBoundedJoinSemilattice)($Cont("ContElim", \u03C3))(toClausesStateFwd(\u03BC))),
  Functor0: () => functorClauses,
  Functor1: () => functorElim
};
var varDefsFwd = (dictMonadError) => {
  const Apply0 = dictMonadError.MonadThrow0().Monad0().Bind1().Apply0();
  const $0 = Apply0.Functor0();
  return (dictBoundedLattice) => (v) => {
    if (v._1._2.tag === "Nil") {
      return Apply0.apply($0.map(Let)(varDefFwd(dictMonadError)(dictBoundedLattice)(v._1._1)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2));
    }
    if (v._1._2.tag === "Cons") {
      return Apply0.apply($0.map(Let)(varDefFwd(dictMonadError)(dictBoundedLattice)(v._1._1)))(varDefsFwd(dictMonadError)(dictBoundedLattice)($Tuple(
        $NonEmpty(v._1._2._1, v._1._2._2),
        v._2
      )));
    }
    fail();
  };
};
var varDefsBwd = (dictBoundedJoinSemilattice) => (v) => (v1) => {
  if (v.tag === "Let") {
    if (v1._1._2.tag === "Nil") {
      return $Tuple(
        $NonEmpty($VarDef2(v1._1._1._1, exprBwd(dictBoundedJoinSemilattice)(v._1._2)(v1._1._1._2)), Nil),
        exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2)
      );
    }
    if (v1._1._2.tag === "Cons") {
      const v2 = varDefsBwd(dictBoundedJoinSemilattice)(v._2)($Tuple($NonEmpty(v1._1._2._1, v1._1._2._2), v1._2));
      return $Tuple(
        $NonEmpty($VarDef2(v1._1._1._1, exprBwd(dictBoundedJoinSemilattice)(v._1._2)(v1._1._1._2)), $List("Cons", v2._1._1, v2._1._2)),
        v2._2
      );
    }
  }
  return throwException(error("absurd"))();
};
var varDefFwd = (dictMonadError) => {
  const Apply0 = dictMonadError.MonadThrow0().Monad0().Bind1().Apply0();
  return (dictBoundedLattice) => {
    const top = dictBoundedLattice.BoundedMeetSemilattice1().top;
    return (v) => Apply0.apply(Apply0.Functor0().map(VarDef)(desugarableClausesElim.desug(dictMonadError)(dictBoundedLattice)($NonEmpty(
      $Tuple($NonEmpty(v._1, Nil), $Expr2("Dictionary", top, Nil)),
      Nil
    ))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2));
  };
};
var recDefsFwd = (dictMonadError) => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  const traverse2 = traversableNonEmptyList.traverse(Monad0.Applicative0());
  return (dictBoundedLattice) => {
    const top = dictBoundedLattice.BoundedMeetSemilattice1().top;
    return (xcs) => Monad0.Bind1().Apply0().Functor0().map((() => {
      const $0 = RecDefs(top);
      return (x) => $0(fromFoldable16(x));
    })())(traverse2(recDefFwd(dictMonadError)(dictBoundedLattice))((() => {
      const $0 = wrappedOperation("groupBy")(groupBy((x) => (y) => x._1 === y._1))(xcs);
      return $NonEmpty($0._1, listMap(RecDef)($0._2));
    })()));
  };
};
var recDefsBwd = (dictBoundedJoinSemilattice) => (v) => (xcs) => {
  const $0 = v._2;
  const go = (v1) => $NonEmpty(
    recDefBwd(dictBoundedJoinSemilattice)($Tuple(v1._1._1._1, $$get(showString)(mapDictString)(v1._1._1._1)($0)))(v1._1),
    (() => {
      if (v1._2.tag === "Nil") {
        return Nil;
      }
      if (v1._2.tag === "Cons") {
        const $1 = go($NonEmpty(v1._2._1, v1._2._2));
        return $List("Cons", $1._1, $1._2);
      }
      fail();
    })()
  );
  return bindNonEmptyList.bind(go(wrappedOperation("groupBy")(groupBy((x) => (y) => x._1 === y._1))(xcs)))(identity);
};
var recDefFwd = (dictMonadError) => (dictBoundedLattice) => (xcs) => dictMonadError.MonadThrow0().Monad0().Bind1().Apply0().Functor0().map((v) => $Tuple(xcs._1._1, v))(desugarableClausesElim.desug(dictMonadError)(dictBoundedLattice)($NonEmpty(
  xcs._1._2,
  listMap(snd)(xcs._2)
)));
var recDefBwd = (dictBoundedJoinSemilattice) => (v) => (v1) => {
  const $0 = v._1;
  const $1 = toClausesStateBwd(clausesStateBwd(dictBoundedJoinSemilattice)($Cont("ContElim", v._2))(toClausesStateFwd($NonEmpty(
    v1._1._2,
    listMap(snd)(v1._2)
  ))));
  return $NonEmpty($Tuple($0, $1._1), listMap((v2) => $Tuple($0, v2))($1._2));
};
var listCompFwd = (dictMonadError) => {
  const Bind1 = dictMonadError.MonadThrow0().Monad0().Bind1();
  const Functor0 = Bind1.Apply0().Functor0();
  return (dictBoundedLattice) => (v) => {
    if (v._2._1.tag === "Nil") {
      return Functor0.map((f) => f($Expr("Constr", v._1, "Nil", Nil)))(Functor0.map(econs(v._1))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2._2)));
    }
    if (v._2._1.tag === "Cons") {
      if (v._2._1._1.tag === "ListCompGuard") {
        const $0 = v._2._1._1._1;
        const $1 = v._1;
        return Bind1.bind(listCompFwd(dictMonadError)(dictBoundedLattice)($Tuple($1, $Tuple(v._2._1._2, v._2._2))))((e) => Functor0.map(App2($Expr(
          "Lambda",
          $1,
          $Elim(
            "ElimConstr",
            fromFoldable10([
              $Tuple("True", $Cont("ContExpr", e)),
              $Tuple("False", $Cont("ContExpr", $Expr("Constr", $1, "Nil", Nil)))
            ])
          )
        )))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($0)));
      }
      if (v._2._1._1.tag === "ListCompDecl") {
        const $0 = v._2._1._1._1._2;
        const $1 = v._1;
        return Bind1.bind(clausesStateFwd(dictBoundedLattice)(dictMonadError)($List(
          "Cons",
          $Tuple(
            $List("Cons", $Either("Left", v._2._1._1._1._1), Nil),
            $Tuple(Nil, $Expr2("ListComp", $1, v._2._2, v._2._1._2))
          ),
          Nil
        )))((\u03C3) => Functor0.map(App2($Expr("Lambda", $1, \u03C3.tag === "ContElim" ? \u03C3._1 : throwException(error("Eliminator expected"))())))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($0)));
      }
      if (v._2._1._1.tag === "ListCompGen") {
        const $0 = v._2._1._1._2;
        const $1 = v._1;
        return Bind1.bind(clausesStateFwd(dictBoundedLattice)(dictMonadError)((() => {
          const $2 = orElseFwd($1)($Tuple(
            $List("Cons", $Either("Left", v._2._1._1._1), Nil),
            $Expr2("ListComp", $1, v._2._2, v._2._1._2)
          ));
          return $List(
            "Cons",
            $Tuple($2._1._1, $Tuple(Nil, $2._1._2)),
            listMap((m) => $Tuple(m._1, $Tuple(Nil, m._2)))($2._2)
          );
        })()))((\u03C3) => Functor0.map(App2($Expr(
          "App",
          $Expr("Var", "concatMap"),
          $Expr("Lambda", $1, \u03C3.tag === "ContElim" ? \u03C3._1 : throwException(error("Eliminator expected"))())
        )))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($0)));
      }
    }
    fail();
  };
};
var listCompBwd = (dictBoundedJoinSemilattice) => {
  const $0 = dictBoundedJoinSemilattice.JoinSemilattice0();
  const orElseBwd1 = orElseBwd(dictBoundedJoinSemilattice);
  return (v) => (v1) => {
    const $1 = (e, p, qs, s0, s0$p, \u03B1$p, \u03C3) => {
      const $12 = clausesStateBwd(dictBoundedJoinSemilattice)($Cont("ContElim", \u03C3))($List(
        "Cons",
        $Tuple(
          $List("Cons", $Either("Left", p), Nil),
          $Tuple(Nil, $Expr2("ListComp", void 0, s0$p, qs))
        ),
        Nil
      ));
      if ($12.tag === "Cons" && $12._1._1.tag === "Cons" && $12._1._1._1.tag === "Left" && $12._1._1._2.tag === "Nil" && $12._1._2._1.tag === "Nil" && $12._1._2._2.tag === "ListComp" && $12._2.tag === "Nil") {
        return $Tuple(
          $0.join($12._1._2._2._1)(\u03B1$p),
          $Tuple($List("Cons", $Qualifier("ListCompDecl", $VarDef2(p, exprBwd(dictBoundedJoinSemilattice)(e)(s0))), $12._1._2._2._3), $12._1._2._2._2)
        );
      }
      fail();
    };
    if (v.tag === "Constr") {
      if (v._3.tag === "Cons" && v._3._2.tag === "Cons" && v._3._2._1.tag === "Constr" && v._3._2._1._3.tag === "Nil" && v._3._2._2.tag === "Nil" && v1._1.tag === "Nil" && v._2 === ":" && v._3._2._1._2 === "Nil") {
        return $Tuple($0.join(v._3._2._1._1)(v._1), $Tuple(Nil, exprBwd(dictBoundedJoinSemilattice)(v._3._1)(v1._2)));
      }
      return throwException(error("absurd"))();
    }
    if (v.tag === "App" && v1._1.tag === "Cons") {
      if (v._1.tag === "Lambda") {
        if (v._1._2.tag === "ElimConstr" && v1._1._1.tag === "ListCompGuard") {
          const $2 = listCompBwd(dictBoundedJoinSemilattice)((() => {
            const $22 = $$get(showString)(mapDictString)("True")(v._1._2._1);
            if ($22.tag === "ContExpr") {
              return $22._1;
            }
            return throwException(error("Expression expected"))();
          })())($Tuple(v1._1._2, v1._2));
          const $3 = $$get(showString)(mapDictString)("False")(v._1._2._1);
          const $4 = $3.tag === "ContExpr" ? $3._1 : throwException(error("Expression expected"))();
          if ($4.tag === "Constr" && $4._3.tag === "Nil" && $4._2 === "Nil") {
            return $Tuple(
              $0.join($0.join($2._1)(v._1._1))($4._1),
              $Tuple($List("Cons", $Qualifier("ListCompGuard", exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._1._1._1)), $2._2._1), $2._2._2)
            );
          }
          fail();
        }
        if (v1._1._1.tag === "ListCompDecl") {
          return $1(v._2, v1._1._1._1._1, v1._1._2, v1._1._1._1._2, v1._2, v._1._1, v._1._2);
        }
        return throwException(error("absurd"))();
      }
      if (v._1.tag === "App" && v._1._1.tag === "Var" && v._1._1._1 === "concatMap" && v._1._2.tag === "Lambda" && v1._1._1.tag === "ListCompGen") {
        const $2 = orElseBwd1($Tuple(
          $List("Cons", $Either("Left", v1._1._1._1), Nil),
          $Expr2("ListComp", void 0, v1._2, v1._1._2)
        ))((() => {
          const $22 = nonEmptyListNonEmptyList.nonEmpty(clausesStateBwd(dictBoundedJoinSemilattice)($Cont("ContElim", v._1._2._2))((() => {
            const $23 = orElseFwd()($Tuple(
              $List("Cons", $Either("Left", v1._1._1._1), Nil),
              $Expr2("ListComp", void 0, v1._2, v1._1._2)
            ));
            return $List(
              "Cons",
              $Tuple($23._1._1, $Tuple(Nil, $23._1._2)),
              listMap((m) => $Tuple(m._1, $Tuple(Nil, m._2)))($23._2)
            );
          })()));
          return $NonEmpty(
            (() => {
              if ($22._1._2._1.tag === "Nil") {
                return $Tuple($22._1._1, $22._1._2._2);
              }
              fail();
            })(),
            listMap((v2) => {
              if (v2._2._1.tag === "Nil") {
                return $Tuple(v2._1, v2._2._2);
              }
              fail();
            })($22._2)
          );
        })());
        if ($2._2.tag === "ListComp") {
          return $Tuple(
            $0.join($0.join($2._2._1)(v._1._2._1))($2._1),
            $Tuple($List("Cons", $Qualifier("ListCompGen", v1._1._1._1, exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._1._1._2)), $2._2._3), $2._2._2)
          );
        }
        fail();
      }
    }
    return throwException(error("absurd"))();
  };
};
var exprFwd = (dictBoundedLattice) => {
  const top = dictBoundedLattice.BoundedMeetSemilattice1().top;
  return (dictMonadError) => {
    const Monad0 = dictMonadError.MonadThrow0().Monad0();
    const Applicative0 = Monad0.Applicative0();
    const Bind1 = Monad0.Bind1();
    const Apply0 = Bind1.Apply0();
    const Functor0 = Apply0.Functor0();
    const traverse2 = traversableList.traverse(Applicative0);
    return (dictJoinSemilattice) => (v) => {
      if (v.tag === "Var") {
        return Applicative0.pure($Expr("Var", v._1));
      }
      if (v.tag === "Op") {
        return Applicative0.pure($Expr("Op", v._1));
      }
      if (v.tag === "Int") {
        return Applicative0.pure($Expr("Int", v._1, v._2));
      }
      if (v.tag === "Float") {
        return Applicative0.pure($Expr("Float", v._1, v._2));
      }
      if (v.tag === "Str") {
        return Applicative0.pure($Expr("Str", v._1, v._2));
      }
      if (v.tag === "Constr") {
        return Functor0.map(Constr(v._1)(v._2))(traverse2(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0()))(v._3));
      }
      if (v.tag === "Dictionary") {
        const $0 = v._1;
        const v1 = unzip(v._2);
        const $1 = v1._2;
        return Bind1.bind(traverse2(desugarableDictEntryExpr.desug(dictMonadError)(dictBoundedLattice))(v1._1))((ks$p) => Bind1.bind(traverse2(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0()))($1))((es) => Functor0.map(Dictionary($0))(Applicative0.pure(zipWith((k) => (v2) => $Pair(
          k,
          v2
        ))(ks$p)(es)))));
      }
      if (v.tag === "Matrix") {
        return Apply0.apply(Functor0.map((f) => f($Tuple(v._3._1, v._3._2)))(Functor0.map(Matrix(v._1))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._4));
      }
      if (v.tag === "Lambda") {
        return Functor0.map(Lambda(top))(desugarableClausesElim.desug(dictMonadError)(dictBoundedLattice)(v._1));
      }
      if (v.tag === "Project") {
        const $0 = v._2;
        return Functor0.map((f) => f($0))(Functor0.map(Project)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1)));
      }
      if (v.tag === "DProject") {
        return Apply0.apply(Functor0.map(DProject)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2));
      }
      if (v.tag === "App") {
        return Apply0.apply(Functor0.map(App2)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2));
      }
      if (v.tag === "BinaryApp") {
        return Apply0.apply(Functor0.map(App2)(Functor0.map(App2($Expr("Op", v._2)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._3));
      }
      if (v.tag === "MatchAs") {
        return Apply0.apply(Functor0.map(App2)(Functor0.map(Lambda(top))(desugarableClausesElim.desug(dictMonadError)(dictBoundedLattice)($NonEmpty(
          $Tuple($NonEmpty(v._2._1._1, Nil), v._2._1._2),
          listMap((x) => $Tuple($NonEmpty(x._1, Nil), x._2))(v._2._2)
        )))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1));
      }
      if (v.tag === "IfElse") {
        return Apply0.apply(Functor0.map(App2)(Functor0.map(Lambda(top))(Apply0.apply(Functor0.map(elimBool)(Functor0.map(ContExpr)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2))))(Functor0.map(ContExpr)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._3))))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1));
      }
      if (v.tag === "ListEmpty") {
        return Applicative0.pure($Expr("Constr", v._1, "Nil", Nil));
      }
      if (v.tag === "ListNonEmpty") {
        return Apply0.apply(Functor0.map(econs(v._1))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2)))(desugarableListRestExpr.desug(dictMonadError)(dictBoundedLattice)(v._3));
      }
      if (v.tag === "ListEnum") {
        return Apply0.apply(Functor0.map(App2)(Functor0.map(App2($Expr("Var", "enumFromTo")))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2));
      }
      if (v.tag === "ListComp") {
        return listCompFwd(dictMonadError)(dictBoundedLattice)($Tuple(v._1, $Tuple(v._3, v._2)));
      }
      if (v.tag === "Let") {
        return varDefsFwd(dictMonadError)(dictBoundedLattice)($Tuple(v._1, v._2));
      }
      if (v.tag === "LetRec") {
        return Apply0.apply(Functor0.map(LetRec)(recDefsFwd(dictMonadError)(dictBoundedLattice)(v._1)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2));
      }
      fail();
    };
  };
};
var exprBwd = (dictBoundedJoinSemilattice) => {
  const $0 = functorExpr2.map((() => {
    const $02 = dictBoundedJoinSemilattice.bot;
    return (v) => $02;
  })());
  return (v) => (v1) => {
    const $1 = (e, qs, s) => {
      const v2 = listCompBwd(dictBoundedJoinSemilattice)(e)($Tuple(qs, s));
      return $Expr2("ListComp", v2._1, v2._2._2, v2._2._1);
    };
    if (v.tag === "Var") {
      if (v1.tag === "Var") {
        return $Expr2("Var", v1._1);
      }
      if (v1.tag === "ListComp") {
        return $1(v, v1._3, v1._2);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Op") {
      if (v1.tag === "Op") {
        return $Expr2("Op", v1._1);
      }
      if (v1.tag === "ListComp") {
        return $1(v, v1._3, v1._2);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Int") {
      if (v1.tag === "Int") {
        return $Expr2("Int", v._1, v1._2);
      }
      if (v1.tag === "ListComp") {
        return $1(v, v1._3, v1._2);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Float") {
      if (v1.tag === "Float") {
        return $Expr2("Float", v._1, v1._2);
      }
      if (v1.tag === "ListComp") {
        return $1(v, v1._3, v1._2);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Str") {
      if (v1.tag === "Str") {
        return $Expr2("Str", v._1, v1._2);
      }
      if (v1.tag === "ListComp") {
        return $1(v, v1._3, v1._2);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Constr") {
      if (v1.tag === "Constr") {
        return $Expr2(
          "Constr",
          v._1,
          v1._2,
          listMap((() => {
            const $2 = exprBwd(dictBoundedJoinSemilattice);
            return (v$1) => $2(v$1._1)(v$1._2);
          })())(zipWith(Tuple)(v._3)(v1._3))
        );
      }
      if (v._3.tag === "Nil") {
        if (v1.tag === "ListEmpty") {
          return $Expr2("ListEmpty", v._1);
        }
        if (v1.tag === "ListComp") {
          return $1(v, v1._3, v1._2);
        }
        return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
      }
      if (v._3.tag === "Cons" && v._3._2.tag === "Cons" && v._3._2._2.tag === "Nil" && v1.tag === "ListNonEmpty") {
        return $Expr2("ListNonEmpty", v._1, exprBwd(dictBoundedJoinSemilattice)(v._3._1)(v1._2), desugarableListRestExpr.desugBwd(dictBoundedJoinSemilattice)(v._3._2._1)(v1._3));
      }
      if (v1.tag === "ListComp") {
        return $1(v, v1._3, v1._2);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Dictionary") {
      if (v1.tag === "Dictionary") {
        return $Expr2(
          "Dictionary",
          v._1,
          zipWith((v2) => {
            const $2 = v2._1;
            const $3 = v2._2;
            return (v3) => $Tuple(desugarableDictEntryExpr.desugBwd(dictBoundedJoinSemilattice)($2)(v3._1), exprBwd(dictBoundedJoinSemilattice)($3)(v3._2));
          })(v._2)(v1._2)
        );
      }
      if (v1.tag === "ListComp") {
        return $1(v, v1._3, v1._2);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Matrix") {
      if (v1.tag === "Matrix") {
        return $Expr2("Matrix", v._1, exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2), $Tuple(v1._3._1, v1._3._2), exprBwd(dictBoundedJoinSemilattice)(v._4)(v1._4));
      }
      if (v1.tag === "ListComp") {
        return $1(v, v1._3, v1._2);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Lambda") {
      if (v1.tag === "Lambda") {
        return $Expr2("Lambda", toClausesStateBwd(clausesStateBwd(dictBoundedJoinSemilattice)($Cont("ContElim", v._2))(toClausesStateFwd(v1._1))));
      }
      if (v1.tag === "ListComp") {
        return $1(v, v1._3, v1._2);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Project") {
      if (v1.tag === "Project") {
        return $Expr2("Project", exprBwd(dictBoundedJoinSemilattice)(v._1)(v1._1), v._2);
      }
      if (v1.tag === "ListComp") {
        return $1(v, v1._3, v1._2);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "App") {
      if (v1.tag === "App") {
        return $Expr2("App", exprBwd(dictBoundedJoinSemilattice)(v._1)(v1._1), exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2));
      }
      if (v._1.tag === "App") {
        if (v._1._1.tag === "Op") {
          if (v1.tag === "BinaryApp") {
            return $Expr2("BinaryApp", exprBwd(dictBoundedJoinSemilattice)(v._1._2)(v1._1), v1._2, exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._3));
          }
          if (v1.tag === "ListComp") {
            return $1(v, v1._3, v1._2);
          }
          return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
        }
        if (v._1._1.tag === "Var" && v._1._1._1 === "enumFromTo" && v1.tag === "ListEnum") {
          return $Expr2("ListEnum", exprBwd(dictBoundedJoinSemilattice)(v._1._2)(v1._1), exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2));
        }
        if (v1.tag === "ListComp") {
          return $1(v, v1._3, v1._2);
        }
        return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
      }
      if (v._1.tag === "Lambda") {
        if (v1.tag === "MatchAs") {
          return $Expr2(
            "MatchAs",
            exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._1),
            (() => {
              const $2 = toClausesStateBwd(clausesStateBwd(dictBoundedJoinSemilattice)($Cont("ContElim", v._1._2))(toClausesStateFwd($NonEmpty(
                $Tuple($NonEmpty(v1._2._1._1, Nil), v1._2._1._2),
                listMap((x) => $Tuple($NonEmpty(x._1, Nil), x._2))(v1._2._2)
              ))));
              return $NonEmpty($Tuple($2._1._1._1, $2._1._2), listMap((x) => $Tuple(x._1._1, x._2))($2._2));
            })()
          );
        }
        if (v._1._2.tag === "ElimConstr" && v1.tag === "IfElse") {
          return $Expr2(
            "IfElse",
            exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._1),
            Object.hasOwn(v._1._2._1, "True") ? exprBwd(dictBoundedJoinSemilattice)((() => {
              const $2 = $$get(showString)(mapDictString)("True")(v._1._2._1);
              if ($2.tag === "ContExpr") {
                return $2._1;
              }
              return throwException(error("Expression expected"))();
            })())(v1._2) : $0(v1._2),
            Object.hasOwn(v._1._2._1, "False") ? exprBwd(dictBoundedJoinSemilattice)((() => {
              const $2 = $$get(showString)(mapDictString)("False")(v._1._2._1);
              if ($2.tag === "ContExpr") {
                return $2._1;
              }
              return throwException(error("Expression expected"))();
            })())(v1._3) : $0(v1._3)
          );
        }
      }
      if (v1.tag === "ListComp") {
        return $1(v, v1._3, v1._2);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v1.tag === "ListComp") {
      return $1(v, v1._3, v1._2);
    }
    if (v.tag === "Let") {
      if (v1.tag === "Let") {
        const $2 = varDefsBwd(dictBoundedJoinSemilattice)($Expr("Let", v._1, v._2))($Tuple(v1._1, v1._2));
        return $Expr2("Let", $2._1, $2._2);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "LetRec") {
      if (v1.tag === "LetRec") {
        return $Expr2("LetRec", recDefsBwd(dictBoundedJoinSemilattice)(v._1)(v1._1), exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2));
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "DProject" && v1.tag === "DProject") {
      return $Expr2("DProject", exprBwd(dictBoundedJoinSemilattice)(v._1)(v1._1), exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2));
    }
    return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
  };
};
var clausesStateFwd = (dictBoundedLattice) => {
  const top = dictBoundedLattice.BoundedMeetSemilattice1().top;
  return (dictMonadError) => {
    const Monad0 = dictMonadError.MonadThrow0().Monad0();
    const Bind1 = Monad0.Bind1();
    const $0 = Bind1.Apply0().Functor0();
    const popArgFwd2 = popArgFwd(dictMonadError);
    const popVarFwd2 = popVarFwd(dictMonadError);
    const popRecordFwd2 = popRecordFwd(dictMonadError);
    const popListVarFwd2 = popListVarFwd(dictMonadError);
    const popConstrFwd2 = popConstrFwd(dictMonadError);
    const Applicative0 = Monad0.Applicative0();
    const sequence1 = traversableList.traverse(Applicative0)(identity2);
    const rtraverse1 = bitraversableTuple.bitraverse(Applicative0)(Applicative0.pure);
    return (ks) => {
      const $1 = (p) => Bind1.bind(popConstrFwd2(defined(dataTypeForCtr.dataTypeFor(monadThrowExceptT2)(definitely("clausesStateFwd ctrFor failed for: " + showPattern(p))(ctrFor(p)))))(ks))((kss) => $0.map((x) => $Cont(
        "ContElim",
        $Elim("ElimConstr", fromFoldable24(x))
      ))(sequence1(listMap(rtraverse1(clausesStateFwd(dictBoundedLattice)(dictMonadError)))(kss))));
      if (ks.tag === "Nil") {
        return throwException(error("absurd"))();
      }
      if (ks.tag === "Cons") {
        if (ks._1._1.tag === "Nil") {
          if (ks._1._2._1.tag === "Nil" && ks._2.tag === "Nil") {
            return $0.map(ContExpr)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(ks._1._2._2));
          }
          return $0.map((() => {
            const $2 = Lambda(top);
            return (x) => $Cont("ContExpr", $2(x.tag === "ContElim" ? x._1 : throwException(error("Eliminator expected"))()));
          })())(Bind1.bind(popArgFwd2(ks))(clausesStateFwd(dictBoundedLattice)(dictMonadError)));
        }
        if (ks._1._1.tag === "Cons") {
          if (ks._1._1._1.tag === "Left") {
            if (ks._1._1._1._1.tag === "PVar") {
              const $2 = ks._1._1._1._1._1;
              return $0.map((() => {
                const $3 = ElimVar($2);
                return (x) => $Cont("ContElim", $3(x));
              })())(Bind1.bind(popVarFwd2($2)(ks))(clausesStateFwd(dictBoundedLattice)(dictMonadError)));
            }
            if (ks._1._1._1._1.tag === "PRecord") {
              const $2 = ks._1._1._1._1._1;
              return $0.map((() => {
                const $3 = ElimDict(keys2($2));
                return (x) => $Cont("ContElim", $3(x));
              })())(Bind1.bind(popRecordFwd2(listMap(fst)($2))(ks))(clausesStateFwd(dictBoundedLattice)(dictMonadError)));
            }
            return $1(ks._1._1._1);
          }
          if (ks._1._1._1.tag === "Right" && ks._1._1._1._1.tag === "PListVar") {
            const $2 = ks._1._1._1._1._1;
            return $0.map((() => {
              const $3 = ElimVar($2);
              return (x) => $Cont("ContElim", $3(x));
            })())(Bind1.bind(popListVarFwd2($2)(ks))(clausesStateFwd(dictBoundedLattice)(dictMonadError)));
          }
          return $1(ks._1._1._1);
        }
      }
      fail();
    };
  };
};
var clausesStateBwd = (dictBoundedJoinSemilattice) => (\u03BA0) => (ks) => {
  const $0 = (\u03C3) => popArgBwd(clausesStateBwd(dictBoundedJoinSemilattice)($Cont("ContElim", \u03C3))(defined(popArgFwd1(ks))));
  const $1 = (m, p) => popConstrBwd(mapMaybe((v1) => {
    const $12 = clausesStateBwd(dictBoundedJoinSemilattice);
    const $2 = _lookup(Nothing, Just, v1._1, m);
    if ($2.tag === "Just") {
      return $Maybe("Just", $Tuple(v1._1, $12($2._1)(v1._2)));
    }
    return Nothing;
  })(defined(popConstrFwd1(defined(dataTypeForCtr.dataTypeFor(monadThrowExceptT2)(definitely("absurd")(ctrFor(p)))))(ks))))(ks);
  if (ks.tag === "Nil") {
    return throwException(error("absurd"))();
  }
  if (\u03BA0.tag === "ContExpr") {
    if (ks.tag === "Cons" && ks._1._1.tag === "Nil") {
      if (ks._1._2._1.tag === "Nil" && ks._2.tag === "Nil") {
        return $List(
          "Cons",
          $Tuple(Nil, $Tuple(Nil, exprBwd(dictBoundedJoinSemilattice)(\u03BA0._1)(ks._1._2._2))),
          Nil
        );
      }
      if (\u03BA0._1.tag === "Lambda") {
        return $0(\u03BA0._1._2);
      }
    }
    return throwException(error("absurd"))();
  }
  if (\u03BA0.tag === "ContElim") {
    if (ks.tag === "Cons" && ks._1._1.tag === "Cons") {
      if (ks._1._1._1.tag === "Left") {
        if (ks._1._1._1._1.tag === "PVar") {
          if (\u03BA0._1.tag === "ElimVar") {
            return popVarBwd(\u03BA0._1._1)(clausesStateBwd(dictBoundedJoinSemilattice)(\u03BA0._1._2)(defined(popVarFwd1(\u03BA0._1._1)(ks))));
          }
          if (\u03BA0._1.tag === "ElimConstr") {
            return $1(\u03BA0._1._1, ks._1._1._1);
          }
          return throwException(error(throwException(error("Shape mismatch"))()))();
        }
        if (ks._1._1._1._1.tag === "PRecord" && \u03BA0._1.tag === "ElimDict") {
          const $2 = ks._1._1._1._1._1;
          return popRecordBwd(listMap(fst)($2))(clausesStateBwd(dictBoundedJoinSemilattice)(\u03BA0._1._2)(defined(popRecordFwd1(listMap(fst)($2))(ks))));
        }
        if (\u03BA0._1.tag === "ElimConstr") {
          return $1(\u03BA0._1._1, ks._1._1._1);
        }
        return throwException(error(throwException(error("Shape mismatch"))()))();
      }
      if (ks._1._1._1.tag === "Right" && ks._1._1._1._1.tag === "PListVar" && \u03BA0._1.tag === "ElimVar") {
        return popListVarBwd(\u03BA0._1._1)(clausesStateBwd(dictBoundedJoinSemilattice)(\u03BA0._1._2)(defined(popListVarFwd1(\u03BA0._1._1)(ks))));
      }
      if (\u03BA0._1.tag === "ElimConstr") {
        return $1(\u03BA0._1._1, ks._1._1._1);
      }
    }
    return throwException(error(throwException(error("Shape mismatch"))()))();
  }
  fail();
};
var moduleFwd = (dictMonadError) => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  const $0 = Monad0.Bind1().Apply0().Functor0();
  const varDefFwd1 = varDefFwd(dictMonadError);
  const recDefsFwd1 = recDefsFwd(dictMonadError);
  const traverse2 = traversableList.traverse(Monad0.Applicative0());
  return (dictBoundedLattice) => {
    const varDefFwd2 = varDefFwd1(dictBoundedLattice);
    const recDefsFwd2 = recDefsFwd1(dictBoundedLattice);
    return (v) => $0.map(Module)(traverse2((v1) => {
      if (v1.tag === "Left") {
        return $0.map(Left)(varDefFwd2(v1._1));
      }
      if (v1.tag === "Right") {
        return $0.map(Right)(recDefsFwd2(v1._1));
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
    })(v._1))(identity)));
  };
};

// output-es/Pretty/index.js
var $ExprType = (tag) => tag;
var hcat = /* @__PURE__ */ (() => foldableList.foldMap(monoidColumns)(unsafeCoerce))();
var hcat1 = /* @__PURE__ */ (() => foldableArray.foldMap(monoidColumns)(unsafeCoerce))();
var toUnfoldable8 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
var toUnfoldable14 = /* @__PURE__ */ toAscUnfoldable(unfoldableList);
var Simple = /* @__PURE__ */ $ExprType("Simple");
var Expression = /* @__PURE__ */ $ExprType("Expression");
var vert = (dictFoldable) => {
  const fromFoldable27 = dictFoldable.foldr(Cons)(Nil);
  return (delim) => {
    const vert$p = (v) => {
      if (v.tag === "Nil") {
        return empty2;
      }
      if (v.tag === "Cons") {
        if (v._2.tag === "Nil") {
          return v._1;
        }
        if (v._2.tag === "Cons") {
          return atop(beside(v._1)(delim))(vert$p($List("Cons", v._2._1, v._2._2)));
        }
      }
      fail();
    };
    return (x) => vert$p(fromFoldable27(x));
  };
};
var vert1 = /* @__PURE__ */ vert(foldableArray);
var semi = /* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" ;"));
var replacement = [
  /* @__PURE__ */ $Tuple("( ", "("),
  /* @__PURE__ */ $Tuple(" )", ")"),
  /* @__PURE__ */ $Tuple("[ ", "["),
  /* @__PURE__ */ $Tuple(" ]", "]"),
  /* @__PURE__ */ $Tuple("{ ", "{"),
  /* @__PURE__ */ $Tuple(" }", "}"),
  /* @__PURE__ */ $Tuple(". ", "."),
  /* @__PURE__ */ $Tuple(" .", "."),
  /* @__PURE__ */ $Tuple(". ", "."),
  /* @__PURE__ */ $Tuple(" ,", ","),
  /* @__PURE__ */ $Tuple(" ;", ";"),
  /* @__PURE__ */ $Tuple("| ", "|"),
  /* @__PURE__ */ $Tuple(" |", "|"),
  /* @__PURE__ */ $Tuple("\u2E28 ", "\u2E28"),
  /* @__PURE__ */ $Tuple(" \u2E29", "\u2E29"),
  /* @__PURE__ */ $Tuple(" @", "@")
];
var pattRepPairs = /* @__PURE__ */ arrayMap((v) => $Tuple(v._1, v._2))(replacement);
var removeDocWS = (v) => ({
  width: v.width,
  height: v.height,
  lines: arrayMap((x) => foldlArray((curr) => (v$1) => replaceAll(v$1._1)(v$1._2)(curr))(drop(length(take3(1)(x)))(x))(pattRepPairs))(v.lines)
});
var prettyP = (dictPretty) => (x) => intercalate3("\n")(removeDocWS(dictPretty.pretty(x)).lines);
var nil2 = /* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" []"));
var intersperse$p = (v) => (v1) => {
  if (v.tag === "Cons") {
    if (v._2.tag === "Nil") {
      return v._1;
    }
    return atop(beside(v._1)(v1))(intersperse$p(v._2)(v1));
  }
  if (v.tag === "Nil") {
    return empty2;
  }
  fail();
};
var helperMatch = (pss) => $NonEmpty(
  $Tuple($NonEmpty(pss._1._1, Nil), pss._1._2),
  listMap((v) => $Tuple($NonEmpty(v._1, Nil), v._2))(pss._2)
);
var getPrec = (x) => {
  const v = lookup(ordString)(x)(opDefs);
  if (v.tag === "Just") {
    return v._1.prec;
  }
  if (v.tag === "Nothing") {
    return -1;
  }
  fail();
};
var exprType = (v) => {
  if (v.tag === "Var") {
    return Simple;
  }
  if (v.tag === "Op") {
    return Simple;
  }
  if (v.tag === "Int") {
    return Simple;
  }
  if (v.tag === "Float") {
    return Simple;
  }
  if (v.tag === "Str") {
    return Simple;
  }
  if (v.tag === "Constr") {
    if (v._3.tag === "Nil") {
      return Simple;
    }
    return Expression;
  }
  if (v.tag === "Dictionary") {
    return Simple;
  }
  if (v.tag === "Matrix") {
    return Simple;
  }
  if (v.tag === "Lambda") {
    return Simple;
  }
  if (v.tag === "Project") {
    return Simple;
  }
  if (v.tag === "DProject") {
    return Simple;
  }
  if (v.tag === "App") {
    return Expression;
  }
  if (v.tag === "BinaryApp") {
    return Expression;
  }
  if (v.tag === "MatchAs") {
    return Simple;
  }
  if (v.tag === "IfElse") {
    return Simple;
  }
  if (v.tag === "ListEmpty") {
    return Simple;
  }
  if (v.tag === "ListNonEmpty") {
    return Simple;
  }
  if (v.tag === "ListEnum") {
    return Simple;
  }
  if (v.tag === "ListComp") {
    return Simple;
  }
  if (v.tag === "Let") {
    return Expression;
  }
  if (v.tag === "LetRec") {
    return Expression;
  }
  fail();
};
var comma = /* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" ,"));
var hcomma = (dictFoldable) => {
  const $0 = dictFoldable.foldr(Cons)(Nil);
  return (x) => hcat(intercalate2($List("Cons", comma, Nil))(listMap(applicativeList.pure)($0(x))));
};
var hcomma1 = /* @__PURE__ */ hcomma(foldableList);
var hcomma2 = /* @__PURE__ */ hcomma(foldableArray);
var hcomma3 = /* @__PURE__ */ hcomma(foldableDict);
var prettyRecordOrDict = (dictPretty) => (sep) => (kdelim) => (bracify) => (prettyKey) => (xvs) => bracify(hcomma1(listMap((v) => hcat1([
  beside(v._1)(sep),
  dictPretty.pretty(v._2)
]))(listMap((v) => $Tuple(kdelim(prettyKey(v._1)), v._2))(xvs))));
var between2 = (l) => (r) => (doc) => beside(beside(l)(doc))(r);
var brackets = /* @__PURE__ */ between2(/* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" [")))(/* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" ]")));
var curlyBraces = /* @__PURE__ */ between2(/* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" {")))(/* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" }")));
var keyBracks = /* @__PURE__ */ between2(/* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" [")))(/* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" ]")));
var parens = /* @__PURE__ */ between2(/* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" (")))(/* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" )")));
var prettyParensOpt = (dictPretty) => (x) => {
  const doc = dictPretty.pretty(x);
  if (contains(" ")(intercalate3("\n")(doc.lines))) {
    return parens(doc);
  }
  return doc;
};
var prettyConstr = (dictPretty) => (v) => (v1) => {
  const $0 = (c, xs) => hcat($List(
    "Cons",
    checkOneLine(split("\n")(" " + showCtr(c))),
    listMap(prettyParensOpt(dictPretty))(xs)
  ));
  if (v1.tag === "Cons") {
    if (v1._2.tag === "Cons") {
      if (v === "Pair") {
        return assertWith("")(v1._2._2.tag === "Nil")(parens(hcomma2([dictPretty.pretty(v1._1), dictPretty.pretty(v1._2._1)])));
      }
      if (v === "Nil") {
        return assertWith("")(v1.tag === "Nil")(nil2);
      }
      if (v === ":") {
        return assertWith("")(v1._2._2.tag === "Nil")(parens(hcat1([
          dictPretty.pretty(v1._1),
          checkOneLine(split("\n")(" :")),
          dictPretty.pretty(v1._2._1)
        ])));
      }
      return $0(v, v1);
    }
    if (v === "Nil") {
      return assertWith("")(v1.tag === "Nil")(nil2);
    }
    if (v1._2.tag === "Nil") {
      return beside(checkOneLine(split("\n")(" " + showCtr(v))))(dictPretty.pretty(v1._1));
    }
    return $0(v, v1);
  }
  if (v === "Nil") {
    return assertWith("")(v1.tag === "Nil")(nil2);
  }
  return $0(v, v1);
};
var parentheses = /* @__PURE__ */ between2(/* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" (")))(/* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" )")));
var prettyPattern = {
  pretty: (v) => {
    if (v.tag === "PVar") {
      return checkOneLine(split("\n")(" " + v._1));
    }
    if (v.tag === "PRecord") {
      return curlyBraces(prettyListBindPattern.pretty(v._1));
    }
    if (v.tag === "PConstr") {
      if (v._2.tag === "Nil") {
        return parentheses((() => {
          if (v._1 === "Pair") {
            return prettyPattConstr(checkOneLine(split("\n")(" ,")))(v._2);
          }
          if (v._1 === ":") {
            return prettyPattConstr(checkOneLine(split("\n")(" :")))(v._2);
          }
          return beside(checkOneLine(split("\n")(" " + v._1)))(prettyPattConstr(empty2)(v._2));
        })());
      }
      if (v._2.tag === "Cons") {
        if (v._2._2.tag === "Nil") {
          return beside(checkOneLine(split("\n")(" " + v._1)))(prettyPattern.pretty(v._2._1));
        }
        return parentheses((() => {
          if (v._1 === "Pair") {
            return prettyPattConstr(checkOneLine(split("\n")(" ,")))(v._2);
          }
          if (v._1 === ":") {
            return prettyPattConstr(checkOneLine(split("\n")(" :")))(v._2);
          }
          return beside(checkOneLine(split("\n")(" " + v._1)))(prettyPattConstr(empty2)(v._2));
        })());
      }
      fail();
    }
    if (v.tag === "PListEmpty") {
      return brackets(empty2);
    }
    if (v.tag === "PListNonEmpty") {
      return beside(beside(checkOneLine(split("\n")(" [")))(prettyPattern.pretty(v._1)))(prettyListRestPattern.pretty(v._2));
    }
    fail();
  }
};
var prettyListRestPattern = {
  pretty: (v) => {
    if (v.tag === "PListVar") {
      return checkOneLine(split("\n")(" " + v._1));
    }
    if (v.tag === "PListNext") {
      return beside(beside(checkOneLine(split("\n")(" ,")))(prettyPattern.pretty(v._1)))(prettyListRestPattern.pretty(v._2));
    }
    if (v.tag === "PListEnd") {
      return checkOneLine(split("\n")(" ]"));
    }
    fail();
  }
};
var prettyListBindPattern = {
  pretty: (v) => {
    if (v.tag === "Cons") {
      if (v._2.tag === "Nil") {
        return beside(beside(checkOneLine(split("\n")(" " + v._1._1)))(checkOneLine(split("\n")(" :"))))(prettyPattern.pretty(v._1._2));
      }
      return atop(beside(beside(beside(checkOneLine(split("\n")(" " + v._1._1)))(checkOneLine(split("\n")(" :"))))(prettyPattern.pretty(v._1._2)))(checkOneLine(split("\n")(" ,"))))(prettyListBindPattern.pretty(v._2));
    }
    if (v.tag === "Nil") {
      return empty2;
    }
    fail();
  }
};
var prettyPattConstr = (v) => (v1) => {
  if (v1.tag === "Nil") {
    return empty2;
  }
  if (v1.tag === "Cons") {
    if (v1._2.tag === "Nil") {
      return prettyPattern.pretty(v1._1);
    }
    return beside(beside(prettyPattern.pretty(v1._1))(v))(prettyPattConstr(v)(v1._2));
  }
  fail();
};
var arrayBrackets = /* @__PURE__ */ between2(/* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" [|")))(/* @__PURE__ */ checkOneLine(/* @__PURE__ */ split("\n")(" |]")));
var prettyExpr = (dictHighlightable) => ({
  pretty: (v) => {
    if (v.tag === "Var") {
      return checkOneLine(split("\n")(" " + v._1));
    }
    if (v.tag === "Int") {
      return dictHighlightable.highlightIf(v._1)(checkOneLine(split("\n")(" " + showIntImpl(v._2))));
    }
    if (v.tag === "Float") {
      return dictHighlightable.highlightIf(v._1)(checkOneLine(split("\n")(" " + showNumberImpl(v._2))));
    }
    if (v.tag === "Str") {
      return dictHighlightable.highlightIf(v._1)(checkOneLine(split("\n")(" " + showStringImpl(v._2))));
    }
    if (v.tag === "Dictionary") {
      return dictHighlightable.highlightIf(v._1)(prettyRecordOrDict(prettyExpr(dictHighlightable))(checkOneLine(split("\n")(" :")))(keyBracks)(curlyBraces)(prettyExpr(dictHighlightable).pretty)(listMap(toTuple)(v._2)));
    }
    if (v.tag === "Constr") {
      return dictHighlightable.highlightIf(v._1)(prettyConstr(prettyExpr(dictHighlightable))(v._2)(v._3));
    }
    if (v.tag === "Matrix") {
      return dictHighlightable.highlightIf(v._1)(prettyMatrix(dictHighlightable)(v._2)(v._3._1)(v._3._2)(v._4));
    }
    if (v.tag === "Lambda") {
      return hcat1([dictHighlightable.highlightIf(v._1)(checkOneLine(split("\n")(" fun"))), prettyElim(dictHighlightable).pretty(v._2)]);
    }
    if (v.tag === "Op") {
      return parens(checkOneLine(split("\n")(" " + v._1)));
    }
    if (v.tag === "Let") {
      return atop(hcat1([
        checkOneLine(split("\n")(" let")),
        prettyElim(dictHighlightable).pretty(v._1._1),
        checkOneLine(split("\n")(" =")),
        prettyExpr(dictHighlightable).pretty(v._1._2),
        checkOneLine(split("\n")(" in"))
      ]))(prettyExpr(dictHighlightable).pretty(v._2));
    }
    if (v.tag === "LetRec") {
      return atop(hcat1([
        checkOneLine(split("\n")(" let")),
        prettyDictElim(dictHighlightable).pretty(v._1._2),
        checkOneLine(split("\n")(" in"))
      ]))(prettyExpr(dictHighlightable).pretty(v._2));
    }
    if (v.tag === "Project") {
      return beside(beside(prettyExpr(dictHighlightable).pretty(v._1))(checkOneLine(split("\n")(" ."))))(checkOneLine(split("\n")(" " + v._2)));
    }
    if (v.tag === "DProject") {
      return beside(beside(beside(beside(prettyExpr(dictHighlightable).pretty(v._1))(checkOneLine(split("\n")(" ."))))(checkOneLine(split("\n")(" ["))))(prettyExpr(dictHighlightable).pretty(v._2)))(checkOneLine(split("\n")(" ]")));
    }
    if (v.tag === "App") {
      return hcat1([prettyExpr(dictHighlightable).pretty(v._1), prettyExpr(dictHighlightable).pretty(v._2)]);
    }
    fail();
  }
});
var prettyElim = (dictHighlightable) => ({
  pretty: (v) => {
    if (v.tag === "ElimVar") {
      return hcat1([
        checkOneLine(split("\n")(" " + v._1)),
        checkOneLine(split("\n")(" ->")),
        prettyCont(dictHighlightable).pretty(v._2)
      ]);
    }
    if (v.tag === "ElimConstr") {
      return hcomma3(_fmapObject(v._1, prettyCont(dictHighlightable).pretty));
    }
    if (v.tag === "ElimDict") {
      return hcat1([
        curlyBraces(hcomma1(listMap(text)(toUnfoldable8(v._1)))),
        checkOneLine(split("\n")(" ->")),
        curlyBraces(prettyCont(dictHighlightable).pretty(v._2))
      ]);
    }
    fail();
  }
});
var prettyDictElim = (dictHighlightable) => ({
  pretty: (\u03C1) => {
    const go = (v) => {
      if (v.tag === "Nil") {
        return empty2;
      }
      if (v.tag === "Cons") {
        if (v._2.tag === "Nil") {
          return prettyBindElim(dictHighlightable).pretty(v._1);
        }
        return atop(beside(go(v._2))(semi))(prettyBindElim(dictHighlightable).pretty(v._1));
      }
      fail();
    };
    return go(toUnfoldable14(\u03C1));
  }
});
var prettyCont = (dictHighlightable) => ({
  pretty: (v) => {
    if (v.tag === "ContExpr") {
      return prettyExpr(dictHighlightable).pretty(v._1);
    }
    if (v.tag === "ContElim") {
      return prettyElim(dictHighlightable).pretty(v._1);
    }
    fail();
  }
});
var prettyBindElim = (dictHighlightable) => ({
  pretty: (v) => hcat1([
    checkOneLine(split("\n")(" " + v._1)),
    checkOneLine(split("\n")(" =")),
    prettyElim(dictHighlightable).pretty(v._2)
  ])
});
var prettyMatrix = (dictHighlightable) => (e1) => (i) => (j) => (e2) => arrayBrackets(beside(beside(beside(beside(prettyExpr(dictHighlightable).pretty(e1))(checkOneLine(split("\n")(" <-"))))(checkOneLine(split("\n")(" " + i + "\xD7" + j))))(checkOneLine(split("\n")(" in"))))(prettyExpr(dictHighlightable).pretty(e2)));
var prettyVal = (dictHighlightable) => ({ pretty: (v) => dictHighlightable.highlightIf(v._1)(prettyBaseVal(dictHighlightable).pretty(v._2)) });
var prettyFun = (dictHighlightable) => ({
  pretty: (v) => {
    if (v.tag === "Closure") {
      return beside(checkOneLine(split("\n")(" cl")))(parentheses(beside(beside(beside(beside(prettyEnv(dictHighlightable).pretty(v._1))(checkOneLine(split("\n")(" ,"))))(prettyDictElim(dictHighlightable).pretty(v._2)))(checkOneLine(split("\n")(" ,"))))(prettyElim(dictHighlightable).pretty(v._3))));
    }
    if (v.tag === "Foreign") {
      return checkOneLine(split("\n")(" " + v._1._1));
    }
    if (v.tag === "PartialConstr") {
      return prettyConstr(prettyVal(dictHighlightable))(v._1)(v._2);
    }
    fail();
  }
});
var prettyEnv = (dictHighlightable) => ({
  pretty: (v) => {
    const go = (v1) => {
      if (v1.tag === "Nil") {
        return empty2;
      }
      if (v1.tag === "Cons") {
        return atop(beside(beside(beside(checkOneLine(split("\n")(" " + v1._1._1)))(checkOneLine(split("\n")(" ->"))))(prettyVal(dictHighlightable).pretty(v1._1._2)))(checkOneLine(split("\n")(" ,"))))(go(v1._2));
      }
      fail();
    };
    return brackets(go(toUnfoldable14(v)));
  }
});
var prettyBaseVal = (dictHighlightable) => ({
  pretty: (v) => {
    if (v.tag === "Int") {
      return checkOneLine(split("\n")(" " + showIntImpl(v._1)));
    }
    if (v.tag === "Float") {
      return checkOneLine(split("\n")(" " + showNumberImpl(v._1)));
    }
    if (v.tag === "Str") {
      return checkOneLine(split("\n")(" " + showStringImpl(v._1)));
    }
    if (v.tag === "Dictionary") {
      return prettyRecordOrDict(prettyVal(dictHighlightable))(checkOneLine(split("\n")(" :")))(keyBracks)(curlyBraces)((v1) => dictHighlightable.highlightIf(v1._2)(checkOneLine(split("\n")(" " + showStringImpl(v1._1)))))(listMap((v1) => $Tuple(
        $Tuple(v1._1, v1._2._1),
        v1._2._2
      ))(toUnfoldable14(v._1)));
    }
    if (v.tag === "Constr") {
      return prettyConstr(prettyVal(dictHighlightable))(v._1)(v._2);
    }
    if (v.tag === "Matrix") {
      return vert1(comma)(arrayMap((() => {
        const $0 = arrayMap(prettyVal(dictHighlightable).pretty);
        return (x) => hcomma2($0(x));
      })())(v._1._1));
    }
    if (v.tag === "Fun") {
      return prettyFun(dictHighlightable).pretty(v._1);
    }
    fail();
  }
});
var prettyVarDefs = (dictAnn) => ({
  pretty: (ds) => intersperse$p((() => {
    const $0 = prettyVarDef(dictAnn);
    return $List("Cons", $0.pretty(ds._1), listMap($0.pretty)(ds._2));
  })())(checkOneLine(split("\n")(" ;")))
});
var prettyVarDef = (dictAnn) => ({
  pretty: (v) => beside(beside(prettyPattern.pretty(v._1))(checkOneLine(split("\n")(" ="))))(prettyExpr1(dictAnn).pretty(v._2))
});
var prettyNonEmptyListPattern = (dictAnn) => ({
  pretty: (pss) => intersperse$p(listMap(prettyClause(dictAnn)(checkOneLine(split("\n")(" ->"))))(listMap(Clause)((() => {
    const $0 = helperMatch(pss);
    return $List("Cons", $0._1, $0._2);
  })())))(checkOneLine(split("\n")(" ;")))
});
var prettyNonEmptyListNonEmpt = (dictAnn) => ({
  pretty: (hs) => intersperse$p((() => {
    const $0 = prettyNonEmptyListBranch(dictAnn);
    return $List("Cons", $0.pretty(hs._1), listMap($0.pretty)(hs._2));
  })())(checkOneLine(split("\n")(" ;")))
});
var prettyNonEmptyListBranch = (dictAnn) => ({
  pretty: (h) => intersperse$p((() => {
    const $0 = prettyBranch(dictAnn);
    return $List("Cons", $0.pretty(h._1), listMap($0.pretty)(h._2));
  })())(checkOneLine(split("\n")(" ;")))
});
var prettyListRest = (dictAnn) => {
  const $0 = dictAnn.Highlightable0();
  return {
    pretty: (v) => {
      if (v.tag === "Next") {
        if (v._2.tag === "Dictionary") {
          return atop(beside($0.highlightIf(v._1)(checkOneLine(split("\n")(" ,"))))($0.highlightIf(v._1)(curlyBraces(prettyDictEntries(dictAnn)(beside)(v._2._2)))))(prettyListRest(dictAnn).pretty(v._3));
        }
        return beside(beside($0.highlightIf(v._1)(checkOneLine(split("\n")(" ,"))))(prettyExpr1(dictAnn).pretty(v._2)))(prettyListRest(dictAnn).pretty(v._3));
      }
      if (v.tag === "End") {
        return $0.highlightIf(v._1)(checkOneLine(split("\n")(" ]")));
      }
      fail();
    }
  };
};
var prettyListQualifier = (dictAnn) => ({
  pretty: (v) => {
    const $0 = (q, qs) => beside(beside(prettyListQualifier(dictAnn).pretty($List("Cons", q, Nil)))(checkOneLine(split("\n")(" ,"))))(prettyListQualifier(dictAnn).pretty(qs));
    if (v.tag === "Cons") {
      if (v._2.tag === "Nil") {
        if (v._1.tag === "ListCompGuard") {
          return prettyExpr1(dictAnn).pretty(v._1._1);
        }
        if (v._1.tag === "ListCompDecl") {
          return beside(checkOneLine(split("\n")(" let")))(prettyVarDef(dictAnn).pretty(v._1._1));
        }
        if (v._1.tag === "ListCompGen") {
          return beside(beside(prettyPattern.pretty(v._1._1))(checkOneLine(split("\n")(" <-"))))(prettyExpr1(dictAnn).pretty(v._1._2));
        }
      }
      return $0(v._1, v._2);
    }
    if (v.tag === "Nil") {
      return empty2;
    }
    fail();
  }
});
var prettyFirstGroup = (dictAnn) => ({ pretty: (v) => prettyNonEmptyListNonEmpt(dictAnn).pretty(wrappedOperation("groupBy")(groupBy((p) => (q) => p._1 === q._1))(v)) });
var prettyExpr1 = (dictAnn) => {
  const $0 = dictAnn.Highlightable0();
  return {
    pretty: (v) => {
      if (v.tag === "Var") {
        return checkOneLine(split("\n")(" " + v._1));
      }
      if (v.tag === "Op") {
        return parentheses(checkOneLine(split("\n")(" " + v._1)));
      }
      if (v.tag === "Int") {
        return $0.highlightIf(v._1)(checkOneLine(split("\n")(" " + showIntImpl(v._2))));
      }
      if (v.tag === "Float") {
        return $0.highlightIf(v._1)(checkOneLine(split("\n")(" " + showNumberImpl(v._2))));
      }
      if (v.tag === "Str") {
        return $0.highlightIf(v._1)(checkOneLine(split("\n")(' "' + v._2 + '"')));
      }
      if (v.tag === "Constr") {
        if (v._2 === "Explained") {
          if (v._3.tag === "Nil") {
            return throwException(error("malformed explanation"))();
          }
          if (v._3.tag === "Cons") {
            if (v._3._1.tag === "Str") {
              return $0.highlightIf(v._1)(beside(beside(checkOneLine(split("\n")(" @")))(checkOneLine(split("\n")(" " + v._3._1._2))))(checkOneLine(split("\n")(" @"))));
            }
            return throwException(error("malformed explanation"))();
          }
          fail();
        }
        return $0.highlightIf(v._1)(prettyConstr(prettyExpr1(dictAnn))(v._2)(v._3));
      }
      if (v.tag === "Dictionary") {
        return $0.highlightIf(v._1)(curlyBraces(prettyDictEntries(dictAnn)(atop)(v._2)));
      }
      if (v.tag === "Matrix") {
        return $0.highlightIf(v._1)(arrayBrackets(beside(beside(beside(beside(prettyExpr1(dictAnn).pretty(v._2))(checkOneLine(split("\n")(" |"))))(parentheses(beside(beside(checkOneLine(split("\n")(" " + v._3._1)))(checkOneLine(split("\n")(" ,"))))(checkOneLine(split("\n")(" " + v._3._2))))))(checkOneLine(split("\n")(" in"))))(prettyExpr1(dictAnn).pretty(v._4))));
      }
      if (v.tag === "Lambda") {
        return parentheses(beside(checkOneLine(split("\n")(" fun")))(prettyClauses(dictAnn).pretty(v._1)));
      }
      if (v.tag === "Project") {
        return beside(beside(prettySimple(dictAnn)(v._1))(checkOneLine(split("\n")(" ."))))(checkOneLine(split("\n")(" " + v._2)));
      }
      if (v.tag === "DProject") {
        return beside(beside(beside(beside(prettySimple(dictAnn)(v._1))(checkOneLine(split("\n")(" ."))))(checkOneLine(split("\n")(" ["))))(prettySimple(dictAnn)(v._2)))(checkOneLine(split("\n")(" ]")));
      }
      if (v.tag === "App") {
        return prettyAppChain(dictAnn)($Expr2("App", v._1, v._2));
      }
      if (v.tag === "BinaryApp") {
        return prettyBinApp(dictAnn)(0)($Expr2("BinaryApp", v._1, v._2, v._3));
      }
      if (v.tag === "MatchAs") {
        return atop(beside(beside(checkOneLine(split("\n")(" match")))(prettyExpr1(dictAnn).pretty(v._1)))(checkOneLine(split("\n")(" as"))))(curlyBraces(prettyNonEmptyListPattern(dictAnn).pretty(v._2)));
      }
      if (v.tag === "IfElse") {
        return beside(beside(beside(beside(beside(checkOneLine(split("\n")(" if")))(prettyExpr1(dictAnn).pretty(v._1)))(checkOneLine(split("\n")(" then"))))(prettyExpr1(dictAnn).pretty(v._2)))(checkOneLine(split("\n")(" else"))))(prettyExpr1(dictAnn).pretty(v._3));
      }
      if (v.tag === "ListEmpty") {
        return $0.highlightIf(v._1)(brackets(empty2));
      }
      if (v.tag === "ListNonEmpty") {
        if (v._2.tag === "Dictionary") {
          return atop(beside($0.highlightIf(v._1)(checkOneLine(split("\n")(" ["))))($0.highlightIf(v._1)(curlyBraces(prettyDictEntries(dictAnn)(beside)(v._2._2)))))(prettyListRest(dictAnn).pretty(v._3));
        }
        return beside(beside($0.highlightIf(v._1)(checkOneLine(split("\n")(" ["))))(prettyExpr1(dictAnn).pretty(v._2)))(prettyListRest(dictAnn).pretty(v._3));
      }
      if (v.tag === "ListEnum") {
        return brackets(beside(beside(prettyExpr1(dictAnn).pretty(v._1))(checkOneLine(split("\n")(" .."))))(prettyExpr1(dictAnn).pretty(v._2)));
      }
      if (v.tag === "ListComp") {
        return $0.highlightIf(v._1)(brackets(beside(beside(prettyExpr1(dictAnn).pretty(v._2))(checkOneLine(split("\n")(" |"))))(prettyListQualifier(dictAnn).pretty(v._3))));
      }
      if (v.tag === "Let") {
        return atop(beside(beside(checkOneLine(split("\n")(" let")))(prettyVarDefs(dictAnn).pretty(v._1)))(checkOneLine(split("\n")(" in"))))(prettyExpr1(dictAnn).pretty(v._2));
      }
      if (v.tag === "LetRec") {
        return atop(beside(beside(checkOneLine(split("\n")(" let")))(prettyFirstGroup(dictAnn).pretty(v._1)))(checkOneLine(split("\n")(" in"))))(prettyExpr1(dictAnn).pretty(v._2));
      }
      fail();
    }
  };
};
var prettyDictEntry = (dictAnn) => ({
  pretty: (v) => {
    if (v.tag === "ExprKey") {
      return beside(beside(checkOneLine(split("\n")(" [")))(prettyExpr1(dictAnn).pretty(v._1)))(checkOneLine(split("\n")(" ]")));
    }
    if (v.tag === "VarKey") {
      return dictAnn.Highlightable0().highlightIf(v._1)(checkOneLine(split("\n")(" " + v._2)));
    }
    fail();
  }
});
var prettyClauses = (dictAnn) => ({
  pretty: (v) => intersperse$p((() => {
    const $0 = prettyClause(dictAnn)(checkOneLine(split("\n")(" =")));
    return $List("Cons", $0(v._1), listMap($0)(v._2));
  })())(checkOneLine(split("\n")(" ;")))
});
var prettyBranch = (dictAnn) => ({
  pretty: (v) => beside(checkOneLine(split("\n")(" " + v._1)))(prettyClause(dictAnn)(checkOneLine(split("\n")(" =")))($Tuple(
    v._2._1,
    v._2._2
  )))
});
var prettySimple = (dictAnn) => (s) => {
  const v = exprType(s);
  if (v === "Simple") {
    return prettyExpr1(dictAnn).pretty(s);
  }
  if (v === "Expression") {
    return parentheses(prettyExpr1(dictAnn).pretty(s));
  }
  fail();
};
var prettyDictEntries = (dictAnn) => (v) => (v1) => {
  if (v1.tag === "Nil") {
    return empty2;
  }
  if (v1.tag === "Cons") {
    if (v1._2.tag === "Nil") {
      return beside(beside(prettyDictEntry(dictAnn).pretty(v1._1._1))(checkOneLine(split("\n")(" :"))))(prettyExpr1(dictAnn).pretty(v1._1._2));
    }
    return v(beside(prettyDictEntries(dictAnn)(v)($List("Cons", $Tuple(v1._1._1, v1._1._2), Nil)))(checkOneLine(split("\n")(" ,"))))(prettyDictEntries(dictAnn)(v)(v1._2));
  }
  fail();
};
var prettyClause = (dictAnn) => (sep) => (v) => beside(beside(prettyPattConstr(empty2)($List("Cons", v._1._1, v._1._2)))(sep))(prettyExpr1(dictAnn).pretty(v._2));
var prettyBinApp = (dictAnn) => (v) => (v1) => {
  if (v1.tag === "BinaryApp") {
    const prec$p = getPrec(v1._2);
    if (getPrec(v1._2) === -1) {
      return beside(beside(prettyBinApp(dictAnn)(prec$p)(v1._1))(checkOneLine(split("\n")(" `" + v1._2 + "`"))))(prettyBinApp(dictAnn)(prec$p)(v1._3));
    }
    if (prec$p <= v) {
      return parentheses(beside(beside(prettyBinApp(dictAnn)(prec$p)(v1._1))(checkOneLine(split("\n")(" " + v1._2))))(prettyBinApp(dictAnn)(prec$p)(v1._3)));
    }
    return beside(beside(prettyBinApp(dictAnn)(prec$p)(v1._1))(checkOneLine(split("\n")(" " + v1._2))))(prettyBinApp(dictAnn)(prec$p)(v1._3));
  }
  return prettyAppChain(dictAnn)(v1);
};
var prettyAppChain = (dictAnn) => (v) => {
  if (v.tag === "App") {
    return beside(prettyAppChain(dictAnn)(v._1))(prettySimple(dictAnn)(v._2));
  }
  return prettySimple(dictAnn)(v);
};

// output-es/Primitive/index.js
var fanin2 = /* @__PURE__ */ fanin(categoryFn)(choiceFn);
var isZeroNumber = { isZero: ($0) => 0 === $0 };
var isZeroInt = { isZero: ($0) => 0 === $0 };
var typeError = (v) => (typeName2) => throwException(error(typeName2 + " expected; got " + intercalate3("\n")(removeDocWS(prettyBaseVal(highlightableUnit).pretty(functorBaseVal.map((v$1) => {
})(v))).lines)))();
var string = {
  pack: Str,
  unpack: (v) => {
    if (v.tag === "Str") {
      return v._1;
    }
    return typeError(v)("Str");
  }
};
var unary = (dictBoundedJoinSemilattice) => {
  const bot = dictBoundedJoinSemilattice.bot;
  return (id) => (f) => $Tuple(
    id,
    $Val(
      bot,
      $BaseVal(
        "Fun",
        $Fun(
          "Foreign",
          $Tuple(
            id,
            $ForeignOp$p({
              arity: 1,
              "op'": (dictMonadWithGraphAlloc) => {
                const $$new = dictMonadWithGraphAlloc.new(typeNameBaseVal);
                return (dictMonadError) => (v) => {
                  if (v.tag === "Cons" && v._2.tag === "Nil") {
                    return $$new(Val)($$$Map("Two", Leaf2, v._1._1, void 0, Leaf2))(f.o.pack(f.fwd(f.i.unpack(v._1._2))));
                  }
                  fail();
                };
              },
              op: (dictAnn) => (dictMonadError) => (v) => {
                if (v.tag === "Cons" && v._2.tag === "Nil") {
                  return dictMonadError.MonadThrow0().Monad0().Applicative0().pure($Tuple(
                    functorBaseVal.map((v$1) => {
                    })(v._1._2),
                    $Val(v._1._1, f.o.pack(f.fwd(f.i.unpack(v._1._2))))
                  ));
                }
                fail();
              },
              op_bwd: (dictAnn) => (v) => $List("Cons", $Val(v._2._1, f.i.pack(f.i.unpack(v._1))), Nil)
            })
          ),
          Nil
        )
      )
    )
  );
};
var number = {
  pack: Float,
  unpack: (v) => {
    if (v.tag === "Float") {
      return v._1;
    }
    return typeError(v)("Float");
  }
};
var intOrNumberOrString = {
  pack: (v) => {
    if (v.tag === "Left") {
      return $BaseVal("Int", v._1);
    }
    if (v.tag === "Right") {
      if (v._1.tag === "Left") {
        return $BaseVal("Float", v._1._1);
      }
      if (v._1.tag === "Right") {
        return $BaseVal("Str", v._1._1);
      }
    }
    fail();
  },
  unpack: (v) => {
    if (v.tag === "Int") {
      return $Either("Left", v._1);
    }
    if (v.tag === "Float") {
      return $Either("Right", $Either("Left", v._1));
    }
    if (v.tag === "Str") {
      return $Either("Right", $Either("Right", v._1));
    }
    return typeError(v)("Int, Float or Str");
  }
};
var intOrNumber = {
  pack: (v) => {
    if (v.tag === "Left") {
      return $BaseVal("Int", v._1);
    }
    if (v.tag === "Right") {
      return $BaseVal("Float", v._1);
    }
    fail();
  },
  unpack: (v) => {
    if (v.tag === "Int") {
      return $Either("Left", v._1);
    }
    if (v.tag === "Float") {
      return $Either("Right", v._1);
    }
    return typeError(v)("Int or Float");
  }
};
var $$int = {
  pack: Int,
  unpack: (v) => {
    if (v.tag === "Int") {
      return v._1;
    }
    return typeError(v)("Int");
  }
};
var intPair = {
  pack: (v) => $BaseVal(
    "Constr",
    "Pair",
    $List(
      "Cons",
      $Val(v._1._2, $BaseVal("Int", v._1._1)),
      $List("Cons", $Val(v._2._2, $BaseVal("Int", v._2._1)), Nil)
    )
  ),
  unpack: (v) => {
    if (v.tag === "Constr" && v._2.tag === "Cons" && v._2._2.tag === "Cons" && v._2._2._2.tag === "Nil" && v._1 === "Pair") {
      return $Tuple(
        $Tuple(v._2._1._2.tag === "Int" ? v._2._1._2._1 : typeError(v._2._1._2)("Int"), v._2._1._1),
        $Tuple(v._2._2._1._2.tag === "Int" ? v._2._2._1._2._1 : typeError(v._2._2._1._2)("Int"), v._2._2._1._1)
      );
    }
    return typeError(v)("Pair");
  }
};
var $$boolean = {
  pack: (v) => {
    if (v) {
      return $BaseVal("Constr", "True", Nil);
    }
    return $BaseVal("Constr", "False", Nil);
  },
  unpack: (v) => {
    if (v.tag === "Constr" && v._2.tag === "Nil") {
      if (v._1 === "True") {
        return true;
      }
      if (v._1 === "False") {
        return false;
      }
    }
    return typeError(v)("Boolean");
  }
};
var binaryZero = (dictBoundedJoinSemilattice) => {
  const bot = dictBoundedJoinSemilattice.bot;
  return (dictIsZero) => (id) => (f) => $Tuple(
    id,
    $Val(
      bot,
      $BaseVal(
        "Fun",
        $Fun(
          "Foreign",
          $Tuple(
            id,
            $ForeignOp$p({
              arity: 2,
              "op'": (dictMonadWithGraphAlloc) => {
                const $$new = dictMonadWithGraphAlloc.new(typeNameBaseVal);
                return (dictMonadError) => (v) => {
                  if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._2.tag === "Nil") {
                    const $0 = f.i.unpack(v._1._2);
                    const $1 = f.i.unpack(v._2._1._2);
                    return $$new(Val)((() => {
                      if (dictIsZero.isZero($0)) {
                        return $$$Map("Two", Leaf2, v._1._1, void 0, Leaf2);
                      }
                      if (dictIsZero.isZero($1)) {
                        return $$$Map("Two", Leaf2, v._2._1._1, void 0, Leaf2);
                      }
                      return insert(ordVertex)(v._2._1._1)()($$$Map(
                        "Two",
                        Leaf2,
                        v._1._1,
                        void 0,
                        Leaf2
                      ));
                    })())(f.o.pack(f.fwd($0)($1)));
                  }
                  fail();
                };
              },
              op: (dictAnn) => (dictMonadError) => (v) => {
                if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._2.tag === "Nil") {
                  const $0 = f.i.unpack(v._1._2);
                  const $1 = f.i.unpack(v._2._1._2);
                  return dictMonadError.MonadThrow0().Monad0().Applicative0().pure($Tuple(
                    $Tuple(functorBaseVal.map((v$1) => {
                    })(v._1._2), functorBaseVal.map((v$1) => {
                    })(v._2._1._2)),
                    $Val(
                      (() => {
                        if (dictIsZero.isZero($0)) {
                          return v._1._1;
                        }
                        if (dictIsZero.isZero($1)) {
                          return v._2._1._1;
                        }
                        return dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0().meet(v._1._1)(v._2._1._1);
                      })(),
                      f.o.pack(f.fwd($0)($1))
                    )
                  ));
                }
                fail();
              },
              op_bwd: (dictAnn) => {
                const bot1 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0().bot;
                return (v) => {
                  const $0 = f.i.unpack(v._1._1);
                  const $1 = f.i.unpack(v._1._2);
                  if (dictIsZero.isZero($0)) {
                    return $List("Cons", $Val(v._2._1, f.i.pack($0)), $List("Cons", $Val(bot1, f.i.pack($1)), Nil));
                  }
                  if (dictIsZero.isZero($1)) {
                    return $List("Cons", $Val(bot1, f.i.pack($0)), $List("Cons", $Val(v._2._1, f.i.pack($1)), Nil));
                  }
                  return $List("Cons", $Val(v._2._1, f.i.pack($0)), $List("Cons", $Val(v._2._1, f.i.pack($1)), Nil));
                };
              }
            })
          ),
          Nil
        )
      )
    )
  );
};
var binary = (dictBoundedJoinSemilattice) => {
  const bot = dictBoundedJoinSemilattice.bot;
  return (id) => (f) => $Tuple(
    id,
    $Val(
      bot,
      $BaseVal(
        "Fun",
        $Fun(
          "Foreign",
          $Tuple(
            id,
            $ForeignOp$p({
              arity: 2,
              "op'": (dictMonadWithGraphAlloc) => {
                const $$new = dictMonadWithGraphAlloc.new(typeNameBaseVal);
                return (dictMonadError) => (v) => {
                  if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._2.tag === "Nil") {
                    return $$new(Val)(insert(ordVertex)(v._2._1._1)()($$$Map(
                      "Two",
                      Leaf2,
                      v._1._1,
                      void 0,
                      Leaf2
                    )))(f.o.pack(f.fwd(f.i1.unpack(v._1._2))(f.i2.unpack(v._2._1._2))));
                  }
                  fail();
                };
              },
              op: (dictAnn) => (dictMonadError) => (v) => {
                if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._2.tag === "Nil") {
                  return dictMonadError.MonadThrow0().Monad0().Applicative0().pure($Tuple(
                    $Tuple(functorBaseVal.map((v$1) => {
                    })(v._1._2), functorBaseVal.map((v$1) => {
                    })(v._2._1._2)),
                    $Val(
                      dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0().meet(v._1._1)(v._2._1._1),
                      f.o.pack(f.fwd(f.i1.unpack(v._1._2))(f.i2.unpack(v._2._1._2)))
                    )
                  ));
                }
                fail();
              },
              op_bwd: (dictAnn) => (v) => $List(
                "Cons",
                $Val(v._2._1, f.i1.pack(f.i1.unpack(v._1._1))),
                $List("Cons", $Val(v._2._1, f.i2.pack(f.i2.unpack(v._1._2))), Nil)
              )
            })
          ),
          Nil
        )
      )
    )
  );
};
var asNumberString = { as: (v) => throwException(error("Non-uniform argument types"))() };
var asNumberIntOrNumber = { as: Right };
var asIntNumberOrString = { as: (x) => $Either("Left", toNumber(x)) };
var asIntNumber = { as: toNumber };
var asIntIntOrNumber = { as: Left };
var asBooleanBoolean = { as: (x) => x };
var union6 = (dictAs) => (dictAs1) => (dictAs2) => (dictAs3) => (v) => (v1) => (v2) => (v3) => {
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
  }
  fail();
};
var unionStr = (dictAs) => (dictAs1) => union6(dictAs)(dictAs)(dictAs1)(dictAs1);

// output-es/EvalGraph/index.js
var setSet4 = /* @__PURE__ */ setSet(ordDVertex);
var disjointUnion2 = /* @__PURE__ */ disjointUnion(mapEnvStringVal);
var fromFoldable11 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)()(m))(Leaf2))();
var show22 = /* @__PURE__ */ (() => showSet(showString).show)();
var toUnfoldable9 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
var union1 = /* @__PURE__ */ (() => setSet(ordString).union)();
var fv = /* @__PURE__ */ (() => fVDict(fVElim).fv)();
var fromFoldable17 = /* @__PURE__ */ fromFoldable2(foldableList);
var greaterThanOrEq = /* @__PURE__ */ (() => {
  const $0 = ordTuple(ordInt)(ordInt);
  return (a1) => (a2) => $0.compare(a1)(a2) !== "LT";
})();
var show3 = (v) => "(Tuple " + showIntImpl(v._1) + " " + showIntImpl(v._2) + ")";
var concatM = (dictMonad) => foldableList.foldr((() => {
  const $0 = dictMonad.Bind1();
  return (f) => (g) => (a) => $0.bind(f(a))(g);
})())(dictMonad.Applicative0().pure);
var fwdSlice2 = /* @__PURE__ */ fwdSlice(graphGraphImpl);
var bwdSlice2 = /* @__PURE__ */ bwdSlice(graphGraphImpl);
var matchMany = (dictMonadWithGraphAlloc) => {
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const $0 = Monad0.Applicative0();
  const $1 = Monad0.Bind1();
  const $$throw2 = $$throw(dictMonadWithGraphAlloc.MonadError1().MonadThrow0());
  return (v) => (v1) => {
    if (v.tag === "Nil") {
      return $0.pure($Tuple(empty, $Tuple(v1, setSet4.empty)));
    }
    if (v.tag === "Cons") {
      if (v1.tag === "ContElim") {
        const $2 = v._2;
        return $1.bind(match(dictMonadWithGraphAlloc)(v._1)(v1._1))((v3) => {
          const $3 = v3._2._2;
          const $4 = v3._1;
          return $1.bind(matchMany(dictMonadWithGraphAlloc)($2)(v3._2._1))((v4) => $0.pure($Tuple(
            disjointUnion2($4)(v4._1),
            $Tuple(v4._2._1, setSet4.union($3)(v4._2._2))
          )));
        });
      }
      if (v1.tag === "ContExpr") {
        return $$throw2(showIntImpl((() => {
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
            return go$r;
          };
          return go(0)(v._2) + 1 | 0;
        })()) + " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?");
      }
    }
    fail();
  };
};
var match = (dictMonadWithGraphAlloc) => {
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const $0 = Monad0.Applicative0();
  const Bind1 = Monad0.Bind1();
  const MonadError1 = dictMonadWithGraphAlloc.MonadError1();
  const withMsg2 = withMsg(MonadError1);
  const consistentWith2 = consistentWith(MonadError1);
  const MonadThrow0 = MonadError1.MonadThrow0();
  return (v) => (v1) => {
    if (v1.tag === "ElimVar") {
      if (v1._1 === "_") {
        return $0.pure($Tuple(empty, $Tuple(v1._2, setSet4.empty)));
      }
      const $1 = v1._1;
      return $0.pure($Tuple(
        (() => {
          const $2 = {};
          $2[$1] = v;
          return $2;
        })(),
        $Tuple(v1._2, setSet4.empty)
      ));
    }
    if (v1.tag === "ElimConstr") {
      if (v._2.tag === "Constr") {
        const $1 = v._2._1;
        const $2 = v1._1;
        const $3 = v._2;
        const $4 = v._2._2;
        const $5 = v._1;
        return Bind1.bind(withMsg2("Pattern mismatch")(consistentWith2($$$Map("Two", Leaf2, $1, void 0, Leaf2))(mapObjectString.keys($2))))(() => Bind1.bind(orElse(MonadThrow0)("Incomplete patterns: no branch for " + showCtr($1))(_lookup(
          Nothing,
          Just,
          $1,
          $2
        )))((\u03BA) => Bind1.bind(matchMany(dictMonadWithGraphAlloc)($4)(\u03BA))((v3) => $0.pure($Tuple(
          v3._1,
          $Tuple(v3._2._1, insert(ordDVertex)($Tuple($5, (k) => k(typeNameBaseVal)($3)))()(v3._2._2))
        )))));
      }
      return Bind1.bind(dataTypeForSetCtr.dataTypeFor(MonadThrow0)(mapObjectString.keys(v1._1)))((d) => MonadThrow0.throwError(error("Pattern mismatch: found " + intercalate3("\n")(removeDocWS(prettyVal(highlightableVertex).pretty(v)).lines) + ", expected " + d._1)));
    }
    if (v1.tag === "ElimDict") {
      if (v._2.tag === "Dictionary") {
        const $1 = v._2;
        const $2 = v1._1;
        const $3 = v._2._1;
        const $4 = v._1;
        const $5 = v1._2;
        return Bind1.bind(check(MonadThrow0)(difference2(ordString)($2)(fromFoldable11(mapObjectString.keys($3))).tag === "Leaf")("Pattern mismatch: found " + show22(mapObjectString.keys($3)) + ", expected " + show22($2)))(() => Bind1.bind(matchMany(dictMonadWithGraphAlloc)(listMap((k) => $$get(showString)(mapObjectString)(k)($3)._2)(toUnfoldable9($2)))($5))((v3) => $0.pure($Tuple(
          v3._1,
          $Tuple(v3._2._1, insert(ordDVertex)($Tuple($4, (k) => k(typeNameBaseVal)($1)))()(v3._2._2))
        ))));
      }
      return MonadThrow0.throwError(error("Pattern mismatch: found " + intercalate3("\n")(removeDocWS(prettyVal(highlightableVertex).pretty(v)).lines) + ", expected " + show22(v1._1)));
    }
    fail();
  };
};
var closeDefs = (dictMonadWithGraphAlloc) => {
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const traverse2 = traversableDict.traverse(Monad0.Applicative0());
  const $$new = dictMonadWithGraphAlloc.new(typeNameBaseVal);
  return (\u03B3) => (\u03C1) => (\u03B1s) => Monad0.Bind1().Apply0().Functor0().map(Env)(traverse2((\u03C3) => {
    const \u03C1$p = forDefs(\u03C1)(\u03C3);
    return $$new(Val)(addresses(\u03B1s))($BaseVal(
      "Fun",
      $Fun(
        "Closure",
        (() => {
          const $0 = union1(fv(\u03C1$p))(fVElim.fv(\u03C3));
          return filterWithKey2((x) => {
            const $1 = setSet(ordString).member(x)($0);
            return (v) => $1;
          })(\u03B3);
        })(),
        \u03C1$p,
        \u03C3
      )
    ));
  })(\u03C1));
};
var $$eval = (dictMonadWithGraphAlloc) => {
  const MonadError1 = dictMonadWithGraphAlloc.MonadError1();
  const withMsg2 = withMsg(MonadError1);
  const MonadThrow0 = MonadError1.MonadThrow0();
  const $$new = dictMonadWithGraphAlloc.new(typeNameBaseVal);
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const Bind1 = Monad0.Bind1();
  const $0 = Bind1.Apply0().Functor0();
  const Applicative0 = Monad0.Applicative0();
  const traverse2 = traversableList.traverse(Applicative0);
  const traverse3 = traversablePair.traverse(Applicative0);
  const checkArity3 = checkArity(MonadError1);
  const sequence1 = traversableArray.traverse(Applicative0)(identity7);
  const match1 = match(dictMonadWithGraphAlloc);
  const closeDefs1 = closeDefs(dictMonadWithGraphAlloc);
  return (v) => (v1) => (v2) => {
    if (v1.tag === "Var") {
      return withMsg2("Variable lookup")(lookup$p(MonadThrow0)(showString)(mapEnvStringVal)(v1._1)(v));
    }
    if (v1.tag === "Op") {
      return withMsg2("Variable lookup")(lookup$p(MonadThrow0)(showString)(mapEnvStringVal)(v1._1)(v));
    }
    if (v1.tag === "Int") {
      return $$new(Val)(insert(ordVertex)(v1._1)()(addresses(v2)))($BaseVal("Int", v1._2));
    }
    if (v1.tag === "Float") {
      return $$new(Val)(insert(ordVertex)(v1._1)()(addresses(v2)))($BaseVal("Float", v1._2));
    }
    if (v1.tag === "Str") {
      return $$new(Val)(insert(ordVertex)(v1._1)()(addresses(v2)))($BaseVal("Str", v1._2));
    }
    if (v1.tag === "Dictionary") {
      const $1 = v1._1;
      return Bind1.bind($0.map(unzip4)(traverse2(traverse3((() => {
        const $2 = $$eval(dictMonadWithGraphAlloc)(v);
        return (a) => $2(a)(v2);
      })()))(v1._2)))((v3) => {
        const v4 = unzip(listMap((v$1) => $Tuple(v$1._2.tag === "Str" ? v$1._2._1 : typeError(v$1._2)("Str"), v$1._1))(v3._1));
        return $$new(Val)(insert(ordVertex)($1)()(addresses(v2)))($BaseVal(
          "Dictionary",
          fromFoldable17(zipWith(Tuple)(v4._1)(zipWith(Tuple)(v4._2)(v3._2)))
        ));
      });
    }
    if (v1.tag === "Constr") {
      const $1 = v1._2;
      const $2 = v1._3;
      const $3 = v1._1;
      return Bind1.bind(checkArity3($1)((() => {
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
          return go$r;
        };
        return go(0)($2);
      })()))(() => Bind1.bind(traverse2((() => {
        const $4 = $$eval(dictMonadWithGraphAlloc)(v);
        return (a) => $4(a)(v2);
      })())($2))((vs) => $$new(Val)(insert(ordVertex)($3)()(addresses(v2)))($BaseVal("Constr", $1, vs))));
    }
    if (v1.tag === "Matrix") {
      const $1 = v1._2;
      const $2 = v1._3._1;
      const $3 = v1._3._2;
      const $4 = v1._1;
      return Bind1.bind($$eval(dictMonadWithGraphAlloc)(v)(v1._4)(v2))((v3) => {
        const v5 = intPair.unpack(v3._2);
        const $5 = v5._1._1;
        const $6 = v5._2._1;
        const $7 = v5._1._2;
        const $8 = v5._2._2;
        return Bind1.bind(check(MonadThrow0)(greaterThanOrEq($Tuple($5, $6))($Tuple(1, 1)))("array must be at least (" + show3($Tuple(1, 1)) + "); got (" + show3($Tuple(
          $5,
          $6
        )) + ")"))(() => Bind1.bind(sequence1(arrayBind(range2(1)($5))((i) => [
          sequence1(arrayBind(range2(1)($6))((j) => [
            $$eval(dictMonadWithGraphAlloc)(unionWith2((v$1) => identity14)(v)(disjointUnion2((() => {
              const $9 = {};
              $9[$2] = $Val($7, $BaseVal("Int", i));
              return $9;
            })())((() => {
              const $9 = {};
              $9[$3] = $Val($8, $BaseVal("Int", j));
              return $9;
            })())))($1)(v2)
          ]))
        ])))((vss) => $$new(Val)(insert(ordVertex)($4)()(addresses(v2)))($BaseVal(
          "Matrix",
          $Tuple(vss, $Tuple($Tuple($5, $7), $Tuple($6, $8)))
        ))));
      });
    }
    if (v1.tag === "Lambda") {
      return $$new(Val)(insert(ordVertex)(v1._1)()(addresses(v2)))($BaseVal(
        "Fun",
        $Fun(
          "Closure",
          (() => {
            const $1 = fVElim.fv(v1._2);
            return filterWithKey2((x) => {
              const $2 = setSet(ordString).member(x)($1);
              return (v$1) => $2;
            })(v);
          })(),
          empty,
          v1._2
        )
      ));
    }
    if (v1.tag === "Project") {
      const $1 = v1._2;
      return Bind1.bind($$eval(dictMonadWithGraphAlloc)(v)(v1._1)(v2))((v3) => {
        if (v3._2.tag === "Dictionary") {
          return withMsg2("Dict lookup")(orElse(MonadThrow0)('Key "' + $1 + '" not found')((() => {
            const $2 = _lookup(Nothing, Just, $1, v3._2._1);
            if ($2.tag === "Just") {
              return $Maybe("Just", $2._1._2);
            }
            return Nothing;
          })()));
        }
        return MonadThrow0.throwError(error("Found " + intercalate3("\n")(removeDocWS(prettyVal(highlightableVertex).pretty(v3)).lines) + ", expected dictionary"));
      });
    }
    if (v1.tag === "DProject") {
      const $1 = v1._2;
      return Bind1.bind($$eval(dictMonadWithGraphAlloc)(v)(v1._1)(v2))((v3) => Bind1.bind($$eval(dictMonadWithGraphAlloc)(v)($1)(v2))((v$p) => {
        if (v3._2.tag === "Dictionary") {
          if (v$p._2.tag === "Str") {
            return withMsg2("Dict lookup")(orElse(MonadThrow0)('Key "' + v$p._2._1 + '" not found')((() => {
              const $2 = _lookup(Nothing, Just, v$p._2._1, v3._2._1);
              if ($2.tag === "Just") {
                return $Maybe("Just", $2._1._2);
              }
              return Nothing;
            })()));
          }
          return MonadThrow0.throwError(error("Found " + intercalate3("\n")(removeDocWS(prettyVal(highlightableVertex).pretty(v$p)).lines) + ", expected string"));
        }
        return MonadThrow0.throwError(error("Found " + intercalate3("\n")(removeDocWS(prettyVal(highlightableVertex).pretty(v3)).lines) + ", expected dict"));
      }));
    }
    if (v1.tag === "App") {
      const $1 = v1._2;
      return Bind1.bind($$eval(dictMonadWithGraphAlloc)(v)(v1._1)(v2))((v3) => Bind1.bind($$eval(dictMonadWithGraphAlloc)(v)($1)(v2))((v$p) => apply2(dictMonadWithGraphAlloc)(v3)(v$p)));
    }
    if (v1.tag === "Let") {
      const $1 = v1._2;
      const $2 = v1._1._1;
      return Bind1.bind($$eval(dictMonadWithGraphAlloc)(v)(v1._1._2)(v2))((v3) => Bind1.bind(match1(v3)($2))((v4) => $$eval(dictMonadWithGraphAlloc)(unionWith2((v$1) => identity14)(v)(v4._1))($1)(v4._2._2)));
    }
    if (v1.tag === "LetRec") {
      const $1 = v1._1;
      const $2 = v1._2;
      const inserted\u03B1 = insert(ordDVertex)($Tuple(v1._1._1, (k) => k(typeNameRecDefs)($1)))()(v2);
      return Bind1.bind(closeDefs1(v)(v1._1._2)(inserted\u03B1))((\u03B3$p) => $$eval(dictMonadWithGraphAlloc)(unionWith2((v$1) => identity14)(v)(\u03B3$p))($2)(inserted\u03B1));
    }
    fail();
  };
};
var apply2 = (dictMonadWithGraphAlloc) => {
  const Bind1 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0().Bind1();
  const closeDefs1 = closeDefs(dictMonadWithGraphAlloc);
  const match1 = match(dictMonadWithGraphAlloc);
  const $$new = dictMonadWithGraphAlloc.new(typeNameBaseVal);
  const MonadError1 = dictMonadWithGraphAlloc.MonadError1();
  const MonadThrow0 = MonadError1.MonadThrow0();
  return (v) => (v1) => {
    const $0 = (v2) => MonadThrow0.throwError(error("Found " + intercalate3("\n")(removeDocWS(prettyVal(highlightableVertex).pretty(v2)).lines) + ", expected function"));
    if (v._2.tag === "Fun") {
      if (v._2._1.tag === "Closure") {
        const $1 = v._2;
        const $2 = v._1;
        const $3 = v._2._1._1;
        const $4 = v._2._1._3;
        return Bind1.bind(closeDefs1($3)(v._2._1._2)($$$Map(
          "Two",
          Leaf2,
          $Tuple($2, (k) => k(typeNameBaseVal)($1)),
          void 0,
          Leaf2
        )))((\u03B32) => Bind1.bind(match1(v1)($4))((v3) => $$eval(dictMonadWithGraphAlloc)(unionWith2((v$1) => identity14)(unionWith2((v$1) => identity14)($3)(\u03B32))(v3._1))(v3._2._1.tag === "ContExpr" ? v3._2._1._1 : throwException(error("Expression expected"))())(insert(ordDVertex)($Tuple(
          $2,
          (k) => k(typeNameBaseVal)($1)
        ))()(v3._2._2))));
      }
      if (v._2._1.tag === "Foreign") {
        const $1 = v._2._1._1._2;
        const vs$p = foldableList.foldr(Cons)($List("Cons", v1, Nil))(v._2._1._2);
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
            return go$r;
          };
          return $1._1.arity > go(0)(vs$p);
        })()) {
          return $$new(Val)($$$Map("Two", Leaf2, v._1, void 0, Leaf2))($BaseVal(
            "Fun",
            $Fun("Foreign", $Tuple(v._2._1._1._1, $1), vs$p)
          ));
        }
        return $1._1["op'"](dictMonadWithGraphAlloc)(MonadError1)(vs$p);
      }
      if (v._2._1.tag === "PartialConstr") {
        const $1 = v._1;
        const n = defined(arity(monadThrowExceptT(monadIdentity))(v._2._1._1));
        const v$p = (() => {
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
            return go$r;
          };
          return go(0)(v._2._1._2) < (n - 1 | 0);
        })() ? $BaseVal(
          "Fun",
          $Fun(
            "PartialConstr",
            v._2._1._1,
            foldableList.foldr(Cons)($List("Cons", v1, Nil))(v._2._1._2)
          )
        ) : $BaseVal("Constr", v._2._1._1, foldableList.foldr(Cons)($List("Cons", v1, Nil))(v._2._1._2));
        return Bind1.bind(check(MonadThrow0)((() => {
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
            return go$r;
          };
          return go(0)(v._2._1._2) < n;
        })())("Too many arguments to " + showCtr(v._2._1._1)))(() => $$new(Val)($$$Map(
          "Two",
          Leaf2,
          $1,
          void 0,
          Leaf2
        ))(v$p));
      }
    }
    return $0(v1);
  };
};
var eval_module = (dictMonadWithGraphAlloc) => {
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const $0 = Monad0.Bind1();
  const eval1 = $$eval(dictMonadWithGraphAlloc);
  const match1 = match(dictMonadWithGraphAlloc);
  const closeDefs1 = closeDefs(dictMonadWithGraphAlloc);
  return (\u03B3) => {
    const go = (v) => (v1) => (v2) => {
      if (v1.tag === "Nil") {
        return Monad0.Applicative0().pure(v);
      }
      if (v1.tag === "Cons") {
        if (v1._1.tag === "Left") {
          const $1 = v1._2;
          const $2 = v1._1._1._1;
          return $0.bind(eval1(unionWith2((v$1) => identity14)(\u03B3)(v))(v1._1._1._2)(v2))((v3) => $0.bind(match1(v3)($2))((v4) => go(unionWith2((v$1) => identity14)(v)(v4._1))($1)(v4._2._2)));
        }
        if (v1._1.tag === "Right") {
          const $1 = v1._1._1;
          const $2 = v1._2;
          return $0.bind(closeDefs1(unionWith2((v$1) => identity14)(\u03B3)(v))(v1._1._1._2)(insert(ordDVertex)($Tuple(
            v1._1._1._1,
            (k) => k(typeNameRecDefs)($1)
          ))()(v2)))((\u03B3$p$p) => go(unionWith2((v$1) => identity14)(v)(\u03B3$p$p))($2)(v2));
        }
      }
      fail();
    };
    return go(empty);
  };
};
var eval_progCxt = (dictMonadWithGraphAlloc) => {
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const $0 = Monad0.Bind1();
  const eval_module1 = eval_module(dictMonadWithGraphAlloc);
  const $1 = Monad0.Applicative0();
  const eval1 = $$eval(dictMonadWithGraphAlloc);
  const concatM1 = concatM(Monad0);
  return (v) => concatM1(foldableList.foldr(Cons)(listMap((v1) => (\u03B3) => {
    const $2 = v1._1;
    return $0.bind(eval1(\u03B3)(v1._2)(setSet4.empty))((v2) => $1.pure(unionWith2((v$1) => identity14)(\u03B3)((() => {
      const $3 = {};
      $3[$2] = v2;
      return $3;
    })())));
  })(reverse(v.datasets)))(listMap((mod) => (\u03B3) => $0.bind(eval_module1(\u03B3)(mod)(setSet4.empty))((\u03B3$p) => $1.pure(unionWith2((v$1) => identity14)(\u03B3)(\u03B3$p))))(reverse(v.mods))))(v.primitives);
};
var graphEval = (dictMonadError) => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  const bindStateT2 = bindStateT(Monad0);
  const $0 = monadAllocAllocT(Monad0);
  const fresh1 = $0.fresh;
  const alloc = traversableExpr.traverse($0.Monad0().Applicative0())((v) => fresh1);
  const runWithGraphT_spy2 = runWithGraphT_spy({
    Applicative0: () => applicativeStateT(Monad0),
    Bind1: () => bindStateT(Monad0)
  })(graphGraphImpl);
  const eval1 = $$eval(monadWithGraphAllocWithGr(dictMonadError));
  const check2 = check(monadThrowStateT(MonadThrow0));
  return (v) => (e) => {
    const $1 = v["\u03B3"];
    const $2 = spyFunWhen(false)("fwdSlice")((x) => $Tuple(showVertices(x._1), showEdgeList(toEdgeList(graphGraphImpl)(x._2))))(showGraph(graphGraphImpl))(fwdSlice2);
    const $3 = spyFunWhen(false)("bwdSlice")((x) => $Tuple(showVertices(x._1), showEdgeList(toEdgeList(graphGraphImpl)(x._2))))(showGraph(graphGraphImpl))(bwdSlice2);
    return Monad0.Bind1().bind(runAllocT(Monad0)(bindStateT2.bind(alloc(e))((e\u03B1) => bindStateT2.bind(runWithGraphT_spy2(eval1($1)(e\u03B1)(Leaf2))(verticesEnvExprVertex.vertices($EnvExpr(
      $1,
      e\u03B1
    ))))((v1) => {
      const $4 = v1._1;
      const $5 = v1._2;
      return bindStateT2.bind(check2(difference2(ordDVertex)(verticesValVertex.vertices($5))(verticesGraphImpl.vertices($4)).tag === "Leaf")("outputs in graph"))(() => applicativeStateT(Monad0).pure($Tuple(
        $4,
        $Tuple($EnvExpr($1, e\u03B1), $5)
      )));
    })))(v.n))((v1) => Monad0.Applicative0().pure({
      g: v1._2._2._1,
      graph_fwd: (a) => (b) => $2($Tuple(a, b)),
      graph_bwd: (a) => (b) => $3($Tuple(a, b)),
      "in\u03B1": v1._2._2._2._1,
      "out\u03B1": v1._2._2._2._2
    }));
  };
};

// output-es/Parsing.String/index.js
var updatePosSingle = (v) => (cp) => (after) => {
  if (cp === 10) {
    return { index: v.index + 1 | 0, line: v.line + 1 | 0, column: 1 };
  }
  if (cp === 13) {
    const v2 = codePointAt(0)(after);
    if (v2.tag === "Just" && v2._1 === 10) {
      return { index: v.index + 1 | 0, line: v.line, column: v.column };
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
    const v = uncons5(before);
    if (v.tag === "Nothing") {
      updatePosString$c = false;
      updatePosString$r = pos;
      continue;
    }
    if (v.tag === "Just") {
      updatePosString$a0 = v._1.tail === "" ? updatePosSingle(pos)(v._1.head)(after) : updatePosSingle(pos)(v._1.head)(v._1.tail);
      updatePosString$a1 = v._1.tail;
      updatePosString$a2 = after;
      continue;
    }
    fail();
  }
  return updatePosString$r;
};
var satisfyCodePoint = (f) => (v, $0, $1, $2, $3) => {
  const v3 = uncons5(v._1);
  if (v3.tag === "Nothing") {
    return $2(v, $ParseError("Unexpected EOF", v._2));
  }
  if (v3.tag === "Just") {
    if (f(v3._1.head)) {
      return $3($ParseState(v3._1.tail, updatePosSingle(v._2)(v3._1.head)(v3._1.tail), true), v3._1.head);
    }
    return $2(v, $ParseError("Predicate unsatisfied", v._2));
  }
  fail();
};
var satisfy = (f) => (v, $0, $1, $2, $3) => {
  const v3 = uncons5(v._1);
  if (v3.tag === "Nothing") {
    return $2(v, $ParseError("Unexpected EOF", v._2));
  }
  if (v3.tag === "Just") {
    if (v3._1.head < 0 || v3._1.head > 65535) {
      return $2(v, $ParseError("Expected Char", v._2));
    }
    if (v3._1.head >= -2147483648 && v3._1.head <= 2147483647) {
      const ch = fromCharCode(v3._1.head);
      if (f(ch)) {
        return $3($ParseState(v3._1.tail, updatePosSingle(v._2)(v3._1.head)(v3._1.tail), true), ch);
      }
      return $2(v, $ParseError("Predicate unsatisfied", v._2));
    }
  }
  fail();
};
var eof = (v, $0, $1, $2, $3) => {
  if (v._1 === "") {
    return $3($ParseState(v._1, v._2, true), void 0);
  }
  return $2(v, $ParseError("Expected EOF", v._2));
};
var consumeWith = (f) => (v, $0, $1, $2, $3) => {
  const v3 = f(v._1);
  if (v3.tag === "Left") {
    return $2(v, $ParseError(v3._1, v._2));
  }
  if (v3.tag === "Right") {
    return $3($ParseState(v3._1.remainder, updatePosString(v._2)(v3._1.consumed)(v3._1.remainder), v3._1.consumed !== ""), v3._1.value);
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

// output-es/Data.CodePoint.Unicode/index.js
var isUpper = (x) => checkAttr([512, 524288])(x);
var isSpace = (c) => {
  if (c <= 823) {
    return c === 32 || c >= 9 && c <= 13 || c === 160;
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
  return diff <= 9 && diff >= 0 || (() => {
    const diff$2 = c - 97 | 0;
    return diff$1 <= 5 && diff$1 >= 0 || diff$2 <= 5 && diff$2 >= 0;
  })();
};
var isAlphaNum = (x) => checkAttr([524288, 512, 4096, 1048576, 16384, 8388608, 4194304, 2097152, 131072, 256, 16777216])(x);
var isAlpha = (x) => checkAttr([4096, 512, 524288, 1048576, 16384])(x);
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

// output-es/Parsing.String.Basic/index.js
var show1 = /* @__PURE__ */ showArrayImpl(showCharImpl);
var satisfyCP = (p) => satisfy((x) => p(toCharCode(x)));
var space = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isSpace))("space");
var upper2 = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isUpper))("uppercase letter");
var oneOf = (ss) => withLazyErrorMessage(satisfy((a) => elem(eqChar)(a)(ss)))((v) => "one of " + show1(ss));
var octDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isOctDigit))("oct digit");
var noneOf = (ss) => withLazyErrorMessage(satisfy((a) => notElem(eqChar)(a)(ss)))((v) => "none of " + show1(ss));
var letter = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlpha))("letter");
var hexDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isHexDigit))("hex digit");
var digit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isDecDigit))("digit");
var alphaNum = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlphaNum))("letter or digit");

// output-es/Data.String.Unicode/index.js
var convert = (f) => {
  const $0 = arrayMap(f);
  return (x) => fromCodePointArray($0(toCodePointArray(x)));
};
var toLowerSimple = /* @__PURE__ */ convert(uTowlower);
var toUpperSimple = /* @__PURE__ */ convert(uTowupper);

// output-es/Parsing.Token/index.js
var identity23 = (x) => x;
var choice3 = /* @__PURE__ */ choice(foldableArray);
var toUnfoldable10 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
var theReservedNames = (v) => {
  if (v.caseSensitive) {
    return sortBy2(ordString.compare)(v.reservedNames);
  }
  return sortBy2(ordString.compare)(arrayMap(toLower)(v.reservedNames));
};
var oneLineComment = (v) => {
  const $0 = skipMany(satisfy((v1) => v1 !== "\n"));
  return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => {
    const $1 = state1._3;
    return string2(v.commentLine)(
      state1,
      more,
      lift1,
      (v2$1, $2) => $$throw2($ParseState(v2$1._1, v2$1._2, $1), $2),
      (state2, a) => more((v2$1) => more((v3) => $0(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
    );
  }));
};
var isReserved = (isReserved$a0$copy) => (isReserved$a1$copy) => {
  let isReserved$a0 = isReserved$a0$copy, isReserved$a1 = isReserved$a1$copy, isReserved$c = true, isReserved$r;
  while (isReserved$c) {
    const names = isReserved$a0, name2 = isReserved$a1;
    const v = uncons2(names);
    if (v.tag === "Nothing") {
      isReserved$c = false;
      isReserved$r = false;
      continue;
    }
    if (v.tag === "Just") {
      const v1 = ordString.compare(v._1.head)(name2);
      if (v1 === "LT") {
        isReserved$a0 = v._1.tail;
        isReserved$a1 = name2;
        continue;
      }
      if (v1 === "EQ") {
        isReserved$c = false;
        isReserved$r = true;
        continue;
      }
      if (v1 === "GT") {
        isReserved$c = false;
        isReserved$r = false;
        continue;
      }
    }
    fail();
  }
  return isReserved$r;
};
var inCommentSingle = (v) => {
  const startEnd = [...toCharArray(v.commentEnd), ...toCharArray(v.commentStart)];
  const go$lazy = binding(() => lazyParserT.defer((v$1) => {
    const $0 = skipMany1(noneOf(startEnd));
    const $1 = withErrorMessage((() => {
      const $12 = oneOf(startEnd);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => $12(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => go$lazy()(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
      )));
    })())("end of comment");
    return (v2, $2, $3, $4, $5) => {
      const $6 = v2._1;
      const $7 = v2._2;
      return $2((v3) => $2((v1) => string2(v.commentEnd)(
        $ParseState($6, $7, false),
        $2,
        $3,
        (v2$1, $8) => $2((v5) => {
          const $9 = v2._1;
          const $10 = v2._2;
          return $2((v3$1) => {
            const $11 = (v4, $112) => {
              const $12 = v4._3;
              return $2((v5$1) => {
                if ($12) {
                  return $4(v4, $112);
                }
                return $1(v2, $2, $3, $4, $5);
              });
            };
            return $2((v2$2) => $2((v1$1) => $0(
              $ParseState($9, $10, false),
              $2,
              $3,
              $11,
              (state2, a) => $2((v2$3) => $2((v3$2) => go$lazy()(state2, $2, $3, $11, (state3, a$1) => $2((v4) => $5(state3, a$1)))))
            )));
          });
        }),
        (state2, a) => $2((v2$1) => $5(state2, void 0))
      )));
    };
  }));
  const go = go$lazy();
  return go;
};
var multiLineComment = (v) => {
  const $0 = inComment(v);
  return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => {
    const $1 = state1._3;
    return string2(v.commentStart)(
      state1,
      more,
      lift1,
      (v2$1, $2) => $$throw2($ParseState(v2$1._1, v2$1._2, $1), $2),
      (state2, a) => more((v2$1) => more((v3) => $0(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
    );
  }));
};
var inCommentMulti = (v) => {
  const startEnd = [...toCharArray(v.commentEnd), ...toCharArray(v.commentStart)];
  const go$lazy = binding(() => lazyParserT.defer((v$1) => {
    const $0 = multiLineComment(v);
    const $1 = skipMany1(noneOf(startEnd));
    const $2 = withErrorMessage((() => {
      const $22 = oneOf(startEnd);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => $22(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => go$lazy()(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
      )));
    })())("end of comment");
    return (v2, $3, $4, $5, $6) => {
      const $7 = v2._1;
      const $8 = v2._2;
      return $3((v3) => $3((v1) => string2(v.commentEnd)(
        $ParseState($7, $8, false),
        $3,
        $4,
        (v2$1, $9) => $3((v5) => {
          const $10 = v2._1;
          const $11 = v2._2;
          return $3((v3$1) => {
            const $12 = (v4, $122) => {
              const $13 = v4._3;
              return $3((v5$1) => {
                if ($13) {
                  return $5(v4, $122);
                }
                const $14 = v2._1;
                const $15 = v2._2;
                return $3((v3$2) => {
                  const $16 = (v4$1, $162) => {
                    const $17 = v4$1._3;
                    return $3((v5$2) => {
                      if ($17) {
                        return $5(v4$1, $162);
                      }
                      return $2(v2, $3, $4, $5, $6);
                    });
                  };
                  return $3((v2$2) => $3((v1$1) => $1(
                    $ParseState($14, $15, false),
                    $3,
                    $4,
                    $16,
                    (state2, a) => $3((v2$3) => $3((v3$3) => go$lazy()(state2, $3, $4, $16, (state3, a$1) => $3((v4$1) => $6(state3, a$1)))))
                  )));
                });
              });
            };
            return $3((v2$2) => $3((v1$1) => $0(
              $ParseState($10, $11, false),
              $3,
              $4,
              $12,
              (state2, a) => $3((v2$3) => $3((v3$2) => go$lazy()(state2, $3, $4, $12, (state3, a$1) => $3((v4) => $6(state3, a$1)))))
            )));
          });
        }),
        (state2, a) => $3((v2$1) => $6(state2, void 0))
      )));
    };
  }));
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
    return skipMany((() => {
      const $0 = withErrorMessage(multiLineComment(v))("");
      return (v2, $1, $2, $3, $4) => {
        const $5 = v2._1;
        const $6 = v2._2;
        return $1((v3) => skipMany1(satisfyCodePoint(isSpace))(
          $ParseState($5, $6, false),
          $1,
          $2,
          (v4, $7) => {
            const $8 = v4._3;
            return $1((v5) => {
              if ($8) {
                return $3(v4, $7);
              }
              return $0(v2, $1, $2, $3, $4);
            });
          },
          $4
        ));
      };
    })());
  }
  if (v.commentStart === "") {
    return skipMany((() => {
      const $0 = withErrorMessage(oneLineComment(v))("");
      return (v2, $1, $2, $3, $4) => {
        const $5 = v2._1;
        const $6 = v2._2;
        return $1((v3) => skipMany1(satisfyCodePoint(isSpace))(
          $ParseState($5, $6, false),
          $1,
          $2,
          (v4, $7) => {
            const $8 = v4._3;
            return $1((v5) => {
              if ($8) {
                return $3(v4, $7);
              }
              return $0(v2, $1, $2, $3, $4);
            });
          },
          $4
        ));
      };
    })());
  }
  return skipMany((() => {
    const $0 = oneLineComment(v);
    const $1 = withErrorMessage(multiLineComment(v))("");
    return (v2, $2, $3, $4, $5) => {
      const $6 = v2._1;
      const $7 = v2._2;
      return $2((v3) => skipMany1(satisfyCodePoint(isSpace))(
        $ParseState($6, $7, false),
        $2,
        $3,
        (v4, $8) => {
          const $9 = v4._3;
          return $2((v5) => {
            if ($9) {
              return $4(v4, $8);
            }
            const $10 = v2._1;
            const $11 = v2._2;
            return $2((v3$1) => $0(
              $ParseState($10, $11, false),
              $2,
              $3,
              (v4$1, $12) => {
                const $13 = v4$1._3;
                return $2((v5$1) => {
                  if ($13) {
                    return $4(v4$1, $12);
                  }
                  return $1(v2, $2, $3, $4, $5);
                });
              },
              $5
            ));
          });
        },
        $5
      ));
    };
  })());
};
var makeTokenParser = (v) => {
  const $0 = withErrorMessage(satisfy((v$1) => v$1 === "-"))("'-'");
  const $1 = withErrorMessage(satisfy((v$1) => v$1 === "+"))("'+'");
  const sign1 = (v2, $22, $32, $42, $52) => {
    const $62 = v2._1;
    const $72 = v2._2;
    return $22((v3) => $22((v1) => $0(
      $ParseState($62, $72, false),
      $22,
      $32,
      (v4, $82) => {
        const $92 = v4._3;
        return $22((v5) => {
          if ($92) {
            return $42(v4, $82);
          }
          const $102 = v2._1;
          const $112 = v2._2;
          return $22((v3$1) => $22((v1$1) => $1(
            $ParseState($102, $112, false),
            $22,
            $32,
            (v4$1, $122) => {
              const $132 = v4$1._3;
              return $22((v5$1) => {
                if ($132) {
                  return $42(v4$1, $122);
                }
                return $52(v2, identity23);
              });
            },
            (state2, a) => $22((v2$1) => $52(state2, identity23))
          )));
        });
      },
      (state2, a) => $22((v2$1) => $52(state2, (a$1) => -a$1))
    )));
  };
  const $2 = oneOf(["o", "O"]);
  const $3 = some2(alternativeParserT)(lazyParserT)(octDigit);
  const octal = (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => $2(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => more((v1$1) => $3(
      state2,
      more,
      lift1,
      $$throw2,
      (state2$1, a$1) => more((v2$2) => {
        const $42 = foldlArray((v1$2) => (v2$3) => {
          if (v1$2.tag === "Nothing") {
            return Nothing;
          }
          if (v1$2.tag === "Just") {
            const $43 = hexDigitToInt(toCharCode(v2$3));
            if ($43.tag === "Just") {
              return $Maybe("Just", (8 * v1$2._1 | 0) + $43._1 | 0);
            }
            return Nothing;
          }
          fail();
        })($Maybe("Just", 0))(a$1);
        if ($42.tag === "Nothing") {
          return fail2("not digits")(state2$1, more, lift1, $$throw2, (state3, a$2) => more((v4) => done(state3, a$2)));
        }
        if ($42.tag === "Just") {
          const $52 = $42._1;
          return more((v4) => done(state2$1, $52));
        }
        fail();
      })
    ))))
  )));
  const $4 = whiteSpace$p(v);
  const semi2 = (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(";")(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => $4(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ";"))))))
  ))));
  const $5 = oneOf(["x", "X"]);
  const $6 = some2(alternativeParserT)(lazyParserT)(hexDigit);
  const hexadecimal = (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => $5(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => more((v1$1) => $6(
      state2,
      more,
      lift1,
      $$throw2,
      (state2$1, a$1) => more((v2$2) => {
        const $72 = foldlArray((v1$2) => (v2$3) => {
          if (v1$2.tag === "Nothing") {
            return Nothing;
          }
          if (v1$2.tag === "Just") {
            const $73 = hexDigitToInt(toCharCode(v2$3));
            if ($73.tag === "Just") {
              return $Maybe("Just", (16 * v1$2._1 | 0) + $73._1 | 0);
            }
            return Nothing;
          }
          fail();
        })($Maybe("Just", 0))(a$1);
        if ($72.tag === "Nothing") {
          return fail2("not digits")(state2$1, more, lift1, $$throw2, (state3, a$2) => more((v4) => done(state3, a$2)));
        }
        if ($72.tag === "Just") {
          const $82 = $72._1;
          return more((v4) => done(state2$1, $82));
        }
        fail();
      })
    ))))
  )));
  const fraction = withErrorMessage((() => {
    const $72 = withErrorMessage(satisfy((v$1) => v$1 === "."))("'.'");
    return (state1, more, lift1, $$throw2, done) => more((v1) => $72(
      state1,
      more,
      lift1,
      $$throw2,
      (state2, a) => more((v2) => {
        const $82 = withErrorMessage(some2(alternativeParserT)(lazyParserT)(digit))("fraction");
        return more((v1$1) => $82(
          state2,
          more,
          lift1,
          $$throw2,
          (state2$1, a$1) => more((v2$1) => {
            const $92 = foldrArray((v1$2) => (v2$2) => {
              if (v2$2.tag === "Nothing") {
                return Nothing;
              }
              if (v2$2.tag === "Just") {
                const $93 = hexDigitToInt(toCharCode(v1$2));
                if ($93.tag === "Just") {
                  return $Maybe("Just", (v2$2._1 + toNumber($93._1)) / 10);
                }
                if ($93.tag === "Nothing") {
                  return Nothing;
                }
              }
              fail();
            })($Maybe("Just", 0))(a$1);
            if ($92.tag === "Nothing") {
              return fail2("not digit")(state2$1, more, lift1, $$throw2, done);
            }
            if ($92.tag === "Just") {
              return done(state2$1, $92._1);
            }
            fail();
          })
        ));
      })
    ));
  })())("fraction");
  const escapeGap = withErrorMessage((() => {
    const $72 = some2(alternativeParserT)(lazyParserT)(space);
    const $82 = withErrorMessage(satisfy((v$1) => v$1 === "\\"))("'\\\\'");
    return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => $72(
      state1,
      more,
      lift1,
      $$throw2,
      (state2, a) => more((v2$1) => more((v3) => $82(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
    )));
  })())("end of string gap");
  const escapeEmpty = withErrorMessage(satisfy((v$1) => v$1 === "&"))("'&'");
  const $7 = some2(alternativeParserT)(lazyParserT)(digit);
  const decimal = (state1, more, lift1, $$throw2, done) => more((v1) => $7(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2) => {
      const $82 = foldlArray((v1$1) => (v2$1) => {
        if (v1$1.tag === "Nothing") {
          return Nothing;
        }
        if (v1$1.tag === "Just") {
          const $83 = hexDigitToInt(toCharCode(v2$1));
          if ($83.tag === "Just") {
            return $Maybe("Just", (10 * v1$1._1 | 0) + $83._1 | 0);
          }
          return Nothing;
        }
        fail();
      })($Maybe("Just", 0))(a);
      if ($82.tag === "Nothing") {
        return fail2("not digits")(state2, more, lift1, $$throw2, done);
      }
      if ($82.tag === "Just") {
        return done(state2, $82._1);
      }
      fail();
    })
  ));
  const power = (e) => {
    if (e < 0) {
      return 1 / power(-e);
    }
    return pow(10)(toNumber(e));
  };
  const $8 = oneOf(["e", "E"]);
  const exponent$p = withErrorMessage((state1, more, lift1, $$throw2, done) => more((v1) => $8(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2) => more((v1$1) => sign1(
      state2,
      more,
      lift1,
      $$throw2,
      (state2$1, a$1) => more((v2$1) => {
        const $92 = withErrorMessage(decimal)("exponent");
        return more((v1$2) => $92(state2$1, more, lift1, $$throw2, (state2$2, a$2) => more((v2$2) => done(state2$2, power(a$1(a$2))))));
      })
    )))
  )))("exponent");
  const fractExponent = (n) => (v2, $92, $102, $112, $122) => {
    const $132 = v2._1;
    const $142 = v2._2;
    return $92((v3) => {
      const $152 = (v4, $153) => {
        const $162 = v4._3;
        return $92((v5) => {
          if ($162) {
            return $112(v4, $153);
          }
          return $92((v1) => exponent$p(v2, $92, $102, $112, (state2, a) => $92((v2$1) => $122(state2, toNumber(n) * a))));
        });
      };
      return $92((v1) => fraction(
        $ParseState($132, $142, false),
        $92,
        $102,
        $152,
        (state2, a) => $92((v2$1) => $92((v1$1) => {
          const $162 = (state2$1, a$1) => $92((v2$2) => $122(state2$1, (toNumber(n) + a) * a$1));
          const $17 = state2._1;
          const $18 = state2._2;
          return $92((v3$1) => exponent$p(
            $ParseState($17, $18, false),
            $92,
            $102,
            (v4, $19) => {
              const $20 = v4._3;
              return $92((v5) => {
                if ($20) {
                  return $152(v4, $19);
                }
                return $162(state2, 1);
              });
            },
            $162
          ));
        }))
      ));
    });
  };
  const decimalFloat = (state1, more, lift1, $$throw2, done) => more((v1) => decimal(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2) => {
      const $92 = fractExponent(a);
      const $102 = state2._1;
      const $112 = state2._2;
      return more((v3) => more((v1$1) => $92(
        $ParseState($102, $112, false),
        more,
        lift1,
        (v4, $122) => {
          const $132 = v4._3;
          return more((v5) => {
            if ($132) {
              return $$throw2(v4, $122);
            }
            return done(state2, $Either("Left", a));
          });
        },
        (state2$1, a$1) => more((v2$1) => done(state2$1, $Either("Right", a$1)))
      )));
    })
  ));
  const zeroNumber = withErrorMessage((() => {
    const $92 = withErrorMessage(satisfy((v$1) => v$1 === "0"))("'0'");
    return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => $92(
      state1,
      more,
      lift1,
      $$throw2,
      (state2, a) => more((v2$1) => more((v3) => {
        const $102 = state2._1;
        const $112 = state2._2;
        return more((v3$1) => hexadecimal(
          $ParseState($102, $112, false),
          more,
          lift1,
          (v4, $122) => {
            const $132 = v4._3;
            return more((v5) => {
              if ($132) {
                return $$throw2(v4, $122);
              }
              const $142 = state2._1;
              const $152 = state2._2;
              return more((v3$2) => octal(
                $ParseState($142, $152, false),
                more,
                lift1,
                (v4$1, $162) => {
                  const $17 = v4$1._3;
                  return more((v5$1) => {
                    if ($17) {
                      return $$throw2(v4$1, $162);
                    }
                    const $18 = state2._1;
                    const $19 = state2._2;
                    return more((v3$3) => decimal(
                      $ParseState($18, $19, false),
                      more,
                      lift1,
                      (v4$2, $20) => {
                        const $21 = v4$2._3;
                        return more((v5$2) => {
                          if ($21) {
                            return $$throw2(v4$2, $20);
                          }
                          return more((v4$3) => done(state2, 0));
                        });
                      },
                      (state3, a$1) => more((v4$2) => done(state3, a$1))
                    ));
                  });
                },
                (state3, a$1) => more((v4$1) => done(state3, a$1))
              ));
            });
          },
          (state3, a$1) => more((v4) => done(state3, a$1))
        ));
      }))
    )));
  })())("");
  const $9 = whiteSpace$p(v);
  const comma2 = (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(",")(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => $9(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ","))))))
  ))));
  const $10 = choice3(arrayMap((v1) => {
    const $102 = v1._1;
    const $112 = v1._2;
    const $122 = withErrorMessage(satisfy((v$1) => v$1 === $102))(showCharImpl($102));
    return (state1, more, lift1, $$throw2, done) => more((v1$1) => $122(state1, more, lift1, $$throw2, (state2, a) => more((v2) => done(state2, $112))));
  })(zip(["a", "b", "f", "n", "r", "t", "v", "\\", '"', "'"])(["\x07", "\b", "\f", "\n", "\r", "	", "\v", "\\", '"', "'"])));
  const $11 = withErrorMessage(satisfy((v$1) => v$1 === "o"))("'o'");
  const $12 = some2(alternativeParserT)(lazyParserT)(octDigit);
  const $13 = withErrorMessage(satisfy((v$1) => v$1 === "x"))("'x'");
  const $14 = some2(alternativeParserT)(lazyParserT)(hexDigit);
  const $15 = choice3(arrayMap((v1) => {
    const $152 = v1._2;
    return (v1$1, $162, $17, $18, $19) => {
      const $20 = v1$1._3;
      return $162((v1$2) => string2(v1._1)(v1$1, $162, $17, (v2, $21) => $18($ParseState(v2._1, v2._2, $20), $21), (state2, a) => $162((v2) => $19(state2, $152))));
    };
  })(zip([
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
  const $16 = withErrorMessage((() => {
    const $162 = withErrorMessage(satisfy((v$1) => v$1 === "^"))("'^'");
    return (state1, more, lift1, $$throw2, done) => more((v1) => $162(
      state1,
      more,
      lift1,
      $$throw2,
      (state2, a) => more((v2) => more((v1$1) => upper2(
        state2,
        more,
        lift1,
        $$throw2,
        (state2$1, a$1) => more((v2$1) => {
          const $17 = (toCharCode(a$1) - 65 | 0) + 1 | 0;
          if ($17 >= -2147483648 && $17 <= 2147483647) {
            return done(state2$1, fromCharCode($17));
          }
          return fail2("invalid character code (should not happen)")(state2$1, more, lift1, $$throw2, done);
        })
      )))
    ));
  })())("escape code");
  const escapeCode = (v2, $17, $18, $19, $20) => {
    const $21 = v2._1;
    const $22 = v2._2;
    return $17((v3) => $10(
      $ParseState($21, $22, false),
      $17,
      $18,
      (v4, $23) => {
        const $24 = v4._3;
        return $17((v5) => {
          if ($24) {
            return $19(v4, $23);
          }
          const $25 = v2._1;
          const $26 = v2._2;
          return $17((v3$1) => {
            const $27 = (v4$1, $272) => {
              const $28 = v4$1._3;
              return $17((v5$1) => {
                if ($28) {
                  return $19(v4$1, $272);
                }
                const $29 = v2._1;
                const $30 = v2._2;
                return $17((v3$2) => $15(
                  $ParseState($29, $30, false),
                  $17,
                  $18,
                  (v4$2, $31) => {
                    const $32 = v4$2._3;
                    return $17((v5$2) => {
                      if ($32) {
                        return $19(v4$2, $31);
                      }
                      return $16(v2, $17, $18, $19, $20);
                    });
                  },
                  $20
                ));
              });
            };
            return $17((v1) => $17((v3$2) => decimal(
              $ParseState($25, $26, false),
              $17,
              $18,
              (v4$1, $28) => {
                const $29 = v4$1._3;
                return $17((v5$1) => {
                  if ($29) {
                    return $27(v4$1, $28);
                  }
                  const $30 = (state2, a) => $17((v2$1) => {
                    if (a > 1114111) {
                      return fail2("invalid escape sequence")(state2, $17, $18, $27, $20);
                    }
                    if (a >= -2147483648 && a <= 2147483647) {
                      return $20(state2, fromCharCode(a));
                    }
                    return fail2("invalid character code (should not happen)")(state2, $17, $18, $27, $20);
                  });
                  return $17((v3$3) => {
                    const $31 = (v4$2, $312) => {
                      const $32 = v4$2._3;
                      return $17((v5$2) => {
                        if ($32) {
                          return $27(v4$2, $312);
                        }
                        return $17((v2$1) => $17((v1$1) => $13(
                          $ParseState($25, $26, false),
                          $17,
                          $18,
                          $27,
                          (state2, a) => $17((v2$2) => $17((v3$4) => $17((v1$2) => $14(
                            state2,
                            $17,
                            $18,
                            $27,
                            (state2$1, a$1) => $17((v2$3) => {
                              const $33 = foldlArray((v1$3) => (v2$4) => {
                                if (v1$3.tag === "Nothing") {
                                  return Nothing;
                                }
                                if (v1$3.tag === "Just") {
                                  const $332 = hexDigitToInt(toCharCode(v2$4));
                                  if ($332.tag === "Just") {
                                    return $Maybe("Just", (16 * v1$3._1 | 0) + $332._1 | 0);
                                  }
                                  return Nothing;
                                }
                                fail();
                              })($Maybe("Just", 0))(a$1);
                              if ($33.tag === "Nothing") {
                                return fail2("not digits")(state2$1, $17, $18, $27, (state3, a$2) => $17((v4$3) => $30(state3, a$2)));
                              }
                              if ($33.tag === "Just") {
                                const $34 = $33._1;
                                return $17((v4$3) => $30(state2$1, $34));
                              }
                              fail();
                            })
                          ))))
                        )));
                      });
                    };
                    return $17((v2$1) => $17((v1$1) => $11(
                      $ParseState($25, $26, false),
                      $17,
                      $18,
                      $31,
                      (state2, a) => $17((v2$2) => $17((v3$4) => $17((v1$2) => $12(
                        state2,
                        $17,
                        $18,
                        $31,
                        (state2$1, a$1) => $17((v2$3) => {
                          const $32 = foldlArray((v1$3) => (v2$4) => {
                            if (v1$3.tag === "Nothing") {
                              return Nothing;
                            }
                            if (v1$3.tag === "Just") {
                              const $322 = hexDigitToInt(toCharCode(v2$4));
                              if ($322.tag === "Just") {
                                return $Maybe("Just", (8 * v1$3._1 | 0) + $322._1 | 0);
                              }
                              return Nothing;
                            }
                            fail();
                          })($Maybe("Just", 0))(a$1);
                          if ($32.tag === "Nothing") {
                            return fail2("not digits")(state2$1, $17, $18, $31, (state3, a$2) => $17((v4$2) => $30(state3, a$2)));
                          }
                          if ($32.tag === "Just") {
                            const $33 = $32._1;
                            return $17((v4$2) => $30(state2$1, $33));
                          }
                          fail();
                        })
                      ))))
                    )));
                  });
                });
              },
              (state2, a) => $17((v2$1) => {
                if (a > 1114111) {
                  return fail2("invalid escape sequence")(state2, $17, $18, $27, $20);
                }
                if (a >= -2147483648 && a <= 2147483647) {
                  return $20(state2, fromCharCode(a));
                }
                return fail2("invalid character code (should not happen)")(state2, $17, $18, $27, $20);
              })
            )));
          });
        });
      },
      $20
    ));
  };
  return {
    identifier: (() => {
      const $17 = withErrorMessage((state1, more, lift1, $$throw2, done) => more((v1) => v.identStart(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2) => {
          const $172 = many2(alternativeParserT)(lazyParserT)(v.identLetter);
          return more((v1$1) => $172(
            state2,
            more,
            lift1,
            $$throw2,
            (state2$1, a$1) => more((v2$1) => done(state2$1, singleton(a) + fromCharArray(a$1)))
          ));
        })
      )))("identifier");
      const $18 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => {
        const $19 = (state2, a) => more((v2$1) => more((v3) => $18(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))));
        const $20 = state1._3;
        return more((v1$1) => $17(
          state1,
          more,
          lift1,
          (v2$1, $21) => $$throw2($ParseState(v2$1._1, v2$1._2, $20), $21),
          (state2, a) => more((v2$1) => {
            if (isReserved(theReservedNames(v))(v.caseSensitive ? a : toLower(a))) {
              return fail2("reserved word " + showStringImpl(a))(
                state2,
                more,
                lift1,
                (v2$2, $21) => $$throw2($ParseState(v2$2._1, v2$2._2, $20), $21),
                $19
              );
            }
            return $19(state2, a);
          })
        ));
      }));
    })(),
    reserved: (name2) => {
      const $17 = (() => {
        if (v.caseSensitive) {
          return (state1, more, lift1, $$throw2, done) => more((v1) => string2(name2)(state1, more, lift1, $$throw2, (state2, a) => more((v2) => done(state2, name2))));
        }
        const msg = showStringImpl(name2);
        const walk = (name$p) => {
          const v1 = uncons(name$p);
          if (v1.tag === "Nothing") {
            return (state1, v$1, v1$1, v2, done) => done(state1, void 0);
          }
          if (v1.tag === "Just") {
            const $173 = withErrorMessage((() => {
              if (checkAttr([4096, 512, 524288, 1048576, 16384])(toCharCode(v1._1.head))) {
                const $174 = toChar(toLowerSimple(singleton(v1._1.head)));
                if ($174.tag === "Just") {
                  const $183 = toChar(toUpperSimple(singleton(v1._1.head)));
                  if ($183.tag === "Just") {
                    const $192 = $183._1;
                    const $20 = withErrorMessage(satisfy((v$1) => v$1 === $174._1))(showCharImpl($174._1));
                    const $21 = withErrorMessage(satisfy((v$1) => v$1 === $192))(showCharImpl($192));
                    return (v2, $22, $23, $24, $25) => {
                      const $26 = v2._1;
                      const $27 = v2._2;
                      return $22((v3) => $20(
                        $ParseState($26, $27, false),
                        $22,
                        $23,
                        (v4, $28) => {
                          const $29 = v4._3;
                          return $22((v5) => {
                            if ($29) {
                              return $24(v4, $28);
                            }
                            return $21(v2, $22, $23, $24, $25);
                          });
                        },
                        $25
                      ));
                    };
                  }
                }
              }
              return withErrorMessage(satisfy((v$1) => v$1 === v1._1.head))(showCharImpl(v1._1.head));
            })())(msg);
            const $182 = walk(v1._1.tail);
            return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1$1) => $173(
              state1,
              more,
              lift1,
              $$throw2,
              (state2, a) => more((v2$1) => more((v3) => $182(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
            )));
          }
          fail();
        };
        const $172 = walk(name2);
        return (state1, more, lift1, $$throw2, done) => more((v1) => $172(state1, more, lift1, $$throw2, (state2, a) => more((v2) => done(state2, name2))));
      })();
      const $18 = withErrorMessage(notFollowedBy(v.identLetter))("end of " + name2);
      const $19 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => {
        const $20 = state1._3;
        return more((v2$1) => more((v1$1) => $17(
          state1,
          more,
          lift1,
          (v2$2, $21) => $$throw2($ParseState(v2$2._1, v2$2._2, $20), $21),
          (state2, a) => more((v2$2) => more((v3) => $18(
            state2,
            more,
            lift1,
            (v2$3, $21) => $$throw2($ParseState(v2$3._1, v2$3._2, $20), $21),
            (state3, a$1) => more((v4) => more((v2$3) => more((v3$1) => $19(state3, more, lift1, $$throw2, (state3$1, a$2) => more((v4$1) => done(state3$1, a$1))))))
          )))
        )));
      }));
    },
    operator: (() => {
      const $17 = withErrorMessage((state1, more, lift1, $$throw2, done) => more((v1) => v.opStart(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2) => {
          const $172 = many2(alternativeParserT)(lazyParserT)(v.opLetter);
          return more((v1$1) => $172(
            state2,
            more,
            lift1,
            $$throw2,
            (state2$1, a$1) => more((v2$1) => done(state2$1, singleton(a) + fromCharArray(a$1)))
          ));
        })
      )))("operator");
      const $18 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => {
        const $19 = (state2, a) => more((v2$1) => more((v3) => $18(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))));
        const $20 = state1._3;
        return more((v1$1) => $17(
          state1,
          more,
          lift1,
          (v2$1, $21) => $$throw2($ParseState(v2$1._1, v2$1._2, $20), $21),
          (state2, a) => more((v2$1) => {
            if (isReserved(sortBy2(ordString.compare)(v.reservedOpNames))(a)) {
              return fail2("reserved operator " + a)(state2, more, lift1, (v2$2, $21) => $$throw2($ParseState(v2$2._1, v2$2._2, $20), $21), $19);
            }
            return $19(state2, a);
          })
        ));
      }));
    })(),
    reservedOp: (name2) => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => {
        const $18 = state1._3;
        return more((v1$1) => string2(name2)(
          state1,
          more,
          lift1,
          (v2$1, $19) => $$throw2($ParseState(v2$1._1, v2$1._2, $18), $19),
          (state2, a) => more((v2$1) => withErrorMessage(notFollowedBy(v.opLetter))("end of " + name2)(
            state2,
            more,
            lift1,
            (v2$2, $19) => $$throw2($ParseState(v2$2._1, v2$2._2, $18), $19),
            (state2$1, a$1) => more((v2$2) => more((v3) => $17(state2$1, more, lift1, $$throw2, (state3, a$2) => more((v4) => done(state3, a$1)))))
          ))
        ));
      }));
    },
    charLiteral: withErrorMessage((() => {
      const $17 = between(withErrorMessage(satisfy((v$1) => v$1 === "'"))("'\\''"))(withErrorMessage(withErrorMessage(satisfy((v$1) => v$1 === "'"))("'\\''"))("end of character"))((() => {
        const $172 = satisfy((c) => c !== "'" && c !== "\\" && c > "");
        const $182 = withErrorMessage((() => {
          const $183 = withErrorMessage(satisfy((v$1) => v$1 === "\\"))("'\\\\'");
          return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => $183(
            state1,
            more,
            lift1,
            $$throw2,
            (state2, a) => more((v2$1) => more((v3) => escapeCode(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
          )));
        })())("literal character");
        return (v2, $19, $20, $21, $22) => {
          const $23 = v2._1;
          const $24 = v2._2;
          return $19((v3) => $172(
            $ParseState($23, $24, false),
            $19,
            $20,
            (v4, $25) => {
              const $26 = v4._3;
              return $19((v5) => {
                if ($26) {
                  return $21(v4, $25);
                }
                return $182(v2, $19, $20, $21, $22);
              });
            },
            $22
          ));
        };
      })());
      const $18 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => $17(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $18(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))))
      )));
    })())("character"),
    stringLiteral: (() => {
      const $17 = withErrorMessage((() => {
        const $172 = between(withErrorMessage(satisfy((v$1) => v$1 === '"'))(`'"'`))(withErrorMessage(withErrorMessage(satisfy((v$1) => v$1 === '"'))(`'"'`))("end of string"))(many(alternativeParserT)(lazyParserT)((() => {
          const $173 = satisfy((c) => c !== '"' && c !== "\\" && c > "");
          const $182 = withErrorMessage((() => {
            const $183 = withErrorMessage(satisfy((v$1) => v$1 === "\\"))("'\\\\'");
            return (state1, more, lift1, $$throw2, done) => more((v1) => $183(
              state1,
              more,
              lift1,
              $$throw2,
              (state2, a) => more((v2) => {
                const $19 = state2._1;
                const $20 = state2._2;
                return more((v3) => more((v1$1) => escapeGap(
                  $ParseState($19, $20, false),
                  more,
                  lift1,
                  (v4, $21) => {
                    const $22 = v4._3;
                    return more((v5) => {
                      if ($22) {
                        return $$throw2(v4, $21);
                      }
                      const $23 = state2._1;
                      const $24 = state2._2;
                      return more((v3$1) => more((v1$2) => escapeEmpty(
                        $ParseState($23, $24, false),
                        more,
                        lift1,
                        (v4$1, $25) => {
                          const $26 = v4$1._3;
                          return more((v5$1) => {
                            if ($26) {
                              return $$throw2(v4$1, $25);
                            }
                            return more((v1$3) => escapeCode(state2, more, lift1, $$throw2, (state2$1, a$1) => more((v2$1) => done(state2$1, $Maybe("Just", a$1)))));
                          });
                        },
                        (state2$1, a$1) => more((v2$1) => done(state2$1, Nothing))
                      )));
                    });
                  },
                  (state2$1, a$1) => more((v2$1) => done(state2$1, Nothing))
                )));
              })
            ));
          })())("string character");
          return (v2, $19, $20, $21, $22) => {
            const $23 = v2._1;
            const $24 = v2._2;
            return $19((v3) => $19((v1) => $173(
              $ParseState($23, $24, false),
              $19,
              $20,
              (v4, $25) => {
                const $26 = v4._3;
                return $19((v5) => {
                  if ($26) {
                    return $21(v4, $25);
                  }
                  return $182(v2, $19, $20, $21, $22);
                });
              },
              (state2, a) => $19((v2$1) => $22(state2, $Maybe("Just", a)))
            )));
          };
        })()));
        return (state1, more, lift1, $$throw2, done) => more((v1) => $172(
          state1,
          more,
          lift1,
          $$throw2,
          (state2, a) => more((v2) => done(
            state2,
            fromCharArray(toUnfoldable10(foldableList.foldr((v1$1) => (v2$1) => {
              if (v1$1.tag === "Nothing") {
                return v2$1;
              }
              if (v1$1.tag === "Just") {
                return $List("Cons", v1$1._1, v2$1);
              }
              fail();
            })(Nil)(a)))
          ))
        ));
      })())("literal string");
      const $18 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => $17(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $18(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))))
      )));
    })(),
    natural: withErrorMessage((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => {
        const $18 = (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))));
        const $19 = state1._1;
        const $20 = state1._2;
        return more((v3) => zeroNumber(
          $ParseState($19, $20, false),
          more,
          lift1,
          (v4, $21) => {
            const $22 = v4._3;
            return more((v5) => {
              if ($22) {
                return $$throw2(v4, $21);
              }
              return decimal(state1, more, lift1, $$throw2, $18);
            });
          },
          $18
        ));
      }));
    })())("natural"),
    integer: withErrorMessage((() => {
      const $17 = whiteSpace$p(v);
      const $18 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => {
        const $19 = (state2, a) => more((v2$1) => more((v3) => $18(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))));
        return more((v1$1) => more((v2$1) => more((v1$2) => sign1(
          state1,
          more,
          lift1,
          $$throw2,
          (state2, a) => more((v2$2) => more((v3) => $17(
            state2,
            more,
            lift1,
            $$throw2,
            (state3, a$1) => more((v4) => more((v2$3) => more((v1$3) => {
              const $20 = state3._1;
              const $21 = state3._2;
              return more((v3$1) => zeroNumber(
                $ParseState($20, $21, false),
                more,
                lift1,
                (v4$1, $22) => {
                  const $23 = v4$1._3;
                  return more((v5) => {
                    if ($23) {
                      return $$throw2(v4$1, $22);
                    }
                    return decimal(state3, more, lift1, $$throw2, (state2$1, a$2) => more((v2$4) => $19(state2$1, a(a$2))));
                  });
                },
                (state2$1, a$2) => more((v2$4) => $19(state2$1, a(a$2)))
              ));
            })))
          )))
        ))));
      }));
    })())("integer"),
    float: withErrorMessage((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => more((v1$1) => decimal(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => fractExponent(a)(
          state2,
          more,
          lift1,
          $$throw2,
          (state2$1, a$1) => more((v2$2) => more((v3) => $17(state2$1, more, lift1, $$throw2, (state3, a$2) => more((v4) => done(state3, a$1)))))
        ))
      ))));
    })())("float"),
    naturalOrFloat: withErrorMessage((() => {
      const $17 = withErrorMessage(satisfy((v$1) => v$1 === "0"))("'0'");
      const $18 = fractExponent(0);
      const $19 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => {
        const $20 = (state2, a) => more((v2$1) => more((v3) => $19(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))));
        const $21 = state1._1;
        const $22 = state1._2;
        return more((v3) => {
          const $23 = (v4, $232) => {
            const $24 = v4._3;
            return more((v5) => {
              if ($24) {
                return $$throw2(v4, $232);
              }
              return decimalFloat(state1, more, lift1, $$throw2, $20);
            });
          };
          return more((v2$1) => more((v1$1) => $17(
            $ParseState($21, $22, false),
            more,
            lift1,
            $23,
            (state2, a) => more((v2$2) => more((v3$1) => {
              const $24 = state2._1;
              const $25 = state2._2;
              return more((v3$2) => {
                const $26 = (v4, $262) => {
                  const $27 = v4._3;
                  return more((v5) => {
                    if ($27) {
                      return $23(v4, $262);
                    }
                    const $28 = state2._1;
                    const $29 = state2._2;
                    return more((v3$3) => decimalFloat(
                      $ParseState($28, $29, false),
                      more,
                      lift1,
                      (v4$1, $30) => {
                        const $31 = v4$1._3;
                        return more((v5$1) => {
                          if ($31) {
                            return $23(v4$1, $30);
                          }
                          const $32 = state2._1;
                          const $33 = state2._2;
                          return more((v3$4) => more((v1$2) => $18(
                            $ParseState($32, $33, false),
                            more,
                            lift1,
                            (v4$2, $34) => {
                              const $35 = v4$2._3;
                              return more((v5$2) => {
                                if ($35) {
                                  return $23(v4$2, $34);
                                }
                                return more((v4$3) => $20(state2, $Either("Left", 0)));
                              });
                            },
                            (state2$1, a$1) => more((v2$3) => more((v4$2) => $20(state2$1, $Either("Right", a$1))))
                          )));
                        });
                      },
                      (state3, a$1) => more((v4$1) => $20(state3, a$1))
                    ));
                  });
                };
                return more((v1$2) => more((v3$3) => hexadecimal(
                  $ParseState($24, $25, false),
                  more,
                  lift1,
                  (v4, $27) => {
                    const $28 = v4._3;
                    return more((v5) => {
                      if ($28) {
                        return $26(v4, $27);
                      }
                      return octal(
                        $ParseState($24, $25, false),
                        more,
                        lift1,
                        $26,
                        (state2$1, a$1) => more((v2$3) => more((v4$1) => $20(state2$1, $Either("Left", a$1))))
                      );
                    });
                  },
                  (state2$1, a$1) => more((v2$3) => more((v4) => $20(state2$1, $Either("Left", a$1))))
                )));
              });
            }))
          )));
        });
      }));
    })())("number"),
    decimal,
    hexadecimal,
    octal,
    symbol: (name2) => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(name2)(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, name2))))))
      ))));
    },
    lexeme: (p) => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => p(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))))
      )));
    },
    whiteSpace: whiteSpace$p(v),
    parens: (p) => between((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2("(")(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "("))))))
      ))));
    })())((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(")")(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ")"))))))
      ))));
    })())(p),
    braces: (p) => between((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2("{")(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "{"))))))
      ))));
    })())((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2("}")(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "}"))))))
      ))));
    })())(p),
    angles: (p) => between((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2("<")(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "<"))))))
      ))));
    })())((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(">")(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ">"))))))
      ))));
    })())(p),
    brackets: (p) => between((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2("[")(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "["))))))
      ))));
    })())((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2("]")(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "]"))))))
      ))));
    })())(p),
    semi: semi2,
    comma: comma2,
    colon: (() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(":")(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ":"))))))
      ))));
    })(),
    dot: (() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(".")(
        state1,
        more,
        lift1,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "."))))))
      ))));
    })(),
    semiSep: (p) => sepBy(p)(semi2),
    semiSep1: (p) => sepBy1(p)(semi2),
    commaSep: (p) => sepBy(p)(comma2),
    commaSep1: (p) => sepBy1(p)(comma2)
  };
};

// output-es/Util.Parse/index.js
var some3 = (p) => {
  const $0 = some(alternativeParserT)(lazyParserT)(p);
  return (state1, more, lift1, $$throw2, done) => more((v1) => $0(state1, more, lift1, $$throw2, (state2, a) => more((v2) => done(state2, nonEmptyListNonEmptyList.nonEmpty(a)))));
};
var sepBy1_try = (p) => (sep) => {
  const $0 = many(alternativeParserT)(lazyParserT)((v1, $02, $1, $2, $3) => {
    const $4 = v1._3;
    return $02((v2) => $02((v1$1) => sep(
      v1,
      $02,
      $1,
      (v2$1, $5) => $2($ParseState(v2$1._1, v2$1._2, $4), $5),
      (state2, a) => $02((v2$1) => $02((v3) => p(state2, $02, $1, (v2$2, $5) => $2($ParseState(v2$2._1, v2$2._2, $4), $5), (state3, a$1) => $02((v4) => $3(state3, a$1)))))
    )));
  });
  return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => p(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => $0(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, $NonEmpty(a, a$1))))))
  )));
};
var sepBy_try = (p) => (sep) => {
  const $0 = sepBy1_try(p)(sep);
  return (v2, $1, $2, $3, $4) => {
    const $5 = v2._1;
    const $6 = v2._2;
    return $1((v3) => $1((v1) => $0(
      $ParseState($5, $6, false),
      $1,
      $2,
      (v4, $7) => {
        const $8 = v4._3;
        return $1((v5) => {
          if ($8) {
            return $3(v4, $7);
          }
          return $4(v2, Nil);
        });
      },
      (state2, a) => $1((v2$1) => $4(state2, $List("Cons", a._1, a._2)))
    )));
  };
};

// output-es/Parse/index.js
var fromFoldable18 = /* @__PURE__ */ (() => fromFoldableImpl(foldableList.foldr))();
var fromFoldable19 = /* @__PURE__ */ (() => fromFoldableImpl(foldableNonEmptyList.foldr))();
var onlyIf = (b) => (a) => {
  const $0 = b ? (state1, v, v1, v2, done) => done(state1, void 0) : fail2("No alternative");
  return (state1, more, lift1, $$throw2, done) => more((v1) => $0(state1, more, lift1, $$throw2, (state2, a$1) => more((v2) => done(state2, a))));
};
var choose2 = /* @__PURE__ */ choose(altParserT);
var fanin3 = /* @__PURE__ */ fanin(categoryFn)(choiceFn);
var identity24 = (x) => x;
var operators = (binaryOp) => fromFoldable18(listMap(arrayMap((v) => $Operator(
  "Infix",
  (() => {
    const $0 = binaryOp(v.op);
    return (v1, $1, $2, $3, $4) => {
      const $5 = v1._3;
      return $0(v1, $1, $2, (v2, $6) => $3($ParseState(v2._1, v2._2, $5), $6), $4);
    };
  })(),
  v.assoc
)))(listMap(fromFoldable19)(groupBy((x) => (y) => x.prec === y.prec)(sortBy((x) => (x$1) => {
  const $0 = ordInt.compare(x.prec)(x$1.prec);
  if ($0 === "GT") {
    return LT;
  }
  if ($0 === "EQ") {
    return EQ;
  }
  if ($0 === "LT") {
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
    identStart: (() => {
      const $0 = withErrorMessage(satisfy((v) => v === "_"))("'_'");
      return (v2, $1, $2, $3, $4) => {
        const $5 = v2._1;
        const $6 = v2._2;
        return $1((v3) => letter(
          $ParseState($5, $6, false),
          $1,
          $2,
          (v4, $7) => {
            const $8 = v4._3;
            return $1((v5) => {
              if ($8) {
                return $3(v4, $7);
              }
              return $0(v2, $1, $2, $3, $4);
            });
          },
          $4
        ));
      };
    })(),
    identLetter: (() => {
      const $0 = oneOf(["_", "'"]);
      return (v2, $1, $2, $3, $4) => {
        const $5 = v2._1;
        const $6 = v2._2;
        return $1((v3) => alphaNum(
          $ParseState($5, $6, false),
          $1,
          $2,
          (v4, $7) => {
            const $8 = v4._3;
            return $1((v5) => {
              if ($8) {
                return $3(v4, $7);
              }
              return $0(v2, $1, $2, $3, $4);
            });
          },
          $4
        ));
      };
    })(),
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
  return (state1, more, lift1, $$throw2, done) => more((v1) => $0(state1, more, lift1, $$throw2, (state2, a) => more((v2) => done(state2, void 0))));
})();
var topLevel = (p) => (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => more((v2$1) => more((v1$1) => token.whiteSpace(
  state1,
  more,
  lift1,
  $$throw2,
  (state2, a) => more((v2$2) => more((v3) => p(
    state2,
    more,
    lift1,
    $$throw2,
    (state3, a$1) => more((v4) => more((v2$3) => more((v3$1) => eof(state3, more, lift1, $$throw2, (state3$1, a$2) => more((v4$1) => done(state3$1, a$1))))))
  )))
)))));
var lBracket = /* @__PURE__ */ (() => {
  const $0 = token.symbol("[");
  return (state1, more, lift1, $$throw2, done) => more((v1) => $0(state1, more, lift1, $$throw2, (state2, a) => more((v2) => done(state2, void 0))));
})();
var lArrow = /* @__PURE__ */ (() => token.reservedOp("<-"))();
var keyword = (str$p) => {
  if (elem(eqString)(str$p)(languageDef.reservedNames)) {
    return token.reserved(str$p);
  }
  return throwException(error(str$p + " is not a reserved word"))();
};
var ident = (state1, more, lift1, $$throw2, done) => more((v1) => token.identifier(
  state1,
  more,
  lift1,
  $$throw2,
  (state2, a) => more((v2) => onlyIf(!isCtrName(a))(a)(state2, more, lift1, $$throw2, done))
));
var field = (p) => (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => ident(
  state1,
  more,
  lift1,
  $$throw2,
  (state2, a) => more((v2$1) => {
    const $0 = Tuple(a);
    return more((v3) => more((v2$2) => more((v1$1) => token.colon(
      state2,
      more,
      lift1,
      $$throw2,
      (state2$1, a$1) => more((v2$3) => more((v3$1) => p(state2$1, more, lift1, $$throw2, (state3, a$2) => more((v4) => more((v4$1) => done(state3, $0(a$2)))))))
    ))));
  })
)));
var equals = /* @__PURE__ */ (() => token.reservedOp("="))();
var patternDelim = (v2, $0, $1, $2, $3) => {
  const $4 = v2._1;
  const $5 = v2._2;
  return $0((v3) => rArrow(
    $ParseState($4, $5, false),
    $0,
    $1,
    (v4, $6) => {
      const $7 = v4._3;
      return $0((v5) => {
        if ($7) {
          return $2(v4, $6);
        }
        return equals(v2, $0, $1, $2, $3);
      });
    },
    $3
  ));
};
var ellipsis = /* @__PURE__ */ (() => token.reservedOp(".."))();
var ctr = (state1, more, lift1, $$throw2, done) => more((v1) => token.identifier(
  state1,
  more,
  lift1,
  $$throw2,
  (state2, a) => more((v2) => onlyIf(isCtrName(a))(a)(state2, more, lift1, $$throw2, done))
));
var simplePattern = (pattern$p) => {
  const $0 = token.brackets((state1, v, v1, v2, done) => done(state1, PListEmpty));
  const go$lazy = binding(() => lazyParserT.defer((v) => (v2, $12, $22, $32, $4) => {
    const $5 = v2._1;
    const $6 = v2._2;
    return $12((v3) => $12((v2$1) => $12((v1) => rBracket(
      $ParseState($5, $6, false),
      $12,
      $22,
      (v4, $7) => {
        const $8 = v4._3;
        return $12((v5) => {
          if ($8) {
            return $32(v4, $7);
          }
          return $12((v2$2) => $12((v1$1) => token.comma(
            v2,
            $12,
            $22,
            $32,
            (state2, a) => $12((v2$3) => $12((v3$1) => $12((v2$4) => $12((v1$2) => pattern$p(
              state2,
              $12,
              $22,
              $32,
              (state2$1, a$1) => $12((v2$5) => {
                const $9 = PListNext(a$1);
                return $12((v3$2) => go$lazy()(
                  state2$1,
                  $12,
                  $22,
                  $32,
                  (state3, a$2) => $12((v4$1) => {
                    const $10 = $9(a$2);
                    return $12((v4$2) => $4(state3, $10));
                  })
                ));
              })
            )))))
          )));
        });
      },
      (state2, a) => $12((v2$2) => $12((v3$1) => $12((v4) => $4(state2, PListEnd))))
    ))));
  }));
  const go = go$lazy();
  const $1 = token.braces((state1, more, lift1, $$throw2, done) => more((v1) => sepBy(field(pattern$p))(token.comma)(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2) => done(state2, $Pattern("PRecord", a)))
  )));
  const $2 = token.parens(pattern$p);
  const $3 = token.parens((state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => pattern$p(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => token.comma(
      state2,
      more,
      lift1,
      $$throw2,
      (state3, a$1) => more((v4) => more((v2$2) => more((v1$2) => pattern$p(
        state3,
        more,
        lift1,
        $$throw2,
        (state2$1, a$2) => more((v2$3) => done(
          state2$1,
          $Pattern("PConstr", "Pair", $List("Cons", a, $List("Cons", a$2, Nil)))
        ))
      ))))
    )))
  )))));
  return (v2, $4, $5, $6, $7) => {
    const $8 = v2._1;
    const $9 = v2._2;
    return $4((v3) => $0(
      $ParseState($8, $9, false),
      $4,
      $5,
      (v2$1, $10) => $4((v5) => {
        const $11 = v2._1;
        const $12 = v2._2;
        return $4((v3$1) => {
          const $13 = (v4, $132) => {
            const $14 = v4._3;
            return $4((v5$1) => {
              if ($14) {
                return $6(v4, $132);
              }
              const $15 = v2._1;
              const $16 = v2._2;
              return $4((v3$2) => $4((v1) => $4((v1$1) => ctr(
                $ParseState($15, $16, false),
                $4,
                $5,
                (v2$2, $17) => $4((v5$2) => {
                  const $18 = v2._1;
                  const $19 = v2._2;
                  return $4((v3$3) => $1(
                    $ParseState($18, $19, false),
                    $4,
                    $5,
                    (v2$3, $20) => $4((v5$3) => {
                      const $21 = v2._1;
                      const $22 = v2._2;
                      return $4((v3$4) => $4((v1$2) => ident(
                        $ParseState($21, $22, false),
                        $4,
                        $5,
                        (v2$4, $23) => $4((v5$4) => {
                          const $24 = v2._1;
                          const $25 = v2._2;
                          return $4((v3$5) => $2($ParseState($24, $25, false), $4, $5, (v2$5, $26) => $4((v5$5) => $3(v2, $4, $5, $6, $7)), $7));
                        }),
                        (state2, a) => $4((v2$4) => $7(state2, $Pattern("PVar", a)))
                      )));
                    }),
                    $7
                  ));
                }),
                (state2, a) => $4((v2$2) => {
                  const $17 = PConstr(a);
                  return $4((v2$3) => $7(state2, $17(Nil)));
                })
              ))));
            });
          };
          return $4((v2$2) => $4((v1) => lBracket(
            $ParseState($11, $12, false),
            $4,
            $5,
            $13,
            (state2, a) => $4((v2$3) => $4((v3$2) => $4((v2$4) => $4((v1$1) => pattern$p(
              state2,
              $4,
              $5,
              $13,
              (state2$1, a$1) => $4((v2$5) => {
                const $14 = PListNonEmpty(a$1);
                return $4((v3$3) => go(
                  state2$1,
                  $4,
                  $5,
                  $13,
                  (state3, a$2) => $4((v4) => {
                    const $15 = $14(a$2);
                    return $4((v4$1) => $7(state3, $15));
                  })
                ));
              })
            )))))
          )));
        });
      }),
      $7
    ));
  };
};
var pattern = /* @__PURE__ */ (() => {
  const $0 = buildExprParser(operators((op) => (state1, more, lift1, $$throw2, done) => more((v1) => token.operator(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2) => onlyIf(":" === definitely("absurd")(charAt2(0)(a)) && op === a)((\u03C0) => (\u03C0$p) => $Pattern(
      "PConstr",
      a,
      $List("Cons", \u03C0, $List("Cons", \u03C0$p, Nil))
    ))(state2, more, lift1, $$throw2, done))
  ))));
  const go$lazy = binding(() => lazyParserT.defer((v) => $0((() => {
    const rest = (v$1) => {
      if (v$1.tag === "PConstr") {
        const $12 = v$1._1;
        const $2 = v$1._2;
        const $3 = simplePattern(go$lazy());
        return (v2, $4, $5, $6, $7) => {
          const $8 = v2._1;
          const $9 = v2._2;
          return $4((v3) => {
            const $10 = (v4, $102) => {
              const $11 = v4._3;
              return $4((v5) => {
                if ($11) {
                  return $6(v4, $102);
                }
                return $7(v2, v$1);
              });
            };
            return $4((v1) => $3(
              $ParseState($8, $9, false),
              $4,
              $5,
              $10,
              (state2, a) => $4((v2$1) => rest($Pattern(
                "PConstr",
                $12,
                foldableList.foldr(Cons)($List("Cons", a, Nil))($2)
              ))(state2, $4, $5, $10, $7))
            ));
          });
        };
      }
      return (state1, v$2, v1, v2, done) => done(state1, v$1);
    };
    const $1 = simplePattern(go$lazy());
    return (state1, more, lift1, $$throw2, done) => more((v1) => $1(state1, more, lift1, $$throw2, (state2, a) => more((v2) => rest(a)(state2, more, lift1, $$throw2, done))));
  })())));
  const go = go$lazy();
  return go;
})();
var varDefs = (expr$p) => {
  const $0 = keyword("let");
  const $1 = sepBy1_try((state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => more((v2$1) => more((v1$1) => pattern(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2$2) => more((v3) => equals(
      state2,
      more,
      lift1,
      $$throw2,
      (state3, a$1) => more((v4) => more((v2$3) => {
        const $12 = VarDef2(a);
        return more((v3$1) => expr$p(state3, more, lift1, $$throw2, (state3$1, a$2) => more((v4$1) => done(state3$1, $12(a$2)))));
      }))
    )))
  ))))))(token.semi);
  return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => $0(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => $1(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
  )));
};
var clause_uncurried = (expr$p) => (delim) => (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => pattern(
  state1,
  more,
  lift1,
  $$throw2,
  (state2, a) => more((v2$1) => {
    const $0 = Tuple(a);
    return more((v3) => more((v2$2) => more((v1$1) => delim(
      state2,
      more,
      lift1,
      $$throw2,
      (state2$1, a$1) => more((v2$3) => more((v3$1) => expr$p(state2$1, more, lift1, $$throw2, (state3, a$2) => more((v4) => more((v4$1) => done(state3, $0(a$2)))))))
    ))));
  })
)));
var clause_curried = (expr$p) => (delim) => {
  const $0 = some3(simplePattern(pattern));
  return (state1, more, lift1, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => $0(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2$1) => {
      const $1 = Tuple(a);
      return more((v3) => more((v2$2) => more((v1$2) => delim(
        state2,
        more,
        lift1,
        $$throw2,
        (state2$1, a$1) => more((v2$3) => more((v3$1) => expr$p(
          state2$1,
          more,
          lift1,
          $$throw2,
          (state3, a$2) => more((v4) => more((v4$1) => {
            const $2 = $1(a$2);
            return more((v2$4) => done(state3, $2));
          }))
        )))
      ))));
    })
  ))));
};
var recDefs = (expr$p) => {
  const $0 = keyword("let");
  const $1 = sepBy1_try((() => {
    const $12 = clause_curried(expr$p)(equals);
    return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => ident(
      state1,
      more,
      lift1,
      $$throw2,
      (state2, a) => more((v2$1) => {
        const $2 = Tuple(a);
        return more((v3) => $12(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, $2(a$1)))));
      })
    )));
  })())(token.semi);
  return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => $0(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => $1(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
  )));
};
var defs = (expr$p) => {
  const $0 = choose2((() => {
    const $02 = varDefs(expr$p);
    return (v1, $1, $2, $3, $4) => {
      const $5 = v1._3;
      return $02(v1, $1, $2, (v2, $6) => $3($ParseState(v2._1, v2._2, $5), $6), $4);
    };
  })())(recDefs(expr$p));
  return (state1, more, lift1, $$throw2, done) => more((v1) => $0(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2) => done(state2, $List("Cons", a, Nil)))
  ));
};
var branches = (expr$p) => (branch_) => {
  const $0 = branch_(expr$p)(patternDelim);
  const $1 = token.braces(sepBy1(branch_(expr$p)(rArrow))(token.semi));
  return (v2, $2, $3, $4, $5) => {
    const $6 = v2._1;
    const $7 = v2._2;
    return $2((v3) => $2((v1) => $0(
      $ParseState($6, $7, false),
      $2,
      $3,
      (v4, $8) => {
        const $9 = v4._3;
        return $2((v5) => {
          if ($9) {
            return $4(v4, $8);
          }
          return $1(v2, $2, $3, $4, $5);
        });
      },
      (state2, a) => $2((v2$1) => $5(state2, $NonEmpty(a, Nil)))
    )));
  };
};
var bar = /* @__PURE__ */ (() => token.reservedOp("|"))();
var backtick = /* @__PURE__ */ (() => {
  const $0 = token.symbol("`");
  return (state1, more, lift1, $$throw2, done) => more((v1) => $0(state1, more, lift1, $$throw2, (state2, a) => more((v2) => done(state2, void 0))));
})();
var expr_$lazy = /* @__PURE__ */ binding(() => {
  const $0 = buildExprParser([
    [
      $Operator(
        "Infix",
        (state1, more, lift1, $$throw2, done) => more((v1) => between(backtick)(backtick)(ident)(
          state1,
          more,
          lift1,
          $$throw2,
          (state2, a) => more((v2) => done(state2, (e) => (e$p) => $Expr2("BinaryApp", e, a, e$p)))
        )),
        AssocLeft
      )
    ],
    ...operators((op) => (state1, more, lift1, $$throw2, done) => more((v1) => token.operator(
      state1,
      more,
      lift1,
      $$throw2,
      (state2, a) => more((v2) => onlyIf(op === a)((() => {
        if (op === ".") {
          return (e) => (e$p) => {
            if (e$p.tag === "Var") {
              return $Expr2("Project", e, e$p._1);
            }
            return throwException(error('Field names are not first class; got "' + intercalate3("\n")(removeDocWS(prettyExpr1(annUnit).pretty(e$p)).lines) + '".'))();
          };
        }
        if (":" === definitely("absurd")(charAt2(0)(a))) {
          return (e) => (e$p) => $Expr2("Constr", void 0, a, $List("Cons", e, $List("Cons", e$p, Nil)));
        }
        return (e) => (e$p) => $Expr2("BinaryApp", e, op, e$p);
      })())(state2, more, lift1, $$throw2, done))
    )))
  ]);
  const go$lazy = binding(() => lazyParserT.defer((v) => $0((() => {
    const $1 = keyword("match");
    const $2 = keyword("as");
    const $3 = branches(go$lazy())(clause_uncurried);
    const $4 = keyword("if");
    const $5 = keyword("then");
    const $6 = keyword("else");
    const $7 = keyword("fun");
    const $8 = branches(go$lazy())(clause_curried);
    const $9 = (() => {
      const $92 = sepBy1(defs(go$lazy()))(token.semi);
      const $10 = between(token.symbol("[|"))(token.symbol("|]"))((() => {
        const $102 = Matrix2();
        const $112 = token.parens((state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => ident(
          state1,
          more,
          lift1,
          $$throw2,
          (state2, a) => more((v2$1) => {
            const $113 = Tuple(a);
            return more((v3) => more((v2$2) => more((v1$1) => token.comma(
              state2,
              more,
              lift1,
              $$throw2,
              (state2$1, a$1) => more((v2$3) => more((v3$1) => ident(state2$1, more, lift1, $$throw2, (state3, a$2) => more((v4) => more((v4$1) => done(state3, $113(a$2)))))))
            ))));
          })
        ))));
        const $122 = keyword("in");
        return (state1, more, lift1, $$throw2, done) => more((v2) => more((v2$1) => more((v1) => more((v2$2) => more((v1$1) => go$lazy()(
          state1,
          more,
          lift1,
          $$throw2,
          (state2, a) => more((v2$3) => more((v3) => bar(
            state2,
            more,
            lift1,
            $$throw2,
            (state3, a$1) => more((v4) => more((v2$4) => {
              const $13 = $102(a);
              return more((v3$1) => $112(
                state3,
                more,
                lift1,
                $$throw2,
                (state3$1, a$2) => more((v4$1) => {
                  const $14 = $13(a$2);
                  return more((v3$2) => more((v2$5) => more((v1$2) => $122(
                    state3$1,
                    more,
                    lift1,
                    $$throw2,
                    (state2$1, a$3) => more((v2$6) => more((v3$3) => go$lazy()(state2$1, more, lift1, $$throw2, (state3$2, a$4) => more((v4$2) => more((v4$3) => done(state3$2, $14(a$4)))))))
                  ))));
                })
              ));
            }))
          )))
        ))))));
      })());
      const $11 = token.brackets((state1, v$1, v1, v2, done) => done(state1, $Expr2("ListEmpty", void 0)));
      const $12 = (() => {
        const $122 = ListNonEmpty();
        const go$1$lazy = binding(() => lazyParserT.defer((v$1) => {
          const $132 = Next();
          return (v2, $142, $152, $16, $17) => {
            const $18 = v2._1;
            const $19 = v2._2;
            return $142((v3) => $142((v2$1) => $142((v1) => rBracket(
              $ParseState($18, $19, false),
              $142,
              $152,
              (v4, $20) => {
                const $21 = v4._3;
                return $142((v5) => {
                  if ($21) {
                    return $16(v4, $20);
                  }
                  return $142((v2$2) => $142((v1$1) => token.comma(
                    v2,
                    $142,
                    $152,
                    $16,
                    (state2, a) => $142((v2$3) => $142((v3$1) => $142((v2$4) => $142((v1$2) => go$lazy()(
                      state2,
                      $142,
                      $152,
                      $16,
                      (state2$1, a$1) => $142((v2$5) => {
                        const $22 = $132(a$1);
                        return $142((v3$2) => go$1$lazy()(
                          state2$1,
                          $142,
                          $152,
                          $16,
                          (state3, a$2) => $142((v4$1) => {
                            const $23 = $22(a$2);
                            return $142((v4$2) => $17(state3, $23));
                          })
                        ));
                      })
                    )))))
                  )));
                });
              },
              (state2, a) => $142((v2$2) => $142((v3$1) => $142((v4) => $17(state2, $ListRest("End", void 0)))))
            ))));
          };
        }));
        const go$1 = go$1$lazy();
        const $13 = token.brackets((() => {
          const $132 = ListComp();
          const $142 = sepBy1((() => {
            const $143 = keyword("let");
            return (v2, $152, $16, $17, $18) => {
              const $19 = v2._1;
              const $20 = v2._2;
              return $152((v3) => {
                const $21 = (v4, $212) => {
                  const $22 = v4._3;
                  return $152((v5) => {
                    if ($22) {
                      return $17(v4, $212);
                    }
                    const $23 = v2._1;
                    const $24 = v2._2;
                    return $152((v3$1) => $152((v1) => {
                      const $25 = (v4$1, $252) => {
                        const $26 = v4$1._3;
                        return $152((v5$1) => {
                          if ($26) {
                            return $17(v4$1, $252);
                          }
                          return $152((v1$1) => go$lazy()(v2, $152, $16, $17, (state2, a) => $152((v2$1) => $18(state2, $Qualifier("ListCompGuard", a)))));
                        });
                      };
                      return $152((v2$1) => $152((v1$1) => $152((v2$2) => $152((v1$2) => $152((v2$3) => $152((v1$3) => $143(
                        $ParseState($23, $24, false),
                        $152,
                        $16,
                        $25,
                        (state2, a) => $152((v2$4) => $152((v3$2) => pattern(
                          state2,
                          $152,
                          $16,
                          $25,
                          (state3, a$1) => $152((v4$1) => $152((v2$5) => $152((v3$3) => equals(
                            state3,
                            $152,
                            $16,
                            $25,
                            (state3$1, a$2) => $152((v4$2) => $152((v2$6) => {
                              const $26 = VarDef2(a$1);
                              return $152((v3$4) => go$lazy()(
                                state3$1,
                                $152,
                                $16,
                                $25,
                                (state3$2, a$3) => $152((v4$3) => {
                                  const $27 = $26(a$3);
                                  return $152((v2$7) => $18(state3$2, $Qualifier("ListCompDecl", $27)));
                                })
                              ));
                            }))
                          ))))
                        )))
                      )))))));
                    }));
                  });
                };
                return $152((v2$1) => $152((v2$2) => $152((v1) => $152((v1$1) => pattern(
                  $ParseState($19, $20, false),
                  $152,
                  $16,
                  $21,
                  (state2, a) => $152((v2$3) => {
                    const $22 = ListCompGen(a);
                    return $152((v2$4) => $152((v3$1) => lArrow(
                      state2,
                      $152,
                      $16,
                      $21,
                      (state3, a$1) => $152((v4) => $152((v3$2) => go$lazy()(state3, $152, $16, $21, (state3$1, a$2) => $152((v4$1) => $18(state3$1, $22(a$2))))))
                    )));
                  })
                )))));
              });
            };
          })())(token.comma);
          return (state1, more, lift1, $$throw2, done) => more((v2) => more((v2$1) => more((v1) => more((v2$2) => more((v3) => go$lazy()(
            state1,
            more,
            lift1,
            $$throw2,
            (state3, a) => more((v4) => {
              const $152 = $132(a);
              return more((v2$3) => more((v3$1) => bar(
                state3,
                more,
                lift1,
                $$throw2,
                (state3$1, a$1) => more((v4$1) => more((v3$2) => more((v1$1) => $142(
                  state3$1,
                  more,
                  lift1,
                  $$throw2,
                  (state2, a$2) => more((v2$4) => {
                    const $16 = $List("Cons", a$2._1, a$2._2);
                    return more((v4$2) => done(state2, $152($16)));
                  })
                ))))
              )));
            })
          ))))));
        })());
        const $14 = token.brackets((state1, more, lift1, $$throw2, done) => more((v2) => more((v2$1) => more((v1) => more((v2$2) => more((v3) => go$lazy()(
          state1,
          more,
          lift1,
          $$throw2,
          (state3, a) => more((v4) => {
            const $142 = ListEnum(a);
            return more((v2$3) => more((v3$1) => ellipsis(
              state3,
              more,
              lift1,
              $$throw2,
              (state3$1, a$1) => more((v4$1) => more((v3$2) => go$lazy()(state3$1, more, lift1, $$throw2, (state3$2, a$2) => more((v4$2) => done(state3$2, $142(a$2))))))
            )));
          })
        )))))));
        const $15 = (() => {
          const $152 = Constr2();
          const $16 = (() => {
            const $162 = token.braces((() => {
              const $163 = sepBy((() => {
                const $164 = token.brackets((state1, more, lift1, $$throw2, done) => more((v1) => go$lazy()(
                  state1,
                  more,
                  lift1,
                  $$throw2,
                  (state2, a) => more((v2) => done(state2, $DictEntry("ExprKey", a)))
                )));
                const $172 = VarKey();
                return (v2, $18, $19, $20, $21) => {
                  const $22 = v2._1;
                  const $23 = v2._2;
                  return $18((v3) => {
                    const $24 = (v4, $242) => {
                      const $25 = v4._3;
                      return $18((v5) => {
                        if ($25) {
                          return $20(v4, $242);
                        }
                        return $18((v2$1) => $18((v1) => $18((v2$2) => $18((v1$1) => $18((v1$2) => ident(
                          v2,
                          $18,
                          $19,
                          $20,
                          (state2, a) => $18((v2$3) => {
                            const $26 = $172(a);
                            return $18((v2$4) => $18((v3$1) => token.colon(
                              state2,
                              $18,
                              $19,
                              $20,
                              (state3, a$1) => $18((v4$1) => $18((v2$5) => {
                                const $27 = Tuple($26);
                                return $18((v3$2) => go$lazy()(state3, $18, $19, $20, (state3$1, a$2) => $18((v4$2) => $21(state3$1, $27(a$2)))));
                              }))
                            )));
                          })
                        ))))));
                      });
                    };
                    return $18((v2$1) => $18((v1) => $18((v2$2) => $18((v1$1) => $164(
                      $ParseState($22, $23, false),
                      $18,
                      $19,
                      $24,
                      (state2, a) => $18((v2$3) => $18((v3$1) => token.colon(
                        state2,
                        $18,
                        $19,
                        $24,
                        (state3, a$1) => $18((v4) => $18((v2$4) => {
                          const $25 = Tuple(a);
                          return $18((v3$2) => go$lazy()(state3, $18, $19, $24, (state3$1, a$2) => $18((v4$1) => $21(state3$1, $25(a$2)))));
                        }))
                      )))
                    )))));
                  });
                };
              })())(token.comma);
              const $17 = Dictionary3();
              return (state1, more, lift1, $$throw2, done) => more((v1) => $163(state1, more, lift1, $$throw2, (state2, a) => more((v2) => done(state2, $17(a)))));
            })());
            const simpleExprOrProjection = (() => {
              const $17 = (() => {
                const $172 = (() => {
                  const $173 = withErrorMessage(satisfy((v$1) => v$1 === "-"))("'-'");
                  const $18 = (() => {
                    const $182 = withErrorMessage(satisfy((v$1) => v$1 === "+"))("'+'");
                    const $19 = (() => {
                      const $192 = withErrorMessage(satisfy((v$1) => v$1 === "-"))("'-'");
                      const $20 = (() => {
                        const $202 = withErrorMessage(satisfy((v$1) => v$1 === "+"))("'+'");
                        const $21 = (() => {
                          const $212 = Str2();
                          const $22 = (() => {
                            const $222 = (() => {
                              const $223 = (() => {
                                const $224 = token.parens(go$lazy());
                                const $23 = token.parens(token.operator);
                                const $24 = (() => {
                                  const $242 = token.parens((state1, more, lift1, $$throw2, done) => more((v2) => more((v2$1) => more((v3) => more((v2$2) => more((v1) => go$lazy()(
                                    state1,
                                    more,
                                    lift1,
                                    $$throw2,
                                    (state2, a) => more((v2$3) => more((v3$1) => token.comma(
                                      state2,
                                      more,
                                      lift1,
                                      $$throw2,
                                      (state3, a$1) => more((v4) => more((v4$1) => more((v3$2) => go$lazy()(
                                        state3,
                                        more,
                                        lift1,
                                        $$throw2,
                                        (state3$1, a$2) => more((v4$2) => done(
                                          state3$1,
                                          $Expr2("Constr", void 0, "Pair", $List("Cons", a, $List("Cons", a$2, Nil)))
                                        ))
                                      ))))
                                    )))
                                  )))))));
                                  return (v2, $25, $26, $27, $28) => {
                                    const $29 = v2._1;
                                    const $30 = v2._2;
                                    return $25((v3) => {
                                      const $31 = (v4, $312) => {
                                        const $32 = v4._3;
                                        return $25((v5) => {
                                          if ($32) {
                                            return $27(v4, $312);
                                          }
                                          const $33 = v2._1;
                                          const $34 = v2._2;
                                          return $25((v3$1) => $25((v1) => token.stringLiteral(
                                            $ParseState($33, $34, false),
                                            $25,
                                            $26,
                                            (v4$1, $35) => {
                                              const $36 = v4$1._3;
                                              return $25((v5$1) => {
                                                if ($36) {
                                                  return $27(v4$1, $35);
                                                }
                                                const $37 = v2._1;
                                                const $38 = v2._2;
                                                return $25((v3$2) => $224(
                                                  $ParseState($37, $38, false),
                                                  $25,
                                                  $26,
                                                  (v2$1, $39) => $25((v5$2) => {
                                                    const $40 = v2._1;
                                                    const $41 = v2._2;
                                                    return $25((v3$3) => $25((v1$1) => $23(
                                                      $ParseState($40, $41, false),
                                                      $25,
                                                      $26,
                                                      (v2$2, $42) => $25((v5$3) => $242(v2, $25, $26, $27, $28)),
                                                      (state2, a) => $25((v2$2) => $28(state2, $Expr2("Op", a)))
                                                    )));
                                                  }),
                                                  $28
                                                ));
                                              });
                                            },
                                            (state2, a) => $25((v2$1) => $28(state2, $212(a)))
                                          )));
                                        });
                                      };
                                      return $25((v1) => {
                                        const $32 = (state2, a) => $25((v2$1) => {
                                          const $322 = Int2();
                                          return $25((v1$1) => token.natural(
                                            state2,
                                            $25,
                                            $26,
                                            (v2$2, $33) => $31($ParseState(v2$2._1, v2$2._2, false), $33),
                                            (state2$1, a$1) => $25((v2$2) => $28(state2$1, $322(a(a$1))))
                                          ));
                                        });
                                        return $25((v3$1) => $25((v1$1) => $192(
                                          $ParseState($29, $30, false),
                                          $25,
                                          $26,
                                          (v4, $33) => {
                                            const $34 = v4._3;
                                            return $25((v5) => {
                                              if ($34) {
                                                return $31($ParseState(v4._1, v4._2, false), $33);
                                              }
                                              return $25((v3$2) => $25((v1$2) => $202(
                                                $ParseState($29, $30, false),
                                                $25,
                                                $26,
                                                (v4$1, $35) => {
                                                  const $36 = v4$1._3;
                                                  return $25((v5$1) => {
                                                    if ($36) {
                                                      return $31($ParseState(v4$1._1, v4$1._2, false), $35);
                                                    }
                                                    return $32($ParseState($29, $30, false), identity24);
                                                  });
                                                },
                                                (state2, a) => $25((v2$1) => $32(state2, identity24))
                                              )));
                                            });
                                          },
                                          (state2, a) => $25((v2$1) => $32(state2, (a$1) => -a$1))
                                        )));
                                      });
                                    });
                                  };
                                })();
                                return (v2, $25, $26, $27, $28) => {
                                  const $29 = v2._1;
                                  const $30 = v2._2;
                                  return $25((v3) => {
                                    const $31 = (v4, $312) => {
                                      const $32 = v4._3;
                                      return $25((v5) => {
                                        if ($32) {
                                          return $27(v4, $312);
                                        }
                                        return $24(v2, $25, $26, $27, $28);
                                      });
                                    };
                                    return $25((v1) => {
                                      const $32 = (state2, a) => $25((v2$1) => {
                                        const $322 = Float2();
                                        return $25((v1$1) => token.float(
                                          state2,
                                          $25,
                                          $26,
                                          (v2$2, $33) => $31($ParseState(v2$2._1, v2$2._2, false), $33),
                                          (state2$1, a$1) => $25((v2$2) => $28(state2$1, $322(a(a$1))))
                                        ));
                                      });
                                      return $25((v3$1) => $25((v1$1) => $173(
                                        $ParseState($29, $30, false),
                                        $25,
                                        $26,
                                        (v4, $33) => {
                                          const $34 = v4._3;
                                          return $25((v5) => {
                                            if ($34) {
                                              return $31($ParseState(v4._1, v4._2, false), $33);
                                            }
                                            return $25((v3$2) => $25((v1$2) => $182(
                                              $ParseState($29, $30, false),
                                              $25,
                                              $26,
                                              (v4$1, $35) => {
                                                const $36 = v4$1._3;
                                                return $25((v5$1) => {
                                                  if ($36) {
                                                    return $31($ParseState(v4$1._1, v4$1._2, false), $35);
                                                  }
                                                  return $32($ParseState($29, $30, false), identity24);
                                                });
                                              },
                                              (state2, a) => $25((v2$1) => $32(state2, identity24))
                                            )));
                                          });
                                        },
                                        (state2, a) => $25((v2$1) => $32(state2, (a$1) => -a$1))
                                      )));
                                    });
                                  });
                                };
                              })();
                              return (v2, $23, $24, $25, $26) => {
                                const $27 = v2._1;
                                const $28 = v2._2;
                                return $23((v3) => $23((v1) => ident(
                                  $ParseState($27, $28, false),
                                  $23,
                                  $24,
                                  (v2$1, $29) => $23((v5) => $223(v2, $23, $24, $25, $26)),
                                  (state2, a) => $23((v2$1) => $26(state2, $Expr2("Var", a)))
                                )));
                              };
                            })();
                            return (v2, $23, $24, $25, $26) => {
                              const $27 = v2._1;
                              const $28 = v2._2;
                              return $23((v3) => $162(
                                $ParseState($27, $28, false),
                                $23,
                                $24,
                                (v4, $29) => {
                                  const $30 = v4._3;
                                  return $23((v5) => {
                                    if ($30) {
                                      return $25(v4, $29);
                                    }
                                    return $222(v2, $23, $24, $25, $26);
                                  });
                                },
                                $26
                              ));
                            };
                          })();
                          return (v2, $23, $24, $25, $26) => {
                            const $27 = v2._1;
                            const $28 = v2._2;
                            return $23((v3) => $23((v1) => $23((v1$1) => ctr(
                              $ParseState($27, $28, false),
                              $23,
                              $24,
                              (v2$1, $29) => $23((v5) => $22(v2, $23, $24, $25, $26)),
                              (state2, a) => $23((v2$1) => {
                                const $29 = $152(a);
                                return $23((v2$2) => $26(state2, $29(Nil)));
                              })
                            ))));
                          };
                        })();
                        return (v2, $22, $23, $24, $25) => {
                          const $26 = v2._1;
                          const $27 = v2._2;
                          return $22((v3) => $14(
                            $ParseState($26, $27, false),
                            $22,
                            $23,
                            (v4, $28) => {
                              const $29 = v4._3;
                              return $22((v5) => {
                                if ($29) {
                                  return $24(v4, $28);
                                }
                                return $21(v2, $22, $23, $24, $25);
                              });
                            },
                            $25
                          ));
                        };
                      })();
                      return (v2, $21, $22, $23, $24) => {
                        const $25 = v2._1;
                        const $26 = v2._2;
                        return $21((v3) => $13(
                          $ParseState($25, $26, false),
                          $21,
                          $22,
                          (v4, $27) => {
                            const $28 = v4._3;
                            return $21((v5) => {
                              if ($28) {
                                return $23(v4, $27);
                              }
                              return $20(v2, $21, $22, $23, $24);
                            });
                          },
                          $24
                        ));
                      };
                    })();
                    return (v2, $20, $21, $22, $23) => {
                      const $24 = v2._1;
                      const $25 = v2._2;
                      return $20((v3) => {
                        const $26 = (v4, $262) => {
                          const $27 = v4._3;
                          return $20((v5) => {
                            if ($27) {
                              return $22(v4, $262);
                            }
                            return $19(v2, $20, $21, $22, $23);
                          });
                        };
                        return $20((v2$1) => $20((v1) => lBracket(
                          $ParseState($24, $25, false),
                          $20,
                          $21,
                          $26,
                          (state2, a) => $20((v2$2) => $20((v3$1) => $20((v2$3) => $20((v1$1) => go$lazy()(
                            state2,
                            $20,
                            $21,
                            $26,
                            (state2$1, a$1) => $20((v2$4) => {
                              const $27 = $122(a$1);
                              return $20((v3$2) => go$1(
                                state2$1,
                                $20,
                                $21,
                                $26,
                                (state3, a$2) => $20((v4) => {
                                  const $28 = $27(a$2);
                                  return $20((v4$1) => $23(state3, $28));
                                })
                              ));
                            })
                          )))))
                        )));
                      });
                    };
                  })();
                  return (v2, $19, $20, $21, $22) => {
                    const $23 = v2._1;
                    const $24 = v2._2;
                    return $19((v3) => $11($ParseState($23, $24, false), $19, $20, (v2$1, $25) => $19((v5) => $18(v2, $19, $20, $21, $22)), $22));
                  };
                })();
                return (v2, $18, $19, $20, $21) => {
                  const $22 = v2._1;
                  const $23 = v2._2;
                  return $18((v3) => $10(
                    $ParseState($22, $23, false),
                    $18,
                    $19,
                    (v4, $24) => {
                      const $25 = v4._3;
                      return $18((v5) => {
                        if ($25) {
                          return $20(v4, $24);
                        }
                        return $172(v2, $18, $19, $20, $21);
                      });
                    },
                    $21
                  ));
                };
              })();
              return (state1, more, lift1, $$throw2, done) => more((v1) => $17(
                state1,
                more,
                lift1,
                $$throw2,
                (state2, a) => more((v2) => {
                  const $18 = DProject2(a);
                  const $19 = token.reservedOp(".");
                  const $20 = token.brackets(expr_$lazy());
                  const $21 = Project2(a);
                  const $22 = token.reservedOp(".");
                  const $23 = state2._1;
                  const $24 = state2._2;
                  return more((v3) => {
                    const $25 = (v4, $252) => {
                      const $26 = v4._3;
                      return more((v5) => {
                        if ($26) {
                          return $$throw2(v4, $252);
                        }
                        const $27 = state2._1;
                        const $28 = state2._2;
                        return more((v3$1) => {
                          const $29 = (v4$1, $292) => {
                            const $30 = v4$1._3;
                            return more((v5$1) => {
                              if ($30) {
                                return $$throw2(v4$1, $292);
                              }
                              return done(state2, a);
                            });
                          };
                          return more((v1$1) => more((v2$1) => more((v1$2) => $22(
                            $ParseState($27, $28, false),
                            more,
                            lift1,
                            $29,
                            (state2$1, a$1) => more((v2$2) => more((v3$2) => ident(state2$1, more, lift1, $29, (state3, a$2) => more((v4$1) => more((v2$3) => done(state3, $21(a$2)))))))
                          ))));
                        });
                      });
                    };
                    return more((v1$1) => more((v2$1) => more((v1$2) => $19(
                      $ParseState($23, $24, false),
                      more,
                      lift1,
                      $25,
                      (state2$1, a$1) => more((v2$2) => more((v3$1) => $20(state2$1, more, lift1, $25, (state3, a$2) => more((v4) => more((v2$3) => done(state3, $18(a$2)))))))
                    ))));
                  });
                })
              ));
            })();
            const rest = (v$1) => {
              if (v$1.tag === "Constr") {
                const $172 = v$1._2;
                const $18 = v$1._3;
                const $19 = v$1._1;
                return (v2, $20, $21, $22, $23) => {
                  const $24 = v2._1;
                  const $25 = v2._2;
                  return $20((v3) => {
                    const $26 = (v4, $262) => {
                      const $27 = v4._3;
                      return $20((v5) => {
                        if ($27) {
                          return $22(v4, $262);
                        }
                        return $23(v2, v$1);
                      });
                    };
                    return $20((v1) => simpleExprOrProjection(
                      $ParseState($24, $25, false),
                      $20,
                      $21,
                      $26,
                      (state2, a) => $20((v2$1) => rest($Expr2(
                        "Constr",
                        $19,
                        $172,
                        foldableList.foldr(Cons)($List("Cons", a, Nil))($18)
                      ))(state2, $20, $21, $26, $23))
                    ));
                  });
                };
              }
              const $17 = App3(v$1);
              return (v2, $18, $19, $20, $21) => {
                const $22 = v2._1;
                const $23 = v2._2;
                return $18((v3) => {
                  const $24 = (v4, $242) => {
                    const $25 = v4._3;
                    return $18((v5) => {
                      if ($25) {
                        return $20(v4, $242);
                      }
                      return $21(v2, v$1);
                    });
                  };
                  return $18((v1) => $18((v1$1) => simpleExprOrProjection(
                    $ParseState($22, $23, false),
                    $18,
                    $19,
                    $24,
                    (state2, a) => $18((v2$1) => {
                      const $25 = $17(a);
                      return $18((v2$2) => rest($25)(state2, $18, $19, $24, $21));
                    })
                  )));
                });
              };
            };
            return (state1, more, lift1, $$throw2, done) => more((v1) => simpleExprOrProjection(
              state1,
              more,
              lift1,
              $$throw2,
              (state2, a) => more((v2) => rest(a)(state2, more, lift1, $$throw2, done))
            ));
          })();
          return (v2, $17, $18, $19, $20) => {
            const $21 = v2._1;
            const $22 = v2._2;
            return $17((v3) => {
              const $23 = (v4, $232) => {
                const $24 = v4._3;
                return $17((v5) => {
                  if ($24) {
                    return $19(v4, $232);
                  }
                  return $16(v2, $17, $18, $19, $20);
                });
              };
              return $17((v1) => $17((v1$1) => $92(
                $ParseState($21, $22, false),
                $17,
                $18,
                $23,
                (state2, a) => $17((v2$1) => {
                  const $24 = bindList.bind($List("Cons", a._1, a._2))(identity5);
                  return $17((v2$2) => {
                    const $25 = foldableList.foldr((def) => fanin3(Let2)(LetRec2)(def));
                    const $26 = keyword("in");
                    return $17((v1$2) => $17((v1$3) => $17((v2$3) => $17((v1$4) => $26(
                      state2,
                      $17,
                      $18,
                      $23,
                      (state2$1, a$1) => $17((v2$4) => $17((v3$1) => go$lazy()(
                        state2$1,
                        $17,
                        $18,
                        $23,
                        (state3, a$2) => $17((v4) => $17((v2$5) => {
                          const $27 = $25(a$2);
                          return $17((v2$6) => $20(state3, $27($24)));
                        }))
                      )))
                    )))));
                  });
                })
              )));
            });
          };
        })();
        return (v2, $16, $17, $18, $19) => {
          const $20 = v2._1;
          const $21 = v2._2;
          return $16((v3) => {
            const $22 = (v4, $222) => {
              const $23 = v4._3;
              return $16((v5) => {
                if ($23) {
                  return $18(v4, $222);
                }
                return $15(v2, $16, $17, $18, $19);
              });
            };
            return $16((v1) => $16((v2$1) => $16((v1$1) => $7(
              $ParseState($20, $21, false),
              $16,
              $17,
              $22,
              (state2, a) => $16((v2$2) => $16((v3$1) => $8(state2, $16, $17, $22, (state3, a$1) => $16((v4) => $16((v2$3) => $19(state3, $Expr2("Lambda", a$1)))))))
            ))));
          });
        };
      })();
      return (v2, $13, $14, $15, $16) => {
        const $17 = v2._1;
        const $18 = v2._2;
        return $13((v3) => {
          const $19 = (v4, $192) => {
            const $20 = v4._3;
            return $13((v5) => {
              if ($20) {
                return $15(v4, $192);
              }
              return $12(v2, $13, $14, $15, $16);
            });
          };
          return $13((v2$1) => $13((v2$2) => $13((v1) => $13((v2$3) => $13((v2$4) => $13((v1$1) => $13((v2$5) => $13((v3$1) => $13((v2$6) => $13((v1$2) => $4(
            $ParseState($17, $18, false),
            $13,
            $14,
            $19,
            (state2, a) => $13((v2$7) => $13((v3$2) => go$lazy()(
              state2,
              $13,
              $14,
              $19,
              (state3, a$1) => $13((v4) => $13((v4$1) => {
                const $20 = IfElse(a$1);
                return $13((v2$8) => $13((v3$3) => $5(
                  state3,
                  $13,
                  $14,
                  $19,
                  (state3$1, a$2) => $13((v4$2) => $13((v3$4) => go$lazy()(
                    state3$1,
                    $13,
                    $14,
                    $19,
                    (state3$2, a$3) => $13((v4$3) => {
                      const $21 = $20(a$3);
                      return $13((v2$9) => $13((v3$5) => $6(
                        state3$2,
                        $13,
                        $14,
                        $19,
                        (state3$3, a$4) => $13((v4$4) => $13((v3$6) => go$lazy()(state3$3, $13, $14, $19, (state3$4, a$5) => $13((v4$5) => $16(state3$4, $21(a$5))))))
                      )));
                    })
                  )))
                )));
              }))
            )))
          )))))))))));
        });
      };
    })();
    return (v2, $10, $11, $12, $13) => {
      const $14 = v2._1;
      const $15 = v2._2;
      return $10((v3) => {
        const $16 = (v4, $162) => {
          const $17 = v4._3;
          return $10((v5) => {
            if ($17) {
              return $12(v4, $162);
            }
            return $9(v2, $10, $11, $12, $13);
          });
        };
        return $10((v2$1) => $10((v1) => $10((v2$2) => $10((v1$1) => $10((v2$3) => $10((v1$2) => $1(
          $ParseState($14, $15, false),
          $10,
          $11,
          $16,
          (state2, a) => $10((v2$4) => $10((v3$1) => go$lazy()(
            state2,
            $10,
            $11,
            $16,
            (state3, a$1) => $10((v4) => $10((v2$5) => $10((v3$2) => $2(
              state3,
              $10,
              $11,
              $16,
              (state3$1, a$2) => $10((v4$1) => $10((v2$6) => {
                const $17 = MatchAs(a$1);
                return $10((v3$3) => $3(state3$1, $10, $11, $16, (state3$2, a$3) => $10((v4$2) => $13(state3$2, $17(a$3)))));
              }))
            ))))
          )))
        )))))));
      });
    };
  })())));
  const go = go$lazy();
  return go;
});
var expr_ = /* @__PURE__ */ expr_$lazy();
var module_ = /* @__PURE__ */ (() => {
  const $0 = topLevel((() => {
    const $02 = sepBy_try(defs(expr_))(token.semi);
    return (state1, more, lift1, $$throw2, done) => more((v2) => more((v1) => $02(
      state1,
      more,
      lift1,
      $$throw2,
      (state2, a) => more((v2$1) => more((v3) => token.semi(state2, more, lift1, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))))
    )));
  })());
  return (state1, more, lift1, $$throw2, done) => more((v1) => $0(
    state1,
    more,
    lift1,
    $$throw2,
    (state2, a) => more((v2) => done(state2, $Module(bindList.bind(a)(identity5))))
  ));
})();

// output-es/Trace/index.js
var $AppTrace = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
var $ForeignTrace$p = (_1, _2) => ({ tag: "ForeignTrace'", _1, _2 });
var $Match = (tag, _1, _2) => ({ tag, _1, _2 });
var $Trace = (tag, _1, _2, _3, _4) => ({ tag, _1, _2, _3, _4 });
var $VarDef3 = (_1, _2) => ({ tag: "VarDef", _1, _2 });
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
    return go$r;
  };
  return go(Leaf2);
})();
var Const2 = /* @__PURE__ */ $Trace("Const");
var bVMatch = {
  bv: (v) => {
    if (v.tag === "MatchVar") {
      return $$$Map("Two", Leaf2, v._1, void 0, Leaf2);
    }
    if (v.tag === "MatchVarAnon") {
      return Leaf2;
    }
    if (v.tag === "MatchConstr") {
      return unions2(listMap(bVMatch.bv)(v._2));
    }
    if (v.tag === "MatchDict") {
      return fold((z) => (v$1) => union(ordString)(z))(Leaf2)(_fmapObject(v._1, bVMatch.bv));
    }
    fail();
  }
};

// output-es/Eval/index.js
var disjointUnion3 = /* @__PURE__ */ disjointUnion(mapEnvStringVal);
var fromFoldable20 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)()(m))(Leaf2))();
var show23 = /* @__PURE__ */ (() => showSet(showString).show)();
var toUnfoldable11 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
var fromFoldable110 = /* @__PURE__ */ fromFoldable2(foldableList);
var union7 = /* @__PURE__ */ (() => setSet(ordString).union)();
var fv2 = /* @__PURE__ */ (() => fVDict(fVElim).fv)();
var unzip5 = /* @__PURE__ */ unzip3(functorList);
var fromFoldable25 = /* @__PURE__ */ (() => fromFoldableImpl(foldableList.foldr))();
var greaterThanOrEq1 = /* @__PURE__ */ (() => {
  const $0 = ordTuple(ordInt)(ordInt);
  return (a1) => (a2) => $0.compare(a1)(a2) !== "LT";
})();
var show32 = (v) => "(Tuple " + showIntImpl(v._1) + " " + showIntImpl(v._2) + ")";
var erase1 = /* @__PURE__ */ (() => functorElim.map((v) => {
}))();
var matchMany2 = (dictMonadError) => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  const $0 = Monad0.Applicative0();
  const $1 = Monad0.Bind1();
  return (dictAnn) => {
    const BoundedMeetSemilattice1 = dictAnn.BoundedLattice1().BoundedMeetSemilattice1();
    const top = BoundedMeetSemilattice1.top;
    return (v) => (v1) => {
      if (v.tag === "Nil") {
        return $0.pure($Tuple(empty, $Tuple(v1, $Tuple(top, Nil))));
      }
      if (v.tag === "Cons") {
        if (v1.tag === "ContElim") {
          const $2 = v._2;
          return $1.bind(match4(dictMonadError)(dictAnn)(v._1)(v1._1))((v3) => {
            const $3 = v3._2._2._2;
            const $4 = v3._2._2._1;
            const $5 = v3._1;
            return $1.bind(matchMany2(dictMonadError)(dictAnn)($2)(v3._2._1))((v4) => $0.pure($Tuple(
              disjointUnion3($5)(v4._1),
              $Tuple(v4._2._1, $Tuple(BoundedMeetSemilattice1.MeetSemilattice0().meet($4)(v4._2._2._1), $List("Cons", $3, v4._2._2._2)))
            )));
          });
        }
        if (v1.tag === "ContExpr") {
          return MonadThrow0.throwError(error(showIntImpl((() => {
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
              return go$r;
            };
            return go(0)(v._2) + 1 | 0;
          })()) + " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"));
        }
      }
      fail();
    };
  };
};
var match4 = (dictMonadError) => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  const $0 = Monad0.Applicative0();
  const Bind1 = Monad0.Bind1();
  const withMsg2 = withMsg(dictMonadError);
  const consistentWith2 = consistentWith(dictMonadError);
  return (dictAnn) => {
    const BoundedMeetSemilattice1 = dictAnn.BoundedLattice1().BoundedMeetSemilattice1();
    const top = BoundedMeetSemilattice1.top;
    const $1 = BoundedMeetSemilattice1.MeetSemilattice0();
    const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
    return (v) => (v1) => {
      if (v1.tag === "ElimVar") {
        if (v1._1 === "_") {
          return $0.pure($Tuple(
            empty,
            $Tuple(v1._2, $Tuple(top, $Match("MatchVarAnon", $Val(void 0, functorBaseVal.map((v$1) => {
            })(v._2)))))
          ));
        }
        const $2 = v1._1;
        return $0.pure($Tuple(
          (() => {
            const $3 = {};
            $3[$2] = v;
            return $3;
          })(),
          $Tuple(v1._2, $Tuple(top, $Match("MatchVar", $2, $Val(void 0, functorBaseVal.map((v$1) => {
          })(v._2)))))
        ));
      }
      if (v1.tag === "ElimConstr") {
        if (v._2.tag === "Constr") {
          const $2 = v._2._1;
          const $3 = v1._1;
          const $4 = v._2._2;
          const $5 = v._1;
          return Bind1.bind(withMsg2("Pattern mismatch")(consistentWith2($$$Map("Two", Leaf2, $2, void 0, Leaf2))(mapObjectString.keys($3))))(() => Bind1.bind(orElse(MonadThrow0)("Incomplete patterns: no branch for " + showCtr($2))(_lookup(
            Nothing,
            Just,
            $2,
            $3
          )))((\u03BA) => Bind1.bind(matchMany2(dictMonadError)(dictAnn)($4)(\u03BA))((v2) => $0.pure($Tuple(
            v2._1,
            $Tuple(v2._2._1, $Tuple($1.meet($5)(v2._2._2._1), $Match("MatchConstr", $2, v2._2._2._2)))
          )))));
        }
        return Bind1.bind(dataTypeForSetCtr.dataTypeFor(MonadThrow0)(mapObjectString.keys(v1._1)))((d) => MonadThrow0.throwError(error("Pattern mismatch: found " + prettyP2(v) + ", expected " + d._1)));
      }
      if (v1.tag === "ElimDict") {
        if (v._2.tag === "Dictionary") {
          const $2 = v1._1;
          const $3 = v._2._1;
          const $4 = v._1;
          const $5 = v1._2;
          return Bind1.bind(check(MonadThrow0)(difference2(ordString)($2)(fromFoldable20(mapObjectString.keys($3))).tag === "Leaf")("Pattern mismatch: found " + show23(mapObjectString.keys($3)) + ", expected " + show23($2)))(() => {
            const xs$p = toUnfoldable11($2);
            return Bind1.bind(matchMany2(dictMonadError)(dictAnn)(listMap((k) => $$get(showString)(mapObjectString)(k)($3)._2)(xs$p))($5))((v2) => $0.pure($Tuple(
              v2._1,
              $Tuple(
                v2._2._1,
                $Tuple($1.meet($4)(v2._2._2._1), $Match("MatchDict", fromFoldable110(zipWith(Tuple)(xs$p)(v2._2._2._2))))
              )
            )));
          });
        }
        return MonadThrow0.throwError(error("Pattern mismatch: found " + prettyP2(v) + ", expected " + show23(v1._1)));
      }
      fail();
    };
  };
};
var closeDefs2 = (\u03B3) => (\u03C1) => (\u03B1) => _fmapObject(
  \u03C1,
  (\u03C3) => {
    const \u03C1$p = forDefs(\u03C1)(\u03C3);
    return $Val(
      \u03B1,
      $BaseVal(
        "Fun",
        $Fun(
          "Closure",
          (() => {
            const $0 = union7(fv2(\u03C1$p))(fVElim.fv(\u03C3));
            return filterWithKey2((x) => {
              const $1 = setSet(ordString).member(x)($0);
              return (v) => $1;
            })(\u03B3);
          })(),
          \u03C1$p,
          \u03C3
        )
      )
    );
  }
);
var checkArity2 = (dictMonadError) => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  return (c) => (n) => MonadThrow0.Monad0().Bind1().bind(arity(MonadThrow0)(c))((n$p) => check(MonadThrow0)(n$p >= n)(showCtr(c) + " got " + showIntImpl(n) + " argument(s), expects at most " + showIntImpl(n$p)));
};
var $$eval2 = (dictMonadError) => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  const Bind1 = Monad0.Bind1();
  const Functor0 = Bind1.Apply0().Functor0();
  const Applicative0 = Monad0.Applicative0();
  const traverse2 = traversableList.traverse(Applicative0);
  const traverse3 = traversablePair.traverse(Applicative0);
  const checkArity1 = checkArity2(dictMonadError);
  const sequence1 = traversableList.traverse(Applicative0)(identity2);
  const match1 = match4(dictMonadError);
  return (dictAnn) => {
    const $0 = dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0();
    const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
    const match22 = match1(dictAnn);
    return (v) => (v1) => {
      if (v._2.tag === "Var") {
        const $1 = v._2._1;
        return Functor0.map((v2) => $Tuple($Trace("Var", $1), v2))(lookup$p(MonadThrow0)(showString)(mapEnvStringVal)($1)(v._1));
      }
      if (v._2.tag === "Op") {
        const $1 = v._2._1;
        return Functor0.map((v2) => $Tuple($Trace("Op", $1), v2))(lookup$p(MonadThrow0)(showString)(mapEnvStringVal)($1)(v._1));
      }
      if (v._2.tag === "Int") {
        return Applicative0.pure($Tuple(Const2, $Val($0.meet(v._2._1)(v1), $BaseVal("Int", v._2._2))));
      }
      if (v._2.tag === "Float") {
        return Applicative0.pure($Tuple(Const2, $Val($0.meet(v._2._1)(v1), $BaseVal("Float", v._2._2))));
      }
      if (v._2.tag === "Str") {
        return Applicative0.pure($Tuple(Const2, $Val($0.meet(v._2._1)(v1), $BaseVal("Str", v._2._2))));
      }
      if (v._2.tag === "Dictionary") {
        const $1 = v._2._1;
        const $2 = v._1;
        return Bind1.bind(Functor0.map((x) => {
          const $3 = unzip(listMap(toTuple)(x));
          return $Tuple(unzip5($3._1), unzip5($3._2));
        })(traverse2(traverse3((e) => $$eval2(dictMonadError)(dictAnn)($EnvExpr($2, e))(v1)))(v._2._2)))((v2) => {
          const v3 = unzip5(listMap((v$1) => $Tuple(v$1._2.tag === "Str" ? v$1._2._1 : typeError(v$1._2)("Str"), v$1._1))(v2._1._2));
          const d = fromFoldable110(zipWith(Tuple)(v3._1)(zipWith(Tuple)(v3._2)(v2._2._2)));
          return Applicative0.pure($Tuple(
            $Trace(
              "Dictionary",
              zipWith(Tuple)(v3._1)(zipWith(Tuple)(v2._1._1)(v2._2._1)),
              _fmapObject(d, (x) => $Val(void 0, functorBaseVal.map((v$1) => {
              })(x._2._2)))
            ),
            $Val($0.meet($1)(v1), $BaseVal("Dictionary", d))
          ));
        });
      }
      if (v._2.tag === "Constr") {
        const $1 = v._2._2;
        const $2 = v._2._3;
        const $3 = v._2._1;
        const $4 = v._1;
        return Bind1.bind(checkArity1($1)((() => {
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
            return go$r;
          };
          return go(0)($2);
        })()))(() => Bind1.bind(Functor0.map(unzip5)(traverse2((e) => $$eval2(dictMonadError)(dictAnn)($EnvExpr($4, e))(v1))($2)))((v2) => Applicative0.pure($Tuple(
          $Trace("Constr", $1, v2._1),
          $Val($0.meet($3)(v1), $BaseVal("Constr", $1, v2._2))
        ))));
      }
      if (v._2.tag === "Matrix") {
        const $1 = v._2._2;
        const $2 = v._2._3._1;
        const $3 = v._2._3._2;
        const $4 = v._2._1;
        const $5 = v._1;
        return Bind1.bind($$eval2(dictMonadError)(dictAnn)($EnvExpr($5, v._2._4))(v1))((v2) => {
          const $6 = v2._1;
          const v4 = intPair.unpack(v2._2._2);
          const $7 = v4._1._1;
          const $8 = v4._2._1;
          const $9 = v4._1._2;
          const $10 = v4._2._2;
          return Bind1.bind(check(MonadThrow0)(greaterThanOrEq1($Tuple($7, $8))($Tuple(1, 1)))("array must be at least (" + show32($Tuple(
            1,
            1
          )) + "); got (" + show32($Tuple($7, $8)) + ")"))(() => Bind1.bind(Functor0.map((() => {
            const $11 = listMap((x) => {
              const $112 = unzip5(x);
              return $Tuple(fromFoldable25($112._1), fromFoldable25($112._2));
            });
            return (x) => {
              const $12 = unzip5($11(x));
              return $Tuple(fromFoldable25($12._1), fromFoldable25($12._2));
            };
          })())(sequence1(bindList.bind(range(1)($7))((i) => $List(
            "Cons",
            sequence1(bindList.bind(range(1)($8))((j) => $List(
              "Cons",
              $$eval2(dictMonadError)(dictAnn)($EnvExpr(
                unionWith2((v$1) => identity14)($5)(disjointUnion3((() => {
                  const $11 = {};
                  $11[$2] = $Val($9, $BaseVal("Int", i));
                  return $11;
                })())((() => {
                  const $11 = {};
                  $11[$3] = $Val($10, $BaseVal("Int", j));
                  return $11;
                })())),
                $1
              ))(v1),
              Nil
            ))),
            Nil
          )))))((v5) => Applicative0.pure($Tuple(
            $Trace("Matrix", v5._1, $Tuple($2, $3), $Tuple($7, $8), $6),
            $Val($0.meet($4)(v1), $BaseVal("Matrix", $Tuple(v5._2, $Tuple($Tuple($7, $9), $Tuple($8, $10)))))
          ))));
        });
      }
      if (v._2.tag === "Lambda") {
        return Applicative0.pure($Tuple(
          Const2,
          $Val(
            $0.meet(v._2._1)(v1),
            $BaseVal(
              "Fun",
              $Fun(
                "Closure",
                (() => {
                  const $1 = fVElim.fv(v._2._2);
                  return filterWithKey2((x) => {
                    const $2 = setSet(ordString).member(x)($1);
                    return (v$1) => $2;
                  })(v._1);
                })(),
                empty,
                v._2._2
              )
            )
          )
        ));
      }
      if (v._2.tag === "Project") {
        const $1 = v._2._2;
        return Bind1.bind($$eval2(dictMonadError)(dictAnn)($EnvExpr(v._1, v._2._1))(v1))((v2) => {
          if (v2._2._2.tag === "Dictionary") {
            return orElse(MonadThrow0)('Key "' + $1 + '" not found')((() => {
              const $2 = _lookup(Nothing, Just, $1, v2._2._2._1);
              if ($2.tag === "Just") {
                return $Maybe("Just", $Tuple($Trace("DProject", v2._1, Nothing, $1), $2._1._2));
              }
              return Nothing;
            })());
          }
          return MonadThrow0.throwError(error("Found " + prettyP2(v2._2) + ", expected record"));
        });
      }
      if (v._2.tag === "DProject") {
        const $1 = v._2._2;
        const $2 = v._1;
        return Bind1.bind($$eval2(dictMonadError)(dictAnn)($EnvExpr($2, v._2._1))(v1))((v2) => {
          const $3 = v2._1;
          const $4 = v2._2;
          return Bind1.bind($$eval2(dictMonadError)(dictAnn)($EnvExpr($2, $1))(v1))((v4) => {
            if ($4._2.tag === "Dictionary") {
              if (v4._2._2.tag === "Str") {
                return orElse(MonadThrow0)('Key "' + v4._2._2._1 + '" not found')((() => {
                  const $5 = _lookup(Nothing, Just, v4._2._2._1, $4._2._1);
                  if ($5.tag === "Just") {
                    return $Maybe("Just", $Tuple($Trace("DProject", $3, $Maybe("Just", v4._1), v4._2._2._1), $5._1._2));
                  }
                  return Nothing;
                })());
              }
              return MonadThrow0.throwError(error("Found " + prettyP2(v4._2) + ", expected string"));
            }
            return MonadThrow0.throwError(error("Found " + prettyP2($4) + ", expected dict"));
          });
        });
      }
      if (v._2.tag === "App") {
        const $1 = v._2._2;
        const $2 = v._1;
        return Bind1.bind($$eval2(dictMonadError)(dictAnn)($EnvExpr($2, v._2._1))(v1))((v2) => {
          const $3 = v2._1;
          const $4 = v2._2;
          return Bind1.bind($$eval2(dictMonadError)(dictAnn)($EnvExpr($2, $1))(v1))((v4) => {
            const $5 = v4._1;
            return Bind1.bind(apply3(dictMonadError)(dictAnn)($Tuple($4, v4._2)))((v5) => Applicative0.pure($Tuple($Trace("App", $3, $5, v5._1), v5._2)));
          });
        });
      }
      if (v._2.tag === "Let") {
        const $1 = v._2._2;
        const $2 = v._1;
        const $3 = v._2._1._1;
        return Bind1.bind($$eval2(dictMonadError)(dictAnn)($EnvExpr($2, v._2._1._2))(v1))((v2) => {
          const $4 = v2._1;
          return Bind1.bind(match22(v2._2)($3))((v4) => {
            const $5 = v4._2._2._2;
            return Bind1.bind($$eval2(dictMonadError)(dictAnn)($EnvExpr(unionWith2((v$1) => identity14)($2)(v4._1), $1))(v4._2._2._1))((v5) => Applicative0.pure($Tuple(
              $Trace("Let", $VarDef3($5, $4), v5._1),
              v5._2
            )));
          });
        });
      }
      if (v._2.tag === "LetRec") {
        const $1 = v._2._1._2;
        return Bind1.bind($$eval2(dictMonadError)(dictAnn)($EnvExpr(
          unionWith2((v$1) => identity14)(v._1)(closeDefs2(v._1)($1)($0.meet(v._2._1._1)(v1))),
          v._2._2
        ))($0.meet(v._2._1._1)(v1)))((v2) => Applicative0.pure($Tuple(
          $Trace("LetRec", $RecDefs(void 0, _fmapObject($1, erase1)), v2._1),
          v2._2
        )));
      }
      fail();
    };
  };
};
var apply3 = (dictMonadError) => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  const Bind1 = Monad0.Bind1();
  const match1 = match4(dictMonadError);
  const $0 = Monad0.Applicative0();
  return (dictAnn) => {
    const match22 = match1(dictAnn);
    const prettyP2 = prettyP(prettyVal(dictAnn.Highlightable0()));
    return (v) => {
      const $1 = (v1) => MonadThrow0.throwError(error("Found " + prettyP2(v1) + ", expected function"));
      if (v._1._2.tag === "Fun") {
        if (v._1._2._1.tag === "Closure") {
          const $2 = v._1._1;
          const $3 = v._1._2._1._1;
          const $4 = v._1._2._1._2;
          const \u03B32 = closeDefs2($3)($4)($2);
          return Bind1.bind(match22(v._2)(v._1._2._1._3))((v2) => {
            const $5 = v2._2._2._2;
            return Bind1.bind($$eval2(dictMonadError)(dictAnn)($EnvExpr(
              unionWith2((v$1) => identity14)(unionWith2((v$1) => identity14)($3)(\u03B32))(v2._1),
              v2._2._1.tag === "ContExpr" ? v2._2._1._1 : throwException(error("Expression expected"))()
            ))(dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0().meet($2)(v2._2._2._1)))((v3) => $0.pure($Tuple(
              $AppTrace("AppClosure", fromFoldable20(mapObjectString.keys($4)), $5, v3._1),
              v3._2
            )));
          });
        }
        if (v._1._2._1.tag === "Foreign") {
          const $2 = v._1._2._1._1._1;
          const $3 = v._1._2._1._2;
          const $4 = v._1._2._1._1._2;
          const vs$p = foldableList.foldr(Cons)($List("Cons", v._2, Nil))($3);
          return Bind1.bind((() => {
            const $5 = $4._1;
            return Bind1.bind((() => {
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
                return go$r;
              };
              return $5.arity > go(0)(vs$p);
            })() ? $0.pure($Tuple(Nothing, $Val(v._1._1, $BaseVal("Fun", $Fun("Foreign", $Tuple($2, $4), vs$p))))) : Bind1.Apply0().Functor0().map((v$1) => $Tuple($Maybe("Just", v$1._1), v$1._2))($5.op(dictAnn)(dictMonadError)(vs$p)))((v3) => $0.pure($Tuple(
              $Tuple($2, $ForeignTrace$p($ForeignOp$p($5), v3._1)),
              v3._2
            )));
          })())((v2) => $0.pure($Tuple(
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
                  return go$r;
                };
                return go(0)($3) + 1 | 0;
              })(),
              v2._1
            ),
            v2._2
          )));
        }
        if (v._1._2._1.tag === "PartialConstr") {
          const $2 = v._1._2._1._1;
          const $3 = v._1._2._1._2;
          const n = defined(arity(monadThrowExceptT(monadIdentity))($2));
          const v$p = (() => {
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
              return go$r;
            };
            return go(0)($3) < (n - 1 | 0);
          })() ? $Val(
            v._1._1,
            $BaseVal(
              "Fun",
              $Fun("PartialConstr", $2, foldableList.foldr(Cons)($List("Cons", v._2, Nil))($3))
            )
          ) : $Val(
            v._1._1,
            $BaseVal("Constr", $2, foldableList.foldr(Cons)($List("Cons", v._2, Nil))($3))
          );
          return Bind1.bind(check(MonadThrow0)((() => {
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
              return go$r;
            };
            return go(0)($3) < n;
          })())("Too many arguments to " + showCtr($2)))(() => $0.pure($Tuple($AppTrace("AppConstr", $2), v$p)));
        }
      }
      return $1(v._2);
    };
  };
};
var apply22 = (dictMonadError) => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  const $0 = Monad0.Bind1();
  const apply1 = apply3(dictMonadError);
  return (dictAnn) => {
    const apply32 = apply1(dictAnn);
    return (v) => {
      const $1 = v._2._2;
      return $0.bind(apply32($Tuple(v._1, v._2._1)))((v3) => {
        const $2 = v3._1;
        return $0.bind(apply32($Tuple(v3._2, $1)))((v4) => Monad0.Applicative0().pure($Tuple($Tuple($2, v4._1), v4._2)));
      });
    };
  };
};

// output-es/EvalBwd/index.js
var disjointUnion_inv2 = /* @__PURE__ */ disjointUnion_inv(ordString)(mapEnvStringVal);
var toUnfoldable15 = /* @__PURE__ */ toAscUnfoldable(unfoldableList);
var fromFoldable21 = /* @__PURE__ */ fromFoldable2(foldableList);
var fromFoldable111 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)()(m))(Leaf2))();
var botOfUnit$x215Raw$x2152 = (dictBoundedJoinSemilattice) => ({
  botOf: (() => {
    const $0 = dictBoundedJoinSemilattice.bot;
    const $1 = functorVal.map((() => {
      const $12 = dictBoundedJoinSemilattice.bot;
      return (v) => $12;
    })());
    return (x) => $Tuple($0, $1(x._2));
  })()
});
var union8 = /* @__PURE__ */ (() => setSet(ordString).union)();
var disjointUnion4 = /* @__PURE__ */ disjointUnion(mapEnvStringVal);
var foldl1 = /* @__PURE__ */ (() => foldable1NonEmpty(foldableList).foldl1)();
var matchManyBwd = (dictAnn) => (v) => (v1) => (v2) => (v3) => {
  if (v3.tag === "Nil") {
    if (isEmpty2(v)) {
      return $Tuple(Nil, v1);
    }
    return throwException(error("absurd"))();
  }
  if (v3.tag === "Cons") {
    const v4 = disjointUnion_inv2(bVMatch.bv(v3._1))(v);
    const v5 = matchBwd(dictAnn)(v4._1)(v1)(v2)(v3._1);
    const v7 = matchManyBwd(dictAnn)(v4._2)($Cont("ContElim", v5._2))(v2)(v3._2);
    return $Tuple(foldableList.foldr(Cons)($List("Cons", v5._1, Nil))(v7._1), v7._2);
  }
  fail();
};
var matchBwd = (dictAnn) => {
  const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
  const $0 = functorVal.map((() => {
    const $02 = BoundedJoinSemilattice0.bot;
    return (v) => $02;
  })());
  const bot1 = BoundedJoinSemilattice0.bot;
  return (v) => (v1) => (v2) => (v3) => {
    if (v3.tag === "MatchVar") {
      if (eqMap(eqString)(eqUnit).eq(mapObjectString.keys(v))($$$Map(
        "Two",
        Leaf2,
        v3._1,
        void 0,
        Leaf2
      ))) {
        return $Tuple($$get(showString)(mapEnvStringVal)(v3._1)(v), $Elim("ElimVar", v3._1, v1));
      }
      return $Tuple($0(v3._2), $Elim("ElimVar", v3._1, v1));
    }
    if (v3.tag === "MatchVarAnon") {
      if (isEmpty2(v)) {
        return $Tuple($0(v3._1), $Elim("ElimVar", "_", v1));
      }
      return throwException(error("absurd"))();
    }
    if (v3.tag === "MatchConstr") {
      const $1 = v3._1;
      const v4 = matchManyBwd(dictAnn)(v)(v1)(v2)(reverse(v3._2));
      const $2 = v4._2;
      return $Tuple(
        $Val(v2, $BaseVal("Constr", $1, v4._1)),
        $Elim(
          "ElimConstr",
          (() => {
            const $3 = {};
            $3[$1] = $2;
            return $3;
          })()
        )
      );
    }
    if (v3.tag === "MatchDict") {
      const v4 = unzip(toUnfoldable15(v3._1));
      const v5 = matchManyBwd(dictAnn)(v)(v1)(v2)(reverse(v4._2));
      return $Tuple(
        $Val(v2, $BaseVal("Dictionary", fromFoldable21(zipWith(Tuple)(v4._1)(listMap((v6) => $Tuple(bot1, v6))(v5._1))))),
        $Elim("ElimDict", fromFoldable111(mapObjectString.keys(v3._1)), v5._2)
      );
    }
    fail();
  };
};
var closeDefsBwd = (dictAnn) => {
  const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
  const JoinSemilattice0 = BoundedJoinSemilattice0.JoinSemilattice0();
  const join1 = mapDictString.unionWith(joinSemilatticeElim(JoinSemilattice0).join);
  const bot1 = BoundedJoinSemilattice0.bot;
  return (\u03B3) => {
    const v = foldrWithIndexDefault(foldableWithIndexStringDi)((f) => (v2) => (v1) => {
      const v22 = $$get(showString)(mapEnvStringVal)(f)(\u03B3);
      if (v22._2.tag === "Fun" && v22._2._1.tag === "Closure") {
        const $0 = v22._2._1._3;
        return $Tuple(
          mutate(($1) => () => {
            $1[f] = $0;
            return $1;
          })(v1._1),
          $Tuple(
            unionWith2(joinSemilatticeVal(JoinSemilattice0).join)(v1._2._1)(v22._2._1._1),
            $Tuple(join1(v1._2._2._1)(v22._2._1._2), JoinSemilattice0.join(v1._2._2._2)(v22._1))
          )
        );
      }
      return throwException(error("absurd"))();
    })($Tuple(empty, $Tuple(empty, $Tuple(empty, bot1))))(\u03B3);
    return $Tuple(v._2._1, $Tuple(join1(v._2._2._1)(v._1), v._2._2._2));
  };
};
var evalBwd$p = (dictAnn) => {
  const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
  const bot1 = BoundedJoinSemilattice0.bot;
  const JoinSemilattice0 = BoundedJoinSemilattice0.JoinSemilattice0();
  const join1 = JoinSemilattice0.join;
  const matchBwd1 = matchBwd(dictAnn);
  const closeDefsBwd1 = closeDefsBwd(dictAnn);
  return (v) => (v1) => {
    const $0 = (t, v2, x) => {
      const v3 = evalBwd$p(dictAnn)($Val(
        bot1,
        $BaseVal(
          "Dictionary",
          (() => {
            const $02 = {};
            $02[x] = $Tuple(bot1, v2);
            return $02;
          })()
        )
      ))(t);
      return $Tuple(v3._1, $Tuple($Expr("Project", v3._2._1, x), v3._2._2));
    };
    const $1 = (t, v2, x) => {
      const v3 = evalBwd$p(dictAnn)($Val(
        bot1,
        $BaseVal(
          "Dictionary",
          (() => {
            const $12 = {};
            $12[x] = $Tuple(bot1, v2);
            return $12;
          })()
        )
      ))(t);
      return $Tuple(v3._1, $Tuple($Expr("Project", v3._2._1, x), v3._2._2));
    };
    const $2 = (t, t$p, v2, x) => {
      const v3 = evalBwd$p(dictAnn)($Val(
        bot1,
        $BaseVal(
          "Dictionary",
          (() => {
            const $22 = {};
            $22[x] = $Tuple(bot1, v2);
            return $22;
          })()
        )
      ))(t);
      return $Tuple(v3._1, $Tuple($Expr("DProject", v3._2._1, evalBwd$p(dictAnn)($Val(bot1, $BaseVal("Str", x)))(t$p)._2._1), v3._2._2));
    };
    const $3 = (t1, t2, t3, v2) => {
      const v3 = applyBwd(dictAnn)($Tuple(t3, v2));
      const v4 = evalBwd$p(dictAnn)(v3._1)(t1);
      const v5 = evalBwd$p(dictAnn)(v3._2)(t2);
      return $Tuple(
        unionWith2(joinSemilatticeVal(JoinSemilattice0).join)(v4._1)(v5._1),
        $Tuple($Expr("App", v4._2._1, v5._2._1), join1(v4._2._2)(v5._2._2))
      );
    };
    const $4 = (t1, t2, v2, w) => {
      const v3 = evalBwd$p(dictAnn)(v2)(t2);
      const v4 = append_inv(ordString)(mapEnvStringVal)(bVMatch.bv(w))(v3._1);
      const v5 = matchBwd1(v4._2)($Cont("ContExpr", $Expr("Dictionary", bot1, Nil)))(v3._2._2)(w);
      const v6 = evalBwd$p(dictAnn)(v5._1)(t1);
      return $Tuple(
        unionWith2(joinSemilatticeVal(JoinSemilattice0).join)(v4._1)(v6._1),
        $Tuple($Expr("Let", $VarDef(v5._2, v6._2._1), v3._2._1), v6._2._2)
      );
    };
    const $5 = (t, v2, \u03C1) => {
      const v3 = evalBwd$p(dictAnn)(v2)(t);
      const v4 = append_inv(ordString)(mapEnvStringVal)(fromFoldable111(mapObjectString.keys(\u03C1)))(v3._1);
      const v5 = closeDefsBwd1(v4._2);
      return $Tuple(
        unionWith2(joinSemilatticeVal(JoinSemilattice0).join)(v4._1)(v5._1),
        $Tuple($Expr("LetRec", $RecDefs(join1(v3._2._2)(v5._2._2), v5._2._1), v3._2._1), join1(v3._2._2)(v5._2._2))
      );
    };
    if (v1.tag === "Var") {
      const $6 = v1._1;
      return $Tuple(
        (() => {
          const $7 = {};
          $7[$6] = v;
          return $7;
        })(),
        $Tuple($Expr("Var", $6), bot1)
      );
    }
    if (v1.tag === "Op") {
      const $6 = v1._1;
      return $Tuple(
        (() => {
          const $7 = {};
          $7[$6] = v;
          return $7;
        })(),
        $Tuple($Expr("Op", $6), bot1)
      );
    }
    if (v1.tag === "Const") {
      if (v._2.tag === "Str") {
        return $Tuple(empty, $Tuple($Expr("Str", v._1, v._2._1), v._1));
      }
      if (v._2.tag === "Int") {
        return $Tuple(empty, $Tuple($Expr("Int", v._1, v._2._1), v._1));
      }
      if (v._2.tag === "Float") {
        return $Tuple(empty, $Tuple($Expr("Float", v._1, v._2._1), v._1));
      }
      if (v._2.tag === "Fun" && v._2._1.tag === "Closure") {
        return $Tuple(v._2._1._1, $Tuple($Expr("Lambda", v._1, v._2._1._3), v._1));
      }
      return throwException(error("absurd"))();
    }
    if (v._2.tag === "Dictionary") {
      if (v1.tag === "Dictionary") {
        const s\u03B1vs$p = expandableDictDict(botOfUnit$x215Raw$x2152(BoundedJoinSemilattice0))((() => {
          const $6 = expandableValRawVal(BoundedJoinSemilattice0);
          return { expand: (v$1) => (v1$1) => $Tuple(v$1._1, $6.expand(v$1._2)(v1$1._2)) };
        })()).expand(v._2._1)(_fmapObject(v1._2, (v2) => $Tuple(void 0, v2)));
        const \u03B3e\u03B1s = listMap((v2) => evalBwd$p(dictAnn)($Val(
          $$get(showString)(mapDictString)(v2._1)(s\u03B1vs$p)._1,
          $BaseVal("Str", v2._1)
        ))(v2._2._1))(v1._1);
        const \u03B3e\u03B1s$p = listMap((v2) => evalBwd$p(dictAnn)($$get(showString)(mapDictString)(v2._1)(s\u03B1vs$p)._2)(v2._2._2))(v1._1);
        return $Tuple(
          foldableList.foldr((v$1) => (v1$1) => unionWith2(joinSemilatticeVal(JoinSemilattice0).join)(v$1)(v1$1))(empty)(foldableList.foldr(Cons)(listMap(fst)(\u03B3e\u03B1s$p))(listMap(fst)(\u03B3e\u03B1s))),
          $Tuple(
            $Expr(
              "Dictionary",
              v._1,
              listMap(fromTuple)(zipWith(Tuple)(listMap((x) => x._2._1)(\u03B3e\u03B1s))(listMap((x) => x._2._1)(\u03B3e\u03B1s$p)))
            ),
            foldableList.foldr(join1)(v._1)(foldableList.foldr(Cons)(listMap((x) => x._2._2)(\u03B3e\u03B1s$p))(listMap((x) => x._2._2)(\u03B3e\u03B1s)))
          )
        );
      }
      if (v1.tag === "Project") {
        return $0(v1._1, v, v1._2);
      }
      if (v1.tag === "DProject") {
        if (v1._2.tag === "Nothing") {
          return $1(v1._1, v, v1._3);
        }
        if (v1._2.tag === "Just") {
          return $2(v1._1, v1._2._1, v, v1._3);
        }
        return throwException(error("absurd"))();
      }
      if (v1.tag === "App") {
        return $3(v1._1, v1._2, v1._3, v);
      }
      if (v1.tag === "Let") {
        return $4(v1._1._2, v1._2, v, v1._1._1);
      }
      if (v1.tag === "LetRec") {
        return $5(v1._2, v, v1._1._2);
      }
      return throwException(error("absurd"))();
    }
    if (v._2.tag === "Constr") {
      if (v1.tag === "Constr") {
        const v2 = foldableList.foldr((v22) => (v3) => {
          const v4 = evalBwd$p(dictAnn)(v22._1)(v22._2);
          return $Tuple(
            unionWith2(joinSemilatticeVal(JoinSemilattice0).join)(v3._1)(v4._1),
            $Tuple($List("Cons", v4._2._1, v3._2._1), join1(v3._2._2)(v4._2._2))
          );
        })($Tuple(empty, $Tuple(Nil, v._1)))(zipWith(Tuple)(v._2._2)(v1._2));
        return $Tuple(v2._1, $Tuple($Expr("Constr", v._1, v1._1, v2._2._1), v2._2._2));
      }
      if (v1.tag === "Project") {
        return $0(v1._1, v, v1._2);
      }
      if (v1.tag === "DProject") {
        if (v1._2.tag === "Nothing") {
          return $1(v1._1, v, v1._3);
        }
        if (v1._2.tag === "Just") {
          return $2(v1._1, v1._2._1, v, v1._3);
        }
        return throwException(error("absurd"))();
      }
      if (v1.tag === "App") {
        return $3(v1._1, v1._2, v1._3, v);
      }
      if (v1.tag === "Let") {
        return $4(v1._1._2, v1._2, v, v1._1._1);
      }
      if (v1.tag === "LetRec") {
        return $5(v1._2, v, v1._1._2);
      }
      return throwException(error("absurd"))();
    }
    if (v._2.tag === "Matrix" && v1.tag === "Matrix") {
      const $6 = v1._3._1;
      const $7 = v1._3._2;
      const $8 = v1._1;
      const $9 = v._2._1._1;
      const $10 = v1._2._1;
      const $11 = v1._2._2;
      const $12 = nonEmptyListNonEmptyList.nonEmpty(bindList.bind(applyList.apply(listMap(Tuple)(range(1)($6)))(range(1)($7)))(applicativeList.pure));
      const v3 = foldl1((v42) => {
        const $13 = v42._2._1;
        const $14 = v42._2._2._1;
        const $15 = v42._2._2._2._1;
        const $16 = v42._2._2._2._2;
        const $17 = v42._1;
        return (v5) => $Tuple(
          unionWith2(joinSemilatticeVal(JoinSemilattice0).join)($17)(v5._1),
          $Tuple(
            joinSemilatticeExpr(JoinSemilattice0).join($13)(v5._2._1),
            $Tuple(join1($14)(v5._2._2._1), $Tuple(join1($15)(v5._2._2._2._1), join1($16)(v5._2._2._2._2)))
          )
        );
      })((() => {
        const $13 = (v32) => {
          const v42 = evalBwd$p(dictAnn)(definitely("index within bounds")(index(definitely("index within bounds")(index($9)(v32._1 - 1 | 0)))(v32._2 - 1 | 0)))(definitely("index within bounds")(index(definitely("index within bounds")(index($8)(v32._1 - 1 | 0)))(v32._2 - 1 | 0)));
          const v5 = append_inv(ordString)(mapEnvStringVal)(union8($$$Map(
            "Two",
            Leaf2,
            $10,
            void 0,
            Leaf2
          ))($$$Map("Two", Leaf2, $11, void 0, Leaf2)))(v42._1);
          const \u03B30 = unionWith2((v$1) => identity14)(disjointUnion4((() => {
            const $133 = {};
            $133[$10] = $Val(bot1, $BaseVal("Int", $6));
            return $133;
          })())((() => {
            const $133 = {};
            $133[$11] = $Val(bot1, $BaseVal("Int", $7));
            return $133;
          })()))(v5._2);
          const $132 = $$get(showString)(mapEnvStringVal)($10)(\u03B30);
          const $14 = $$get(showString)(mapEnvStringVal)($11)(\u03B30);
          if ($132._2.tag === "Int" && $14._2.tag === "Int") {
            return $Tuple(v5._1, $Tuple(v42._2._1, $Tuple(v42._2._2, $Tuple($132._1, $14._1))));
          }
          fail();
        };
        return $NonEmpty($13($12._1), listMap(($14) => $13($14))($12._2));
      })());
      const v4 = evalBwd$p(dictAnn)($Val(
        bot1,
        $BaseVal(
          "Constr",
          "Pair",
          $List(
            "Cons",
            $Val(join1(v3._2._2._2._1)(v._2._1._2._1._2), $BaseVal("Int", $6)),
            $List("Cons", $Val(join1(v3._2._2._2._2)(v._2._1._2._2._2), $BaseVal("Int", $7)), Nil)
          )
        )
      ))(v1._4);
      return $Tuple(
        unionWith2(joinSemilatticeVal(JoinSemilattice0).join)(v3._1)(v4._1),
        $Tuple($Expr("Matrix", v._1, v3._2._1, $Tuple($10, $11), v4._2._1), join1(join1(v._1)(v3._2._2._1))(v4._2._2))
      );
    }
    if (v1.tag === "Project") {
      return $0(v1._1, v, v1._2);
    }
    if (v1.tag === "DProject") {
      if (v1._2.tag === "Nothing") {
        return $1(v1._1, v, v1._3);
      }
      if (v1._2.tag === "Just") {
        return $2(v1._1, v1._2._1, v, v1._3);
      }
      return throwException(error("absurd"))();
    }
    if (v1.tag === "App") {
      return $3(v1._1, v1._2, v1._3, v);
    }
    if (v1.tag === "Let") {
      return $4(v1._1._2, v1._2, v, v1._1._1);
    }
    if (v1.tag === "LetRec") {
      return $5(v1._2, v, v1._1._2);
    }
    return throwException(error("absurd"))();
  };
};
var applyBwd = (dictAnn) => {
  const closeDefsBwd1 = closeDefsBwd(dictAnn);
  const matchBwd1 = matchBwd(dictAnn);
  const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
  const JoinSemilattice0 = BoundedJoinSemilattice0.JoinSemilattice0();
  const bot1 = BoundedJoinSemilattice0.bot;
  return (v) => {
    if (v._1.tag === "AppClosure") {
      const v2 = evalBwd$p(dictAnn)(v._2)(v._1._3);
      const v3 = append_inv(ordString)(mapEnvStringVal)(bVMatch.bv(v._1._2))(v2._1);
      const v4 = append_inv(ordString)(mapEnvStringVal)(v._1._1)(v3._1);
      const v5 = closeDefsBwd1(v4._2);
      const v6 = matchBwd1(v3._2)($Cont("ContExpr", v2._2._1))(v2._2._2)(v._1._2);
      return $Tuple(
        $Val(
          JoinSemilattice0.join(v2._2._2)(v5._2._2),
          $BaseVal("Fun", $Fun("Closure", unionWith2(joinSemilatticeVal(JoinSemilattice0).join)(v4._1)(v5._1), v5._2._1, v6._2))
        ),
        v6._1
      );
    }
    if (v._1.tag === "AppForeign") {
      if (v._1._2._2._1._1.arity > v._1._1) {
        if (v._2._2.tag === "Fun" && v._2._2._1.tag === "Foreign") {
          const $02 = definitely("absurd")(unsnoc(v._2._2._1._2));
          return $Tuple(
            $Val(v._2._1, $BaseVal("Fun", $Fun("Foreign", $Tuple(v._1._2._1, $ForeignOp$p(v._1._2._2._1._1)), $02.init))),
            $02.last
          );
        }
        fail();
      }
      const $0 = definitely("absurd")(unsnoc(v._1._2._2._1._1.op_bwd(dictAnn)($Tuple(definitely("absurd")(v._1._2._2._2), v._2))));
      return $Tuple($Val(bot1, $BaseVal("Fun", $Fun("Foreign", $Tuple(v._1._2._1, $ForeignOp$p(v._1._2._2._1._1)), $0.init))), $0.last);
    }
    if (v._1.tag === "AppConstr") {
      if (v._2._2.tag === "Constr") {
        if (v._2._2._1 === v._1._1) {
          const v33 = definitely("absurd")(unsnoc(v._2._2._2));
          return $Tuple($Val(v._2._1, $BaseVal("Fun", $Fun("PartialConstr", v._1._1, v33.init))), v33.last);
        }
        const v32 = definitely("absurd")(unsnoc(throwException(error("absurd"))()._1));
        return $Tuple(
          $Val(throwException(error("absurd"))()._2, $BaseVal("Fun", $Fun("PartialConstr", v._1._1, v32.init))),
          v32.last
        );
      }
      if (v._2._2.tag === "Fun" && v._2._2._1.tag === "PartialConstr" && v._2._2._1._1 === v._1._1) {
        const v32 = definitely("absurd")(unsnoc(v._2._2._1._2));
        return $Tuple($Val(v._2._1, $BaseVal("Fun", $Fun("PartialConstr", v._1._1, v32.init))), v32.last);
      }
      const v3 = definitely("absurd")(unsnoc(throwException(error("absurd"))()._1));
      return $Tuple(
        $Val(throwException(error("absurd"))()._2, $BaseVal("Fun", $Fun("PartialConstr", v._1._1, v3.init))),
        v3.last
      );
    }
    fail();
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

// output-es/Primitive.Defs/index.js
var erase = /* @__PURE__ */ (() => functorVal.map((v) => {
}))();
var unzip6 = /* @__PURE__ */ unzip3(functorDict);
var foldM4 = (dictMonad) => (f) => (b0) => foldableDict.foldl((b) => (a) => dictMonad.Bind1().bind(b)((a$1) => f(a$1)(a)))(dictMonad.Applicative0().pure(b0));
var foldWithIndexM = (dictMonad) => (f) => (a0) => foldableWithIndexStringDi.foldlWithIndex((i) => (ma) => (b) => dictMonad.Bind1().bind(ma)((() => {
  const $0 = f(i);
  return (a) => $0(a)(b);
})()))(dictMonad.Applicative0().pure(a0));
var disjointUnion5 = /* @__PURE__ */ disjointUnion(mapDictString);
var unary2 = /* @__PURE__ */ unary(boundedJoinSemilatticeUni);
var binary2 = /* @__PURE__ */ binary(boundedJoinSemilatticeUni);
var binaryZero2 = /* @__PURE__ */ binaryZero(boundedJoinSemilatticeUni);
var binaryZero1 = /* @__PURE__ */ (() => binaryZero2({ isZero: fanin2(isZeroInt.isZero)(isZeroNumber.isZero) }))();
var binaryZero22 = /* @__PURE__ */ binaryZero2(isZeroInt);
var pow3 = /* @__PURE__ */ union6(asNumberIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber)((x) => (y) => pow(toNumber(x))(toNumber(y)))(pow);
var numToStr = (v2) => {
  if (v2.tag === "Left") {
    return showIntImpl(v2._1);
  }
  if (v2.tag === "Right") {
    return showNumberImpl(v2._1);
  }
  fail();
};
var notEquals = /* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((x) => (y) => x !== y)(/* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((x) => (y) => x !== y)((x) => (y) => x !== y));
var matrixUpdate = /* @__PURE__ */ $Tuple(
  "matrixUpdate",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 3,
    "op'": (dictMonadWithGraphAlloc) => {
      const $$new = dictMonadWithGraphAlloc.new(typeNameBaseVal);
      return (dictMonadError) => {
        const $$throw2 = $$throw(dictMonadError.MonadThrow0());
        return (v) => {
          if (v.tag === "Cons" && v._1._2.tag === "Matrix" && v._2.tag === "Cons" && v._2._1._2.tag === "Constr" && v._2._1._2._2.tag === "Cons" && v._2._1._2._2._1._2.tag === "Int" && v._2._1._2._2._2.tag === "Cons" && v._2._1._2._2._2._1._2.tag === "Int" && v._2._1._2._2._2._2.tag === "Nil" && v._2._2.tag === "Cons" && v._2._2._2.tag === "Nil" && v._2._1._2._1 === "Pair") {
            const $0 = v._2._2._1;
            return $$new(Val)($$$Map("Two", Leaf2, v._1._1, void 0, Leaf2))($BaseVal(
              "Matrix",
              matrixPut(v._2._1._2._2._1._2._1)(v._2._1._2._2._2._1._2._1)((v$1) => $0)(v._1._2._1)
            ));
          }
          return $$throw2("Matrix, pair of integers and value expected");
        };
      };
    },
    op: (dictAnn) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (v) => {
        if (v.tag === "Cons" && v._1._2.tag === "Matrix" && v._2.tag === "Cons" && v._2._1._2.tag === "Constr" && v._2._1._2._2.tag === "Cons" && v._2._1._2._2._1._2.tag === "Int" && v._2._1._2._2._2.tag === "Cons" && v._2._1._2._2._2._1._2.tag === "Int" && v._2._1._2._2._2._2.tag === "Nil" && v._2._2.tag === "Cons" && v._2._2._2.tag === "Nil" && v._2._1._2._1 === "Pair") {
          const $0 = v._2._1._2._2._1._2._1;
          const $1 = v._2._1._2._2._2._1._2._1;
          const $2 = v._2._2._1;
          return MonadThrow0.Monad0().Applicative0().pure($Tuple(
            $Tuple($Tuple($0, $1), $Val(void 0, functorBaseVal.map((v$1) => {
            })(matrixGet($0)($1)(v._1._2._1)._2))),
            $Val(v._1._1, $BaseVal("Matrix", matrixPut($0)($1)((v$1) => $2)(v._1._2._1)))
          ));
        }
        return MonadThrow0.throwError(error("Matrix, pair of integers and value expected"));
      };
    },
    op_bwd: (dictAnn) => {
      const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
      const bot = BoundedJoinSemilattice0.bot;
      return (v) => {
        if (v._2._2.tag === "Matrix") {
          return $List(
            "Cons",
            $Val(
              v._2._1,
              $BaseVal(
                "Matrix",
                matrixPut(v._1._1._1)(v._1._1._2)((() => {
                  const $0 = BoundedJoinSemilattice0.bot;
                  const $1 = $Val($0, functorBaseVal.map((v$1) => $0)(v._1._2._2));
                  return (v$1) => $1;
                })())(v._2._2._1)
              )
            ),
            $List(
              "Cons",
              $Val(
                bot,
                $BaseVal(
                  "Constr",
                  "Pair",
                  $List(
                    "Cons",
                    $Val(bot, $BaseVal("Int", v._1._1._1)),
                    $List("Cons", $Val(bot, $BaseVal("Int", v._1._1._2)), Nil)
                  )
                )
              ),
              $List("Cons", matrixGet(v._1._1._1)(v._1._1._2)(v._2._2._1), Nil)
            )
          );
        }
        fail();
      };
    }
  })
);
var matrixLookup = /* @__PURE__ */ $Tuple(
  "!",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    "op'": (dictMonadWithGraphAlloc) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (v) => {
        if (v.tag === "Cons" && v._1._2.tag === "Matrix" && v._2.tag === "Cons" && v._2._1._2.tag === "Constr" && v._2._1._2._2.tag === "Cons" && v._2._1._2._2._1._2.tag === "Int" && v._2._1._2._2._2.tag === "Cons" && v._2._1._2._2._2._1._2.tag === "Int" && v._2._1._2._2._2._2.tag === "Nil" && v._2._2.tag === "Nil" && v._2._1._2._1 === "Pair") {
          return MonadThrow0.Monad0().Applicative0().pure(matrixGet(v._2._1._2._2._1._2._1)(v._2._1._2._2._2._1._2._1)(v._1._2._1));
        }
        return MonadThrow0.throwError(error("Matrix and pair of integers expected"));
      };
    },
    op: (dictAnn) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (v) => {
        if (v.tag === "Cons" && v._1._2.tag === "Matrix" && v._2.tag === "Cons" && v._2._1._2.tag === "Constr" && v._2._1._2._2.tag === "Cons" && v._2._1._2._2._1._2.tag === "Int" && v._2._1._2._2._2.tag === "Cons" && v._2._1._2._2._2._1._2.tag === "Int" && v._2._1._2._2._2._2.tag === "Nil" && v._2._2.tag === "Nil" && v._2._1._2._1 === "Pair") {
          const $0 = v._2._1._2._2._1._2._1;
          const $1 = v._2._1._2._2._2._1._2._1;
          return MonadThrow0.Monad0().Applicative0().pure($Tuple(
            $Tuple(functorMatrixRep.map((v$1) => {
            })(v._1._2._1), $Tuple($0, $1)),
            matrixGet($0)($1)(v._1._2._1)
          ));
        }
        return MonadThrow0.throwError(error("Matrix and pair of integers expected"));
      };
    },
    op_bwd: (dictAnn) => {
      const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
      const bot = BoundedJoinSemilattice0.bot;
      return (v) => {
        const $0 = v._2;
        return $List(
          "Cons",
          $Val(
            bot,
            $BaseVal(
              "Matrix",
              matrixPut(v._1._2._1)(v._1._2._2)((v$1) => $0)(functorMatrixRep.map((() => {
                const $1 = BoundedJoinSemilattice0.bot;
                return (v$1) => $1;
              })())(v._1._1))
            )
          ),
          $List(
            "Cons",
            $Val(
              bot,
              $BaseVal(
                "Constr",
                "Pair",
                $List(
                  "Cons",
                  $Val(bot, $BaseVal("Int", v._1._2._1)),
                  $List("Cons", $Val(bot, $BaseVal("Int", v._1._2._2)), Nil)
                )
              )
            ),
            Nil
          )
        );
      };
    }
  })
);
var log3 = (v2) => {
  if (v2.tag === "Left") {
    return log(toNumber(v2._1));
  }
  if (v2.tag === "Right") {
    return log(v2._1);
  }
  fail();
};
var lessThanEquals = /* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((a1) => (a2) => a1 <= a2)(/* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((a1) => (a2) => a1 <= a2)((a1) => (a2) => a1 <= a2));
var lessThan = /* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((a1) => (a2) => a1 < a2)(/* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((a1) => (a2) => a1 < a2)((a1) => (a2) => a1 < a2));
var greaterThanEquals = /* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((a1) => (a2) => a1 >= a2)(/* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((a1) => (a2) => a1 >= a2)((a1) => (a2) => a1 >= a2));
var greaterThan = /* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((a1) => (a2) => a1 > a2)(/* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((a1) => (a2) => a1 > a2)((a1) => (a2) => a1 > a2));
var extern = (dictBoundedJoinSemilattice) => {
  const bot = dictBoundedJoinSemilattice.bot;
  return (v) => $Tuple(v._1, $Val(bot, $BaseVal("Fun", $Fun("Foreign", $Tuple(v._1, v._2), Nil))));
};
var extern1 = /* @__PURE__ */ extern(boundedJoinSemilatticeUni);
var error_ = /* @__PURE__ */ $Tuple(
  "error",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 1,
    "op'": (dictMonadWithGraphAlloc) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (v) => {
        if (v.tag === "Cons" && v._1._2.tag === "Str" && v._2.tag === "Nil") {
          return MonadThrow0.Monad0().Applicative0().pure(throwException(error(v._1._2._1))());
        }
        return MonadThrow0.throwError(error("String expected"));
      };
    },
    op: (dictAnn) => (dictMonadError) => {
      const $$throw2 = $$throw(dictMonadError.MonadThrow0());
      return (v) => {
        if (v.tag === "Cons" && v._1._2.tag === "Str" && v._2.tag === "Nil") {
          return throwException(error(v._1._2._1))();
        }
        return $$throw2("String expected");
      };
    },
    op_bwd: (dictAnn) => (v) => throwException(error("unimplemented"))()
  })
);
var divide = /* @__PURE__ */ union6(asNumberIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber)((x) => (y) => toNumber(x) / toNumber(y))(numDiv);
var dims = /* @__PURE__ */ $Tuple(
  "dims",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 1,
    "op'": (dictMonadWithGraphAlloc) => {
      const $$new = dictMonadWithGraphAlloc.new(typeNameBaseVal);
      return (dictMonadError) => {
        const MonadThrow0 = dictMonadError.MonadThrow0();
        const $0 = MonadThrow0.Monad0().Bind1();
        return (v) => {
          if (v.tag === "Cons" && v._1._2.tag === "Matrix" && v._2.tag === "Nil") {
            const $1 = v._1._2._1._2._2._1;
            const $2 = v._1._1;
            const $3 = v._1._2._1._2._2._2;
            return $0.bind($$new(Val)($$$Map("Two", Leaf2, v._1._2._1._2._1._2, void 0, Leaf2))($BaseVal(
              "Int",
              v._1._2._1._2._1._1
            )))((v1) => $0.bind($$new(Val)($$$Map("Two", Leaf2, $3, void 0, Leaf2))($BaseVal("Int", $1)))((v2) => $$new(Val)($$$Map(
              "Two",
              Leaf2,
              $2,
              void 0,
              Leaf2
            ))($BaseVal("Constr", "Pair", $List("Cons", v1, $List("Cons", v2, Nil))))));
          }
          return MonadThrow0.throwError(error("Matrix expected"));
        };
      };
    },
    op: (dictAnn) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (v) => {
        if (v.tag === "Cons" && v._1._2.tag === "Matrix" && v._2.tag === "Nil") {
          return MonadThrow0.Monad0().Applicative0().pure($Tuple(
            arrayMap(arrayMap(erase))(v._1._2._1._1),
            $Val(
              v._1._1,
              $BaseVal(
                "Constr",
                "Pair",
                $List(
                  "Cons",
                  $Val(v._1._2._1._2._1._2, $BaseVal("Int", v._1._2._1._2._1._1)),
                  $List("Cons", $Val(v._1._2._1._2._2._2, $BaseVal("Int", v._1._2._1._2._2._1)), Nil)
                )
              )
            )
          ));
        }
        return MonadThrow0.throwError(error("Matrix expected"));
      };
    },
    op_bwd: (dictAnn) => {
      const botOf = functorVal.map((() => {
        const $0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0().bot;
        return (v) => $0;
      })());
      return (v) => {
        if (v._2._2.tag === "Constr" && v._2._2._2.tag === "Cons" && v._2._2._2._1._2.tag === "Int" && v._2._2._2._2.tag === "Cons" && v._2._2._2._2._1._2.tag === "Int" && v._2._2._2._2._2.tag === "Nil" && v._2._2._1 === "Pair") {
          return $List(
            "Cons",
            $Val(
              v._2._1,
              $BaseVal(
                "Matrix",
                $Tuple(
                  arrayMap(arrayMap(botOf))(v._1),
                  $Tuple($Tuple(v._2._2._2._1._2._1, v._2._2._2._1._1), $Tuple(v._2._2._2._2._1._2._1, v._2._2._2._2._1._1))
                )
              )
            ),
            Nil
          );
        }
        fail();
      };
    }
  })
);
var dict_map = /* @__PURE__ */ $Tuple(
  "dict_map",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    "op'": (dictMonadWithGraphAlloc) => {
      const apply4 = apply2(dictMonadWithGraphAlloc);
      const $$new = dictMonadWithGraphAlloc.new(typeNameBaseVal);
      return (dictMonadError) => {
        const MonadThrow0 = dictMonadError.MonadThrow0();
        const Monad0 = MonadThrow0.Monad0();
        const Bind1 = Monad0.Bind1();
        const traverse1 = traversableDict.traverse(Monad0.Applicative0());
        return (v) => {
          if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._1._2.tag === "Dictionary" && v._2._2.tag === "Nil") {
            const $0 = v._1;
            const $1 = v._2._1._1;
            return Bind1.bind(traverse1((v2) => {
              const $2 = v2._1;
              return Bind1.Apply0().Functor0().map((v3) => $Tuple($2, v3))(apply4($0)(v2._2));
            })(v._2._1._2._1))((d$p) => $$new(Val)($$$Map("Two", Leaf2, $1, void 0, Leaf2))($BaseVal(
              "Dictionary",
              d$p
            )));
          }
          return MonadThrow0.throwError(error("Function and dictionary expected"));
        };
      };
    },
    op: (dictAnn) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      const Monad0 = MonadThrow0.Monad0();
      const Bind1 = Monad0.Bind1();
      const $0 = Bind1.Apply0().Functor0();
      const Applicative0 = Monad0.Applicative0();
      const traverse1 = traversableDict.traverse(Applicative0);
      const apply4 = apply3(dictMonadError)(dictAnn);
      return (v) => {
        if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._1._2.tag === "Dictionary" && v._2._2.tag === "Nil") {
          const $1 = v._1;
          const $2 = v._2._1._1;
          return Bind1.bind($0.map(unzip6)(traverse1((v2) => {
            const $3 = v2._1;
            return $0.map((m) => $Tuple(m._1, $Tuple($3, m._2)))(apply4($Tuple($1, v2._2)));
          })(v._2._1._2._1)))((v2) => Applicative0.pure($Tuple(
            $Tuple($Val(void 0, functorBaseVal.map((v$1) => {
            })($1._2)), v2._1),
            $Val($2, $BaseVal("Dictionary", v2._2))
          )));
        }
        return MonadThrow0.throwError(error("Function and dictionary expected"));
      };
    },
    op_bwd: (dictAnn) => {
      const applyBwd2 = applyBwd(dictAnn);
      const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
      return (v) => {
        if (v._2._2.tag === "Dictionary") {
          const v2 = unzip6(intersectionWith_Object((t) => (v3) => {
            const $0 = applyBwd2($Tuple(t, v3._2));
            return $Tuple($0._1, $Tuple(v3._1, $0._2));
          })(v._1._2)(v._2._2._1));
          return $List(
            "Cons",
            fold((z) => (v$1) => joinSemilatticeVal(BoundedJoinSemilattice0.JoinSemilattice0()).join(z))((() => {
              const $0 = BoundedJoinSemilattice0.bot;
              return $Val($0, functorBaseVal.map((v$1) => $0)(v._1._1._2));
            })())(v2._1),
            $List("Cons", $Val(v._2._1, $BaseVal("Dictionary", v2._2)), Nil)
          );
        }
        fail();
      };
    }
  })
);
var dict_intersectionWith = /* @__PURE__ */ $Tuple(
  "dict_intersectionWith",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 3,
    "op'": (dictMonadWithGraphAlloc) => {
      const apply4 = apply2(dictMonadWithGraphAlloc);
      const $$new = dictMonadWithGraphAlloc.new(typeNameBaseVal);
      return (dictMonadError) => {
        const MonadThrow0 = dictMonadError.MonadThrow0();
        const Monad0 = MonadThrow0.Monad0();
        const Bind1 = Monad0.Bind1();
        const $0 = Bind1.Apply0().Functor0();
        const Applicative0 = Monad0.Applicative0();
        return (v) => {
          if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._1._2.tag === "Dictionary" && v._2._2.tag === "Cons" && v._2._2._1._2.tag === "Dictionary" && v._2._2._2.tag === "Nil") {
            const $1 = v._1;
            const $2 = v._2._1._1;
            const $3 = v._2._2._1._1;
            return Bind1.bind($0.map(Dictionary2)($0.map(DictRep)(traversableDict.traverse(Applicative0)(identity15)(intersectionWith_Object((v2) => (v3) => {
              const $4 = v3._2;
              const $5 = v2._1;
              const $6 = v3._1;
              return Bind1.bind(Bind1.bind(apply4($1)(v2._2))((a) => apply4(a)($4)))((v4) => Bind1.bind($$new(Val)(insert(ordVertex)($6)()($$$Map(
                "Two",
                Leaf2,
                $5,
                void 0,
                Leaf2
              )))(v4._2))((v5) => $0.map(Tuple(v5._1))(Applicative0.pure(v4))));
            })(v._2._1._2._1)(v._2._2._1._2._1)))))((v$p) => $$new(Val)(insert(ordVertex)($3)()($$$Map(
              "Two",
              Leaf2,
              $2,
              void 0,
              Leaf2
            )))(v$p));
          }
          return MonadThrow0.throwError(error("Function and two dictionaries expected"));
        };
      };
    },
    op: (dictAnn) => {
      const $0 = dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0();
      return (dictMonadError) => {
        const MonadThrow0 = dictMonadError.MonadThrow0();
        const Monad0 = MonadThrow0.Monad0();
        const Bind1 = Monad0.Bind1();
        const Applicative0 = Monad0.Applicative0();
        const apply23 = apply22(dictMonadError)(dictAnn);
        return (v) => {
          if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._1._2.tag === "Dictionary" && v._2._2.tag === "Cons" && v._2._2._1._2.tag === "Dictionary" && v._2._2._2.tag === "Nil") {
            const $1 = v._1;
            const $2 = v._2._1._1;
            const $3 = v._2._2._1._1;
            return Bind1.bind(traversableDict.traverse(Applicative0)(identity15)(intersectionWith_Object((v2) => {
              const $4 = v2._2;
              const $5 = v2._1;
              return (v3) => {
                const $6 = v3._1;
                return Bind1.Apply0().Functor0().map((v4) => $Tuple($0.meet($5)($6), v4))(apply23($Tuple($1, $Tuple($4, v3._2))));
              };
            })(v._2._1._2._1)(v._2._2._1._2._1)))((d$p$p) => Applicative0.pure($Tuple(
              $Tuple($Val(void 0, functorBaseVal.map((v$1) => {
              })($1._2)), _fmapObject(d$p$p, (x) => x._2._1)),
              $Val($0.meet($2)($3), $BaseVal("Dictionary", _fmapObject(d$p$p, (m) => $Tuple(m._1, m._2._2))))
            )));
          }
          return MonadThrow0.throwError(error("Function and two dictionaries expected"));
        };
      };
    },
    op_bwd: (dictAnn) => {
      const apply2Bwd2 = apply2Bwd(dictAnn);
      const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
      return (v) => {
        if (v._2._2.tag === "Dictionary") {
          const d$p = intersectionWith_Object((tt) => (v2) => $Tuple(v2._1, apply2Bwd2($Tuple(tt, v2._2))))(v._1._2)(v._2._2._1);
          return $List(
            "Cons",
            fold((z) => (v$1) => joinSemilatticeVal(BoundedJoinSemilattice0.JoinSemilattice0()).join(z))((() => {
              const $0 = BoundedJoinSemilattice0.bot;
              return $Val($0, functorBaseVal.map((v$1) => $0)(v._1._1._2));
            })())(_fmapObject(d$p, (x) => x._2._1)),
            $List(
              "Cons",
              $Val(v._2._1, $BaseVal("Dictionary", _fmapObject(d$p, (m) => $Tuple(m._1, m._2._2._1)))),
              $List(
                "Cons",
                $Val(v._2._1, $BaseVal("Dictionary", _fmapObject(d$p, (m) => $Tuple(m._1, m._2._2._2)))),
                Nil
              )
            )
          );
        }
        fail();
      };
    }
  })
);
var dict_get = /* @__PURE__ */ $Tuple(
  "dict_get",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    "op'": (dictMonadWithGraphAlloc) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (v) => {
        if (v.tag === "Cons" && v._1._2.tag === "Str" && v._2.tag === "Cons" && v._2._1._2.tag === "Dictionary" && v._2._2.tag === "Nil") {
          return orElse(MonadThrow0)('Key "' + v._1._2._1 + '" not found')((() => {
            const $0 = _lookup(Nothing, Just, v._1._2._1, v._2._1._2._1);
            if ($0.tag === "Just") {
              return $Maybe("Just", $0._1._2);
            }
            return Nothing;
          })());
        }
        return MonadThrow0.throwError(error("String and dictionary expected"));
      };
    },
    op: (dictAnn) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (v) => {
        if (v.tag === "Cons" && v._1._2.tag === "Str" && v._2.tag === "Cons" && v._2._1._2.tag === "Dictionary" && v._2._2.tag === "Nil") {
          const $0 = v._1._2._1;
          return MonadThrow0.Monad0().Bind1().Apply0().Functor0().map((v1) => $Tuple($0, v1))(orElse(MonadThrow0)('Key "' + $0 + '" not found')((() => {
            const $1 = _lookup(Nothing, Just, $0, v._2._1._2._1);
            if ($1.tag === "Just") {
              return $Maybe("Just", $1._1._2);
            }
            return Nothing;
          })()));
        }
        return MonadThrow0.throwError(error("String and dictionary expected"));
      };
    },
    op_bwd: (dictAnn) => {
      const bot = dictAnn.BoundedLattice1().BoundedJoinSemilattice0().bot;
      return (v) => {
        const $0 = v._1;
        return $List(
          "Cons",
          $Val(bot, $BaseVal("Str", $0)),
          $List(
            "Cons",
            $Val(
              bot,
              $BaseVal(
                "Dictionary",
                (() => {
                  const $1 = {};
                  $1[$0] = $Tuple(bot, v._2);
                  return $1;
                })()
              )
            ),
            Nil
          )
        );
      };
    }
  })
);
var dict_foldl = /* @__PURE__ */ $Tuple(
  "dict_foldl",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 3,
    "op'": (dictMonadWithGraphAlloc) => {
      const apply4 = apply2(dictMonadWithGraphAlloc);
      return (dictMonadError) => {
        const MonadThrow0 = dictMonadError.MonadThrow0();
        const Monad0 = MonadThrow0.Monad0();
        return (v) => {
          if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._2.tag === "Cons" && v._2._2._1._2.tag === "Dictionary" && v._2._2._2.tag === "Nil") {
            const $0 = v._1;
            return foldM4(Monad0)((u1) => (v2) => {
              const $1 = v2._2;
              return Monad0.Bind1().bind(apply4($0)(u1))((a) => apply4(a)($1));
            })(v._2._1)(v._2._2._1._2._1);
          }
          return MonadThrow0.throwError(error("Function, value and dictionary expected"));
        };
      };
    },
    op: (dictAnn) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      const Monad0 = MonadThrow0.Monad0();
      const Bind1 = Monad0.Bind1();
      const $0 = Bind1.Apply0().Functor0();
      const apply23 = apply22(dictMonadError)(dictAnn);
      return (v) => {
        if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._2.tag === "Cons" && v._2._2._1._2.tag === "Dictionary" && v._2._2._2.tag === "Nil") {
          const $1 = v._1;
          return Bind1.bind(foldWithIndexM(Monad0)((s) => (v2) => {
            const $2 = v2._1;
            const $3 = v2._2;
            return (v3) => $0.map((v$1) => $Tuple($List("Cons", $Tuple(s, v$1._1), $2), v$1._2))(apply23($Tuple(
              $1,
              $Tuple($3, v3._2)
            )));
          })($Tuple(Nil, v._2._1))(v._2._2._1._2._1))((v2) => Monad0.Applicative0().pure($Tuple(
            $Tuple($Val(void 0, functorBaseVal.map((v$1) => {
            })($1._2)), v2._1),
            v2._2
          )));
        }
        return MonadThrow0.throwError(error("Function, value and dictionary expected"));
      };
    },
    op_bwd: (dictAnn) => {
      const apply2Bwd2 = apply2Bwd(dictAnn);
      const BoundedJoinSemilattice0 = dictAnn.BoundedLattice1().BoundedJoinSemilattice0();
      const bot = BoundedJoinSemilattice0.bot;
      return (v) => {
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
                const $0 = v$1._1._1;
                const v5 = apply2Bwd2($Tuple(v$1._1._2, b._2._1));
                return $Tuple(
                  joinSemilatticeVal(BoundedJoinSemilattice0.JoinSemilattice0()).join(b._1)(v5._1),
                  $Tuple(
                    v5._2._1,
                    mutate(($1) => () => {
                      $1[$0] = $Tuple(bot, v5._2._2);
                      return $1;
                    })(b._2._2)
                  )
                );
              })();
              go$a1 = v$1._2;
              continue;
            }
            fail();
          }
          return go$r;
        };
        const v2 = go($Tuple(
          (() => {
            const $0 = BoundedJoinSemilattice0.bot;
            return $Val($0, functorBaseVal.map((v$1) => $0)(v._1._1._2));
          })(),
          $Tuple(v._2, empty)
        ))(v._1._2);
        return $List(
          "Cons",
          v2._1,
          $List("Cons", v2._2._1, $List("Cons", $Val(bot, $BaseVal("Dictionary", v2._2._2)), Nil))
        );
      };
    }
  })
);
var dict_disjointUnion = /* @__PURE__ */ $Tuple(
  "dict_disjointUnion",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    "op'": (dictMonadWithGraphAlloc) => {
      const $$new = dictMonadWithGraphAlloc.new(typeNameBaseVal);
      return (dictMonadError) => {
        const $$throw2 = $$throw(dictMonadError.MonadThrow0());
        return (v) => {
          if (v.tag === "Cons" && v._1._2.tag === "Dictionary" && v._2.tag === "Cons" && v._2._1._2.tag === "Dictionary" && v._2._2.tag === "Nil") {
            return $$new(Val)(insert(ordVertex)(v._2._1._1)()($$$Map(
              "Two",
              Leaf2,
              v._1._1,
              void 0,
              Leaf2
            )))($BaseVal("Dictionary", disjointUnion5(v._1._2._1)(v._2._1._2._1)));
          }
          return $$throw2("Dictionaries expected");
        };
      };
    },
    op: (dictAnn) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (v) => {
        if (v.tag === "Cons" && v._1._2.tag === "Dictionary" && v._2.tag === "Cons" && v._2._1._2.tag === "Dictionary" && v._2._2.tag === "Nil") {
          const $0 = v._2._1._2._1;
          return MonadThrow0.Monad0().Applicative0().pure($Tuple(
            $Tuple(_fmapObject(v._1._2._1, (v$1) => {
            }), _fmapObject($0, (v$1) => {
            })),
            $Val(dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0().meet(v._1._1)(v._2._1._1), $BaseVal("Dictionary", disjointUnion5(v._1._2._1)($0)))
          ));
        }
        return MonadThrow0.throwError(error("Dictionaries expected"));
      };
    },
    op_bwd: (dictAnn) => (v) => {
      if (v._2._2.tag === "Dictionary") {
        return $List(
          "Cons",
          $Val(v._2._1, $BaseVal("Dictionary", mapFObjectString.difference(v._2._2._1)(v._1._2))),
          $List("Cons", $Val(v._2._1, $BaseVal("Dictionary", mapFObjectString.difference(v._2._2._1)(v._1._1))), Nil)
        );
      }
      fail();
    }
  })
);
var dict_difference = /* @__PURE__ */ $Tuple(
  "dict_difference",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    "op'": (dictMonadWithGraphAlloc) => {
      const $$new = dictMonadWithGraphAlloc.new(typeNameBaseVal);
      return (dictMonadError) => {
        const $$throw2 = $$throw(dictMonadError.MonadThrow0());
        return (v) => {
          if (v.tag === "Cons" && v._1._2.tag === "Dictionary" && v._2.tag === "Cons" && v._2._1._2.tag === "Dictionary" && v._2._2.tag === "Nil") {
            return $$new(Val)(insert(ordVertex)(v._2._1._1)()($$$Map(
              "Two",
              Leaf2,
              v._1._1,
              void 0,
              Leaf2
            )))($BaseVal("Dictionary", mapFObjectString.difference(v._1._2._1)(v._2._1._2._1)));
          }
          return $$throw2("Dictionaries expected.");
        };
      };
    },
    op: (dictAnn) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (v) => {
        if (v.tag === "Cons" && v._1._2.tag === "Dictionary" && v._2.tag === "Cons" && v._2._1._2.tag === "Dictionary" && v._2._2.tag === "Nil") {
          return MonadThrow0.Monad0().Applicative0().pure($Tuple(
            void 0,
            $Val(
              dictAnn.BoundedLattice1().BoundedMeetSemilattice1().MeetSemilattice0().meet(v._1._1)(v._2._1._1),
              $BaseVal("Dictionary", mapFObjectString.difference(v._1._2._1)(v._2._1._2._1))
            )
          ));
        }
        return MonadThrow0.throwError(error("Dictionaries expected."));
      };
    },
    op_bwd: (dictAnn) => (v) => {
      if (v._2._2.tag === "Dictionary") {
        return $List(
          "Cons",
          $Val(v._2._1, $BaseVal("Dictionary", v._2._2._1)),
          $List("Cons", $Val(v._2._1, $BaseVal("Dictionary", empty)), Nil)
        );
      }
      fail();
    }
  })
);
var debugLog = /* @__PURE__ */ $Tuple(
  "debugLog",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 1,
    "op'": (dictMonadWithGraphAlloc) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (v) => {
        if (v.tag === "Cons" && v._2.tag === "Nil") {
          const $0 = v._1;
          return MonadThrow0.Monad0().Applicative0().pure(_trace($0, (v$1) => $0));
        }
        return MonadThrow0.throwError(error("Single value expected"));
      };
    },
    op: (dictAnn) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (v) => {
        if (v.tag === "Cons" && v._2.tag === "Nil") {
          const $0 = v._1;
          return MonadThrow0.Monad0().Applicative0().pure($Tuple(void 0, _trace($0, (v$1) => $0)));
        }
        return MonadThrow0.throwError(error("Single value expected"));
      };
    },
    op_bwd: (dictAnn) => (v) => throwException(error("unimplemented"))()
  })
);
var primitives = /* @__PURE__ */ fromFoldable2(foldableArray)([
  /* @__PURE__ */ $Tuple(
    ":",
    /* @__PURE__ */ $Val(void 0, /* @__PURE__ */ $BaseVal("Fun", /* @__PURE__ */ $Fun("PartialConstr", ":", Nil)))
  ),
  /* @__PURE__ */ unary2("ceiling")({ i: number, o: $$int, fwd: ceil2 }),
  /* @__PURE__ */ extern1(debugLog),
  /* @__PURE__ */ extern1(dims),
  /* @__PURE__ */ extern1(error_),
  /* @__PURE__ */ unary2("floor")({ i: number, o: $$int, fwd: floor2 }),
  /* @__PURE__ */ unary2("log")({ i: intOrNumber, o: number, fwd: log3 }),
  /* @__PURE__ */ unary2("numToStr")({ i: intOrNumber, o: string, fwd: numToStr }),
  /* @__PURE__ */ binary2("+")({
    i1: intOrNumber,
    i2: intOrNumber,
    o: intOrNumber,
    fwd: /* @__PURE__ */ union6(asIntIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber)(intAdd)(numAdd)
  }),
  /* @__PURE__ */ binary2("-")({
    i1: intOrNumber,
    i2: intOrNumber,
    o: intOrNumber,
    fwd: /* @__PURE__ */ union6(asIntIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber)(intSub)(numSub)
  }),
  /* @__PURE__ */ binaryZero1("*")({
    i: intOrNumber,
    o: intOrNumber,
    fwd: /* @__PURE__ */ union6(asIntIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber)(intMul)(numMul)
  }),
  /* @__PURE__ */ binaryZero1("**")({ i: intOrNumber, o: intOrNumber, fwd: pow3 }),
  /* @__PURE__ */ binaryZero1("/")({ i: intOrNumber, o: intOrNumber, fwd: divide }),
  /* @__PURE__ */ binary2("==")({
    i1: intOrNumberOrString,
    i2: intOrNumberOrString,
    o: $$boolean,
    fwd: /* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)(eqIntImpl)(/* @__PURE__ */ unionStr(asBooleanBoolean)(asNumberString)(eqNumberImpl)(eqStringImpl))
  }),
  /* @__PURE__ */ binary2("/=")({ i1: intOrNumberOrString, i2: intOrNumberOrString, o: $$boolean, fwd: notEquals }),
  /* @__PURE__ */ binary2("<")({ i1: intOrNumberOrString, i2: intOrNumberOrString, o: $$boolean, fwd: lessThan }),
  /* @__PURE__ */ binary2(">")({ i1: intOrNumberOrString, i2: intOrNumberOrString, o: $$boolean, fwd: greaterThan }),
  /* @__PURE__ */ binary2("<=")({ i1: intOrNumberOrString, i2: intOrNumberOrString, o: $$boolean, fwd: lessThanEquals }),
  /* @__PURE__ */ binary2(">=")({ i1: intOrNumberOrString, i2: intOrNumberOrString, o: $$boolean, fwd: greaterThanEquals }),
  /* @__PURE__ */ binary2("++")({ i1: string, i2: string, o: string, fwd: concatString }),
  /* @__PURE__ */ extern1(matrixLookup),
  /* @__PURE__ */ extern1(dict_difference),
  /* @__PURE__ */ extern1(dict_disjointUnion),
  /* @__PURE__ */ extern1(dict_foldl),
  /* @__PURE__ */ extern1(dict_get),
  /* @__PURE__ */ extern1(dict_intersectionWith),
  /* @__PURE__ */ extern1(dict_map),
  /* @__PURE__ */ extern1(matrixUpdate),
  /* @__PURE__ */ binaryZero22("div")({ i: $$int, o: $$int, fwd: intDiv2 }),
  /* @__PURE__ */ binaryZero22("mod")({ i: $$int, o: $$int, fwd: intMod }),
  /* @__PURE__ */ binaryZero22("quot")({ i: $$int, o: $$int, fwd: quot }),
  /* @__PURE__ */ binaryZero22("rem")({ i: $$int, o: $$int, fwd: rem })
]);

// output-es/ProgCxt/index.js
var union9 = /* @__PURE__ */ (() => setSet(ordDVertex).union)();
var unions4 = /* @__PURE__ */ (() => {
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
        go$a0 = unionWith(ordDVertex)($$const)(b)(v._1);
        go$a1 = v._2;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go(Leaf2);
})();
var identity25 = (x) => x;
var verticesProgCxtVertex = {
  vertices: (v) => union9(unions13(listMap(verticesValVertex.vertices)(mapObjectString.values(v.primitives))))(union9(unions4(listMap(verticesModuleVertex.vertices)(v.mods)))(unions4(listMap((x) => verticesExprVertex.vertices(x._2))(v.datasets))))
};
var functorProgCxt = {
  map: (f) => (m) => ({
    primitives: _fmapObject(m.primitives, functorVal.map(f)),
    mods: listMap(functorModule.map(f))(m.mods),
    datasets: listMap((m$1) => $Tuple(m$1._1, functorExpr.map(f)(m$1._2)))(m.datasets)
  })
};
var foldableProgCxt = {
  foldl: (f) => (z) => (m) => {
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
          go$a0 = foldableModule.foldl(f)(b)(v._1);
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      return go$r;
    };
    return fold((z$1) => (v) => foldableVal.foldl(f)(z$1))(go((() => {
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
            go$1$a0 = foldableExpr.foldl(f)(b)(v._1._2);
            go$1$a1 = v._2;
            continue;
          }
          fail();
        }
        return go$1$r;
      };
      return go$1(z)(m.datasets);
    })())(m.mods))(m.primitives);
  },
  foldr: (f) => (z) => (m) => foldableList.foldr((b) => (a) => foldableExpr.foldr(f)(a)(b._2))(foldableList.foldr((() => {
    const $0 = foldrDefault(foldableModule)(f);
    return (b) => (a) => $0(a)(b);
  })())(foldableEnv.foldr(f)(z)(m.primitives))(m.mods))(m.datasets),
  foldMap: (dictMonoid) => {
    const $0 = dictMonoid.Semigroup0();
    const foldMap5 = foldableList.foldMap(dictMonoid);
    const foldMap7 = foldableExpr.foldMap(dictMonoid);
    const foldMap9 = foldableEnv.foldMap(dictMonoid);
    return (f) => (m) => $0.append(foldMap5((() => {
      const $1 = foldMap7(f);
      return (v) => $1(v._2);
    })())(m.datasets))($0.append(foldMap5(foldableModule.foldMap(dictMonoid)(f))(m.mods))(foldMap9(f)(m.primitives)));
  }
};
var traversableProgCxt = {
  traverse: (dictApplicative) => {
    const Apply0 = dictApplicative.Apply0();
    const traverse5 = traversableList.traverse(dictApplicative);
    const traverse7 = traversableExpr.traverse(dictApplicative);
    const traverse8 = traversableModule.traverse(dictApplicative);
    const traverse9 = traversableEnv.traverse(dictApplicative);
    return (f) => (m) => Apply0.apply(Apply0.apply(Apply0.Functor0().map((v1) => (v2) => (v3) => ({ primitives: v3, mods: v2, datasets: v1 }))(traverse5(traversableTuple.traverse(dictApplicative)(traverse7(f)))(m.datasets)))(traverse5(traverse8(f))(m.mods)))(traverse9(f)(m.primitives));
  },
  sequence: (dictApplicative) => (v) => traversableProgCxt.traverse(dictApplicative)(identity25)(v),
  Functor0: () => functorProgCxt,
  Foldable1: () => foldableProgCxt
};

// output-es/Module/index.js
var boundedLattice2 = { BoundedJoinSemilattice0: () => boundedJoinSemilatticeUni, BoundedMeetSemilattice1: () => boundedMeetSemilatticeUni };
var concatM2 = (dictMonad) => foldrArray((() => {
  const $0 = dictMonad.Bind1();
  return (f) => (g) => (a) => $0.bind(f(a))(g);
})())(dictMonad.Applicative0().pure);
var parse = (dictMonadError) => {
  const $0 = dictMonadError.MonadThrow0();
  const $1 = $0.Monad0().Applicative0().pure;
  return (src) => {
    const $2 = runParserT1(src);
    return (x) => {
      const $3 = $2(x);
      if ($3.tag === "Left") {
        return $0.throwError(error(showParseError.show($3._1)));
      }
      if ($3.tag === "Right") {
        return $1($3._1);
      }
      fail();
    };
  };
};
var parseProgram = (loadFile2) => (folders) => (file) => (dictMonadAff) => (dictMonadError) => dictMonadAff.MonadEffect0().Monad0().Bind1().bind(loadFile2(folders)(file)(dictMonadAff)(dictMonadError))((() => {
  const $0 = parse(dictMonadError);
  return (a) => $0(a)(topLevel(expr_));
})());
var module_2 = (dictMonadAff) => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const Bind1 = Monad0.Bind1();
  const Applicative0 = Monad0.Applicative0();
  return (dictMonadError) => {
    const parse1 = parse(dictMonadError);
    const desugarModuleFwd = moduleFwd(dictMonadError)(boundedLattice2);
    return (loadFile2) => (folders) => (file) => (v) => {
      const $0 = v.mods;
      return Bind1.bind(Applicative0.pure())(() => Bind1.bind(loadFile2(folders)(file)(dictMonadAff)(dictMonadError))((src) => Bind1.bind(Bind1.bind(parse1(src)(module_))(desugarModuleFwd))((mod) => Applicative0.pure({
        primitives: v.primitives,
        mods: $List("Cons", mod, $0),
        datasets: v.datasets
      }))));
    };
  };
};
var initialConfig = (dictMonadError) => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  const Bind1 = Monad0.Bind1();
  const Applicative0 = Monad0.Applicative0();
  const $0 = monadAllocAllocT(Monad0);
  const fresh1 = $0.fresh;
  const alloc = traversableProgCxt.traverse($0.Monad0().Applicative0())((v) => fresh1);
  const $1 = bindStateT(Monad0);
  const runWithGraphT_spy2 = runWithGraphT_spy({
    Applicative0: () => applicativeStateT(Monad0),
    Bind1: () => bindStateT(Monad0)
  })(graphGraphImpl);
  const eval_progCxt2 = eval_progCxt(monadWithGraphAllocWithGr(dictMonadError));
  return (dictFV) => (e) => (progCxt) => Bind1.bind(Applicative0.pure())(() => Bind1.bind(runAllocT(Monad0)($1.bind(alloc(progCxt))((progCxt$p) => $1.bind(runWithGraphT_spy2(eval_progCxt2(progCxt$p))(verticesProgCxtVertex.vertices(progCxt$p)))((v) => applicativeStateT(Monad0).pure($Tuple(
    progCxt$p,
    (() => {
      const $2 = dictFV.fv(e);
      return filterWithKey2((x) => {
        const $3 = setSet(ordString).member(x)($2);
        return (v$1) => $3;
      })(v._2);
    })()
  )))))(0))((v) => Applicative0.pure({ n: v._1, progCxt: v._2._2._1, "\u03B3": v._2._2._2 })));
};
var prepConfig = (dictMonadAff) => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const $0 = Monad0.Bind1();
  return (dictMonadError) => {
    const desug1 = exprFwd(boundedLattice2)(dictMonadError)(joinSemilatticeUnit);
    const initialConfig1 = initialConfig(dictMonadError)(fVExpr);
    return (v) => (file) => (progCxt) => $0.bind(parseProgram(v.loadFile)(v.fluidSrcPaths)(file)(dictMonadAff)(dictMonadError))((s) => $0.bind(desug1(s))((e) => $0.bind(initialConfig1(e)(progCxt))((gconfig) => Monad0.Applicative0().pure({
      s,
      e,
      gconfig
    }))));
  };
};
var datasetAs = (dictMonadAff) => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const $0 = Monad0.Bind1();
  return (dictMonadError) => {
    const desug1 = exprFwd(boundedLattice2)(dictMonadError)(joinSemilatticeUnit);
    return (loadFile2) => (folders) => (v) => (v1) => {
      const $1 = v1.datasets;
      const $2 = v._1;
      return $0.bind($0.bind(parseProgram(loadFile2)(folders)(v._2)(dictMonadAff)(dictMonadError))(desug1))((e\u03B1) => Monad0.Applicative0().pure({
        primitives: v1.primitives,
        mods: v1.mods,
        datasets: $List("Cons", $Tuple($2, e\u03B1), $1)
      }));
    };
  };
};
var loadProgCxt = (dictMonadAff) => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const $0 = Monad0.Bind1();
  const concatM1 = concatM2(Monad0);
  const module_1 = module_2(dictMonadAff);
  const datasetAs1 = datasetAs(dictMonadAff);
  return (dictMonadError) => {
    const module_22 = module_1(dictMonadError);
    const datasetAs = datasetAs1(dictMonadError);
    return (v) => (mods) => (datasets) => $0.bind($0.bind(Monad0.Applicative0().pure({
      primitives,
      mods: Nil,
      datasets: Nil
    }))(concatM1(arrayMap(module_22(v.loadFile)(v.fluidSrcPaths))(["lib/prelude", ...mods]))))(concatM1(arrayMap((() => {
      const $1 = datasetAs(v.loadFile)(v.fluidSrcPaths);
      return (x) => $1($Tuple(x._1, x._2));
    })())(datasets)));
  };
};

// output-es/Node.Encoding/index.js
var $Encoding = (tag) => tag;
var ASCII = /* @__PURE__ */ $Encoding("ASCII");

// output-es/Data.Nullable/foreign.js
function nullable(a, r, f) {
  return a == null ? r : f(a);
}

// output-es/Node.FS.Constants/foreign.js
import { constants } from "node:fs";
var f_OK = constants.F_OK;
var r_OK = constants.R_OK;
var w_OK = constants.W_OK;
var x_OK = constants.X_OK;
var copyFile_EXCL = constants.COPYFILE_EXCL;
var copyFile_FICLONE = constants.COPYFILE_FICLONE;
var copyFile_FICLONE_FORCE = constants.COPYFILE_FICLONE_FORCE;

// output-es/Node.FS.Async/foreign.js
import {
  access,
  copyFile,
  mkdtemp,
  rename,
  truncate,
  chown,
  chmod,
  stat,
  lstat,
  link as link2,
  symlink,
  readlink,
  realpath,
  unlink,
  rmdir,
  rm,
  mkdir,
  readdir,
  utimes,
  readFile,
  writeFile,
  appendFile,
  open,
  read as read3,
  write as write2,
  close
} from "node:fs";

// output-es/Node.FS.Async/index.js
var handleCallback = (cb) => (err, a) => {
  const v = nullable(err, Nothing, Just);
  if (v.tag === "Nothing") {
    return cb($Either("Right", a))();
  }
  if (v.tag === "Just") {
    return cb($Either("Left", v._1))();
  }
  fail();
};
var readTextFile = (encoding) => (file) => (cb) => {
  const $0 = {
    encoding: (() => {
      if (encoding === "ASCII") {
        return "ASCII";
      }
      if (encoding === "UTF8") {
        return "UTF8";
      }
      if (encoding === "UTF16LE") {
        return "UTF16LE";
      }
      if (encoding === "UCS2") {
        return "UCS2";
      }
      if (encoding === "Base64") {
        return "Base64";
      }
      if (encoding === "Latin1") {
        return "Latin1";
      }
      if (encoding === "Binary") {
        return "Binary";
      }
      if (encoding === "Hex") {
        return "Hex";
      }
      fail();
    })()
  };
  return () => readFile(file, $0, handleCallback(cb));
};
var stat2 = (file) => (cb) => () => stat(file, handleCallback(cb));

// output-es/Node.FS.Aff/index.js
var toAff1 = (f) => (a) => {
  const $0 = f(a);
  return makeAff((k) => {
    const $1 = $0(k);
    return () => {
      $1();
      return nonCanceler;
    };
  });
};
var toAff2 = (f) => (a) => (b) => {
  const $0 = f(a)(b);
  return makeAff((k) => {
    const $1 = $0(k);
    return () => {
      $1();
      return nonCanceler;
    };
  });
};

// output-es/Node.FS.Stats/foreign.js
var isFileImpl = (s) => s.isFile();

// output-es/Module.Node/index.js
var $$try3 = /* @__PURE__ */ $$try(monadErrorAff);
var findM = (dictMonad) => {
  const $0 = dictMonad.Bind1().Apply0();
  return (dictFoldable) => (xs) => (f) => (base) => dictFoldable.foldr((x) => (acc) => $0.apply($0.Functor0().map(altMaybe.alt)(acc))(f(x)))(dictMonad.Applicative0().pure(base))(xs);
};
var loadFile = (folders) => (v) => (dictMonadAff) => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const $0 = Monad0.Bind1();
  const findM1 = findM(Monad0)(foldableArray);
  return (dictMonadError) => $0.bind(findM1(arrayMap((() => {
    const $1 = v + ".fld";
    return (a) => a + "/" + $1;
  })())(folders))((v1) => $0.bind(dictMonadAff.liftAff($$try3(toAff1(stat2)(v1))))((stats) => Monad0.Applicative0().pure((() => {
    if (stats.tag === "Left") {
      return false;
    }
    if (stats.tag === "Right") {
      return isFileImpl(stats._1);
    }
    fail();
  })() ? $Maybe("Just", v1) : Nothing)))(Nothing))((url) => {
    if (url.tag === "Nothing") {
      return throwException(error("File " + v + " not found."))();
    }
    if (url.tag === "Just") {
      return dictMonadAff.liftAff(toAff2(readTextFile)(ASCII)(url._1));
    }
    fail();
  });
};
var loadProgCxt2 = (dictMonadAff) => {
  const loadProgCxt1 = loadProgCxt(dictMonadAff);
  return (dictMonadError) => {
    const loadProgCxt22 = loadProgCxt1(dictMonadError);
    return (fluidSrcPaths) => loadProgCxt22({ loadFile, fluidSrcPaths });
  };
};
var prepConfig = (dictMonadAff) => {
  const prepConfig1 = prepConfig(dictMonadAff);
  return (dictMonadError) => {
    const prepConfig2 = prepConfig1(dictMonadError);
    return (fluidSrcPaths) => prepConfig2({ loadFile, fluidSrcPaths });
  };
};

// output-es/Test.Query/index.js
var prepConfig3 = /* @__PURE__ */ prepConfig(monadAffAff)(monadErrorAff);
var graphEval2 = /* @__PURE__ */ graphEval(monadErrorAff);
var forWithIndex = /* @__PURE__ */ (() => {
  const $0 = traversableWithIndexList.traverseWithIndex(applicativeAff);
  return (b) => (a) => $0(a)(b);
})();
var fromFoldable26 = /* @__PURE__ */ (() => foldableSet.foldr(Cons)(Nil))();
var findMatDim = (vd) => {
  if (vd((dictTypeName) => dictTypeName.typeName) === "MatrixDim") {
    return $Maybe("Just", vd((dictTypeName) => unsafeCoerce));
  }
  return Nothing;
};
var testQuery = {
  file: "slicing/convolution/edgeDetect",
  imports: ["lib/convolution", "slicing/convolution/filter/edge-detect", "slicing/convolution/test-image"],
  fwd_expect: "",
  query: findMatDim,
  intermediates: Leaf2
};
var findMat = (vd) => {
  if (vd((dictTypeName) => dictTypeName.typeName) === "BaseVal") {
    const v1 = vd((dictTypeName) => unsafeCoerce);
    if (v1.tag === "Matrix" && v1._1._2._1._1 === 3 && v1._1._2._2._1 === 3) {
      return $Maybe("Just", v1);
    }
  }
  return Nothing;
};
var main = /* @__PURE__ */ (() => {
  const $0 = fromAff((() => {
    const fluidSrcPaths = ["test/fluid", "fluid"];
    return _bind(loadProgCxt2(monadAffAff)(monadErrorAff)(fluidSrcPaths)(testQuery.imports)([]))((progCxt) => _bind(prepConfig3(fluidSrcPaths)("slicing/convolution/edgeDetect")(progCxt))((v) => _bind(graphEval2(v.gconfig)(v.e))((ge) => _bind(forWithIndex(fromFoldable26(runQuery(ordBaseVal(ordVertex))(graphGraphImpl)(findMat)(ge.g)))((i) => (out) => _bind(_liftEffect(log2(showIntImpl(i))))(() => _bind(_liftEffect(log2(intercalate3("\n")(removeDocWS(prettyBaseVal(highlightableUnit).pretty(functorBaseVal.map((v$1) => {
    })(out))).lines))))(() => _liftEffect(log2(""))))))(() => _pure()))));
  })());
  return () => {
    $0();
  };
})();

// <stdin>
main();
