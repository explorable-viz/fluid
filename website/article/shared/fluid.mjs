#!/usr/bin/env node
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

// output-es/Control.Apply/index.js
var identity = (x) => x;

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
var identity2 = (x) => x;

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
var monoidMaybe = (dictSemigroup) => {
  const semigroupMaybe1 = {
    append: (v) => (v1) => {
      if (v.tag === "Nothing") {
        return v1;
      }
      if (v1.tag === "Nothing") {
        return v;
      }
      if (v.tag === "Just" && v1.tag === "Just") {
        return $Maybe("Just", dictSemigroup.append(v._1)(v1._1));
      }
      fail();
    }
  };
  return { mempty: Nothing, Semigroup0: () => semigroupMaybe1 };
};
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

// output-es/Effect/foreign.js
var pureE = function(a) {
  return function() {
    return a;
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
      return go$r;
    };
    return (x) => go(f(x));
  },
  Monad0: () => monadIdentity
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

// output-es/Data.Array.ST.Iterator/index.js
var $Iterator = (_1, _2) => ({ tag: "Iterator", _1, _2 });
var pushWhile = (p) => (iter) => (array) => () => {
  let $$break = false;
  const $0 = iter._2;
  while ((() => {
    const $1 = $$break;
    return !$1;
  })()) {
    const i = $0.value;
    const mx = iter._1(i);
    if (mx.tag === "Just" && p(mx._1)) {
      array.push(mx._1);
      iter._2.value;
      const $1 = iter._2.value;
      iter._2.value = $1 + 1 | 0;
      continue;
    }
    $$break = true;
  }
};
var iterate = (iter) => (f) => () => {
  let $$break = false;
  const $0 = iter._2;
  while ((() => {
    const $1 = $$break;
    return !$1;
  })()) {
    const i = $0.value;
    const $1 = $0.value;
    $0.value = $1 + 1 | 0;
    const mx = iter._1(i);
    if (mx.tag === "Just") {
      f(mx._1)();
      continue;
    }
    if (mx.tag === "Nothing") {
      $$break = true;
      continue;
    }
    fail();
  }
};

// output-es/Data.Maybe.First/index.js
var semigroupFirst = {
  append: (v) => (v1) => {
    if (v.tag === "Just") {
      return v;
    }
    return v1;
  }
};
var monoidFirst = { mempty: Nothing, Semigroup0: () => semigroupFirst };

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
var identity3 = (x) => x;
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
var lookup = (dictFoldable) => {
  const foldMap22 = dictFoldable.foldMap(monoidFirst);
  return (dictEq) => (a) => foldMap22((v) => {
    if (dictEq.eq(a)(v._1)) {
      return $Maybe("Just", v._2);
    }
    return Nothing;
  });
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
var ordStringImpl = unsafeCompareImpl;
var ordCharImpl = unsafeCompareImpl;

// output-es/Data.Ord/index.js
var ordString = { compare: /* @__PURE__ */ ordStringImpl(LT)(EQ)(GT), Eq0: () => eqString };
var ordInt = { compare: /* @__PURE__ */ ordIntImpl(LT)(EQ)(GT), Eq0: () => eqInt };
var ordChar = { compare: /* @__PURE__ */ ordCharImpl(LT)(EQ)(GT), Eq0: () => eqChar };

// output-es/Unsafe.Coerce/foreign.js
var unsafeCoerce = function(x) {
  return x;
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
  return function(apply5) {
    return function(map2) {
      return function(pure3) {
        return function(f) {
          return function(array) {
            function go(bot, top) {
              switch (top - bot) {
                case 0:
                  return pure3([]);
                case 1:
                  return map2(array1)(f(array[bot]));
                case 2:
                  return apply5(map2(array2)(f(array[bot])))(f(array[bot + 1]));
                case 3:
                  return apply5(apply5(map2(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                default:
                  var pivot = bot + Math.floor((top - bot) / 4) * 2;
                  return apply5(map2(concat22)(go(bot, pivot)))(go(pivot, top));
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
var identity4 = (x) => x;
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
  sequence: (dictApplicative) => traversableArray.traverse(dictApplicative)(identity4),
  Functor0: () => functorArray,
  Foldable1: () => foldableArray
};

// output-es/Data.Array/foreign.js
var range = function(start) {
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
var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
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
var reverse = function(l) {
  return l.slice().reverse();
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
var filter = function(f) {
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
var slice = function(s) {
  return function(e) {
    return function(l) {
      return l.slice(s, e);
    };
  };
};
var zipWith = function(f) {
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
var zip = /* @__PURE__ */ zipWith(Tuple);
var updateAt = /* @__PURE__ */ _updateAt(Just)(Nothing);
var uncons = /* @__PURE__ */ unconsImpl((v) => Nothing)((x) => (xs) => $Maybe("Just", { head: x, tail: xs }));
var toUnfoldable = (dictUnfoldable) => (xs) => {
  const len = xs.length;
  return dictUnfoldable.unfoldr((i) => {
    if (i < len) {
      return $Maybe("Just", $Tuple(xs[i], i + 1 | 0));
    }
    return Nothing;
  })(0);
};
var sortBy = (comp) => sortByImpl2(comp)((v) => {
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
var sortWith = (dictOrd) => (f) => sortBy((x) => (y) => dictOrd.compare(f(x))(f(y)));
var mapWithIndex = (f) => (xs) => zipWith(f)(range(0)(xs.length - 1 | 0))(xs);
var init = (xs) => {
  if (xs.length === 0) {
    return Nothing;
  }
  return $Maybe("Just", slice(0)(xs.length - 1 | 0)(xs));
};
var index = /* @__PURE__ */ indexImpl(Just)(Nothing);
var unsnoc = (xs) => applyMaybe.apply((() => {
  const $0 = init(xs);
  if ($0.tag === "Just") {
    return $Maybe(
      "Just",
      (() => {
        const $1 = $0._1;
        return (v1) => ({ init: $1, last: v1 });
      })()
    );
  }
  return Nothing;
})())(index(xs)(xs.length - 1 | 0));
var groupBy = (op) => (xs) => {
  const result = [];
  const $0 = { value: 0 };
  const iter = $Iterator((v) => index(xs)(v), $0);
  iterate(iter)((x) => () => {
    const sub1 = [];
    sub1.push(x);
    pushWhile(op(x))(iter)(sub1)();
    result.push(sub1);
  })();
  return result;
};
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
var drop = (n) => (xs) => {
  if (n < 1) {
    return xs;
  }
  return slice(n)(xs.length)(xs);
};
var cons2 = (x) => (xs) => [x, ...xs];
var some = (dictAlternative) => (dictLazy) => (v) => dictAlternative.Applicative0().Apply0().apply(dictAlternative.Plus1().Alt0().Functor0().map(cons2)(v))(dictLazy.defer((v1) => many(dictAlternative)(dictLazy)(v)));
var many = (dictAlternative) => (dictLazy) => (v) => dictAlternative.Plus1().Alt0().alt(some(dictAlternative)(dictLazy)(v))(dictAlternative.Applicative0().pure([]));
var concatMap = (b) => (a) => arrayBind(a)(b);
var mapMaybe = (f) => concatMap((x) => {
  const $0 = f(x);
  if ($0.tag === "Nothing") {
    return [];
  }
  if ($0.tag === "Just") {
    return [$0._1];
  }
  fail();
});

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
var identity5 = (x) => x;
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
  sequence: (dictApplicative) => traversableList.traverse(dictApplicative)(identity5),
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
var length2 = function(s) {
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
var take = function(n) {
  return function(s) {
    return s.substr(0, n);
  };
};
var drop2 = function(n) {
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
  return $Maybe("Just", { head: charAt(0)(v), tail: drop2(1)(v) });
};
var toChar = /* @__PURE__ */ _toChar(Just)(Nothing);
var stripSuffix = (v) => (str) => {
  const v1 = splitAt(length2(str) - length2(v) | 0)(str);
  if (v1.after === v) {
    return $Maybe("Just", v1.before);
  }
  return Nothing;
};
var stripPrefix = (v) => (str) => {
  const v1 = splitAt(length2(v))(str);
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
var trim = function(s) {
  return s.trim();
};
var joinWith = function(s) {
  return function(xs) {
    return xs.join(s);
  };
};

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

// output-es/Partial/foreign.js
var _crashWith = function(msg) {
  throw new Error(msg);
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
var altExceptT = (dictSemigroup) => (dictMonad) => {
  const Bind1 = dictMonad.Bind1();
  const $0 = dictMonad.Applicative0();
  const $1 = Bind1.Apply0().Functor0();
  const functorExceptT1 = {
    map: (f) => $1.map((m) => {
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
    alt: (v) => (v1) => Bind1.bind(v)((rm2) => {
      if (rm2.tag === "Right") {
        return $0.pure($Either("Right", rm2._1));
      }
      if (rm2.tag === "Left") {
        const $2 = rm2._1;
        return Bind1.bind(v1)((rn) => {
          if (rn.tag === "Right") {
            return $0.pure($Either("Right", rn._1));
          }
          if (rn.tag === "Left") {
            return $0.pure($Either("Left", dictSemigroup.append($2)(rn._1)));
          }
          fail();
        });
      }
      fail();
    }),
    Functor0: () => functorExceptT1
  };
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

// output-es/Control.Monad.Reader.Trans/index.js
var bindReaderT = (dictBind) => {
  const $0 = dictBind.Apply0();
  const $1 = $0.Functor0();
  const applyReaderT1 = (() => {
    const functorReaderT1 = {
      map: (x) => {
        const $2 = $1.map(x);
        return (v) => (x$1) => $2(v(x$1));
      }
    };
    return { apply: (v) => (v1) => (r) => $0.apply(v(r))(v1(r)), Functor0: () => functorReaderT1 };
  })();
  return { bind: (v) => (k) => (r) => dictBind.bind(v(r))((a) => k(a)(r)), Apply0: () => applyReaderT1 };
};
var monadReaderT = (dictMonad) => {
  const $0 = dictMonad.Applicative0();
  const $1 = $0.Apply0();
  const applicativeReaderT1 = (() => {
    const $2 = $1.Functor0();
    const functorReaderT1 = {
      map: (x) => {
        const $3 = $2.map(x);
        return (v) => (x$1) => $3(v(x$1));
      }
    };
    const applyReaderT1 = { apply: (v) => (v1) => (r) => $1.apply(v(r))(v1(r)), Functor0: () => functorReaderT1 };
    return {
      pure: (x) => {
        const $3 = $0.pure(x);
        return (v) => $3;
      },
      Apply0: () => applyReaderT1
    };
  })();
  const bindReaderT1 = bindReaderT(dictMonad.Bind1());
  return { Applicative0: () => applicativeReaderT1, Bind1: () => bindReaderT1 };
};

// output-es/Control.Monad.State.Trans/index.js
var evalStateT = (dictFunctor) => (v) => (s) => dictFunctor.map(fst)(v(s));
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
var monadEffectState = (dictMonadEffect) => {
  const Monad0 = dictMonadEffect.Monad0();
  const monadStateT1 = { Applicative0: () => applicativeStateT(Monad0), Bind1: () => bindStateT(Monad0) };
  return {
    liftEffect: (x) => {
      const $0 = dictMonadEffect.liftEffect(x);
      return (s) => Monad0.Bind1().bind($0)((x$1) => Monad0.Applicative0().pure($Tuple(x$1, s)));
    },
    Monad0: () => monadStateT1
  };
};
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
var monadAffState = (dictMonadAff) => {
  const MonadEffect0 = dictMonadAff.MonadEffect0();
  const monadEffectState2 = monadEffectState(MonadEffect0);
  return {
    liftAff: (() => {
      const $0 = MonadEffect0.Monad0();
      return (x) => {
        const $1 = dictMonadAff.liftAff(x);
        return (s) => $0.Bind1().bind($1)((x$1) => $0.Applicative0().pure($Tuple(x$1, s)));
      };
    })(),
    MonadEffect0: () => monadEffectState2
  };
};

// output-es/Effect.Console/foreign.js
var log = function(s) {
  return function() {
    console.log(s);
  };
};

// output-es/Data.List/index.js
var identity7 = (x) => x;
var unzip = /* @__PURE__ */ (() => foldableList.foldr((v) => {
  const $0 = v._1;
  const $1 = v._2;
  return (v1) => $Tuple($List("Cons", $0, v1._1), $List("Cons", $1, v1._2));
})($Tuple(Nil, Nil)))();
var toUnfoldable2 = (dictUnfoldable) => dictUnfoldable.unfoldr((xs) => {
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
var sortBy2 = (cmp) => {
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
var reverse2 = /* @__PURE__ */ (() => {
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
        go$r = reverse2(v);
        continue;
      }
      if (v2.tag === "Nil") {
        go$c = false;
        go$r = reverse2(v);
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
var unsnoc2 = (lst) => {
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
    return $Maybe("Just", { init: reverse2($0._1.revInit), last: $0._1.last });
  }
  return Nothing;
};
var zipWith2 = (f) => (xs) => (ys) => {
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
  return reverse2(go(xs)(ys)(Nil));
};
var range3 = (start) => (end) => {
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
var mapMaybe2 = (f) => {
  const go = (go$a0$copy) => (go$a1$copy) => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v1.tag === "Nil") {
        go$c = false;
        go$r = reverse2(v);
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
      return $Step("Done", reverse2(acc));
    }
    fail();
  })())))(Nil);
};
var some2 = (dictAlternative) => (dictLazy) => (v) => dictAlternative.Applicative0().Apply0().apply(dictAlternative.Plus1().Alt0().Functor0().map(Cons)(v))(dictLazy.defer((v1) => many2(dictAlternative)(dictLazy)(v)));
var many2 = (dictAlternative) => (dictLazy) => (v) => dictAlternative.Plus1().Alt0().alt(some2(dictAlternative)(dictLazy)(v))(dictAlternative.Applicative0().pure(Nil));
var groupBy2 = (v) => (v1) => {
  if (v1.tag === "Nil") {
    return Nil;
  }
  if (v1.tag === "Cons") {
    const v2 = span(v(v1._1))(v1._2);
    return $List("Cons", $NonEmpty(v1._1, v2.init), groupBy2(v)(v2.rest));
  }
  fail();
};
var drop3 = (drop$a0$copy) => (drop$a1$copy) => {
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
var filter2 = (p) => {
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
          go$r = $Step2("Cons", v._1, filter2(p)(v._2));
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
var replicate2 = (dictUnfoldable) => (n) => (v) => dictUnfoldable.unfoldr((i) => {
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
var toUnfoldable3 = (dictUnfoldable) => (m) => {
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
var lookup2 = (dictOrd) => (k) => {
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
          const max4 = maxNode(m._1);
          down$c = false;
          down$r = $Maybe(
            "Just",
            $Tuple(m._3, removeMaxNode($List("Cons", $TreeContext("TwoLeft", max4.key, max4.value, m._4), ctx))(m._1))
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
          const max4 = maxNode(m._1);
          down$c = false;
          down$r = $Maybe(
            "Just",
            $Tuple(m._3, removeMaxNode($List("Cons", $TreeContext("ThreeLeft", max4.key, max4.value, m._4, m._5, m._6, m._7), ctx))(m._1))
          );
          continue;
        }
        if (v === "EQ") {
          const max4 = maxNode(m._4);
          down$c = false;
          down$r = $Maybe(
            "Just",
            $Tuple(m._6, removeMaxNode($List("Cons", $TreeContext("ThreeMiddle", m._1, m._2, m._3, max4.key, max4.value, m._7), ctx))(m._4))
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
  return { eq: (m1) => (m2) => eq1(toUnfoldable3(unfoldableArray)(m1))(toUnfoldable3(unfoldableArray)(m2)) };
};
var fromFoldable = (dictOrd) => (dictFoldable) => dictFoldable.foldl((m) => (v) => insert(dictOrd)(v._1)(v._2)(m))(Leaf2);
var filterWithKey = (dictOrd) => {
  const fromFoldable111 = fromFoldable(dictOrd)(foldableList2);
  return (predicate) => {
    const $0 = filter2((v) => predicate(v._1)(v._2));
    return (x) => fromFoldable111($0(toUnfoldable3(unfoldableList2)(x)));
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
  const v = f(lookup2(dictOrd)(k)(m));
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

// output-es/Data.Set/index.js
var fromFoldable1 = /* @__PURE__ */ (() => fromFoldableImpl(foldableList.foldr))();
var tailRecM2 = (f) => (a) => (b) => monadRecST.tailRecM((o) => f(o.a)(o.b))({ a, b });
var union = (dictOrd) => (v) => (v1) => unionWith(dictOrd)($$const)(v)(v1);
var toUnfoldable4 = (dictUnfoldable) => {
  const $0 = toUnfoldable2(dictUnfoldable);
  return (x) => $0(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(x));
};
var toUnfoldable1 = /* @__PURE__ */ toUnfoldable4(unfoldableArray);
var size2 = (v) => size(v);
var showSet = (dictShow) => ({ show: (s) => "(fromFoldable " + showArrayImpl(dictShow.show)(toUnfoldable1(s)) + ")" });
var member = (dictOrd) => (a) => (v) => {
  const $0 = lookup2(dictOrd)(a)(v);
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
var intersection = (dictOrd) => {
  const fromFoldable34 = foldlArray((m) => (a) => insert(dictOrd)(a)()(m))(Leaf2);
  return (s1) => (s2) => {
    const rs = fromFoldable1(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(s2));
    const rl = rs.length;
    const ls = fromFoldable1(foldableWithIndexMap.foldrWithIndex((k) => (v) => (acc) => $List("Cons", k, acc))(Nil)(s1));
    const ll = ls.length;
    return fromFoldable34((() => {
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

// output-es/Data.Number/foreign.js
var isFiniteImpl = isFinite;
var ceil = Math.ceil;
var floor = Math.floor;
var log2 = Math.log;
var pow = function(n) {
  return function(p) {
    return Math.pow(n, p);
  };
};
var round = Math.round;

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
var fromStringAsImpl = function(just) {
  return function(nothing) {
    return function(radix) {
      var digits;
      if (radix < 11) {
        digits = "[0-" + (radix - 1).toString() + "]";
      } else if (radix === 11) {
        digits = "[0-9a]";
      } else {
        digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
      }
      var pattern2 = new RegExp("^[\\+\\-]?" + digits + "+$", "i");
      return function(s) {
        if (pattern2.test(s)) {
          var i = parseInt(s, radix);
          return (i | 0) === i ? just(i) : nothing;
        } else {
          return nothing;
        }
      };
    };
  };
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
var fromStringAs = /* @__PURE__ */ fromStringAsImpl(Just)(Nothing);
var fromString = /* @__PURE__ */ fromStringAs(10);
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
function _foldM(bind3) {
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
            acc = bind3(acc)(g(k));
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
  return (x) => toUnfoldable(dictUnfoldable)($0($1(x)));
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
var lookup3 = ($0) => ($1) => _lookup(Nothing, Just, $0, $1);
var isSubmap = (dictEq) => (m1) => (m2) => all2((k) => (v) => _lookup(false, dictEq.eq(v), k, m2))(m1);
var isEmpty2 = /* @__PURE__ */ all2((v) => (v1) => false);
var insert2 = (k) => (v) => mutate(($0) => () => {
  $0[k] = v;
  return $0;
});
var functorObject = { map: (f) => (m) => _fmapObject(m, f) };
var functorWithIndexObject = { mapWithIndex: mapWithKey, Functor0: () => functorObject };
var fromFoldable2 = (dictFoldable) => {
  const fromFoldable111 = fromFoldableImpl(dictFoldable.foldr);
  return (l) => {
    const s = {};
    for (const v of fromFoldable111(l)) {
      s[v._1] = v._2;
    }
    return s;
  };
};
var foldM = (dictMonad) => {
  const bind12 = dictMonad.Bind1().bind;
  return (f) => (z) => _foldM(bind12)(f)(dictMonad.Applicative0().pure(z));
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
var concatArray = function(xs) {
  return function(ys) {
    if (xs.length === 0)
      return ys;
    if (ys.length === 0)
      return xs;
    return xs.concat(ys);
  };
};

// output-es/Data.Semigroup/index.js
var semigroupString = { append: concatString };
var semigroupArray = { append: concatArray };

// output-es/Data.Semigroup.Foldable/index.js
var minimum = (dictOrd) => {
  const semigroupMin = {
    append: (v) => (v1) => {
      const v$1 = dictOrd.compare(v)(v1);
      if (v$1 === "LT") {
        return v;
      }
      if (v$1 === "EQ") {
        return v;
      }
      if (v$1 === "GT") {
        return v1;
      }
      fail();
    }
  };
  return (dictFoldable1) => dictFoldable1.foldMap1(semigroupMin)(unsafeCoerce);
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
  return function(apply5) {
    return function(map2) {
      return function(f) {
        var buildFrom = function(x, ys) {
          return apply5(map2(consList)(f(x)))(ys);
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
  const v1 = unsnoc2(v._2);
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
  const $0 = unsnoc2(v._2);
  if ($0.tag === "Just") {
    return $List("Cons", v._1, $0._1.init);
  }
  return Nil;
};

// output-es/Data.Profunctor/index.js
var profunctorFn = { dimap: (a2b) => (c2d) => (b2c) => (x) => c2d(b2c(a2b(x))) };

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
var identity12 = (x) => x;
var isEmptySet = { isEmpty };
var isEmptyObject = { isEmpty: isEmpty2 };
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
var unsafeArrayArray = {
  unsafeIndex: (xs) => (i) => definitely("index within bounds")(index(xs)(i)),
  unsafeUpdateAt: (i) => (x) => {
    const $0 = updateAt(i)(x);
    return (x$1) => definitely("index within bounds")($0(x$1));
  }
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
  return (f) => (x) => (y) => Bind1.bind($0.apply($0.Functor0().map(f)(x))(y))(identity2);
};
var assertWith = (v) => (v1) => {
  if (v1) {
    return identity12;
  }
  return (v2) => throwException(error("Assertion failure: " + v))();
};
var assertWhen = (v) => (v1) => {
  if (!v) {
    return (v$1) => identity12;
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
var identity13 = (x) => x;
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
  lookup: lookup3,
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
  const toUnfoldable15 = dictMap.toUnfoldable(unfoldableList);
  return (m) => assertWith("")(dictMap.Set0().size(m) === 1)(definitely("singleton map")((() => {
    const $0 = toUnfoldable15(m);
    if ($0.tag === "Nil") {
      return Nothing;
    }
    if ($0.tag === "Cons") {
      return $Maybe("Just", $0._1);
    }
    fail();
  })()));
};

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
    /* @__PURE__ */ $Tuple("MultiView", 1),
    /* @__PURE__ */ $Tuple("Paragraph", 1),
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
  /* @__PURE__ */ dataType("ParaFragment")([/* @__PURE__ */ $Tuple("Text", 1), /* @__PURE__ */ $Tuple("Link", 2)])
]);
var ctrToDataType = /* @__PURE__ */ (() => fromFoldable2(foldableList)(bindList.bind(listMap((d) => listMap((v) => $Tuple(
  v,
  d
))(toUnfoldable5(fromFoldable12(mapObjectString.keys(d._2)))))(dataTypes))(identity7)))();
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
var identity14 = (x) => x;
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
var traversableDict = {
  traverse: (dictApplicative) => {
    const $0 = traversableWithIndexObject.traverseWithIndex(dictApplicative);
    return (f) => (m) => dictApplicative.Apply0().Functor0().map((v1) => v1)($0((v) => f)(m));
  },
  sequence: (dictApplicative) => (v) => traversableDict.traverse(dictApplicative)(identity14)(v),
  Functor0: () => functorDict,
  Foldable1: () => foldableDict
};

// output-es/Graph/index.js
var fromFoldable4 = /* @__PURE__ */ (() => fromFoldableImpl(foldableSet.foldr))();
var fromFoldable13 = /* @__PURE__ */ (() => fromFoldableImpl(foldableList.foldr))();
var fromFoldable32 = /* @__PURE__ */ (() => foldableSet.foldr(Cons)(Nil))();
var Vertex = (x) => x;
var eqVertex = { eq: (x) => (y) => x === y };
var ordVertex = { compare: (x) => (y) => ordString.compare(x)(y), Eq0: () => eqVertex };
var eqDVertex$p = { eq: (v) => (v1) => v._1 === v1._1 };
var ordDVertex$p = { compare: (v) => (v1) => ordString.compare(v._1)(v1._1), Eq0: () => eqDVertex$p };
var unions1 = /* @__PURE__ */ foldlArray(/* @__PURE__ */ union(ordDVertex$p))(Leaf2);
var verticesDict = (dictVertices) => {
  const vertices1 = dictVertices.vertices;
  return { vertices: (d) => unions1(arrayMap(vertices1)(values(d))) };
};
var showVertices = (\u03B1s) => "{" + joinWith(", ")(fromFoldable4(map(ordString)(unsafeCoerce)(\u03B1s))) + "}";
var showEdgeList = (es) => joinWith("\n")([
  "digraph G {",
  ...arrayMap((v) => "   " + v)([
    "rankdir = RL",
    ...arrayMap((v) => v._1._1 + " -> {" + joinWith(", ")(fromFoldable4(map(ordString)(unsafeCoerce)(v._2))) + "}")(fromFoldable13(reverse2(es)))
  ]),
  "}"
]);
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
var inEdges$p = (dictGraph) => (g) => (\u03B1) => fromFoldable32(map(ordTuple(ordVertex)(ordVertex))((v) => $Tuple(v, \u03B1))(dictGraph.inN(g)(\u03B1)));
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
  return go($0($Tuple(fromFoldable32(\u03B1s), Nil)));
};
var addresses = (dictVertices) => {
  const $0 = map(ordVertex)((x) => x._1);
  return (x) => $0(dictVertices.vertices(x));
};

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
  sequence: (dictApplicative) => traversablePair.traverse(dictApplicative)(identity4),
  Functor0: () => functorPair,
  Foldable1: () => foldablePair
};
var toTuple = (v) => $Tuple(v._1, v._2);
var unzip3 = (xys) => unzip(listMap(toTuple)(xys));

// output-es/Lattice/index.js
var identity15 = (x) => x;
var meetSemilatticeUnit = { meet: (v) => identity15 };
var joinSemilatticeUnit = { join: (v) => identity15 };
var boundedMeetSemilatticeUni = { top: void 0, MeetSemilattice0: () => meetSemilatticeUnit };
var boundedJoinSemilatticeUni = { bot: void 0, JoinSemilattice0: () => joinSemilatticeUnit };

// output-es/Doc/index.js
var $DocCommentElem = (tag, _1) => ({ tag, _1 });
var $DocOpt = (tag, _1) => ({ tag, _1 });
var unions = /* @__PURE__ */ (() => {
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
        go$a0 = unionWith(ordDVertex$p)($$const)(b)(v._1);
        go$a1 = v._2;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go(Leaf2);
})();
var identity16 = (x) => x;
var Unquote = (value0) => $DocCommentElem("Unquote", value0);
var None = /* @__PURE__ */ $DocOpt("None");
var Doc = (value0) => $DocOpt("Doc", value0);
var verticesDocOptVertex = (dictVertices) => ({
  vertices: (v) => {
    if (v.tag === "None") {
      return Leaf2;
    }
    if (v.tag === "Doc") {
      return unions(listMap((v$1) => {
        if (v$1.tag === "Token") {
          return Leaf2;
        }
        if (v$1.tag === "Unquote") {
          return dictVertices.vertices(v$1._1);
        }
        fail();
      })(v._1));
    }
    fail();
  }
});
var showDocCommentElem = (dictShow) => ({
  show: (v) => {
    if (v.tag === "Token") {
      return "Token " + showStringImpl(v._1);
    }
    if (v.tag === "Unquote") {
      return "Unquote " + dictShow.show(v._1);
    }
    fail();
  }
});
var showDocOpt = (dictShow) => ({
  show: (v) => {
    if (v.tag === "None") {
      return "None";
    }
    if (v.tag === "Doc") {
      return "Doc " + showList(showDocCommentElem(dictShow)).show(v._1);
    }
    fail();
  }
});
var semigroupDocOpt = {
  append: (v) => (v1) => {
    if (v1.tag === "None") {
      return v;
    }
    if (v.tag === "None") {
      return v1;
    }
    if (v.tag === "Doc" && v1.tag === "Doc") {
      return $DocOpt("Doc", foldableList.foldr(Cons)(v1._1)(v._1));
    }
    fail();
  }
};
var functorDocOpt = (dictFunctor) => ({
  map: (f) => (m) => {
    if (m.tag === "None") {
      return None;
    }
    if (m.tag === "Doc") {
      return $DocOpt(
        "Doc",
        listMap((m$1) => {
          if (m$1.tag === "Token") {
            return $DocCommentElem("Token", m$1._1);
          }
          if (m$1.tag === "Unquote") {
            return $DocCommentElem("Unquote", dictFunctor.map(f)(m$1._1));
          }
          fail();
        })(m._1)
      );
    }
    fail();
  }
});
var foldableDocOpt = (dictFoldable) => ({
  foldl: (f) => (z) => (m) => {
    if (m.tag === "None") {
      return z;
    }
    if (m.tag === "Doc") {
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
              if (v._1.tag === "Token") {
                return b;
              }
              if (v._1.tag === "Unquote") {
                return dictFoldable.foldl(f)(b)(v._1._1);
              }
              fail();
            })();
            go$a1 = v._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      return go(z)(m._1);
    }
    fail();
  },
  foldr: (f) => (z) => (m) => {
    if (m.tag === "None") {
      return z;
    }
    if (m.tag === "Doc") {
      return foldableList.foldr((b) => (a) => {
        if (b.tag === "Token") {
          return a;
        }
        if (b.tag === "Unquote") {
          return dictFoldable.foldr(f)(a)(b._1);
        }
        fail();
      })(z)(m._1);
    }
    fail();
  },
  foldMap: (dictMonoid) => {
    const mempty = dictMonoid.mempty;
    const foldMap22 = foldableList.foldMap(dictMonoid);
    const mempty$1 = dictMonoid.mempty;
    const foldMap2$1 = dictFoldable.foldMap(dictMonoid);
    return (f) => (m) => {
      if (m.tag === "None") {
        return mempty;
      }
      if (m.tag === "Doc") {
        return foldMap22((m$1) => {
          if (m$1.tag === "Token") {
            return mempty$1;
          }
          if (m$1.tag === "Unquote") {
            return foldMap2$1(f)(m$1._1);
          }
          fail();
        })(m._1);
      }
      fail();
    };
  }
});
var traversableDocCommentElem = (dictTraversable) => {
  const $0 = dictTraversable.Functor0();
  const functorDocCommentElem1 = {
    map: (f) => (m) => {
      if (m.tag === "Token") {
        return $DocCommentElem("Token", m._1);
      }
      if (m.tag === "Unquote") {
        return $DocCommentElem("Unquote", $0.map(f)(m._1));
      }
      fail();
    }
  };
  const $1 = dictTraversable.Foldable1();
  const foldableDocCommentElem1 = {
    foldl: (f) => (z) => (m) => {
      if (m.tag === "Token") {
        return z;
      }
      if (m.tag === "Unquote") {
        return $1.foldl(f)(z)(m._1);
      }
      fail();
    },
    foldr: (f) => (z) => (m) => {
      if (m.tag === "Token") {
        return z;
      }
      if (m.tag === "Unquote") {
        return $1.foldr(f)(z)(m._1);
      }
      fail();
    },
    foldMap: (dictMonoid) => {
      const mempty = dictMonoid.mempty;
      const foldMap22 = $1.foldMap(dictMonoid);
      return (f) => (m) => {
        if (m.tag === "Token") {
          return mempty;
        }
        if (m.tag === "Unquote") {
          return foldMap22(f)(m._1);
        }
        fail();
      };
    }
  };
  return {
    traverse: (dictApplicative) => {
      const traverse2 = dictTraversable.traverse(dictApplicative);
      return (f) => (m) => {
        if (m.tag === "Token") {
          return dictApplicative.pure($DocCommentElem("Token", m._1));
        }
        if (m.tag === "Unquote") {
          return dictApplicative.Apply0().Functor0().map((v1) => $DocCommentElem("Unquote", v1))(traverse2(f)(m._1));
        }
        fail();
      };
    },
    sequence: (dictApplicative) => (v) => traversableDocCommentElem(dictTraversable).traverse(dictApplicative)(identity16)(v),
    Functor0: () => functorDocCommentElem1,
    Foldable1: () => foldableDocCommentElem1
  };
};
var traversableDocOpt = (dictTraversable) => {
  const functorDocOpt1 = functorDocOpt(dictTraversable.Functor0());
  const foldableDocOpt1 = foldableDocOpt(dictTraversable.Foldable1());
  return {
    traverse: (dictApplicative) => {
      const traverse2 = traversableList.traverse(dictApplicative);
      const traverse3 = traversableDocCommentElem(dictTraversable).traverse(dictApplicative);
      return (f) => (m) => {
        if (m.tag === "None") {
          return dictApplicative.pure(None);
        }
        if (m.tag === "Doc") {
          return dictApplicative.Apply0().Functor0().map((v1) => $DocOpt("Doc", v1))(traverse2(traverse3(f))(m._1));
        }
        fail();
      };
    },
    sequence: (dictApplicative) => (v) => traversableDocOpt(dictTraversable).traverse(dictApplicative)(identity16)(v),
    Functor0: () => functorDocOpt1,
    Foldable1: () => foldableDocOpt1
  };
};

// output-es/Expr/index.js
var $Cont = (tag, _1) => ({ tag, _1 });
var $Elim = (tag, _1, _2) => ({ tag, _1, _2 });
var $Expr = (tag, _1, _2, _3, _4, _5) => ({ tag, _1, _2, _3, _4, _5 });
var $RecDefs = (_1, _2) => ({ tag: "RecDefs", _1, _2 });
var $VarDef = (_1, _2) => ({ tag: "VarDef", _1, _2 });
var union4 = /* @__PURE__ */ (() => setSet(ordDVertex$p).union)();
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
        go$a0 = unionWith(ordDVertex$p)($$const)(b)(v._1);
        go$a1 = v._2;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go(Leaf2);
})();
var identity17 = (x) => x;
var setSet2 = /* @__PURE__ */ setSet(ordString);
var fromFoldable5 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)()(m))(Leaf2))();
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
var asMaplet2 = /* @__PURE__ */ asMaplet(mapDictString);
var ContExpr = (value0) => $Cont("ContExpr", value0);
var Dictionary = (value0) => (value1) => (value2) => $Expr("Dictionary", value0, value1, value2);
var Constr = (value0) => (value1) => (value2) => (value3) => $Expr("Constr", value0, value1, value2, value3);
var Matrix = (value0) => (value1) => (value2) => (value3) => (value4) => $Expr("Matrix", value0, value1, value2, value3, value4);
var Lambda = (value0) => (value1) => $Expr("Lambda", value0, value1);
var Project = (value0) => (value1) => (value2) => $Expr("Project", value0, value1, value2);
var DProject = (value0) => (value1) => (value2) => $Expr("DProject", value0, value1, value2);
var App2 = (value0) => (value1) => (value2) => $Expr("App", value0, value1, value2);
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
      return union4($$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2))(verticesDocOptVertex(verticesExprVertex).vertices(v._2));
    }
    if (v.tag === "Float") {
      return union4($$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2))(verticesDocOptVertex(verticesExprVertex).vertices(v._2));
    }
    if (v.tag === "Str") {
      return union4($$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2))(verticesDocOptVertex(verticesExprVertex).vertices(v._2));
    }
    if (v.tag === "Dictionary") {
      return union4($$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2))(union4(unions12(listMap((v1) => union4(verticesExprVertex.vertices(v1._1))(verticesExprVertex.vertices(v1._2)))(v._3)))(verticesDocOptVertex(verticesExprVertex).vertices(v._2)));
    }
    if (v.tag === "Constr") {
      return union4($$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2))(union4(unions12(listMap(verticesExprVertex.vertices)(v._4)))(verticesDocOptVertex(verticesExprVertex).vertices(v._2)));
    }
    if (v.tag === "Matrix") {
      return union4($$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2))(union4(verticesExprVertex.vertices(v._3))(union4(verticesExprVertex.vertices(v._5))(verticesDocOptVertex(verticesExprVertex).vertices(v._2))));
    }
    if (v.tag === "Lambda") {
      return union4($$$Map("Two", Leaf2, $Tuple(v._1, pack1(v)), void 0, Leaf2))(verticesElimVertex.vertices(v._2));
    }
    if (v.tag === "Project") {
      return union4(verticesDocOptVertex(verticesExprVertex).vertices(v._1))(verticesExprVertex.vertices(v._2));
    }
    if (v.tag === "DProject") {
      return union4(verticesExprVertex.vertices(v._2))(union4(verticesExprVertex.vertices(v._3))(verticesDocOptVertex(verticesExprVertex).vertices(v._1)));
    }
    if (v.tag === "App") {
      return union4(verticesExprVertex.vertices(v._2))(union4(verticesExprVertex.vertices(v._3))(verticesDocOptVertex(verticesExprVertex).vertices(v._1)));
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
      return $Expr("Int", f(m._1), functorDocOpt(functorExpr).map(f)(m._2), m._3);
    }
    if (m.tag === "Float") {
      return $Expr("Float", f(m._1), functorDocOpt(functorExpr).map(f)(m._2), m._3);
    }
    if (m.tag === "Str") {
      return $Expr("Str", f(m._1), functorDocOpt(functorExpr).map(f)(m._2), m._3);
    }
    if (m.tag === "Dictionary") {
      return $Expr(
        "Dictionary",
        f(m._1),
        functorDocOpt(functorExpr).map(f)(m._2),
        listMap((() => {
          const $0 = functorExpr.map(f);
          return (v) => $Pair($0(v._1), $0(v._2));
        })())(m._3)
      );
    }
    if (m.tag === "Constr") {
      return $Expr("Constr", f(m._1), functorDocOpt(functorExpr).map(f)(m._2), m._3, listMap(functorExpr.map(f))(m._4));
    }
    if (m.tag === "Matrix") {
      return $Expr("Matrix", f(m._1), functorDocOpt(functorExpr).map(f)(m._2), functorExpr.map(f)(m._3), m._4, functorExpr.map(f)(m._5));
    }
    if (m.tag === "Lambda") {
      return $Expr("Lambda", f(m._1), functorElim.map(f)(m._2));
    }
    if (m.tag === "Project") {
      return $Expr("Project", functorDocOpt(functorExpr).map(f)(m._1), functorExpr.map(f)(m._2), m._3);
    }
    if (m.tag === "DProject") {
      return $Expr("DProject", functorDocOpt(functorExpr).map(f)(m._1), functorExpr.map(f)(m._2), functorExpr.map(f)(m._3));
    }
    if (m.tag === "App") {
      return $Expr("App", functorDocOpt(functorExpr).map(f)(m._1), functorExpr.map(f)(m._2), functorExpr.map(f)(m._3));
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
      return foldableDocOpt(foldableExpr).foldl(f)(f(z)(m._1))(m._2);
    }
    if (m.tag === "Float") {
      return foldableDocOpt(foldableExpr).foldl(f)(f(z)(m._1))(m._2);
    }
    if (m.tag === "Str") {
      return foldableDocOpt(foldableExpr).foldl(f)(f(z)(m._1))(m._2);
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
      return go(foldableDocOpt(foldableExpr).foldl(f)(f(z)(m._1))(m._2))(m._3);
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
      return go(foldableDocOpt(foldableExpr).foldl(f)(f(z)(m._1))(m._2))(m._4);
    }
    if (m.tag === "Matrix") {
      return foldableExpr.foldl(f)(foldableExpr.foldl(f)(foldableDocOpt(foldableExpr).foldl(f)(f(z)(m._1))(m._2))(m._3))(m._5);
    }
    if (m.tag === "Lambda") {
      return foldableElim.foldl(f)(f(z)(m._1))(m._2);
    }
    if (m.tag === "Project") {
      return foldableExpr.foldl(f)(foldableDocOpt(foldableExpr).foldl(f)(z)(m._1))(m._2);
    }
    if (m.tag === "DProject") {
      return foldableExpr.foldl(f)(foldableExpr.foldl(f)(foldableDocOpt(foldableExpr).foldl(f)(z)(m._1))(m._2))(m._3);
    }
    if (m.tag === "App") {
      return foldableExpr.foldl(f)(foldableExpr.foldl(f)(foldableDocOpt(foldableExpr).foldl(f)(z)(m._1))(m._2))(m._3);
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
      return f(m._1)(foldableDocOpt(foldableExpr).foldr(f)(z)(m._2));
    }
    if (m.tag === "Float") {
      return f(m._1)(foldableDocOpt(foldableExpr).foldr(f)(z)(m._2));
    }
    if (m.tag === "Str") {
      return f(m._1)(foldableDocOpt(foldableExpr).foldr(f)(z)(m._2));
    }
    if (m.tag === "Dictionary") {
      return f(m._1)(foldableDocOpt(foldableExpr).foldr(f)(foldableList.foldr((() => {
        const $0 = foldableExpr.foldr(f);
        const $1 = foldrDefault(foldablePair)((b) => (a) => $0(a)(b));
        return (b) => (a) => $1(a)(b);
      })())(z)(m._3))(m._2));
    }
    if (m.tag === "Constr") {
      return f(m._1)(foldableDocOpt(foldableExpr).foldr(f)(foldableList.foldr((() => {
        const $0 = foldableExpr.foldr(f);
        return (b) => (a) => $0(a)(b);
      })())(z)(m._4))(m._2));
    }
    if (m.tag === "Matrix") {
      return f(m._1)(foldableDocOpt(foldableExpr).foldr(f)(foldableExpr.foldr(f)(foldableExpr.foldr(f)(z)(m._5))(m._3))(m._2));
    }
    if (m.tag === "Lambda") {
      return f(m._1)(foldableElim.foldr(f)(z)(m._2));
    }
    if (m.tag === "Project") {
      return foldableDocOpt(foldableExpr).foldr(f)(foldableExpr.foldr(f)(z)(m._2))(m._1);
    }
    if (m.tag === "DProject") {
      return foldableDocOpt(foldableExpr).foldr(f)(foldableExpr.foldr(f)(foldableExpr.foldr(f)(z)(m._3))(m._2))(m._1);
    }
    if (m.tag === "App") {
      return foldableDocOpt(foldableExpr).foldr(f)(foldableExpr.foldr(f)(foldableExpr.foldr(f)(z)(m._3))(m._2))(m._1);
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
        return $0.append(f(m._1))(foldableDocOpt(foldableExpr).foldMap(dictMonoid)(f)(m._2));
      }
      if (m.tag === "Float") {
        return $0.append(f(m._1))(foldableDocOpt(foldableExpr).foldMap(dictMonoid)(f)(m._2));
      }
      if (m.tag === "Str") {
        return $0.append(f(m._1))(foldableDocOpt(foldableExpr).foldMap(dictMonoid)(f)(m._2));
      }
      if (m.tag === "Dictionary") {
        return $0.append(f(m._1))($0.append(foldableDocOpt(foldableExpr).foldMap(dictMonoid)(f)(m._2))(foldMap3(foldablePair.foldMap(dictMonoid)(foldableExpr.foldMap(dictMonoid)(f)))(m._3)));
      }
      if (m.tag === "Constr") {
        return $0.append(f(m._1))($0.append(foldableDocOpt(foldableExpr).foldMap(dictMonoid)(f)(m._2))(foldMap3(foldableExpr.foldMap(dictMonoid)(f))(m._4)));
      }
      if (m.tag === "Matrix") {
        return $0.append(f(m._1))($0.append(foldableDocOpt(foldableExpr).foldMap(dictMonoid)(f)(m._2))($0.append(foldableExpr.foldMap(dictMonoid)(f)(m._3))(foldableExpr.foldMap(dictMonoid)(f)(m._5))));
      }
      if (m.tag === "Lambda") {
        return $0.append(f(m._1))(foldableElim.foldMap(dictMonoid)(f)(m._2));
      }
      if (m.tag === "Project") {
        return $0.append(foldableDocOpt(foldableExpr).foldMap(dictMonoid)(f)(m._1))(foldableExpr.foldMap(dictMonoid)(f)(m._2));
      }
      if (m.tag === "DProject") {
        return $0.append(foldableDocOpt(foldableExpr).foldMap(dictMonoid)(f)(m._1))($0.append(foldableExpr.foldMap(dictMonoid)(f)(m._2))(foldableExpr.foldMap(dictMonoid)(f)(m._3)));
      }
      if (m.tag === "App") {
        return $0.append(foldableDocOpt(foldableExpr).foldMap(dictMonoid)(f)(m._1))($0.append(foldableExpr.foldMap(dictMonoid)(f)(m._2))(foldableExpr.foldMap(dictMonoid)(f)(m._3)));
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
        const $1 = m._3;
        return Apply0.apply($0.map((v3) => (v4) => $Expr("Int", v3, v4, $1))(f(m._1)))(traversableDocOpt(traversableExpr).traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "Float") {
        const $1 = m._3;
        return Apply0.apply($0.map((v3) => (v4) => $Expr("Float", v3, v4, $1))(f(m._1)))(traversableDocOpt(traversableExpr).traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "Str") {
        const $1 = m._3;
        return Apply0.apply($0.map((v3) => (v4) => $Expr("Str", v3, v4, $1))(f(m._1)))(traversableDocOpt(traversableExpr).traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "Dictionary") {
        return Apply0.apply(Apply0.apply($0.map((v3) => (v4) => (v5) => $Expr("Dictionary", v3, v4, v5))(f(m._1)))(traversableDocOpt(traversableExpr).traverse(dictApplicative)(f)(m._2)))(traverse5(traverse6(traversableExpr.traverse(dictApplicative)(f)))(m._3));
      }
      if (m.tag === "Constr") {
        const $1 = m._3;
        return Apply0.apply(Apply0.apply($0.map((v4) => (v5) => (v6) => $Expr("Constr", v4, v5, $1, v6))(f(m._1)))(traversableDocOpt(traversableExpr).traverse(dictApplicative)(f)(m._2)))(traverse5(traversableExpr.traverse(dictApplicative)(f))(m._4));
      }
      if (m.tag === "Matrix") {
        const $1 = m._4;
        return Apply0.apply(Apply0.apply(Apply0.apply($0.map((v5) => (v6) => (v7) => (v8) => $Expr("Matrix", v5, v6, v7, $1, v8))(f(m._1)))(traversableDocOpt(traversableExpr).traverse(dictApplicative)(f)(m._2)))(traversableExpr.traverse(dictApplicative)(f)(m._3)))(traversableExpr.traverse(dictApplicative)(f)(m._5));
      }
      if (m.tag === "Lambda") {
        return Apply0.apply($0.map((v2) => (v3) => $Expr("Lambda", v2, v3))(f(m._1)))(traversableElim.traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "Project") {
        const $1 = m._3;
        return Apply0.apply($0.map((v3) => (v4) => $Expr("Project", v3, v4, $1))(traversableDocOpt(traversableExpr).traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2));
      }
      if (m.tag === "DProject") {
        return Apply0.apply(Apply0.apply($0.map((v3) => (v4) => (v5) => $Expr("DProject", v3, v4, v5))(traversableDocOpt(traversableExpr).traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2)))(traversableExpr.traverse(dictApplicative)(f)(m._3));
      }
      if (m.tag === "App") {
        return Apply0.apply(Apply0.apply($0.map((v3) => (v4) => (v5) => $Expr("App", v3, v4, v5))(traversableDocOpt(traversableExpr).traverse(dictApplicative)(f)(m._1)))(traversableExpr.traverse(dictApplicative)(f)(m._2)))(traversableExpr.traverse(dictApplicative)(f)(m._3));
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
  sequence: (dictApplicative) => traversableModule.traverse(dictApplicative)(identity4),
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
      return setSet2.union(fVDocOptExpr.fv(v._2))(unions3(listMap((v1) => setSet2.union(fVExpr.fv(v1._1))(fVExpr.fv(v1._2)))(v._3)));
    }
    if (v.tag === "Constr") {
      return setSet2.union(fVDocOptExpr.fv(v._2))(unions3(listMap(fVExpr.fv)(v._4)));
    }
    if (v.tag === "Matrix") {
      return setSet2.union(fVDocOptExpr.fv(v._2))(setSet2.union(fVExpr.fv(v._3))(fVExpr.fv(v._5)));
    }
    if (v.tag === "Lambda") {
      return fVElim.fv(v._2);
    }
    if (v.tag === "Project") {
      return setSet2.union(fVDocOptExpr.fv(v._1))(fVExpr.fv(v._2));
    }
    if (v.tag === "DProject") {
      return setSet2.union(fVDocOptExpr.fv(v._1))(setSet2.union(fVExpr.fv(v._2))(fVExpr.fv(v._3)));
    }
    if (v.tag === "App") {
      return setSet2.union(fVDocOptExpr.fv(v._1))(setSet2.union(fVExpr.fv(v._2))(fVExpr.fv(v._3)));
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
var fVDocOptExpr = {
  fv: (v) => {
    if (v.tag === "None") {
      return Leaf2;
    }
    if (v.tag === "Doc") {
      return unions3(listMap(fVDocCommentElem.fv)(v._1));
    }
    fail();
  }
};
var fVDocCommentElem = {
  fv: (v) => {
    if (v.tag === "Token") {
      return Leaf2;
    }
    if (v.tag === "Unquote") {
      return fVExpr.fv(v._1);
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
      uncons$a0 = $CatQueue(reverse2(v._2), Nil);
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
var snoc2 = (cat) => (a) => {
  if (cat.tag === "CatNil") {
    return $CatList("CatCons", a, $CatQueue(Nil, Nil));
  }
  if (cat.tag === "CatCons") {
    return $CatList(
      "CatCons",
      cat._1,
      $CatQueue(
        cat._2._1,
        $List("Cons", $CatList("CatCons", a, $CatQueue(Nil, Nil)), cat._2._2)
      )
    );
  }
  fail();
};

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
            const $0 = lookup2(dictOrd)(v1._1._1._1)(state.unvisited);
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
                const $02 = lookup2(dictOrd)(v1._1._1._1)(v);
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
var eqSet = { eq: (v) => (v1) => eqMap(eqVertex)(eqUnit).eq(v)(v1) };
var eq = /* @__PURE__ */ (() => eqObject(eqSet).eq)();
var fromFoldable15 = /* @__PURE__ */ foldlArray((m) => (a) => insert(ordVertex)(a)()(m))(Leaf2);
var toUnfoldable6 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
var fromFoldable22 = /* @__PURE__ */ (() => foldableSet.foldr(Cons)(Nil))();
var toUnfoldable12 = /* @__PURE__ */ toAscUnfoldable(unfoldableArray);
var fromFoldable33 = /* @__PURE__ */ fromFoldable(ordVertex)(foldableArray);
var verticesGraphImpl = {
  vertices: (v) => fold((z) => (v$1) => (a) => insert(ordDVertex$p)(a)()(z))(Leaf2)(_mapWithKey(
    v._1.out,
    (k) => (v1) => $Tuple(k, v1._2)
  ))
};
var eqGraphImpl = { eq: (v) => (v1) => eq(_fmapObject(v._1.out, fst))(_fmapObject(v1._1.out, fst)) };
var sinks$p = (m) => fromFoldable15(arrayMap((x) => x._1)(filter((x) => x._2._1.tag === "Leaf")(toArrayWithKey(Tuple)(m))));
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
    const \u03B1s$p = fromFoldable22(\u03B1s);
    const es$p = reverse2(es);
    const in_ = inMap(\u03B1s$p)(es$p)();
    const out = outMap(\u03B1s$p)(es$p)();
    return $GraphImpl({ out, in_, sinks: sinks$p(out), sources: sinks$p(in_), vertices: map(ordVertex)(Vertex)(mapObjectString.keys(out)) });
  },
  topologicalSort: (v) => reverse2(topologicalSort(ordVertex)(fromFoldable33(arrayMap((x) => $Tuple(
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
  const monadStateT2 = { Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) };
  return {
    extend: (\u03B1) => (\u03B1s) => {
      const $2 = Cons($Tuple(\u03B1, \u03B1s));
      const $3 = $1.state((s) => $Tuple(void 0, $2(s)));
      return (s) => $0.map((v1) => $Tuple(void 0, v1._2))($3(s));
    },
    Monad0: () => monadStateT2
  };
};
var monadAllocAllocT = (dictMonad) => {
  const monadStateT2 = { Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) };
  return {
    fresh: bindStateT(dictMonad).bind(monadStateStateT(dictMonad).state((s) => {
      const s$p = 1 + s | 0;
      return $Tuple(s$p, s$p);
    }))((n) => applicativeStateT(dictMonad).pure(showIntImpl(n))),
    Monad0: () => monadStateT2
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
      return range3($0)(v._2);
    })())),
    v._1
  )
)));
var monadAllocWithGraphAllocT = (dictMonad) => {
  const monadStateT2 = { Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) };
  const monadStateT1 = { Applicative0: () => applicativeStateT(monadStateT2), Bind1: () => bindStateT(monadStateT2) };
  return {
    fresh: (() => {
      const $0 = monadAllocAllocT(dictMonad).fresh;
      return (s) => monadStateT2.Bind1().bind($0)((x) => monadStateT2.Applicative0().pure($Tuple(x, s)));
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
    const $0 = runWithGraphT2(dictGraph);
    const $1 = spyFunWhenM2(false)("runWithGraphT")((() => {
      const $12 = map(ordVertex)((x) => x._1);
      return (x) => showVertices($12(x));
    })())((x) => showEdgeList(toEdgeList(dictGraph)(x._1)));
    return (x) => $1($0(x));
  };
};
var runWithGraphT_spy1 = /* @__PURE__ */ runWithGraphT_spy(monadIdentity);
var monadWithGraphAllocWithGr = (dictMonadError) => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  const monadStateT2 = { Applicative0: () => applicativeStateT(Monad0), Bind1: () => bindStateT(Monad0) };
  const bindStateT2 = bindStateT(monadStateT2);
  const monadAllocWithGraphAllocT1 = monadAllocWithGraphAllocT(Monad0);
  const fresh1 = monadAllocWithGraphAllocT1.fresh;
  const monadWithGraphWithGraphT1 = monadWithGraphWithGraphT(monadStateT2);
  const monadErrorStateT2 = monadErrorStateT(monadErrorStateT(dictMonadError));
  return {
    new: (dictTypeName) => (constr) => (\u03B1s) => (vd) => bindStateT2.bind(fresh1)((\u03B1) => {
      const v = constr(\u03B1)(vd);
      return bindStateT2.bind(monadWithGraphWithGraphT1.extend($Tuple(\u03B1, (k) => k(dictTypeName)(v)))(\u03B1s))(() => applicativeStateT(monadStateT2).pure(v));
    }),
    MonadAlloc0: () => monadAllocWithGraphAllocT1,
    MonadError1: () => monadErrorStateT2,
    MonadWithGraph2: () => monadWithGraphWithGraphT1
  };
};

// output-es/Graph.Slice/index.js
var pure = /* @__PURE__ */ (() => applicativeStateT(monadIdentity).pure)();
var extend = /* @__PURE__ */ (() => monadWithGraphWithGraphT(monadIdentity).extend)();
var tailRecM = /* @__PURE__ */ (() => monadRecStateT(monadRecIdentity).tailRecM)();
var member3 = /* @__PURE__ */ (() => setSet(ordVertex).member)();
var fromFoldable8 = /* @__PURE__ */ (() => foldableSet.foldr(Cons)(Nil))();
var fwdSlice = (dictGraph) => {
  const runWithGraph_spy = runWithGraphT_spy1(dictGraph);
  return (v) => {
    const $0 = v._2;
    const $1 = v._1;
    return runWithGraph_spy(tailRecM((v1) => {
      if (v1.es.tag === "Nil") {
        return pure($Step("Done", void 0));
      }
      if (v1.es.tag === "Cons") {
        const $2 = lookup2(ordVertex)(v1.es._1._1)(v1.pending);
        const \u03B2s = (() => {
          if ($2.tag === "Nothing") {
            return $$$Map("Two", Leaf2, v1.es._1._2, void 0, Leaf2);
          }
          if ($2.tag === "Just") {
            return insert(ordVertex)(v1.es._1._2)()($2._1);
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
    })({ pending: Leaf2, es: inEdges(dictGraph)($0)($1) }))(assertWhen(true)("inputs are sinks")((v$1) => difference2(ordVertex)($1)(dictGraph.sinks($0)).tag === "Leaf")(map(ordDVertex$p)((\u03B1) => $Tuple(
      \u03B1,
      dictGraph.vertexData($0)(\u03B1)
    ))($1)))._1;
  };
};
var bwdSlice = (dictGraph) => {
  const runWithGraph_spy = runWithGraphT_spy1(dictGraph);
  const addresses2 = addresses(dictGraph.Vertices1());
  return (v) => {
    const $0 = v._2;
    const $1 = v._1;
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
          return bindStateT(monadIdentity).bind(extend($Tuple($3, $2))(v1.pending._1._2))(() => pure($Step(
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
            "\u03B1s": foldableList.foldr(Cons)(v1["\u03B1s"]._2)(fromFoldable8(\u03B2s)),
            pending: $List("Cons", $Tuple($Tuple(v1["\u03B1s"]._1, dictGraph.vertexData($0)(v1["\u03B1s"]._1)), \u03B2s), v1.pending)
          }
        ));
      }
      fail();
    })({
      visited: Leaf2,
      "\u03B1s": fromFoldable8(assertWhen(true)("inputs are sinks")((v$1) => difference2(ordVertex)($1)(addresses2($0)).tag === "Leaf")($1)),
      pending: Nil
    }))(Leaf2)._1;
  };
};

// output-es/Data.Profunctor.Choice/index.js
var identity18 = (x) => x;
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
  })(identity18)(identity1))($1.compose(dictChoice.right(r))(dictChoice.left(l)));
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
            var length4 = str.length;
            if (index3 < 0 || index3 >= length4)
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
  const v = length2(s);
  if (v === 0) {
    return Nothing;
  }
  if (v === 1) {
    return $Maybe("Just", { head: toCharCode(charAt(0)(s)), tail: "" });
  }
  const cu1 = toCharCode(charAt(1)(s));
  const cu0 = toCharCode(charAt(0)(s));
  if (55296 <= cu0 && cu0 <= 56319 && 56320 <= cu1 && cu1 <= 57343) {
    return $Maybe("Just", { head: (((cu0 - 55296 | 0) * 1024 | 0) + (cu1 - 56320 | 0) | 0) + 65536 | 0, tail: drop2(2)(s) });
  }
  return $Maybe("Just", { head: cu0, tail: drop2(1)(s) });
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
  if (55296 <= cu0 && cu0 <= 56319 && length2(s) > 1) {
    const cu1 = toCharCode(charAt(1)(s));
    if (56320 <= cu1 && cu1 <= 57343) {
      return (((cu0 - 55296 | 0) * 1024 | 0) + (cu1 - 56320 | 0) | 0) + 65536 | 0;
    }
  }
  return cu0;
};
var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
var indexOf2 = (p) => (s) => {
  const $0 = indexOf(p)(s);
  if ($0.tag === "Just") {
    return $Maybe("Just", toCodePointArray(take($0._1)(s)).length);
  }
  return Nothing;
};
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

// output-es/Data.Show.Generic/foreign.js
var intercalate3 = function(separator) {
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
    return "(" + intercalate3(" ")([ctor, ...v1]) + ")";
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
    return (state1, more, lift12, $$throw2, done) => force(m)(state1, more, lift12, $$throw2, done);
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
var functorParserT = { map: (f) => (v) => (state1, more, lift12, $$throw2, done) => more((v1) => v(state1, more, lift12, $$throw2, (state2, a) => more((v2) => done(state2, f(a))))) };
var applyParserT = {
  apply: (v) => (v1) => (state1, more, lift12, $$throw2, done) => more((v2) => v(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, f) => more((v3) => v1(state2, more, lift12, $$throw2, (state3, a) => more((v4) => done(state3, f(a)))))
  )),
  Functor0: () => functorParserT
};
var bindParserT = {
  bind: (v) => (next) => (state1, more, lift12, $$throw2, done) => more((v1) => v(state1, more, lift12, $$throw2, (state2, a) => more((v2) => next(a)(state2, more, lift12, $$throw2, done)))),
  Apply0: () => applyParserT
};
var applicativeParserT = { pure: (a) => (state1, v, v1, v2, done) => done(state1, a), Apply0: () => applyParserT };
var monadParserT = { Applicative0: () => applicativeParserT, Bind1: () => bindParserT };
var monadRecParserT = {
  tailRecM: (next) => (initArg) => (state1, more, lift12, $$throw2, done) => {
    const loop = (state2, arg, gas) => next(arg)(
      state2,
      more,
      lift12,
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
var fail2 = (message2) => (state1, more, lift12, $$throw2, done) => more((v1) => position(
  state1,
  more,
  lift12,
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
var skipMany1 = (p) => (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => p(
  state1,
  more,
  lift12,
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
        lift12,
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
var sepBy1 = (p) => (sep) => (state1, more, lift12, $$throw2, done) => more((v1) => p(
  state1,
  more,
  lift12,
  $$throw2,
  (state2, a) => more((v2) => {
    const $0 = manyRec2((state1$1, more$1, lift1$1, throw$1, done$1) => more$1((v2$1) => more$1((v1$1) => sep(
      state1$1,
      more$1,
      lift1$1,
      throw$1,
      (state2$1, a$1) => more$1((v2$2) => more$1((v3) => p(state2$1, more$1, lift1$1, throw$1, (state3, a$2) => more$1((v4) => done$1(state3, a$2)))))
    ))));
    return more((v1$1) => $0(state2, more, lift12, $$throw2, (state2$1, a$1) => more((v2$1) => done(state2$1, $NonEmpty(a, a$1)))));
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
var between = (open2) => (close2) => (p) => (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => more((v2$1) => more((v1$1) => open2(
  state1,
  more,
  lift12,
  $$throw2,
  (state2, a) => more((v2$2) => more((v3) => p(
    state2,
    more,
    lift12,
    $$throw2,
    (state3, a$1) => more((v4) => more((v2$3) => more((v3$1) => close2(state3, more, lift12, $$throw2, (state3$1, a$2) => more((v4$1) => done(state3$1, a$1))))))
  )))
)))));

// output-es/Parsing.Expr/index.js
var $Assoc = (tag) => tag;
var $Operator = (tag, _1, _2) => ({ tag, _1, _2 });
var choice2 = /* @__PURE__ */ choice(foldableList);
var identity19 = (x) => x;
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
var rassocP = (x) => (rassocOp) => (prefixP) => (term) => (postfixP) => (state1, more, lift12, $$throw2, done) => more((v1) => rassocOp(
  state1,
  more,
  lift12,
  $$throw2,
  (state2, a) => more((v2) => more((v1$1) => more((v1$2) => more((v1$3) => prefixP(
    state2,
    more,
    lift12,
    $$throw2,
    (state2$1, a$1) => more((v2$1) => more((v1$4) => term(
      state2$1,
      more,
      lift12,
      $$throw2,
      (state2$2, a$2) => more((v2$2) => more((v1$5) => postfixP(
        state2$2,
        more,
        lift12,
        $$throw2,
        (state2$3, a$3) => more((v2$3) => {
          const $0 = a$3(a$1(a$2));
          return more((v2$4) => rassocP1($0)(rassocOp)(prefixP)(term)(postfixP)(state2$3, more, lift12, $$throw2, (state2$4, a$4) => more((v2$5) => done(state2$4, a(x)(a$4)))));
        })
      )))
    )))
  )))))
));
var nassocP = (x) => (nassocOp) => (prefixP) => (term) => (postfixP) => (state1, more, lift12, $$throw2, done) => more((v1) => nassocOp(
  state1,
  more,
  lift12,
  $$throw2,
  (state2, a) => more((v2) => more((v1$1) => more((v1$2) => prefixP(
    state2,
    more,
    lift12,
    $$throw2,
    (state2$1, a$1) => more((v2$1) => more((v1$3) => term(
      state2$1,
      more,
      lift12,
      $$throw2,
      (state2$2, a$2) => more((v2$2) => more((v1$4) => postfixP(
        state2$2,
        more,
        lift12,
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
var lassocP = (x) => (lassocOp) => (prefixP) => (term) => (postfixP) => (state1, more, lift12, $$throw2, done) => more((v1) => lassocOp(
  state1,
  more,
  lift12,
  $$throw2,
  (state2, a) => more((v2) => more((v1$1) => more((v1$2) => prefixP(
    state2,
    more,
    lift12,
    $$throw2,
    (state2$1, a$1) => more((v2$1) => more((v1$3) => term(
      state2$1,
      more,
      lift12,
      $$throw2,
      (state2$2, a$2) => more((v2$2) => more((v1$4) => postfixP(
        state2$2,
        more,
        lift12,
        $$throw2,
        (state2$3, a$3) => more((v2$3) => {
          const $0 = a$3(a$1(a$2));
          return more((v2$4) => lassocP1(a(x)($0))(lassocOp)(prefixP)(term)(postfixP)(state2$3, more, lift12, $$throw2, done));
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
  return (state1, more, lift12, $$throw2, done) => more((v1) => {
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
              return $32(v2$1, identity19);
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
              return $32(v2$1, identity19);
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
              return $42(v2$1, identity19);
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
              return $42(v2$1, identity19);
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
              return $52(v2$1, identity19);
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
              return $52(v2$1, identity19);
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
        lift12,
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
              lift12,
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
                    lift12,
                    (v4$2, $14) => {
                      const $15 = v4$2._3;
                      return more((v5$2) => {
                        if ($15) {
                          return $$throw2(v4$2, $14);
                        }
                        return $3(state2, more, lift12, $$throw2, done);
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
        lift12,
        $$throw2,
        (state2$1, a$1) => more((v2$1) => more((v1$3) => {
          const $12 = state2$1._1;
          const $22 = state2$1._2;
          return more((v3) => postfixOp(
            $ParseState($12, $22, false),
            more,
            lift12,
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
        lift12,
        (v4, $4) => {
          const $5 = v4._3;
          return more((v5) => {
            if ($5) {
              return $$throw2(v4, $4);
            }
            return $1(state1, identity19);
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

// output-es/Data.Unit/index.js
var showUnit = { show: (v) => "unit" };

// output-es/SExpr/index.js
var $DictEntry = (tag, _1, _2) => ({ tag, _1, _2 });
var $Expr2 = (tag, _1, _2, _3, _4, _5) => ({ tag, _1, _2, _3, _4, _5 });
var $ListRest = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
var $ListRestPattern = (tag, _1, _2) => ({ tag, _1, _2 });
var $Module = (_1) => ({ tag: "Module", _1 });
var $Pattern = (tag, _1, _2) => ({ tag, _1, _2 });
var $Qualifier = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
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
var fromFoldable9 = /* @__PURE__ */ fromFoldable2(foldableArray);
var fromFoldable16 = /* @__PURE__ */ fromFoldable2(foldableNonEmptyList);
var fromFoldable23 = /* @__PURE__ */ fromFoldable2(foldableList);
var monadErrorExceptT2 = /* @__PURE__ */ monadErrorExceptT(monadIdentity);
var PListEnd = /* @__PURE__ */ $ListRestPattern("PListEnd");
var PListNext = (value0) => (value1) => $ListRestPattern("PListNext", value0, value1);
var PConstr = (value0) => (value1) => $Pattern("PConstr", value0, value1);
var PListEmpty = /* @__PURE__ */ $Pattern("PListEmpty");
var PListNonEmpty = (value0) => (value1) => $Pattern("PListNonEmpty", value0, value1);
var Clause = (x) => x;
var Int = (value0) => (value1) => (value2) => $Expr2("Int", value0, value1, value2);
var Float = (value0) => (value1) => (value2) => $Expr2("Float", value0, value1, value2);
var Str = (value0) => (value1) => (value2) => $Expr2("Str", value0, value1, value2);
var Constr2 = (value0) => (value1) => (value2) => (value3) => $Expr2("Constr", value0, value1, value2, value3);
var Dictionary2 = (value0) => (value1) => (value2) => $Expr2("Dictionary", value0, value1, value2);
var Matrix2 = (value0) => (value1) => (value2) => (value3) => (value4) => $Expr2("Matrix", value0, value1, value2, value3, value4);
var Project2 = (value0) => (value1) => (value2) => $Expr2("Project", value0, value1, value2);
var DProject2 = (value0) => (value1) => (value2) => $Expr2("DProject", value0, value1, value2);
var MatchAs = (value0) => (value1) => $Expr2("MatchAs", value0, value1);
var IfElse = (value0) => (value1) => (value2) => $Expr2("IfElse", value0, value1, value2);
var ListNonEmpty = (value0) => (value1) => (value2) => (value3) => $Expr2("ListNonEmpty", value0, value1, value2, value3);
var ListEnum = (value0) => (value1) => $Expr2("ListEnum", value0, value1);
var ListComp = (value0) => (value1) => (value2) => (value3) => $Expr2("ListComp", value0, value1, value2, value3);
var Let2 = (value0) => (value1) => $Expr2("Let", value0, value1);
var LetRec2 = (value0) => (value1) => $Expr2("LetRec", value0, value1);
var VarKey = (value0) => (value1) => $DictEntry("VarKey", value0, value1);
var Next = (value0) => (value1) => (value2) => $ListRest("Next", value0, value1, value2);
var ListCompGen = (value0) => (value1) => (value2) => $Qualifier("ListCompGen", value0, value1, value2);
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
          return $Expr2("Int", x._1._1._1._1, x._1._1._1._2._1, x._1._1._1._2._2);
        }
        if (x._1._1.tag === "Inr") {
          if (x._1._1._1.tag === "Inl") {
            return $Expr2("Float", x._1._1._1._1._1, x._1._1._1._1._2._1, x._1._1._1._1._2._2);
          }
          if (x._1._1._1.tag === "Inr") {
            if (x._1._1._1._1.tag === "Inl") {
              return $Expr2("Str", x._1._1._1._1._1._1, x._1._1._1._1._1._2._1, x._1._1._1._1._1._2._2);
            }
            if (x._1._1._1._1.tag === "Inr") {
              if (x._1._1._1._1._1.tag === "Inl") {
                return $Expr2("Constr", x._1._1._1._1._1._1._1, x._1._1._1._1._1._1._2._1, x._1._1._1._1._1._1._2._2._1, x._1._1._1._1._1._1._2._2._2);
              }
              if (x._1._1._1._1._1.tag === "Inr") {
                if (x._1._1._1._1._1._1.tag === "Inl") {
                  return $Expr2("Dictionary", x._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._2._1, x._1._1._1._1._1._1._1._2._2);
                }
                if (x._1._1._1._1._1._1.tag === "Inr") {
                  if (x._1._1._1._1._1._1._1.tag === "Inl") {
                    return $Expr2(
                      "Matrix",
                      x._1._1._1._1._1._1._1._1._1,
                      x._1._1._1._1._1._1._1._1._2._1,
                      x._1._1._1._1._1._1._1._1._2._2._1,
                      x._1._1._1._1._1._1._1._1._2._2._2._1,
                      x._1._1._1._1._1._1._1._1._2._2._2._2
                    );
                  }
                  if (x._1._1._1._1._1._1._1.tag === "Inr") {
                    if (x._1._1._1._1._1._1._1._1.tag === "Inl") {
                      return $Expr2("Lambda", x._1._1._1._1._1._1._1._1._1);
                    }
                    if (x._1._1._1._1._1._1._1._1.tag === "Inr") {
                      if (x._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                        return $Expr2("Project", x._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._2._1, x._1._1._1._1._1._1._1._1._1._1._2._2);
                      }
                      if (x._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                        if (x._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                          return $Expr2("DProject", x._1._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._1._2._1, x._1._1._1._1._1._1._1._1._1._1._1._2._2);
                        }
                        if (x._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                          if (x._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                            return $Expr2("App", x._1._1._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._1._1._2._1, x._1._1._1._1._1._1._1._1._1._1._1._1._2._2);
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
                                    return $Expr2("ListEmpty", x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2);
                                  }
                                  if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                    if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                      return $Expr2(
                                        "ListNonEmpty",
                                        x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                        x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._1,
                                        x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._2._1,
                                        x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._2._2
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
                                            x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._2._1,
                                            x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._2._2
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
      return $Sum(
        "Inr",
        $Sum("Inr", $Sum("Inl", $Product(x._1, $Product(x._2, x._3))))
      );
    }
    if (x.tag === "Float") {
      return $Sum(
        "Inr",
        $Sum("Inr", $Sum("Inr", $Sum("Inl", $Product(x._1, $Product(x._2, x._3)))))
      );
    }
    if (x.tag === "Str") {
      return $Sum(
        "Inr",
        $Sum(
          "Inr",
          $Sum("Inr", $Sum("Inr", $Sum("Inl", $Product(x._1, $Product(x._2, x._3)))))
        )
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
              $Sum(
                "Inr",
                $Sum("Inl", $Product(x._1, $Product(x._2, $Product(x._3, x._4))))
              )
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
            $Sum(
              "Inr",
              $Sum(
                "Inr",
                $Sum("Inr", $Sum("Inl", $Product(x._1, $Product(x._2, x._3))))
              )
            )
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
                    $Sum(
                      "Inl",
                      $Product(x._1, $Product(x._2, $Product(x._3, $Product(x._4, x._5))))
                    )
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
                                    $Sum(
                                      "Inr",
                                      $Sum("Inl", $Product(x._1, $Product(x._2, $Product(x._3, x._4))))
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
                                        $Sum(
                                          "Inr",
                                          $Sum(
                                            "Inl",
                                            $Product(x._1, $Product(x._2, $Product(x._3, x._4)))
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
    const $1 = genericShowConstructor(genericShowArgsProduct((() => {
      const $12 = showDocOpt(showExpr(dictShow));
      return { genericShowArgs: (v) => [$12.show(v)] };
    })())(genericShowArgsProduct({ genericShowArgs: (v) => [showPattern1.show(v)] })((() => {
      const $12 = showExpr(dictShow);
      return { genericShowArgs: (v) => [$12.show(v)] };
    })())))(ListCompGenIsSymbol);
    const $2 = genericShowConstructor((() => {
      const $22 = showVarDef(dictShow);
      return { genericShowArgs: (v) => [$22.show(v)] };
    })())(ListCompDeclIsSymbol);
    if (c.tag === "ListCompGuard") {
      return $0["genericShow'"](c._1);
    }
    if (c.tag === "ListCompGen") {
      return $1["genericShow'"]($Product(c._1, $Product(c._2, c._3)));
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
  const genericShowArgsProduct3 = genericShowArgsProduct({ genericShowArgs: (v) => [dictShow.show(v)] });
  return {
    show: (c) => genericShowSum4(genericShowSum5((() => {
      const $0 = genericShowConstructor(genericShowArgsProduct3(genericShowArgsProduct((() => {
        const $02 = showDocOpt(showExpr(dictShow));
        return { genericShowArgs: (v) => [$02.show(v)] };
      })())(genericShowArgsArgument1)))(IntIsSymbol);
      const $1 = genericShowConstructor(genericShowArgsProduct3(genericShowArgsProduct((() => {
        const $12 = showDocOpt(showExpr(dictShow));
        return { genericShowArgs: (v) => [$12.show(v)] };
      })())(genericShowArgsArgument2)))(FloatIsSymbol);
      const $2 = (() => {
        const $22 = genericShowConstructor(genericShowArgsProduct3(genericShowArgsProduct((() => {
          const $23 = showDocOpt(showExpr(dictShow));
          return { genericShowArgs: (v) => [$23.show(v)] };
        })())(genericShowArgsArgument)))(StrIsSymbol);
        const $3 = (() => {
          const $32 = genericShowConstructor(genericShowArgsProduct3(genericShowArgsProduct((() => {
            const $33 = showDocOpt(showExpr(dictShow));
            return { genericShowArgs: (v) => [$33.show(v)] };
          })())(genericShowArgsProduct(genericShowArgsArgument)((() => {
            const $33 = showList(showExpr(dictShow));
            return { genericShowArgs: (v) => [$33.show(v)] };
          })()))))(ConstrIsSymbol);
          const $4 = (() => {
            const $42 = genericShowConstructor(genericShowArgsProduct3(genericShowArgsProduct((() => {
              const $43 = showDocOpt(showExpr(dictShow));
              return { genericShowArgs: (v) => [$43.show(v)] };
            })())((() => {
              const $43 = showList((() => {
                const $44 = showDictEntry(dictShow);
                const $52 = showExpr(dictShow);
                return { show: (v) => "(Tuple " + $44.show(v._1) + " " + $52.show(v._2) + ")" };
              })());
              return { genericShowArgs: (v) => [$43.show(v)] };
            })())))(DictionaryIsSymbol);
            const $5 = (() => {
              const $52 = genericShowConstructor(genericShowArgsProduct3(genericShowArgsProduct((() => {
                const $53 = showDocOpt(showExpr(dictShow));
                return { genericShowArgs: (v) => [$53.show(v)] };
              })())(genericShowArgsProduct((() => {
                const $53 = showExpr(dictShow);
                return { genericShowArgs: (v) => [$53.show(v)] };
              })())(genericShowArgsProduct1((() => {
                const $53 = showExpr(dictShow);
                return { genericShowArgs: (v) => [$53.show(v)] };
              })())))))(MatrixIsSymbol);
              const $6 = (() => {
                const $62 = genericShowConstructor((() => {
                  const $63 = showClauses(dictShow);
                  return { genericShowArgs: (v) => [$63.show(v)] };
                })())(LambdaIsSymbol);
                const $7 = (() => {
                  const $72 = genericShowConstructor(genericShowArgsProduct((() => {
                    const $73 = showDocOpt(showExpr(dictShow));
                    return { genericShowArgs: (v) => [$73.show(v)] };
                  })())(genericShowArgsProduct((() => {
                    const $73 = showExpr(dictShow);
                    return { genericShowArgs: (v) => [$73.show(v)] };
                  })())(genericShowArgsArgument)))(ProjectIsSymbol);
                  const $8 = (() => {
                    const $82 = genericShowConstructor(genericShowArgsProduct((() => {
                      const $83 = showDocOpt(showExpr(dictShow));
                      return { genericShowArgs: (v) => [$83.show(v)] };
                    })())(genericShowArgsProduct((() => {
                      const $83 = showExpr(dictShow);
                      return { genericShowArgs: (v) => [$83.show(v)] };
                    })())((() => {
                      const $83 = showExpr(dictShow);
                      return { genericShowArgs: (v) => [$83.show(v)] };
                    })())))(DProjectIsSymbol);
                    const $9 = (() => {
                      const $92 = genericShowConstructor(genericShowArgsProduct((() => {
                        const $93 = showDocOpt(showExpr(dictShow));
                        return { genericShowArgs: (v) => [$93.show(v)] };
                      })())(genericShowArgsProduct((() => {
                        const $93 = showExpr(dictShow);
                        return { genericShowArgs: (v) => [$93.show(v)] };
                      })())((() => {
                        const $93 = showExpr(dictShow);
                        return { genericShowArgs: (v) => [$93.show(v)] };
                      })())))(AppIsSymbol);
                      const $10 = (() => {
                        const $102 = genericShowConstructor(genericShowArgsProduct((() => {
                          const $103 = showExpr(dictShow);
                          return { genericShowArgs: (v) => [$103.show(v)] };
                        })())(genericShowArgsProduct(genericShowArgsArgument)((() => {
                          const $103 = showExpr(dictShow);
                          return { genericShowArgs: (v) => [$103.show(v)] };
                        })())))(BinaryAppIsSymbol);
                        const $11 = (() => {
                          const $112 = genericShowConstructor(genericShowArgsProduct((() => {
                            const $113 = showExpr(dictShow);
                            return { genericShowArgs: (v) => [$113.show(v)] };
                          })())((() => {
                            const $113 = showNonEmptyList(showTuple1(showExpr(dictShow)));
                            return { genericShowArgs: (v) => [$113.show(v)] };
                          })()))(MatchAsIsSymbol);
                          const $12 = (() => {
                            const $122 = genericShowConstructor(genericShowArgsProduct((() => {
                              const $123 = showExpr(dictShow);
                              return { genericShowArgs: (v) => [$123.show(v)] };
                            })())(genericShowArgsProduct((() => {
                              const $123 = showExpr(dictShow);
                              return { genericShowArgs: (v) => [$123.show(v)] };
                            })())((() => {
                              const $123 = showExpr(dictShow);
                              return { genericShowArgs: (v) => [$123.show(v)] };
                            })())))(IfElseIsSymbol);
                            const $13 = (() => {
                              const $132 = genericShowConstructor(genericShowArgsProduct3((() => {
                                const $133 = showDocOpt(showExpr(dictShow));
                                return { genericShowArgs: (v) => [$133.show(v)] };
                              })()))(ListEmptyIsSymbol);
                              const $14 = (() => {
                                const $142 = genericShowConstructor(genericShowArgsProduct3(genericShowArgsProduct((() => {
                                  const $143 = showDocOpt(showExpr(dictShow));
                                  return { genericShowArgs: (v) => [$143.show(v)] };
                                })())(genericShowArgsProduct((() => {
                                  const $143 = showExpr(dictShow);
                                  return { genericShowArgs: (v) => [$143.show(v)] };
                                })())((() => {
                                  const $143 = showListRest(dictShow);
                                  return { genericShowArgs: (v) => [$143.show(v)] };
                                })()))))(ListNonEmptyIsSymbol);
                                const $15 = (() => {
                                  const $152 = genericShowConstructor(genericShowArgsProduct((() => {
                                    const $153 = showExpr(dictShow);
                                    return { genericShowArgs: (v) => [$153.show(v)] };
                                  })())((() => {
                                    const $153 = showExpr(dictShow);
                                    return { genericShowArgs: (v) => [$153.show(v)] };
                                  })()))(ListEnumIsSymbol);
                                  const $16 = (() => {
                                    const $162 = genericShowConstructor(genericShowArgsProduct3(genericShowArgsProduct((() => {
                                      const $163 = showDocOpt(showExpr(dictShow));
                                      return { genericShowArgs: (v) => [$163.show(v)] };
                                    })())(genericShowArgsProduct((() => {
                                      const $163 = showExpr(dictShow);
                                      return { genericShowArgs: (v) => [$163.show(v)] };
                                    })())((() => {
                                      const $163 = showList(showQualifier(dictShow));
                                      return { genericShowArgs: (v) => [$163.show(v)] };
                                    })()))))(ListCompIsSymbol);
                                    const $17 = (() => {
                                      const $172 = genericShowConstructor(genericShowArgsProduct((() => {
                                        const $173 = showNonEmptyList(showVarDef(dictShow));
                                        return { genericShowArgs: (v) => [$173.show(v)] };
                                      })())((() => {
                                        const $173 = showExpr(dictShow);
                                        return { genericShowArgs: (v) => [$173.show(v)] };
                                      })()))(LetIsSymbol);
                                      const $18 = (() => {
                                        const $182 = genericShowConstructor(genericShowArgsProduct((() => {
                                          const $183 = showNonEmptyList(showTuple(showClause(dictShow)));
                                          return { genericShowArgs: (v) => [$183.show(v)] };
                                        })())((() => {
                                          const $183 = showExpr(dictShow);
                                          return { genericShowArgs: (v) => [$183.show(v)] };
                                        })()))(LetRecIsSymbol);
                                        return {
                                          "genericShow'": (v) => {
                                            if (v.tag === "Inl") {
                                              return $172["genericShow'"](v._1);
                                            }
                                            if (v.tag === "Inr") {
                                              return $182["genericShow'"](v._1);
                                            }
                                            fail();
                                          }
                                        };
                                      })();
                                      return {
                                        "genericShow'": (v) => {
                                          if (v.tag === "Inl") {
                                            return $162["genericShow'"](v._1);
                                          }
                                          if (v.tag === "Inr") {
                                            return $18["genericShow'"](v._1);
                                          }
                                          fail();
                                        }
                                      };
                                    })();
                                    return {
                                      "genericShow'": (v) => {
                                        if (v.tag === "Inl") {
                                          return $152["genericShow'"](v._1);
                                        }
                                        if (v.tag === "Inr") {
                                          return $17["genericShow'"](v._1);
                                        }
                                        fail();
                                      }
                                    };
                                  })();
                                  return {
                                    "genericShow'": (v) => {
                                      if (v.tag === "Inl") {
                                        return $142["genericShow'"](v._1);
                                      }
                                      if (v.tag === "Inr") {
                                        return $16["genericShow'"](v._1);
                                      }
                                      fail();
                                    }
                                  };
                                })();
                                return {
                                  "genericShow'": (v) => {
                                    if (v.tag === "Inl") {
                                      return $132["genericShow'"](v._1);
                                    }
                                    if (v.tag === "Inr") {
                                      return $15["genericShow'"](v._1);
                                    }
                                    fail();
                                  }
                                };
                              })();
                              return {
                                "genericShow'": (v) => {
                                  if (v.tag === "Inl") {
                                    return $122["genericShow'"](v._1);
                                  }
                                  if (v.tag === "Inr") {
                                    return $14["genericShow'"](v._1);
                                  }
                                  fail();
                                }
                              };
                            })();
                            return {
                              "genericShow'": (v) => {
                                if (v.tag === "Inl") {
                                  return $112["genericShow'"](v._1);
                                }
                                if (v.tag === "Inr") {
                                  return $13["genericShow'"](v._1);
                                }
                                fail();
                              }
                            };
                          })();
                          return {
                            "genericShow'": (v) => {
                              if (v.tag === "Inl") {
                                return $102["genericShow'"](v._1);
                              }
                              if (v.tag === "Inr") {
                                return $12["genericShow'"](v._1);
                              }
                              fail();
                            }
                          };
                        })();
                        return {
                          "genericShow'": (v) => {
                            if (v.tag === "Inl") {
                              return $92["genericShow'"](v._1);
                            }
                            if (v.tag === "Inr") {
                              return $11["genericShow'"](v._1);
                            }
                            fail();
                          }
                        };
                      })();
                      return {
                        "genericShow'": (v) => {
                          if (v.tag === "Inl") {
                            return $82["genericShow'"](v._1);
                          }
                          if (v.tag === "Inr") {
                            return $10["genericShow'"](v._1);
                          }
                          fail();
                        }
                      };
                    })();
                    return {
                      "genericShow'": (v) => {
                        if (v.tag === "Inl") {
                          return $72["genericShow'"](v._1);
                        }
                        if (v.tag === "Inr") {
                          return $9["genericShow'"](v._1);
                        }
                        fail();
                      }
                    };
                  })();
                  return {
                    "genericShow'": (v) => {
                      if (v.tag === "Inl") {
                        return $62["genericShow'"](v._1);
                      }
                      if (v.tag === "Inr") {
                        return $8["genericShow'"](v._1);
                      }
                      fail();
                    }
                  };
                })();
                return {
                  "genericShow'": (v) => {
                    if (v.tag === "Inl") {
                      return $52["genericShow'"](v._1);
                    }
                    if (v.tag === "Inr") {
                      return $7["genericShow'"](v._1);
                    }
                    fail();
                  }
                };
              })();
              return {
                "genericShow'": (v) => {
                  if (v.tag === "Inl") {
                    return $42["genericShow'"](v._1);
                  }
                  if (v.tag === "Inr") {
                    return $6["genericShow'"](v._1);
                  }
                  fail();
                }
              };
            })();
            return {
              "genericShow'": (v) => {
                if (v.tag === "Inl") {
                  return $32["genericShow'"](v._1);
                }
                if (v.tag === "Inr") {
                  return $5["genericShow'"](v._1);
                }
                fail();
              }
            };
          })();
          return {
            "genericShow'": (v) => {
              if (v.tag === "Inl") {
                return $22["genericShow'"](v._1);
              }
              if (v.tag === "Inr") {
                return $4["genericShow'"](v._1);
              }
              fail();
            }
          };
        })();
        return {
          "genericShow'": (v) => {
            if (v.tag === "Inl") {
              return $1["genericShow'"](v._1);
            }
            if (v.tag === "Inr") {
              return $3["genericShow'"](v._1);
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
      return $Qualifier("ListCompGen", functorDocOpt(functorExpr2).map(f)(m._1), m._2, functorExpr2.map(f)(m._3));
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
      return $Expr2("Int", f(m._1), functorDocOpt(functorExpr2).map(f)(m._2), m._3);
    }
    if (m.tag === "Float") {
      return $Expr2("Float", f(m._1), functorDocOpt(functorExpr2).map(f)(m._2), m._3);
    }
    if (m.tag === "Str") {
      return $Expr2("Str", f(m._1), functorDocOpt(functorExpr2).map(f)(m._2), m._3);
    }
    if (m.tag === "Constr") {
      return $Expr2("Constr", f(m._1), functorDocOpt(functorExpr2).map(f)(m._2), m._3, listMap(functorExpr2.map(f))(m._4));
    }
    if (m.tag === "Dictionary") {
      return $Expr2(
        "Dictionary",
        f(m._1),
        functorDocOpt(functorExpr2).map(f)(m._2),
        listMap((() => {
          const $0 = functorDictEntry.map(f);
          const $1 = functorExpr2.map(f);
          return (v) => $Tuple($0(v._1), $1(v._2));
        })())(m._3)
      );
    }
    if (m.tag === "Matrix") {
      return $Expr2("Matrix", f(m._1), functorDocOpt(functorExpr2).map(f)(m._2), functorExpr2.map(f)(m._3), m._4, functorExpr2.map(f)(m._5));
    }
    if (m.tag === "Lambda") {
      return $Expr2("Lambda", functorClauses.map(f)(m._1));
    }
    if (m.tag === "Project") {
      return $Expr2("Project", functorDocOpt(functorExpr2).map(f)(m._1), functorExpr2.map(f)(m._2), m._3);
    }
    if (m.tag === "DProject") {
      return $Expr2("DProject", functorDocOpt(functorExpr2).map(f)(m._1), functorExpr2.map(f)(m._2), functorExpr2.map(f)(m._3));
    }
    if (m.tag === "App") {
      return $Expr2("App", functorDocOpt(functorExpr2).map(f)(m._1), functorExpr2.map(f)(m._2), functorExpr2.map(f)(m._3));
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
      return $Expr2("ListEmpty", f(m._1), functorDocOpt(functorExpr2).map(f)(m._2));
    }
    if (m.tag === "ListNonEmpty") {
      return $Expr2("ListNonEmpty", f(m._1), functorDocOpt(functorExpr2).map(f)(m._2), functorExpr2.map(f)(m._3), functorListRest.map(f)(m._4));
    }
    if (m.tag === "ListEnum") {
      return $Expr2("ListEnum", functorExpr2.map(f)(m._1), functorExpr2.map(f)(m._2));
    }
    if (m.tag === "ListComp") {
      return $Expr2("ListComp", f(m._1), functorDocOpt(functorExpr2).map(f)(m._2), functorExpr2.map(f)(m._3), listMap(functorQualifier.map(f))(m._4));
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
              zipWith2(Tuple)(v)(listMap((v2) => {
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
          drop3((() => {
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
        $Pattern("PConstr", c$p, replicate2(unfoldableList)(defined(arity(monadThrowExceptT2)(c$p)))($Pattern("PVar", "_")))
      ))(difference3(toUnfoldable7(fromFoldable12(mapObjectString.keys(defined(dataTypeForCtr.dataTypeFor(monadThrowExceptT2)(v._1._1))._2))))($List(
        "Cons",
        v._1._1,
        Nil
      )));
    }
    if (v._1.tag === "PListEmpty") {
      return $List(
        "Cons",
        $Either("Left", $Pattern("PConstr", ":", replicate2(unfoldableList)(2)($Pattern("PVar", "_")))),
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
var elimBool = (\u03BA) => (\u03BA$p) => $Elim("ElimConstr", fromFoldable9([$Tuple("True", \u03BA), $Tuple("False", \u03BA$p)]));
var econs = (\u03B1) => (doc) => (e) => (e$p) => $Expr("Constr", \u03B1, doc, ":", $List("Cons", e, $List("Cons", e$p, Nil)));
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
    const $3 = (v$1) => $Tuple(take2($2)(v$1._1), $Tuple(drop3($2)(v$1._1), v$1._2));
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
                  zipWith2(Tuple)(listMap(fst)($42))(listMap((v2) => {
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
        $Expr2("ListEmpty", \u03B1, None)
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
        return Monad0.Applicative0().pure($Expr("Constr", v._1, None, "Nil", Nil));
      }
      if (v.tag === "Next") {
        return Apply0.apply(Apply0.Functor0().map(econs(v._1)(None))(desugarableExprExpr.desug(dictMonadError)(dictBoundedLattice)(v._2)))(desugarableListRestExpr.desug(dictMonadError)(dictBoundedLattice)(v._3));
      }
      fail();
    };
  },
  desugBwd: (dictBoundedJoinSemilattice) => (v) => (v1) => {
    if (v.tag === "Constr") {
      if (v1.tag === "End") {
        return $ListRest("End", v._1);
      }
      if (v._4.tag === "Cons" && v._4._2.tag === "Cons" && v._4._2._2.tag === "Nil" && v1.tag === "Next") {
        return $ListRest(
          "Next",
          v._1,
          desugarableExprExpr.desugBwd(dictBoundedJoinSemilattice)(v._4._1)(v1._2),
          desugarableListRestExpr.desugBwd(dictBoundedJoinSemilattice)(v._4._2._1)(v1._3)
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
      return dictMonadError.MonadThrow0().Monad0().Applicative0().pure($Expr("Str", v._1, None, v._2));
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
      $Tuple($NonEmpty(v._1, Nil), $Expr2("Dictionary", top, None, Nil)),
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
      const $0 = wrappedOperation("groupBy")(groupBy2((x) => (y) => x._1 === y._1))(xcs);
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
  return bindNonEmptyList.bind(go(wrappedOperation("groupBy")(groupBy2((x) => (y) => x._1 === y._1))(xcs)))(identity2);
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
      return Functor0.map((f) => f($Expr("Constr", v._1, None, "Nil", Nil)))(Functor0.map(econs(v._1)(None))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2._2)));
    }
    if (v._2._1.tag === "Cons") {
      if (v._2._1._1.tag === "ListCompGuard") {
        const $0 = v._2._1._1._1;
        const $1 = v._1;
        return Bind1.bind(listCompFwd(dictMonadError)(dictBoundedLattice)($Tuple($1, $Tuple(v._2._1._2, v._2._2))))((e) => Functor0.map(App2(None)($Expr(
          "Lambda",
          $1,
          $Elim(
            "ElimConstr",
            fromFoldable9([
              $Tuple("True", $Cont("ContExpr", e)),
              $Tuple("False", $Cont("ContExpr", $Expr("Constr", $1, None, "Nil", Nil)))
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
            $Tuple(Nil, $Expr2("ListComp", $1, None, v._2._2, v._2._1._2))
          ),
          Nil
        )))((\u03C3) => Functor0.map(App2(None)($Expr(
          "Lambda",
          $1,
          \u03C3.tag === "ContElim" ? \u03C3._1 : throwException(error("Eliminator expected"))()
        )))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($0)));
      }
      if (v._2._1._1.tag === "ListCompGen") {
        const $0 = v._2._1._1._3;
        const $1 = v._1;
        const ks = orElseFwd($1)($Tuple(
          $List("Cons", $Either("Left", v._2._1._1._2), Nil),
          $Expr2("ListComp", $1, None, v._2._2, v._2._1._2)
        ));
        return Bind1.bind(desugComment(dictBoundedLattice)(dictMonadError)(v._2._1._1._1))((edoc) => Bind1.bind(clausesStateFwd(dictBoundedLattice)(dictMonadError)($List(
          "Cons",
          $Tuple(ks._1._1, $Tuple(Nil, ks._1._2)),
          listMap((m) => $Tuple(m._1, $Tuple(Nil, m._2)))(ks._2)
        )))((\u03C3) => Functor0.map(App2(edoc)($Expr(
          "App",
          None,
          $Expr("Var", "concatMap"),
          $Expr("Lambda", $1, \u03C3.tag === "ContElim" ? \u03C3._1 : throwException(error("Eliminator expected"))())
        )))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($0))));
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
          $Tuple(Nil, $Expr2("ListComp", void 0, None, s0$p, qs))
        ),
        Nil
      ));
      if ($12.tag === "Cons" && $12._1._1.tag === "Cons" && $12._1._1._1.tag === "Left" && $12._1._1._2.tag === "Nil" && $12._1._2._1.tag === "Nil" && $12._1._2._2.tag === "ListComp" && $12._1._2._2._2.tag === "None" && $12._2.tag === "Nil") {
        return $Tuple(
          $0.join($12._1._2._2._1)(\u03B1$p),
          $Tuple($List("Cons", $Qualifier("ListCompDecl", $VarDef2(p, exprBwd(dictBoundedJoinSemilattice)(e)(s0))), $12._1._2._2._4), $12._1._2._2._3)
        );
      }
      fail();
    };
    if (v.tag === "Constr") {
      if (v._4.tag === "Cons" && v._4._2.tag === "Cons" && v._4._2._1.tag === "Constr" && v._4._2._1._4.tag === "Nil" && v._4._2._2.tag === "Nil" && v1._1.tag === "Nil" && v._3 === ":" && v._4._2._1._3 === "Nil") {
        return $Tuple($0.join(v._4._2._1._1)(v._1), $Tuple(Nil, exprBwd(dictBoundedJoinSemilattice)(v._4._1)(v1._2)));
      }
      return throwException(error("absurd"))();
    }
    if (v.tag === "App" && v1._1.tag === "Cons") {
      if (v._2.tag === "Lambda") {
        if (v._2._2.tag === "ElimConstr" && v1._1._1.tag === "ListCompGuard") {
          const $2 = listCompBwd(dictBoundedJoinSemilattice)((() => {
            const $22 = $$get(showString)(mapDictString)("True")(v._2._2._1);
            if ($22.tag === "ContExpr") {
              return $22._1;
            }
            return throwException(error("Expression expected"))();
          })())($Tuple(v1._1._2, v1._2));
          const $3 = $$get(showString)(mapDictString)("False")(v._2._2._1);
          const $4 = $3.tag === "ContExpr" ? $3._1 : throwException(error("Expression expected"))();
          if ($4.tag === "Constr" && $4._4.tag === "Nil" && $4._3 === "Nil") {
            return $Tuple(
              $0.join($0.join($2._1)(v._2._1))($4._1),
              $Tuple($List("Cons", $Qualifier("ListCompGuard", exprBwd(dictBoundedJoinSemilattice)(v._3)(v1._1._1._1)), $2._2._1), $2._2._2)
            );
          }
          fail();
        }
        if (v1._1._1.tag === "ListCompDecl") {
          return $1(v._3, v1._1._1._1._1, v1._1._2, v1._1._1._1._2, v1._2, v._2._1, v._2._2);
        }
        return throwException(error("absurd"))();
      }
      if (v._2.tag === "App" && v._2._2.tag === "Var" && v._2._2._1 === "concatMap" && v._2._3.tag === "Lambda" && v1._1._1.tag === "ListCompGen") {
        const $2 = orElseBwd1($Tuple(
          $List("Cons", $Either("Left", v1._1._1._2), Nil),
          $Expr2("ListComp", void 0, None, v1._2, v1._1._2)
        ))((() => {
          const $22 = nonEmptyListNonEmptyList.nonEmpty(clausesStateBwd(dictBoundedJoinSemilattice)($Cont("ContElim", v._2._3._2))((() => {
            const $23 = orElseFwd()($Tuple(
              $List("Cons", $Either("Left", v1._1._1._2), Nil),
              $Expr2("ListComp", void 0, None, v1._2, v1._1._2)
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
            $0.join($0.join($2._2._1)(v._2._3._1))($2._1),
            $Tuple(
              $List("Cons", $Qualifier("ListCompGen", None, v1._1._1._2, exprBwd(dictBoundedJoinSemilattice)(v._3)(v1._1._1._3)), $2._2._4),
              $2._2._3
            )
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
        const $0 = v._3;
        const $1 = v._1;
        return Bind1.bind(desugComment(dictBoundedLattice)(dictMonadError)(v._2))((edoc) => Applicative0.pure($Expr("Int", $1, edoc, $0)));
      }
      if (v.tag === "Float") {
        const $0 = v._3;
        const $1 = v._1;
        return Bind1.bind(desugComment(dictBoundedLattice)(dictMonadError)(v._2))((edoc) => Applicative0.pure($Expr("Float", $1, edoc, $0)));
      }
      if (v.tag === "Str") {
        const $0 = v._3;
        const $1 = v._1;
        return Bind1.bind(desugComment(dictBoundedLattice)(dictMonadError)(v._2))((edoc) => Applicative0.pure($Expr("Str", $1, edoc, $0)));
      }
      if (v.tag === "Constr") {
        const $0 = v._3;
        const $1 = v._4;
        const $2 = v._1;
        return Bind1.bind(desugComment(dictBoundedLattice)(dictMonadError)(v._2))((edoc) => Functor0.map(Constr($2)(edoc)($0))(traverse2(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0()))($1)));
      }
      if (v.tag === "Dictionary") {
        const $0 = v._2;
        const $1 = v._1;
        const v1 = unzip(v._3);
        const $2 = v1._2;
        return Bind1.bind(traverse2(desugarableDictEntryExpr.desug(dictMonadError)(dictBoundedLattice))(v1._1))((ks$p) => Bind1.bind(traverse2(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0()))($2))((es) => Bind1.bind(desugComment(dictBoundedLattice)(dictMonadError)($0))((edoc) => Functor0.map(Dictionary($1)(edoc))(Applicative0.pure(zipWith2((k) => (v2) => $Pair(
          k,
          v2
        ))(ks$p)(es))))));
      }
      if (v.tag === "Matrix") {
        const $0 = v._3;
        const $1 = v._5;
        const $2 = v._4._1;
        const $3 = v._4._2;
        const $4 = v._1;
        return Bind1.bind(desugComment(dictBoundedLattice)(dictMonadError)(v._2))((edoc) => Apply0.apply(Functor0.map((f) => f($Tuple($2, $3)))(Functor0.map(Matrix($4)(edoc))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($0))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($1)));
      }
      if (v.tag === "Lambda") {
        return Functor0.map(Lambda(top))(desugarableClausesElim.desug(dictMonadError)(dictBoundedLattice)(v._1));
      }
      if (v.tag === "Project") {
        const $0 = v._2;
        const $1 = v._3;
        return Bind1.bind(desugComment(dictBoundedLattice)(dictMonadError)(v._1))((edoc) => Functor0.map((f) => f($1))(Functor0.map(Project(edoc))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($0))));
      }
      if (v.tag === "DProject") {
        const $0 = v._2;
        const $1 = v._3;
        return Bind1.bind(desugComment(dictBoundedLattice)(dictMonadError)(v._1))((edoc) => Apply0.apply(Functor0.map(DProject(edoc))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($0)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($1)));
      }
      if (v.tag === "App") {
        const $0 = v._2;
        const $1 = v._3;
        return Bind1.bind(desugComment(dictBoundedLattice)(dictMonadError)(v._1))((edoc) => Apply0.apply(Functor0.map(App2(edoc))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($0)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($1)));
      }
      if (v.tag === "BinaryApp") {
        return Apply0.apply(Functor0.map(App2(None))(Functor0.map(App2(None)($Expr("Op", v._2)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._3));
      }
      if (v.tag === "MatchAs") {
        return Apply0.apply(Functor0.map(App2(None))(Functor0.map(Lambda(top))(desugarableClausesElim.desug(dictMonadError)(dictBoundedLattice)($NonEmpty(
          $Tuple($NonEmpty(v._2._1._1, Nil), v._2._1._2),
          listMap((x) => $Tuple($NonEmpty(x._1, Nil), x._2))(v._2._2)
        )))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1));
      }
      if (v.tag === "IfElse") {
        return Apply0.apply(Functor0.map(App2(None))(Functor0.map(Lambda(top))(Apply0.apply(Functor0.map(elimBool)(Functor0.map(ContExpr)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2))))(Functor0.map(ContExpr)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._3))))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1));
      }
      if (v.tag === "ListEmpty") {
        const $0 = v._1;
        return Bind1.bind(desugComment(dictBoundedLattice)(dictMonadError)(v._2))((edoc) => Applicative0.pure($Expr("Constr", $0, edoc, "Nil", Nil)));
      }
      if (v.tag === "ListNonEmpty") {
        const $0 = v._4;
        const $1 = v._3;
        const $2 = v._1;
        return Bind1.bind(desugComment(dictBoundedLattice)(dictMonadError)(v._2))((edoc) => Apply0.apply(Functor0.map(econs($2)(edoc))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($1)))(desugarableListRestExpr.desug(dictMonadError)(dictBoundedLattice)($0)));
      }
      if (v.tag === "ListEnum") {
        return Apply0.apply(Functor0.map(App2(None))(Functor0.map(App2(None)($Expr("Var", "enumFromTo")))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2));
      }
      if (v.tag === "ListComp") {
        if (v._4.tag === "Cons" && v._4._1.tag === "ListCompGen") {
          return listCompFwd(dictMonadError)(dictBoundedLattice)($Tuple(
            v._1,
            $Tuple($List("Cons", $Qualifier("ListCompGen", v._2, v._4._1._2, v._4._1._3), v._4._2), v._3)
          ));
        }
        return listCompFwd(dictMonadError)(dictBoundedLattice)($Tuple(v._1, $Tuple(v._4, v._3)));
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
    const $1 = (doc, doc$p, e, q, qs, s) => {
      const v2 = listCompBwd(dictBoundedJoinSemilattice)(e)($Tuple($List("Cons", q, qs), s));
      return $Expr2("ListComp", v2._1, desugCommentBwd(dictBoundedJoinSemilattice)(doc)(doc$p), v2._2._2, v2._2._1);
    };
    const $2 = (e, qs, s) => {
      const v2 = listCompBwd(dictBoundedJoinSemilattice)(e)($Tuple(qs, s));
      return $Expr2("ListComp", v2._1, None, v2._2._2, v2._2._1);
    };
    if (v.tag === "Var") {
      if (v1.tag === "Var") {
        return $Expr2("Var", v1._1);
      }
      if (v1.tag === "ListComp") {
        return $2(v, v1._4, v1._3);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Op") {
      if (v1.tag === "Op") {
        return $Expr2("Op", v1._1);
      }
      if (v1.tag === "ListComp") {
        return $2(v, v1._4, v1._3);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Int") {
      if (v1.tag === "Int") {
        return $Expr2("Int", v._1, desugCommentBwd(dictBoundedJoinSemilattice)(v._2)(v1._2), v1._3);
      }
      if (v1.tag === "ListComp") {
        return $2(v, v1._4, v1._3);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Float") {
      if (v1.tag === "Float") {
        return $Expr2("Float", v._1, desugCommentBwd(dictBoundedJoinSemilattice)(v._2)(v1._2), v1._3);
      }
      if (v1.tag === "ListComp") {
        return $2(v, v1._4, v1._3);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Str") {
      if (v1.tag === "Str") {
        return $Expr2("Str", v._1, desugCommentBwd(dictBoundedJoinSemilattice)(v._2)(v1._2), v1._3);
      }
      if (v1.tag === "ListComp") {
        return $2(v, v1._4, v1._3);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Constr") {
      if (v1.tag === "Constr") {
        return $Expr2(
          "Constr",
          v._1,
          desugCommentBwd(dictBoundedJoinSemilattice)(v._2)(v1._2),
          v1._3,
          listMap((() => {
            const $3 = exprBwd(dictBoundedJoinSemilattice);
            return (v$1) => $3(v$1._1)(v$1._2);
          })())(zipWith2(Tuple)(v._4)(v1._4))
        );
      }
      if (v._4.tag === "Nil") {
        if (v1.tag === "ListEmpty") {
          return $Expr2("ListEmpty", v._1, desugCommentBwd(dictBoundedJoinSemilattice)(v._2)(v1._2));
        }
        if (v1.tag === "ListComp") {
          return $2(v, v1._4, v1._3);
        }
        return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
      }
      if (v._4.tag === "Cons" && v._4._2.tag === "Cons" && v._4._2._2.tag === "Nil" && v1.tag === "ListNonEmpty") {
        return $Expr2(
          "ListNonEmpty",
          v._1,
          desugCommentBwd(dictBoundedJoinSemilattice)(v._2)(v1._2),
          exprBwd(dictBoundedJoinSemilattice)(v._4._1)(v1._3),
          desugarableListRestExpr.desugBwd(dictBoundedJoinSemilattice)(v._4._2._1)(v1._4)
        );
      }
      if (v1.tag === "ListComp") {
        return $2(v, v1._4, v1._3);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Dictionary") {
      if (v1.tag === "Dictionary") {
        return $Expr2(
          "Dictionary",
          v._1,
          desugCommentBwd(dictBoundedJoinSemilattice)(v._2)(v1._2),
          zipWith2((v2) => {
            const $3 = v2._1;
            const $4 = v2._2;
            return (v3) => $Tuple(desugarableDictEntryExpr.desugBwd(dictBoundedJoinSemilattice)($3)(v3._1), exprBwd(dictBoundedJoinSemilattice)($4)(v3._2));
          })(v._3)(v1._3)
        );
      }
      if (v1.tag === "ListComp") {
        return $2(v, v1._4, v1._3);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Matrix") {
      if (v1.tag === "Matrix") {
        return $Expr2(
          "Matrix",
          v._1,
          desugCommentBwd(dictBoundedJoinSemilattice)(v._2)(v1._2),
          exprBwd(dictBoundedJoinSemilattice)(v._3)(v1._3),
          $Tuple(v1._4._1, v1._4._2),
          exprBwd(dictBoundedJoinSemilattice)(v._5)(v1._5)
        );
      }
      if (v1.tag === "ListComp") {
        return $2(v, v1._4, v1._3);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Lambda") {
      if (v1.tag === "Lambda") {
        return $Expr2("Lambda", toClausesStateBwd(clausesStateBwd(dictBoundedJoinSemilattice)($Cont("ContElim", v._2))(toClausesStateFwd(v1._1))));
      }
      if (v1.tag === "ListComp") {
        return $2(v, v1._4, v1._3);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "Project") {
      if (v1.tag === "Project") {
        return $Expr2("Project", desugCommentBwd(dictBoundedJoinSemilattice)(v._1)(v1._1), exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2), v._3);
      }
      if (v1.tag === "ListComp") {
        return $2(v, v1._4, v1._3);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v.tag === "App") {
      if (v1.tag === "App") {
        return $Expr2(
          "App",
          desugCommentBwd(dictBoundedJoinSemilattice)(v._1)(v1._1),
          exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2),
          exprBwd(dictBoundedJoinSemilattice)(v._3)(v1._3)
        );
      }
      if (v._2.tag === "App") {
        if (v._2._2.tag === "Op") {
          if (v1.tag === "BinaryApp") {
            return $Expr2("BinaryApp", exprBwd(dictBoundedJoinSemilattice)(v._2._3)(v1._1), v1._2, exprBwd(dictBoundedJoinSemilattice)(v._3)(v1._3));
          }
          if (v1.tag === "ListComp") {
            if (v1._4.tag === "Cons" && v1._4._1.tag === "ListCompGen") {
              return $1(v._1, v1._2, v, v1._4._1, v1._4._2, v1._3);
            }
            return $2(v, v1._4, v1._3);
          }
          return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
        }
        if (v._2._2.tag === "Var" && v._2._2._1 === "enumFromTo" && v1.tag === "ListEnum") {
          return $Expr2("ListEnum", exprBwd(dictBoundedJoinSemilattice)(v._2._3)(v1._1), exprBwd(dictBoundedJoinSemilattice)(v._3)(v1._2));
        }
        if (v1.tag === "ListComp") {
          if (v1._4.tag === "Cons" && v1._4._1.tag === "ListCompGen") {
            return $1(v._1, v1._2, v, v1._4._1, v1._4._2, v1._3);
          }
          return $2(v, v1._4, v1._3);
        }
        return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
      }
      if (v._2.tag === "Lambda") {
        if (v1.tag === "MatchAs") {
          return $Expr2(
            "MatchAs",
            exprBwd(dictBoundedJoinSemilattice)(v._3)(v1._1),
            (() => {
              const $3 = toClausesStateBwd(clausesStateBwd(dictBoundedJoinSemilattice)($Cont("ContElim", v._2._2))(toClausesStateFwd($NonEmpty(
                $Tuple($NonEmpty(v1._2._1._1, Nil), v1._2._1._2),
                listMap((x) => $Tuple($NonEmpty(x._1, Nil), x._2))(v1._2._2)
              ))));
              return $NonEmpty($Tuple($3._1._1._1, $3._1._2), listMap((x) => $Tuple(x._1._1, x._2))($3._2));
            })()
          );
        }
        if (v._2._2.tag === "ElimConstr" && v1.tag === "IfElse") {
          return $Expr2(
            "IfElse",
            exprBwd(dictBoundedJoinSemilattice)(v._3)(v1._1),
            Object.hasOwn(v._2._2._1, "True") ? exprBwd(dictBoundedJoinSemilattice)((() => {
              const $3 = $$get(showString)(mapDictString)("True")(v._2._2._1);
              if ($3.tag === "ContExpr") {
                return $3._1;
              }
              return throwException(error("Expression expected"))();
            })())(v1._2) : $0(v1._2),
            Object.hasOwn(v._2._2._1, "False") ? exprBwd(dictBoundedJoinSemilattice)((() => {
              const $3 = $$get(showString)(mapDictString)("False")(v._2._2._1);
              if ($3.tag === "ContExpr") {
                return $3._1;
              }
              return throwException(error("Expression expected"))();
            })())(v1._3) : $0(v1._3)
          );
        }
      }
      if (v1.tag === "ListComp") {
        return $2(v, v1._4, v1._3);
      }
      return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
    }
    if (v1.tag === "ListComp") {
      return $2(v, v1._4, v1._3);
    }
    if (v.tag === "Let") {
      if (v1.tag === "Let") {
        const $3 = varDefsBwd(dictBoundedJoinSemilattice)($Expr("Let", v._1, v._2))($Tuple(v1._1, v1._2));
        return $Expr2("Let", $3._1, $3._2);
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
      return $Expr2(
        "DProject",
        desugCommentBwd(dictBoundedJoinSemilattice)(v._1)(v1._1),
        exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2),
        exprBwd(dictBoundedJoinSemilattice)(v._3)(v1._3)
      );
    }
    return throwException(error("ExprBwd failed, Right: " + show2(v1)))();
  };
};
var desugCommentBwd = (dictBoundedJoinSemilattice) => (v) => (v1) => {
  if (v.tag === "None") {
    if (v1.tag === "None") {
      return None;
    }
    if (v1.tag === "Doc") {
      return throwException(error("E Doc.None S Doc"))();
    }
    fail();
  }
  if (v.tag === "Doc") {
    if (v1.tag === "Doc") {
      return $DocOpt("Doc", commentBwd(dictBoundedJoinSemilattice)(v._1)(v1._1));
    }
    if (v1.tag === "None") {
      return throwException(error("E Doc S Doc.None"))();
    }
  }
  fail();
};
var desugComment = (dictBoundedLattice) => (dictMonadError) => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  return (v) => {
    if (v.tag === "None") {
      return Monad0.Applicative0().pure(None);
    }
    if (v.tag === "Doc") {
      return Monad0.Bind1().Apply0().Functor0().map(Doc)(commentFwd(dictBoundedLattice)(dictMonadError)(v._1));
    }
    fail();
  };
};
var commentFwd = (dictBoundedLattice) => (dictMonadError) => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  const Apply0 = Monad0.Bind1().Apply0();
  return (v) => {
    if (v.tag === "Cons") {
      return Apply0.apply(Apply0.Functor0().map(Cons)(commentElemFwd(dictBoundedLattice)(dictMonadError)(v._1)))(commentFwd(dictBoundedLattice)(dictMonadError)(v._2));
    }
    if (v.tag === "Nil") {
      return Monad0.Applicative0().pure(Nil);
    }
    fail();
  };
};
var commentElemFwd = (dictBoundedLattice) => {
  const JoinSemilattice0 = dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0();
  return (dictMonadError) => {
    const Monad0 = dictMonadError.MonadThrow0().Monad0();
    return (v) => {
      if (v.tag === "Token") {
        return Monad0.Applicative0().pure($DocCommentElem("Token", v._1));
      }
      if (v.tag === "Unquote") {
        return Monad0.Bind1().Apply0().Functor0().map(Unquote)(exprFwd(dictBoundedLattice)(dictMonadError)(JoinSemilattice0)(v._1));
      }
      fail();
    };
  };
};
var commentElemBwd = (dictBoundedJoinSemilattice) => (v) => (v1) => {
  if (v.tag === "Token") {
    if (v1.tag === "Token") {
      return $DocCommentElem("Token", v1._1);
    }
    return throwException(error("commentElemBwd mismatch"))();
  }
  if (v.tag === "Unquote" && v1.tag === "Unquote") {
    return $DocCommentElem("Unquote", exprBwd(dictBoundedJoinSemilattice)(v._1)(v1._1));
  }
  return throwException(error("commentElemBwd mismatch"))();
};
var commentBwd = (dictBoundedJoinSemilattice) => (v) => (v1) => {
  if (v.tag === "Cons") {
    if (v1.tag === "Cons") {
      return $List("Cons", commentElemBwd(dictBoundedJoinSemilattice)(v._1)(v1._1), commentBwd(dictBoundedJoinSemilattice)(v._2)(v1._2));
    }
    return throwException(error("commentBwd mismatch"))();
  }
  if (v.tag === "Nil" && v1.tag === "Nil") {
    return Nil;
  }
  return throwException(error("commentBwd mismatch"))();
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
    const sequence1 = traversableList.traverse(Applicative0)(identity5);
    const rtraverse1 = bitraversableTuple.bitraverse(Applicative0)(Applicative0.pure);
    return (ks) => {
      const $1 = (p) => Bind1.bind(popConstrFwd2(defined(dataTypeForCtr.dataTypeFor(monadThrowExceptT2)(definitely("clausesStateFwd ctrFor failed for: " + showPattern(p))(ctrFor(p)))))(ks))((kss) => $0.map((x) => $Cont(
        "ContElim",
        $Elim("ElimConstr", fromFoldable23(x))
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
  const $1 = (m, p) => popConstrBwd(mapMaybe2((v1) => {
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
    })(v._1))(identity2)));
  };
};

// output-es/Util.Pretty/index.js
var intercalate4 = (sep) => (xs) => foldlArray((v) => (v1) => {
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
  const v = uncons(xs);
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
  return slice(0)($0)(v.lines);
};
var indentedExpression = (v) => (v1) => zipWith(concatString)(replicate(slice(1)(v1.lines.length)(v1.lines).length)(foldlArray(concatString)("")(replicate(toCodePointArray(lastLine(v)).length)(" "))))(slice(1)(v1.lines.length)(v1.lines));
var beside = (v) => (v1) => ({ width: v.width + v1.width | 0, height: v.height + v1.height | 0, lines: [...allButLast(v), lastLine(v) + "" + firstLine(v1), ...indentedExpression(v)(v1)] });
var semigroupColumns = { append: (v) => (v1) => beside(v)(v1) };
var monoidColumns = { mempty: empty2, Semigroup0: () => semigroupColumns };

// output-es/Pretty/index.js
var $ExprType = (tag) => tag;
var hcat = /* @__PURE__ */ (() => foldableList.foldMap(monoidColumns)(unsafeCoerce))();
var hcat1 = /* @__PURE__ */ (() => foldableArray.foldMap(monoidColumns)(unsafeCoerce))();
var toUnfoldable8 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
var toUnfoldable13 = /* @__PURE__ */ toAscUnfoldable(unfoldableList);
var Simple = /* @__PURE__ */ $ExprType("Simple");
var Expression = /* @__PURE__ */ $ExprType("Expression");
var vert = (dictFoldable) => {
  const fromFoldable26 = dictFoldable.foldr(Cons)(Nil);
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
    return (x) => vert$p(fromFoldable26(x));
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
var prettyDocCommentElem = (dictPretty) => ({
  pretty: (v) => {
    if (v.tag === "Token") {
      return checkOneLine(split("\n")(" " + v._1));
    }
    if (v.tag === "Unquote") {
      return beside(beside(checkOneLine(split("\n")(" ${")))(dictPretty.pretty(v._1)))(checkOneLine(split("\n")(" }")));
    }
    fail();
  }
});
var prettyListDocCommentElem = (dictPretty) => {
  const $0 = prettyDocCommentElem(dictPretty);
  return {
    pretty: (v) => {
      if (v.tag === "Cons") {
        if (v._2.tag === "Nil") {
          return beside($0.pretty(v._1))(checkOneLine(split("\n")(' """')));
        }
        return beside($0.pretty(v._1))(prettyListDocCommentElem(dictPretty).pretty(v._2));
      }
      if (v.tag === "Nil") {
        return empty2;
      }
      fail();
    }
  };
};
var prettyDocOpt = (dictPretty) => ({
  pretty: (v) => {
    if (v.tag === "Doc") {
      return beside(checkOneLine(split("\n")(' """')))(prettyListDocCommentElem(dictPretty).pretty(v._1));
    }
    if (v.tag === "None") {
      return empty2;
    }
    fail();
  }
});
var pattRepPairs = /* @__PURE__ */ arrayMap((v) => $Tuple(v._1, v._2))(replacement);
var removeDocWS = (v) => ({
  width: v.width,
  height: v.height,
  lines: arrayMap((x) => foldlArray((curr) => (v$1) => replaceAll(v$1._1)(v$1._2)(curr))(drop2(length2(take3(1)(x)))(x))(pattRepPairs))(v.lines)
});
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
  const v = lookup2(ordString)(x)(opDefs);
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
    if (v._4.tag === "Nil") {
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
  if (contains(" ")(intercalate4("\n")(doc.lines))) {
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
      return beside(prettyDocOpt(prettyExpr(dictHighlightable)).pretty(v._2))(dictHighlightable.highlightIf(v._1)(checkOneLine(split("\n")(" " + showIntImpl(v._3)))));
    }
    if (v.tag === "Float") {
      return beside(prettyDocOpt(prettyExpr(dictHighlightable)).pretty(v._2))(dictHighlightable.highlightIf(v._1)(checkOneLine(split("\n")(" " + showNumberImpl(v._3)))));
    }
    if (v.tag === "Str") {
      return beside(prettyDocOpt(prettyExpr(dictHighlightable)).pretty(v._2))(dictHighlightable.highlightIf(v._1)(checkOneLine(split("\n")(" " + showStringImpl(v._3)))));
    }
    if (v.tag === "Dictionary") {
      return beside(prettyDocOpt(prettyExpr(dictHighlightable)).pretty(v._2))(dictHighlightable.highlightIf(v._1)(prettyRecordOrDict(prettyExpr(dictHighlightable))(checkOneLine(split("\n")(" :")))(keyBracks)(curlyBraces)(prettyExpr(dictHighlightable).pretty)(listMap(toTuple)(v._3))));
    }
    if (v.tag === "Constr") {
      return beside(prettyDocOpt(prettyExpr(dictHighlightable)).pretty(v._2))(dictHighlightable.highlightIf(v._1)(prettyConstr(prettyExpr(dictHighlightable))(v._3)(v._4)));
    }
    if (v.tag === "Matrix") {
      return beside(prettyDocOpt(prettyExpr(dictHighlightable)).pretty(v._2))(dictHighlightable.highlightIf(v._1)(prettyMatrix(dictHighlightable)(v._3)(v._4._1)(v._4._2)(v._5)));
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
      return beside(beside(beside(prettyDocOpt(prettyExpr(dictHighlightable)).pretty(v._1))(prettyExpr(dictHighlightable).pretty(v._2)))(checkOneLine(split("\n")(" ."))))(checkOneLine(split("\n")(" " + v._3)));
    }
    if (v.tag === "DProject") {
      return beside(beside(beside(beside(beside(prettyDocOpt(prettyExpr(dictHighlightable)).pretty(v._1))(prettyExpr(dictHighlightable).pretty(v._2)))(checkOneLine(split("\n")(" ."))))(checkOneLine(split("\n")(" ["))))(prettyExpr(dictHighlightable).pretty(v._3)))(checkOneLine(split("\n")(" ]")));
    }
    if (v.tag === "App") {
      return beside(prettyDocOpt(prettyExpr(dictHighlightable)).pretty(v._1))(hcat1([
        prettyExpr(dictHighlightable).pretty(v._2),
        prettyExpr(dictHighlightable).pretty(v._3)
      ]));
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
    return go(toUnfoldable13(\u03C1));
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
var prettyVal = (dictHighlightable) => ({ pretty: (v) => beside(prettyDocOpt(prettyVal(dictHighlightable)).pretty(v._2))(dictHighlightable.highlightIf(v._1)(prettyBaseVal(dictHighlightable).pretty(v._3))) });
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
    return brackets(go(toUnfoldable13(v)));
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
      ))(toUnfoldable13(v._1)));
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
          return atop(beside($0.highlightIf(v._1)(checkOneLine(split("\n")(" ,"))))($0.highlightIf(v._1)(curlyBraces(prettyDictEntries(dictAnn)(beside)(v._2._3)))))(prettyListRest(dictAnn).pretty(v._3));
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
          return beside(beside(beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._1._1))(prettyPattern.pretty(v._1._2)))(checkOneLine(split("\n")(" <-"))))(prettyExpr1(dictAnn).pretty(v._1._3));
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
var prettyFirstGroup = (dictAnn) => ({ pretty: (v) => prettyNonEmptyListNonEmpt(dictAnn).pretty(wrappedOperation("groupBy")(groupBy2((p) => (q) => p._1 === q._1))(v)) });
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
        return beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._2))($0.highlightIf(v._1)(checkOneLine(split("\n")(" " + showIntImpl(v._3)))));
      }
      if (v.tag === "Float") {
        return beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._2))($0.highlightIf(v._1)(checkOneLine(split("\n")(" " + showNumberImpl(v._3)))));
      }
      if (v.tag === "Str") {
        return beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._2))($0.highlightIf(v._1)(checkOneLine(split("\n")(' "' + v._3 + '"'))));
      }
      if (v.tag === "Constr") {
        return beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._2))($0.highlightIf(v._1)(prettyConstr(prettyExpr1(dictAnn))(v._3)(v._4)));
      }
      if (v.tag === "Dictionary") {
        return beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._2))($0.highlightIf(v._1)(curlyBraces(prettyDictEntries(dictAnn)(atop)(v._3))));
      }
      if (v.tag === "Matrix") {
        return beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._2))($0.highlightIf(v._1)(arrayBrackets(beside(beside(beside(beside(prettyExpr1(dictAnn).pretty(v._3))(checkOneLine(split("\n")(" |"))))(parentheses(beside(beside(checkOneLine(split("\n")(" " + v._4._1)))(checkOneLine(split("\n")(" ,"))))(checkOneLine(split("\n")(" " + v._4._2))))))(checkOneLine(split("\n")(" in"))))(prettyExpr1(dictAnn).pretty(v._5)))));
      }
      if (v.tag === "Lambda") {
        return parentheses(beside(checkOneLine(split("\n")(" fun")))(prettyClauses(dictAnn).pretty(v._1)));
      }
      if (v.tag === "Project") {
        return beside(beside(beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._1))(prettySimple(dictAnn)(v._2)))(checkOneLine(split("\n")(" ."))))(checkOneLine(split("\n")(" " + v._3)));
      }
      if (v.tag === "DProject") {
        return beside(beside(beside(beside(beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._1))(prettySimple(dictAnn)(v._2)))(checkOneLine(split("\n")(" ."))))(checkOneLine(split("\n")(" ["))))(prettySimple(dictAnn)(v._3)))(checkOneLine(split("\n")(" ]")));
      }
      if (v.tag === "App") {
        return beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._1))(prettyAppChain(dictAnn)($Expr2("App", v._1, v._2, v._3)));
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
        return beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._2))($0.highlightIf(v._1)(brackets(empty2)));
      }
      if (v.tag === "ListNonEmpty") {
        if (v._3.tag === "Dictionary") {
          return atop(beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._2))(beside($0.highlightIf(v._1)(checkOneLine(split("\n")(" ["))))($0.highlightIf(v._1)(curlyBraces(prettyDictEntries(dictAnn)(beside)(v._3._3))))))(prettyListRest(dictAnn).pretty(v._4));
        }
        return beside(beside(beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._2))($0.highlightIf(v._1)(checkOneLine(split("\n")(" [")))))(prettyExpr1(dictAnn).pretty(v._3)))(prettyListRest(dictAnn).pretty(v._4));
      }
      if (v.tag === "ListEnum") {
        return brackets(beside(beside(prettyExpr1(dictAnn).pretty(v._1))(checkOneLine(split("\n")(" .."))))(prettyExpr1(dictAnn).pretty(v._2)));
      }
      if (v.tag === "ListComp") {
        return beside(prettyDocOpt(prettyExpr1(dictAnn)).pretty(v._2))($0.highlightIf(v._1)(brackets(beside(beside(prettyExpr1(dictAnn).pretty(v._3))(checkOneLine(split("\n")(" |"))))(prettyListQualifier(dictAnn).pretty(v._4)))));
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
    return beside(prettyAppChain(dictAnn)(v._2))(prettySimple(dictAnn)(v._3));
  }
  return prettySimple(dictAnn)(v);
};

// output-es/Val/index.js
var $BaseVal = (tag, _1, _2) => ({ tag, _1, _2 });
var $EnvExpr = (_1, _2) => ({ tag: "EnvExpr", _1, _2 });
var $ForeignOp$p = (_1) => ({ tag: "ForeignOp'", _1 });
var $Fun = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
var $Val = (_1, _2, _3) => ({ tag: "Val", _1, _2, _3 });
var setSet3 = /* @__PURE__ */ setSet(ordDVertex$p);
var unions2 = /* @__PURE__ */ foldlArray(/* @__PURE__ */ union(ordDVertex$p))(Leaf2);
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
        go$a0 = unionWith(ordDVertex$p)($$const)(b)(v._1);
        go$a1 = v._2;
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go(Leaf2);
})();
var foldMap2 = /* @__PURE__ */ foldMap({ mempty: Leaf2, Semigroup0: () => ({ append: union(ordDVertex$p) }) });
var identity22 = (x) => x;
var boundedLattice = { BoundedJoinSemilattice0: () => boundedJoinSemilatticeUni, BoundedMeetSemilattice1: () => boundedMeetSemilatticeUni };
var setSet1 = /* @__PURE__ */ setSet(ordString);
var toUnfoldable14 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
var intersection2 = /* @__PURE__ */ intersection(ordString);
var fromFoldable10 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)()(m))(Leaf2))();
var MatrixRep = (x) => x;
var Val = (value0) => (value1) => (value2) => $Val(value0, value1, value2);
var Int2 = (value0) => $BaseVal("Int", value0);
var Float2 = (value0) => $BaseVal("Float", value0);
var Str2 = (value0) => $BaseVal("Str", value0);
var Dictionary3 = (value0) => $BaseVal("Dictionary", value0);
var DictRep = (x) => x;
var Env = (x) => x;
var typeNameVal = { typeName: (v) => "Val" };
var pack2 = (x) => (k) => k(typeNameVal)(x);
var typeNameMatrixDim = { typeName: (v) => "MatrixDim" };
var pack12 = (x) => (k) => k(typeNameMatrixDim)(x);
var typeNameDictKey = { typeName: (v) => "DictKey" };
var pack22 = (x) => (k) => k(typeNameDictKey)(x);
var verticesValVertex = {
  vertices: (v) => setSet3.union($$$Map("Two", Leaf2, $Tuple(v._1, pack2(v)), void 0, Leaf2))(verticesBaseValVertex.vertices(v._3))
};
var verticesMatrixRepVertex = {
  vertices: (v) => setSet3.union(unions2(concat(arrayMap(arrayMap(verticesValVertex.vertices))(v._1))))(setSet3.union($$$Map(
    "Two",
    Leaf2,
    $Tuple(v._2._1._2, pack12(v._2._1)),
    void 0,
    Leaf2
  ))($$$Map("Two", Leaf2, $Tuple(v._2._2._2, pack12(v._2._2)), void 0, Leaf2)))
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
    $Tuple(v1._1, pack22($Tuple(k, v1._1))),
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
var highlightableUnit = { highlightIf: (v) => identity22 };
var functorMatrixDim = { map: (f) => (m) => $Tuple(m._1, f(m._2)) };
var functorVal = { map: (f) => (m) => $Val(f(m._1), functorDocOpt(functorVal).map(f)(m._2), functorBaseVal.map(f)(m._3)) };
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
var functorDictRep = { map: (f) => (m) => _fmapObject(m, (v) => $Tuple(f(v._1), functorVal.map(f)(v._2))) };
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
var foldableMatrixDim = { foldl: (f) => (z) => (m) => f(z)(m._2), foldr: (f) => (z) => (m) => f(m._2)(z), foldMap: (dictMonoid) => (f) => (m) => f(m._2) };
var traversableMatrixDim = {
  traverse: (dictApplicative) => (f) => (m) => dictApplicative.Apply0().Functor0().map((v1) => v1)(traversableTuple.traverse(dictApplicative)(f)(m)),
  sequence: (dictApplicative) => (v) => traversableMatrixDim.traverse(dictApplicative)(identity22)(v),
  Functor0: () => functorMatrixDim,
  Foldable1: () => foldableMatrixDim
};
var foldableVal = {
  foldl: (f) => (z) => (m) => foldableBaseVal.foldl(f)(foldableDocOpt(foldableVal).foldl(f)(f(z)(m._1))(m._2))(m._3),
  foldr: (f) => (z) => (m) => f(m._1)(foldableDocOpt(foldableVal).foldr(f)(foldableBaseVal.foldr(f)(z)(m._3))(m._2)),
  foldMap: (dictMonoid) => {
    const $0 = dictMonoid.Semigroup0();
    return (f) => (m) => $0.append(f(m._1))($0.append(foldableDocOpt(foldableVal).foldMap(dictMonoid)(f)(m._2))(foldableBaseVal.foldMap(dictMonoid)(f)(m._3)));
  }
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
            go$a0 = foldableVal.foldl(f)(b)(v._1);
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
            go$a0 = foldableVal.foldl(f)(b)(v._1);
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
      return foldableList.foldr((b) => (a) => foldableVal.foldr(f)(a)(b))(z)(m._2);
    }
    if (m.tag === "PartialConstr") {
      return foldableList.foldr((b) => (a) => foldableVal.foldr(f)(a)(b))(z)(m._2);
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
  foldr: (f) => (z) => (m) => foldrArray((b) => (a) => foldableVal.foldr(f)(a)(b))(z)(values(m)),
  foldMap: (dictMonoid) => {
    const foldMap1 = foldMap(dictMonoid);
    return (f) => (m) => {
      const $0 = foldableVal.foldMap(dictMonoid)(f);
      return foldMap1((v) => $0)(m);
    };
  }
};
var foldableDictRep = {
  foldl: (f) => (acc) => (v) => fold((z) => (v$1) => (v1) => foldableVal.foldl(f)(f(z)(v1._1))(v1._2))(acc)(v),
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
            go$a0 = foldableVal.foldl(f)(b)(v._1);
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
      return foldableList.foldr((b) => (a) => foldableVal.foldr(f)(a)(b))(z)(m._2);
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
    return (f) => (m) => Apply0.apply(Apply0.apply(Apply0.Functor0().map((v3) => (v4) => (v5) => $Val(v3, v4, v5))(f(m._1)))(traversableDocOpt(traversableVal).traverse(dictApplicative)(f)(m._2)))(traversableBaseVal.traverse(dictApplicative)(f)(m._3));
  },
  sequence: (dictApplicative) => (v) => traversableVal.traverse(dictApplicative)(identity22)(v),
  Functor0: () => functorVal,
  Foldable1: () => foldableVal
};
var traversableMatrixRep = {
  traverse: (dictApplicative) => {
    const bitraverse1 = bitraversableTuple.bitraverse(dictApplicative);
    const traverse8 = traversableArray.traverse(dictApplicative);
    return (f) => (v) => dictApplicative.Apply0().Functor0().map(MatrixRep)(bitraverse1(traverse8(traverse8(traversableVal.traverse(dictApplicative)(f))))(bitraverse1(traversableMatrixDim.traverse(dictApplicative)(f))(traversableMatrixDim.traverse(dictApplicative)(f)))(v));
  },
  sequence: (dictApplicative) => traversableMatrixRep.traverse(dictApplicative)(identity4),
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
  sequence: (dictApplicative) => (v) => traversableFun.traverse(dictApplicative)(identity22)(v),
  Functor0: () => functorFun,
  Foldable1: () => foldableFun
};
var traversableEnv = {
  traverse: (dictApplicative) => {
    const traverse8 = traversableDict.traverse(dictApplicative);
    return (f) => (m) => dictApplicative.Apply0().Functor0().map((v1) => v1)(traverse8(traversableVal.traverse(dictApplicative)(f))(m));
  },
  sequence: (dictApplicative) => (v) => traversableEnv.traverse(dictApplicative)(identity22)(v),
  Functor0: () => functorEnv,
  Foldable1: () => foldableEnv
};
var traversableDictRep = {
  traverse: (dictApplicative) => {
    const traverse8 = traversableDict.traverse(dictApplicative);
    const bitraverse1 = bitraversableTuple.bitraverse(dictApplicative);
    return (f) => (v) => dictApplicative.Apply0().Functor0().map(DictRep)(traverse8(bitraverse1(f)(traversableVal.traverse(dictApplicative)(f)))(v));
  },
  sequence: (dictApplicative) => traversableDictRep.traverse(dictApplicative)(identity4),
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
  sequence: (dictApplicative) => (v) => traversableBaseVal.traverse(dictApplicative)(identity22)(v),
  Functor0: () => functorBaseVal,
  Foldable1: () => foldableBaseVal
};
var annUnit = { Highlightable0: () => highlightableUnit, BoundedLattice1: () => boundedLattice };
var reaches = (\u03C1) => (xs) => {
  const dom_\u03C1 = mapObjectString.keys(\u03C1);
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
        go$a0 = foldableList.foldr(Cons)(v._2)(toUnfoldable14(intersection2(fVElim.fv($$get(showString)(mapDictString)(v._1)(\u03C1)))(dom_\u03C1)));
        go$a1 = setSet1.union($$$Map("Two", Leaf2, v._1, void 0, Leaf2))(v1);
        continue;
      }
      fail();
    }
    return go$r;
  };
  return go(toUnfoldable14(xs))(setSet1.empty);
};
var matrixPut = (i) => (j) => (\u03B4v) => (v) => {
  const vs_i = definitely("index within bounds")(index(v._1)(i - 1 | 0));
  return $Tuple(
    unsafeArrayArray.unsafeUpdateAt(i - 1 | 0)(unsafeArrayArray.unsafeUpdateAt(j - 1 | 0)(\u03B4v(definitely("index within bounds")(index(vs_i)(j - 1 | 0))))(vs_i))(v._1),
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
  const $0 = reaches(\u03C1)(intersection2(fVElim.fv(\u03C3))(fromFoldable10(mapObjectString.keys(\u03C1))));
  return filterWithKey2((x) => {
    const $1 = setSet(ordString).member(x)($0);
    return (v) => $1;
  })(\u03C1);
};

// output-es/Primitive/index.js
var fanin2 = /* @__PURE__ */ fanin(categoryFn)(choiceFn);
var isZeroNumber = { isZero: ($0) => 0 === $0 };
var isZeroInt = { isZero: ($0) => 0 === $0 };
var unary = (dictBoundedJoinSemilattice) => {
  const bot = dictBoundedJoinSemilattice.bot;
  return (id) => (f) => $Tuple(
    id,
    $Val(
      bot,
      None,
      $BaseVal(
        "Fun",
        $Fun(
          "Foreign",
          $Tuple(
            id,
            $ForeignOp$p({
              arity: 1,
              op: (dictMonadWithGraphAlloc) => {
                const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
                return (dictMonadError) => (dictLoadFile) => (v) => {
                  if (v.tag === "Cons" && v._2.tag === "Nil") {
                    const $0 = v._1._2;
                    return $$new((a) => Val(a)($0))($$$Map("Two", Leaf2, v._1._1, void 0, Leaf2))(f.o.pack(f.fwd(f.i.unpack(v._1._3))));
                  }
                  fail();
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
var typeError = (v) => (typeName2) => throwException(error(typeName2 + " expected; got " + intercalate4("\n")(removeDocWS(prettyBaseVal(highlightableUnit).pretty(functorBaseVal.map((v$1) => {
})(v))).lines)))();
var string = {
  pack: Str2,
  unpack: (v) => {
    if (v.tag === "Str") {
      return v._1;
    }
    return typeError(v)("Str");
  }
};
var number = {
  pack: Float2,
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
  pack: Int2,
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
      $Val(v._1._2, None, $BaseVal("Int", v._1._1)),
      $List("Cons", $Val(v._2._2, None, $BaseVal("Int", v._2._1)), Nil)
    )
  ),
  unpack: (v) => {
    if (v.tag === "Constr" && v._2.tag === "Cons" && v._2._2.tag === "Cons" && v._2._2._2.tag === "Nil" && v._1 === "Pair") {
      return $Tuple(
        $Tuple(v._2._1._3.tag === "Int" ? v._2._1._3._1 : typeError(v._2._1._3)("Int"), v._2._1._1),
        $Tuple(v._2._2._1._3.tag === "Int" ? v._2._2._1._3._1 : typeError(v._2._2._1._3)("Int"), v._2._2._1._1)
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
      None,
      $BaseVal(
        "Fun",
        $Fun(
          "Foreign",
          $Tuple(
            id,
            $ForeignOp$p({
              arity: 2,
              op: (dictMonadWithGraphAlloc) => {
                const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
                return (dictMonadError) => (dictLoadFile) => (v) => {
                  if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._2.tag === "Nil") {
                    const $0 = f.i.unpack(v._1._3);
                    const $1 = f.i.unpack(v._2._1._3);
                    return $$new((a) => Val(a)(None))((() => {
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
      None,
      $BaseVal(
        "Fun",
        $Fun(
          "Foreign",
          $Tuple(
            id,
            $ForeignOp$p({
              arity: 2,
              op: (dictMonadWithGraphAlloc) => {
                const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
                return (dictMonadError) => (dictLoadFile) => (v) => {
                  if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._2.tag === "Nil") {
                    return $$new((a) => Val(a)(None))(insert(ordVertex)(v._2._1._1)()($$$Map(
                      "Two",
                      Leaf2,
                      v._1._1,
                      void 0,
                      Leaf2
                    )))(f.o.pack(f.fwd(f.i1.unpack(v._1._3))(f.i2.unpack(v._2._1._3))));
                  }
                  fail();
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
var setSet4 = /* @__PURE__ */ setSet(ordVertex);
var disjointUnion2 = /* @__PURE__ */ disjointUnion(mapEnvStringVal);
var fromFoldable11 = /* @__PURE__ */ (() => foldableSet.foldl((m) => (a) => insert(ordString)(a)()(m))(Leaf2))();
var show22 = /* @__PURE__ */ (() => showSet(showString).show)();
var toUnfoldable9 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
var union1 = /* @__PURE__ */ (() => setSet(ordString).union)();
var fv = /* @__PURE__ */ (() => fVDict(fVElim).fv)();
var pack3 = (x) => (k) => k(typeNameVal)(x);
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
      if (v._3.tag === "Constr") {
        const $1 = v._3._1;
        const $2 = v1._1;
        const $3 = v._3._2;
        const $4 = v._1;
        return Bind1.bind(withMsg2("Pattern mismatch")(consistentWith2($$$Map("Two", Leaf2, $1, void 0, Leaf2))(mapObjectString.keys($2))))(() => Bind1.bind(orElse(MonadThrow0)("Incomplete patterns: no branch for " + showCtr($1))(_lookup(
          Nothing,
          Just,
          $1,
          $2
        )))((\u03BA) => Bind1.bind(matchMany(dictMonadWithGraphAlloc)($3)(\u03BA))((v2) => $0.pure($Tuple(
          v2._1,
          $Tuple(v2._2._1, insert(ordVertex)($4)()(v2._2._2))
        )))));
      }
      return Bind1.bind(dataTypeForSetCtr.dataTypeFor(MonadThrow0)(mapObjectString.keys(v1._1)))((d) => MonadThrow0.throwError(error("Pattern mismatch: found " + intercalate4("\n")(removeDocWS(prettyVal(highlightableVertex).pretty(v)).lines) + ", expected " + d._1)));
    }
    if (v1.tag === "ElimDict") {
      if (v._3.tag === "Dictionary") {
        const $1 = v1._1;
        const $2 = v._3._1;
        const $3 = v._1;
        const $4 = v1._2;
        return Bind1.bind(check(MonadThrow0)(difference2(ordString)($1)(fromFoldable11(mapObjectString.keys($2))).tag === "Leaf")("Pattern mismatch: found " + show22(mapObjectString.keys($2)) + ", expected " + show22($1)))(() => Bind1.bind(matchMany(dictMonadWithGraphAlloc)(listMap((k) => $$get(showString)(mapObjectString)(k)($2)._2)(toUnfoldable9($1)))($4))((v2) => $0.pure($Tuple(
          v2._1,
          $Tuple(v2._2._1, insert(ordVertex)($3)()(v2._2._2))
        ))));
      }
      return MonadThrow0.throwError(error("Pattern mismatch: found " + intercalate4("\n")(removeDocWS(prettyVal(highlightableVertex).pretty(v)).lines) + ", expected " + show22(v1._1)));
    }
    fail();
  };
};
var closeDefs = (dictMonadWithGraphAlloc) => {
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const traverse2 = traversableDict.traverse(Monad0.Applicative0());
  const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
  return (\u03B3) => (\u03C1) => (\u03B1s) => Monad0.Bind1().Apply0().Functor0().map(Env)(traverse2((\u03C3) => {
    const \u03C1$p = forDefs(\u03C1)(\u03C3);
    return $$new((a) => Val(a)(None))(\u03B1s)($BaseVal(
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
var new$p = (dictMonadWithGraphAlloc) => {
  const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
  const MonadWithGraph2 = dictMonadWithGraphAlloc.MonadWithGraph2();
  const Monad0 = MonadWithGraph2.Monad0();
  const Bind1 = Monad0.Bind1();
  const fresh = dictMonadWithGraphAlloc.MonadAlloc0().fresh;
  return (dictLoadFile) => (v) => (v1) => (v2) => (v3) => {
    if (v2.tag === "None") {
      return $$new((\u03B1s$p) => (u$p) => $Val(\u03B1s$p, None, u$p))(v1)(v3);
    }
    return Bind1.bind(fresh)((\u03B1) => Bind1.bind(evalDocOpt(dictMonadWithGraphAlloc)(dictLoadFile)(unionWith2((v$1) => identity13)(v)((() => {
      const $0 = {};
      $0.this = $Val(\u03B1, None, v3);
      return $0;
    })()))(v2))((vdoc) => Bind1.bind(MonadWithGraph2.extend($Tuple(\u03B1, pack3($Val(\u03B1, vdoc, v3))))(v1))(() => Monad0.Applicative0().pure($Val(\u03B1, vdoc, v3)))));
  };
};
var evalDocOpt = (dictMonadWithGraphAlloc) => {
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const Applicative0 = Monad0.Applicative0();
  const $0 = Monad0.Bind1().Apply0().Functor0();
  const sequence2 = traversableList.traverse(Applicative0)(identity5);
  return (dictLoadFile) => (v) => (v1) => {
    if (v1.tag === "None") {
      return Applicative0.pure(None);
    }
    if (v1.tag === "Doc") {
      return $0.map(Doc)(sequence2(listMap((v2) => {
        if (v2.tag === "Token") {
          return Applicative0.pure($DocCommentElem("Token", v2._1));
        }
        if (v2.tag === "Unquote") {
          return $0.map(Unquote)($$eval(dictMonadWithGraphAlloc)(dictLoadFile)(v)(v2._1)(setSet4.empty));
        }
        fail();
      })(v1._1)));
    }
    fail();
  };
};
var $$eval = (dictMonadWithGraphAlloc) => {
  const MonadError1 = dictMonadWithGraphAlloc.MonadError1();
  const withMsg2 = withMsg(MonadError1);
  const MonadThrow0 = MonadError1.MonadThrow0();
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const Bind1 = Monad0.Bind1();
  const $0 = Bind1.Apply0().Functor0();
  const Applicative0 = Monad0.Applicative0();
  const traverse2 = traversableList.traverse(Applicative0);
  const traverse3 = traversablePair.traverse(Applicative0);
  const checkArity2 = checkArity(MonadError1);
  const sequence2 = traversableArray.traverse(Applicative0)(identity4);
  const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
  const match1 = match(dictMonadWithGraphAlloc);
  const closeDefs1 = closeDefs(dictMonadWithGraphAlloc);
  return (dictLoadFile) => (v) => (v1) => (v2) => {
    if (v1.tag === "Var") {
      return withMsg2("Variable lookup")(lookup$p(MonadThrow0)(showString)(mapEnvStringVal)(v1._1)(v));
    }
    if (v1.tag === "Op") {
      return withMsg2("Variable lookup")(lookup$p(MonadThrow0)(showString)(mapEnvStringVal)(v1._1)(v));
    }
    if (v1.tag === "Int") {
      return new$p(dictMonadWithGraphAlloc)(dictLoadFile)(v)(insert(ordVertex)(v1._1)()(v2))(v1._2)($BaseVal("Int", v1._3));
    }
    if (v1.tag === "Float") {
      return new$p(dictMonadWithGraphAlloc)(dictLoadFile)(v)(insert(ordVertex)(v1._1)()(v2))(v1._2)($BaseVal("Float", v1._3));
    }
    if (v1.tag === "Str") {
      return new$p(dictMonadWithGraphAlloc)(dictLoadFile)(v)(insert(ordVertex)(v1._1)()(v2))(v1._2)($BaseVal("Str", v1._3));
    }
    if (v1.tag === "Dictionary") {
      const $1 = v1._2;
      const $2 = v1._1;
      return Bind1.bind($0.map(unzip3)(traverse2(traverse3((() => {
        const $3 = $$eval(dictMonadWithGraphAlloc)(dictLoadFile)(v);
        return (a) => $3(a)(v2);
      })()))(v1._3)))((v3) => {
        const v4 = unzip(listMap((v$1) => $Tuple(v$1._3.tag === "Str" ? v$1._3._1 : typeError(v$1._3)("Str"), v$1._1))(v3._1));
        return new$p(dictMonadWithGraphAlloc)(dictLoadFile)(v)(insert(ordVertex)($2)()(v2))($1)($BaseVal(
          "Dictionary",
          fromFoldable17(zipWith2(Tuple)(v4._1)(zipWith2(Tuple)(v4._2)(v3._2)))
        ));
      });
    }
    if (v1.tag === "Constr") {
      const $1 = v1._3;
      const $2 = v1._2;
      const $3 = v1._4;
      const $4 = v1._1;
      return Bind1.bind(checkArity2($1)((() => {
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
        return go(0)($3);
      })()))(() => Bind1.bind(traverse2((() => {
        const $5 = $$eval(dictMonadWithGraphAlloc)(dictLoadFile)(v);
        return (a) => $5(a)(v2);
      })())($3))((vs) => new$p(dictMonadWithGraphAlloc)(dictLoadFile)(v)(insert(ordVertex)($4)()(v2))($2)($BaseVal("Constr", $1, vs))));
    }
    if (v1.tag === "Matrix") {
      const $1 = v1._2;
      const $2 = v1._3;
      const $3 = v1._4._1;
      const $4 = v1._4._2;
      const $5 = v1._1;
      return Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictLoadFile)(v)(v1._5)(v2))((v3) => {
        const v5 = intPair.unpack(v3._3);
        const $6 = v5._1._1;
        const $7 = v5._2._1;
        const $8 = v5._1._2;
        const $9 = v5._2._2;
        return Bind1.bind(check(MonadThrow0)(greaterThanOrEq($Tuple($6, $7))($Tuple(1, 1)))("array must be at least (" + show3($Tuple(1, 1)) + "); got (" + show3($Tuple(
          $6,
          $7
        )) + ")"))(() => Bind1.bind(sequence2(arrayBind(range(1)($6))((i) => [
          sequence2(arrayBind(range(1)($7))((j) => [
            $$eval(dictMonadWithGraphAlloc)(dictLoadFile)(unionWith2((v$1) => identity13)(v)(disjointUnion2((() => {
              const $10 = {};
              $10[$3] = $Val($8, None, $BaseVal("Int", i));
              return $10;
            })())((() => {
              const $10 = {};
              $10[$4] = $Val($9, None, $BaseVal("Int", j));
              return $10;
            })())))($2)(v2)
          ]))
        ])))((vss) => new$p(dictMonadWithGraphAlloc)(dictLoadFile)(v)(insert(ordVertex)($5)()(v2))($1)($BaseVal(
          "Matrix",
          $Tuple(vss, $Tuple($Tuple($6, $8), $Tuple($7, $9)))
        ))));
      });
    }
    if (v1.tag === "Lambda") {
      return $$new((a) => Val(a)(None))(insert(ordVertex)(v1._1)()(v2))($BaseVal(
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
      const $1 = v1._1;
      const $2 = v1._3;
      return Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictLoadFile)(v)(v1._2)(v2))((v3) => {
        if (v3._3.tag === "Dictionary") {
          return Bind1.bind(withMsg2("Dict lookup")(orElse(MonadThrow0)('Key "' + $2 + '" not found')((() => {
            const $3 = _lookup(Nothing, Just, $2, v3._3._1);
            if ($3.tag === "Just") {
              return $Maybe("Just", $3._1._2);
            }
            return Nothing;
          })())))((v$p) => concatDocs(dictMonadWithGraphAlloc)(dictLoadFile)(v)(v$p)($1));
        }
        return MonadThrow0.throwError(error("Found " + intercalate4("\n")(removeDocWS(prettyVal(highlightableVertex).pretty(v3)).lines) + ", expected dictionary"));
      });
    }
    if (v1.tag === "DProject") {
      const $1 = v1._1;
      const $2 = v1._3;
      return Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictLoadFile)(v)(v1._2)(v2))((v3) => Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictLoadFile)(v)($2)(v2))((v$p) => {
        if (v3._3.tag === "Dictionary") {
          if (v$p._3.tag === "Str") {
            return Bind1.bind(withMsg2("Dict lookup")(orElse(MonadThrow0)('Key "' + v$p._3._1 + '" not found')((() => {
              const $3 = _lookup(Nothing, Just, v$p._3._1, v3._3._1);
              if ($3.tag === "Just") {
                return $Maybe("Just", $3._1._2);
              }
              return Nothing;
            })())))((v$p$p) => concatDocs(dictMonadWithGraphAlloc)(dictLoadFile)(v)(v$p$p)($1));
          }
          return MonadThrow0.throwError(error("Found " + intercalate4("\n")(removeDocWS(prettyVal(highlightableVertex).pretty(v$p)).lines) + ", expected string"));
        }
        return MonadThrow0.throwError(error("Found " + intercalate4("\n")(removeDocWS(prettyVal(highlightableVertex).pretty(v3)).lines) + ", expected dict"));
      }));
    }
    if (v1.tag === "App") {
      const $1 = v1._1;
      const $2 = v1._3;
      return Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictLoadFile)(v)(v1._2)(v2))((v3) => Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictLoadFile)(v)($2)(v2))((v$p) => Bind1.bind(apply2(dictMonadWithGraphAlloc)(dictLoadFile)(v3)(v$p))((v4) => {
        const $3 = v4._3;
        const $4 = v4._1;
        return Bind1.bind(evalDocOpt(dictMonadWithGraphAlloc)(dictLoadFile)(unionWith2((v$1) => identity13)(v)((() => {
          const $5 = {};
          $5.this = v4;
          return $5;
        })()))($1))((vdoc) => Applicative0.pure($Val($4, vdoc, $3)));
      })));
    }
    if (v1.tag === "Let") {
      const $1 = v1._2;
      const $2 = v1._1._1;
      return Bind1.bind($$eval(dictMonadWithGraphAlloc)(dictLoadFile)(v)(v1._1._2)(v2))((v3) => Bind1.bind(match1(v3)($2))((v4) => $$eval(dictMonadWithGraphAlloc)(dictLoadFile)(unionWith2((v$1) => identity13)(v)(v4._1))($1)(v4._2._2)));
    }
    if (v1.tag === "LetRec") {
      const $1 = v1._2;
      const $2 = v1._1._1;
      return Bind1.bind(closeDefs1(v)(v1._1._2)(insert(ordVertex)($2)()(v2)))((\u03B3$p) => $$eval(dictMonadWithGraphAlloc)(dictLoadFile)(unionWith2((v$1) => identity13)(v)(\u03B3$p))($1)(insert(ordVertex)($2)()(v2)));
    }
    fail();
  };
};
var concatDocs = (dictMonadWithGraphAlloc) => {
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  return (dictLoadFile) => (\u03B3) => (v) => (doc) => {
    const $0 = v._3;
    const $1 = v._2;
    const $2 = v._1;
    return Monad0.Bind1().bind(evalDocOpt(dictMonadWithGraphAlloc)(dictLoadFile)(unionWith2((v$1) => identity13)(\u03B3)((() => {
      const $3 = {};
      $3.this = $Val($2, None, $0);
      return $3;
    })()))(doc))((vdoc$p) => Monad0.Applicative0().pure($Val($2, semigroupDocOpt.append(vdoc$p)($1), $0)));
  };
};
var apply2 = (dictMonadWithGraphAlloc) => {
  const Bind1 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0().Bind1();
  const closeDefs1 = closeDefs(dictMonadWithGraphAlloc);
  const match1 = match(dictMonadWithGraphAlloc);
  const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
  const MonadError1 = dictMonadWithGraphAlloc.MonadError1();
  const MonadThrow0 = MonadError1.MonadThrow0();
  return (dictLoadFile) => (v) => (v1) => {
    const $0 = (v2) => MonadThrow0.throwError(error("Found " + intercalate4("\n")(removeDocWS(prettyVal(highlightableVertex).pretty(v2)).lines) + ", expected function"));
    if (v._3.tag === "Fun") {
      if (v._3._1.tag === "Closure") {
        const $1 = v._1;
        const $2 = v._3._1._1;
        const $3 = v._3._1._3;
        return Bind1.bind(closeDefs1($2)(v._3._1._2)($$$Map("Two", Leaf2, $1, void 0, Leaf2)))((\u03B32) => Bind1.bind(match1(v1)($3))((v3) => $$eval(dictMonadWithGraphAlloc)(dictLoadFile)(unionWith2((v$1) => identity13)(unionWith2((v$1) => identity13)($2)(\u03B32))(v3._1))(v3._2._1.tag === "ContExpr" ? v3._2._1._1 : throwException(error("Expression expected"))())(insert(ordVertex)($1)()(v3._2._2))));
      }
      if (v._3._1.tag === "Foreign") {
        const $1 = v._3._1._1._2;
        const vs$p = foldableList.foldr(Cons)($List("Cons", v1, Nil))(v._3._1._2);
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
          return $$new((a) => Val(a)(None))($$$Map("Two", Leaf2, v._1, void 0, Leaf2))($BaseVal(
            "Fun",
            $Fun("Foreign", $Tuple(v._3._1._1._1, $1), vs$p)
          ));
        }
        return $1._1.op(dictMonadWithGraphAlloc)(MonadError1)(dictLoadFile)(vs$p);
      }
      if (v._3._1.tag === "PartialConstr") {
        const $1 = v._1;
        const n = defined(arity(monadThrowExceptT(monadIdentity))(v._3._1._1));
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
          return go(0)(v._3._1._2) < (n - 1 | 0);
        })() ? $BaseVal(
          "Fun",
          $Fun(
            "PartialConstr",
            v._3._1._1,
            foldableList.foldr(Cons)($List("Cons", v1, Nil))(v._3._1._2)
          )
        ) : $BaseVal("Constr", v._3._1._1, foldableList.foldr(Cons)($List("Cons", v1, Nil))(v._3._1._2));
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
          return go(0)(v._3._1._2) < n;
        })())("Too many arguments to " + showCtr(v._3._1._1)))(() => $$new((a) => Val(a)(None))($$$Map(
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
  return (dictLoadFile) => {
    const eval2 = eval1(dictLoadFile);
    return (\u03B3) => {
      const go = (v) => (v1) => (v2) => {
        if (v1.tag === "Nil") {
          return Monad0.Applicative0().pure(v);
        }
        if (v1.tag === "Cons") {
          if (v1._1.tag === "Left") {
            const $1 = v1._2;
            const $2 = v1._1._1._1;
            return $0.bind(eval2(unionWith2((v$1) => identity13)(\u03B3)(v))(v1._1._1._2)(v2))((v3) => $0.bind(match1(v3)($2))((v4) => go(unionWith2((v$1) => identity13)(v)(v4._1))($1)(v4._2._2)));
          }
          if (v1._1.tag === "Right") {
            const $1 = v1._2;
            return $0.bind(closeDefs1(unionWith2((v$1) => identity13)(\u03B3)(v))(v1._1._1._2)(insert(ordVertex)(v1._1._1._1)()(v2)))((\u03B3$p$p) => go(unionWith2((v$1) => identity13)(v)(\u03B3$p$p))($1)(v2));
          }
        }
        fail();
      };
      return go(empty);
    };
  };
};
var eval_progCxt = (dictMonadWithGraphAlloc) => {
  const Monad0 = dictMonadWithGraphAlloc.MonadWithGraph2().Monad0();
  const $0 = Monad0.Bind1();
  const eval_module1 = eval_module(dictMonadWithGraphAlloc);
  const $1 = Monad0.Applicative0();
  const eval1 = $$eval(dictMonadWithGraphAlloc);
  const concatM1 = concatM(Monad0);
  return (dictLoadFile) => {
    const eval_module2 = eval_module1(dictLoadFile);
    const eval2 = eval1(dictLoadFile);
    return (v) => concatM1(foldableList.foldr(Cons)(listMap((v1) => (\u03B3) => {
      const $2 = v1._1;
      return $0.bind(eval2(\u03B3)(v1._2)(setSet4.empty))((v2) => $1.pure(unionWith2((v$1) => identity13)(\u03B3)((() => {
        const $3 = {};
        $3[$2] = v2;
        return $3;
      })())));
    })(reverse2(v.datasets)))(listMap((mod) => (\u03B3) => $0.bind(eval_module2(\u03B3)(mod)(setSet4.empty))((\u03B3$p) => $1.pure(unionWith2((v$1) => identity13)(\u03B3)(\u03B3$p))))(reverse2(v.mods))))(v.primitives);
  };
};
var graphEval = (dictMonadAff) => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const bindStateT2 = bindStateT(Monad0);
  const $0 = monadAllocAllocT(Monad0);
  const fresh1 = $0.fresh;
  const alloc = traversableExpr.traverse($0.Monad0().Applicative0())((v) => fresh1);
  const runWithGraphT_spy2 = runWithGraphT_spy({
    Applicative0: () => applicativeStateT(Monad0),
    Bind1: () => bindStateT(Monad0)
  })(graphGraphImpl);
  const $1 = monadAffState(dictMonadAff).MonadEffect0().Monad0();
  const $2 = dictMonadAff.MonadEffect0().Monad0();
  return (dictLoadFile) => (dictMonadError) => {
    const eval1 = $$eval(monadWithGraphAllocWithGr(dictMonadError))((() => {
      const loadFile1 = dictLoadFile.loadFile(dictMonadError)(dictMonadAff);
      return {
        loadFile: (dictMonadError1) => (dictMonadAff1) => (folders) => {
          const $3 = loadFile1(folders);
          return (x) => {
            const $4 = $3(x);
            return (s) => $1.Bind1().bind((s$1) => $2.Bind1().bind($4)((x$1) => $2.Applicative0().pure($Tuple(x$1, s$1))))((x$1) => $1.Applicative0().pure($Tuple(
              x$1,
              s
            )));
          };
        }
      };
    })());
    const check2 = check(monadThrowStateT(dictMonadError.MonadThrow0()));
    return (v) => (e) => {
      const $3 = v["\u03B3"];
      const $4 = spyFunWhen(false)("fwdSlice")((x) => $Tuple(showVertices(x._1), showEdgeList(toEdgeList(graphGraphImpl)(x._2))))(showGraph(graphGraphImpl))(fwdSlice2);
      const $5 = spyFunWhen(false)("bwdSlice")((x) => $Tuple(showVertices(x._1), showEdgeList(toEdgeList(graphGraphImpl)(x._2))))(showGraph(graphGraphImpl))(bwdSlice2);
      return Monad0.Bind1().bind(runAllocT(Monad0)(bindStateT2.bind(alloc(e))((e\u03B1) => bindStateT2.bind(runWithGraphT_spy2(eval1($3)(e\u03B1)(Leaf2))(verticesEnvExprVertex.vertices($EnvExpr(
        $3,
        e\u03B1
      ))))((v1) => {
        const $6 = v1._1;
        const $7 = v1._2;
        return bindStateT2.bind(check2(difference2(ordDVertex$p)(verticesValVertex.vertices($7))(verticesGraphImpl.vertices($6)).tag === "Leaf")("outputs in graph"))(() => applicativeStateT(Monad0).pure($Tuple(
          $6,
          $Tuple($EnvExpr($3, e\u03B1), $7)
        )));
      })))(v.n))((v1) => Monad0.Applicative0().pure({
        g: v1._2._2._1,
        graph_fwd: (a) => (b) => $4($Tuple(a, b)),
        graph_bwd: (a) => (b) => $5($Tuple(a, b)),
        "in\u03B1": v1._2._2._2._1,
        "out\u03B1": v1._2._2._2._2
      }));
    };
  };
};

// output-es/ExitCodes/index.js
var $ExitCode = (tag) => tag;
var Success = /* @__PURE__ */ $ExitCode("Success");
var $$Error = /* @__PURE__ */ $ExitCode("Error");
var MisuseOfShellBuiltins = /* @__PURE__ */ $ExitCode("MisuseOfShellBuiltins");
var CLIUsageError = /* @__PURE__ */ $ExitCode("CLIUsageError");
var DataFormatError = /* @__PURE__ */ $ExitCode("DataFormatError");
var CannotOpenInput = /* @__PURE__ */ $ExitCode("CannotOpenInput");
var AddresseeUnknown = /* @__PURE__ */ $ExitCode("AddresseeUnknown");
var HostNameUnknown = /* @__PURE__ */ $ExitCode("HostNameUnknown");
var ServiceUnavailable = /* @__PURE__ */ $ExitCode("ServiceUnavailable");
var InternalSoftwareError = /* @__PURE__ */ $ExitCode("InternalSoftwareError");
var SystemError = /* @__PURE__ */ $ExitCode("SystemError");
var CriticalOSFileMissing = /* @__PURE__ */ $ExitCode("CriticalOSFileMissing");
var CannotCreateOutputFile = /* @__PURE__ */ $ExitCode("CannotCreateOutputFile");
var IOError = /* @__PURE__ */ $ExitCode("IOError");
var TemporaryFailure = /* @__PURE__ */ $ExitCode("TemporaryFailure");
var RemoteError = /* @__PURE__ */ $ExitCode("RemoteError");
var PermissionDenied = /* @__PURE__ */ $ExitCode("PermissionDenied");
var ConfigurationError = /* @__PURE__ */ $ExitCode("ConfigurationError");
var CannotExecute = /* @__PURE__ */ $ExitCode("CannotExecute");
var CommandNotFound = /* @__PURE__ */ $ExitCode("CommandNotFound");
var InvalidExitArgument = /* @__PURE__ */ $ExitCode("InvalidExitArgument");
var SIGHUP = /* @__PURE__ */ $ExitCode("SIGHUP");
var SIGINT = /* @__PURE__ */ $ExitCode("SIGINT");
var SIGQUIT = /* @__PURE__ */ $ExitCode("SIGQUIT");
var SIGILL = /* @__PURE__ */ $ExitCode("SIGILL");
var SIGABRT = /* @__PURE__ */ $ExitCode("SIGABRT");
var SIGFPE = /* @__PURE__ */ $ExitCode("SIGFPE");
var SIGKILL = /* @__PURE__ */ $ExitCode("SIGKILL");
var SIGSEGV = /* @__PURE__ */ $ExitCode("SIGSEGV");
var SIGPIPE = /* @__PURE__ */ $ExitCode("SIGPIPE");
var SIGALRM = /* @__PURE__ */ $ExitCode("SIGALRM");
var SIGTERM = /* @__PURE__ */ $ExitCode("SIGTERM");
var eqExitCode = {
  eq: (x) => (y) => {
    if (x === "Success") {
      return y === "Success";
    }
    if (x === "Error") {
      return y === "Error";
    }
    if (x === "MisuseOfShellBuiltins") {
      return y === "MisuseOfShellBuiltins";
    }
    if (x === "CLIUsageError") {
      return y === "CLIUsageError";
    }
    if (x === "DataFormatError") {
      return y === "DataFormatError";
    }
    if (x === "CannotOpenInput") {
      return y === "CannotOpenInput";
    }
    if (x === "AddresseeUnknown") {
      return y === "AddresseeUnknown";
    }
    if (x === "HostNameUnknown") {
      return y === "HostNameUnknown";
    }
    if (x === "ServiceUnavailable") {
      return y === "ServiceUnavailable";
    }
    if (x === "InternalSoftwareError") {
      return y === "InternalSoftwareError";
    }
    if (x === "SystemError") {
      return y === "SystemError";
    }
    if (x === "CriticalOSFileMissing") {
      return y === "CriticalOSFileMissing";
    }
    if (x === "CannotCreateOutputFile") {
      return y === "CannotCreateOutputFile";
    }
    if (x === "IOError") {
      return y === "IOError";
    }
    if (x === "TemporaryFailure") {
      return y === "TemporaryFailure";
    }
    if (x === "RemoteError") {
      return y === "RemoteError";
    }
    if (x === "PermissionDenied") {
      return y === "PermissionDenied";
    }
    if (x === "ConfigurationError") {
      return y === "ConfigurationError";
    }
    if (x === "CannotExecute") {
      return y === "CannotExecute";
    }
    if (x === "CommandNotFound") {
      return y === "CommandNotFound";
    }
    if (x === "InvalidExitArgument") {
      return y === "InvalidExitArgument";
    }
    if (x === "SIGHUP") {
      return y === "SIGHUP";
    }
    if (x === "SIGINT") {
      return y === "SIGINT";
    }
    if (x === "SIGQUIT") {
      return y === "SIGQUIT";
    }
    if (x === "SIGILL") {
      return y === "SIGILL";
    }
    if (x === "SIGABRT") {
      return y === "SIGABRT";
    }
    if (x === "SIGFPE") {
      return y === "SIGFPE";
    }
    if (x === "SIGKILL") {
      return y === "SIGKILL";
    }
    if (x === "SIGSEGV") {
      return y === "SIGSEGV";
    }
    if (x === "SIGPIPE") {
      return y === "SIGPIPE";
    }
    if (x === "SIGALRM") {
      return y === "SIGALRM";
    }
    return x === "SIGTERM" && y === "SIGTERM";
  }
};
var ordExitCode = {
  compare: (x) => (y) => {
    if (x === "Success") {
      if (y === "Success") {
        return EQ;
      }
      return LT;
    }
    if (y === "Success") {
      return GT;
    }
    if (x === "Error") {
      if (y === "Error") {
        return EQ;
      }
      return LT;
    }
    if (y === "Error") {
      return GT;
    }
    if (x === "MisuseOfShellBuiltins") {
      if (y === "MisuseOfShellBuiltins") {
        return EQ;
      }
      return LT;
    }
    if (y === "MisuseOfShellBuiltins") {
      return GT;
    }
    if (x === "CLIUsageError") {
      if (y === "CLIUsageError") {
        return EQ;
      }
      return LT;
    }
    if (y === "CLIUsageError") {
      return GT;
    }
    if (x === "DataFormatError") {
      if (y === "DataFormatError") {
        return EQ;
      }
      return LT;
    }
    if (y === "DataFormatError") {
      return GT;
    }
    if (x === "CannotOpenInput") {
      if (y === "CannotOpenInput") {
        return EQ;
      }
      return LT;
    }
    if (y === "CannotOpenInput") {
      return GT;
    }
    if (x === "AddresseeUnknown") {
      if (y === "AddresseeUnknown") {
        return EQ;
      }
      return LT;
    }
    if (y === "AddresseeUnknown") {
      return GT;
    }
    if (x === "HostNameUnknown") {
      if (y === "HostNameUnknown") {
        return EQ;
      }
      return LT;
    }
    if (y === "HostNameUnknown") {
      return GT;
    }
    if (x === "ServiceUnavailable") {
      if (y === "ServiceUnavailable") {
        return EQ;
      }
      return LT;
    }
    if (y === "ServiceUnavailable") {
      return GT;
    }
    if (x === "InternalSoftwareError") {
      if (y === "InternalSoftwareError") {
        return EQ;
      }
      return LT;
    }
    if (y === "InternalSoftwareError") {
      return GT;
    }
    if (x === "SystemError") {
      if (y === "SystemError") {
        return EQ;
      }
      return LT;
    }
    if (y === "SystemError") {
      return GT;
    }
    if (x === "CriticalOSFileMissing") {
      if (y === "CriticalOSFileMissing") {
        return EQ;
      }
      return LT;
    }
    if (y === "CriticalOSFileMissing") {
      return GT;
    }
    if (x === "CannotCreateOutputFile") {
      if (y === "CannotCreateOutputFile") {
        return EQ;
      }
      return LT;
    }
    if (y === "CannotCreateOutputFile") {
      return GT;
    }
    if (x === "IOError") {
      if (y === "IOError") {
        return EQ;
      }
      return LT;
    }
    if (y === "IOError") {
      return GT;
    }
    if (x === "TemporaryFailure") {
      if (y === "TemporaryFailure") {
        return EQ;
      }
      return LT;
    }
    if (y === "TemporaryFailure") {
      return GT;
    }
    if (x === "RemoteError") {
      if (y === "RemoteError") {
        return EQ;
      }
      return LT;
    }
    if (y === "RemoteError") {
      return GT;
    }
    if (x === "PermissionDenied") {
      if (y === "PermissionDenied") {
        return EQ;
      }
      return LT;
    }
    if (y === "PermissionDenied") {
      return GT;
    }
    if (x === "ConfigurationError") {
      if (y === "ConfigurationError") {
        return EQ;
      }
      return LT;
    }
    if (y === "ConfigurationError") {
      return GT;
    }
    if (x === "CannotExecute") {
      if (y === "CannotExecute") {
        return EQ;
      }
      return LT;
    }
    if (y === "CannotExecute") {
      return GT;
    }
    if (x === "CommandNotFound") {
      if (y === "CommandNotFound") {
        return EQ;
      }
      return LT;
    }
    if (y === "CommandNotFound") {
      return GT;
    }
    if (x === "InvalidExitArgument") {
      if (y === "InvalidExitArgument") {
        return EQ;
      }
      return LT;
    }
    if (y === "InvalidExitArgument") {
      return GT;
    }
    if (x === "SIGHUP") {
      if (y === "SIGHUP") {
        return EQ;
      }
      return LT;
    }
    if (y === "SIGHUP") {
      return GT;
    }
    if (x === "SIGINT") {
      if (y === "SIGINT") {
        return EQ;
      }
      return LT;
    }
    if (y === "SIGINT") {
      return GT;
    }
    if (x === "SIGQUIT") {
      if (y === "SIGQUIT") {
        return EQ;
      }
      return LT;
    }
    if (y === "SIGQUIT") {
      return GT;
    }
    if (x === "SIGILL") {
      if (y === "SIGILL") {
        return EQ;
      }
      return LT;
    }
    if (y === "SIGILL") {
      return GT;
    }
    if (x === "SIGABRT") {
      if (y === "SIGABRT") {
        return EQ;
      }
      return LT;
    }
    if (y === "SIGABRT") {
      return GT;
    }
    if (x === "SIGFPE") {
      if (y === "SIGFPE") {
        return EQ;
      }
      return LT;
    }
    if (y === "SIGFPE") {
      return GT;
    }
    if (x === "SIGKILL") {
      if (y === "SIGKILL") {
        return EQ;
      }
      return LT;
    }
    if (y === "SIGKILL") {
      return GT;
    }
    if (x === "SIGSEGV") {
      if (y === "SIGSEGV") {
        return EQ;
      }
      return LT;
    }
    if (y === "SIGSEGV") {
      return GT;
    }
    if (x === "SIGPIPE") {
      if (y === "SIGPIPE") {
        return EQ;
      }
      return LT;
    }
    if (y === "SIGPIPE") {
      return GT;
    }
    if (x === "SIGALRM") {
      if (y === "SIGALRM") {
        return EQ;
      }
      return LT;
    }
    if (y === "SIGALRM") {
      return GT;
    }
    if (x === "SIGTERM" && y === "SIGTERM") {
      return EQ;
    }
    fail();
  },
  Eq0: () => eqExitCode
};
var enumExitCode = {
  succ: (v) => {
    if (v === "Success") {
      return $Maybe("Just", $$Error);
    }
    if (v === "Error") {
      return $Maybe("Just", MisuseOfShellBuiltins);
    }
    if (v === "MisuseOfShellBuiltins") {
      return $Maybe("Just", CLIUsageError);
    }
    if (v === "CLIUsageError") {
      return $Maybe("Just", DataFormatError);
    }
    if (v === "DataFormatError") {
      return $Maybe("Just", CannotOpenInput);
    }
    if (v === "CannotOpenInput") {
      return $Maybe("Just", AddresseeUnknown);
    }
    if (v === "AddresseeUnknown") {
      return $Maybe("Just", HostNameUnknown);
    }
    if (v === "HostNameUnknown") {
      return $Maybe("Just", ServiceUnavailable);
    }
    if (v === "ServiceUnavailable") {
      return $Maybe("Just", InternalSoftwareError);
    }
    if (v === "InternalSoftwareError") {
      return $Maybe("Just", SystemError);
    }
    if (v === "SystemError") {
      return $Maybe("Just", CriticalOSFileMissing);
    }
    if (v === "CriticalOSFileMissing") {
      return $Maybe("Just", CannotCreateOutputFile);
    }
    if (v === "CannotCreateOutputFile") {
      return $Maybe("Just", IOError);
    }
    if (v === "IOError") {
      return $Maybe("Just", TemporaryFailure);
    }
    if (v === "TemporaryFailure") {
      return $Maybe("Just", RemoteError);
    }
    if (v === "RemoteError") {
      return $Maybe("Just", PermissionDenied);
    }
    if (v === "PermissionDenied") {
      return $Maybe("Just", ConfigurationError);
    }
    if (v === "ConfigurationError") {
      return $Maybe("Just", CannotExecute);
    }
    if (v === "CannotExecute") {
      return $Maybe("Just", CommandNotFound);
    }
    if (v === "CommandNotFound") {
      return $Maybe("Just", InvalidExitArgument);
    }
    if (v === "InvalidExitArgument") {
      return $Maybe("Just", SIGHUP);
    }
    if (v === "SIGHUP") {
      return $Maybe("Just", SIGINT);
    }
    if (v === "SIGINT") {
      return $Maybe("Just", SIGQUIT);
    }
    if (v === "SIGQUIT") {
      return $Maybe("Just", SIGILL);
    }
    if (v === "SIGILL") {
      return $Maybe("Just", SIGABRT);
    }
    if (v === "SIGABRT") {
      return $Maybe("Just", SIGFPE);
    }
    if (v === "SIGFPE") {
      return $Maybe("Just", SIGKILL);
    }
    if (v === "SIGKILL") {
      return $Maybe("Just", SIGSEGV);
    }
    if (v === "SIGSEGV") {
      return $Maybe("Just", SIGPIPE);
    }
    if (v === "SIGPIPE") {
      return $Maybe("Just", SIGALRM);
    }
    if (v === "SIGALRM") {
      return $Maybe("Just", SIGTERM);
    }
    if (v === "SIGTERM") {
      return Nothing;
    }
    fail();
  },
  pred: (v) => {
    if (v === "Success") {
      return Nothing;
    }
    if (v === "Error") {
      return $Maybe("Just", Success);
    }
    if (v === "MisuseOfShellBuiltins") {
      return $Maybe("Just", $$Error);
    }
    if (v === "CLIUsageError") {
      return $Maybe("Just", MisuseOfShellBuiltins);
    }
    if (v === "DataFormatError") {
      return $Maybe("Just", CLIUsageError);
    }
    if (v === "CannotOpenInput") {
      return $Maybe("Just", DataFormatError);
    }
    if (v === "AddresseeUnknown") {
      return $Maybe("Just", CannotOpenInput);
    }
    if (v === "HostNameUnknown") {
      return $Maybe("Just", AddresseeUnknown);
    }
    if (v === "ServiceUnavailable") {
      return $Maybe("Just", HostNameUnknown);
    }
    if (v === "InternalSoftwareError") {
      return $Maybe("Just", ServiceUnavailable);
    }
    if (v === "SystemError") {
      return $Maybe("Just", InternalSoftwareError);
    }
    if (v === "CriticalOSFileMissing") {
      return $Maybe("Just", SystemError);
    }
    if (v === "CannotCreateOutputFile") {
      return $Maybe("Just", CriticalOSFileMissing);
    }
    if (v === "IOError") {
      return $Maybe("Just", CannotCreateOutputFile);
    }
    if (v === "TemporaryFailure") {
      return $Maybe("Just", IOError);
    }
    if (v === "RemoteError") {
      return $Maybe("Just", TemporaryFailure);
    }
    if (v === "PermissionDenied") {
      return $Maybe("Just", RemoteError);
    }
    if (v === "ConfigurationError") {
      return $Maybe("Just", PermissionDenied);
    }
    if (v === "CannotExecute") {
      return $Maybe("Just", ConfigurationError);
    }
    if (v === "CommandNotFound") {
      return $Maybe("Just", CannotExecute);
    }
    if (v === "InvalidExitArgument") {
      return $Maybe("Just", CommandNotFound);
    }
    if (v === "SIGHUP") {
      return $Maybe("Just", InvalidExitArgument);
    }
    if (v === "SIGINT") {
      return $Maybe("Just", SIGHUP);
    }
    if (v === "SIGQUIT") {
      return $Maybe("Just", SIGINT);
    }
    if (v === "SIGILL") {
      return $Maybe("Just", SIGQUIT);
    }
    if (v === "SIGABRT") {
      return $Maybe("Just", SIGILL);
    }
    if (v === "SIGFPE") {
      return $Maybe("Just", SIGABRT);
    }
    if (v === "SIGKILL") {
      return $Maybe("Just", SIGFPE);
    }
    if (v === "SIGSEGV") {
      return $Maybe("Just", SIGKILL);
    }
    if (v === "SIGPIPE") {
      return $Maybe("Just", SIGSEGV);
    }
    if (v === "SIGALRM") {
      return $Maybe("Just", SIGPIPE);
    }
    if (v === "SIGTERM") {
      return $Maybe("Just", SIGALRM);
    }
    fail();
  },
  Ord0: () => ordExitCode
};
var boundedExitCode = { bottom: Success, top: SIGTERM, Ord0: () => ordExitCode };
var boundedEnumExitCode = {
  cardinality: 32,
  toEnum: (v) => {
    if (v === 0) {
      return $Maybe("Just", Success);
    }
    if (v === 1) {
      return $Maybe("Just", $$Error);
    }
    if (v === 2) {
      return $Maybe("Just", MisuseOfShellBuiltins);
    }
    if (v === 64) {
      return $Maybe("Just", CLIUsageError);
    }
    if (v === 65) {
      return $Maybe("Just", DataFormatError);
    }
    if (v === 66) {
      return $Maybe("Just", CannotOpenInput);
    }
    if (v === 67) {
      return $Maybe("Just", AddresseeUnknown);
    }
    if (v === 68) {
      return $Maybe("Just", HostNameUnknown);
    }
    if (v === 69) {
      return $Maybe("Just", ServiceUnavailable);
    }
    if (v === 70) {
      return $Maybe("Just", InternalSoftwareError);
    }
    if (v === 71) {
      return $Maybe("Just", SystemError);
    }
    if (v === 72) {
      return $Maybe("Just", CriticalOSFileMissing);
    }
    if (v === 73) {
      return $Maybe("Just", CannotCreateOutputFile);
    }
    if (v === 74) {
      return $Maybe("Just", IOError);
    }
    if (v === 75) {
      return $Maybe("Just", TemporaryFailure);
    }
    if (v === 76) {
      return $Maybe("Just", RemoteError);
    }
    if (v === 77) {
      return $Maybe("Just", PermissionDenied);
    }
    if (v === 78) {
      return $Maybe("Just", ConfigurationError);
    }
    if (v === 126) {
      return $Maybe("Just", CannotExecute);
    }
    if (v === 127) {
      return $Maybe("Just", CommandNotFound);
    }
    if (v === 128) {
      return $Maybe("Just", InvalidExitArgument);
    }
    if (v === 129) {
      return $Maybe("Just", SIGHUP);
    }
    if (v === 130) {
      return $Maybe("Just", SIGINT);
    }
    if (v === 131) {
      return $Maybe("Just", SIGQUIT);
    }
    if (v === 132) {
      return $Maybe("Just", SIGILL);
    }
    if (v === 134) {
      return $Maybe("Just", SIGABRT);
    }
    if (v === 136) {
      return $Maybe("Just", SIGFPE);
    }
    if (v === 137) {
      return $Maybe("Just", SIGKILL);
    }
    if (v === 139) {
      return $Maybe("Just", SIGSEGV);
    }
    if (v === 141) {
      return $Maybe("Just", SIGPIPE);
    }
    if (v === 142) {
      return $Maybe("Just", SIGALRM);
    }
    if (v === 143) {
      return $Maybe("Just", SIGTERM);
    }
    return Nothing;
  },
  fromEnum: (v) => {
    if (v === "Success") {
      return 0;
    }
    if (v === "Error") {
      return 1;
    }
    if (v === "MisuseOfShellBuiltins") {
      return 2;
    }
    if (v === "CLIUsageError") {
      return 64;
    }
    if (v === "DataFormatError") {
      return 65;
    }
    if (v === "CannotOpenInput") {
      return 66;
    }
    if (v === "AddresseeUnknown") {
      return 67;
    }
    if (v === "HostNameUnknown") {
      return 68;
    }
    if (v === "ServiceUnavailable") {
      return 69;
    }
    if (v === "InternalSoftwareError") {
      return 70;
    }
    if (v === "SystemError") {
      return 71;
    }
    if (v === "CriticalOSFileMissing") {
      return 72;
    }
    if (v === "CannotCreateOutputFile") {
      return 73;
    }
    if (v === "IOError") {
      return 74;
    }
    if (v === "TemporaryFailure") {
      return 75;
    }
    if (v === "RemoteError") {
      return 76;
    }
    if (v === "PermissionDenied") {
      return 77;
    }
    if (v === "ConfigurationError") {
      return 78;
    }
    if (v === "CannotExecute") {
      return 126;
    }
    if (v === "CommandNotFound") {
      return 127;
    }
    if (v === "InvalidExitArgument") {
      return 128;
    }
    if (v === "SIGHUP") {
      return 129;
    }
    if (v === "SIGINT") {
      return 130;
    }
    if (v === "SIGQUIT") {
      return 131;
    }
    if (v === "SIGILL") {
      return 132;
    }
    if (v === "SIGABRT") {
      return 134;
    }
    if (v === "SIGFPE") {
      return 136;
    }
    if (v === "SIGKILL") {
      return 137;
    }
    if (v === "SIGSEGV") {
      return 139;
    }
    if (v === "SIGPIPE") {
      return 141;
    }
    if (v === "SIGALRM") {
      return 142;
    }
    if (v === "SIGTERM") {
      return 143;
    }
    fail();
  },
  Bounded0: () => boundedExitCode,
  Enum1: () => enumExitCode
};

// output-es/Data.Monoid/index.js
var monoidArray = { mempty: [], Semigroup0: () => semigroupArray };
var monoidRecord = () => (dictMonoidRecord) => {
  const semigroupRecord1 = { append: dictMonoidRecord.SemigroupRecord0().appendRecord($$Proxy) };
  return { mempty: dictMonoidRecord.memptyRecord($$Proxy), Semigroup0: () => semigroupRecord1 };
};

// output-es/File/index.js
var Folder = (x) => x;

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

// output-es/Data.String.Regex/foreign.js
var regexImpl = function(left) {
  return function(right) {
    return function(s1) {
      return function(s2) {
        try {
          return right(new RegExp(s1, s2));
        } catch (e) {
          return left(e.message);
        }
      };
    };
  };
};
var split2 = function(r) {
  return function(s) {
    return s.split(r);
  };
};

// output-es/Data.String.Regex/index.js
var regex = (s) => (f) => regexImpl(Left)(Right)(s)((f.global ? "g" : "") + (f.ignoreCase ? "i" : "") + (f.multiline ? "m" : "") + (f.dotAll ? "s" : "") + (f.sticky ? "y" : "") + (f.unicode ? "u" : ""));

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

// output-es/Data.String.Regex.Flags/index.js
var noFlags = { global: false, ignoreCase: false, multiline: false, dotAll: false, sticky: false, unicode: false };

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
var toUnfoldable10 = /* @__PURE__ */ toUnfoldable2(unfoldableArray);
var theReservedNames = (v) => {
  if (v.caseSensitive) {
    return sortBy(ordString.compare)(v.reservedNames);
  }
  return sortBy(ordString.compare)(arrayMap(toLower)(v.reservedNames));
};
var oneLineComment = (v) => {
  const $0 = skipMany(satisfy((v1) => v1 !== "\n"));
  return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => {
    const $1 = state1._3;
    return string2(v.commentLine)(
      state1,
      more,
      lift12,
      (v2$1, $2) => $$throw2($ParseState(v2$1._1, v2$1._2, $1), $2),
      (state2, a) => more((v2$1) => more((v3) => $0(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
    );
  }));
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
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => $12(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => go$lazy()(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
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
  return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => {
    const $1 = state1._3;
    return string2(v.commentStart)(
      state1,
      more,
      lift12,
      (v2$1, $2) => $$throw2($ParseState(v2$1._1, v2$1._2, $1), $2),
      (state2, a) => more((v2$1) => more((v3) => $0(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
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
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => $22(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => go$lazy()(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
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
  const $3 = some(alternativeParserT)(lazyParserT)(octDigit);
  const octal = (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => $2(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => more((v1$1) => $3(
      state2,
      more,
      lift12,
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
          return fail2("not digits")(state2$1, more, lift12, $$throw2, (state3, a$2) => more((v4) => done(state3, a$2)));
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
  const semi2 = (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(";")(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => $4(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ";"))))))
  ))));
  const $5 = oneOf(["x", "X"]);
  const $6 = some(alternativeParserT)(lazyParserT)(hexDigit);
  const hexadecimal = (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => $5(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => more((v1$1) => $6(
      state2,
      more,
      lift12,
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
          return fail2("not digits")(state2$1, more, lift12, $$throw2, (state3, a$2) => more((v4) => done(state3, a$2)));
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
    return (state1, more, lift12, $$throw2, done) => more((v1) => $72(
      state1,
      more,
      lift12,
      $$throw2,
      (state2, a) => more((v2) => {
        const $82 = withErrorMessage(some(alternativeParserT)(lazyParserT)(digit))("fraction");
        return more((v1$1) => $82(
          state2,
          more,
          lift12,
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
              return fail2("not digit")(state2$1, more, lift12, $$throw2, done);
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
    const $72 = some(alternativeParserT)(lazyParserT)(space);
    const $82 = withErrorMessage(satisfy((v$1) => v$1 === "\\"))("'\\\\'");
    return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => $72(
      state1,
      more,
      lift12,
      $$throw2,
      (state2, a) => more((v2$1) => more((v3) => $82(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
    )));
  })())("end of string gap");
  const escapeEmpty = withErrorMessage(satisfy((v$1) => v$1 === "&"))("'&'");
  const $7 = some(alternativeParserT)(lazyParserT)(digit);
  const decimal = (state1, more, lift12, $$throw2, done) => more((v1) => $7(
    state1,
    more,
    lift12,
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
        return fail2("not digits")(state2, more, lift12, $$throw2, done);
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
  const exponent$p = withErrorMessage((state1, more, lift12, $$throw2, done) => more((v1) => $8(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2) => more((v1$1) => sign1(
      state2,
      more,
      lift12,
      $$throw2,
      (state2$1, a$1) => more((v2$1) => {
        const $92 = withErrorMessage(decimal)("exponent");
        return more((v1$2) => $92(state2$1, more, lift12, $$throw2, (state2$2, a$2) => more((v2$2) => done(state2$2, power(a$1(a$2))))));
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
  const decimalFloat = (state1, more, lift12, $$throw2, done) => more((v1) => decimal(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2) => {
      const $92 = fractExponent(a);
      const $102 = state2._1;
      const $112 = state2._2;
      return more((v3) => more((v1$1) => $92(
        $ParseState($102, $112, false),
        more,
        lift12,
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
    return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => $92(
      state1,
      more,
      lift12,
      $$throw2,
      (state2, a) => more((v2$1) => more((v3) => {
        const $102 = state2._1;
        const $112 = state2._2;
        return more((v3$1) => hexadecimal(
          $ParseState($102, $112, false),
          more,
          lift12,
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
                lift12,
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
                      lift12,
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
  const comma2 = (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(",")(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => $9(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ","))))))
  ))));
  const $10 = choice3(arrayMap((v1) => {
    const $102 = v1._1;
    const $112 = v1._2;
    const $122 = withErrorMessage(satisfy((v$1) => v$1 === $102))(showCharImpl($102));
    return (state1, more, lift12, $$throw2, done) => more((v1$1) => $122(state1, more, lift12, $$throw2, (state2, a) => more((v2) => done(state2, $112))));
  })(zip(["a", "b", "f", "n", "r", "t", "v", "\\", '"', "'"])(["\x07", "\b", "\f", "\n", "\r", "	", "\v", "\\", '"', "'"])));
  const $11 = withErrorMessage(satisfy((v$1) => v$1 === "o"))("'o'");
  const $12 = some(alternativeParserT)(lazyParserT)(octDigit);
  const $13 = withErrorMessage(satisfy((v$1) => v$1 === "x"))("'x'");
  const $14 = some(alternativeParserT)(lazyParserT)(hexDigit);
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
    return (state1, more, lift12, $$throw2, done) => more((v1) => $162(
      state1,
      more,
      lift12,
      $$throw2,
      (state2, a) => more((v2) => more((v1$1) => upper2(
        state2,
        more,
        lift12,
        $$throw2,
        (state2$1, a$1) => more((v2$1) => {
          const $17 = (toCharCode(a$1) - 65 | 0) + 1 | 0;
          if ($17 >= -2147483648 && $17 <= 2147483647) {
            return done(state2$1, fromCharCode($17));
          }
          return fail2("invalid character code (should not happen)")(state2$1, more, lift12, $$throw2, done);
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
      const $17 = withErrorMessage((state1, more, lift12, $$throw2, done) => more((v1) => v.identStart(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2) => {
          const $172 = many(alternativeParserT)(lazyParserT)(v.identLetter);
          return more((v1$1) => $172(
            state2,
            more,
            lift12,
            $$throw2,
            (state2$1, a$1) => more((v2$1) => done(state2$1, singleton(a) + fromCharArray(a$1)))
          ));
        })
      )))("identifier");
      const $18 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => {
        const $19 = (state2, a) => more((v2$1) => more((v3) => $18(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))));
        const $20 = state1._3;
        return more((v1$1) => $17(
          state1,
          more,
          lift12,
          (v2$1, $21) => $$throw2($ParseState(v2$1._1, v2$1._2, $20), $21),
          (state2, a) => more((v2$1) => {
            if (isReserved(theReservedNames(v))(v.caseSensitive ? a : toLower(a))) {
              return fail2("reserved word " + showStringImpl(a))(
                state2,
                more,
                lift12,
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
          return (state1, more, lift12, $$throw2, done) => more((v1) => string2(name2)(state1, more, lift12, $$throw2, (state2, a) => more((v2) => done(state2, name2))));
        }
        const msg = showStringImpl(name2);
        const walk = (name$p) => {
          const v1 = uncons2(name$p);
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
            return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1$1) => $173(
              state1,
              more,
              lift12,
              $$throw2,
              (state2, a) => more((v2$1) => more((v3) => $182(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
            )));
          }
          fail();
        };
        const $172 = walk(name2);
        return (state1, more, lift12, $$throw2, done) => more((v1) => $172(state1, more, lift12, $$throw2, (state2, a) => more((v2) => done(state2, name2))));
      })();
      const $18 = withErrorMessage(notFollowedBy(v.identLetter))("end of " + name2);
      const $19 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => {
        const $20 = state1._3;
        return more((v2$1) => more((v1$1) => $17(
          state1,
          more,
          lift12,
          (v2$2, $21) => $$throw2($ParseState(v2$2._1, v2$2._2, $20), $21),
          (state2, a) => more((v2$2) => more((v3) => $18(
            state2,
            more,
            lift12,
            (v2$3, $21) => $$throw2($ParseState(v2$3._1, v2$3._2, $20), $21),
            (state3, a$1) => more((v4) => more((v2$3) => more((v3$1) => $19(state3, more, lift12, $$throw2, (state3$1, a$2) => more((v4$1) => done(state3$1, a$1))))))
          )))
        )));
      }));
    },
    operator: (() => {
      const $17 = withErrorMessage((state1, more, lift12, $$throw2, done) => more((v1) => v.opStart(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2) => {
          const $172 = many(alternativeParserT)(lazyParserT)(v.opLetter);
          return more((v1$1) => $172(
            state2,
            more,
            lift12,
            $$throw2,
            (state2$1, a$1) => more((v2$1) => done(state2$1, singleton(a) + fromCharArray(a$1)))
          ));
        })
      )))("operator");
      const $18 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => {
        const $19 = (state2, a) => more((v2$1) => more((v3) => $18(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))));
        const $20 = state1._3;
        return more((v1$1) => $17(
          state1,
          more,
          lift12,
          (v2$1, $21) => $$throw2($ParseState(v2$1._1, v2$1._2, $20), $21),
          (state2, a) => more((v2$1) => {
            if (isReserved(sortBy(ordString.compare)(v.reservedOpNames))(a)) {
              return fail2("reserved operator " + a)(state2, more, lift12, (v2$2, $21) => $$throw2($ParseState(v2$2._1, v2$2._2, $20), $21), $19);
            }
            return $19(state2, a);
          })
        ));
      }));
    })(),
    reservedOp: (name2) => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => {
        const $18 = state1._3;
        return more((v1$1) => string2(name2)(
          state1,
          more,
          lift12,
          (v2$1, $19) => $$throw2($ParseState(v2$1._1, v2$1._2, $18), $19),
          (state2, a) => more((v2$1) => withErrorMessage(notFollowedBy(v.opLetter))("end of " + name2)(
            state2,
            more,
            lift12,
            (v2$2, $19) => $$throw2($ParseState(v2$2._1, v2$2._2, $18), $19),
            (state2$1, a$1) => more((v2$2) => more((v3) => $17(state2$1, more, lift12, $$throw2, (state3, a$2) => more((v4) => done(state3, a$1)))))
          ))
        ));
      }));
    },
    charLiteral: withErrorMessage((() => {
      const $17 = between(withErrorMessage(satisfy((v$1) => v$1 === "'"))("'\\''"))(withErrorMessage(withErrorMessage(satisfy((v$1) => v$1 === "'"))("'\\''"))("end of character"))((() => {
        const $172 = satisfy((c) => c !== "'" && c !== "\\" && c > "");
        const $182 = withErrorMessage((() => {
          const $183 = withErrorMessage(satisfy((v$1) => v$1 === "\\"))("'\\\\'");
          return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => $183(
            state1,
            more,
            lift12,
            $$throw2,
            (state2, a) => more((v2$1) => more((v3) => escapeCode(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
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
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => $17(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $18(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))))
      )));
    })())("character"),
    stringLiteral: (() => {
      const $17 = withErrorMessage((() => {
        const $172 = between(withErrorMessage(satisfy((v$1) => v$1 === '"'))(`'"'`))(withErrorMessage(withErrorMessage(satisfy((v$1) => v$1 === '"'))(`'"'`))("end of string"))(many2(alternativeParserT)(lazyParserT)((() => {
          const $173 = satisfy((c) => c !== '"' && c !== "\\" && c > "");
          const $182 = withErrorMessage((() => {
            const $183 = withErrorMessage(satisfy((v$1) => v$1 === "\\"))("'\\\\'");
            return (state1, more, lift12, $$throw2, done) => more((v1) => $183(
              state1,
              more,
              lift12,
              $$throw2,
              (state2, a) => more((v2) => {
                const $19 = state2._1;
                const $20 = state2._2;
                return more((v3) => more((v1$1) => escapeGap(
                  $ParseState($19, $20, false),
                  more,
                  lift12,
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
                        lift12,
                        (v4$1, $25) => {
                          const $26 = v4$1._3;
                          return more((v5$1) => {
                            if ($26) {
                              return $$throw2(v4$1, $25);
                            }
                            return more((v1$3) => escapeCode(state2, more, lift12, $$throw2, (state2$1, a$1) => more((v2$1) => done(state2$1, $Maybe("Just", a$1)))));
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
        return (state1, more, lift12, $$throw2, done) => more((v1) => $172(
          state1,
          more,
          lift12,
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
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => $17(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $18(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))))
      )));
    })(),
    natural: withErrorMessage((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => {
        const $18 = (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))));
        const $19 = state1._1;
        const $20 = state1._2;
        return more((v3) => zeroNumber(
          $ParseState($19, $20, false),
          more,
          lift12,
          (v4, $21) => {
            const $22 = v4._3;
            return more((v5) => {
              if ($22) {
                return $$throw2(v4, $21);
              }
              return decimal(state1, more, lift12, $$throw2, $18);
            });
          },
          $18
        ));
      }));
    })())("natural"),
    integer: withErrorMessage((() => {
      const $17 = whiteSpace$p(v);
      const $18 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => {
        const $19 = (state2, a) => more((v2$1) => more((v3) => $18(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))));
        return more((v1$1) => more((v2$1) => more((v1$2) => sign1(
          state1,
          more,
          lift12,
          $$throw2,
          (state2, a) => more((v2$2) => more((v3) => $17(
            state2,
            more,
            lift12,
            $$throw2,
            (state3, a$1) => more((v4) => more((v2$3) => more((v1$3) => {
              const $20 = state3._1;
              const $21 = state3._2;
              return more((v3$1) => zeroNumber(
                $ParseState($20, $21, false),
                more,
                lift12,
                (v4$1, $22) => {
                  const $23 = v4$1._3;
                  return more((v5) => {
                    if ($23) {
                      return $$throw2(v4$1, $22);
                    }
                    return decimal(state3, more, lift12, $$throw2, (state2$1, a$2) => more((v2$4) => $19(state2$1, a(a$2))));
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
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => more((v1$1) => decimal(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => fractExponent(a)(
          state2,
          more,
          lift12,
          $$throw2,
          (state2$1, a$1) => more((v2$2) => more((v3) => $17(state2$1, more, lift12, $$throw2, (state3, a$2) => more((v4) => done(state3, a$1)))))
        ))
      ))));
    })())("float"),
    naturalOrFloat: withErrorMessage((() => {
      const $17 = withErrorMessage(satisfy((v$1) => v$1 === "0"))("'0'");
      const $18 = fractExponent(0);
      const $19 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => {
        const $20 = (state2, a) => more((v2$1) => more((v3) => $19(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))));
        const $21 = state1._1;
        const $22 = state1._2;
        return more((v3) => {
          const $23 = (v4, $232) => {
            const $24 = v4._3;
            return more((v5) => {
              if ($24) {
                return $$throw2(v4, $232);
              }
              return decimalFloat(state1, more, lift12, $$throw2, $20);
            });
          };
          return more((v2$1) => more((v1$1) => $17(
            $ParseState($21, $22, false),
            more,
            lift12,
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
                      lift12,
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
                            lift12,
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
                  lift12,
                  (v4, $27) => {
                    const $28 = v4._3;
                    return more((v5) => {
                      if ($28) {
                        return $26(v4, $27);
                      }
                      return octal(
                        $ParseState($24, $25, false),
                        more,
                        lift12,
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
      return (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(name2)(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, name2))))))
      ))));
    },
    lexeme: (p) => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => p(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))))
      )));
    },
    whiteSpace: whiteSpace$p(v),
    parens: (p) => between((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2("(")(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "("))))))
      ))));
    })())((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(")")(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ")"))))))
      ))));
    })())(p),
    braces: (p) => between((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2("{")(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "{"))))))
      ))));
    })())((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2("}")(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "}"))))))
      ))));
    })())(p),
    angles: (p) => between((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2("<")(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "<"))))))
      ))));
    })())((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(">")(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ">"))))))
      ))));
    })())(p),
    brackets: (p) => between((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2("[")(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "["))))))
      ))));
    })())((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2("]")(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "]"))))))
      ))));
    })())(p),
    semi: semi2,
    comma: comma2,
    colon: (() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(":")(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, ":"))))))
      ))));
    })(),
    dot: (() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => string2(".")(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2$1) => more((v3) => $17(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => more((v2$2) => done(state3, "."))))))
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
  const $0 = some2(alternativeParserT)(lazyParserT)(p);
  return (state1, more, lift12, $$throw2, done) => more((v1) => $0(state1, more, lift12, $$throw2, (state2, a) => more((v2) => done(state2, nonEmptyListNonEmptyList.nonEmpty(a)))));
};
var sepBy1_try = (p) => (sep) => {
  const $0 = many2(alternativeParserT)(lazyParserT)((v1, $02, $1, $2, $3) => {
    const $4 = v1._3;
    return $02((v2) => $02((v1$1) => sep(
      v1,
      $02,
      $1,
      (v2$1, $5) => $2($ParseState(v2$1._1, v2$1._2, $4), $5),
      (state2, a) => $02((v2$1) => $02((v3) => p(state2, $02, $1, (v2$2, $5) => $2($ParseState(v2$2._1, v2$2._2, $4), $5), (state3, a$1) => $02((v4) => $3(state3, a$1)))))
    )));
  });
  return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => p(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => $0(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, $NonEmpty(a, a$1))))))
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
  return (state1, more, lift12, $$throw2, done) => more((v1) => $0(state1, more, lift12, $$throw2, (state2, a$1) => more((v2) => done(state2, a))));
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
)))(listMap(fromFoldable19)(groupBy2((x) => (y) => x.prec === y.prec)(sortBy2((x) => (x$1) => {
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
  return (state1, more, lift12, $$throw2, done) => more((v1) => $0(state1, more, lift12, $$throw2, (state2, a) => more((v2) => done(state2, void 0))));
})();
var topLevel = (p) => (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => more((v2$1) => more((v1$1) => token.whiteSpace(
  state1,
  more,
  lift12,
  $$throw2,
  (state2, a) => more((v2$2) => more((v3) => p(
    state2,
    more,
    lift12,
    $$throw2,
    (state3, a$1) => more((v4) => more((v2$3) => more((v3$1) => eof(state3, more, lift12, $$throw2, (state3$1, a$2) => more((v4$1) => done(state3$1, a$1))))))
  )))
)))));
var lBracket = /* @__PURE__ */ (() => {
  const $0 = token.symbol("[");
  return (state1, more, lift12, $$throw2, done) => more((v1) => $0(state1, more, lift12, $$throw2, (state2, a) => more((v2) => done(state2, void 0))));
})();
var lArrow = /* @__PURE__ */ (() => token.reservedOp("<-"))();
var keyword = (str$p) => {
  if (elem(eqString)(str$p)(languageDef.reservedNames)) {
    return token.reserved(str$p);
  }
  return throwException(error(str$p + " is not a reserved word"))();
};
var ident = (state1, more, lift12, $$throw2, done) => more((v1) => token.identifier(
  state1,
  more,
  lift12,
  $$throw2,
  (state2, a) => more((v2) => onlyIf(!isCtrName(a))(a)(state2, more, lift12, $$throw2, done))
));
var field = (p) => (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => ident(
  state1,
  more,
  lift12,
  $$throw2,
  (state2, a) => more((v2$1) => {
    const $0 = Tuple(a);
    return more((v3) => more((v2$2) => more((v1$1) => token.colon(
      state2,
      more,
      lift12,
      $$throw2,
      (state2$1, a$1) => more((v2$3) => more((v3$1) => p(state2$1, more, lift12, $$throw2, (state3, a$2) => more((v4) => more((v4$1) => done(state3, $0(a$2)))))))
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
var docCommentLetter = /* @__PURE__ */ satisfy((c) => c !== '"' && c !== "$" && !isSpace(toCharCode(c)));
var docCommentDelim = (state1, more, lift12, $$throw2, done) => more((v1) => string2('"""')(
  state1,
  more,
  lift12,
  $$throw2,
  (state2, a) => more((v2) => done(state2, void 0))
));
var ctr = (state1, more, lift12, $$throw2, done) => more((v1) => token.identifier(
  state1,
  more,
  lift12,
  $$throw2,
  (state2, a) => more((v2) => onlyIf(isCtrName(a))(a)(state2, more, lift12, $$throw2, done))
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
  const $1 = token.braces((state1, more, lift12, $$throw2, done) => more((v1) => sepBy(field(pattern$p))(token.comma)(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2) => done(state2, $Pattern("PRecord", a)))
  )));
  const $2 = token.parens(pattern$p);
  const $3 = token.parens((state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => pattern$p(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => token.comma(
      state2,
      more,
      lift12,
      $$throw2,
      (state3, a$1) => more((v4) => more((v2$2) => more((v1$2) => pattern$p(
        state3,
        more,
        lift12,
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
  const $0 = buildExprParser(operators((op) => (state1, more, lift12, $$throw2, done) => more((v1) => token.operator(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2) => onlyIf(":" === definitely("absurd")(charAt2(0)(a)) && op === a)((\u03C0) => (\u03C0$p) => $Pattern(
      "PConstr",
      a,
      $List("Cons", \u03C0, $List("Cons", \u03C0$p, Nil))
    ))(state2, more, lift12, $$throw2, done))
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
    return (state1, more, lift12, $$throw2, done) => more((v1) => $1(state1, more, lift12, $$throw2, (state2, a) => more((v2) => rest(a)(state2, more, lift12, $$throw2, done))));
  })())));
  const go = go$lazy();
  return go;
})();
var varDefs = (expr$p) => {
  const $0 = keyword("let");
  const $1 = sepBy1_try((state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => more((v2$1) => more((v1$1) => pattern(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2$2) => more((v3) => equals(
      state2,
      more,
      lift12,
      $$throw2,
      (state3, a$1) => more((v4) => more((v2$3) => {
        const $12 = VarDef2(a);
        return more((v3$1) => expr$p(state3, more, lift12, $$throw2, (state3$1, a$2) => more((v4$1) => done(state3$1, $12(a$2)))));
      }))
    )))
  ))))))(token.semi);
  return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => $0(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => $1(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
  )));
};
var commentToken = /* @__PURE__ */ (() => {
  const $0 = some(alternativeParserT)(lazyParserT)(docCommentLetter);
  return (state1, more, lift12, $$throw2, done) => more((v1) => more((v1$1) => $0(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2) => {
      const $1 = fromCharArray(a);
      return more((v2$1) => done(state2, $DocCommentElem("Token", $1)));
    })
  )));
})();
var commentExpr = (expr$p) => (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => string2("$")(
  state1,
  more,
  lift12,
  $$throw2,
  (state2, a) => more((v2$1) => more((v3) => more((v1$1) => between(string2("{"))(string2("}"))(expr$p)(
    state2,
    more,
    lift12,
    $$throw2,
    (state2$1, a$1) => more((v2$2) => more((v4) => done(state2$1, $DocCommentElem("Unquote", a$1))))
  ))))
)));
var docCommentToken = (expr$p) => (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => {
  const $0 = (state2, a) => more((v2$1) => more((v3) => token.whiteSpace(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))));
  return more((v2$1) => more((v1$1) => token.whiteSpace(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2$2) => more((v3) => {
      const $1 = state2._1;
      const $2 = state2._2;
      return more((v3$1) => commentToken(
        $ParseState($1, $2, false),
        more,
        lift12,
        (v2$3, $3) => more((v5) => commentExpr(expr$p)(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => $0(state3, a$1)))),
        (state3, a$1) => more((v4) => $0(state3, a$1))
      ));
    }))
  )));
}));
var docComment$p = (expr$p) => token.lexeme(withErrorMessage((() => {
  const $0 = between(docCommentDelim)(withErrorMessage(docCommentDelim)("end of docComment"))(many2(alternativeParserT)(lazyParserT)(docCommentToken(expr$p)));
  return (state1, more, lift12, $$throw2, done) => more((v1) => $0(state1, more, lift12, $$throw2, (state2, a) => more((v2) => done(state2, a))));
})())("docComment"));
var docComment = (expr$p) => {
  const $0 = docComment$p(expr$p);
  return (v2, $1, $2, $3, $4) => {
    const $5 = v2._1;
    const $6 = v2._2;
    return $1((v3) => $1((v1) => $0($ParseState($5, $6, false), $1, $2, (v2$1, $7) => $1((v5) => $4(v2, None)), (state2, a) => $1((v2$1) => $4(state2, $DocOpt("Doc", a))))));
  };
};
var clause_uncurried = (expr$p) => (delim) => (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => pattern(
  state1,
  more,
  lift12,
  $$throw2,
  (state2, a) => more((v2$1) => {
    const $0 = Tuple(a);
    return more((v3) => more((v2$2) => more((v1$1) => delim(
      state2,
      more,
      lift12,
      $$throw2,
      (state2$1, a$1) => more((v2$3) => more((v3$1) => expr$p(state2$1, more, lift12, $$throw2, (state3, a$2) => more((v4) => more((v4$1) => done(state3, $0(a$2)))))))
    ))));
  })
)));
var clause_curried = (expr$p) => (delim) => {
  const $0 = some3(simplePattern(pattern));
  return (state1, more, lift12, $$throw2, done) => more((v1) => more((v2) => more((v1$1) => $0(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2$1) => {
      const $1 = Tuple(a);
      return more((v3) => more((v2$2) => more((v1$2) => delim(
        state2,
        more,
        lift12,
        $$throw2,
        (state2$1, a$1) => more((v2$3) => more((v3$1) => expr$p(
          state2$1,
          more,
          lift12,
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
    return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => ident(
      state1,
      more,
      lift12,
      $$throw2,
      (state2, a) => more((v2$1) => {
        const $2 = Tuple(a);
        return more((v3) => $12(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, $2(a$1)))));
      })
    )));
  })())(token.semi);
  return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => $0(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2$1) => more((v3) => $1(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a$1)))))
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
  return (state1, more, lift12, $$throw2, done) => more((v1) => $0(
    state1,
    more,
    lift12,
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
  return (state1, more, lift12, $$throw2, done) => more((v1) => $0(state1, more, lift12, $$throw2, (state2, a) => more((v2) => done(state2, void 0))));
})();
var expr_$lazy = /* @__PURE__ */ binding(() => {
  const $0 = buildExprParser([
    [
      $Operator(
        "Infix",
        (state1, more, lift12, $$throw2, done) => more((v1) => between(backtick)(backtick)(ident)(
          state1,
          more,
          lift12,
          $$throw2,
          (state2, a) => more((v2) => done(state2, (e) => (e$p) => $Expr2("BinaryApp", e, a, e$p)))
        )),
        AssocLeft
      )
    ],
    ...operators((op) => (state1, more, lift12, $$throw2, done) => more((v1) => token.operator(
      state1,
      more,
      lift12,
      $$throw2,
      (state2, a) => more((v2) => onlyIf(op === a)((() => {
        if (op === ".") {
          return (e) => (e$p) => {
            if (e$p.tag === "Var") {
              return $Expr2("Project", None, e, e$p._1);
            }
            return throwException(error('Field names are not first class; got "' + intercalate4("\n")(removeDocWS(prettyExpr1(annUnit).pretty(e$p)).lines) + '".'))();
          };
        }
        if (":" === definitely("absurd")(charAt2(0)(a))) {
          return (e) => (e$p) => $Expr2("Constr", void 0, None, a, $List("Cons", e, $List("Cons", e$p, Nil)));
        }
        return (e) => (e$p) => $Expr2("BinaryApp", e, op, e$p);
      })())(state2, more, lift12, $$throw2, done))
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
    const $9 = sepBy1(defs(go$lazy()))(token.semi);
    const simpleExprOrProjection = (doc) => {
      const $102 = withErrorMessage(satisfy((v$1) => v$1 === "-"))("'-'");
      const $11 = withErrorMessage(satisfy((v$1) => v$1 === "+"))("'+'");
      const $12 = withErrorMessage(satisfy((v$1) => v$1 === "-"))("'-'");
      const $13 = withErrorMessage(satisfy((v$1) => v$1 === "+"))("'+'");
      const $14 = between(token.symbol("[|"))(token.symbol("|]"))((() => {
        const $142 = Matrix2()(doc);
        const $152 = token.parens((state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => ident(
          state1,
          more,
          lift12,
          $$throw2,
          (state2, a) => more((v2$1) => {
            const $153 = Tuple(a);
            return more((v3) => more((v2$2) => more((v1$1) => token.comma(
              state2,
              more,
              lift12,
              $$throw2,
              (state2$1, a$1) => more((v2$3) => more((v3$1) => ident(state2$1, more, lift12, $$throw2, (state3, a$2) => more((v4) => more((v4$1) => done(state3, $153(a$2)))))))
            ))));
          })
        ))));
        const $162 = keyword("in");
        return (state1, more, lift12, $$throw2, done) => more((v2) => more((v2$1) => more((v1) => more((v2$2) => more((v1$1) => go$lazy()(
          state1,
          more,
          lift12,
          $$throw2,
          (state2, a) => more((v2$3) => more((v3) => bar(
            state2,
            more,
            lift12,
            $$throw2,
            (state3, a$1) => more((v4) => more((v2$4) => {
              const $172 = $142(a);
              return more((v3$1) => $152(
                state3,
                more,
                lift12,
                $$throw2,
                (state3$1, a$2) => more((v4$1) => {
                  const $182 = $172(a$2);
                  return more((v3$2) => more((v2$5) => more((v1$2) => $162(
                    state3$1,
                    more,
                    lift12,
                    $$throw2,
                    (state2$1, a$3) => more((v2$6) => more((v3$3) => go$lazy()(state2$1, more, lift12, $$throw2, (state3$2, a$4) => more((v4$2) => more((v4$3) => done(state3$2, $182(a$4)))))))
                  ))));
                })
              ));
            }))
          )))
        ))))));
      })());
      const $15 = token.brackets((state1, v$1, v1, v2, done) => done(state1, $Expr2("ListEmpty", void 0, doc)));
      const $16 = ListNonEmpty()(doc);
      const go$1$lazy = binding(() => lazyParserT.defer((v$1) => {
        const $172 = Next();
        return (v2, $182, $192, $202, $21) => {
          const $22 = v2._1;
          const $23 = v2._2;
          return $182((v3) => $182((v2$1) => $182((v1) => rBracket(
            $ParseState($22, $23, false),
            $182,
            $192,
            (v4, $24) => {
              const $25 = v4._3;
              return $182((v5) => {
                if ($25) {
                  return $202(v4, $24);
                }
                return $182((v2$2) => $182((v1$1) => token.comma(
                  v2,
                  $182,
                  $192,
                  $202,
                  (state2, a) => $182((v2$3) => $182((v3$1) => $182((v2$4) => $182((v1$2) => go$lazy()(
                    state2,
                    $182,
                    $192,
                    $202,
                    (state2$1, a$1) => $182((v2$5) => {
                      const $26 = $172(a$1);
                      return $182((v3$2) => go$1$lazy()(
                        state2$1,
                        $182,
                        $192,
                        $202,
                        (state3, a$2) => $182((v4$1) => {
                          const $27 = $26(a$2);
                          return $182((v4$2) => $21(state3, $27));
                        })
                      ));
                    })
                  )))))
                )));
              });
            },
            (state2, a) => $182((v2$2) => $182((v3$1) => $182((v4) => $21(state2, $ListRest("End", void 0)))))
          ))));
        };
      }));
      const go$1 = go$1$lazy();
      const $17 = Constr2()(doc);
      const $18 = token.braces((() => {
        const $182 = sepBy((() => {
          const $183 = token.brackets((state1, more, lift12, $$throw2, done) => more((v1) => go$lazy()(
            state1,
            more,
            lift12,
            $$throw2,
            (state2, a) => more((v2) => done(state2, $DictEntry("ExprKey", a)))
          )));
          const $193 = VarKey();
          return (v2, $202, $21, $22, $23) => {
            const $24 = v2._1;
            const $25 = v2._2;
            return $202((v3) => {
              const $26 = (v4, $262) => {
                const $27 = v4._3;
                return $202((v5) => {
                  if ($27) {
                    return $22(v4, $262);
                  }
                  return $202((v2$1) => $202((v1) => $202((v2$2) => $202((v1$1) => $202((v1$2) => ident(
                    v2,
                    $202,
                    $21,
                    $22,
                    (state2, a) => $202((v2$3) => {
                      const $28 = $193(a);
                      return $202((v2$4) => $202((v3$1) => token.colon(
                        state2,
                        $202,
                        $21,
                        $22,
                        (state3, a$1) => $202((v4$1) => $202((v2$5) => {
                          const $29 = Tuple($28);
                          return $202((v3$2) => go$lazy()(state3, $202, $21, $22, (state3$1, a$2) => $202((v4$2) => $23(state3$1, $29(a$2)))));
                        }))
                      )));
                    })
                  ))))));
                });
              };
              return $202((v2$1) => $202((v1) => $202((v2$2) => $202((v1$1) => $183(
                $ParseState($24, $25, false),
                $202,
                $21,
                $26,
                (state2, a) => $202((v2$3) => $202((v3$1) => token.colon(
                  state2,
                  $202,
                  $21,
                  $26,
                  (state3, a$1) => $202((v4) => $202((v2$4) => {
                    const $27 = Tuple(a);
                    return $202((v3$2) => go$lazy()(state3, $202, $21, $26, (state3$1, a$2) => $202((v4$1) => $23(state3$1, $27(a$2)))));
                  }))
                )))
              )))));
            });
          };
        })())(token.comma);
        const $192 = Dictionary2()(doc);
        return (state1, more, lift12, $$throw2, done) => more((v1) => $182(state1, more, lift12, $$throw2, (state2, a) => more((v2) => done(state2, $192(a)))));
      })());
      const $19 = Str()(doc);
      const $20 = (() => {
        const $202 = token.parens((state1, more, lift12, $$throw2, done) => more((v2) => more((v2$1) => more((v3) => more((v2$2) => more((v1) => go$lazy()(
          state1,
          more,
          lift12,
          $$throw2,
          (state2, a) => more((v2$3) => more((v3$1) => token.comma(
            state2,
            more,
            lift12,
            $$throw2,
            (state3, a$1) => more((v4) => more((v4$1) => more((v3$2) => go$lazy()(
              state3,
              more,
              lift12,
              $$throw2,
              (state3$1, a$2) => more((v4$2) => done(
                state3$1,
                $Expr2("Constr", void 0, doc, "Pair", $List("Cons", a, $List("Cons", a$2, Nil)))
              ))
            ))))
          )))
        )))))));
        const $21 = token.brackets((() => {
          const $212 = ListComp()(doc);
          const $222 = sepBy1((() => {
            const $223 = ListCompGen(None);
            const $232 = keyword("let");
            return (v2, $242, $25, $26, $27) => {
              const $28 = v2._1;
              const $29 = v2._2;
              return $242((v3) => {
                const $30 = (v4, $302) => {
                  const $31 = v4._3;
                  return $242((v5) => {
                    if ($31) {
                      return $26(v4, $302);
                    }
                    const $32 = v2._1;
                    const $33 = v2._2;
                    return $242((v3$1) => $242((v1) => {
                      const $34 = (v4$1, $342) => {
                        const $35 = v4$1._3;
                        return $242((v5$1) => {
                          if ($35) {
                            return $26(v4$1, $342);
                          }
                          return $242((v1$1) => go$lazy()(v2, $242, $25, $26, (state2, a) => $242((v2$1) => $27(state2, $Qualifier("ListCompGuard", a)))));
                        });
                      };
                      return $242((v2$1) => $242((v1$1) => $242((v2$2) => $242((v1$2) => $242((v2$3) => $242((v1$3) => $232(
                        $ParseState($32, $33, false),
                        $242,
                        $25,
                        $34,
                        (state2, a) => $242((v2$4) => $242((v3$2) => pattern(
                          state2,
                          $242,
                          $25,
                          $34,
                          (state3, a$1) => $242((v4$1) => $242((v2$5) => $242((v3$3) => equals(
                            state3,
                            $242,
                            $25,
                            $34,
                            (state3$1, a$2) => $242((v4$2) => $242((v2$6) => {
                              const $35 = VarDef2(a$1);
                              return $242((v3$4) => go$lazy()(
                                state3$1,
                                $242,
                                $25,
                                $34,
                                (state3$2, a$3) => $242((v4$3) => {
                                  const $36 = $35(a$3);
                                  return $242((v2$7) => $27(state3$2, $Qualifier("ListCompDecl", $36)));
                                })
                              ));
                            }))
                          ))))
                        )))
                      )))))));
                    }));
                  });
                };
                return $242((v2$1) => $242((v2$2) => $242((v1) => $242((v1$1) => pattern(
                  $ParseState($28, $29, false),
                  $242,
                  $25,
                  $30,
                  (state2, a) => $242((v2$3) => {
                    const $31 = $223(a);
                    return $242((v2$4) => $242((v3$1) => lArrow(
                      state2,
                      $242,
                      $25,
                      $30,
                      (state3, a$1) => $242((v4) => $242((v3$2) => go$lazy()(state3, $242, $25, $30, (state3$1, a$2) => $242((v4$1) => $27(state3$1, $31(a$2))))))
                    )));
                  })
                )))));
              });
            };
          })())(token.comma);
          return (state1, more, lift12, $$throw2, done) => more((v2) => more((v2$1) => more((v1) => more((v2$2) => more((v3) => go$lazy()(
            state1,
            more,
            lift12,
            $$throw2,
            (state3, a) => more((v4) => {
              const $232 = $212(a);
              return more((v2$3) => more((v3$1) => bar(
                state3,
                more,
                lift12,
                $$throw2,
                (state3$1, a$1) => more((v4$1) => more((v3$2) => more((v1$1) => $222(
                  state3$1,
                  more,
                  lift12,
                  $$throw2,
                  (state2, a$2) => more((v2$4) => {
                    const $242 = $List("Cons", a$2._1, a$2._2);
                    return more((v4$2) => done(state2, $232($242)));
                  })
                ))))
              )));
            })
          ))))));
        })());
        const $22 = token.parens(go$lazy());
        const $23 = token.brackets((state1, more, lift12, $$throw2, done) => more((v2) => more((v2$1) => more((v1) => more((v2$2) => more((v3) => go$lazy()(
          state1,
          more,
          lift12,
          $$throw2,
          (state3, a) => more((v4) => {
            const $232 = ListEnum(a);
            return more((v2$3) => more((v3$1) => ellipsis(
              state3,
              more,
              lift12,
              $$throw2,
              (state3$1, a$1) => more((v4$1) => more((v3$2) => go$lazy()(state3$1, more, lift12, $$throw2, (state3$2, a$2) => more((v4$2) => done(state3$2, $232(a$2))))))
            )));
          })
        )))))));
        const $24 = token.parens(token.operator);
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
                return $25((v3$1) => $25((v1) => ident(
                  $ParseState($33, $34, false),
                  $25,
                  $26,
                  (v2$1, $35) => $25((v5$1) => {
                    const $36 = v2._1;
                    const $37 = v2._2;
                    return $25((v3$2) => $22(
                      $ParseState($36, $37, false),
                      $25,
                      $26,
                      (v2$2, $38) => $25((v5$2) => {
                        const $39 = v2._1;
                        const $40 = v2._2;
                        return $25((v3$3) => $23(
                          $ParseState($39, $40, false),
                          $25,
                          $26,
                          (v4$1, $41) => {
                            const $42 = v4$1._3;
                            return $25((v5$3) => {
                              if ($42) {
                                return $27(v4$1, $41);
                              }
                              const $43 = v2._3;
                              return $25((v1$1) => $24(
                                v2,
                                $25,
                                $26,
                                (v2$3, $44) => $27($ParseState(v2$3._1, v2$3._2, $43), $44),
                                (state2, a) => $25((v2$3) => $28(state2, $Expr2("Op", a)))
                              ));
                            });
                          },
                          $28
                        ));
                      }),
                      $28
                    ));
                  }),
                  (state2, a) => $25((v2$1) => $28(state2, $Expr2("Var", a)))
                )));
              });
            };
            return $25((v3$1) => $14(
              $ParseState($29, $30, false),
              $25,
              $26,
              (v4, $32) => {
                const $33 = v4._3;
                return $25((v5) => {
                  if ($33) {
                    return $31(v4, $32);
                  }
                  return $25((v3$2) => $15(
                    $ParseState($29, $30, false),
                    $25,
                    $26,
                    (v2$1, $34) => $25((v5$1) => $25((v3$3) => {
                      const $35 = (v4$1, $352) => {
                        const $36 = v4$1._3;
                        return $25((v5$2) => {
                          if ($36) {
                            return $31(v4$1, $352);
                          }
                          return $25((v3$4) => $25((v1) => $25((v1$1) => ctr(
                            $ParseState($29, $30, false),
                            $25,
                            $26,
                            (v2$2, $37) => $25((v5$3) => $25((v3$5) => $18(
                              $ParseState($29, $30, false),
                              $25,
                              $26,
                              (v4$2, $38) => {
                                const $39 = v4$2._3;
                                return $25((v5$4) => {
                                  if ($39) {
                                    return $31(v4$2, $38);
                                  }
                                  return $25((v3$6) => {
                                    const $40 = (v4$3, $402) => {
                                      const $41 = v4$3._3;
                                      return $25((v5$5) => {
                                        if ($41) {
                                          return $31(v4$3, $402);
                                        }
                                        return $25((v3$7) => {
                                          const $42 = (v4$4, $422) => {
                                            const $43 = v4$4._3;
                                            return $25((v5$6) => {
                                              if ($43) {
                                                return $31(v4$4, $422);
                                              }
                                              return $25((v3$8) => $25((v1$2) => token.stringLiteral(
                                                $ParseState($29, $30, false),
                                                $25,
                                                $26,
                                                (v4$5, $44) => {
                                                  const $45 = v4$5._3;
                                                  return $25((v5$7) => {
                                                    if ($45) {
                                                      return $31(v4$5, $44);
                                                    }
                                                    return $25((v3$9) => $202(
                                                      $ParseState($29, $30, false),
                                                      $25,
                                                      $26,
                                                      (v2$3, $46) => $25((v5$8) => $21($ParseState($29, $30, false), $25, $26, $31, $28)),
                                                      $28
                                                    ));
                                                  });
                                                },
                                                (state2, a) => $25((v2$3) => $28(state2, $19(a)))
                                              )));
                                            });
                                          };
                                          return $25((v1$2) => {
                                            const $43 = (state2, a) => $25((v2$3) => {
                                              const $432 = Int()(doc);
                                              return $25((v1$3) => token.natural(
                                                state2,
                                                $25,
                                                $26,
                                                (v2$4, $44) => $42($ParseState(v2$4._1, v2$4._2, false), $44),
                                                (state2$1, a$1) => $25((v2$4) => $28(state2$1, $432(a(a$1))))
                                              ));
                                            });
                                            return $25((v3$8) => $25((v1$3) => $102(
                                              $ParseState($29, $30, false),
                                              $25,
                                              $26,
                                              (v4$4, $44) => {
                                                const $45 = v4$4._3;
                                                return $25((v5$6) => {
                                                  if ($45) {
                                                    return $42($ParseState(v4$4._1, v4$4._2, false), $44);
                                                  }
                                                  return $25((v3$9) => $25((v1$4) => $11(
                                                    $ParseState($29, $30, false),
                                                    $25,
                                                    $26,
                                                    (v4$5, $46) => {
                                                      const $47 = v4$5._3;
                                                      return $25((v5$7) => {
                                                        if ($47) {
                                                          return $42($ParseState(v4$5._1, v4$5._2, false), $46);
                                                        }
                                                        return $43($ParseState($29, $30, false), identity24);
                                                      });
                                                    },
                                                    (state2, a) => $25((v2$3) => $43(state2, identity24))
                                                  )));
                                                });
                                              },
                                              (state2, a) => $25((v2$3) => $43(state2, (a$1) => -a$1))
                                            )));
                                          });
                                        });
                                      });
                                    };
                                    return $25((v1$2) => {
                                      const $41 = (state2, a) => $25((v2$3) => {
                                        const $412 = Float()(doc);
                                        return $25((v1$3) => token.float(
                                          state2,
                                          $25,
                                          $26,
                                          (v2$4, $42) => $40($ParseState(v2$4._1, v2$4._2, false), $42),
                                          (state2$1, a$1) => $25((v2$4) => $28(state2$1, $412(a(a$1))))
                                        ));
                                      });
                                      return $25((v3$7) => $25((v1$3) => $12(
                                        $ParseState($29, $30, false),
                                        $25,
                                        $26,
                                        (v4$3, $42) => {
                                          const $43 = v4$3._3;
                                          return $25((v5$5) => {
                                            if ($43) {
                                              return $40($ParseState(v4$3._1, v4$3._2, false), $42);
                                            }
                                            return $25((v3$8) => $25((v1$4) => $13(
                                              $ParseState($29, $30, false),
                                              $25,
                                              $26,
                                              (v4$4, $44) => {
                                                const $45 = v4$4._3;
                                                return $25((v5$6) => {
                                                  if ($45) {
                                                    return $40($ParseState(v4$4._1, v4$4._2, false), $44);
                                                  }
                                                  return $41($ParseState($29, $30, false), identity24);
                                                });
                                              },
                                              (state2, a) => $25((v2$3) => $41(state2, identity24))
                                            )));
                                          });
                                        },
                                        (state2, a) => $25((v2$3) => $41(state2, (a$1) => -a$1))
                                      )));
                                    });
                                  });
                                });
                              },
                              $28
                            ))),
                            (state2, a) => $25((v2$2) => {
                              const $37 = $17(a);
                              return $25((v2$3) => $28(state2, $37(Nil)));
                            })
                          ))));
                        });
                      };
                      return $25((v2$2) => $25((v1) => lBracket(
                        $ParseState($29, $30, false),
                        $25,
                        $26,
                        $35,
                        (state2, a) => $25((v2$3) => $25((v3$4) => $25((v2$4) => $25((v1$1) => go$lazy()(
                          state2,
                          $25,
                          $26,
                          $35,
                          (state2$1, a$1) => $25((v2$5) => {
                            const $36 = $16(a$1);
                            return $25((v3$5) => go$1(
                              state2$1,
                              $25,
                              $26,
                              $35,
                              (state3, a$2) => $25((v4$1) => {
                                const $37 = $36(a$2);
                                return $25((v4$2) => $28(state3, $37));
                              })
                            ));
                          })
                        )))))
                      )));
                    })),
                    $28
                  ));
                });
              },
              $28
            ));
          });
        };
      })();
      return (state1, more, lift12, $$throw2, done) => more((v1) => $20(
        state1,
        more,
        lift12,
        $$throw2,
        (state2, a) => more((v2) => {
          const $21 = DProject2(doc)(a);
          const $22 = token.reservedOp(".");
          const $23 = token.brackets(expr_$lazy());
          const $24 = Project2(doc)(a);
          const $25 = token.reservedOp(".");
          const $26 = state2._1;
          const $27 = state2._2;
          return more((v3) => {
            const $28 = (v4, $282) => {
              const $29 = v4._3;
              return more((v5) => {
                if ($29) {
                  return $$throw2(v4, $282);
                }
                const $30 = state2._1;
                const $31 = state2._2;
                return more((v3$1) => {
                  const $32 = (v4$1, $322) => {
                    const $33 = v4$1._3;
                    return more((v5$1) => {
                      if ($33) {
                        return $$throw2(v4$1, $322);
                      }
                      return done(state2, a);
                    });
                  };
                  return more((v1$1) => more((v2$1) => more((v1$2) => $25(
                    $ParseState($30, $31, false),
                    more,
                    lift12,
                    $32,
                    (state2$1, a$1) => more((v2$2) => more((v3$2) => ident(state2$1, more, lift12, $32, (state3, a$2) => more((v4$1) => more((v2$3) => done(state3, $24(a$2)))))))
                  ))));
                });
              });
            };
            return more((v1$1) => more((v2$1) => more((v1$2) => $22(
              $ParseState($26, $27, false),
              more,
              lift12,
              $28,
              (state2$1, a$1) => more((v2$2) => more((v3$1) => $23(state2$1, more, lift12, $28, (state3, a$2) => more((v4) => more((v2$3) => done(state3, $21(a$2)))))))
            ))));
          });
        })
      ));
    };
    const rest = (v$1) => (v1) => {
      if (v1.tag === "Constr") {
        const $103 = v1._3;
        const $11 = v1._2;
        const $12 = v1._4;
        const $13 = v1._1;
        const $14 = docComment(go$lazy());
        return (v2, $15, $16, $17, $18) => {
          const $19 = v2._1;
          const $20 = v2._2;
          return $15((v3) => {
            const $21 = (v4, $212) => {
              const $22 = v4._3;
              return $15((v5) => {
                if ($22) {
                  return $17(v4, $212);
                }
                return $18(v2, v1);
              });
            };
            return $15((v1$1) => $14(
              $ParseState($19, $20, false),
              $15,
              $16,
              $21,
              (state2, a) => $15((v2$1) => {
                const $22 = simpleExprOrProjection(a);
                return $15((v1$2) => $22(
                  state2,
                  $15,
                  $16,
                  $21,
                  (state2$1, a$1) => $15((v2$2) => rest(a)($Expr2(
                    "Constr",
                    $13,
                    $11,
                    $103,
                    foldableList.foldr(Cons)($List("Cons", a$1, Nil))($12)
                  ))(state2$1, $15, $16, $21, $18))
                ));
              })
            ));
          });
        };
      }
      const $102 = docComment(go$lazy());
      return (v2, $11, $12, $13, $14) => {
        const $15 = v2._1;
        const $16 = v2._2;
        return $11((v3) => {
          const $17 = (v4, $172) => {
            const $18 = v4._3;
            return $11((v5) => {
              if ($18) {
                return $13(v4, $172);
              }
              return $14(v2, v1);
            });
          };
          return $11((v1$1) => $11((v1$2) => $102(
            $ParseState($15, $16, false),
            $11,
            $12,
            $17,
            (state2, a) => $11((v2$1) => simpleExprOrProjection(a)(
              state2,
              $11,
              $12,
              $17,
              (state2$1, a$1) => $11((v2$2) => rest(v$1)($Expr2("App", v$1, v1, a$1))(state2$1, $11, $12, $17, $14))
            ))
          )));
        });
      };
    };
    const $10 = docComment(go$lazy());
    return (v2, $11, $12, $13, $14) => {
      const $15 = v2._1;
      const $16 = v2._2;
      return $11((v3) => {
        const $17 = (v4, $172) => {
          const $18 = v4._3;
          return $11((v5) => {
            if ($18) {
              return $13(v4, $172);
            }
            const $19 = v2._1;
            const $20 = v2._2;
            return $11((v3$1) => {
              const $21 = (v4$1, $212) => {
                const $22 = v4$1._3;
                return $11((v5$1) => {
                  if ($22) {
                    return $13(v4$1, $212);
                  }
                  const $23 = v2._1;
                  const $24 = v2._2;
                  return $11((v3$2) => {
                    const $25 = (v4$2, $252) => {
                      const $26 = v4$2._3;
                      return $11((v5$2) => {
                        if ($26) {
                          return $13(v4$2, $252);
                        }
                        const $27 = v2._1;
                        const $28 = v2._2;
                        return $11((v3$3) => {
                          const $29 = (v4$3, $292) => {
                            const $30 = v4$3._3;
                            return $11((v5$3) => {
                              if ($30) {
                                return $13(v4$3, $292);
                              }
                              return $11((v1) => $10(
                                v2,
                                $11,
                                $12,
                                $13,
                                (state2, a) => $11((v2$1) => {
                                  const $31 = simpleExprOrProjection(a);
                                  const $32 = rest(a);
                                  return $11((v1$1) => $31(state2, $11, $12, $13, (state2$1, a$1) => $11((v2$2) => $32(a$1)(state2$1, $11, $12, $13, $14))));
                                })
                              ));
                            });
                          };
                          return $11((v1) => $11((v1$1) => $9(
                            $ParseState($27, $28, false),
                            $11,
                            $12,
                            $29,
                            (state2, a) => $11((v2$1) => {
                              const $30 = bindList.bind($List("Cons", a._1, a._2))(identity7);
                              return $11((v2$2) => {
                                const $31 = foldableList.foldr((def) => fanin3(Let2)(LetRec2)(def));
                                const $32 = keyword("in");
                                return $11((v1$2) => $11((v1$3) => $11((v2$3) => $11((v1$4) => $32(
                                  state2,
                                  $11,
                                  $12,
                                  $29,
                                  (state2$1, a$1) => $11((v2$4) => $11((v3$4) => go$lazy()(
                                    state2$1,
                                    $11,
                                    $12,
                                    $29,
                                    (state3, a$2) => $11((v4$3) => $11((v2$5) => {
                                      const $33 = $31(a$2);
                                      return $11((v2$6) => $14(state3, $33($30)));
                                    }))
                                  )))
                                )))));
                              });
                            })
                          )));
                        });
                      });
                    };
                    return $11((v1) => $11((v2$1) => $11((v1$1) => $7(
                      $ParseState($23, $24, false),
                      $11,
                      $12,
                      $25,
                      (state2, a) => $11((v2$2) => $11((v3$3) => $8(state2, $11, $12, $25, (state3, a$1) => $11((v4$2) => $11((v2$3) => $14(state3, $Expr2("Lambda", a$1)))))))
                    ))));
                  });
                });
              };
              return $11((v2$1) => $11((v2$2) => $11((v1) => $11((v2$3) => $11((v2$4) => $11((v1$1) => $11((v2$5) => $11((v3$2) => $11((v2$6) => $11((v1$2) => $4(
                $ParseState($19, $20, false),
                $11,
                $12,
                $21,
                (state2, a) => $11((v2$7) => $11((v3$3) => go$lazy()(
                  state2,
                  $11,
                  $12,
                  $21,
                  (state3, a$1) => $11((v4$1) => $11((v4$2) => {
                    const $22 = IfElse(a$1);
                    return $11((v2$8) => $11((v3$4) => $5(
                      state3,
                      $11,
                      $12,
                      $21,
                      (state3$1, a$2) => $11((v4$3) => $11((v3$5) => go$lazy()(
                        state3$1,
                        $11,
                        $12,
                        $21,
                        (state3$2, a$3) => $11((v4$4) => {
                          const $23 = $22(a$3);
                          return $11((v2$9) => $11((v3$6) => $6(
                            state3$2,
                            $11,
                            $12,
                            $21,
                            (state3$3, a$4) => $11((v4$5) => $11((v3$7) => go$lazy()(state3$3, $11, $12, $21, (state3$4, a$5) => $11((v4$6) => $14(state3$4, $23(a$5))))))
                          )));
                        })
                      )))
                    )));
                  }))
                )))
              )))))))))));
            });
          });
        };
        return $11((v2$1) => $11((v1) => $11((v2$2) => $11((v1$1) => $11((v2$3) => $11((v1$2) => $1(
          $ParseState($15, $16, false),
          $11,
          $12,
          $17,
          (state2, a) => $11((v2$4) => $11((v3$1) => go$lazy()(
            state2,
            $11,
            $12,
            $17,
            (state3, a$1) => $11((v4) => $11((v2$5) => $11((v3$2) => $2(
              state3,
              $11,
              $12,
              $17,
              (state3$1, a$2) => $11((v4$1) => $11((v2$6) => {
                const $18 = MatchAs(a$1);
                return $11((v3$3) => $3(state3$1, $11, $12, $17, (state3$2, a$3) => $11((v4$2) => $14(state3$2, $18(a$3)))));
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
    return (state1, more, lift12, $$throw2, done) => more((v2) => more((v1) => $02(
      state1,
      more,
      lift12,
      $$throw2,
      (state2, a) => more((v2$1) => more((v3) => token.semi(state2, more, lift12, $$throw2, (state3, a$1) => more((v4) => done(state3, a)))))
    )));
  })());
  return (state1, more, lift12, $$throw2, done) => more((v1) => $0(
    state1,
    more,
    lift12,
    $$throw2,
    (state2, a) => more((v2) => done(state2, $Module(bindList.bind(a)(identity7))))
  ));
})();

// output-es/Primitive.Defs/index.js
var foldM4 = (dictMonad) => (f) => (b0) => foldableDict.foldl((b) => (a) => dictMonad.Bind1().bind(b)((a$1) => f(a$1)(a)))(dictMonad.Applicative0().pure(b0));
var disjointUnion3 = /* @__PURE__ */ disjointUnion(mapDictString);
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
    op: (dictMonadWithGraphAlloc) => {
      const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
      return (dictMonadError) => {
        const $$throw2 = $$throw(dictMonadError.MonadThrow0());
        return (dictLoadFile) => (v) => {
          if (v.tag === "Cons" && v._1._3.tag === "Matrix" && v._2.tag === "Cons" && v._2._1._3.tag === "Constr" && v._2._1._3._2.tag === "Cons" && v._2._1._3._2._1._3.tag === "Int" && v._2._1._3._2._2.tag === "Cons" && v._2._1._3._2._2._1._3.tag === "Int" && v._2._1._3._2._2._2.tag === "Nil" && v._2._2.tag === "Cons" && v._2._2._2.tag === "Nil" && v._2._1._3._1 === "Pair") {
            const $0 = v._2._2._1;
            return $$new((a) => Val(a)(None))($$$Map("Two", Leaf2, v._1._1, void 0, Leaf2))($BaseVal(
              "Matrix",
              matrixPut(v._2._1._3._2._1._3._1)(v._2._1._3._2._2._1._3._1)((v$1) => $0)(v._1._3._1)
            ));
          }
          return $$throw2("Matrix, pair of integers and value expected");
        };
      };
    }
  })
);
var matrixLookup = /* @__PURE__ */ $Tuple(
  "!",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    op: (dictMonadWithGraphAlloc) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (dictLoadFile) => (v) => {
        if (v.tag === "Cons" && v._1._3.tag === "Matrix" && v._2.tag === "Cons" && v._2._1._3.tag === "Constr" && v._2._1._3._2.tag === "Cons" && v._2._1._3._2._1._3.tag === "Int" && v._2._1._3._2._2.tag === "Cons" && v._2._1._3._2._2._1._3.tag === "Int" && v._2._1._3._2._2._2.tag === "Nil" && v._2._2.tag === "Nil" && v._2._1._3._1 === "Pair") {
          return MonadThrow0.Monad0().Applicative0().pure(matrixGet(v._2._1._3._2._1._3._1)(v._2._1._3._2._2._1._3._1)(v._1._3._1));
        }
        return MonadThrow0.throwError(error("Matrix and pair of integers expected"));
      };
    }
  })
);
var log3 = (v2) => {
  if (v2.tag === "Left") {
    return log2(toNumber(v2._1));
  }
  if (v2.tag === "Right") {
    return log2(v2._1);
  }
  fail();
};
var lessThanEquals = /* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((a1) => (a2) => a1 <= a2)(/* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((a1) => (a2) => a1 <= a2)((a1) => (a2) => a1 <= a2));
var lessThan = /* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((a1) => (a2) => a1 < a2)(/* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((a1) => (a2) => a1 < a2)((a1) => (a2) => a1 < a2));
var greaterThanEquals = /* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((a1) => (a2) => a1 >= a2)(/* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((a1) => (a2) => a1 >= a2)((a1) => (a2) => a1 >= a2));
var greaterThan = /* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asIntNumberOrString)(asIntNumberOrString)((a1) => (a2) => a1 > a2)(/* @__PURE__ */ union6(asBooleanBoolean)(asBooleanBoolean)(asNumberString)(asNumberString)((a1) => (a2) => a1 > a2)((a1) => (a2) => a1 > a2));
var extern = (dictBoundedJoinSemilattice) => {
  const bot = dictBoundedJoinSemilattice.bot;
  return (v) => $Tuple(v._1, $Val(bot, None, $BaseVal("Fun", $Fun("Foreign", $Tuple(v._1, v._2), Nil))));
};
var extern1 = /* @__PURE__ */ extern(boundedJoinSemilatticeUni);
var error_ = /* @__PURE__ */ $Tuple(
  "error",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 1,
    op: (dictMonadWithGraphAlloc) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (dictLoadFile) => (v) => {
        if (v.tag === "Cons" && v._1._3.tag === "Str" && v._2.tag === "Nil") {
          return MonadThrow0.Monad0().Applicative0().pure(throwException(error(v._1._3._1))());
        }
        return MonadThrow0.throwError(error("String expected"));
      };
    }
  })
);
var divide = /* @__PURE__ */ union6(asNumberIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber)((x) => (y) => toNumber(x) / toNumber(y))(numDiv);
var dims = /* @__PURE__ */ $Tuple(
  "dims",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 1,
    op: (dictMonadWithGraphAlloc) => {
      const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
      return (dictMonadError) => {
        const MonadThrow0 = dictMonadError.MonadThrow0();
        const $0 = MonadThrow0.Monad0().Bind1();
        return (dictLoadFile) => (v) => {
          if (v.tag === "Cons" && v._1._3.tag === "Matrix" && v._2.tag === "Nil") {
            const $1 = v._1._3._1._2._2._1;
            const $2 = v._1._1;
            const $3 = v._1._3._1._2._2._2;
            return $0.bind($$new((a) => Val(a)(None))($$$Map("Two", Leaf2, v._1._3._1._2._1._2, void 0, Leaf2))($BaseVal(
              "Int",
              v._1._3._1._2._1._1
            )))((v1) => $0.bind($$new((a) => Val(a)(None))($$$Map("Two", Leaf2, $3, void 0, Leaf2))($BaseVal(
              "Int",
              $1
            )))((v2) => $$new((a) => Val(a)(None))($$$Map("Two", Leaf2, $2, void 0, Leaf2))($BaseVal(
              "Constr",
              "Pair",
              $List("Cons", v1, $List("Cons", v2, Nil))
            ))));
          }
          return MonadThrow0.throwError(error("Matrix expected"));
        };
      };
    }
  })
);
var dict_map = /* @__PURE__ */ $Tuple(
  "dict_map",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    op: (dictMonadWithGraphAlloc) => {
      const apply5 = apply2(dictMonadWithGraphAlloc);
      const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
      return (dictMonadError) => {
        const MonadThrow0 = dictMonadError.MonadThrow0();
        const Monad0 = MonadThrow0.Monad0();
        const Bind1 = Monad0.Bind1();
        const traverse1 = traversableDict.traverse(Monad0.Applicative0());
        return (dictLoadFile) => {
          const apply12 = apply5(dictLoadFile);
          return (v) => {
            if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._1._3.tag === "Dictionary" && v._2._2.tag === "Nil") {
              const $0 = v._1;
              const $1 = v._2._1._1;
              return Bind1.bind(traverse1((v2) => {
                const $2 = v2._1;
                return Bind1.Apply0().Functor0().map((v3) => $Tuple($2, v3))(apply12($0)(v2._2));
              })(v._2._1._3._1))((d$p) => $$new((a) => Val(a)(None))($$$Map("Two", Leaf2, $1, void 0, Leaf2))($BaseVal(
                "Dictionary",
                d$p
              )));
            }
            return MonadThrow0.throwError(error("Function and dictionary expected"));
          };
        };
      };
    }
  })
);
var dict_intersectionWith = /* @__PURE__ */ $Tuple(
  "dict_intersectionWith",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 3,
    op: (dictMonadWithGraphAlloc) => {
      const apply5 = apply2(dictMonadWithGraphAlloc);
      const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
      return (dictMonadError) => {
        const MonadThrow0 = dictMonadError.MonadThrow0();
        const Monad0 = MonadThrow0.Monad0();
        const Bind1 = Monad0.Bind1();
        const Applicative0 = Monad0.Applicative0();
        const $0 = Bind1.Apply0().Functor0();
        return (dictLoadFile) => {
          const apply12 = apply5(dictLoadFile);
          return (v) => {
            if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._1._3.tag === "Dictionary" && v._2._2.tag === "Cons" && v._2._2._1._3.tag === "Dictionary" && v._2._2._2.tag === "Nil") {
              const $1 = v._1;
              const $2 = v._2._1._1;
              const $3 = v._2._2._1._1;
              return Bind1.bind($0.map(Dictionary3)($0.map(DictRep)(traversableDict.traverse(Applicative0)(identity14)(intersectionWith_Object((v2) => (v3) => {
                const $4 = v3._2;
                const $5 = v2._1;
                const $6 = v3._1;
                return Bind1.bind(Bind1.bind(apply12($1)(v2._2))((a) => apply12(a)($4)))((v4) => Bind1.bind($$new((a) => Val(a)(None))(insert(ordVertex)($6)()($$$Map(
                  "Two",
                  Leaf2,
                  $5,
                  void 0,
                  Leaf2
                )))(v4._3))((v5) => Applicative0.pure($Tuple(v5._1, v4))));
              })(v._2._1._3._1)(v._2._2._1._3._1)))))((v$p) => $$new((a) => Val(a)(None))(insert(ordVertex)($3)()($$$Map(
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
      };
    }
  })
);
var dict_get = /* @__PURE__ */ $Tuple(
  "dict_get",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    op: (dictMonadWithGraphAlloc) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (dictLoadFile) => (v) => {
        if (v.tag === "Cons" && v._1._3.tag === "Str" && v._2.tag === "Cons" && v._2._1._3.tag === "Dictionary" && v._2._2.tag === "Nil") {
          return orElse(MonadThrow0)('Key "' + v._1._3._1 + '" not found')((() => {
            const $0 = _lookup(Nothing, Just, v._1._3._1, v._2._1._3._1);
            if ($0.tag === "Just") {
              return $Maybe("Just", $0._1._2);
            }
            return Nothing;
          })());
        }
        return MonadThrow0.throwError(error("String and dictionary expected"));
      };
    }
  })
);
var dict_foldl = /* @__PURE__ */ $Tuple(
  "dict_foldl",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 3,
    op: (dictMonadWithGraphAlloc) => {
      const apply5 = apply2(dictMonadWithGraphAlloc);
      return (dictMonadError) => {
        const MonadThrow0 = dictMonadError.MonadThrow0();
        const Monad0 = MonadThrow0.Monad0();
        return (dictLoadFile) => {
          const apply12 = apply5(dictLoadFile);
          return (v) => {
            if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._2.tag === "Cons" && v._2._2._1._3.tag === "Dictionary" && v._2._2._2.tag === "Nil") {
              const $0 = v._1;
              return foldM4(Monad0)((u1) => (v2) => {
                const $1 = v2._2;
                return Monad0.Bind1().bind(apply12($0)(u1))((a) => apply12(a)($1));
              })(v._2._1)(v._2._2._1._3._1);
            }
            return MonadThrow0.throwError(error("Function, value and dictionary expected"));
          };
        };
      };
    }
  })
);
var dict_disjointUnion = /* @__PURE__ */ $Tuple(
  "dict_disjointUnion",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    op: (dictMonadWithGraphAlloc) => {
      const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
      return (dictMonadError) => {
        const $$throw2 = $$throw(dictMonadError.MonadThrow0());
        return (dictLoadFile) => (v) => {
          if (v.tag === "Cons" && v._1._3.tag === "Dictionary" && v._2.tag === "Cons" && v._2._1._3.tag === "Dictionary" && v._2._2.tag === "Nil") {
            return $$new((a) => Val(a)(None))(insert(ordVertex)(v._2._1._1)()($$$Map(
              "Two",
              Leaf2,
              v._1._1,
              void 0,
              Leaf2
            )))($BaseVal("Dictionary", disjointUnion3(v._1._3._1)(v._2._1._3._1)));
          }
          return $$throw2("Dictionaries expected");
        };
      };
    }
  })
);
var dict_difference = /* @__PURE__ */ $Tuple(
  "dict_difference",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 2,
    op: (dictMonadWithGraphAlloc) => {
      const $$new = dictMonadWithGraphAlloc.new(typeNameVal);
      return (dictMonadError) => {
        const $$throw2 = $$throw(dictMonadError.MonadThrow0());
        return (dictLoadFile) => (v) => {
          if (v.tag === "Cons" && v._1._3.tag === "Dictionary" && v._2.tag === "Cons" && v._2._1._3.tag === "Dictionary" && v._2._2.tag === "Nil") {
            return $$new((a) => Val(a)(None))(insert(ordVertex)(v._2._1._1)()($$$Map(
              "Two",
              Leaf2,
              v._1._1,
              void 0,
              Leaf2
            )))($BaseVal("Dictionary", mapFObjectString.difference(v._1._3._1)(v._2._1._3._1)));
          }
          return $$throw2("Dictionaries expected.");
        };
      };
    }
  })
);
var debugLog = /* @__PURE__ */ $Tuple(
  "debugLog",
  /* @__PURE__ */ $ForeignOp$p({
    arity: 1,
    op: (dictMonadWithGraphAlloc) => (dictMonadError) => {
      const MonadThrow0 = dictMonadError.MonadThrow0();
      return (dictLoadFile) => (v) => {
        if (v.tag === "Cons" && v._2.tag === "Nil") {
          const $0 = v._1;
          return MonadThrow0.Monad0().Applicative0().pure(_trace($0, (v$1) => $0));
        }
        return MonadThrow0.throwError(error("Single value expected"));
      };
    }
  })
);
var primitives = /* @__PURE__ */ fromFoldable2(foldableArray)([
  /* @__PURE__ */ $Tuple(
    ":",
    /* @__PURE__ */ $Val(void 0, None, /* @__PURE__ */ $BaseVal("Fun", /* @__PURE__ */ $Fun("PartialConstr", ":", Nil)))
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
var union7 = /* @__PURE__ */ (() => setSet(ordDVertex$p).union)();
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
        go$a0 = unionWith(ordDVertex$p)($$const)(b)(v._1);
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
  vertices: (v) => union7(unions13(listMap(verticesValVertex.vertices)(mapObjectString.values(v.primitives))))(union7(unions4(listMap(verticesModuleVertex.vertices)(v.mods)))(unions4(listMap((x) => verticesExprVertex.vertices(x._2))(v.datasets))))
};
var functorProgCxt = {
  map: (f) => (m) => ({
    fluidSrcPaths: m.fluidSrcPaths,
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
    return (f) => (m) => Apply0.apply(Apply0.apply(Apply0.Functor0().map((v1) => (v2) => (v3) => ({ fluidSrcPaths: m.fluidSrcPaths, primitives: v3, mods: v2, datasets: v1 }))(traverse5(traversableTuple.traverse(dictApplicative)(traverse7(f)))(m.datasets)))(traverse5(traverse8(f))(m.mods)))(traverse9(f)(m.primitives));
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
var parseProgram = (dictLoadFile) => (folders) => (file) => (dictMonadAff) => (dictMonadError) => dictMonadAff.MonadEffect0().Monad0().Bind1().bind(dictLoadFile.loadFile(dictMonadError)(dictMonadAff)(folders)(file))((() => {
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
    return (dictLoadFile) => {
      const loadFile = dictLoadFile.loadFile(dictMonadError)(dictMonadAff);
      return (folders) => (file) => (v) => {
        const $0 = v.mods;
        return Bind1.bind(Applicative0.pure())(() => Bind1.bind(loadFile(folders)(file))((src) => Bind1.bind(Bind1.bind(parse1(src)(module_))(desugarModuleFwd))((mod) => Applicative0.pure({
          fluidSrcPaths: v.fluidSrcPaths,
          primitives: v.primitives,
          mods: $List("Cons", mod, $0),
          datasets: v.datasets
        }))));
      };
    };
  };
};
var initialConfig = (dictMonadAff) => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
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
  const $2 = monadAffState(dictMonadAff).MonadEffect0().Monad0();
  const $3 = dictMonadAff.MonadEffect0().Monad0();
  return (dictMonadError) => {
    const eval_progCxt2 = eval_progCxt(monadWithGraphAllocWithGr(dictMonadError));
    return (dictLoadFile) => {
      const eval_progCxt1 = eval_progCxt2((() => {
        const loadFile1 = dictLoadFile.loadFile(dictMonadError)(dictMonadAff);
        return {
          loadFile: (dictMonadError1) => (dictMonadAff1) => (folders) => {
            const $4 = loadFile1(folders);
            return (x) => {
              const $5 = $4(x);
              return (s) => $2.Bind1().bind((s$1) => $3.Bind1().bind($5)((x$1) => $3.Applicative0().pure($Tuple(x$1, s$1))))((x$1) => $2.Applicative0().pure($Tuple(
                x$1,
                s
              )));
            };
          }
        };
      })());
      return (dictFV) => (e) => (progCxt) => Bind1.bind(Applicative0.pure())(() => Bind1.bind(runAllocT(Monad0)($1.bind(alloc(progCxt))((progCxt$p) => $1.bind(runWithGraphT_spy2(eval_progCxt1(progCxt$p))(verticesProgCxtVertex.vertices(progCxt$p)))((v) => applicativeStateT(Monad0).pure($Tuple(
        progCxt$p,
        (() => {
          const $4 = dictFV.fv(e);
          return filterWithKey2((x) => {
            const $5 = setSet(ordString).member(x)($4);
            return (v$1) => $5;
          })(v._2);
        })()
      )))))(0))((v) => Applicative0.pure({ n: v._1, progCxt: v._2._2._1, "\u03B3": v._2._2._2 })));
    };
  };
};
var prepConfig = (dictMonadAff) => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const $0 = Monad0.Bind1();
  const initialConfig1 = initialConfig(dictMonadAff);
  return (dictMonadError) => {
    const desug1 = exprFwd(boundedLattice2)(dictMonadError)(joinSemilatticeUnit);
    const initialConfig2 = initialConfig1(dictMonadError);
    return (dictLoadFile) => {
      const initialConfig3 = initialConfig2(dictLoadFile)(fVExpr);
      return (v) => (file) => (progCxt) => $0.bind(parseProgram(dictLoadFile)(v.fluidSrcPaths)(file)(dictMonadAff)(dictMonadError))((s) => $0.bind(desug1(s))((e) => $0.bind(initialConfig3(e)(progCxt))((gconfig) => Monad0.Applicative0().pure({
        s,
        e,
        gconfig
      }))));
    };
  };
};
var datasetAs = (dictMonadAff) => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const $0 = Monad0.Bind1();
  return (dictMonadError) => {
    const desug1 = exprFwd(boundedLattice2)(dictMonadError)(joinSemilatticeUnit);
    return (dictLoadFile) => (folders) => (v) => (v1) => {
      const $1 = v1.datasets;
      const $2 = v._1;
      return $0.bind($0.bind(parseProgram(dictLoadFile)(folders)(v._2)(dictMonadAff)(dictMonadError))(desug1))((e\u03B1) => Monad0.Applicative0().pure({
        fluidSrcPaths: v1.fluidSrcPaths,
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
    const datasetAs2 = datasetAs1(dictMonadError);
    return (dictLoadFile) => {
      const module_3 = module_22(dictLoadFile);
      const datasetAs3 = datasetAs2(dictLoadFile);
      return (v) => (mods) => (datasets) => $0.bind($0.bind(Monad0.Applicative0().pure({
        fluidSrcPaths: v.fluidSrcPaths,
        primitives,
        mods: Nil,
        datasets: Nil
      }))(concatM1(arrayMap(module_3(v.fluidSrcPaths))(["lib/prelude", ...mods]))))(concatM1(arrayMap((() => {
        const $1 = datasetAs3(v.fluidSrcPaths);
        return (x) => $1($Tuple(x._1, x._2));
      })())(datasets)));
    };
  };
};

// output-es/Node.Encoding/index.js
var $Encoding = (tag) => tag;
var UTF8 = /* @__PURE__ */ $Encoding("UTF8");

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

// output-es/Foreign/foreign.js
var isArray = Array.isArray || function(value) {
  return Object.prototype.toString.call(value) === "[object Array]";
};

// output-es/Node.FS.Stats/foreign.js
var isFileImpl = (s) => s.isFile();

// output-es/Module.Node/index.js
var $$try3 = /* @__PURE__ */ $$try(monadErrorAff);
var findM = (dictMonad) => {
  const $0 = dictMonad.Bind1().Apply0();
  return (dictFoldable) => (xs) => (f) => (base) => dictFoldable.foldr((x) => (acc) => $0.apply($0.Functor0().map(altMaybe.alt)(acc))(f(x)))(dictMonad.Applicative0().pure(base))(xs);
};
var loadFileNodeT = (dictMonadAff) => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  const $0 = Monad0.Bind1();
  const findM1 = findM(Monad0)(foldableArray);
  return (dictMonadError) => ({
    loadFile: (dictMonadError1) => (dictMonadAff1) => (folders) => (v) => $0.bind(findM1(arrayMap((() => {
      const $1 = v + ".fld";
      return (a) => a + "/" + $1;
    })())(folders))((v1) => $0.bind(dictMonadAff1.liftAff($$try3(toAff1(stat2)(v1))))((stats) => Monad0.Applicative0().pure((() => {
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
        return dictMonadAff1.liftAff(toAff2(readTextFile)(UTF8)(url._1));
      }
      fail();
    })
  });
};

// output-es/Options.Applicative.Internal.Utils/index.js
var whitespaceRegex = /* @__PURE__ */ (() => {
  const v = regex("\\s+")(noFlags);
  if (v.tag === "Left") {
    return _crashWith("whitespaceRegex: `\\s+` seems to be invlaid, err: " + v._1);
  }
  if (v.tag === "Right") {
    return v._1;
  }
  fail();
})();
var startsWith = (p) => (s) => {
  const $0 = indexOf2(p)(s);
  if ($0.tag === "Nothing") {
    return false;
  }
  return $0.tag === "Just" && $0._1 === 0;
};
var apApplyFlipped = (dictApply) => (a) => (b) => dictApply.apply(dictApply.Functor0().map(applyFlipped)(a))(b);

// output-es/Text.PrettyPrint.Leijen/index.js
var $Doc = (tag, _1, _2) => ({ tag, _1, _2 });
var $Docs = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
var $LazySimpleDoc = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
var $SimpleDoc = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
var max3 = (x) => (y) => {
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
var min2 = (x) => (y) => {
  const v = ordInt.compare(x)(y);
  if (v === "LT") {
    return x;
  }
  if (v === "EQ") {
    return x;
  }
  if (v === "GT") {
    return y;
  }
  fail();
};
var SFail = /* @__PURE__ */ $SimpleDoc("SFail");
var SEmpty = /* @__PURE__ */ $SimpleDoc("SEmpty");
var SFail$p = /* @__PURE__ */ $LazySimpleDoc("SFail'");
var SEmpty$p = /* @__PURE__ */ $LazySimpleDoc("SEmpty'");
var Fail = /* @__PURE__ */ $Doc("Fail");
var Empty = /* @__PURE__ */ $Doc("Empty");
var Line = /* @__PURE__ */ $Doc("Line");
var Nil3 = /* @__PURE__ */ $Docs("Nil");
var text2 = (v) => {
  if (v === "") {
    return Empty;
  }
  return $Doc("Text", toCodePointArray(v).length, v);
};
var forceSimpleDoc = (v) => {
  if (v.tag === "SFail'") {
    return SFail;
  }
  if (v.tag === "SEmpty'") {
    return SEmpty;
  }
  if (v.tag === "SChar'") {
    return $SimpleDoc("SChar", v._1, forceSimpleDoc(force(v._2)));
  }
  if (v.tag === "SText'") {
    return $SimpleDoc("SText", v._1, v._2, forceSimpleDoc(force(v._3)));
  }
  if (v.tag === "SLine'") {
    return $SimpleDoc("SLine", v._1, forceSimpleDoc(force(v._2)));
  }
  fail();
};
var renderFits = (fits) => (rfrac) => (w) => (headNode) => {
  const r = max3(0)(min2(w)(unsafeClamp(round(toNumber(w) * rfrac))));
  const nicest$p = (n) => (k) => (i) => (ds) => (x) => (y) => {
    const x$p = best(n)(k)($Docs("Cons", i, x, ds));
    if (fits(w)(min2(n)(k))(min2(w - k | 0)((r - k | 0) + n | 0))(x$p)) {
      return x$p;
    }
    return best(n)(k)($Docs("Cons", i, y, ds));
  };
  const best = (v) => (v1) => (v2) => {
    if (v2.tag === "Nil") {
      return SEmpty$p;
    }
    if (v2.tag === "Cons") {
      if (v2._2.tag === "Fail") {
        return SFail$p;
      }
      if (v2._2.tag === "Empty") {
        return best(v)(v1)(v2._3);
      }
      if (v2._2.tag === "Char") {
        const k$p = v1 + 1 | 0;
        return $LazySimpleDoc("SChar'", v2._2._1, defer((v3) => best(v)(k$p)(v2._3)));
      }
      if (v2._2.tag === "Text") {
        const k$p = v1 + v2._2._1 | 0;
        return $LazySimpleDoc("SText'", v2._2._1, v2._2._2, defer((v3) => best(v)(k$p)(v2._3)));
      }
      if (v2._2.tag === "Line") {
        return $LazySimpleDoc("SLine'", v2._1, defer((v3) => best(v2._1)(v2._1)(v2._3)));
      }
      if (v2._2.tag === "FlatAlt") {
        return best(v)(v1)($Docs("Cons", v2._1, v2._2._1, v2._3));
      }
      if (v2._2.tag === "Cat") {
        return best(v)(v1)($Docs("Cons", v2._1, v2._2._1, $Docs("Cons", v2._1, v2._2._2, v2._3)));
      }
      if (v2._2.tag === "Nest") {
        return best(v)(v1)($Docs("Cons", v2._1 + v2._2._1 | 0, v2._2._2, v2._3));
      }
      if (v2._2.tag === "Union") {
        return nicest$p(v)(v1)(v2._1)(v2._3)(v2._2._1)(v2._2._2);
      }
      if (v2._2.tag === "Column") {
        return best(v)(v1)($Docs("Cons", v2._1, v2._2._1(v1), v2._3));
      }
      if (v2._2.tag === "Columns") {
        return best(v)(v1)($Docs("Cons", v2._1, v2._2._1($Maybe("Just", w)), v2._3));
      }
      if (v2._2.tag === "Nesting") {
        return best(v)(v1)($Docs("Cons", v2._1, v2._2._1(v2._1), v2._3));
      }
    }
    fail();
  };
  return forceSimpleDoc(best(0)(0)($Docs("Cons", 0, headNode, Nil3)));
};
var foldr1 = (dictMonoid) => {
  const mempty = dictMonoid.mempty;
  return (f) => (x) => {
    const $0 = unsnoc(x);
    if ($0.tag === "Nothing") {
      return mempty;
    }
    if ($0.tag === "Just") {
      return foldrArray(f)($0._1.last)($0._1.init);
    }
    fail();
  };
};
var flatten = (v) => {
  if (v.tag === "FlatAlt") {
    return v._2;
  }
  if (v.tag === "Cat") {
    return $Doc("Cat", flatten(v._1), flatten(v._2));
  }
  if (v.tag === "Nest") {
    return $Doc("Nest", v._1, flatten(v._2));
  }
  if (v.tag === "Line") {
    return Fail;
  }
  if (v.tag === "Union") {
    return flatten(v._1);
  }
  if (v.tag === "Column") {
    return $Doc("Column", (x) => flatten(v._1(x)));
  }
  if (v.tag === "Columns") {
    return $Doc("Columns", (x) => flatten(v._1(x)));
  }
  if (v.tag === "Nesting") {
    return $Doc("Nesting", (x) => flatten(v._1(x)));
  }
  return v;
};
var softline = /* @__PURE__ */ $Doc(
  "Union",
  /* @__PURE__ */ flatten(/* @__PURE__ */ $Doc("FlatAlt", Line, /* @__PURE__ */ $Doc("Char", " "))),
  /* @__PURE__ */ $Doc("FlatAlt", Line, /* @__PURE__ */ $Doc("Char", " "))
);
var fits1 = (fits1$a0$copy) => (fits1$a1$copy) => (fits1$a2$copy) => (fits1$a3$copy) => {
  let fits1$a0 = fits1$a0$copy, fits1$a1 = fits1$a1$copy, fits1$a2 = fits1$a2$copy, fits1$a3 = fits1$a3$copy, fits1$c = true, fits1$r;
  while (fits1$c) {
    const v = fits1$a0, v1 = fits1$a1, v2 = fits1$a2, v3 = fits1$a3;
    if (v2 < 0) {
      fits1$c = false;
      fits1$r = false;
      continue;
    }
    if (v3.tag === "SFail'") {
      fits1$c = false;
      fits1$r = false;
      continue;
    }
    if (v3.tag === "SEmpty'") {
      fits1$c = false;
      fits1$r = true;
      continue;
    }
    if (v3.tag === "SChar'") {
      fits1$a0 = v;
      fits1$a1 = v1;
      fits1$a2 = v2 - 1 | 0;
      fits1$a3 = force(v3._2);
      continue;
    }
    if (v3.tag === "SText'") {
      fits1$a0 = v;
      fits1$a1 = v1;
      fits1$a2 = v2 - v3._1 | 0;
      fits1$a3 = force(v3._3);
      continue;
    }
    if (v3.tag === "SLine'") {
      fits1$c = false;
      fits1$r = true;
      continue;
    }
    fail();
  }
  return fits1$r;
};
var displayS = (v) => {
  if (v.tag === "SFail") {
    return _crashWith("@SFail@ can not appear uncaught in a rendered @SimpleDoc@");
  }
  if (v.tag === "SEmpty") {
    return "";
  }
  if (v.tag === "SChar") {
    return fromCharArray([v._1]) + displayS(v._2);
  }
  if (v.tag === "SText") {
    return v._2 + displayS(v._3);
  }
  if (v.tag === "SLine") {
    return (v._1 <= 0 ? "\n" : "\n" + fromCharArray(replicate(v._1)(" "))) + displayS(v._2);
  }
  fail();
};
var beside2 = (x) => (y) => $Doc("Cat", x, y);
var docSemigroup = { append: beside2 };
var docMonoid = { mempty: Empty, Semigroup0: () => docSemigroup };
var foldr11 = /* @__PURE__ */ foldr1(docMonoid);
var string3 = /* @__PURE__ */ (() => {
  const $0 = arrayMap(text2);
  const $1 = split("\n");
  return (x) => foldlArray((v) => (v1) => {
    if (v.init) {
      return { init: false, acc: v1 };
    }
    return { init: false, acc: $Doc("Cat", v.acc, $Doc("Cat", $Doc("FlatAlt", Line, $Doc("Char", " ")), v1)) };
  })({ init: true, acc: Empty })($0($1(x))).acc;
})();
var fillBreak = (f) => (x) => $Doc(
  "Column",
  (k1) => $Doc(
    "Cat",
    x,
    $Doc(
      "Column",
      (k2) => {
        const $0 = k2 - k1 | 0;
        if ($0 > f) {
          return $Doc("Nest", f, $Doc("FlatAlt", Line, Empty));
        }
        const $1 = f - $0 | 0;
        const $2 = $1 <= 0 ? "" : fromCharArray(replicate($1)(" "));
        if ($2 === "") {
          return Empty;
        }
        return $Doc("Text", toCodePointArray($2).length, $2);
      }
    )
  )
);
var appendWithSpace = (x) => (y) => $Doc("Cat", x, $Doc("Cat", $Doc("Char", " "), y));
var hsep = /* @__PURE__ */ foldr11(appendWithSpace);
var appendWithLinebreak = (x) => (y) => $Doc("Cat", x, $Doc("Cat", $Doc("FlatAlt", Line, Empty), y));
var vcat = /* @__PURE__ */ foldr11(appendWithLinebreak);
var appendWithLine = (x) => (y) => $Doc("Cat", x, $Doc("Cat", $Doc("FlatAlt", Line, $Doc("Char", " ")), y));
var indent = (i) => (d) => {
  const $0 = i <= 0 ? "" : fromCharArray(replicate(i)(" "));
  return $Doc(
    "Column",
    (k) => $Doc(
      "Nesting",
      (i$1) => $Doc("Nest", k - i$1 | 0, $Doc("Nest", i, $Doc("Cat", $0 === "" ? Empty : $Doc("Text", toCodePointArray($0).length, $0), d)))
    )
  );
};

// output-es/Options.Applicative.Help.Chunk/index.js
var chunkMonoid = (dictSemigroup) => {
  const chunkSemigroup1 = {
    append: (v1) => (v2) => {
      if (v1.tag === "Nothing") {
        return v2;
      }
      if (v2.tag === "Nothing") {
        return v1;
      }
      if (v1.tag === "Just" && v2.tag === "Just") {
        return $Maybe("Just", dictSemigroup.append(v1._1)(v2._1));
      }
      fail();
    }
  };
  return { mempty: Nothing, Semigroup0: () => chunkSemigroup1 };
};
var mempty1 = /* @__PURE__ */ (() => chunkMonoid(docSemigroup).mempty)();
var vcatChunks = /* @__PURE__ */ foldrArray((v1) => (v2) => {
  if (v1.tag === "Nothing") {
    return v2;
  }
  if (v2.tag === "Nothing") {
    return v1;
  }
  if (v1.tag === "Just" && v2.tag === "Just") {
    return $Maybe(
      "Just",
      $Doc(
        "Cat",
        v1._1,
        $Doc("Cat", $Doc("FlatAlt", Line, $Doc("Char", " ")), v2._1)
      )
    );
  }
  fail();
})(mempty1);
var vsepChunks = /* @__PURE__ */ foldrArray((v1) => (v2) => {
  if (v1.tag === "Nothing") {
    return v2;
  }
  if (v2.tag === "Nothing") {
    return v1;
  }
  if (v1.tag === "Just" && v2.tag === "Just") {
    return $Maybe(
      "Just",
      $Doc(
        "Cat",
        v1._1,
        $Doc(
          "Cat",
          $Doc("FlatAlt", Line, $Doc("Char", " ")),
          $Doc(
            "Cat",
            Empty,
            $Doc("Cat", $Doc("FlatAlt", Line, $Doc("Char", " ")), v2._1)
          )
        )
      )
    );
  }
  fail();
})(mempty1);
var chunkBesideOrBelow = (v1) => (v2) => {
  if (v1.tag === "Nothing") {
    return v2;
  }
  if (v2.tag === "Nothing") {
    return v1;
  }
  if (v1.tag === "Just" && v2.tag === "Just") {
    return $Maybe("Just", $Doc("Cat", v1._1, $Doc("Cat", softline, v2._1)));
  }
  fail();
};
var listToChunk = (dictMonoid) => {
  const mempty23 = chunkMonoid(dictMonoid.Semigroup0()).mempty;
  const fold12 = foldableArray.foldMap(dictMonoid)(identity3);
  return (v) => {
    if (v.length === 0) {
      return mempty23;
    }
    return $Maybe("Just", fold12(v));
  };
};
var stringChunk = (v) => {
  if (v === "") {
    return mempty1;
  }
  return $Maybe("Just", v === "" ? Empty : $Doc("Text", toCodePointArray(v).length, v));
};
var paragraph = /* @__PURE__ */ (() => {
  const $0 = foldrArray((x) => {
    const $02 = stringChunk(x);
    return (v2) => {
      if ($02.tag === "Nothing") {
        return v2;
      }
      if (v2.tag === "Nothing") {
        return $02;
      }
      if ($02.tag === "Just" && v2.tag === "Just") {
        return $Maybe("Just", $Doc("Cat", $02._1, $Doc("Cat", softline, v2._1)));
      }
      fail();
    };
  })(mempty1);
  return (x) => $0(x === "" ? [] : split2(whitespaceRegex)(x));
})();
var tabulate$p = (v) => (v1) => {
  if (v1.length === 0) {
    return mempty1;
  }
  return $Maybe(
    "Just",
    vcat(arrayMap((v2) => indent(2)($Doc(
      "Cat",
      fillBreak(v)(v2._1),
      $Doc("Cat", $Doc("Char", " "), v2._2)
    )))(v1))
  );
};

// output-es/Control.Monad.Free/index.js
var $Free = (_1, _2) => ({ tag: "Free", _1, _2 });
var $FreeView = (tag, _1, _2) => ({ tag, _1, _2 });
var toView = (toView$a0$copy) => {
  let toView$a0 = toView$a0$copy, toView$c = true, toView$r;
  while (toView$c) {
    const v = toView$a0;
    if (v._1.tag === "Return") {
      const v2 = uncons4(v._2);
      if (v2.tag === "Nothing") {
        toView$c = false;
        toView$r = $FreeView("Return", v._1._1);
        continue;
      }
      if (v2.tag === "Just") {
        toView$a0 = (() => {
          const $0 = v2._1._1(v._1._1);
          return $Free(
            $0._1,
            (() => {
              if ($0._2.tag === "CatNil") {
                return v2._1._2;
              }
              if (v2._1._2.tag === "CatNil") {
                return $0._2;
              }
              if ($0._2.tag === "CatCons") {
                return $CatList("CatCons", $0._2._1, $CatQueue($0._2._2._1, $List("Cons", v2._1._2, $0._2._2._2)));
              }
              fail();
            })()
          );
        })();
        continue;
      }
      fail();
    }
    if (v._1.tag === "Bind") {
      toView$c = false;
      toView$r = $FreeView(
        "Bind",
        v._1._1,
        (a) => {
          const $0 = v._1._2(a);
          return $Free(
            $0._1,
            (() => {
              if ($0._2.tag === "CatNil") {
                return v._2;
              }
              if (v._2.tag === "CatNil") {
                return $0._2;
              }
              if ($0._2.tag === "CatCons") {
                return $CatList("CatCons", $0._2._1, $CatQueue($0._2._2._1, $List("Cons", v._2, $0._2._2._2)));
              }
              fail();
            })()
          );
        }
      );
      continue;
    }
    fail();
  }
  return toView$r;
};
var resume$p = (k) => (j) => (f) => {
  const v = toView(f);
  if (v.tag === "Return") {
    return j(v._1);
  }
  if (v.tag === "Bind") {
    return k(v._1)(v._2);
  }
  fail();
};
var freeMonad = { Applicative0: () => freeApplicative, Bind1: () => freeBind };
var freeFunctor = { map: (k) => (f) => freeBind.bind(f)((x) => freeApplicative.pure(k(x))) };
var freeBind = { bind: (v) => (k) => $Free(v._1, snoc2(v._2)(k)), Apply0: () => freeApply };
var freeApply = {
  apply: (f) => (a) => $Free(f._1, snoc2(f._2)((f$p) => $Free(a._1, snoc2(a._2)((a$p) => freeApplicative.pure(f$p(a$p)))))),
  Functor0: () => freeFunctor
};
var freeApplicative = { pure: (x) => $Free($FreeView("Return", x), CatNil), Apply0: () => freeApply };
var freeMonadRec = {
  tailRecM: (k) => (a) => {
    const $0 = k(a);
    return $Free(
      $0._1,
      snoc2($0._2)((v) => {
        if (v.tag === "Loop") {
          return freeMonadRec.tailRecM(k)(v._1);
        }
        if (v.tag === "Done") {
          return $Free($FreeView("Return", v._1), CatNil);
        }
        fail();
      })
    );
  },
  Monad0: () => freeMonad
};

// output-es/Options.Applicative.Types/index.js
var $ArgPolicy = (tag) => tag;
var $Backtracking = (tag) => tag;
var $Context = (_1, _2) => ({ tag: "Context", _1, _2 });
var $IsCmdStart = (tag) => tag;
var $MultPE = (_1, _2) => ({ tag: "MultPE", _1, _2 });
var $OptName = (tag, _1) => ({ tag, _1 });
var $OptReader = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
var $OptTree = (tag, _1) => ({ tag, _1 });
var $OptVisibility = (tag) => tag;
var $ParseError2 = (tag, _1, _2) => ({ tag, _1, _2 });
var $Parser = (tag, _1, _2) => ({ tag, _1, _2 });
var $ParserResult = (tag, _1) => ({ tag, _1 });
var $SomeParser = (_1) => ({ tag: "SomeParser", _1 });
var apply3 = /* @__PURE__ */ (() => {
  const $0 = applyExceptT(monadIdentity);
  return (v) => (v1) => (r) => $0.apply(v(r))(v1(r));
})();
var bind = /* @__PURE__ */ (() => bindReaderT(bindExceptT(monadIdentity)).bind)();
var Internal = /* @__PURE__ */ $OptVisibility("Internal");
var Hidden = /* @__PURE__ */ $OptVisibility("Hidden");
var Visible = /* @__PURE__ */ $OptVisibility("Visible");
var CmdStart = /* @__PURE__ */ $IsCmdStart("CmdStart");
var CmdCont = /* @__PURE__ */ $IsCmdStart("CmdCont");
var Backtrack = /* @__PURE__ */ $Backtracking("Backtrack");
var Intersperse = /* @__PURE__ */ $ArgPolicy("Intersperse");
var NoIntersperse = /* @__PURE__ */ $ArgPolicy("NoIntersperse");
var AllPositionals = /* @__PURE__ */ $ArgPolicy("AllPositionals");
var NilP = (value0) => $Parser("NilP", value0);
var ShowHelpText = /* @__PURE__ */ $ParseError2("ShowHelpText");
var ExpectsArgError = (value0) => $ParseError2("ExpectsArgError", value0);
var readerAsk = /* @__PURE__ */ (() => applicativeExceptT(monadIdentity).pure)();
var readMFunctor = {
  map: (f) => (v) => (x) => {
    const $0 = v(x);
    if ($0.tag === "Left") {
      return $Either("Left", $0._1);
    }
    if ($0.tag === "Right") {
      return $Either("Right", f($0._1));
    }
    fail();
  }
};
var readMApply = { apply: (v) => (v1) => apply3(v)(v1), Functor0: () => readMFunctor };
var readMApplicative = {
  pure: /* @__PURE__ */ (() => {
    const $0 = applicativeExceptT(monadIdentity);
    return (x) => {
      const $1 = $0.pure(x);
      return (v) => $1;
    };
  })(),
  Apply0: () => readMApply
};
var parseErrorSemigroup = { append: (v) => (m) => m };
var optVisibilityEq = {
  eq: (x) => (y) => {
    if (x === "Internal") {
      return y === "Internal";
    }
    if (x === "Hidden") {
      return y === "Hidden";
    }
    return x === "Visible" && y === "Visible";
  }
};
var optVisibilityOrd = {
  compare: (x) => (y) => {
    if (x === "Internal") {
      if (y === "Internal") {
        return EQ;
      }
      return LT;
    }
    if (y === "Internal") {
      return GT;
    }
    if (x === "Hidden") {
      if (y === "Hidden") {
        return EQ;
      }
      return LT;
    }
    if (y === "Hidden") {
      return GT;
    }
    if (x === "Visible" && y === "Visible") {
      return EQ;
    }
    fail();
  },
  Eq0: () => optVisibilityEq
};
var optNameEq = {
  eq: (x) => (y) => {
    if (x.tag === "OptShort") {
      return y.tag === "OptShort" && x._1 === y._1;
    }
    return x.tag === "OptLong" && y.tag === "OptLong" && x._1 === y._1;
  }
};
var optNameOrd = {
  compare: (x) => (y) => {
    if (x.tag === "OptShort") {
      if (y.tag === "OptShort") {
        return ordChar.compare(x._1)(y._1);
      }
      return LT;
    }
    if (y.tag === "OptShort") {
      return GT;
    }
    if (x.tag === "OptLong" && y.tag === "OptLong") {
      return ordString.compare(x._1)(y._1);
    }
    fail();
  },
  Eq0: () => optNameEq
};
var completerSemigroup = {
  append: (v) => (v1) => (s) => {
    const $0 = v(s);
    const $1 = v1(s);
    return () => {
      const a$p = $0();
      const a$p$1 = $1();
      return [...a$p, ...a$p$1];
    };
  }
};
var completerMonoid = { mempty: (v) => () => [], Semigroup0: () => completerSemigroup };
var parserInfoFunctor = {
  map: (f) => (i) => ({
    infoParser: parserFunctor.map(f)(i.infoParser),
    infoFailureCode: i.infoFailureCode,
    infoFooter: i.infoFooter,
    infoFullDesc: i.infoFullDesc,
    infoHeader: i.infoHeader,
    infoPolicy: i.infoPolicy,
    infoProgDesc: i.infoProgDesc
  })
};
var parserFunctor = {
  map: (v) => (v1) => {
    if (v1.tag === "NilP") {
      return $Parser("NilP", v(v1._1));
    }
    if (v1.tag === "OptP") {
      return $Parser("OptP", optionFunctor.map(v)(v1._1));
    }
    if (v1.tag === "MultP") {
      return $Parser("MultP", $MultPE(parserFunctor.map((v3) => (x) => v(v3(x)))(v1._1._1), v1._1._2));
    }
    if (v1.tag === "AltP") {
      return $Parser("AltP", parserFunctor.map(v)(v1._1), parserFunctor.map(v)(v1._2));
    }
    if (v1.tag === "BindP") {
      return $Parser(
        "BindP",
        $Free(v1._1._1, snoc2(v1._1._2)((x) => $Free($FreeView("Return", v(x)), CatNil)))
      );
    }
    fail();
  }
};
var optionFunctor = { map: (f) => (o) => ({ optMain: optReaderFunctor.map(f)(o.optMain), optProps: o.optProps }) };
var optReaderFunctor = {
  map: (v) => (v1) => {
    if (v1.tag === "OptReader") {
      const $0 = v1._2;
      return $OptReader(
        "OptReader",
        v1._1,
        {
          crReader: (x) => {
            const $1 = $0.crReader(x);
            if ($1.tag === "Left") {
              return $Either("Left", $1._1);
            }
            if ($1.tag === "Right") {
              return $Either("Right", v($1._1));
            }
            fail();
          },
          crCompleter: $0.crCompleter
        },
        v1._3
      );
    }
    if (v1.tag === "FlagReader") {
      return $OptReader("FlagReader", v1._1, v(v1._2));
    }
    if (v1.tag === "ArgReader") {
      const $0 = v1._1;
      return $OptReader(
        "ArgReader",
        {
          crReader: (x) => {
            const $1 = $0.crReader(x);
            if ($1.tag === "Left") {
              return $Either("Left", $1._1);
            }
            if ($1.tag === "Right") {
              return $Either("Right", v($1._1));
            }
            fail();
          },
          crCompleter: $0.crCompleter
        }
      );
    }
    if (v1.tag === "CmdReader") {
      return $OptReader(
        "CmdReader",
        v1._1,
        v1._2,
        (x) => {
          const $0 = v1._3(x);
          if ($0.tag === "Just") {
            return $Maybe("Just", parserInfoFunctor.map(v)($0._1));
          }
          return Nothing;
        }
      );
    }
    fail();
  }
};
var parserApply = { apply: (a) => (b) => $Parser("MultP", $MultPE(a, b)), Functor0: () => parserFunctor };
var manyM = (p) => freeMonadRec.tailRecM((acc) => $Free(
  $FreeView(
    "Bind",
    $Parser("AltP", parserFunctor.map(Loop)(p), $Parser("NilP", $Step("Done", void 0))),
    (x) => $Free($FreeView("Return", x), CatNil)
  ),
  snoc2(CatNil)((aa) => $Free(
    $FreeView(
      "Return",
      (() => {
        if (aa.tag === "Loop") {
          return $Step("Loop", $List("Cons", aa._1, acc));
        }
        if (aa.tag === "Done") {
          return $Step("Done", reverse2(acc));
        }
        fail();
      })()
    ),
    CatNil
  ))
))(Nil);

// output-es/Options.Applicative.Builder.Internal/index.js
var $DefaultProp = (_1, _2) => ({ tag: "DefaultProp", _1, _2 });
var $Mod = (_1, _2, _3) => ({ tag: "Mod", _1, _2, _3 });
var lookup4 = /* @__PURE__ */ lookup(foldableArray)(eqString);
var identity26 = (x) => x;
var Mod = (value0) => (value1) => (value2) => $Mod(value0, value1, value2);
var optionFieldsHasName = { name: (n) => (fields) => ({ optNames: [n, ...fields.optNames], optCompleter: fields.optCompleter, optNoArgError: fields.optNoArgError }) };
var mkCommand = (m) => {
  const v = m._1({ cmdCommands: [], cmdGroup: Nothing });
  const $0 = v.cmdCommands;
  return $Tuple(v.cmdGroup, $Tuple(arrayMap(fst)($0), $Tuple((v1) => lookup4(v1)($0), void 0)));
};
var modSemigroup = {
  append: (v) => (v1) => $Mod((x) => v1._1(v._1(x)), $DefaultProp(v1._2._1.tag === "Nothing" ? v._2._1 : v1._2._1, v1._2._2.tag === "Nothing" ? v._2._2 : v1._2._2), (x) => v1._3(v._3(x)))
};
var modMonoid = { mempty: /* @__PURE__ */ $Mod(identity26, /* @__PURE__ */ $DefaultProp(Nothing, Nothing), identity26), Semigroup0: () => modSemigroup };
var optionMod = /* @__PURE__ */ Mod(identity26)(/* @__PURE__ */ $DefaultProp(Nothing, Nothing));
var internal = /* @__PURE__ */ optionMod((p) => ({ propVisibility: Internal, propDescMod: p.propDescMod, propHelp: p.propHelp, propMetaVar: p.propMetaVar, propShowDefault: p.propShowDefault }));
var baseProps = /* @__PURE__ */ (() => ({
  propMetaVar: "",
  propVisibility: Visible,
  propHelp: chunkMonoid(docSemigroup).mempty,
  propShowDefault: Nothing,
  propDescMod: Nothing
}))();
var mkProps = (v) => (g) => {
  const $0 = g(baseProps);
  return {
    propShowDefault: applyMaybe.apply(v._2)(v._1),
    propDescMod: $0.propDescMod,
    propHelp: $0.propHelp,
    propMetaVar: $0.propMetaVar,
    propVisibility: $0.propVisibility
  };
};
var mkParser = (v) => (g) => (rdr) => {
  const o = $Parser("OptP", { optMain: rdr, optProps: mkProps(v)(g) });
  if (v._1.tag === "Nothing") {
    return o;
  }
  if (v._1.tag === "Just") {
    return $Parser("AltP", o, $Parser("NilP", v._1._1));
  }
  fail();
};

// output-es/Options.Applicative.Builder/index.js
var identity27 = (x) => x;
var mempty12 = /* @__PURE__ */ (() => chunkMonoid(docSemigroup).mempty)();
var min3 = (x) => (y) => {
  const v = optVisibilityOrd.compare(x)(y);
  if (v === "LT") {
    return x;
  }
  if (v === "EQ") {
    return x;
  }
  if (v === "GT") {
    return y;
  }
  fail();
};
var fold3 = /* @__PURE__ */ (() => foldableArray.foldMap(modMonoid)(identity3))();
var progDesc = (s) => (i) => ({
  infoProgDesc: paragraph(s),
  infoFailureCode: i.infoFailureCode,
  infoFooter: i.infoFooter,
  infoFullDesc: i.infoFullDesc,
  infoHeader: i.infoHeader,
  infoParser: i.infoParser,
  infoPolicy: i.infoPolicy
});
var option = (r) => (m) => {
  const $0 = optionMod((p) => ({ propMetaVar: "ARG", propDescMod: p.propDescMod, propHelp: p.propHelp, propShowDefault: p.propShowDefault, propVisibility: p.propVisibility }));
  const $1 = m._1($0._1({ optNames: [], optCompleter: completerMonoid.mempty, optNoArgError: ExpectsArgError }));
  return mkParser($DefaultProp(
    m._2._1.tag === "Nothing" ? $0._2._1 : m._2._1,
    m._2._2.tag === "Nothing" ? $0._2._2 : m._2._2
  ))((x) => m._3($0._3(x)))($OptReader("OptReader", $1.optNames, { crCompleter: $1.optCompleter, crReader: r }, $1.optNoArgError));
};
var subparser = (m) => {
  const $0 = optionMod((p) => ({ propMetaVar: "COMMAND", propDescMod: p.propDescMod, propHelp: p.propHelp, propShowDefault: p.propShowDefault, propVisibility: p.propVisibility }));
  const v1 = mkCommand(m);
  return mkParser($DefaultProp(
    m._2._1.tag === "Nothing" ? $0._2._1 : m._2._1,
    m._2._2.tag === "Nothing" ? $0._2._2 : m._2._2
  ))((x) => m._3($0._3(x)))($OptReader("CmdReader", v1._1, v1._2._1, v1._2._2._1));
};
var hidden = /* @__PURE__ */ optionMod((p) => ({
  propVisibility: min3(Hidden)(p.propVisibility),
  propDescMod: p.propDescMod,
  propHelp: p.propHelp,
  propMetaVar: p.propMetaVar,
  propShowDefault: p.propShowDefault
}));
var help = (s) => optionMod((p) => ({
  propHelp: paragraph(s),
  propDescMod: p.propDescMod,
  propMetaVar: p.propMetaVar,
  propShowDefault: p.propShowDefault,
  propVisibility: p.propVisibility
}));
var header = (s) => (i) => ({
  infoHeader: paragraph(s),
  infoFailureCode: i.infoFailureCode,
  infoFooter: i.infoFooter,
  infoFullDesc: i.infoFullDesc,
  infoParser: i.infoParser,
  infoPolicy: i.infoPolicy,
  infoProgDesc: i.infoProgDesc
});
var flag$p = (actv) => (v) => mkParser(v._2)(v._3)((() => {
  const $0 = v._1({ flagNames: [], flagActive: actv });
  return $OptReader("FlagReader", $0.flagNames, $0.flagActive);
})());
var eitherReader = (f) => bind(readerAsk)((x) => {
  const $0 = f(x);
  if ($0.tag === "Left") {
    const $1 = monadThrowExceptT(monadIdentity).throwError($ParseError2("ErrorMsg", $0._1));
    return (v) => $1;
  }
  if ($0.tag === "Right") {
    return readMApplicative.pure($0._1);
  }
  fail();
});
var $$int2 = /* @__PURE__ */ eitherReader((s) => {
  const v = fromString(s);
  if (v.tag === "Nothing") {
    return $Either("Left", "Can't parse as Int: `" + showStringImpl(s) + "`");
  }
  if (v.tag === "Just") {
    return $Either("Right", v._1);
  }
  fail();
});
var defaultPrefs = {
  prefMultiSuffix: "",
  prefDisambiguate: false,
  prefShowHelpOnError: false,
  prefShowHelpOnEmpty: false,
  prefBacktrack: Backtrack,
  prefColumns: 80
};
var command = (cmd) => (pinfo) => $Mod(
  (p) => ({ cmdCommands: [$Tuple(cmd, pinfo), ...p.cmdCommands], cmdGroup: p.cmdGroup }),
  $DefaultProp(Nothing, Nothing),
  identity26
);
var abortOption = (err) => (m) => {
  const $0 = fold3([
    $Mod(
      (p) => ({ optNoArgError: (v) => err, optCompleter: p.optCompleter, optNames: p.optNames }),
      $DefaultProp(Nothing, Nothing),
      identity26
    ),
    $Mod(
      identity27,
      $DefaultProp($Maybe("Just", identity27), Nothing),
      identity27
    ),
    optionMod((p) => ({ propMetaVar: "", propDescMod: p.propDescMod, propHelp: p.propHelp, propShowDefault: p.propShowDefault, propVisibility: p.propVisibility }))
  ]);
  return option((() => {
    const $1 = monadThrowExceptT(monadIdentity).throwError(err);
    return (v) => $1;
  })())($Mod(
    (x) => m._1($0._1(x)),
    $DefaultProp(m._2._1.tag === "Nothing" ? $0._2._1 : m._2._1, m._2._2.tag === "Nothing" ? $0._2._2 : m._2._2),
    (x) => m._3($0._3(x))
  ));
};

// output-es/Node.Process/foreign.js
import process from "process";
function exit(code) {
  return () => {
    process.exit(code);
  };
}
function copyArray(xs) {
  return () => xs.slice();
}

// output-es/Node.Process/index.js
var argv = /* @__PURE__ */ (() => copyArray(process.argv))();

// output-es/Node.Stream/foreign.js
function writeStringImpl(w) {
  return (enc) => (s) => (done) => () => w.write(s, enc, done);
}

// output-es/Node.Stream/index.js
var writeString = (w) => (enc) => (s) => (cb) => writeStringImpl(w)((() => {
  if (enc === "ASCII") {
    return "ASCII";
  }
  if (enc === "UTF8") {
    return "UTF8";
  }
  if (enc === "UTF16LE") {
    return "UTF16LE";
  }
  if (enc === "UCS2") {
    return "UCS2";
  }
  if (enc === "Base64") {
    return "Base64";
  }
  if (enc === "Latin1") {
    return "Latin1";
  }
  if (enc === "Binary") {
    return "Binary";
  }
  if (enc === "Hex") {
    return "Hex";
  }
  fail();
})())(s)((x) => cb(nullable(x, Nothing, Just))());

// output-es/Options.Applicative.Internal/index.js
var $ComplResult = (tag, _1, _2) => ({ tag, _1, _2 });
var $TStep = (tag, _1, _2) => ({ tag, _1, _2 });
var monadReaderT2 = /* @__PURE__ */ monadReaderT(monadIdentity);
var monadStateT = { Applicative0: () => applicativeStateT(monadReaderT2), Bind1: () => bindStateT(monadReaderT2) };
var apply4 = /* @__PURE__ */ (() => applyExceptT(monadStateT).apply)();
var bind2 = /* @__PURE__ */ (() => bindExceptT(monadStateT).bind)();
var pure2 = /* @__PURE__ */ (() => applicativeExceptT(monadStateT).pure)();
var alt = /* @__PURE__ */ (() => altExceptT(parseErrorSemigroup)(monadStateT).alt)();
var lift1 = (m) => bindStateT(monadReaderT2).bind(m)((a) => applicativeStateT(monadReaderT2).pure($Either(
  "Right",
  a
)));
var modify_ = /* @__PURE__ */ (() => {
  const $0 = monadStateStateT(monadReaderT2);
  return (f) => $0.state((s) => $Tuple(void 0, f(s)));
})();
var throwError = /* @__PURE__ */ (() => monadThrowExceptT(monadStateT).throwError)();
var identity28 = (x) => x;
var TNil = /* @__PURE__ */ $TStep("TNil");
var ComplResult = (value0) => $ComplResult("ComplResult", value0);
var runListT = (dictMonad) => (xs) => dictMonad.Bind1().bind(xs)((s) => {
  if (s.tag === "TNil") {
    return dictMonad.Applicative0().pure(Nil);
  }
  if (s.tag === "TCons") {
    const $0 = Cons(s._1);
    return dictMonad.Bind1().bind(runListT(dictMonad)(s._2))((a$p) => dictMonad.Applicative0().pure($0(a$p)));
  }
  fail();
});
var runCompletion = (v) => (prefs) => {
  const v1 = v(prefs);
  if (v1.tag === "ComplResult") {
    return Nothing;
  }
  if (v1.tag === "ComplParser") {
    return $Maybe("Just", $Either("Left", $Tuple(v1._1, v1._2)));
  }
  if (v1.tag === "ComplOption") {
    return $Maybe("Just", $Either("Right", v1._1));
  }
  fail();
};
var pFunctor = {
  map: (f) => (v) => (s) => {
    const $0 = v(s);
    return (x) => {
      const $1 = $0(x);
      return $Tuple(
        (() => {
          if ($1._1.tag === "Left") {
            return $Either("Left", $1._1._1);
          }
          if ($1._1.tag === "Right") {
            return $Either("Right", f($1._1._1));
          }
          fail();
        })(),
        $1._2
      );
    };
  }
};
var pApply = { apply: (v) => (v1) => apply4(v)(v1), Functor0: () => pFunctor };
var pBind = { bind: (v) => (k) => bind2(v)((a) => k(a)), Apply0: () => pApply };
var pApplicative = { pure: (a) => pure2(a), Apply0: () => pApply };
var pMonad = { Applicative0: () => pApplicative, Bind1: () => pBind };
var pAlt = { alt: (v) => (v1) => alt(v)(v1), Functor0: () => pFunctor };
var pMonadP$lazy = /* @__PURE__ */ binding(() => ({
  enterContext: (name2) => (pinfo) => lift1(modify_(cons2($Context(name2, pinfo)))),
  exitContext: lift1(modify_(drop(1))),
  getPrefs: lift1((s) => monadReaderT2.Bind1().bind(Identity)((x) => monadReaderT2.Applicative0().pure($Tuple(x, s)))),
  missingArgP: (e) => (v) => pMonadP$lazy().errorP(e),
  exitP: (i) => (v) => (p) => {
    const $0 = throwError($ParseError2("MissingError", i, $SomeParser(p)));
    return (x) => {
      if (x.tag === "Nothing") {
        return $0;
      }
      if (x.tag === "Just") {
        return pure2(x._1);
      }
      fail();
    };
  },
  errorP: (x) => throwError(x),
  Monad0: () => pMonad,
  Alt1: () => pAlt
}));
var pMonadP = /* @__PURE__ */ pMonadP$lazy();
var complResultMonad = { Applicative0: () => complResultApplicative, Bind1: () => complResultBind };
var complResultFunctor = { map: (f) => (a) => complResultBind.bind(a)((a$p) => complResultApplicative.pure(f(a$p))) };
var complResultBind = {
  bind: (m) => (f) => {
    if (m.tag === "ComplResult") {
      return f(m._1);
    }
    if (m.tag === "ComplParser") {
      return $ComplResult("ComplParser", m._1, m._2);
    }
    if (m.tag === "ComplOption") {
      return $ComplResult("ComplOption", m._1);
    }
    fail();
  },
  Apply0: () => complResultApply
};
var complResultApply = {
  apply: (f) => (a) => {
    if (f.tag === "ComplResult") {
      if (a.tag === "ComplResult") {
        return complResultApplicative.pure(f._1(a._1));
      }
      if (a.tag === "ComplParser") {
        return $ComplResult("ComplParser", a._1, a._2);
      }
      if (a.tag === "ComplOption") {
        return $ComplResult("ComplOption", a._1);
      }
      fail();
    }
    if (f.tag === "ComplParser") {
      return $ComplResult("ComplParser", f._1, f._2);
    }
    if (f.tag === "ComplOption") {
      return $ComplResult("ComplOption", f._1);
    }
    fail();
  },
  Functor0: () => complResultFunctor
};
var complResultApplicative = { pure: ComplResult, Apply0: () => complResultApply };
var monadReaderT1 = /* @__PURE__ */ monadReaderT(complResultMonad);
var alt1 = /* @__PURE__ */ (() => altExceptT(parseErrorSemigroup)(monadReaderT1).alt)();
var apply1 = /* @__PURE__ */ (() => applyExceptT(monadReaderT1).apply)();
var pure22 = /* @__PURE__ */ (() => applicativeExceptT(monadReaderT1).pure)();
var bind1 = /* @__PURE__ */ (() => bindExceptT(monadReaderT1).bind)();
var lift3 = (m) => monadReaderT1.Bind1().bind(m)((a) => monadReaderT1.Applicative0().pure($Either("Right", a)));
var completionFunctor = {
  map: (f) => (v) => (x) => {
    const $0 = v(x);
    if ($0.tag === "ComplResult") {
      return $ComplResult(
        "ComplResult",
        (() => {
          if ($0._1.tag === "Left") {
            return $Either("Left", $0._1._1);
          }
          if ($0._1.tag === "Right") {
            return $Either("Right", f($0._1._1));
          }
          fail();
        })()
      );
    }
    if ($0.tag === "ComplParser") {
      return $ComplResult("ComplParser", $0._1, $0._2);
    }
    if ($0.tag === "ComplOption") {
      return $ComplResult("ComplOption", $0._1);
    }
    fail();
  }
};
var completionAlt = { alt: (v) => (v1) => alt1(v)(v1), Functor0: () => completionFunctor };
var completionApply = { apply: (v) => (v1) => apply1(v)(v1), Functor0: () => completionFunctor };
var completionApplicative = { pure: (a) => pure22(a), Apply0: () => completionApply };
var completionBind = { bind: (v) => (k) => bind1(v)((a) => k(a)), Apply0: () => completionApply };
var completionMonad = { Applicative0: () => completionApplicative, Bind1: () => completionBind };
var completionMonadP = {
  enterContext: (v) => (v1) => pure22(),
  exitContext: /* @__PURE__ */ pure22(),
  getPrefs: /* @__PURE__ */ lift3(ComplResult),
  missingArgP: (v) => (x) => lift3((v$1) => $ComplResult("ComplOption", x)),
  exitP: (v) => (a) => (p) => (v1) => lift3((v$1) => $ComplResult("ComplParser", $SomeParser(p), a)),
  errorP: (x) => monadThrowExceptT(monadReaderT1).throwError(x),
  Monad0: () => completionMonad,
  Alt1: () => completionAlt
};
var bimapTStep = (v) => (v1) => (v2) => {
  if (v2.tag === "TNil") {
    return TNil;
  }
  if (v2.tag === "TCons") {
    return $TStep("TCons", v(v2._1), v1(v2._2));
  }
  fail();
};
var listTFunctor = (dictMonad) => ({
  map: (f) => (v) => {
    const $0 = bimapTStep(f)(listTFunctor(dictMonad).map(f));
    return dictMonad.Bind1().bind(v)((a$p) => dictMonad.Applicative0().pure($0(a$p)));
  }
});
var listTAlt = (dictMonad) => {
  const listTFunctor1 = listTFunctor(dictMonad);
  return {
    alt: (xs) => (ys) => dictMonad.Bind1().bind(xs)((s) => {
      if (s.tag === "TNil") {
        return ys;
      }
      if (s.tag === "TCons") {
        return dictMonad.Applicative0().pure($TStep("TCons", s._1, listTAlt(dictMonad).alt(s._2)(ys)));
      }
      fail();
    }),
    Functor0: () => listTFunctor1
  };
};
var listTPlus = (dictMonad) => {
  const listTAlt1 = listTAlt(dictMonad);
  return { empty: dictMonad.Applicative0().pure(TNil), Alt0: () => listTAlt1 };
};
var hoistList = (dictMonad) => foldrArray((x) => (xt) => dictMonad.Applicative0().pure($TStep("TCons", x, xt)))(listTPlus(dictMonad).empty);
var listTMonadTrans = {
  lift: (dictMonad) => {
    const empty3 = listTPlus(dictMonad).empty;
    return (x) => dictMonad.Bind1().bind(x)((a$p) => dictMonad.Applicative0().pure($TStep("TCons", a$p, empty3)));
  }
};
var cut = (dictMonad) => listTMonadTrans.lift({
  Applicative0: () => applicativeStateT(dictMonad),
  Bind1: () => bindStateT(dictMonad)
})(monadStateStateT(dictMonad).state((v) => $Tuple(void 0, true)));
var nondetTMonadTrans = {
  lift: (dictMonad) => {
    const $0 = listTMonadTrans.lift({ Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) });
    return (x) => $0((s) => dictMonad.Bind1().bind(x)((x$1) => dictMonad.Applicative0().pure($Tuple(x$1, s))));
  }
};
var listTBind = (dictMonad) => ({
  bind: (xs) => (f) => dictMonad.Bind1().bind(xs)((s) => {
    if (s.tag === "TNil") {
      return dictMonad.Applicative0().pure(TNil);
    }
    if (s.tag === "TCons") {
      return listTAlt(dictMonad).alt(f(s._1))(listTBind(dictMonad).bind(s._2)(f));
    }
    fail();
  }),
  Apply0: () => listTApply(dictMonad)
});
var listTApply = (dictMonad) => {
  const listTFunctor1 = listTFunctor(dictMonad);
  return {
    apply: (() => {
      const $0 = listTBind(dictMonad);
      return (f) => (a) => $0.bind(f)((f$p) => $0.bind(a)((a$p) => listTApplicative(dictMonad).pure(f$p(a$p))));
    })(),
    Functor0: () => listTFunctor1
  };
};
var listTApplicative = (dictMonad) => ({
  pure: (() => {
    const $0 = hoistList(dictMonad);
    return (x) => $0([x]);
  })(),
  Apply0: () => listTApply(dictMonad)
});
var listTAlternative = (dictMonad) => {
  const listTApplicative1 = listTApplicative(dictMonad);
  const listTPlus1 = listTPlus(dictMonad);
  return { Applicative0: () => listTApplicative1, Plus1: () => listTPlus1 };
};
var nondetTAltOp = (dictMonad) => {
  const monadStateT1 = { Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) };
  const listTBind1 = listTBind(monadStateT1);
  const lift6 = listTMonadTrans.lift(monadStateT1);
  const $$get2 = monadStateStateT(dictMonad).state((s) => $Tuple(s, s));
  const $0 = listTAlternative(monadStateT1);
  const empty3 = $0.Plus1().empty;
  return (m1) => (m2) => listTAlt(monadStateT1).alt(m1)(listTBind1.bind(lift6($$get2))((s) => listTBind1.bind(!s ? $0.Applicative0().pure() : empty3)(() => m2)));
};
var nondetTFunctor = (dictMonad) => ({ map: (f) => listTFunctor({ Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) }).map(f) });
var nondetTAlt = (dictMonad) => {
  const nondetTFunctor1 = nondetTFunctor(dictMonad);
  return {
    alt: (v) => (v1) => listTAlt({ Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) }).alt(v)(v1),
    Functor0: () => nondetTFunctor1
  };
};
var nondetTPlus = (dictMonad) => {
  const nondetTAlt1 = nondetTAlt(dictMonad);
  return {
    empty: listTPlus({ Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) }).empty,
    Alt0: () => nondetTAlt1
  };
};
var nondetTApply = (dictMonad) => {
  const nondetTFunctor1 = nondetTFunctor(dictMonad);
  return {
    apply: (v) => (v1) => listTApply({ Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) }).apply(v)(v1),
    Functor0: () => nondetTFunctor1
  };
};
var nondetTApplicative = (dictMonad) => {
  const nondetTApply1 = nondetTApply(dictMonad);
  return {
    pure: (x) => listTApplicative({ Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) }).pure(x),
    Apply0: () => nondetTApply1
  };
};
var nondetTBind = (dictMonad) => {
  const nondetTApply1 = nondetTApply(dictMonad);
  return {
    bind: (v) => (f) => listTBind({ Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) }).bind(v)((x) => f(x)),
    Apply0: () => nondetTApply1
  };
};
var takeListT = (dictMonad) => {
  const empty3 = listTPlus(dictMonad).empty;
  return (v) => {
    if (v === 0) {
      return (v$1) => empty3;
    }
    const $0 = bimapTStep(identity28)(takeListT(dictMonad)(v - 1 | 0));
    return (x) => dictMonad.Bind1().bind(x)((a$p) => dictMonad.Applicative0().pure($0(a$p)));
  };
};
var disamb = (dictMonad) => {
  const Bind1 = dictMonad.Bind1();
  const evalStateT2 = evalStateT(Bind1.Apply0().Functor0());
  const monadStateT1 = { Applicative0: () => applicativeStateT(dictMonad), Bind1: () => bindStateT(dictMonad) };
  const takeListT1 = takeListT(monadStateT1);
  return (allow_amb) => (xs) => Bind1.bind(evalStateT2(runListT(monadStateT1)(takeListT1(allow_amb ? 1 : 2)(xs)))(false))((xs$p) => dictMonad.Applicative0().pure(xs$p.tag === "Cons" && xs$p._2.tag === "Nil" ? $Maybe("Just", xs$p._1) : Nothing));
};

// output-es/Options.Applicative.Common/index.js
var $OptWord = (_1, _2) => ({ tag: "OptWord", _1, _2 });
var fromFoldable20 = /* @__PURE__ */ (() => fromFoldableImpl(foldableList.foldr))();
var any2 = /* @__PURE__ */ (() => foldableArray.foldMap((() => {
  const semigroupDisj1 = { append: (v) => (v1) => v || v1 };
  return { mempty: false, Semigroup0: () => semigroupDisj1 };
})()))();
var elem2 = /* @__PURE__ */ (() => {
  const any1 = foldableArray.foldMap((() => {
    const semigroupDisj1 = { append: (v) => (v1) => v || v1 };
    return { mempty: false, Semigroup0: () => semigroupDisj1 };
  })());
  return (x) => any1((y) => {
    if (x.tag === "OptShort") {
      return y.tag === "OptShort" && x._1 === y._1;
    }
    return x.tag === "OptLong" && y.tag === "OptLong" && x._1 === y._1;
  });
})();
var simplify = (v) => {
  if (v.tag === "Leaf") {
    return $OptTree("Leaf", v._1);
  }
  if (v.tag === "MultNode") {
    const v1 = arrayBind(v._1)((x) => {
      const $0 = simplify(x);
      if ($0.tag === "MultNode") {
        return $0._1;
      }
      return [$0];
    });
    if (v1.length === 1) {
      return v1[0];
    }
    return $OptTree("MultNode", v1);
  }
  if (v.tag === "AltNode") {
    const v1 = arrayBind(v._1)((x) => {
      const $0 = simplify(x);
      if ($0.tag === "AltNode") {
        return $0._1;
      }
      if ($0.tag === "MultNode" && $0._1.length === 0) {
        return [];
      }
      return [$0];
    });
    if (v1.length === 0) {
      return $OptTree("MultNode", []);
    }
    if (v1.length === 1) {
      return v1[0];
    }
    return $OptTree("AltNode", v1);
  }
  fail();
};
var showOption = (v) => {
  if (v.tag === "OptLong") {
    return "--" + v._1;
  }
  if (v.tag === "OptShort") {
    return fromCharArray(["-", v._1]);
  }
  fail();
};
var parseWord = /* @__PURE__ */ (() => {
  const $0 = foldrArray(Cons)(Nil);
  return (x) => {
    const $1 = $0(toCharArray(x));
    if ($1.tag === "Cons" && $1._1 === "-") {
      if ($1._2.tag === "Cons" && $1._2._1 === "-") {
        return $Maybe(
          "Just",
          (() => {
            const v2 = span((v3) => v3 !== "=")($1._2._2);
            if (v2.rest.tag === "Nil") {
              return $OptWord($OptName("OptLong", fromCharArray(fromFoldable20($1._2._2))), Nothing);
            }
            if (v2.rest.tag === "Cons") {
              return $OptWord(
                $OptName("OptLong", fromCharArray(fromFoldable20(v2.init))),
                $Maybe("Just", fromCharArray(fromFoldable20(v2.rest._2)))
              );
            }
            fail();
          })()
        );
      }
      if ($1._2.tag === "Nil") {
        return Nothing;
      }
      if ($1._2.tag === "Cons") {
        return $Maybe(
          "Just",
          $OptWord(
            $OptName("OptShort", $1._2._1),
            $1._2._2.tag !== "Nil" ? $Maybe("Just", fromCharArray(fromFoldable20($1._2._2))) : Nothing
          )
        );
      }
      fail();
    }
    return Nothing;
  };
})();
var isOptionPrefix = (v) => (v1) => {
  if (v.tag === "OptShort") {
    return v1.tag === "OptShort" && v._1 === v1._1;
  }
  return v.tag === "OptLong" && v1.tag === "OptLong" && startsWith(v._1)(v1._1);
};
var optMatches = (dictMonadP) => {
  const Monad0 = dictMonadP.Monad0();
  const bindStateT2 = bindStateT(Monad0);
  const monadStateStateT2 = monadStateStateT(Monad0);
  const $$get2 = monadStateStateT2.state((s) => $Tuple(s, s));
  const $0 = applicativeStateT(Monad0);
  const $1 = dictMonadP.Monad0().Applicative0().pure;
  return (disambiguate) => (opt) => (v) => {
    if (opt.tag === "OptReader") {
      const $2 = (disambiguate ? any2(isOptionPrefix(v._1))(opt._1) : elem2(v._1)(opt._1)) ? $Maybe("Just", void 0) : Nothing;
      if ($2.tag === "Just") {
        return $Maybe(
          "Just",
          bindStateT2.bind($$get2)((args) => {
            const missing_arg = dictMonadP.missingArgP(opt._3(showOption(v._1)))(opt._2.crCompleter);
            return bindStateT2.bind((() => {
              if (v._2.tag === "Nothing") {
                if (args.tag === "Nil") {
                  return (s) => Monad0.Bind1().bind(missing_arg)((x) => Monad0.Applicative0().pure($Tuple(x, s)));
                }
                if (args.tag === "Cons") {
                  return $0.pure($Tuple(args._1, args._2));
                }
                fail();
              }
              if (v._2.tag === "Just") {
                return $0.pure($Tuple(v._2._1, args));
              }
              fail();
            })())((v1) => {
              const $3 = v1._1;
              const $4 = v1._2;
              return bindStateT2.bind(monadStateStateT2.state((v$1) => $Tuple(void 0, $4)))(() => {
                const $5 = opt._2.crReader($3);
                if ($5.tag === "Right") {
                  const $6 = $1($5._1);
                  return (s) => Monad0.Bind1().bind($6)((x) => Monad0.Applicative0().pure($Tuple(x, s)));
                }
                if ($5.tag === "Left") {
                  const $6 = dictMonadP.errorP($5._1.tag === "ErrorMsg" ? $ParseError2("ErrorMsg", "option " + showOption(v._1) + ": " + $5._1._1) : $5._1);
                  return (s) => Monad0.Bind1().bind($6)((x) => Monad0.Applicative0().pure($Tuple(x, s)));
                }
                fail();
              });
            });
          })
        );
      }
      if ($2.tag === "Nothing") {
        return Nothing;
      }
      fail();
    }
    if (opt.tag === "FlagReader" && (disambiguate ? any2(isOptionPrefix(v._1))(opt._1) : elem2(v._1)(opt._1)) && ((() => {
      if (v._1.tag === "OptShort") {
        return true;
      }
      if (v._1.tag === "OptLong") {
        return false;
      }
      fail();
    })() || (() => {
      if (v._2.tag === "Nothing") {
        return true;
      }
      if (v._2.tag === "Just") {
        return false;
      }
      fail();
    })())) {
      return $Maybe(
        "Just",
        bindStateT2.bind($$get2)((args) => bindStateT2.bind((() => {
          if (v._2.tag === "Just") {
            const $2 = $List("Cons", fromCharArray(["-", ...toCharArray(v._2._1)]), args);
            return monadStateStateT2.state((v$1) => $Tuple(void 0, $2));
          }
          return monadStateStateT2.state((v$1) => $Tuple(void 0, args));
        })())(() => $0.pure(opt._2)))
      );
    }
    return Nothing;
  };
};
var evalParser = (v) => {
  if (v.tag === "NilP") {
    return $Maybe("Just", v._1);
  }
  if (v.tag === "OptP") {
    return Nothing;
  }
  if (v.tag === "MultP") {
    return applyMaybe.apply(evalParser(v._1._1))(evalParser(v._1._2));
  }
  if (v.tag === "AltP") {
    const $0 = evalParser(v._1);
    const $1 = evalParser(v._2);
    if ($0.tag === "Nothing") {
      return $1;
    }
    return $0;
  }
  if (v.tag === "BindP") {
    return resume$p((p) => (k) => {
      const $0 = evalParser(p);
      if ($0.tag === "Just") {
        return evalParser($Parser("BindP", k($0._1)));
      }
      if ($0.tag === "Nothing") {
        return Nothing;
      }
      fail();
    })(Just)(v._1);
  }
  fail();
};
var searchParser = (dictMonad) => {
  const nondetTPlus2 = nondetTPlus(dictMonad);
  const empty3 = nondetTPlus2.empty;
  const $0 = nondetTFunctor(dictMonad);
  const nondetTAltOp2 = nondetTAltOp(dictMonad);
  const oneOf1 = foldrArray(nondetTPlus2.Alt0().alt)(nondetTPlus2.empty);
  return (v) => (v1) => {
    if (v1.tag === "NilP") {
      return empty3;
    }
    if (v1.tag === "OptP") {
      return v(v1._1);
    }
    if (v1.tag === "MultP") {
      const $1 = v1._1._1;
      const $2 = v1._1._2;
      return nondetTAltOp2($0.map((p1$p) => $Parser("MultP", $MultPE(p1$p, $2)))(searchParser(dictMonad)(v)($1)))($0.map((p2$p) => $Parser(
        "MultP",
        $MultPE($1, p2$p)
      ))(searchParser(dictMonad)(v)($2)));
    }
    if (v1.tag === "AltP") {
      return oneOf1([searchParser(dictMonad)(v)(v1._1), searchParser(dictMonad)(v)(v1._2)]);
    }
    if (v1.tag === "BindP") {
      return resume$p((p) => (k) => oneOf1([
        $0.map((p$p) => $Parser(
          "BindP",
          $Free(
            $FreeView("Bind", p$p, (x) => $Free($FreeView("Return", x), CatNil)),
            snoc2(CatNil)(k)
          )
        ))(searchParser(dictMonad)(v)(p)),
        (() => {
          const v2 = evalParser(p);
          if (v2.tag === "Nothing") {
            return empty3;
          }
          if (v2.tag === "Just") {
            return searchParser(dictMonad)(v)($Parser("BindP", k(v2._1)));
          }
          fail();
        })()
      ]))((v$1) => empty3)(v1._1);
    }
    fail();
  };
};
var searchOpt = (dictMonadP) => {
  const $0 = dictMonadP.Monad0();
  const monadStateT2 = { Applicative0: () => applicativeStateT($0), Bind1: () => bindStateT($0) };
  const searchParser1 = searchParser(monadStateT2);
  const optMatches1 = optMatches(dictMonadP);
  const lift2 = nondetTMonadTrans.lift(monadStateT2);
  const $1 = dictMonadP.Alt1().Functor0();
  const empty3 = nondetTPlus(monadStateT2).empty;
  return (pprefs) => (w) => searchParser1((opt) => {
    const v = optMatches1(pprefs.prefDisambiguate && optVisibilityOrd.compare(opt.optProps.propVisibility)(Internal) === "GT")(opt.optMain)(w);
    if (v.tag === "Just") {
      return lift2((s) => $1.map((v1) => $Tuple($Parser("NilP", v1._1), v1._2))(v._1(s)));
    }
    if (v.tag === "Nothing") {
      return empty3;
    }
    fail();
  });
};
var stepParser = (dictMonadP) => {
  const searchOpt1 = searchOpt(dictMonadP);
  return (v) => (v1) => (v2) => (v3) => {
    if (v1 === "AllPositionals") {
      return searchArg(dictMonadP)(v)(v2)(v3);
    }
    if (v1 === "ForwardOptions") {
      const v42 = parseWord(v2);
      if (v42.tag === "Just") {
        return nondetTAlt((() => {
          const $0 = dictMonadP.Monad0();
          return { Applicative0: () => applicativeStateT($0), Bind1: () => bindStateT($0) };
        })()).alt(searchOpt1(v)(v42._1)(v3))(searchArg(dictMonadP)(v)(v2)(v3));
      }
      if (v42.tag === "Nothing") {
        return searchArg(dictMonadP)(v)(v2)(v3);
      }
      fail();
    }
    const v4 = parseWord(v2);
    if (v4.tag === "Just") {
      return searchOpt1(v)(v4._1)(v3);
    }
    if (v4.tag === "Nothing") {
      return searchArg(dictMonadP)(v)(v2)(v3);
    }
    fail();
  };
};
var searchArg = (dictMonadP) => {
  const Monad0 = dictMonadP.Monad0();
  const monadStateT2 = { Applicative0: () => applicativeStateT(Monad0), Bind1: () => bindStateT(Monad0) };
  const searchParser1 = searchParser(monadStateT2);
  const $0 = nondetTApplicative(monadStateT2);
  const cut2 = cut(monadStateT2);
  const lift2 = nondetTMonadTrans.lift(monadStateT2);
  const bindStateT2 = bindStateT(Monad0);
  const $1 = applyStateT(Monad0);
  const monadStateStateT2 = monadStateStateT(Monad0);
  const $$get2 = monadStateStateT2.state((s) => $Tuple(s, s));
  const $2 = dictMonadP.Alt1().Functor0();
  const Apply0 = Monad0.Bind1().Apply0();
  const exitContext = dictMonadP.exitContext;
  const $3 = nondetTFunctor(monadStateT2);
  const empty3 = nondetTPlus(monadStateT2).empty;
  const $4 = dictMonadP.Monad0().Applicative0().pure;
  return (prefs) => (arg) => searchParser1((opt) => nondetTBind(monadStateT2).bind(opt.optMain.tag === "ArgReader" ? cut2 : $0.pure())(() => {
    if (opt.optMain.tag === "CmdReader") {
      const $5 = opt.optMain._3(arg);
      if ($5.tag === "Just") {
        if (prefs.prefBacktrack === "NoBacktrack") {
          const $6 = $5._1;
          return lift2(bindStateT2.bind($1.apply($1.Functor0().map($$const)($$get2))(monadStateStateT2.state((v) => $Tuple(void 0, Nil))))((args) => {
            const $7 = Apply0.apply(Apply0.Functor0().map($$const)(Apply0.apply(Apply0.Functor0().map((v) => identity)(dictMonadP.enterContext(arg)($6)))(runParserInfo(dictMonadP)($6)(args))))(exitContext);
            return (s) => $2.map((v1) => $Tuple($Parser("NilP", v1._1), v1._2))(Monad0.Bind1().bind($7)((x) => Monad0.Applicative0().pure($Tuple(
              x,
              s
            ))));
          }));
        }
        if (prefs.prefBacktrack === "Backtrack") {
          const $6 = $5._1;
          return $3.map(NilP)(lift2((args) => Apply0.apply(Apply0.Functor0().map($$const)(Apply0.apply(Apply0.Functor0().map((v) => identity)(dictMonadP.enterContext(arg)($6)))(runParser(dictMonadP)($6.infoPolicy)(CmdStart)($6.infoParser)(args))))(exitContext)));
        }
        if (prefs.prefBacktrack === "SubparserInline") {
          const $6 = $5._1;
          return lift2(bindStateT2.bind((() => {
            const $7 = dictMonadP.enterContext(arg)($6);
            return (s) => Monad0.Bind1().bind($7)((x) => Monad0.Applicative0().pure($Tuple(x, s)));
          })())(() => applicativeStateT(Monad0).pure($6.infoParser)));
        }
        fail();
      }
      if ($5.tag === "Nothing") {
        return empty3;
      }
      fail();
    }
    if (opt.optMain.tag === "ArgReader") {
      const $5 = opt.optMain._1.crReader(arg);
      const $6 = (() => {
        if ($5.tag === "Left") {
          return dictMonadP.errorP($5._1);
        }
        if ($5.tag === "Right") {
          return $4($5._1);
        }
        fail();
      })();
      return $3.map(NilP)(lift2((s) => Monad0.Bind1().bind($6)((x) => Monad0.Applicative0().pure($Tuple(x, s)))));
    }
    return empty3;
  }));
};
var runParserInfo = (dictMonadP) => (i) => runParserFully(dictMonadP)(i.infoPolicy)(i.infoParser);
var runParserFully = (dictMonadP) => {
  const Monad0 = dictMonadP.Monad0();
  return (policy) => (p) => (args) => Monad0.Bind1().bind(runParser(dictMonadP)(policy)(CmdStart)(p)(args))((v) => {
    if (v._2.tag === "Nil") {
      return Monad0.Applicative0().pure(v._1);
    }
    if (v._2.tag === "Cons") {
      return dictMonadP.errorP($ParseError2(
        "UnexpectedError",
        v._2._1,
        $SomeParser($Parser("NilP", void 0))
      ));
    }
    fail();
  });
};
var runParser = (dictMonadP) => {
  const Monad0 = dictMonadP.Monad0();
  const disamb2 = disamb({
    Applicative0: () => applicativeStateT(Monad0),
    Bind1: () => bindStateT(Monad0)
  });
  const $0 = Monad0.Bind1();
  const getPrefs = dictMonadP.getPrefs;
  const pure4 = dictMonadP.Monad0().Applicative0().pure;
  return (policy) => (isCmdStart) => (p) => (args) => {
    const result = applyMaybe.apply((() => {
      const $1 = evalParser(p);
      if ($1.tag === "Just") {
        return $Maybe("Just", Tuple($1._1));
      }
      return Nothing;
    })())($Maybe("Just", args));
    if (args.tag === "Nil") {
      return dictMonadP.exitP(isCmdStart)(policy)(p)(result);
    }
    if (args.tag === "Cons") {
      if (args._1 === "--" && (policy === "Intersperse" || policy === "NoIntersperse" || policy !== "AllPositionals")) {
        return runParser(dictMonadP)(AllPositionals)(CmdCont)(p)(args._2);
      }
      const $1 = args._1;
      const $2 = args._2;
      return $0.bind(getPrefs)((prefs) => $0.bind(disamb2(!prefs.prefDisambiguate)(stepParser(dictMonadP)(prefs)(policy)($1)(p))($2))((v) => {
        if (v._1.tag === "Nothing") {
          const $3 = dictMonadP.errorP($ParseError2("UnexpectedError", $1, $SomeParser(p)));
          if (result.tag === "Nothing") {
            return $3;
          }
          if (result.tag === "Just") {
            return pure4(result._1);
          }
          fail();
        }
        if (v._1.tag === "Just") {
          return runParser(dictMonadP)((() => {
            if (policy === "NoIntersperse") {
              if ((() => {
                const $3 = parseWord($1);
                if ($3.tag === "Nothing") {
                  return false;
                }
                if ($3.tag === "Just") {
                  return true;
                }
                fail();
              })()) {
                return NoIntersperse;
              }
              return AllPositionals;
            }
            return policy;
          })())(CmdCont)(v._1._1)(v._2);
        }
        fail();
      }));
    }
    fail();
  };
};
var treeMapParser = (g) => {
  const hasArg = (v) => {
    if (v.tag === "NilP") {
      return false;
    }
    if (v.tag === "OptP") {
      return v._1.optMain.tag === "ArgReader";
    }
    if (v.tag === "MultP") {
      return hasArg(v._1._1) || hasArg(v._1._2);
    }
    if (v.tag === "AltP") {
      return hasArg(v._1) || hasArg(v._2);
    }
    if (v.tag === "BindP") {
      return resume$p((p) => (v1) => hasArg(p))((v$1) => false)(v._1);
    }
    fail();
  };
  const go = (v) => (v1) => (v2) => (v3) => (v4) => {
    if (v4.tag === "NilP") {
      return $OptTree("MultNode", []);
    }
    if (v4.tag === "OptP") {
      if (optVisibilityOrd.compare(v4._1.optProps.propVisibility)(Internal) === "GT") {
        return $OptTree("Leaf", v3({ hinfoMulti: v, hinfoDefault: v1, hinfoUnreachableArgs: v2 })(v4._1));
      }
      return $OptTree("MultNode", []);
    }
    if (v4.tag === "MultP") {
      return $OptTree("MultNode", [go(v)(v1)(v2)(v3)(v4._1._1), go(v)(v1)(v2 || hasArg(v4._1._1))(v3)(v4._1._2)]);
    }
    if (v4.tag === "AltP") {
      const $02 = evalParser(v4._1);
      const d$p = v1 || (() => {
        const $1 = evalParser(v4._2);
        return (() => {
          if ($02.tag === "Nothing") {
            return false;
          }
          if ($02.tag === "Just") {
            return true;
          }
          fail();
        })() || (() => {
          if ($1.tag === "Nothing") {
            return false;
          }
          if ($1.tag === "Just") {
            return true;
          }
          fail();
        })();
      })();
      return $OptTree("AltNode", [go(v)(d$p)(v2)(v3)(v4._1), go(v)(d$p)(v2)(v3)(v4._2)]);
    }
    if (v4.tag === "BindP") {
      return resume$p((p) => (k) => {
        const go$p = go(true)(v1)(v2)(v3)(p);
        const v5 = evalParser(p);
        if (v5.tag === "Nothing") {
          return go$p;
        }
        if (v5.tag === "Just") {
          return $OptTree("MultNode", [go$p, go(true)(v1)(v2)(v3)($Parser("BindP", k(v5._1)))]);
        }
        fail();
      })((v$1) => $OptTree("MultNode", []))(v4._1);
    }
    fail();
  };
  const $0 = go(false)(false)(false)(g);
  return (x) => simplify($0(x));
};
var mapParser = (f) => {
  const flatten2 = (v) => {
    if (v.tag === "Leaf") {
      return [v._1];
    }
    if (v.tag === "MultNode") {
      return arrayBind(v._1)(flatten2);
    }
    if (v.tag === "AltNode") {
      return arrayBind(v._1)(flatten2);
    }
    fail();
  };
  const $0 = treeMapParser(f);
  return (x) => flatten2($0(x));
};

// output-es/Options.Applicative.BashCompletion/index.js
var $Richness = (tag, _1, _2) => ({ tag, _1, _2 });
var fromFoldable21 = /* @__PURE__ */ foldrArray(Cons)(Nil);
var identity29 = (x) => x;
var fold4 = /* @__PURE__ */ (() => foldableArray.foldMap(monoidArray)(identity3))();
var sequence = /* @__PURE__ */ (() => traversableArray.traverse(applicativeEffect)(identity4))();
var unLines = (xs) => foldlArray((v) => (v1) => {
  if (v.init) {
    return { init: false, acc: v1 };
  }
  return { init: false, acc: v.acc + "\n" + v1 };
})({ init: true, acc: "" })(xs).acc;
var fromFoldable110 = /* @__PURE__ */ (() => fromFoldableImpl(foldableList.foldr))();
var Standard = /* @__PURE__ */ $Richness("Standard");
var Enriched = (value0) => (value1) => $Richness("Enriched", value0, value1);
var zshCompletionScript = (prog) => (progn) => {
  const $0 = [
    "#compdef " + progn,
    "",
    "local request",
    "local completions",
    "local word",
    "local index=$((CURRENT - 1))",
    "",
    "request=(--bash-completion-enriched --bash-completion-index $index)",
    "for arg in ${words[@]}; do",
    "  request=(${request[@]} --bash-completion-word $arg)",
    "done",
    "",
    "IFS=$'\\n' completions=($( " + prog + ' "${request[@]}" ))',
    "",
    "for word in $completions; do",
    "  local -a parts",
    "",
    "  # Split the line at a tab if there is one.",
    "  IFS=$'\\t' parts=($( echo $word ))",
    "",
    "  if [[ -n $parts[2] ]]; then",
    '     if [[ $word[1] == "-" ]]; then',
    '       local desc=("$parts[1] ($parts[2])")',
    "       compadd -d desc -- $parts[1]",
    "     else",
    '       local desc=($(print -f  "%-019s -- %s" $parts[1] $parts[2]))',
    "       compadd -l -d desc -- $parts[1]",
    "     fi",
    "  else",
    "    compadd -f -- $word",
    "  fi",
    "done"
  ];
  return () => $0;
};
var fishCompletionScript = (prog) => (progn) => {
  const $0 = [
    " function _" + progn,
    "    set -l cl (commandline --tokenize --current-process)",
    "    # Hack around fish issue #3934",
    "    set -l cn (commandline --tokenize --cut-at-cursor --current-process)",
    "    set -l cn (count $cn)",
    "    set -l tmpline --bash-completion-enriched --bash-completion-index $cn",
    "    for arg in $cl",
    "      set tmpline $tmpline --bash-completion-word $arg",
    "    end",
    "    for opt in (" + prog + " $tmpline)",
    "      if test -d $opt",
    '        echo -E "$opt/"',
    "      else",
    '        echo -E "$opt"',
    "      end",
    "    end",
    "end",
    "",
    "complete --no-files --command " + progn + " --arguments '(_" + progn + ")'"
  ];
  return () => $0;
};
var bashCompletionScript = (prog) => (progn) => {
  const $0 = [
    "_" + progn + "()",
    "{",
    "    local CMDLINE",
    "    local IFS=$'\\n'",
    "    CMDLINE=(--bash-completion-index $COMP_CWORD)",
    "",
    "    for arg in ${COMP_WORDS[@]}; do",
    "        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)",
    "    done",
    "",
    "    COMPREPLY=( $(" + prog + ' "${CMDLINE[@]}") )',
    "}",
    "",
    "complete -o filenames -F _" + progn + " " + progn
  ];
  return () => $0;
};
var arraySplitAt = (idx) => (arr) => {
  if (idx === 0) {
    return { init: [], rest: arr };
  }
  return { init: slice(0)(idx)(arr), rest: slice(idx)(arr.length)(arr) };
};
var bashCompletionQuery = (pinfo) => (pprefs) => (richness) => (ws) => (i) => (v) => {
  const v1 = arraySplitAt(i)(ws);
  const filter_names = filter((() => {
    const v22 = index(v1.rest)(0);
    if (v22.tag === "Just") {
      return startsWith(v22._1);
    }
    if (v22.tag === "Nothing") {
      return (v$1) => true;
    }
    fail();
  })());
  const $0 = arrayMap(showOption);
  const add_opt_help1 = (opt) => {
    if (richness.tag === "Standard") {
      return identity29;
    }
    if (richness.tag === "Enriched") {
      const $1 = richness._1;
      return arrayMap((o) => {
        if (opt.optProps.propHelp.tag === "Nothing") {
          return o;
        }
        if (opt.optProps.propHelp.tag === "Just") {
          const $2 = displayS(renderFits(fits1)(1)($1)(opt.optProps.propHelp._1));
          const $3 = $2 === "" ? [] : split("\n")($2);
          return o + "	" + (() => {
            if ($3.length > 0) {
              const $4 = uncons($3);
              const $5 = (() => {
                if ($4.tag === "Just") {
                  return $4._1;
                }
                fail();
              })();
              if ($5.tail.length === 0) {
                return $5.head;
              }
              return $5.head + "...";
            }
            return "";
          })();
        }
        fail();
      });
    }
    fail();
  };
  const v2 = runCompletion(runParserFully(completionMonadP)(pinfo.infoPolicy)(pinfo.infoParser)(fromFoldable21(slice(1)(v1.init.length)(v1.init))))(pprefs);
  if (v2.tag === "Just") {
    if (v2._1.tag === "Left") {
      const $1 = v2._1._1._2;
      const $2 = sequence(mapParser((hinfo) => (opt) => {
        if (opt.optMain.tag === "OptReader") {
          if ($1 === "Intersperse" || $1 === "NoIntersperse" || $1 !== "AllPositionals") {
            const $22 = add_opt_help1(opt)(filter_names($0(opt.optMain._1)));
            return () => $22;
          }
          return () => [];
        }
        if (opt.optMain.tag === "FlagReader") {
          if ($1 === "Intersperse" || $1 === "NoIntersperse" || $1 !== "AllPositionals") {
            const $22 = add_opt_help1(opt)(filter_names($0(opt.optMain._1)));
            return () => $22;
          }
          return () => [];
        }
        if (opt.optMain.tag === "ArgReader") {
          if (hinfo.hinfoUnreachableArgs) {
            return () => [];
          }
          return opt.optMain._1.crCompleter((() => {
            const $22 = index(v1.rest)(0);
            if ($22.tag === "Nothing") {
              return "";
            }
            if ($22.tag === "Just") {
              return $22._1;
            }
            fail();
          })());
        }
        if (opt.optMain.tag === "CmdReader") {
          if (hinfo.hinfoUnreachableArgs) {
            return () => [];
          }
          const $22 = (() => {
            if (richness.tag === "Standard") {
              return identity29;
            }
            if (richness.tag === "Enriched") {
              const $23 = richness._2;
              return arrayMap((cmd) => {
                const $3 = opt.optMain._3(cmd);
                const $4 = (() => {
                  if ($3.tag === "Just") {
                    return $3._1.infoProgDesc;
                  }
                  if ($3.tag === "Nothing") {
                    return Nothing;
                  }
                  fail();
                })();
                if ($4.tag === "Nothing") {
                  return cmd;
                }
                if ($4.tag === "Just") {
                  const $5 = displayS(renderFits(fits1)(1)($23)($4._1));
                  const $6 = $5 === "" ? [] : split("\n")($5);
                  return cmd + "	" + (() => {
                    if ($6.length > 0) {
                      const $7 = uncons($6);
                      const $8 = (() => {
                        if ($7.tag === "Just") {
                          return $7._1;
                        }
                        fail();
                      })();
                      if ($8.tail.length === 0) {
                        return $8.head;
                      }
                      return $8.head + "...";
                    }
                    return "";
                  })();
                }
                fail();
              });
            }
            fail();
          })()(filter_names(opt.optMain._2));
          return () => $22;
        }
        fail();
      })(v2._1._1._1._1));
      return () => {
        const a$p = $2();
        return fold4(a$p);
      };
    }
    if (v2._1.tag === "Right") {
      return v2._1._1((() => {
        const $1 = index(v1.rest)(0);
        if ($1.tag === "Nothing") {
          return "";
        }
        if ($1.tag === "Just") {
          return $1._1;
        }
        fail();
      })());
    }
    fail();
  }
  if (v2.tag === "Nothing") {
    return () => [];
  }
  fail();
};
var bashCompletionParser = (pinfo) => (pprefs) => $Parser(
  "AltP",
  parserFunctor.map((opts) => ({
    execCompletion: (progn) => {
      const $0 = opts(progn);
      return () => {
        const a$p = $0();
        return unLines(a$p);
      };
    }
  }))($Parser(
    "MultP",
    $MultPE(
      $Parser(
        "MultP",
        $MultPE(
          parserFunctor.map(bashCompletionQuery(pinfo)(pprefs))($Parser(
            "AltP",
            $Parser(
              "MultP",
              $MultPE(
                $Parser(
                  "MultP",
                  $MultPE(
                    flag$p(Enriched)($Mod(
                      (x) => internal._1({
                        flagNames: [$OptName("OptLong", "bash-completion-enriched"), ...x.flagNames],
                        flagActive: x.flagActive
                      }),
                      $DefaultProp(
                        internal._2._1.tag === "Nothing" ? Nothing : internal._2._1,
                        internal._2._2.tag === "Nothing" ? Nothing : internal._2._2
                      ),
                      (x) => internal._3(x)
                    )),
                    option($$int2)($Mod(
                      (x) => internal._1({
                        optNames: [$OptName("OptLong", "bash-completion-option-desc-length"), ...x.optNames],
                        optCompleter: x.optCompleter,
                        optNoArgError: x.optNoArgError
                      }),
                      $DefaultProp(
                        $Maybe("Just", 40),
                        internal._2._2.tag === "Nothing" ? Nothing : internal._2._2
                      ),
                      (x) => internal._3(x)
                    ))
                  )
                ),
                option($$int2)($Mod(
                  (x) => internal._1({
                    optNames: [$OptName("OptLong", "bash-completion-command-desc-length"), ...x.optNames],
                    optCompleter: x.optCompleter,
                    optNoArgError: x.optNoArgError
                  }),
                  $DefaultProp(
                    $Maybe("Just", 40),
                    internal._2._2.tag === "Nothing" ? Nothing : internal._2._2
                  ),
                  (x) => internal._3(x)
                ))
              )
            ),
            $Parser("NilP", Standard)
          )),
          parserFunctor.map(fromFoldable110)($Parser(
            "BindP",
            manyM(option(readerAsk)($Mod(
              (x) => internal._1({
                optNames: [$OptName("OptLong", "bash-completion-word"), ...x.optNames],
                optCompleter: x.optCompleter,
                optNoArgError: x.optNoArgError
              }),
              $DefaultProp(
                internal._2._1.tag === "Nothing" ? Nothing : internal._2._1,
                internal._2._2.tag === "Nothing" ? Nothing : internal._2._2
              ),
              (x) => internal._3(x)
            )))
          ))
        )
      ),
      option($$int2)($Mod(
        (x) => internal._1({
          optNames: [$OptName("OptLong", "bash-completion-index"), ...x.optNames],
          optCompleter: x.optCompleter,
          optNoArgError: x.optNoArgError
        }),
        $DefaultProp(
          internal._2._1.tag === "Nothing" ? Nothing : internal._2._1,
          internal._2._2.tag === "Nothing" ? Nothing : internal._2._2
        ),
        (x) => internal._3(x)
      ))
    )
  )),
  $Parser(
    "AltP",
    parserFunctor.map((opts) => ({
      execCompletion: (progn) => {
        const $0 = opts(progn);
        return () => {
          const a$p = $0();
          return unLines(a$p);
        };
      }
    }))(parserFunctor.map(bashCompletionScript)(option(readerAsk)($Mod(
      (x) => internal._1({
        optNames: [$OptName("OptLong", "bash-completion-script"), ...x.optNames],
        optCompleter: x.optCompleter,
        optNoArgError: x.optNoArgError
      }),
      $DefaultProp(
        internal._2._1.tag === "Nothing" ? Nothing : internal._2._1,
        internal._2._2.tag === "Nothing" ? Nothing : internal._2._2
      ),
      (x) => internal._3(x)
    )))),
    $Parser(
      "AltP",
      parserFunctor.map((opts) => ({
        execCompletion: (progn) => {
          const $0 = opts(progn);
          return () => {
            const a$p = $0();
            return unLines(a$p);
          };
        }
      }))(parserFunctor.map(fishCompletionScript)(option(readerAsk)($Mod(
        (x) => internal._1({
          optNames: [$OptName("OptLong", "fish-completion-script"), ...x.optNames],
          optCompleter: x.optCompleter,
          optNoArgError: x.optNoArgError
        }),
        $DefaultProp(
          internal._2._1.tag === "Nothing" ? Nothing : internal._2._1,
          internal._2._2.tag === "Nothing" ? Nothing : internal._2._2
        ),
        (x) => internal._3(x)
      )))),
      parserFunctor.map((opts) => ({
        execCompletion: (progn) => {
          const $0 = opts(progn);
          return () => {
            const a$p = $0();
            return unLines(a$p);
          };
        }
      }))(parserFunctor.map(zshCompletionScript)(option(readerAsk)($Mod(
        (x) => internal._1({
          optNames: [$OptName("OptLong", "zsh-completion-script"), ...x.optNames],
          optCompleter: x.optCompleter,
          optNoArgError: x.optNoArgError
        }),
        $DefaultProp(
          internal._2._1.tag === "Nothing" ? Nothing : internal._2._1,
          internal._2._2.tag === "Nothing" ? Nothing : internal._2._2
        ),
        (x) => internal._3(x)
      ))))
    )
  )
);

// output-es/Options.Applicative.Help.Types/index.js
var chunkMonoid2 = /* @__PURE__ */ chunkMonoid(docSemigroup);
var parserHelpMonoid = /* @__PURE__ */ monoidRecord()(/* @__PURE__ */ (() => {
  const Semigroup0 = chunkMonoid2.Semigroup0();
  const Semigroup0$1 = chunkMonoid2.Semigroup0();
  const Semigroup0$2 = chunkMonoid2.Semigroup0();
  const Semigroup0$3 = chunkMonoid2.Semigroup0();
  const Semigroup0$4 = chunkMonoid2.Semigroup0();
  const Semigroup0$5 = chunkMonoid2.Semigroup0();
  const semigroupRecordCons1 = {
    appendRecord: (v) => (ra) => (rb) => ({
      helpBody: Semigroup0.append(ra.helpBody)(rb.helpBody),
      helpError: Semigroup0$1.append(ra.helpError)(rb.helpError),
      helpFooter: Semigroup0$2.append(ra.helpFooter)(rb.helpFooter),
      helpHeader: Semigroup0$3.append(ra.helpHeader)(rb.helpHeader),
      helpSuggestions: Semigroup0$4.append(ra.helpSuggestions)(rb.helpSuggestions),
      helpUsage: Semigroup0$5.append(ra.helpUsage)(rb.helpUsage)
    })
  };
  return {
    memptyRecord: (v) => ({
      helpBody: chunkMonoid2.mempty,
      helpError: chunkMonoid2.mempty,
      helpFooter: chunkMonoid2.mempty,
      helpHeader: chunkMonoid2.mempty,
      helpSuggestions: chunkMonoid2.mempty,
      helpUsage: chunkMonoid2.mempty
    }),
    SemigroupRecord0: () => semigroupRecordCons1
  };
})());
var helpText = (v) => {
  const $0 = vsepChunks([v.helpError, v.helpSuggestions, v.helpHeader, v.helpUsage, v.helpBody, v.helpFooter]);
  if ($0.tag === "Nothing") {
    return Empty;
  }
  if ($0.tag === "Just") {
    return $0._1;
  }
  fail();
};

// output-es/Options.Applicative.Help.Core/index.js
var fold5 = /* @__PURE__ */ (() => foldableArray.foldMap(monoidArray)(identity3))();
var chunkMonoid3 = /* @__PURE__ */ chunkMonoid(docSemigroup);
var listToChunk2 = /* @__PURE__ */ listToChunk(docMonoid);
var identity30 = (x) => x;
var mempty2 = /* @__PURE__ */ (() => $Tuple(monoidMaybe(semigroupString).mempty, chunkMonoid3.mempty))();
var usageHelp = (chunk) => ({
  helpUsage: chunk,
  helpBody: parserHelpMonoid.mempty.helpBody,
  helpError: parserHelpMonoid.mempty.helpError,
  helpFooter: parserHelpMonoid.mempty.helpFooter,
  helpHeader: parserHelpMonoid.mempty.helpHeader,
  helpSuggestions: parserHelpMonoid.mempty.helpSuggestions
});
var suggestionsHelp = (chunk) => ({
  helpSuggestions: chunk,
  helpBody: parserHelpMonoid.mempty.helpBody,
  helpError: parserHelpMonoid.mempty.helpError,
  helpFooter: parserHelpMonoid.mempty.helpFooter,
  helpHeader: parserHelpMonoid.mempty.helpHeader,
  helpUsage: parserHelpMonoid.mempty.helpUsage
});
var intersperse2 = (sep) => {
  const $0 = mapWithIndex((idx) => (e) => {
    if (idx === 0) {
      return [e];
    }
    return [sep, e];
  });
  return (x) => fold5($0(x));
};
var optDesc = (pprefs) => (style) => (info2) => (opt) => {
  const suffix = info2.hinfoMulti ? stringChunk(pprefs.prefMultiSuffix) : chunkMonoid3.mempty;
  const descs = arrayMap((x) => string3(showOption(x)))(sortBy(optNameOrd.compare)((() => {
    if (opt.optMain.tag === "OptReader") {
      return opt.optMain._1;
    }
    if (opt.optMain.tag === "FlagReader") {
      return opt.optMain._1;
    }
    return [];
  })()));
  return (() => {
    if (opt.optProps.propDescMod.tag === "Nothing") {
      return identity30;
    }
    if (opt.optProps.propDescMod.tag === "Just") {
      return functorMaybe.map(opt.optProps.propDescMod._1);
    }
    fail();
  })()((() => {
    const $0 = listToChunk2(intersperse2(style.descSep)(descs));
    const $1 = stringChunk(opt.optProps.propMetaVar);
    const $2 = (() => {
      if ($0.tag === "Nothing") {
        return $1;
      }
      if ($1.tag === "Nothing") {
        return $0;
      }
      if ($0.tag === "Just" && $1.tag === "Just") {
        return $Maybe("Just", $Doc("Cat", $0._1, $Doc("Cat", $Doc("Char", " "), $1._1)));
      }
      fail();
    })();
    if ((() => {
      if (info2.hinfoDefault && !style.descOptional) {
        return true;
      }
      if (opt.optProps.propVisibility === "Hidden") {
        return !style.descHidden;
      }
      return opt.optProps.propVisibility !== "Visible";
    })()) {
      return chunkMonoid3.mempty;
    }
    if ((() => {
      if ($2.tag === "Nothing") {
        return true;
      }
      if ($2.tag === "Just") {
        return false;
      }
      fail();
    })() || !style.descSurround) {
      if ($2.tag === "Nothing") {
        return suffix;
      }
      if (suffix.tag === "Nothing") {
        return $2;
      }
      if ($2.tag === "Just" && suffix.tag === "Just") {
        return $Maybe("Just", $Doc("Cat", $2._1, suffix._1));
      }
      fail();
    }
    if (info2.hinfoDefault) {
      const $32 = $2.tag === "Just" ? $Maybe(
        "Just",
        $Doc(
          "Cat",
          $Doc("Char", "["),
          $Doc("Cat", $2._1, $Doc("Char", "]"))
        )
      ) : Nothing;
      if ($32.tag === "Nothing") {
        return suffix;
      }
      if (suffix.tag === "Nothing") {
        return $32;
      }
      if ($32.tag === "Just" && suffix.tag === "Just") {
        return $Maybe("Just", $Doc("Cat", $32._1, suffix._1));
      }
      fail();
    }
    if (slice(1)(descs.length)(descs).length === 0) {
      if ($2.tag === "Nothing") {
        return suffix;
      }
      if (suffix.tag === "Nothing") {
        return $2;
      }
      if ($2.tag === "Just" && suffix.tag === "Just") {
        return $Maybe("Just", $Doc("Cat", $2._1, suffix._1));
      }
      fail();
    }
    const $3 = $2.tag === "Just" ? $Maybe(
      "Just",
      $Doc(
        "Cat",
        $Doc("Char", "("),
        $Doc("Cat", $2._1, $Doc("Char", ")"))
      )
    ) : Nothing;
    if ($3.tag === "Nothing") {
      return suffix;
    }
    if (suffix.tag === "Nothing") {
      return $3;
    }
    if ($3.tag === "Just" && suffix.tag === "Just") {
      return $Maybe("Just", $Doc("Cat", $3._1, suffix._1));
    }
    fail();
  })());
};
var headerHelp = (chunk) => ({
  helpHeader: chunk,
  helpBody: parserHelpMonoid.mempty.helpBody,
  helpError: parserHelpMonoid.mempty.helpError,
  helpFooter: parserHelpMonoid.mempty.helpFooter,
  helpSuggestions: parserHelpMonoid.mempty.helpSuggestions,
  helpUsage: parserHelpMonoid.mempty.helpUsage
});
var fullDesc = (pprefs) => {
  const style = { descSep: string3(","), descHidden: true, descOptional: true, descSurround: false };
  const $0 = mapParser((info2) => (opt) => {
    const n = optDesc(pprefs)(style)(info2)(opt);
    if (opt.optProps.propShowDefault.tag === "Just") {
      if ((() => {
        if (n.tag === "Nothing") {
          return false;
        }
        if (n.tag === "Just") {
          return true;
        }
        fail();
      })() && (() => {
        if (opt.optProps.propHelp.tag === "Nothing") {
          return false;
        }
        if (opt.optProps.propHelp.tag === "Just") {
          return true;
        }
        fail();
      })()) {
        return $Maybe(
          "Just",
          $Tuple(
            (() => {
              if (n.tag === "Nothing") {
                return Empty;
              }
              if (n.tag === "Just") {
                return n._1;
              }
              fail();
            })(),
            (() => {
              if (opt.optProps.propHelp.tag === "Nothing") {
                const $02 = $Doc(
                  "Cat",
                  $Doc("Char", "("),
                  $Doc(
                    "Cat",
                    $Doc(
                      "Cat",
                      string3("default:"),
                      $Doc("Cat", $Doc("Char", " "), string3(opt.optProps.propShowDefault._1))
                    ),
                    $Doc("Char", ")")
                  )
                );
                return $Doc("Column", (k) => $Doc("Nesting", (i) => $Doc("Nest", k - i | 0, $02)));
              }
              if (opt.optProps.propHelp.tag === "Just") {
                const $02 = $Doc(
                  "Cat",
                  opt.optProps.propHelp._1,
                  $Doc(
                    "Cat",
                    $Doc("Char", " "),
                    $Doc(
                      "Cat",
                      $Doc("Char", "("),
                      $Doc(
                        "Cat",
                        $Doc(
                          "Cat",
                          string3("default:"),
                          $Doc("Cat", $Doc("Char", " "), string3(opt.optProps.propShowDefault._1))
                        ),
                        $Doc("Char", ")")
                      )
                    )
                  )
                );
                return $Doc("Column", (k) => $Doc("Nesting", (i) => $Doc("Nest", k - i | 0, $02)));
              }
              fail();
            })()
          )
        );
      }
      return Nothing;
    }
    if ((() => {
      if (n.tag === "Nothing") {
        return false;
      }
      if (n.tag === "Just") {
        return true;
      }
      fail();
    })() && (() => {
      if (opt.optProps.propHelp.tag === "Nothing") {
        return false;
      }
      if (opt.optProps.propHelp.tag === "Just") {
        return true;
      }
      fail();
    })()) {
      return $Maybe(
        "Just",
        $Tuple(
          (() => {
            if (n.tag === "Nothing") {
              return Empty;
            }
            if (n.tag === "Just") {
              return n._1;
            }
            fail();
          })(),
          (() => {
            if (opt.optProps.propHelp.tag === "Nothing") {
              return $Doc(
                "Column",
                (k) => $Doc("Nesting", (i) => $Doc("Nest", k - i | 0, Empty))
              );
            }
            const $02 = (() => {
              if (opt.optProps.propHelp.tag === "Nothing") {
                return Empty;
              }
              if (opt.optProps.propHelp.tag === "Just") {
                return opt.optProps.propHelp._1;
              }
              fail();
            })();
            return $Doc("Column", (k) => $Doc("Nesting", (i) => $Doc("Nest", k - i | 0, $02)));
          })()
        )
      );
    }
    return Nothing;
  });
  return (x) => tabulate$p(24)(mapMaybe((x$1) => x$1)($0(x)));
};
var footerHelp = (chunk) => ({
  helpFooter: chunk,
  helpBody: parserHelpMonoid.mempty.helpBody,
  helpError: parserHelpMonoid.mempty.helpError,
  helpHeader: parserHelpMonoid.mempty.helpHeader,
  helpSuggestions: parserHelpMonoid.mempty.helpSuggestions,
  helpUsage: parserHelpMonoid.mempty.helpUsage
});
var fold_tree = (v) => {
  if (v.tag === "Leaf") {
    return v._1;
  }
  if (v.tag === "MultNode") {
    return foldrArray((x) => chunkBesideOrBelow(fold_tree(x)))(chunkMonoid3.mempty)(v._1);
  }
  if (v.tag === "AltNode") {
    const $0 = filter((x) => {
      if (x.tag === "Nothing") {
        return false;
      }
      if (x.tag === "Just") {
        return true;
      }
      fail();
    })(arrayMap(fold_tree)(v._1));
    if ($0.length === 1) {
      return $0[0];
    }
    const $1 = foldrArray((v1) => (v2) => {
      if (v1.tag === "Nothing") {
        return v2;
      }
      if (v2.tag === "Nothing") {
        return v1;
      }
      if (v1.tag === "Just" && v2.tag === "Just") {
        return $Maybe(
          "Just",
          $Doc(
            "Cat",
            v1._1,
            $Doc(
              "Cat",
              softline,
              $Doc("Cat", $Doc("Char", "|"), $Doc("Cat", softline, v2._1))
            )
          )
        );
      }
      fail();
    })(chunkMonoid3.mempty)($0);
    if ($1.tag === "Just") {
      return $Maybe(
        "Just",
        $Doc(
          "Cat",
          $Doc("Char", "("),
          $Doc("Cat", $1._1, $Doc("Char", ")"))
        )
      );
    }
    return Nothing;
  }
  fail();
};
var errorHelp = (chunk) => ({
  helpError: chunk,
  helpBody: parserHelpMonoid.mempty.helpBody,
  helpFooter: parserHelpMonoid.mempty.helpFooter,
  helpHeader: parserHelpMonoid.mempty.helpHeader,
  helpSuggestions: parserHelpMonoid.mempty.helpSuggestions,
  helpUsage: parserHelpMonoid.mempty.helpUsage
});
var cmdDesc = /* @__PURE__ */ mapParser((v) => (opt) => {
  if (opt.optMain.tag === "CmdReader") {
    return $Tuple(
      opt.optMain._1,
      tabulate$p(24)(arrayBind(reverse(opt.optMain._2))((cmd) => arrayBind((() => {
        const $0 = opt.optMain._3(cmd);
        if ($0.tag === "Just") {
          return [$0._1.infoProgDesc];
        }
        return [];
      })())((d) => [
        $Tuple(
          string3(cmd),
          (() => {
            const $0 = (() => {
              if (d.tag === "Nothing") {
                return Empty;
              }
              if (d.tag === "Just") {
                return d._1;
              }
              fail();
            })();
            return $Doc("Column", (k) => $Doc("Nesting", (i) => $Doc("Nest", k - i | 0, $0)));
          })()
        )
      ])))
    );
  }
  return mempty2;
});
var briefDesc$p = (showOptional) => (pprefs) => {
  const $0 = treeMapParser(optDesc(pprefs)({
    descSep: string3("|"),
    descHidden: false,
    descOptional: showOptional,
    descSurround: true
  }));
  return (x) => fold_tree($0(x));
};
var parserUsage = (pprefs) => (p) => (progn) => hsep([
  string3("Usage:"),
  string3(progn),
  (() => {
    const $0 = briefDesc$p(true)(pprefs)(p);
    const $1 = (() => {
      if ($0.tag === "Nothing") {
        return Empty;
      }
      if ($0.tag === "Just") {
        return $0._1;
      }
      fail();
    })();
    return $Doc("Column", (k) => $Doc("Nesting", (i) => $Doc("Nest", k - i | 0, $1)));
  })()
]);
var bodyHelp = (chunk) => ({
  helpBody: chunk,
  helpError: parserHelpMonoid.mempty.helpError,
  helpFooter: parserHelpMonoid.mempty.helpFooter,
  helpHeader: parserHelpMonoid.mempty.helpHeader,
  helpSuggestions: parserHelpMonoid.mempty.helpSuggestions,
  helpUsage: parserHelpMonoid.mempty.helpUsage
});
var parserHelp = (pprefs) => (p) => bodyHelp(vsepChunks([
  (() => {
    const $0 = fullDesc(pprefs)(p);
    if ($0.tag === "Just") {
      return $Maybe(
        "Just",
        $Doc(
          "Cat",
          string3("Available options:"),
          $Doc("Cat", $Doc("FlatAlt", Line, $Doc("Char", " ")), $0._1)
        )
      );
    }
    return Nothing;
  })(),
  ...arrayMap((arr) => {
    const $0 = uncons(arr);
    const v = (() => {
      if ($0.tag === "Just") {
        return $0._1;
      }
      fail();
    })();
    const $1 = (() => {
      if (v.head._1.tag === "Nothing") {
        return "Available commands:";
      }
      if (v.head._1.tag === "Just") {
        return v.head._1._1;
      }
      fail();
    })();
    const $2 = vcatChunks([v.head._2, ...arrayMap(snd)(v.tail)]);
    if ($2.tag === "Just") {
      return $Maybe(
        "Just",
        $Doc(
          "Cat",
          string3($1),
          $Doc("Cat", $Doc("FlatAlt", Line, $Doc("Char", " ")), $2._1)
        )
      );
    }
    return Nothing;
  })(groupBy((x) => (y) => {
    if (x._1.tag === "Nothing") {
      return y._1.tag === "Nothing";
    }
    return x._1.tag === "Just" && y._1.tag === "Just" && x._1._1 === y._1._1;
  })(cmdDesc(p)))
]));

// output-es/Data.Function.Memoize/index.js
var $NatTrie = (_1, _2, _3) => ({ tag: "NatTrie", _1, _2, _3 });
var tabulateNat = {
  tabulate: (f) => {
    const walk = (v) => (v1) => {
      if (v.tag === "Nil") {
        return v1._1;
      }
      if (v.tag === "Cons") {
        if (!v._1) {
          const $0 = v1._2;
          const $1 = walk(v._2);
          return defer((v$1) => force($1(force($0))));
        }
        if (v._1) {
          const $0 = v1._3;
          const $1 = walk(v._2);
          return defer((v$1) => force($1(force($0))));
        }
      }
      fail();
    };
    const build = (n) => $NatTrie(defer((v) => f(n)), defer((v) => build(n * 2 | 0)), defer((v) => build((n * 2 | 0) + 1 | 0)));
    const trie = build(0);
    const bits$p = (bits$p$a0$copy) => (bits$p$a1$copy) => {
      let bits$p$a0 = bits$p$a0$copy, bits$p$a1 = bits$p$a1$copy, bits$p$c = true, bits$p$r;
      while (bits$p$c) {
        const v = bits$p$a0, v1 = bits$p$a1;
        if (v1 === 0) {
          bits$p$c = false;
          bits$p$r = v;
          continue;
        }
        bits$p$a0 = $List("Cons", (v1 & 1) !== 0, v);
        bits$p$a1 = v1 >>> 1;
      }
      return bits$p$r;
    };
    const bits = bits$p(Nil);
    return (n) => walk(bits(n))(trie);
  }
};
var tabulateTuple = (dictTabulate) => (dictTabulate1) => ({
  tabulate: (f) => {
    const f$p = dictTabulate.tabulate((a) => dictTabulate1.tabulate((b) => f($Tuple(a, b))));
    return (v) => {
      const $0 = v._2;
      const $1 = f$p(v._1);
      return defer((v$1) => force(force($1)($0)));
    };
  }
});
var memoize = (dictTabulate) => (f) => {
  const $0 = dictTabulate.tabulate(f);
  return (x) => force($0(x));
};
var memoize2 = (dictTabulate) => (dictTabulate1) => {
  const memoize1 = memoize(tabulateTuple(dictTabulate)(dictTabulate1));
  return (f) => {
    const $0 = memoize1((v) => f(v._1)(v._2));
    return (a) => (b) => $0($Tuple(a, b));
  };
};

// output-es/Options.Applicative.Help.Levenshtein/index.js
var memoize22 = /* @__PURE__ */ memoize2(tabulateNat)(tabulateNat);
var minimum2 = /* @__PURE__ */ minimum(ordInt)(/* @__PURE__ */ foldable1NonEmpty(foldableArray));
var editDistance = (dictEq) => (xs) => (ys) => {
  const dist = (v) => (v1) => {
    if (v === 0) {
      return v1;
    }
    if (v1 === 0) {
      return v;
    }
    return minimum2($NonEmpty(
      dist$p$lazy()(v - 1 | 0)(v1) + 1 | 0,
      [dist$p$lazy()(v)(v1 - 1 | 0) + 1 | 0, dictEq.eq(xs[v - 1 | 0])(ys[v1 - 1 | 0]) ? dist$p$lazy()(v - 1 | 0)(v1 - 1 | 0) : 1 + dist$p$lazy()(v - 1 | 0)(v1 - 1 | 0) | 0]
    ));
  };
  const dist$p$lazy = binding(() => memoize22((a) => (b) => dist(a)(b)));
  const dist$p = dist$p$lazy();
  return dist$p(xs.length)(ys.length);
};

// output-es/Options.Applicative.Extra/index.js
var unWords = (xs) => foldlArray((v) => (v1) => {
  if (v.init) {
    return { init: false, acc: v1 };
  }
  return { init: false, acc: v.acc + " " + v1 };
})({ init: true, acc: "" })(xs).acc;
var fold6 = /* @__PURE__ */ (() => foldableArray.foldMap(monoidArray)(identity3))();
var mempty13 = /* @__PURE__ */ (() => chunkMonoid(docSemigroup).mempty)();
var fold1 = /* @__PURE__ */ (() => foldableArray.foldMap(parserHelpMonoid)(identity3))();
var mempty22 = (v) => () => {
};
var fromFoldable24 = /* @__PURE__ */ foldrArray(Cons)(Nil);
var renderFailure = (failure) => (progn) => {
  const v = failure(progn);
  return $Tuple(
    displayS(renderFits(fits1)(1)(v._2._2._1)(helpText(v._1))),
    v._2._1
  );
};
var parserFailure = (pprefs) => (pinfo) => (msg) => (ctx) => {
  const suggestion_help = suggestionsHelp((() => {
    if (msg.tag === "UnexpectedError") {
      const $0 = msg._1;
      const good = filter((a) => editDistance(eqChar)(toCharArray(a))(toCharArray($0)) < 3)(fold6(mapParser((v) => (v1) => {
        if (v1.optMain.tag === "OptReader") {
          return arrayMap(showOption)(v1.optMain._1);
        }
        if (v1.optMain.tag === "FlagReader") {
          return arrayMap(showOption)(v1.optMain._1);
        }
        if (v1.optMain.tag === "ArgReader") {
          return [];
        }
        if (v1.optMain.tag === "CmdReader") {
          if (v.hinfoUnreachableArgs) {
            return [];
          }
          return v1.optMain._2;
        }
        fail();
      })(msg._2._1)));
      return applyMaybe.apply((() => {
        const $1 = good.length < 2 ? stringChunk("Did you mean this?") : stringChunk("Did you mean one of these?");
        if ($1.tag === "Just") {
          return $Maybe("Just", appendWithLine($1._1));
        }
        return Nothing;
      })())((() => {
        const $1 = vcatChunks(arrayMap(stringChunk)(good));
        if ($1.tag === "Just") {
          return $Maybe("Just", indent(4)($1._1));
        }
        return Nothing;
      })());
    }
    return mempty13;
  })());
  const show_full_help = (() => {
    if (msg.tag === "ShowHelpText") {
      return true;
    }
    if (msg.tag === "MissingError" && msg._1 === "CmdStart" && pprefs.prefShowHelpOnEmpty) {
      return true;
    }
    return pprefs.prefShowHelpOnError;
  })();
  const exit_code = (() => {
    if (msg.tag === "ErrorMsg") {
      return pinfo.infoFailureCode;
    }
    if (msg.tag === "MissingError") {
      return pinfo.infoFailureCode;
    }
    if (msg.tag === "ExpectsArgError") {
      return pinfo.infoFailureCode;
    }
    if (msg.tag === "UnexpectedError") {
      return pinfo.infoFailureCode;
    }
    if (msg.tag === "ShowHelpText") {
      return Success;
    }
    if (msg.tag === "InfoMsg") {
      return Success;
    }
    fail();
  })();
  const error_help = errorHelp((() => {
    if (msg.tag === "ShowHelpText") {
      return mempty13;
    }
    if (msg.tag === "ErrorMsg") {
      return stringChunk(msg._1);
    }
    if (msg.tag === "InfoMsg") {
      return stringChunk(msg._1);
    }
    if (msg.tag === "MissingError") {
      if (msg._1 === "CmdStart" && pprefs.prefShowHelpOnEmpty) {
        return mempty13;
      }
      const $0 = stringChunk("Missing:");
      const $1 = briefDesc$p(false)(pprefs)(msg._2._1);
      if ($0.tag === "Nothing") {
        return $1;
      }
      if ($1.tag === "Nothing") {
        return $0;
      }
      if ($0.tag === "Just" && $1.tag === "Just") {
        return $Maybe("Just", $Doc("Cat", $0._1, $Doc("Cat", $Doc("Char", " "), $1._1)));
      }
      fail();
    }
    if (msg.tag === "ExpectsArgError") {
      return stringChunk("The option `" + msg._1 + "` expects an argument.");
    }
    if (msg.tag === "UnexpectedError") {
      return stringChunk(startsWith("-")(msg._1) ? "Invalid option `" + msg._1 + "'" : "Invalid argument `" + msg._1 + "'");
    }
    fail();
  })());
  return (progn) => $Tuple(
    (() => {
      const $0 = (names, pinfo$p) => fold1([
        (() => {
          const h = headerHelp(pinfo$p.infoHeader);
          const f = footerHelp(pinfo$p.infoFooter);
          if (show_full_help) {
            return fold1([h, f, parserHelp(pprefs)(pinfo$p.infoParser)]);
          }
          return parserHelpMonoid.mempty;
        })(),
        msg.tag === "InfoMsg" ? parserHelpMonoid.mempty : usageHelp(vcatChunks([
          $Maybe("Just", parserUsage(pprefs)(pinfo$p.infoParser)(unWords([progn, ...names]))),
          pinfo$p.infoProgDesc.tag === "Just" ? $Maybe("Just", indent(2)(pinfo$p.infoProgDesc._1)) : Nothing
        ])),
        suggestion_help,
        error_help
      ]);
      const v = index(ctx)(0);
      if (v.tag === "Nothing") {
        return $0([], pinfo);
      }
      if (v.tag === "Just") {
        return $0(reverse(arrayMap((v$1) => v$1._1)(ctx)), v._1._2);
      }
      fail();
    })(),
    $Tuple(exit_code, $Tuple(pprefs.prefColumns, void 0))
  );
};
var helper = /* @__PURE__ */ (() => abortOption(ShowHelpText)(foldableArray.foldMap(modMonoid)(identity3)([
  $Mod(
    optionFieldsHasName.name($OptName("OptLong", "help")),
    $DefaultProp(Nothing, Nothing),
    identity26
  ),
  $Mod(
    optionFieldsHasName.name($OptName("OptShort", "h")),
    $DefaultProp(Nothing, Nothing),
    identity26
  ),
  help("Show this help text"),
  hidden
])))();
var getProgName = () => {
  const a$p = argv();
  const $0 = index(a$p)(1);
  const $1 = (() => {
    if ($0.tag === "Just") {
      const $12 = split("/")($0._1);
      return index($12)($12.length - 1 | 0);
    }
    if ($0.tag === "Nothing") {
      return Nothing;
    }
    fail();
  })();
  if ($1.tag === "Nothing") {
    return "";
  }
  if ($1.tag === "Just") {
    return $1._1;
  }
  fail();
};
var getArgs = () => {
  const a$p = argv();
  return slice(2)(a$p.length)(a$p);
};
var exitSuccess = /* @__PURE__ */ (() => exit(boundedEnumExitCode.fromEnum(Success)))();
var handleParseResult = (v) => {
  if (v.tag === "Success") {
    const $0 = v._1;
    return () => $0;
  }
  if (v.tag === "Failure") {
    const $0 = v._1;
    return () => {
      const progn = getProgName();
      const v1 = renderFailure($0)(progn);
      writeString(v1._2 === "Success" ? process.stdout : process.stderr)(UTF8)(v1._1 + "\n")(mempty22)();
      return exit(boundedEnumExitCode.fromEnum(v1._2))();
    };
  }
  if (v.tag === "CompletionInvoked") {
    const $0 = v._1;
    return () => {
      const progn = getProgName();
      const msg = $0.execCompletion(progn)();
      writeString(process.stdout)(UTF8)(msg)(mempty22)();
      return exitSuccess();
    };
  }
  fail();
};
var execParserPure = (pprefs) => (pinfo) => (args) => {
  const v = runParserFully(pMonadP)(pinfo.infoPolicy)($Parser(
    "AltP",
    parserFunctor.map(Left)(bashCompletionParser(pinfo)(pprefs)),
    parserFunctor.map(Right)(pinfo.infoParser)
  ))(fromFoldable24(args))([])(pprefs);
  if (v._1.tag === "Right") {
    if (v._1._1.tag === "Right") {
      return $ParserResult("Success", v._1._1._1);
    }
    if (v._1._1.tag === "Left") {
      return $ParserResult("CompletionInvoked", v._1._1._1);
    }
    fail();
  }
  if (v._1.tag === "Left") {
    return $ParserResult("Failure", parserFailure(pprefs)(pinfo)(v._1._1)(v._2));
  }
  fail();
};

// output-es/Fluid/index.js
var $Command = (_1) => ({ tag: "Evaluate", _1 });
var $EvalArgs = (_1) => ({ tag: "EvalArgs", _1 });
var loadFileNodeT2 = /* @__PURE__ */ loadFileNodeT(monadAffAff)(monadErrorAff);
var loadProgCxt2 = /* @__PURE__ */ loadProgCxt(monadAffAff)(monadErrorAff)(loadFileNodeT2);
var prepConfig2 = /* @__PURE__ */ prepConfig(monadAffAff)(monadErrorAff)(loadFileNodeT2);
var graphEval2 = /* @__PURE__ */ graphEval(monadAffAff)(loadFileNodeT2)(monadErrorAff);
var fromFoldable25 = /* @__PURE__ */ (() => fromFoldableImpl(foldableList.foldr))();
var Evaluate = (value0) => $Command(value0);
var parseLocal = /* @__PURE__ */ $Parser(
  "AltP",
  /* @__PURE__ */ flag$p(true)(/* @__PURE__ */ (() => {
    const $0 = help("Are you running fluid as a library?");
    const $1 = $0._2._1.tag === "Nothing" ? Nothing : $0._2._1;
    const $2 = $0._2._2.tag === "Nothing" ? Nothing : $0._2._2;
    return $Mod(
      (x) => $0._1({
        flagNames: [$OptName("OptShort", "l"), $OptName("OptLong", "local"), ...x.flagNames],
        flagActive: x.flagActive
      }),
      $DefaultProp($1.tag === "Nothing" ? Nothing : $1, $2.tag === "Nothing" ? Nothing : $2),
      (x) => $0._3(x)
    );
  })()),
  /* @__PURE__ */ $Parser("NilP", false)
);
var parseImports = /* @__PURE__ */ $Parser(
  "BindP",
  /* @__PURE__ */ manyM(/* @__PURE__ */ option(readerAsk)(/* @__PURE__ */ (() => {
    const $0 = help("Comma-separated list of files to import");
    const $1 = $0._2._1.tag === "Nothing" ? Nothing : $0._2._1;
    const $2 = $0._2._2.tag === "Nothing" ? Nothing : $0._2._2;
    return $Mod(
      (x) => $0._1({
        optNames: [$OptName("OptShort", "i"), $OptName("OptLong", "imports"), ...x.optNames],
        optCompleter: x.optCompleter,
        optNoArgError: x.optNoArgError
      }),
      $DefaultProp($1.tag === "Nothing" ? Nothing : $1, $2.tag === "Nothing" ? Nothing : $2),
      (x) => $0._3(x)
    );
  })()))
);
var evaluate = (v) => {
  const $0 = v._1.fileName;
  const fluidSrcPaths = [v._1.fluidSrcPath, ...v._1.local ? ["node_modules/@explorable-viz/fluid/dist/fluid/fluid"] : []];
  return _bind(loadProgCxt2({ fluidSrcPaths })(v._1.imports)(v._1.datasets))((progCxt) => _bind(prepConfig2({ fluidSrcPaths })($0)(progCxt))((v1) => _bind(graphEval2(v1.gconfig)(v1.e))((v2) => _pure(functorVal.map((v$1) => {
  })(v2["out\u03B1"])))));
};
var dispatchCommand = (v) => _bind(evaluate(v._1))((v1) => _liftEffect(log(intercalate4("\n")(removeDocWS(prettyVal(highlightableUnit).pretty(v1)).lines))));
var callback = (v) => {
  if (v.tag === "Left") {
    return log(showErrorImpl(v._1));
  }
  if (v.tag === "Right") {
    return () => {
    };
  }
  fail();
};
var between3 = (p1) => (p2) => (f) => (s) => {
  const $0 = stripPrefix(p1)(s);
  const v = (() => {
    if ($0.tag === "Just") {
      return stripSuffix(p2)($0._1);
    }
    if ($0.tag === "Nothing") {
      return Nothing;
    }
    fail();
  })();
  if (v.tag === "Just") {
    return f(v._1);
  }
  if (v.tag === "Nothing") {
    return $Either("Left", "Expected (Pattern " + showStringImpl(p1) + ")...(Pattern " + showStringImpl(p2) + ") but got ...");
  }
  fail();
};
var parsePair = /* @__PURE__ */ between3("(")(")")((s) => {
  const v = split(",")(s);
  if (v.length === 2) {
    return $Either("Right", $Tuple(trim(v[0]), trim(v[1])));
  }
  return $Either("Left", "Expected a pair but got " + s);
});
var parseDatasets = /* @__PURE__ */ $Parser(
  "BindP",
  /* @__PURE__ */ manyM(/* @__PURE__ */ option(/* @__PURE__ */ eitherReader(parsePair))(/* @__PURE__ */ (() => {
    const $0 = help("Comma-separated list of datasets");
    const $1 = $0._2._1.tag === "Nothing" ? Nothing : $0._2._1;
    const $2 = $0._2._2.tag === "Nothing" ? Nothing : $0._2._2;
    return $Mod(
      (x) => $0._1({
        optNames: [$OptName("OptShort", "d"), $OptName("OptLong", "datasets"), ...x.optNames],
        optCompleter: x.optCompleter,
        optNoArgError: x.optNoArgError
      }),
      $DefaultProp($1.tag === "Nothing" ? Nothing : $1, $2.tag === "Nothing" ? Nothing : $2),
      (x) => $0._3(x)
    );
  })()))
);
var parseEvaluate = /* @__PURE__ */ (() => $Parser(
  "MultP",
  $MultPE(
    $Parser(
      "MultP",
      $MultPE(
        $Parser(
          "MultP",
          $MultPE(
            $Parser(
              "MultP",
              $MultPE(
                parserFunctor.map((v) => (v1) => (v2) => (v3) => (v4) => $EvalArgs({ local: v, imports: v1, datasets: v2, fileName: v3, fluidSrcPath: v4 }))(parseLocal),
                parserFunctor.map(fromFoldable25)(parseImports)
              )
            ),
            parserFunctor.map(fromFoldable25)(parseDatasets)
          )
        ),
        option(readerAsk)((() => {
          const $0 = help("The file to parse");
          const $1 = $0._2._1.tag === "Nothing" ? Nothing : $0._2._1;
          const $2 = $0._2._2.tag === "Nothing" ? Nothing : $0._2._2;
          return $Mod(
            (x) => $0._1({
              optNames: [$OptName("OptShort", "f"), $OptName("OptLong", "file"), ...x.optNames],
              optCompleter: x.optCompleter,
              optNoArgError: x.optNoArgError
            }),
            $DefaultProp($1.tag === "Nothing" ? Nothing : $1, $2.tag === "Nothing" ? Nothing : $2),
            (x) => $0._3(x)
          );
        })())
      )
    ),
    parserFunctor.map(Folder)(option(readerAsk)((() => {
      const $0 = help("The path containing the program files");
      const $1 = $0._2._1.tag === "Nothing" ? Nothing : $0._2._1;
      const $2 = $0._2._2.tag === "Nothing" ? Nothing : $0._2._2;
      return $Mod(
        (x) => $0._1({
          optNames: [$OptName("OptShort", "p"), $OptName("OptLong", "fluid-src-path"), ...x.optNames],
          optCompleter: x.optCompleter,
          optNoArgError: x.optNoArgError
        }),
        $DefaultProp($1.tag === "Nothing" ? Nothing : $1, $2.tag === "Nothing" ? Nothing : $2),
        (x) => $0._3(x)
      );
    })()))
  )
))();
var commands = /* @__PURE__ */ (() => ({ evaluate: parserFunctor.map(Evaluate)(parseEvaluate) }))();
var commandParser = /* @__PURE__ */ (() => subparser(command("evaluate")(progDesc("Evaluate a file")({
  infoParser: commands.evaluate,
  infoFullDesc: true,
  infoProgDesc: mempty12,
  infoHeader: mempty12,
  infoFooter: mempty12,
  infoFailureCode: $$Error,
  infoPolicy: Intersperse
}))))();
var main = /* @__PURE__ */ (() => {
  const $0 = runAff(callback)(_bind(_liftEffect((() => {
    const $02 = header("parse - a simple parser")(progDesc("Parse a file")({
      infoFullDesc: true,
      infoFailureCode: $$Error,
      infoFooter: mempty12,
      infoHeader: mempty12,
      infoParser: apApplyFlipped(parserApply)(commandParser)(helper),
      infoPolicy: Intersperse,
      infoProgDesc: mempty12
    }));
    return () => {
      const a$p = getArgs();
      return handleParseResult(execParserPure(defaultPrefs)($02)(a$p))();
    };
  })()))(dispatchCommand));
  return () => {
    $0();
  };
})();

// <stdin>
main();
