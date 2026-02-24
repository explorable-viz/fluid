function newSTArray() {
  return [];
}
export { newSTArray as new };

export const peekImpl = function (just, nothing, i, xs) {
  return i >= 0 && i < xs.length ? just(xs[i]) : nothing;
};

export const pokeImpl = function (i, a, xs) {
  var ret = i >= 0 && i < xs.length;
  if (ret) xs[i] = a;
  return ret;
};

export const lengthImpl = function (xs) {
  return xs.length;
};

export const popImpl = function (just, nothing, xs) {
  return xs.length > 0 ? just(xs.pop()) : nothing;
};

export const pushAllImpl = function (as, xs) {
  return xs.push.apply(xs, as);
};

export const shiftImpl = function (just, nothing, xs) {
  return xs.length > 0 ? just(xs.shift()) : nothing;
};

export const unshiftAllImpl = function (as, xs) {
  return xs.unshift.apply(xs, as);
};

export const spliceImpl = function (i, howMany, bs, xs) {
  return xs.splice.apply(xs, [i, howMany].concat(bs));
};

function unsafeFreezeThawImpl(xs) {
  return xs;
}

export const unsafeFreezeImpl = unsafeFreezeThawImpl;

export const unsafeThawImpl = unsafeFreezeThawImpl;

function copyImpl(xs) {
  return xs.slice();
}

export const freezeImpl = copyImpl;

export const thawImpl = copyImpl;

export const sortByImpl = (function () {
  function mergeFromTo(compare, fromOrdering, xs1, xs2, from, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
    var c;

    mid = from + ((to - from) >> 1);
    if (mid - from > 1) mergeFromTo(compare, fromOrdering, xs2, xs1, from, mid);
    if (to - mid > 1) mergeFromTo(compare, fromOrdering, xs2, xs1, mid, to);

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

  return function (compare, fromOrdering, xs) {
    if (xs.length < 2) return xs;

    mergeFromTo(compare, fromOrdering, xs, xs.slice(0), 0, xs.length);

    return xs;
  };
})();

export const toAssocArrayImpl = function (xs) {
  var n = xs.length;
  var as = new Array(n);
  for (var i = 0; i < n; i++) as[i] = { value: xs[i], index: i };
  return as;
};
