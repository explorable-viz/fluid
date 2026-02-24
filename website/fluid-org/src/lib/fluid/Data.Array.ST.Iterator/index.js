import * as $runtime from "../runtime.js";
const $Iterator = (_1, _2) => ({tag: "Iterator", _1, _2});
const Iterator = value0 => value1 => $Iterator(value0, value1);
const peek = v => {
  const $0 = v._2;
  return () => {
    const i = $0.value;
    return v._1(i);
  };
};
const next = v => {
  const $0 = v._2;
  return () => {
    const i = $0.value;
    const $1 = $0.value;
    $0.value = $1 + 1 | 0;
    return v._1(i);
  };
};
const pushWhile = p => iter => array => () => {
  let $$break = false;
  const $0 = iter._2;
  while (
    (() => {
      const $1 = $$break;
      return !$1;
    })()
  ) {
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
const pushAll = /* #__PURE__ */ pushWhile(v => true);
const iterator = f => {
  const $0 = Iterator(f);
  return () => {
    const $1 = {value: 0};
    return $0($1);
  };
};
const iterate = iter => f => () => {
  let $$break = false;
  const $0 = iter._2;
  while (
    (() => {
      const $1 = $$break;
      return !$1;
    })()
  ) {
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
    $runtime.fail();
  }
};
const exhausted = x => {
  const $0 = x._2;
  return () => {
    const i = $0.value;
    const $1 = x._1(i);
    if ($1.tag === "Nothing") { return true; }
    if ($1.tag === "Just") { return false; }
    $runtime.fail();
  };
};
export {$Iterator,   iterate,     pushWhile};
