import * as $runtime from "../runtime.js";
const $Ordering = tag => tag;
const LT = /* #__PURE__ */ $Ordering("LT");
const GT = /* #__PURE__ */ $Ordering("GT");
const EQ = /* #__PURE__ */ $Ordering("EQ");
const showOrdering = {
  show: v => {
    if (v === "LT") { return "LT"; }
    if (v === "GT") { return "GT"; }
    if (v === "EQ") { return "EQ"; }
    $runtime.fail();
  }
};
const semigroupOrdering = {
  append: v => v1 => {
    if (v === "LT") { return LT; }
    if (v === "GT") { return GT; }
    if (v === "EQ") { return v1; }
    $runtime.fail();
  }
};
const invert = v => {
  if (v === "GT") { return LT; }
  if (v === "EQ") { return EQ; }
  if (v === "LT") { return GT; }
  $runtime.fail();
};
const eqOrdering = {
  eq: v => v1 => {
    if (v === "LT") { return v1 === "LT"; }
    if (v === "GT") { return v1 === "GT"; }
    return v === "EQ" && v1 === "EQ";
  }
};
export { EQ, GT, LT, eqOrdering,  semigroupOrdering, };
