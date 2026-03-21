import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import {_dropEffect, _files, _getData, _setData, _setDragImage, _setDropEffect, items, types} from "./foreign.js";
const $DropEffect = tag => tag;
const Copy = /* #__PURE__ */ $DropEffect("Copy");
const Link = /* #__PURE__ */ $DropEffect("Link");
const Move = /* #__PURE__ */ $DropEffect("Move");
const None = /* #__PURE__ */ $DropEffect("None");
const setDropEffect = de => _setDropEffect((() => {
  if (de === "Copy") { return "copy"; }
  if (de === "Link") { return "link"; }
  if (de === "Move") { return "move"; }
  if (de === "None") { return "none"; }
  $runtime.fail();
})());
const setDragImage = _setDragImage;
const setData = v => dat => dt => _setData(v)(dat)(dt);
const getData = v => dt => _getData(v)(dt);
const files = x => Data$dNullable.nullable(_files(x), Data$dMaybe.Nothing, Data$dMaybe.Just);
const eqDropEffect = {
  eq: x => y => {
    if (x === "Copy") { return y === "Copy"; }
    if (x === "Link") { return y === "Link"; }
    if (x === "Move") { return y === "Move"; }
    return x === "None" && y === "None";
  }
};
const ordDropEffect = {
  compare: x => y => {
    if (x === "Copy") {
      if (y === "Copy") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Copy") { return Data$dOrdering.GT; }
    if (x === "Link") {
      if (y === "Link") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Link") { return Data$dOrdering.GT; }
    if (x === "Move") {
      if (y === "Move") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Move") { return Data$dOrdering.GT; }
    if (x === "None" && y === "None") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqDropEffect
};
const dropEffect = dt => {
  const $0 = _dropEffect(dt);
  return () => {
    const a$p = $0();
    if (a$p === "copy") { return Copy; }
    if (a$p === "link") { return Link; }
    if (a$p === "move") { return Move; }
    return None;
  };
};
export {$DropEffect, Copy, Link, Move, None, dropEffect, eqDropEffect, files, getData, ordDropEffect, setData, setDragImage, setDropEffect};
export * from "./foreign.js";
