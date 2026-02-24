import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
const $Method = tag => tag;
const OPTIONS = /* #__PURE__ */ $Method("OPTIONS");
const GET = /* #__PURE__ */ $Method("GET");
const HEAD = /* #__PURE__ */ $Method("HEAD");
const POST = /* #__PURE__ */ $Method("POST");
const PUT = /* #__PURE__ */ $Method("PUT");
const DELETE = /* #__PURE__ */ $Method("DELETE");
const TRACE = /* #__PURE__ */ $Method("TRACE");
const CONNECT = /* #__PURE__ */ $Method("CONNECT");
const PROPFIND = /* #__PURE__ */ $Method("PROPFIND");
const PROPPATCH = /* #__PURE__ */ $Method("PROPPATCH");
const MKCOL = /* #__PURE__ */ $Method("MKCOL");
const COPY = /* #__PURE__ */ $Method("COPY");
const MOVE = /* #__PURE__ */ $Method("MOVE");
const LOCK = /* #__PURE__ */ $Method("LOCK");
const UNLOCK = /* #__PURE__ */ $Method("UNLOCK");
const PATCH = /* #__PURE__ */ $Method("PATCH");
const unCustomMethod = v => v;
const showMethod = {
  show: v => {
    if (v === "OPTIONS") { return "OPTIONS"; }
    if (v === "GET") { return "GET"; }
    if (v === "HEAD") { return "HEAD"; }
    if (v === "POST") { return "POST"; }
    if (v === "PUT") { return "PUT"; }
    if (v === "DELETE") { return "DELETE"; }
    if (v === "TRACE") { return "TRACE"; }
    if (v === "CONNECT") { return "CONNECT"; }
    if (v === "PROPFIND") { return "PROPFIND"; }
    if (v === "PROPPATCH") { return "PROPPATCH"; }
    if (v === "MKCOL") { return "MKCOL"; }
    if (v === "COPY") { return "COPY"; }
    if (v === "MOVE") { return "MOVE"; }
    if (v === "LOCK") { return "LOCK"; }
    if (v === "UNLOCK") { return "UNLOCK"; }
    if (v === "PATCH") { return "PATCH"; }
    $runtime.fail();
  }
};
const showCustomMethod = {show: v => "(CustomMethod " + Data$dShow.showStringImpl(v) + ")"};
const print = v2 => {
  if (v2.tag === "Left") {
    if (v2._1 === "OPTIONS") { return "OPTIONS"; }
    if (v2._1 === "GET") { return "GET"; }
    if (v2._1 === "HEAD") { return "HEAD"; }
    if (v2._1 === "POST") { return "POST"; }
    if (v2._1 === "PUT") { return "PUT"; }
    if (v2._1 === "DELETE") { return "DELETE"; }
    if (v2._1 === "TRACE") { return "TRACE"; }
    if (v2._1 === "CONNECT") { return "CONNECT"; }
    if (v2._1 === "PROPFIND") { return "PROPFIND"; }
    if (v2._1 === "PROPPATCH") { return "PROPPATCH"; }
    if (v2._1 === "MKCOL") { return "MKCOL"; }
    if (v2._1 === "COPY") { return "COPY"; }
    if (v2._1 === "MOVE") { return "MOVE"; }
    if (v2._1 === "LOCK") { return "LOCK"; }
    if (v2._1 === "UNLOCK") { return "UNLOCK"; }
    if (v2._1 === "PATCH") { return "PATCH"; }
    $runtime.fail();
  }
  if (v2.tag === "Right") { return v2._1; }
  $runtime.fail();
};
const parse = handleMethod => handleUnknown => s => {
  const v = Data$dString$dCommon.toUpper(s);
  if (v === "OPTIONS") { return handleMethod(OPTIONS); }
  if (v === "GET") { return handleMethod(GET); }
  if (v === "HEAD") { return handleMethod(HEAD); }
  if (v === "POST") { return handleMethod(POST); }
  if (v === "PUT") { return handleMethod(PUT); }
  if (v === "DELETE") { return handleMethod(DELETE); }
  if (v === "TRACE") { return handleMethod(TRACE); }
  if (v === "CONNECT") { return handleMethod(CONNECT); }
  if (v === "PROPFIND") { return handleMethod(PROPFIND); }
  if (v === "PROPPATCH") { return handleMethod(PROPPATCH); }
  if (v === "MKCOL") { return handleMethod(MKCOL); }
  if (v === "COPY") { return handleMethod(COPY); }
  if (v === "MOVE") { return handleMethod(MOVE); }
  if (v === "LOCK") { return handleMethod(LOCK); }
  if (v === "UNLOCK") { return handleMethod(UNLOCK); }
  if (v === "PATCH") { return handleMethod(PATCH); }
  return handleUnknown(v);
};
const fromString = /* #__PURE__ */ parse(Data$dEither.Left)(x => Data$dEither.$Either("Right", x));
const eqMethod = {
  eq: x => y => {
    if (x === "OPTIONS") { return y === "OPTIONS"; }
    if (x === "GET") { return y === "GET"; }
    if (x === "HEAD") { return y === "HEAD"; }
    if (x === "POST") { return y === "POST"; }
    if (x === "PUT") { return y === "PUT"; }
    if (x === "DELETE") { return y === "DELETE"; }
    if (x === "TRACE") { return y === "TRACE"; }
    if (x === "CONNECT") { return y === "CONNECT"; }
    if (x === "PROPFIND") { return y === "PROPFIND"; }
    if (x === "PROPPATCH") { return y === "PROPPATCH"; }
    if (x === "MKCOL") { return y === "MKCOL"; }
    if (x === "COPY") { return y === "COPY"; }
    if (x === "MOVE") { return y === "MOVE"; }
    if (x === "LOCK") { return y === "LOCK"; }
    if (x === "UNLOCK") { return y === "UNLOCK"; }
    return x === "PATCH" && y === "PATCH";
  }
};
const ordMethod = {
  compare: x => y => {
    if (x === "OPTIONS") {
      if (y === "OPTIONS") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "OPTIONS") { return Data$dOrdering.GT; }
    if (x === "GET") {
      if (y === "GET") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "GET") { return Data$dOrdering.GT; }
    if (x === "HEAD") {
      if (y === "HEAD") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "HEAD") { return Data$dOrdering.GT; }
    if (x === "POST") {
      if (y === "POST") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "POST") { return Data$dOrdering.GT; }
    if (x === "PUT") {
      if (y === "PUT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "PUT") { return Data$dOrdering.GT; }
    if (x === "DELETE") {
      if (y === "DELETE") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "DELETE") { return Data$dOrdering.GT; }
    if (x === "TRACE") {
      if (y === "TRACE") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "TRACE") { return Data$dOrdering.GT; }
    if (x === "CONNECT") {
      if (y === "CONNECT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "CONNECT") { return Data$dOrdering.GT; }
    if (x === "PROPFIND") {
      if (y === "PROPFIND") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "PROPFIND") { return Data$dOrdering.GT; }
    if (x === "PROPPATCH") {
      if (y === "PROPPATCH") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "PROPPATCH") { return Data$dOrdering.GT; }
    if (x === "MKCOL") {
      if (y === "MKCOL") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "MKCOL") { return Data$dOrdering.GT; }
    if (x === "COPY") {
      if (y === "COPY") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "COPY") { return Data$dOrdering.GT; }
    if (x === "MOVE") {
      if (y === "MOVE") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "MOVE") { return Data$dOrdering.GT; }
    if (x === "LOCK") {
      if (y === "LOCK") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "LOCK") { return Data$dOrdering.GT; }
    if (x === "UNLOCK") {
      if (y === "UNLOCK") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "UNLOCK") { return Data$dOrdering.GT; }
    if (x === "PATCH" && y === "PATCH") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqMethod
};
const eqCustomMethod = {eq: x => y => x === y};
const ordCustomMethod = {compare: x => y => Data$dOrd.ordString.compare(x)(y), Eq0: () => eqCustomMethod};
export {
  
  
  
  DELETE,
  GET,
  HEAD,
  
  
  
  
  PATCH,
  POST,
  
  
  PUT,
  
  
  
  
  
  
  
  
  print,
  
  
  
};
