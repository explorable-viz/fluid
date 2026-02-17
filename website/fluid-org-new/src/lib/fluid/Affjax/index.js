// | Note: this module is not intended to be used by end-users.
// | Rather, use the environment-specific library instead:
// | - [`purescript-affjax-node`](https://github.com/purescript-contrib/purescript-affjax-node)
// | - [`purescript-affjax-web`](https://github.com/purescript-contrib/purescript-affjax-web)
// |
// | You should use this module if you are writing a driver for a specific environment.
// | See this module's source code for more details.
// If you want to write a driver for an environment, see the comments
// for the `AffjaxDriver` type and look at the `node` and `web` libraries
// (linked above) as examples.
import * as $runtime from "../runtime.js";
import * as Affjax$dRequestHeader from "../Affjax.RequestHeader/index.js";
import * as Affjax$dResponseFormat from "../Affjax.ResponseFormat/index.js";
import * as Affjax$dResponseHeader from "../Affjax.ResponseHeader/index.js";
import * as Control$dMonad$dError$dClass from "../Control.Monad.Error.Class/index.js";
import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dArgonaut$dParser from "../Data.Argonaut.Parser/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFormURLEncoded from "../Data.FormURLEncoded/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dHTTP$dMethod from "../Data.HTTP.Method/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Effect$dAff$dCompat from "../Effect.Aff.Compat/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Foreign from "../Foreign/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import {_ajax} from "./foreign.js";
const $$$Error = (tag, _1, _2) => ({tag, _1, _2});
const pure = /* #__PURE__ */ (() => Control$dMonad$dExcept$dTrans.applicativeExceptT(Data$dIdentity.monadIdentity).pure)();
const alt = /* #__PURE__ */ (() => Control$dMonad$dExcept$dTrans.altExceptT(Data$dList$dTypes.semigroupNonEmptyList)(Data$dIdentity.monadIdentity).alt)();
const any = /* #__PURE__ */ (() => Data$dFoldable.foldableArray.foldMap((() => {
  const semigroupDisj1 = {append: v => v1 => v || v1};
  return {mempty: false, Semigroup0: () => semigroupDisj1};
})()))();
const $$try = /* #__PURE__ */ Control$dMonad$dError$dClass.try(Effect$dAff.monadErrorAff);
const RequestContentError = value0 => $$$Error("RequestContentError", value0);
const ResponseBodyError = value0 => value1 => $$$Error("ResponseBodyError", value0, value1);
const TimeoutError = /* #__PURE__ */ $$$Error("TimeoutError");
const RequestFailedError = /* #__PURE__ */ $$$Error("RequestFailedError");
const XHROtherError = value0 => $$$Error("XHROtherError", value0);
const request = driver => req => {
  const fromResponse = (() => {
    if (req.responseFormat.tag === "ArrayBuffer") { return Foreign.unsafeReadTagged(Data$dIdentity.monadIdentity)("ArrayBuffer"); }
    if (req.responseFormat.tag === "Blob") { return Foreign.unsafeReadTagged(Data$dIdentity.monadIdentity)("Blob"); }
    if (req.responseFormat.tag === "Document") {
      return x => alt(Foreign.unsafeReadTagged(Data$dIdentity.monadIdentity)("Document")(x))(alt(Foreign.unsafeReadTagged(Data$dIdentity.monadIdentity)("XMLDocument")(x))(Foreign.unsafeReadTagged(Data$dIdentity.monadIdentity)("HTMLDocument")(x)));
    }
    if (req.responseFormat.tag === "Json") {
      const $0 = Control$dMonad$dExcept$dTrans.bindExceptT(Data$dIdentity.monadIdentity);
      return a => $0.bind(Foreign.unsafeReadTagged(Data$dIdentity.monadIdentity)("String")(a))(x => req.responseFormat._1((() => {
        if (x === "") { return pure(Data$dArgonaut$dCore.jsonEmptyObject); }
        const $1 = Data$dArgonaut$dParser._jsonParser(Data$dEither.Left, Data$dEither.Right, x);
        if ($1.tag === "Left") {
          return Control$dMonad$dExcept$dTrans.monadThrowExceptT(Data$dIdentity.monadIdentity).throwError(Data$dNonEmpty.$NonEmpty(
            Foreign.$ForeignError("ForeignError", $1._1),
            Data$dList$dTypes.Nil
          ));
        }
        if ($1.tag === "Right") { return pure($1._1); }
        $runtime.fail();
      })()));
    }
    if (req.responseFormat.tag === "String") { return Foreign.unsafeReadTagged(Data$dIdentity.monadIdentity)("String"); }
    if (req.responseFormat.tag === "Ignore") {
      const $0 = req.responseFormat._1(pure());
      return v => $0;
    }
    $runtime.fail();
  })();
  const send = content => Effect$dAff._map(v => {
    if (v.tag === "Right") {
      const $0 = fromResponse(v._1.body);
      if ($0.tag === "Left") { return Data$dEither.$Either("Left", $$$Error("ResponseBodyError", $0._1._1, v._1)); }
      if ($0.tag === "Right") { return Data$dEither.$Either("Right", {body: $0._1, headers: v._1.headers, status: v._1.status, statusText: v._1.statusText}); }
      $runtime.fail();
    }
    if (v.tag === "Left") {
      return Data$dEither.$Either(
        "Left",
        (() => {
          const message = Effect$dException.message(v._1);
          if (message === "AffjaxTimeoutErrorMessageIdent") { return TimeoutError; }
          if (message === "AffjaxRequestFailedMessageIdent") { return RequestFailedError; }
          return $$$Error("XHROtherError", v._1);
        })()
      );
    }
    $runtime.fail();
  })($$try(Effect$dAff$dCompat.fromEffectFnAff(_ajax(
    driver,
    "AffjaxTimeoutErrorMessageIdent",
    "AffjaxRequestFailedMessageIdent",
    Affjax$dResponseHeader.ResponseHeader,
    {
      method: Data$dHTTP$dMethod.print(req.method),
      url: req.url,
      headers: Data$dFunctor.arrayMap(h => (
        {
          field: (() => {
            if (h.tag === "Accept") { return "Accept"; }
            if (h.tag === "ContentType") { return "Content-Type"; }
            if (h.tag === "RequestHeader") { return h._1; }
            $runtime.fail();
          })(),
          value: (() => {
            if (h.tag === "Accept") { return h._1; }
            if (h.tag === "ContentType") { return h._1; }
            if (h.tag === "RequestHeader") { return h._2; }
            $runtime.fail();
          })()
        }
      ))((() => {
        if (req.content.tag === "Just") {
          if (req.content._1.tag === "FormURLEncoded") {
            if (req.responseFormat.tag === "Json") {
              const $0 = !any(y => {
                if (y.tag === "Accept") { return true; }
                if (y.tag === "ContentType") { return false; }
                if (y.tag === "RequestHeader") { return "Accept" === y._1; }
                $runtime.fail();
              })(req.headers)
                ? Data$dArray.snoc(req.headers)(Affjax$dRequestHeader.$RequestHeader("Accept", "application/json"))
                : req.headers;
              if (
                !any(y => {
                  if (y.tag === "Accept") { return false; }
                  if (y.tag === "ContentType") { return true; }
                  if (y.tag === "RequestHeader") { return "Content-Type" === y._1; }
                  $runtime.fail();
                })($0)
              ) {
                return Data$dArray.snoc($0)(Affjax$dRequestHeader.$RequestHeader("ContentType", "application/x-www-form-urlencoded"));
              }
              return $0;
            }
            if (
              !any(y => {
                if (y.tag === "Accept") { return false; }
                if (y.tag === "ContentType") { return true; }
                if (y.tag === "RequestHeader") { return "Content-Type" === y._1; }
                $runtime.fail();
              })(req.headers)
            ) {
              return Data$dArray.snoc(req.headers)(Affjax$dRequestHeader.$RequestHeader("ContentType", "application/x-www-form-urlencoded"));
            }
            return req.headers;
          }
          if (req.content._1.tag === "Json") {
            if (req.responseFormat.tag === "Json") {
              const $0 = !any(y => {
                if (y.tag === "Accept") { return true; }
                if (y.tag === "ContentType") { return false; }
                if (y.tag === "RequestHeader") { return "Accept" === y._1; }
                $runtime.fail();
              })(req.headers)
                ? Data$dArray.snoc(req.headers)(Affjax$dRequestHeader.$RequestHeader("Accept", "application/json"))
                : req.headers;
              if (
                !any(y => {
                  if (y.tag === "Accept") { return false; }
                  if (y.tag === "ContentType") { return true; }
                  if (y.tag === "RequestHeader") { return "Content-Type" === y._1; }
                  $runtime.fail();
                })($0)
              ) {
                return Data$dArray.snoc($0)(Affjax$dRequestHeader.$RequestHeader("ContentType", "application/json"));
              }
              return $0;
            }
            if (
              !any(y => {
                if (y.tag === "Accept") { return false; }
                if (y.tag === "ContentType") { return true; }
                if (y.tag === "RequestHeader") { return "Content-Type" === y._1; }
                $runtime.fail();
              })(req.headers)
            ) {
              return Data$dArray.snoc(req.headers)(Affjax$dRequestHeader.$RequestHeader("ContentType", "application/json"));
            }
            return req.headers;
          }
          if (
            req.responseFormat.tag === "Json" && !any(y => {
              if (y.tag === "Accept") { return true; }
              if (y.tag === "ContentType") { return false; }
              if (y.tag === "RequestHeader") { return "Accept" === y._1; }
              $runtime.fail();
            })(req.headers)
          ) {
            return Data$dArray.snoc(req.headers)(Affjax$dRequestHeader.$RequestHeader("Accept", "application/json"));
          }
          return req.headers;
        }
        if (req.content.tag === "Nothing") {
          if (
            req.responseFormat.tag === "Json" && !any(y => {
              if (y.tag === "Accept") { return true; }
              if (y.tag === "ContentType") { return false; }
              if (y.tag === "RequestHeader") { return "Accept" === y._1; }
              $runtime.fail();
            })(req.headers)
          ) {
            return Data$dArray.snoc(req.headers)(Affjax$dRequestHeader.$RequestHeader("Accept", "application/json"));
          }
          return req.headers;
        }
        $runtime.fail();
      })()),
      content,
      responseType: (() => {
        if (req.responseFormat.tag === "ArrayBuffer") { return "arraybuffer"; }
        if (req.responseFormat.tag === "Blob") { return "blob"; }
        if (req.responseFormat.tag === "Document") { return "document"; }
        if (req.responseFormat.tag === "Json") { return "text"; }
        if (req.responseFormat.tag === "String") { return "text"; }
        if (req.responseFormat.tag === "Ignore") { return ""; }
        $runtime.fail();
      })(),
      username: (() => {
        if (req.username.tag === "Nothing") { return Data$dNullable.null; }
        if (req.username.tag === "Just") { return Data$dNullable.notNull(req.username._1); }
        $runtime.fail();
      })(),
      password: (() => {
        if (req.password.tag === "Nothing") { return Data$dNullable.null; }
        if (req.password.tag === "Just") { return Data$dNullable.notNull(req.password._1); }
        $runtime.fail();
      })(),
      withCredentials: req.withCredentials,
      timeout: req.timeout.tag === "Just" ? req.timeout._1 : 0.0
    }
  ))));
  if (req.content.tag === "Nothing") { return send(Data$dNullable.null); }
  if (req.content.tag === "Just") {
    if (req.content._1.tag === "ArrayView") { return send(Data$dNullable.notNull(req.content._1._1(Unsafe$dCoerce.unsafeCoerce))); }
    if (req.content._1.tag === "Blob") { return send(Data$dNullable.notNull(req.content._1._1)); }
    if (req.content._1.tag === "Document") { return send(Data$dNullable.notNull(req.content._1._1)); }
    if (req.content._1.tag === "String") { return send(Data$dNullable.notNull(req.content._1._1)); }
    if (req.content._1.tag === "FormData") { return send(Data$dNullable.notNull(req.content._1._1)); }
    if (req.content._1.tag === "FormURLEncoded") {
      const $0 = Data$dFormURLEncoded.encode(req.content._1._1);
      if ($0.tag === "Just") { return send(Data$dNullable.notNull($0._1)); }
      return Effect$dAff._pure(Data$dEither.$Either("Left", $$$Error("RequestContentError", "Body contains values that cannot be encoded as application/x-www-form-urlencoded")));
    }
    if (req.content._1.tag === "Json") { return send(Data$dNullable.notNull(Data$dArgonaut$dCore.stringify(req.content._1._1))); }
  }
  $runtime.fail();
};
const printError = v => {
  if (v.tag === "RequestContentError") { return "There was a problem with the request content: " + v._1; }
  if (v.tag === "ResponseBodyError") { return "There was a problem with the response body: " + Foreign.renderForeignError(v._1); }
  if (v.tag === "TimeoutError") { return "There was a problem making the request: timeout"; }
  if (v.tag === "RequestFailedError") { return "There was a problem making the request: request failed"; }
  if (v.tag === "XHROtherError") { return "There was a problem making the request: " + Effect$dException.message(v._1); }
  $runtime.fail();
};
const defaultRequest = {
  method: /* #__PURE__ */ Data$dEither.$Either("Left", Data$dHTTP$dMethod.GET),
  url: "/",
  headers: [],
  content: Data$dMaybe.Nothing,
  username: Data$dMaybe.Nothing,
  password: Data$dMaybe.Nothing,
  withCredentials: false,
  responseFormat: /* #__PURE__ */ Affjax$dResponseFormat.$ResponseFormat("Ignore", Affjax$dResponseFormat.identity),
  timeout: Data$dMaybe.Nothing
};
const $$delete = driver => rf => u => request(driver)({
  method: Data$dEither.$Either("Left", Data$dHTTP$dMethod.DELETE),
  url: u,
  headers: [],
  content: Data$dMaybe.Nothing,
  username: Data$dMaybe.Nothing,
  password: Data$dMaybe.Nothing,
  withCredentials: false,
  responseFormat: rf,
  timeout: Data$dMaybe.Nothing
});
const delete_ = driver => {
  const $0 = Effect$dAff._map(m => {
    if (m.tag === "Left") { return Data$dEither.$Either("Left", m._1); }
    if (m.tag === "Right") { return Data$dEither.$Either("Right", undefined); }
    $runtime.fail();
  });
  return x => $0($$delete(driver)(Affjax$dResponseFormat.$ResponseFormat("Ignore", Affjax$dResponseFormat.identity))(x));
};
const $$get = driver => rf => u => request(driver)({
  method: Data$dEither.$Either("Left", Data$dHTTP$dMethod.GET),
  url: u,
  headers: [],
  content: Data$dMaybe.Nothing,
  username: Data$dMaybe.Nothing,
  password: Data$dMaybe.Nothing,
  withCredentials: false,
  responseFormat: rf,
  timeout: Data$dMaybe.Nothing
});
const patch = driver => rf => u => c => request(driver)({
  method: Data$dEither.$Either("Left", Data$dHTTP$dMethod.PATCH),
  url: u,
  headers: [],
  content: Data$dMaybe.$Maybe("Just", c),
  username: Data$dMaybe.Nothing,
  password: Data$dMaybe.Nothing,
  withCredentials: false,
  responseFormat: rf,
  timeout: Data$dMaybe.Nothing
});
const patch_ = driver => url => {
  const $0 = Effect$dAff._map(m => {
    if (m.tag === "Left") { return Data$dEither.$Either("Left", m._1); }
    if (m.tag === "Right") { return Data$dEither.$Either("Right", undefined); }
    $runtime.fail();
  });
  return x => $0(patch(driver)(Affjax$dResponseFormat.$ResponseFormat("Ignore", Affjax$dResponseFormat.identity))(url)(x));
};
const post = driver => rf => u => c => request(driver)({
  method: Data$dEither.$Either("Left", Data$dHTTP$dMethod.POST),
  url: u,
  headers: [],
  content: c,
  username: Data$dMaybe.Nothing,
  password: Data$dMaybe.Nothing,
  withCredentials: false,
  responseFormat: rf,
  timeout: Data$dMaybe.Nothing
});
const post_ = driver => url => {
  const $0 = Effect$dAff._map(m => {
    if (m.tag === "Left") { return Data$dEither.$Either("Left", m._1); }
    if (m.tag === "Right") { return Data$dEither.$Either("Right", undefined); }
    $runtime.fail();
  });
  return x => $0(post(driver)(Affjax$dResponseFormat.$ResponseFormat("Ignore", Affjax$dResponseFormat.identity))(url)(x));
};
const put = driver => rf => u => c => request(driver)({
  method: Data$dEither.$Either("Left", Data$dHTTP$dMethod.PUT),
  url: u,
  headers: [],
  content: c,
  username: Data$dMaybe.Nothing,
  password: Data$dMaybe.Nothing,
  withCredentials: false,
  responseFormat: rf,
  timeout: Data$dMaybe.Nothing
});
const put_ = driver => url => {
  const $0 = Effect$dAff._map(m => {
    if (m.tag === "Left") { return Data$dEither.$Either("Left", m._1); }
    if (m.tag === "Right") { return Data$dEither.$Either("Right", undefined); }
    $runtime.fail();
  });
  return x => $0(put(driver)(Affjax$dResponseFormat.$ResponseFormat("Ignore", Affjax$dResponseFormat.identity))(url)(x));
};
export {
  
  
  RequestFailedError,
  
  
  
  
  
  
  $$delete as delete,
  delete_,
  $$get as get,
  patch,
  patch_,
  post,
  post_,
  
  
  put,
  put_,
  request,
  
};
export * from "./foreign.js";
