// | This module defines types for effectful uncurried functions, as well as
// | functions for converting back and forth between them.
// |
// | This makes it possible to give a PureScript type to JavaScript functions
// | such as this one:
// |
// | ```javascript
// | function logMessage(level, message) {
// |   console.log(level + ": " + message);
// | }
// | ```
// |
// | In particular, note that `logMessage` performs effects immediately after
// | receiving all of its parameters, so giving it the type `Data.Function.Fn2
// | String String Unit`, while convenient, would effectively be a lie.
// |
// | One way to handle this would be to convert the function into the normal
// | PureScript form (namely, a curried function returning an Effect action),
// | and performing the marshalling in JavaScript, in the FFI module, like this:
// |
// | ```purescript
// | -- In the PureScript file:
// | foreign import logMessage :: String -> String -> Effect Unit
// | ```
// |
// | ```javascript
// | // In the FFI file:
// | exports.logMessage = function(level) {
// |   return function(message) {
// |     return function() {
// |       logMessage(level, message);
// |     };
// |   };
// | };
// | ```
// |
// | This method, unfortunately, turns out to be both tiresome and error-prone.
// | This module offers an alternative solution. By providing you with:
// |
// |  * the ability to give the real `logMessage` function a PureScript type,
// |    and
// |  * functions for converting between this form and the normal PureScript
// |    form,
// |
// | the FFI boilerplate is no longer needed. The previous example becomes:
// |
// | ```purescript
// | -- In the PureScript file:
// | foreign import logMessageImpl :: EffectFn2 String String Unit
// | ```
// |
// | ```javascript
// | // In the FFI file:
// | exports.logMessageImpl = logMessage
// | ```
// |
// | You can then use `runEffectFn2` to provide a nicer version:
// |
// | ```purescript
// | logMessage :: String -> String -> Effect Unit
// | logMessage = runEffectFn2 logMessageImpl
// | ```
// |
// | (note that this has the same type as the original `logMessage`).
// |
// | Effectively, we have reduced the risk of errors by moving as much code into
// | PureScript as possible, so that we can leverage the type system. Hopefully,
// | this is a little less tiresome too.
// |
// | Here's a slightly more advanced example. Here, because we are using
// | callbacks, we need to use `mkEffectFn{N}` as well.
// |
// | Suppose our `logMessage` changes so that it sometimes sends details of the
// | message to some external server, and in those cases, we want the resulting
// | `HttpResponse` (for whatever reason).
// |
// | ```javascript
// | function logMessage(level, message, callback) {
// |   console.log(level + ": " + message);
// |   if (level > LogLevel.WARN) {
// |     LogAggregatorService.post("/logs", {
// |       level: level,
// |       message: message
// |     }, callback);
// |   } else {
// |     callback(null);
// |   }
// | }
// | ```
// |
// | The import then looks like this:
// | ```purescript
// | foreign import logMessageImpl
// |  EffectFn3
// |    String
// |    String
// |    (EffectFn1 (Nullable HttpResponse) Unit)
// |    Unit
// | ```
// |
// | And, as before, the FFI file is extremely simple:
// |
// | ```javascript
// | exports.logMessageImpl = logMessage
// | ```
// |
// | Finally, we use `runEffectFn{N}` and `mkEffectFn{N}` for a more comfortable
// | PureScript version:
// |
// | ```purescript
// | logMessage ::
// |   String ->
// |   String ->
// |   (Nullable HttpResponse -> Effect Unit) ->
// |   Effect Unit
// | logMessage level message callback =
// |   runEffectFn3 logMessageImpl level message (mkEffectFn1 callback)
// | ```
// |
// | The general naming scheme for functions and types in this module is as
// | follows:
// |
// | * `EffectFn{N}` means, an uncurried function which accepts N arguments and
// |   performs some effects. The first N arguments are the actual function's
// |   argument. The last type argument is the return type.
// | * `runEffectFn{N}` takes an `EffectFn` of N arguments, and converts it into
// |   the normal PureScript form: a curried function which returns an Effect
// |   action.
// | * `mkEffectFn{N}` is the inverse of `runEffectFn{N}`. It can be useful for
// |   callbacks.
// |
import {
  mkEffectFn1,
  mkEffectFn10,
  mkEffectFn2,
  mkEffectFn3,
  mkEffectFn4,
  mkEffectFn5,
  mkEffectFn6,
  mkEffectFn7,
  mkEffectFn8,
  mkEffectFn9,
  runEffectFn1,
  runEffectFn10,
  runEffectFn2,
  runEffectFn3,
  runEffectFn4,
  runEffectFn5,
  runEffectFn6,
  runEffectFn7,
  runEffectFn8,
  runEffectFn9
} from "./foreign.js";
const semigroupEffectFn9 = dictSemigroup => (
  {
    append: f1 => f2 => (a, b, c, d, e, f, g, h, i) => {
      const a$p = f1(a, b, c, d, e, f, g, h, i);
      const a$p$1 = f2(a, b, c, d, e, f, g, h, i);
      return dictSemigroup.append(a$p)(a$p$1);
    }
  }
);
const semigroupEffectFn8 = dictSemigroup => (
  {
    append: f1 => f2 => (a, b, c, d, e, f, g, h) => {
      const a$p = f1(a, b, c, d, e, f, g, h);
      const a$p$1 = f2(a, b, c, d, e, f, g, h);
      return dictSemigroup.append(a$p)(a$p$1);
    }
  }
);
const semigroupEffectFn7 = dictSemigroup => (
  {
    append: f1 => f2 => (a, b, c, d, e, f, g) => {
      const a$p = f1(a, b, c, d, e, f, g);
      const a$p$1 = f2(a, b, c, d, e, f, g);
      return dictSemigroup.append(a$p)(a$p$1);
    }
  }
);
const semigroupEffectFn6 = dictSemigroup => (
  {
    append: f1 => f2 => (a, b, c, d, e, f) => {
      const a$p = f1(a, b, c, d, e, f);
      const a$p$1 = f2(a, b, c, d, e, f);
      return dictSemigroup.append(a$p)(a$p$1);
    }
  }
);
const semigroupEffectFn5 = dictSemigroup => (
  {
    append: f1 => f2 => (a, b, c, d, e) => {
      const a$p = f1(a, b, c, d, e);
      const a$p$1 = f2(a, b, c, d, e);
      return dictSemigroup.append(a$p)(a$p$1);
    }
  }
);
const semigroupEffectFn4 = dictSemigroup => (
  {
    append: f1 => f2 => (a, b, c, d) => {
      const a$p = f1(a, b, c, d);
      const a$p$1 = f2(a, b, c, d);
      return dictSemigroup.append(a$p)(a$p$1);
    }
  }
);
const semigroupEffectFn3 = dictSemigroup => (
  {
    append: f1 => f2 => (a, b, c) => {
      const a$p = f1(a, b, c);
      const a$p$1 = f2(a, b, c);
      return dictSemigroup.append(a$p)(a$p$1);
    }
  }
);
const semigroupEffectFn2 = dictSemigroup => (
  {
    append: f1 => f2 => (a, b) => {
      const a$p = f1(a, b);
      const a$p$1 = f2(a, b);
      return dictSemigroup.append(a$p)(a$p$1);
    }
  }
);
const semigroupEffectFn10 = dictSemigroup => (
  {
    append: f1 => f2 => (a, b, c, d, e, f, g, h, i, j) => {
      const a$p = f1(a, b, c, d, e, f, g, h, i, j);
      const a$p$1 = f2(a, b, c, d, e, f, g, h, i, j);
      return dictSemigroup.append(a$p)(a$p$1);
    }
  }
);
const semigroupEffectFn1 = dictSemigroup => (
  {
    append: f1 => f2 => a => {
      const a$p = f1(a);
      const a$p$1 = f2(a);
      return dictSemigroup.append(a$p)(a$p$1);
    }
  }
);
const monoidEffectFn9 = dictMonoid => {
  const $0 = dictMonoid.mempty;
  const $1 = dictMonoid.Semigroup0();
  const semigroupEffectFn91 = {
    append: f1 => f2 => (a, b, c, d, e, f, g, h, i) => {
      const a$p = f1(a, b, c, d, e, f, g, h, i);
      const a$p$1 = f2(a, b, c, d, e, f, g, h, i);
      return $1.append(a$p)(a$p$1);
    }
  };
  return {mempty: (v, v1, v2, v3, v4, v5, v6, v7, v8) => $0, Semigroup0: () => semigroupEffectFn91};
};
const monoidEffectFn8 = dictMonoid => {
  const $0 = dictMonoid.mempty;
  const $1 = dictMonoid.Semigroup0();
  const semigroupEffectFn81 = {
    append: f1 => f2 => (a, b, c, d, e, f, g, h) => {
      const a$p = f1(a, b, c, d, e, f, g, h);
      const a$p$1 = f2(a, b, c, d, e, f, g, h);
      return $1.append(a$p)(a$p$1);
    }
  };
  return {mempty: (v, v1, v2, v3, v4, v5, v6, v7) => $0, Semigroup0: () => semigroupEffectFn81};
};
const monoidEffectFn7 = dictMonoid => {
  const $0 = dictMonoid.mempty;
  const $1 = dictMonoid.Semigroup0();
  const semigroupEffectFn71 = {
    append: f1 => f2 => (a, b, c, d, e, f, g) => {
      const a$p = f1(a, b, c, d, e, f, g);
      const a$p$1 = f2(a, b, c, d, e, f, g);
      return $1.append(a$p)(a$p$1);
    }
  };
  return {mempty: (v, v1, v2, v3, v4, v5, v6) => $0, Semigroup0: () => semigroupEffectFn71};
};
const monoidEffectFn6 = dictMonoid => {
  const $0 = dictMonoid.mempty;
  const $1 = dictMonoid.Semigroup0();
  const semigroupEffectFn61 = {
    append: f1 => f2 => (a, b, c, d, e, f) => {
      const a$p = f1(a, b, c, d, e, f);
      const a$p$1 = f2(a, b, c, d, e, f);
      return $1.append(a$p)(a$p$1);
    }
  };
  return {mempty: (v, v1, v2, v3, v4, v5) => $0, Semigroup0: () => semigroupEffectFn61};
};
const monoidEffectFn5 = dictMonoid => {
  const $0 = dictMonoid.mempty;
  const $1 = dictMonoid.Semigroup0();
  const semigroupEffectFn51 = {
    append: f1 => f2 => (a, b, c, d, e) => {
      const a$p = f1(a, b, c, d, e);
      const a$p$1 = f2(a, b, c, d, e);
      return $1.append(a$p)(a$p$1);
    }
  };
  return {mempty: (v, v1, v2, v3, v4) => $0, Semigroup0: () => semigroupEffectFn51};
};
const monoidEffectFn4 = dictMonoid => {
  const $0 = dictMonoid.mempty;
  const $1 = dictMonoid.Semigroup0();
  const semigroupEffectFn41 = {
    append: f1 => f2 => (a, b, c, d) => {
      const a$p = f1(a, b, c, d);
      const a$p$1 = f2(a, b, c, d);
      return $1.append(a$p)(a$p$1);
    }
  };
  return {mempty: (v, v1, v2, v3) => $0, Semigroup0: () => semigroupEffectFn41};
};
const monoidEffectFn3 = dictMonoid => {
  const $0 = dictMonoid.mempty;
  const $1 = dictMonoid.Semigroup0();
  const semigroupEffectFn31 = {
    append: f1 => f2 => (a, b, c) => {
      const a$p = f1(a, b, c);
      const a$p$1 = f2(a, b, c);
      return $1.append(a$p)(a$p$1);
    }
  };
  return {mempty: (v, v1, v2) => $0, Semigroup0: () => semigroupEffectFn31};
};
const monoidEffectFn2 = dictMonoid => {
  const $0 = dictMonoid.mempty;
  const $1 = dictMonoid.Semigroup0();
  const semigroupEffectFn21 = {
    append: f1 => f2 => (a, b) => {
      const a$p = f1(a, b);
      const a$p$1 = f2(a, b);
      return $1.append(a$p)(a$p$1);
    }
  };
  return {mempty: (v, v1) => $0, Semigroup0: () => semigroupEffectFn21};
};
const monoidEffectFn10 = dictMonoid => {
  const $0 = dictMonoid.mempty;
  const $1 = dictMonoid.Semigroup0();
  const semigroupEffectFn101 = {
    append: f1 => f2 => (a, b, c, d, e, f, g, h, i, j) => {
      const a$p = f1(a, b, c, d, e, f, g, h, i, j);
      const a$p$1 = f2(a, b, c, d, e, f, g, h, i, j);
      return $1.append(a$p)(a$p$1);
    }
  };
  return {mempty: (v, v1, v2, v3, v4, v5, v6, v7, v8, v9) => $0, Semigroup0: () => semigroupEffectFn101};
};
const monoidEffectFn1 = dictMonoid => {
  const $0 = dictMonoid.mempty;
  const $1 = dictMonoid.Semigroup0();
  const semigroupEffectFn11 = {
    append: f1 => f2 => a => {
      const a$p = f1(a);
      const a$p$1 = f2(a);
      return $1.append(a$p)(a$p$1);
    }
  };
  return {mempty: v => $0, Semigroup0: () => semigroupEffectFn11};
};
export {
  monoidEffectFn1,
  monoidEffectFn10,
  monoidEffectFn2,
  monoidEffectFn3,
  monoidEffectFn4,
  monoidEffectFn5,
  monoidEffectFn6,
  monoidEffectFn7,
  monoidEffectFn8,
  monoidEffectFn9,
  semigroupEffectFn1,
  semigroupEffectFn10,
  semigroupEffectFn2,
  semigroupEffectFn3,
  semigroupEffectFn4,
  semigroupEffectFn5,
  semigroupEffectFn6,
  semigroupEffectFn7,
  semigroupEffectFn8,
  semigroupEffectFn9
};
export * from "./foreign.js";
