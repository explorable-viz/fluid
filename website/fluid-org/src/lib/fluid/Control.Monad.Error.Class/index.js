// | This module defines the `MonadError` type class and its instances.
import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
const throwError = dict => dict.throwError;
const monadThrowMaybe = {throwError: v => Data$dMaybe.Nothing, Monad0: () => Data$dMaybe.monadMaybe};
const monadThrowEither = {throwError: Data$dEither.Left, Monad0: () => Data$dEither.monadEither};
const monadThrowEffect = {throwError: Effect$dException.throwException, Monad0: () => Effect.monadEffect};
const monadErrorMaybe = {
  catchError: v => v1 => {
    if (v.tag === "Nothing") { return v1(); }
    if (v.tag === "Just") { return Data$dMaybe.$Maybe("Just", v._1); }
    $runtime.fail();
  },
  MonadThrow0: () => monadThrowMaybe
};
const monadErrorEither = {
  catchError: v => v1 => {
    if (v.tag === "Left") { return v1(v._1); }
    if (v.tag === "Right") { return Data$dEither.$Either("Right", v._1); }
    $runtime.fail();
  },
  MonadThrow0: () => monadThrowEither
};
const monadErrorEffect = {catchError: b => a => Effect$dException.catchException(a)(b), MonadThrow0: () => monadThrowEffect};
const liftMaybe = dictMonadThrow => {
  const pure = dictMonadThrow.Monad0().Applicative0().pure;
  return error => {
    const $0 = dictMonadThrow.throwError(error);
    return v2 => {
      if (v2.tag === "Nothing") { return $0; }
      if (v2.tag === "Just") { return pure(v2._1); }
      $runtime.fail();
    };
  };
};
const liftEither = dictMonadThrow => {
  const $0 = dictMonadThrow.Monad0().Applicative0().pure;
  return v2 => {
    if (v2.tag === "Left") { return dictMonadThrow.throwError(v2._1); }
    if (v2.tag === "Right") { return $0(v2._1); }
    $runtime.fail();
  };
};
const catchError = dict => dict.catchError;
const catchJust = dictMonadError => p => act => handler => dictMonadError.catchError(act)(e => {
  const v = p(e);
  if (v.tag === "Nothing") { return dictMonadError.MonadThrow0().throwError(e); }
  if (v.tag === "Just") { return handler(v._1); }
  $runtime.fail();
});
const $$try = dictMonadError => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  return a => dictMonadError.catchError(Monad0.Bind1().Apply0().Functor0().map(Data$dEither.Right)(a))(x => Monad0.Applicative0().pure(Data$dEither.$Either("Left", x)));
};
const withResource = dictMonadError => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  const Bind1 = Monad0.Bind1();
  const try1 = $$try(dictMonadError);
  return acquire => release => kleisli => Bind1.bind(acquire)(resource => Bind1.bind(try1(kleisli(resource)))(result => Bind1.bind(release(resource))(() => {
    if (result.tag === "Left") { return MonadThrow0.throwError(result._1); }
    if (result.tag === "Right") { return Monad0.Applicative0().pure(result._1); }
    $runtime.fail();
  })));
};
export {
  
  
  
  
  
  
  
  monadThrowEffect,
  
  
  
  $$try as try,
  
};
