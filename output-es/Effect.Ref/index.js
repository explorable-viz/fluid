// | This module defines the `Ref` type for mutable value references, as well
// | as actions for working with them.
// |
// | You'll notice that all of the functions that operate on a `Ref` (e.g.
// | `new`, `read`, `write`) return their result wrapped in an `Effect`.
// | Working with mutable references is considered effectful in PureScript
// | because of the principle of purity: functions should not have side
// | effects, and should return the same result when called with the same
// | arguments. If a `Ref` could be written to without using `Effect`, that
// | would cause a side effect (the effect of changing the result of subsequent
// | reads for that `Ref`). If there were a function for reading the current
// | value of a `Ref` without the result being wrapped in `Effect`, the result
// | of calling that function would change each time a new value was written to
// | the `Ref`. Even creating a new `Ref` is effectful: if there were a
// | function for creating a new `Ref` with the type `forall s. s -> Ref s`,
// | then calling that function twice with the same argument would not give the
// | same result in each case, since you'd end up with two distinct references
// | which could be updated independently of each other.
// |
// | _Note_: `Control.Monad.ST` provides a pure alternative to `Ref` when
// | mutation is restricted to a local scope.
import {_new, modifyImpl, newWithSelf, read, write} from "./foreign.js";
const $$new = _new;
const modify$p = modifyImpl;
const modify = f => modifyImpl(s => {
  const s$p = f(s);
  return {state: s$p, value: s$p};
});
const modify_ = f => s => () => {
  const $0 = s.value;
  s.value = f($0);
};
export {modify, modify$p, modify_, $$new as new};
export * from "./foreign.js";
