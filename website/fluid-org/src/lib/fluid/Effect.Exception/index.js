// | This module defines an effect, actions and handlers for working
// | with JavaScript exceptions.
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import {catchException, error, message, name, showErrorImpl, stackImpl, throwException} from "./foreign.js";
const $$try = action => catchException(x => () => Data$dEither.$Either("Left", x))(() => {
  const a$p = action();
  return Data$dEither.$Either("Right", a$p);
});
const $$throw = x => throwException(error(x));
const stack = /* #__PURE__ */ stackImpl(Data$dMaybe.Just)(Data$dMaybe.Nothing);
const showError = {show: showErrorImpl};
;
export * from "./foreign.js";
