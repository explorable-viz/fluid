// | This module defines the `Reader` monad.
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
const withReader = Control$dMonad$dReader$dTrans.withReaderT;
const runReader = v => x => v(x);
const mapReader = f => v => x => f(v(x));
export {mapReader, runReader, withReader};
