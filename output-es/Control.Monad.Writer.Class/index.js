// | This module defines the `MonadWriter` type class and its instances.
import * as Data$dTuple from "../Data.Tuple/index.js";
const tell = dict => dict.tell;
const pass = dict => dict.pass;
const listen = dict => dict.listen;
const listens = dictMonadWriter => {
  const Monad1 = dictMonadWriter.MonadTell1().Monad1();
  return f => m => Monad1.Bind1().bind(dictMonadWriter.listen(m))(v => Monad1.Applicative0().pure(Data$dTuple.$Tuple(v._1, f(v._2))));
};
const censor = dictMonadWriter => {
  const Monad1 = dictMonadWriter.MonadTell1().Monad1();
  return f => m => dictMonadWriter.pass(Monad1.Bind1().bind(m)(a => Monad1.Applicative0().pure(Data$dTuple.$Tuple(a, f))));
};
export {censor, listen, listens, pass, tell};
