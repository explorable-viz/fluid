import * as Control$dBiapply from "../Control.Biapply/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const bipure = dict => dict.bipure;
const biapplicativeTuple = {bipure: Data$dTuple.Tuple, Biapply0: () => Control$dBiapply.biapplyTuple};
export {biapplicativeTuple, bipure};
