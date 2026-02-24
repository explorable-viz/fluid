import * as Data$dEither from "../Data.Either/index.js";
import {_jsonParser} from "./foreign.js";
const jsonParser = j => _jsonParser(Data$dEither.Left, Data$dEither.Right, j);
;
export * from "./foreign.js";
