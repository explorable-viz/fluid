import * as Data$dVoid from "../Data.Void/index.js";
const contravariantConst = {cmap: v => v1 => v1};
const cmap = dict => dict.cmap;
const cmapFlipped = dictContravariant => x => f => dictContravariant.cmap(f)(x);
const coerce = dictContravariant => dictFunctor => a => dictFunctor.map(Data$dVoid.absurd)(dictContravariant.cmap(Data$dVoid.absurd)(a));
const imapC = dictContravariant => v => f => dictContravariant.cmap(f);
export {cmap, cmapFlipped, coerce, contravariantConst, imapC};
