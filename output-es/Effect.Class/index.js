import * as Effect from "../Effect/index.js";
const monadEffectEffect = {liftEffect: x => x, Monad0: () => Effect.monadEffect};
const liftEffect = dict => dict.liftEffect;
export {liftEffect, monadEffectEffect};
