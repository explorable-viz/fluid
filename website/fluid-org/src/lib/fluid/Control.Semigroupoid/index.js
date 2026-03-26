const semigroupoidFn = {compose: f => g => x => f(g(x))};
const compose = dict => dict.compose;
const composeFlipped = dictSemigroupoid => f => g => dictSemigroupoid.compose(g)(f);
export {  semigroupoidFn};
