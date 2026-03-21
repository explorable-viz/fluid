const refl = {proof: a => a, Coercible0: () => {}};
const proof = dict => dict.proof;
const to = dictTypeEquals => dictTypeEquals.proof(a => a);
const from = dictTypeEquals => dictTypeEquals.proof(a => a);
export {from, proof, refl, to};
