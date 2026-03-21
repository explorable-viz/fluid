export const peekImpl = function (i, xs) {
  return xs[i];
};

export const pokeImpl = function (i, a, xs) {
  xs[i] = a;
};
