const Op = x => x;
const semigroupoidOp = {compose: v => v1 => x => v1(v(x))};
const semigroupOp = dictSemigroup => ({append: f => g => x => dictSemigroup.append(f(x))(g(x))});
const newtypeOp = {Coercible0: () => {}};
const monoidOp = dictMonoid => {
  const mempty1 = dictMonoid.mempty;
  const $0 = dictMonoid.Semigroup0();
  const semigroupFn = {append: f => g => x => $0.append(f(x))(g(x))};
  return {mempty: v => mempty1, Semigroup0: () => semigroupFn};
};
const contravariantOp = {cmap: f => v => x => v(f(x))};
const categoryOp = {identity: x => x, Semigroupoid0: () => semigroupoidOp};
export {Op, categoryOp, contravariantOp, monoidOp, newtypeOp, semigroupOp, semigroupoidOp};
