const StateR = x => x;
const StateL = x => x;
const stateR = v => v;
const stateL = v => v;
const functorStateR = {
  map: f => k => s => {
    const v = k(s);
    return {accum: v.accum, value: f(v.value)};
  }
};
const functorStateL = {
  map: f => k => s => {
    const v = k(s);
    return {accum: v.accum, value: f(v.value)};
  }
};
const applyStateR = {
  apply: f => x => s => {
    const v = x(s);
    const v1 = f(v.accum);
    return {accum: v1.accum, value: v1.value(v.value)};
  },
  Functor0: () => functorStateR
};
const applyStateL = {
  apply: f => x => s => {
    const v = f(s);
    const v1 = x(v.accum);
    return {accum: v1.accum, value: v.value(v1.value)};
  },
  Functor0: () => functorStateL
};
const applicativeStateR = {pure: a => s => ({accum: s, value: a}), Apply0: () => applyStateR};
const applicativeStateL = {pure: a => s => ({accum: s, value: a}), Apply0: () => applyStateL};
export {  applicativeStateL, applicativeStateR,      };
