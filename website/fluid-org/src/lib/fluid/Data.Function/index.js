const on = f => g => x => y => f(g(x))(g(y));
const flip = f => b => a => f(a)(b);
const $$const = a => v => a;
const applyN = f => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const n = go$a0, acc = go$a1;
      if (n <= 0) {
        go$c = false;
        go$r = acc;
        continue;
      }
      go$a0 = n - 1 | 0;
      go$a1 = f(acc);
    }
    return go$r;
  };
  return go;
};
const applyFlipped = x => f => f(x);
const apply = f => x => f(x);
export {apply, applyFlipped,  $$const as const,  };
