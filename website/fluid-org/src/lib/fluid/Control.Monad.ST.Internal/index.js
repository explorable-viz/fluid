import * as $runtime from "../runtime.js";
import {for as $$for, new as $$new, while as $$while, bind_, foreach, map_, modifyImpl, pure_, read, run, write} from "./foreign.js";
const modify$p = modifyImpl;
const modify = f => modifyImpl(s => {
  const s$p = f(s);
  return {state: s$p, value: s$p};
});
const functorST = {map: map_};
const monadST = {Applicative0: () => applicativeST, Bind1: () => bindST};
const bindST = {bind: bind_, Apply0: () => applyST};
const applyST = {
  apply: f => a => () => {
    const f$p = f();
    const a$p = a();
    return applicativeST.pure(f$p(a$p))();
  },
  Functor0: () => functorST
};
const applicativeST = {pure: pure_, Apply0: () => applyST};
const semigroupST = dictSemigroup => (
  {
    append: a => b => () => {
      const $0 = a();
      const a$p = b();
      return dictSemigroup.append($0)(a$p);
    }
  }
);
const monadRecST = {
  tailRecM: f => a => {
    const $0 = f(a);
    return () => {
      const $1 = $0();
      let r = $1;
      while (
        (() => {
          const $2 = r;
          return $2.tag === "Loop";
        })()
      ) {
        const v = r;
        if (v.tag === "Loop") {
          const e = f(v._1)();
          r = e;
          continue;
        }
        if (v.tag === "Done") { continue; }
        $runtime.fail();
      }
      const $2 = r;
      if ($2.tag === "Done") { return $2._1; }
      $runtime.fail();
    };
  },
  Monad0: () => monadST
};
const monoidST = dictMonoid => {
  const $0 = dictMonoid.Semigroup0();
  const semigroupST1 = {
    append: a => b => () => {
      const $1 = a();
      const a$p = b();
      return $0.append($1)(a$p);
    }
  };
  return {
    mempty: (() => {
      const $1 = dictMonoid.mempty;
      return () => $1;
    })(),
    Semigroup0: () => semigroupST1
  };
};
export {applicativeST,      monadRecST, monadST,  };
export * from "./foreign.js";
