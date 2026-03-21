// | Tuples that are not restricted to two elements.
// |
// | Here is an example of a 3-tuple:
// |
// | 
// | ```purescript
// | > tuple = tuple3 1 "2" 3.0
// | > tuple
// | (Tuple 1 (Tuple "2" (Tuple 3.0 unit)))
// | ```
// | 
// | Notice that a tuple is a nested structure not unlike a list. The type of `tuple` is this:
// | 
// | ```purescript
// | > :t tuple
// | Tuple Int (Tuple String (Tuple Number Unit))
// | ```
// | 
// | That, however, can be abbreviated with the `Tuple3` type:
// | 
// | ```purescript
// | Tuple3 Int String Number
// | ```
// |
// | All tuple functions are numbered from 1 to 10. That is, there's
// | a `get1` and a `get10`.
// |
// | The `getN` functions accept tuples of length N or greater:
// | 
// | ```purescript
// | get1 tuple = 1
// | get3 tuple = 3
// | get4 tuple -- type error. `get4` requires a longer tuple. 
// | ```
// | 
// | The same is true of the `overN` functions:
// | 
// | ```purescript
// | over2 negate (tuple3 1 2 3) = tuple3 1 (-2) 3
// | ```
// |
// | `uncurryN` can be used to convert a function that takes `N` arguments to one that takes an N-tuple:
// | 
// | ```purescript
// | uncurry2 (+) (tuple2 1 2) = 3
// | ```
// | 
// | The reverse `curryN` function converts functions that take
// | N-tuples (which are rare) to functions that take `N` arguments.
// |
// | ---------------
// | In addition to types like `Tuple3`, there are also types like
// | `T3`. Whereas `Tuple3` describes a tuple with exactly three
// | elements, `T3` describes a tuple of length *two or longer*. More
// | specifically, `T3` requires two element plus a "tail" that may be
// | `unit` or more tuple elements. Use types like `T3` when you want to
// | create a set of functions for arbitrary tuples. See the source for how that's done.
// | 
import * as Data$dTuple from "../Data.Tuple/index.js";
const uncurry9 = f$p => v => f$p(v._1)(v._2._1)(v._2._2._1)(v._2._2._2._1)(v._2._2._2._2._1)(v._2._2._2._2._2._1)(v._2._2._2._2._2._2._1)(v._2._2._2._2._2._2._2._1)(v._2._2._2._2._2._2._2._2._1);
const uncurry8 = f$p => v => f$p(v._1)(v._2._1)(v._2._2._1)(v._2._2._2._1)(v._2._2._2._2._1)(v._2._2._2._2._2._1)(v._2._2._2._2._2._2._1)(v._2._2._2._2._2._2._2._1);
const uncurry7 = f$p => v => f$p(v._1)(v._2._1)(v._2._2._1)(v._2._2._2._1)(v._2._2._2._2._1)(v._2._2._2._2._2._1)(v._2._2._2._2._2._2._1);
const uncurry6 = f$p => v => f$p(v._1)(v._2._1)(v._2._2._1)(v._2._2._2._1)(v._2._2._2._2._1)(v._2._2._2._2._2._1);
const uncurry5 = f => v => f(v._1)(v._2._1)(v._2._2._1)(v._2._2._2._1)(v._2._2._2._2._1);
const uncurry4 = f => v => f(v._1)(v._2._1)(v._2._2._1)(v._2._2._2._1);
const uncurry3 = f => v => f(v._1)(v._2._1)(v._2._2._1);
const uncurry2 = f => v => f(v._1)(v._2._1);
const uncurry10 = f$p => v => f$p(v._1)(v._2._1)(v._2._2._1)(v._2._2._2._1)(v._2._2._2._2._1)(v._2._2._2._2._2._1)(v._2._2._2._2._2._2._1)(v._2._2._2._2._2._2._2._1)(v._2._2._2._2._2._2._2._2._1)(v._2._2._2._2._2._2._2._2._2._1);
const uncurry1 = f => v => f(v._1);
const tuple9 = a => b => c => d => e => f => g => h => i => Data$dTuple.$Tuple(
  a,
  Data$dTuple.$Tuple(
    b,
    Data$dTuple.$Tuple(c, Data$dTuple.$Tuple(d, Data$dTuple.$Tuple(e, Data$dTuple.$Tuple(f, Data$dTuple.$Tuple(g, Data$dTuple.$Tuple(h, Data$dTuple.$Tuple(i, undefined)))))))
  )
);
const tuple8 = a => b => c => d => e => f => g => h => Data$dTuple.$Tuple(
  a,
  Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(c, Data$dTuple.$Tuple(d, Data$dTuple.$Tuple(e, Data$dTuple.$Tuple(f, Data$dTuple.$Tuple(g, Data$dTuple.$Tuple(h, undefined)))))))
);
const tuple7 = a => b => c => d => e => f => g => Data$dTuple.$Tuple(
  a,
  Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(c, Data$dTuple.$Tuple(d, Data$dTuple.$Tuple(e, Data$dTuple.$Tuple(f, Data$dTuple.$Tuple(g, undefined))))))
);
const tuple6 = a => b => c => d => e => f => Data$dTuple.$Tuple(
  a,
  Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(c, Data$dTuple.$Tuple(d, Data$dTuple.$Tuple(e, Data$dTuple.$Tuple(f, undefined)))))
);
const tuple5 = a => b => c => d => e => Data$dTuple.$Tuple(a, Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(c, Data$dTuple.$Tuple(d, Data$dTuple.$Tuple(e, undefined)))));
const tuple4 = a => b => c => d => Data$dTuple.$Tuple(a, Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(c, Data$dTuple.$Tuple(d, undefined))));
const tuple3 = a => b => c => Data$dTuple.$Tuple(a, Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(c, undefined)));
const tuple2 = a => b => Data$dTuple.$Tuple(a, Data$dTuple.$Tuple(b, undefined));
const tuple10 = a => b => c => d => e => f => g => h => i => j => Data$dTuple.$Tuple(
  a,
  Data$dTuple.$Tuple(
    b,
    Data$dTuple.$Tuple(
      c,
      Data$dTuple.$Tuple(d, Data$dTuple.$Tuple(e, Data$dTuple.$Tuple(f, Data$dTuple.$Tuple(g, Data$dTuple.$Tuple(h, Data$dTuple.$Tuple(i, Data$dTuple.$Tuple(j, undefined)))))))
    )
  )
);
const tuple1 = a => Data$dTuple.$Tuple(a, undefined);
const over9 = o => v => Data$dTuple.$Tuple(
  v._1,
  Data$dTuple.$Tuple(
    v._2._1,
    Data$dTuple.$Tuple(
      v._2._2._1,
      Data$dTuple.$Tuple(
        v._2._2._2._1,
        Data$dTuple.$Tuple(
          v._2._2._2._2._1,
          Data$dTuple.$Tuple(
            v._2._2._2._2._2._1,
            Data$dTuple.$Tuple(
              v._2._2._2._2._2._2._1,
              Data$dTuple.$Tuple(v._2._2._2._2._2._2._2._1, Data$dTuple.$Tuple(o(v._2._2._2._2._2._2._2._2._1), v._2._2._2._2._2._2._2._2._2))
            )
          )
        )
      )
    )
  )
);
const over8 = o => v => Data$dTuple.$Tuple(
  v._1,
  Data$dTuple.$Tuple(
    v._2._1,
    Data$dTuple.$Tuple(
      v._2._2._1,
      Data$dTuple.$Tuple(
        v._2._2._2._1,
        Data$dTuple.$Tuple(
          v._2._2._2._2._1,
          Data$dTuple.$Tuple(v._2._2._2._2._2._1, Data$dTuple.$Tuple(v._2._2._2._2._2._2._1, Data$dTuple.$Tuple(o(v._2._2._2._2._2._2._2._1), v._2._2._2._2._2._2._2._2)))
        )
      )
    )
  )
);
const over7 = o => v => Data$dTuple.$Tuple(
  v._1,
  Data$dTuple.$Tuple(
    v._2._1,
    Data$dTuple.$Tuple(
      v._2._2._1,
      Data$dTuple.$Tuple(
        v._2._2._2._1,
        Data$dTuple.$Tuple(v._2._2._2._2._1, Data$dTuple.$Tuple(v._2._2._2._2._2._1, Data$dTuple.$Tuple(o(v._2._2._2._2._2._2._1), v._2._2._2._2._2._2._2)))
      )
    )
  )
);
const over6 = o => v => Data$dTuple.$Tuple(
  v._1,
  Data$dTuple.$Tuple(
    v._2._1,
    Data$dTuple.$Tuple(v._2._2._1, Data$dTuple.$Tuple(v._2._2._2._1, Data$dTuple.$Tuple(v._2._2._2._2._1, Data$dTuple.$Tuple(o(v._2._2._2._2._2._1), v._2._2._2._2._2._2))))
  )
);
const over5 = o => v => Data$dTuple.$Tuple(
  v._1,
  Data$dTuple.$Tuple(v._2._1, Data$dTuple.$Tuple(v._2._2._1, Data$dTuple.$Tuple(v._2._2._2._1, Data$dTuple.$Tuple(o(v._2._2._2._2._1), v._2._2._2._2._2))))
);
const over4 = o => v => Data$dTuple.$Tuple(v._1, Data$dTuple.$Tuple(v._2._1, Data$dTuple.$Tuple(v._2._2._1, Data$dTuple.$Tuple(o(v._2._2._2._1), v._2._2._2._2))));
const over3 = o => v => Data$dTuple.$Tuple(v._1, Data$dTuple.$Tuple(v._2._1, Data$dTuple.$Tuple(o(v._2._2._1), v._2._2._2)));
const over2 = o => v => Data$dTuple.$Tuple(v._1, Data$dTuple.$Tuple(o(v._2._1), v._2._2));
const over10 = o => v => Data$dTuple.$Tuple(
  v._1,
  Data$dTuple.$Tuple(
    v._2._1,
    Data$dTuple.$Tuple(
      v._2._2._1,
      Data$dTuple.$Tuple(
        v._2._2._2._1,
        Data$dTuple.$Tuple(
          v._2._2._2._2._1,
          Data$dTuple.$Tuple(
            v._2._2._2._2._2._1,
            Data$dTuple.$Tuple(
              v._2._2._2._2._2._2._1,
              Data$dTuple.$Tuple(
                v._2._2._2._2._2._2._2._1,
                Data$dTuple.$Tuple(v._2._2._2._2._2._2._2._2._1, Data$dTuple.$Tuple(o(v._2._2._2._2._2._2._2._2._2._1), v._2._2._2._2._2._2._2._2._2._2))
              )
            )
          )
        )
      )
    )
  )
);
const over1 = o => v => Data$dTuple.$Tuple(o(v._1), v._2);
const get9 = v => v._2._2._2._2._2._2._2._2._1;
const get8 = v => v._2._2._2._2._2._2._2._1;
const get7 = v => v._2._2._2._2._2._2._1;
const get6 = v => v._2._2._2._2._2._1;
const get5 = v => v._2._2._2._2._1;
const get4 = v => v._2._2._2._1;
const get3 = v => v._2._2._1;
const get2 = v => v._2._1;
const get10 = v => v._2._2._2._2._2._2._2._2._2._1;
const get1 = v => v._1;
const curry9 = z => f$p => a => b => c => d => e => f => g => h => i => f$p(Data$dTuple.$Tuple(
  a,
  Data$dTuple.$Tuple(
    b,
    Data$dTuple.$Tuple(c, Data$dTuple.$Tuple(d, Data$dTuple.$Tuple(e, Data$dTuple.$Tuple(f, Data$dTuple.$Tuple(g, Data$dTuple.$Tuple(h, Data$dTuple.$Tuple(i, z)))))))
  )
));
const curry8 = z => f$p => a => b => c => d => e => f => g => h => f$p(Data$dTuple.$Tuple(
  a,
  Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(c, Data$dTuple.$Tuple(d, Data$dTuple.$Tuple(e, Data$dTuple.$Tuple(f, Data$dTuple.$Tuple(g, Data$dTuple.$Tuple(h, z)))))))
));
const curry7 = z => f$p => a => b => c => d => e => f => g => f$p(Data$dTuple.$Tuple(
  a,
  Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(c, Data$dTuple.$Tuple(d, Data$dTuple.$Tuple(e, Data$dTuple.$Tuple(f, Data$dTuple.$Tuple(g, z))))))
));
const curry6 = z => f$p => a => b => c => d => e => f => f$p(Data$dTuple.$Tuple(
  a,
  Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(c, Data$dTuple.$Tuple(d, Data$dTuple.$Tuple(e, Data$dTuple.$Tuple(f, z)))))
));
const curry5 = z => f => a => b => c => d => e => f(Data$dTuple.$Tuple(a, Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(c, Data$dTuple.$Tuple(d, Data$dTuple.$Tuple(e, z))))));
const curry4 = z => f => a => b => c => d => f(Data$dTuple.$Tuple(a, Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(c, Data$dTuple.$Tuple(d, z)))));
const curry3 = z => f => a => b => c => f(Data$dTuple.$Tuple(a, Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(c, z))));
const curry2 = z => f => a => b => f(Data$dTuple.$Tuple(a, Data$dTuple.$Tuple(b, z)));
const curry10 = z => f$p => a => b => c => d => e => f => g => h => i => j => f$p(Data$dTuple.$Tuple(
  a,
  Data$dTuple.$Tuple(
    b,
    Data$dTuple.$Tuple(
      c,
      Data$dTuple.$Tuple(d, Data$dTuple.$Tuple(e, Data$dTuple.$Tuple(f, Data$dTuple.$Tuple(g, Data$dTuple.$Tuple(h, Data$dTuple.$Tuple(i, Data$dTuple.$Tuple(j, z)))))))
    )
  )
));
const curry1 = z => f => a => f(Data$dTuple.$Tuple(a, z));
export {
  curry1,
  curry10,
  curry2,
  curry3,
  curry4,
  curry5,
  curry6,
  curry7,
  curry8,
  curry9,
  get1,
  get10,
  get2,
  get3,
  get4,
  get5,
  get6,
  get7,
  get8,
  get9,
  over1,
  over10,
  over2,
  over3,
  over4,
  over5,
  over6,
  over7,
  over8,
  over9,
  tuple1,
  tuple10,
  tuple2,
  tuple3,
  tuple4,
  tuple5,
  tuple6,
  tuple7,
  tuple8,
  tuple9,
  uncurry1,
  uncurry10,
  uncurry2,
  uncurry3,
  uncurry4,
  uncurry5,
  uncurry6,
  uncurry7,
  uncurry8,
  uncurry9
};
