// | Utilities for n-eithers: sums types with more than two terms built from nested eithers.
// |
// | Nested eithers arise naturally in sum combinators. You shouldn't
// | represent sum data using nested eithers, but if combinators you're working with
// | create them, utilities in this module will allow to to more easily work
// | with them, including translating to and from more traditional sum types.
// |
// | ```purescript
// | data Color = Red Number | Green Number | Blue Number
// |
// | fromEither3 :: Either3 Number Number Number -> Color
// | fromEither3 = either3 Red Green Blue
// |
// | toEither3 :: Color -> Either3 Number Number Number
// | toEither3 (Red   v) = in1 v
// | toEither3 (Green v) = in2 v
// | toEither3 (Blue  v) = in3 v
// | ```
import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
const in9 = v => Data$dEither.$Either(
  "Right",
  Data$dEither.$Either(
    "Right",
    Data$dEither.$Either(
      "Right",
      Data$dEither.$Either(
        "Right",
        Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Left", v)))))
      )
    )
  )
);
const in8 = v => Data$dEither.$Either(
  "Right",
  Data$dEither.$Either(
    "Right",
    Data$dEither.$Either(
      "Right",
      Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Left", v)))))
    )
  )
);
const in7 = v => Data$dEither.$Either(
  "Right",
  Data$dEither.$Either(
    "Right",
    Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Left", v)))))
  )
);
const in6 = v => Data$dEither.$Either(
  "Right",
  Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Left", v)))))
);
const in5 = v => Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Left", v)))));
const in4 = v => Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Left", v))));
const in3 = v => Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Left", v)));
const in2 = v => Data$dEither.$Either("Right", Data$dEither.$Either("Left", v));
const in10 = v => Data$dEither.$Either(
  "Right",
  Data$dEither.$Either(
    "Right",
    Data$dEither.$Either(
      "Right",
      Data$dEither.$Either(
        "Right",
        Data$dEither.$Either(
          "Right",
          Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Right", Data$dEither.$Either("Left", v)))))
        )
      )
    )
  )
);
const in1 = Data$dEither.Left;
const either9 = a => b => c => d => e => f => g => h => i => y => {
  if (y.tag === "Left") { return a(y._1); }
  if (y.tag === "Right") {
    if (y._1.tag === "Left") { return b(y._1._1); }
    if (y._1.tag === "Right") {
      if (y._1._1.tag === "Left") { return c(y._1._1._1); }
      if (y._1._1.tag === "Right") {
        if (y._1._1._1.tag === "Left") { return d(y._1._1._1._1); }
        if (y._1._1._1.tag === "Right") {
          if (y._1._1._1._1.tag === "Left") { return e(y._1._1._1._1._1); }
          if (y._1._1._1._1.tag === "Right") {
            if (y._1._1._1._1._1.tag === "Left") { return f(y._1._1._1._1._1._1); }
            if (y._1._1._1._1._1.tag === "Right") {
              if (y._1._1._1._1._1._1.tag === "Left") { return g(y._1._1._1._1._1._1._1); }
              if (y._1._1._1._1._1._1.tag === "Right") {
                if (y._1._1._1._1._1._1._1.tag === "Left") { return h(y._1._1._1._1._1._1._1._1); }
                if (y._1._1._1._1._1._1._1.tag === "Right") {
                  if (y._1._1._1._1._1._1._1._1.tag === "Left") { return i(y._1._1._1._1._1._1._1._1._1); }
                  if (y._1._1._1._1._1._1._1._1.tag === "Right") {
                    const spin = spin$a0$copy => {
                      let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
                      while (spin$c) {
                        const v = spin$a0;
                        spin$a0 = v;
                      }
                      return spin$r;
                    };
                    return spin(y._1._1._1._1._1._1._1._1._1);
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  $runtime.fail();
};
const either8 = a => b => c => d => e => f => g => h => y => {
  if (y.tag === "Left") { return a(y._1); }
  if (y.tag === "Right") {
    if (y._1.tag === "Left") { return b(y._1._1); }
    if (y._1.tag === "Right") {
      if (y._1._1.tag === "Left") { return c(y._1._1._1); }
      if (y._1._1.tag === "Right") {
        if (y._1._1._1.tag === "Left") { return d(y._1._1._1._1); }
        if (y._1._1._1.tag === "Right") {
          if (y._1._1._1._1.tag === "Left") { return e(y._1._1._1._1._1); }
          if (y._1._1._1._1.tag === "Right") {
            if (y._1._1._1._1._1.tag === "Left") { return f(y._1._1._1._1._1._1); }
            if (y._1._1._1._1._1.tag === "Right") {
              if (y._1._1._1._1._1._1.tag === "Left") { return g(y._1._1._1._1._1._1._1); }
              if (y._1._1._1._1._1._1.tag === "Right") {
                if (y._1._1._1._1._1._1._1.tag === "Left") { return h(y._1._1._1._1._1._1._1._1); }
                if (y._1._1._1._1._1._1._1.tag === "Right") {
                  const spin = spin$a0$copy => {
                    let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
                    while (spin$c) {
                      const v = spin$a0;
                      spin$a0 = v;
                    }
                    return spin$r;
                  };
                  return spin(y._1._1._1._1._1._1._1._1);
                }
              }
            }
          }
        }
      }
    }
  }
  $runtime.fail();
};
const either7 = a => b => c => d => e => f => g => y => {
  if (y.tag === "Left") { return a(y._1); }
  if (y.tag === "Right") {
    if (y._1.tag === "Left") { return b(y._1._1); }
    if (y._1.tag === "Right") {
      if (y._1._1.tag === "Left") { return c(y._1._1._1); }
      if (y._1._1.tag === "Right") {
        if (y._1._1._1.tag === "Left") { return d(y._1._1._1._1); }
        if (y._1._1._1.tag === "Right") {
          if (y._1._1._1._1.tag === "Left") { return e(y._1._1._1._1._1); }
          if (y._1._1._1._1.tag === "Right") {
            if (y._1._1._1._1._1.tag === "Left") { return f(y._1._1._1._1._1._1); }
            if (y._1._1._1._1._1.tag === "Right") {
              if (y._1._1._1._1._1._1.tag === "Left") { return g(y._1._1._1._1._1._1._1); }
              if (y._1._1._1._1._1._1.tag === "Right") {
                const spin = spin$a0$copy => {
                  let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
                  while (spin$c) {
                    const v = spin$a0;
                    spin$a0 = v;
                  }
                  return spin$r;
                };
                return spin(y._1._1._1._1._1._1._1);
              }
            }
          }
        }
      }
    }
  }
  $runtime.fail();
};
const either6 = a => b => c => d => e => f => y => {
  if (y.tag === "Left") { return a(y._1); }
  if (y.tag === "Right") {
    if (y._1.tag === "Left") { return b(y._1._1); }
    if (y._1.tag === "Right") {
      if (y._1._1.tag === "Left") { return c(y._1._1._1); }
      if (y._1._1.tag === "Right") {
        if (y._1._1._1.tag === "Left") { return d(y._1._1._1._1); }
        if (y._1._1._1.tag === "Right") {
          if (y._1._1._1._1.tag === "Left") { return e(y._1._1._1._1._1); }
          if (y._1._1._1._1.tag === "Right") {
            if (y._1._1._1._1._1.tag === "Left") { return f(y._1._1._1._1._1._1); }
            if (y._1._1._1._1._1.tag === "Right") {
              const spin = spin$a0$copy => {
                let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
                while (spin$c) {
                  const v = spin$a0;
                  spin$a0 = v;
                }
                return spin$r;
              };
              return spin(y._1._1._1._1._1._1);
            }
          }
        }
      }
    }
  }
  $runtime.fail();
};
const either5 = a => b => c => d => e => y => {
  if (y.tag === "Left") { return a(y._1); }
  if (y.tag === "Right") {
    if (y._1.tag === "Left") { return b(y._1._1); }
    if (y._1.tag === "Right") {
      if (y._1._1.tag === "Left") { return c(y._1._1._1); }
      if (y._1._1.tag === "Right") {
        if (y._1._1._1.tag === "Left") { return d(y._1._1._1._1); }
        if (y._1._1._1.tag === "Right") {
          if (y._1._1._1._1.tag === "Left") { return e(y._1._1._1._1._1); }
          if (y._1._1._1._1.tag === "Right") {
            const spin = spin$a0$copy => {
              let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
              while (spin$c) {
                const v = spin$a0;
                spin$a0 = v;
              }
              return spin$r;
            };
            return spin(y._1._1._1._1._1);
          }
        }
      }
    }
  }
  $runtime.fail();
};
const either4 = a => b => c => d => y => {
  if (y.tag === "Left") { return a(y._1); }
  if (y.tag === "Right") {
    if (y._1.tag === "Left") { return b(y._1._1); }
    if (y._1.tag === "Right") {
      if (y._1._1.tag === "Left") { return c(y._1._1._1); }
      if (y._1._1.tag === "Right") {
        if (y._1._1._1.tag === "Left") { return d(y._1._1._1._1); }
        if (y._1._1._1.tag === "Right") {
          const spin = spin$a0$copy => {
            let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
            while (spin$c) {
              const v = spin$a0;
              spin$a0 = v;
            }
            return spin$r;
          };
          return spin(y._1._1._1._1);
        }
      }
    }
  }
  $runtime.fail();
};
const either3 = a => b => c => y => {
  if (y.tag === "Left") { return a(y._1); }
  if (y.tag === "Right") {
    if (y._1.tag === "Left") { return b(y._1._1); }
    if (y._1.tag === "Right") {
      if (y._1._1.tag === "Left") { return c(y._1._1._1); }
      if (y._1._1.tag === "Right") {
        const spin = spin$a0$copy => {
          let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
          while (spin$c) {
            const v = spin$a0;
            spin$a0 = v;
          }
          return spin$r;
        };
        return spin(y._1._1._1);
      }
    }
  }
  $runtime.fail();
};
const either2 = a => b => y => {
  if (y.tag === "Left") { return a(y._1); }
  if (y.tag === "Right") {
    if (y._1.tag === "Left") { return b(y._1._1); }
    if (y._1.tag === "Right") {
      const spin = spin$a0$copy => {
        let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
        while (spin$c) {
          const v = spin$a0;
          spin$a0 = v;
        }
        return spin$r;
      };
      return spin(y._1._1);
    }
  }
  $runtime.fail();
};
const either10 = a => b => c => d => e => f => g => h => i => j => y => {
  if (y.tag === "Left") { return a(y._1); }
  if (y.tag === "Right") {
    if (y._1.tag === "Left") { return b(y._1._1); }
    if (y._1.tag === "Right") {
      if (y._1._1.tag === "Left") { return c(y._1._1._1); }
      if (y._1._1.tag === "Right") {
        if (y._1._1._1.tag === "Left") { return d(y._1._1._1._1); }
        if (y._1._1._1.tag === "Right") {
          if (y._1._1._1._1.tag === "Left") { return e(y._1._1._1._1._1); }
          if (y._1._1._1._1.tag === "Right") {
            if (y._1._1._1._1._1.tag === "Left") { return f(y._1._1._1._1._1._1); }
            if (y._1._1._1._1._1.tag === "Right") {
              if (y._1._1._1._1._1._1.tag === "Left") { return g(y._1._1._1._1._1._1._1); }
              if (y._1._1._1._1._1._1.tag === "Right") {
                if (y._1._1._1._1._1._1._1.tag === "Left") { return h(y._1._1._1._1._1._1._1._1); }
                if (y._1._1._1._1._1._1._1.tag === "Right") {
                  if (y._1._1._1._1._1._1._1._1.tag === "Left") { return i(y._1._1._1._1._1._1._1._1._1); }
                  if (y._1._1._1._1._1._1._1._1.tag === "Right") {
                    if (y._1._1._1._1._1._1._1._1._1.tag === "Left") { return j(y._1._1._1._1._1._1._1._1._1._1); }
                    if (y._1._1._1._1._1._1._1._1._1.tag === "Right") {
                      const spin = spin$a0$copy => {
                        let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
                        while (spin$c) {
                          const v = spin$a0;
                          spin$a0 = v;
                        }
                        return spin$r;
                      };
                      return spin(y._1._1._1._1._1._1._1._1._1._1);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  $runtime.fail();
};
const either1 = y => {
  if (y.tag === "Left") { return y._1; }
  if (y.tag === "Right") {
    const spin = spin$a0$copy => {
      let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
      while (spin$c) {
        const v = spin$a0;
        spin$a0 = v;
      }
      return spin$r;
    };
    return spin(y._1);
  }
  $runtime.fail();
};
const at9 = b => f => y => {
  if (
    y.tag === "Right" && y._1.tag === "Right" && y._1._1.tag === "Right" && y._1._1._1.tag === "Right" && y._1._1._1._1.tag === "Right" && y._1._1._1._1._1.tag === "Right" && y._1._1._1._1._1._1.tag === "Right" && y._1._1._1._1._1._1._1.tag === "Right" && y._1._1._1._1._1._1._1._1.tag === "Left"
  ) {
    return f(y._1._1._1._1._1._1._1._1._1);
  }
  return b;
};
const at8 = b => f => y => {
  if (
    y.tag === "Right" && y._1.tag === "Right" && y._1._1.tag === "Right" && y._1._1._1.tag === "Right" && y._1._1._1._1.tag === "Right" && y._1._1._1._1._1.tag === "Right" && y._1._1._1._1._1._1.tag === "Right" && y._1._1._1._1._1._1._1.tag === "Left"
  ) {
    return f(y._1._1._1._1._1._1._1._1);
  }
  return b;
};
const at7 = b => f => y => {
  if (
    y.tag === "Right" && y._1.tag === "Right" && y._1._1.tag === "Right" && y._1._1._1.tag === "Right" && y._1._1._1._1.tag === "Right" && y._1._1._1._1._1.tag === "Right" && y._1._1._1._1._1._1.tag === "Left"
  ) {
    return f(y._1._1._1._1._1._1._1);
  }
  return b;
};
const at6 = b => f => y => {
  if (y.tag === "Right" && y._1.tag === "Right" && y._1._1.tag === "Right" && y._1._1._1.tag === "Right" && y._1._1._1._1.tag === "Right" && y._1._1._1._1._1.tag === "Left") {
    return f(y._1._1._1._1._1._1);
  }
  return b;
};
const at5 = b => f => y => {
  if (y.tag === "Right" && y._1.tag === "Right" && y._1._1.tag === "Right" && y._1._1._1.tag === "Right" && y._1._1._1._1.tag === "Left") { return f(y._1._1._1._1._1); }
  return b;
};
const at4 = b => f => y => {
  if (y.tag === "Right" && y._1.tag === "Right" && y._1._1.tag === "Right" && y._1._1._1.tag === "Left") { return f(y._1._1._1._1); }
  return b;
};
const at3 = b => f => y => {
  if (y.tag === "Right" && y._1.tag === "Right" && y._1._1.tag === "Left") { return f(y._1._1._1); }
  return b;
};
const at2 = b => f => y => {
  if (y.tag === "Right" && y._1.tag === "Left") { return f(y._1._1); }
  return b;
};
const at10 = b => f => y => {
  if (
    y.tag === "Right" && y._1.tag === "Right" && y._1._1.tag === "Right" && y._1._1._1.tag === "Right" && y._1._1._1._1.tag === "Right" && y._1._1._1._1._1.tag === "Right" && y._1._1._1._1._1._1.tag === "Right" && y._1._1._1._1._1._1._1.tag === "Right" && y._1._1._1._1._1._1._1._1.tag === "Right" && y._1._1._1._1._1._1._1._1._1.tag === "Left"
  ) {
    return f(y._1._1._1._1._1._1._1._1._1._1);
  }
  return b;
};
const at1 = b => f => y => {
  if (y.tag === "Left") { return f(y._1); }
  return b;
};
export {
  at1,
  at10,
  at2,
  at3,
  at4,
  at5,
  at6,
  at7,
  at8,
  at9,
  either1,
  either10,
  either2,
  either3,
  either4,
  either5,
  either6,
  either7,
  either8,
  either9,
  in1,
  in10,
  in2,
  in3,
  in4,
  in5,
  in6,
  in7,
  in8,
  in9
};
