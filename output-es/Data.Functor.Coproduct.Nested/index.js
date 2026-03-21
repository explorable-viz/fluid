import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFunctor$dCoproduct from "../Data.Functor.Coproduct/index.js";
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
const in1 = Data$dFunctor$dCoproduct.left;
const coproduct9 = a => b => c => d => e => f => g => h => i => y => {
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
const coproduct8 = a => b => c => d => e => f => g => h => y => {
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
const coproduct7 = a => b => c => d => e => f => g => y => {
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
const coproduct6 = a => b => c => d => e => f => y => {
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
const coproduct5 = a => b => c => d => e => y => {
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
const coproduct4 = a => b => c => d => y => {
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
const coproduct3 = a => b => c => y => {
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
const coproduct2 = a => b => y => {
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
const coproduct10 = a => b => c => d => e => f => g => h => i => j => y => {
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
const coproduct1 = y => {
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
  coproduct1,
  coproduct10,
  coproduct2,
  coproduct3,
  coproduct4,
  coproduct5,
  coproduct6,
  coproduct7,
  coproduct8,
  coproduct9,
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
