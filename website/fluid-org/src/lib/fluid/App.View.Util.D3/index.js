import * as $runtime from "../runtime.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dShow$dGeneric from "../Data.Show.Generic/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import {
  attrs,
  bandwidth,
  classed,
  colorScale,
  createChild,
  createText,
  datum,
  dimensions,
  isEmpty,
  line,
  on,
  remove,
  rootSelect,
  scaleBand,
  scaleLinear,
  select,
  selectAll,
  setDatum,
  setText,
  styles,
  textDimensions,
  xAxis,
  yAxis
} from "./foreign.js";
const $ElementType = tag => tag;
const fromFoldable = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dFoldable.foldableArray);
const Caption = /* #__PURE__ */ $ElementType("Caption");
const Circle = /* #__PURE__ */ $ElementType("Circle");
const Div = /* #__PURE__ */ $ElementType("Div");
const G = /* #__PURE__ */ $ElementType("G");
const Line = /* #__PURE__ */ $ElementType("Line");
const Path = /* #__PURE__ */ $ElementType("Path");
const Rect = /* #__PURE__ */ $ElementType("Rect");
const Span = /* #__PURE__ */ $ElementType("Span");
const SVG = /* #__PURE__ */ $ElementType("SVG");
const Table = /* #__PURE__ */ $ElementType("Table");
const Text = /* #__PURE__ */ $ElementType("Text");
const TBody = /* #__PURE__ */ $ElementType("TBody");
const TD = /* #__PURE__ */ $ElementType("TD");
const TH = /* #__PURE__ */ $ElementType("TH");
const THead = /* #__PURE__ */ $ElementType("THead");
const TR = /* #__PURE__ */ $ElementType("TR");
const Pattern = /* #__PURE__ */ $ElementType("Pattern");
const genericElementType_ = {
  to: x => {
    if (x.tag === "Inl") { return Caption; }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return Circle; }
      if (x._1.tag === "Inr") {
        if (x._1._1.tag === "Inl") { return Div; }
        if (x._1._1.tag === "Inr") {
          if (x._1._1._1.tag === "Inl") { return G; }
          if (x._1._1._1.tag === "Inr") {
            if (x._1._1._1._1.tag === "Inl") { return Line; }
            if (x._1._1._1._1.tag === "Inr") {
              if (x._1._1._1._1._1.tag === "Inl") { return Path; }
              if (x._1._1._1._1._1.tag === "Inr") {
                if (x._1._1._1._1._1._1.tag === "Inl") { return Rect; }
                if (x._1._1._1._1._1._1.tag === "Inr") {
                  if (x._1._1._1._1._1._1._1.tag === "Inl") { return Span; }
                  if (x._1._1._1._1._1._1._1.tag === "Inr") {
                    if (x._1._1._1._1._1._1._1._1.tag === "Inl") { return SVG; }
                    if (x._1._1._1._1._1._1._1._1.tag === "Inr") {
                      if (x._1._1._1._1._1._1._1._1._1.tag === "Inl") { return Table; }
                      if (x._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                        if (x._1._1._1._1._1._1._1._1._1._1.tag === "Inl") { return Text; }
                        if (x._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                          if (x._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") { return TBody; }
                          if (x._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                            if (x._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") { return TD; }
                            if (x._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                              if (x._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") { return TH; }
                              if (x._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") { return THead; }
                                if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                  if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") { return TR; }
                                  if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") { return Pattern; }
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
            }
          }
        }
      }
    }
    $runtime.fail();
  },
  from: x => {
    if (x === "Caption") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (x === "Circle") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (x === "Div") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))); }
    if (x === "G") {
      return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))));
    }
    if (x === "Line") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))))
      );
    }
    if (x === "Path") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))))
        )
      );
    }
    if (x === "Rect") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))))
          )
        )
      );
    }
    if (x === "Span") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))))
            )
          )
        )
      );
    }
    if (x === "SVG") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))))
              )
            )
          )
        )
      );
    }
    if (x === "Table") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))))
                )
              )
            )
          )
        )
      );
    }
    if (x === "Text") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))))
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x === "TBody") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))))
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x === "TD") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x === "TH") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x === "THead") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum(
                              "Inr",
                              Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x === "TR") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum(
                              "Inr",
                              Data$dGeneric$dRep.$Sum(
                                "Inr",
                                Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x === "Pattern") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum(
                              "Inr",
                              Data$dGeneric$dRep.$Sum(
                                "Inr",
                                Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments)))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    $runtime.fail();
  }
};
const showElementType = {
  show: /* #__PURE__ */ (() => {
    const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Caption"});
    const $1 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Circle"});
    const $2 = (() => {
      const $2 = (() => {
        const $2 = (() => {
          const $2 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Div"});
          const $3 = (() => {
            const $3 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "G"});
            const $4 = (() => {
              const $4 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Line"});
              const $5 = (() => {
                const $5 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Path"});
                const $6 = (() => {
                  const $6 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Rect"});
                  const $7 = (() => {
                    const $7 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Span"});
                    const $8 = (() => {
                      const $8 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "SVG"});
                      const $9 = (() => {
                        const $9 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Table"});
                        const $10 = (() => {
                          const $10 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Text"});
                          const $11 = (() => {
                            const $11 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "TBody"});
                            const $12 = (() => {
                              const $12 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "TD"});
                              const $13 = (() => {
                                const $13 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "TH"});
                                const $14 = (() => {
                                  const $14 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "THead"});
                                  const $15 = (() => {
                                    const $15 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "TR"});
                                    const $16 = (() => {
                                      const $16 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Pattern"});
                                      return {
                                        "genericShow'": v => {
                                          if (v.tag === "Inl") { return $15["genericShow'"](v._1); }
                                          if (v.tag === "Inr") { return $16["genericShow'"](v._1); }
                                          $runtime.fail();
                                        }
                                      };
                                    })();
                                    return {
                                      "genericShow'": v => {
                                        if (v.tag === "Inl") { return $14["genericShow'"](v._1); }
                                        if (v.tag === "Inr") { return $16["genericShow'"](v._1); }
                                        $runtime.fail();
                                      }
                                    };
                                  })();
                                  return {
                                    "genericShow'": v => {
                                      if (v.tag === "Inl") { return $13["genericShow'"](v._1); }
                                      if (v.tag === "Inr") { return $15["genericShow'"](v._1); }
                                      $runtime.fail();
                                    }
                                  };
                                })();
                                return {
                                  "genericShow'": v => {
                                    if (v.tag === "Inl") { return $12["genericShow'"](v._1); }
                                    if (v.tag === "Inr") { return $14["genericShow'"](v._1); }
                                    $runtime.fail();
                                  }
                                };
                              })();
                              return {
                                "genericShow'": v => {
                                  if (v.tag === "Inl") { return $11["genericShow'"](v._1); }
                                  if (v.tag === "Inr") { return $13["genericShow'"](v._1); }
                                  $runtime.fail();
                                }
                              };
                            })();
                            return {
                              "genericShow'": v => {
                                if (v.tag === "Inl") { return $10["genericShow'"](v._1); }
                                if (v.tag === "Inr") { return $12["genericShow'"](v._1); }
                                $runtime.fail();
                              }
                            };
                          })();
                          return {
                            "genericShow'": v => {
                              if (v.tag === "Inl") { return $9["genericShow'"](v._1); }
                              if (v.tag === "Inr") { return $11["genericShow'"](v._1); }
                              $runtime.fail();
                            }
                          };
                        })();
                        return {
                          "genericShow'": v => {
                            if (v.tag === "Inl") { return $8["genericShow'"](v._1); }
                            if (v.tag === "Inr") { return $10["genericShow'"](v._1); }
                            $runtime.fail();
                          }
                        };
                      })();
                      return {
                        "genericShow'": v => {
                          if (v.tag === "Inl") { return $7["genericShow'"](v._1); }
                          if (v.tag === "Inr") { return $9["genericShow'"](v._1); }
                          $runtime.fail();
                        }
                      };
                    })();
                    return {
                      "genericShow'": v => {
                        if (v.tag === "Inl") { return $6["genericShow'"](v._1); }
                        if (v.tag === "Inr") { return $8["genericShow'"](v._1); }
                        $runtime.fail();
                      }
                    };
                  })();
                  return {
                    "genericShow'": v => {
                      if (v.tag === "Inl") { return $5["genericShow'"](v._1); }
                      if (v.tag === "Inr") { return $7["genericShow'"](v._1); }
                      $runtime.fail();
                    }
                  };
                })();
                return {
                  "genericShow'": v => {
                    if (v.tag === "Inl") { return $4["genericShow'"](v._1); }
                    if (v.tag === "Inr") { return $6["genericShow'"](v._1); }
                    $runtime.fail();
                  }
                };
              })();
              return {
                "genericShow'": v => {
                  if (v.tag === "Inl") { return $3["genericShow'"](v._1); }
                  if (v.tag === "Inr") { return $5["genericShow'"](v._1); }
                  $runtime.fail();
                }
              };
            })();
            return {
              "genericShow'": v => {
                if (v.tag === "Inl") { return $2["genericShow'"](v._1); }
                if (v.tag === "Inr") { return $4["genericShow'"](v._1); }
                $runtime.fail();
              }
            };
          })();
          return {
            "genericShow'": v => {
              if (v.tag === "Inl") { return $1["genericShow'"](v._1); }
              if (v.tag === "Inr") { return $3["genericShow'"](v._1); }
              $runtime.fail();
            }
          };
        })();
        return {
          "genericShow'": v => {
            if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
            if (v.tag === "Inr") { return $2["genericShow'"](v._1); }
            $runtime.fail();
          }
        };
      })();
      return x => $2["genericShow'"](genericElementType_.from(x));
    })();
    return x => Data$dString$dCommon.toLower($2(x));
  })()
};
const translate = v => Data$dTuple.$Tuple("transform", "translate(" + Data$dShow.showIntImpl(v.x) + ", " + Data$dShow.showIntImpl(v.y) + ")");
const textWidth = class_ => {
  const $0 = textDimensions(class_);
  return x => $0(x).width;
};
const textHeight = class_ => {
  const $0 = textDimensions(class_);
  return x => $0(x).height;
};
const setStyles = as => sel => styles(sel)(fromFoldable(as));
const setAttrs = as => sel => attrs(sel)(fromFoldable(as));
const scope = ":scope";
const rotate$p = f => Data$dTuple.$Tuple("transform", a => "rotate(" + Data$dShow.showIntImpl(f(a)) + ")");
const rotate = n => Data$dTuple.$Tuple("transform", "rotate(" + Data$dShow.showIntImpl(n) + ")");
const nthChildOf = selector => i => selector + " > :nth-child(" + Data$dShow.showIntImpl(i) + ")";
const create = elementType => as => parent => createChild(parent)(showElementType.show(elementType))(fromFoldable(as));
const addHatchPattern = parent$p => j => col_j => {
  const $0 = createChild(parent$p)(showElementType.show(Pattern))(fromFoldable([
    Data$dTuple.$Tuple("id", "diagonalHatch-" + Data$dShow.showIntImpl(j)),
    Data$dTuple.$Tuple("patternUnits", "userSpaceOnUse"),
    Data$dTuple.$Tuple("width", "2"),
    Data$dTuple.$Tuple("height", "2"),
    Data$dTuple.$Tuple("patternTransform", "rotate(45)")
  ]));
  return () => {
    const pattern = $0();
    createChild(pattern)(showElementType.show(Rect))(fromFoldable([Data$dTuple.$Tuple("width", "3.5"), Data$dTuple.$Tuple("height", "3.5"), Data$dTuple.$Tuple("fill", col_j)]))();
    createChild(pattern)(showElementType.show(Line))(fromFoldable([
      Data$dTuple.$Tuple("x1", "0"),
      Data$dTuple.$Tuple("y", "0"),
      Data$dTuple.$Tuple("x2", "0"),
      Data$dTuple.$Tuple("y2", "3.5"),
      Data$dTuple.$Tuple("stroke", "rgba(255, 255, 255, 1)"),
      Data$dTuple.$Tuple("stroke-width", "1")
    ]))();
  };
};
export {
  
  
  Circle,
  Div,
  G,
  
  Path,
  
  Rect,
  SVG,
  Span,
  TBody,
  TD,
  TH,
  THead,
  TR,
  Table,
  Text,
  addHatchPattern,
  
  fromFoldable,
  
  
  
  
  
  
  
  showElementType,
  
  
  translate
};
export * from "./foreign.js";
