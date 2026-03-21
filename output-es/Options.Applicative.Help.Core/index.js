import * as $runtime from "../runtime.js";
import * as Control$dBind from "../Control.Bind/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dArray$dNonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dFunctorWithIndex from "../Data.FunctorWithIndex/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dMonoid from "../Data.Monoid/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Options$dApplicative$dCommon from "../Options.Applicative.Common/index.js";
import * as Options$dApplicative$dHelp$dChunk from "../Options.Applicative.Help.Chunk/index.js";
import * as Options$dApplicative$dHelp$dTypes from "../Options.Applicative.Help.Types/index.js";
import * as Options$dApplicative$dTypes from "../Options.Applicative.Types/index.js";
import * as Text$dPrettyPrint$dLeijen from "../Text.PrettyPrint.Leijen/index.js";
const fold = /* #__PURE__ */ (() => Data$dFoldable.foldableArray.foldMap(Data$dMonoid.monoidArray)(Data$dFoldable.identity))();
const chunkMonoid = /* #__PURE__ */ (() => {
  const chunkSemigroup1 = {
    append: v1 => v2 => {
      if (v1.tag === "Nothing") { return v2; }
      if (v2.tag === "Nothing") { return v1; }
      if (v1.tag === "Just" && v2.tag === "Just") { return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", v1._1, v2._1)); }
      $runtime.fail();
    }
  };
  return {mempty: Data$dMaybe.Nothing, Semigroup0: () => chunkSemigroup1};
})();
const listToChunk = /* #__PURE__ */ Options$dApplicative$dHelp$dChunk.listToChunk(Text$dPrettyPrint$dLeijen.docMonoid);
const identity = x => x;
const usageHelp = chunk => (
  {
    helpUsage: chunk,
    helpBody: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpBody,
    helpError: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpError,
    helpFooter: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpFooter,
    helpHeader: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpHeader,
    helpSuggestions: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpSuggestions
  }
);
const suggestionsHelp = chunk => (
  {
    helpSuggestions: chunk,
    helpBody: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpBody,
    helpError: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpError,
    helpFooter: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpFooter,
    helpHeader: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpHeader,
    helpUsage: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpUsage
  }
);
const intersperse = sep => {
  const $0 = Data$dFunctorWithIndex.mapWithIndexArray(idx => e => {
    if (idx === 0) { return [e]; }
    return [sep, e];
  });
  return x => fold($0(x));
};
const optDesc = pprefs => style => info => opt => {
  const suffix = info.hinfoMulti ? Options$dApplicative$dHelp$dChunk.stringChunk(pprefs.prefMultiSuffix) : chunkMonoid.mempty;
  const descs = Data$dFunctor.arrayMap(x => Text$dPrettyPrint$dLeijen.string(Options$dApplicative$dCommon.showOption(x)))(Data$dArray.sortBy(Options$dApplicative$dTypes.optNameOrd.compare)((() => {
    if (opt.optMain.tag === "OptReader") { return opt.optMain._1; }
    if (opt.optMain.tag === "FlagReader") { return opt.optMain._1; }
    return [];
  })()));
  return (() => {
    if (opt.optProps.propDescMod.tag === "Nothing") { return identity; }
    if (opt.optProps.propDescMod.tag === "Just") {
      return v1 => {
        if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", opt.optProps.propDescMod._1(v1._1)); }
        return Data$dMaybe.Nothing;
      };
    }
    $runtime.fail();
  })()((() => {
    const $0 = listToChunk(intersperse(style.descSep)(descs));
    const $1 = Options$dApplicative$dHelp$dChunk.stringChunk(opt.optProps.propMetaVar);
    const $2 = (() => {
      if ($0.tag === "Nothing") { return $1; }
      if ($1.tag === "Nothing") { return $0; }
      if ($0.tag === "Just" && $1.tag === "Just") {
        return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", $0._1, Text$dPrettyPrint$dLeijen.$Doc("Cat", Text$dPrettyPrint$dLeijen.$Doc("Char", " "), $1._1)));
      }
      $runtime.fail();
    })();
    if (
      (() => {
        if (info.hinfoDefault && !style.descOptional) { return true; }
        if (opt.optProps.propVisibility === "Hidden") { return !style.descHidden; }
        return opt.optProps.propVisibility !== "Visible";
      })()
    ) {
      return chunkMonoid.mempty;
    }
    if (
      (() => {
        if ($2.tag === "Nothing") { return true; }
        if ($2.tag === "Just") { return false; }
        $runtime.fail();
      })() || !style.descSurround
    ) {
      if ($2.tag === "Nothing") { return suffix; }
      if (suffix.tag === "Nothing") { return $2; }
      if ($2.tag === "Just" && suffix.tag === "Just") { return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", $2._1, suffix._1)); }
      $runtime.fail();
    }
    if (info.hinfoDefault) {
      const $3 = $2.tag === "Just"
        ? Data$dMaybe.$Maybe(
            "Just",
            Text$dPrettyPrint$dLeijen.$Doc(
              "Cat",
              Text$dPrettyPrint$dLeijen.$Doc("Char", "["),
              Text$dPrettyPrint$dLeijen.$Doc("Cat", $2._1, Text$dPrettyPrint$dLeijen.$Doc("Char", "]"))
            )
          )
        : Data$dMaybe.Nothing;
      if ($3.tag === "Nothing") { return suffix; }
      if (suffix.tag === "Nothing") { return $3; }
      if ($3.tag === "Just" && suffix.tag === "Just") { return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", $3._1, suffix._1)); }
      $runtime.fail();
    }
    if (Data$dArray.sliceImpl(1, descs.length, descs).length === 0) {
      if ($2.tag === "Nothing") { return suffix; }
      if (suffix.tag === "Nothing") { return $2; }
      if ($2.tag === "Just" && suffix.tag === "Just") { return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", $2._1, suffix._1)); }
      $runtime.fail();
    }
    const $3 = $2.tag === "Just"
      ? Data$dMaybe.$Maybe(
          "Just",
          Text$dPrettyPrint$dLeijen.$Doc(
            "Cat",
            Text$dPrettyPrint$dLeijen.$Doc("Char", "("),
            Text$dPrettyPrint$dLeijen.$Doc("Cat", $2._1, Text$dPrettyPrint$dLeijen.$Doc("Char", ")"))
          )
        )
      : Data$dMaybe.Nothing;
    if ($3.tag === "Nothing") { return suffix; }
    if (suffix.tag === "Nothing") { return $3; }
    if ($3.tag === "Just" && suffix.tag === "Just") { return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", $3._1, suffix._1)); }
    $runtime.fail();
  })());
};
const headerHelp = chunk => (
  {
    helpHeader: chunk,
    helpBody: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpBody,
    helpError: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpError,
    helpFooter: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpFooter,
    helpSuggestions: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpSuggestions,
    helpUsage: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpUsage
  }
);
const fullDesc = pprefs => {
  const style = {descSep: Text$dPrettyPrint$dLeijen.string(","), descHidden: true, descOptional: true, descSurround: false};
  const $0 = Options$dApplicative$dCommon.mapParser(info => opt => {
    const n = optDesc(pprefs)(style)(info)(opt);
    if (opt.optProps.propShowDefault.tag === "Just") {
      if (
        (() => {
          if (n.tag === "Nothing") { return false; }
          if (n.tag === "Just") { return true; }
          $runtime.fail();
        })() && (() => {
          if (opt.optProps.propHelp.tag === "Nothing") { return false; }
          if (opt.optProps.propHelp.tag === "Just") { return true; }
          $runtime.fail();
        })()
      ) {
        return Data$dMaybe.$Maybe(
          "Just",
          Data$dTuple.$Tuple(
            (() => {
              if (n.tag === "Nothing") { return Text$dPrettyPrint$dLeijen.Empty; }
              if (n.tag === "Just") { return n._1; }
              $runtime.fail();
            })(),
            (() => {
              if (opt.optProps.propHelp.tag === "Nothing") {
                const $0 = Text$dPrettyPrint$dLeijen.$Doc(
                  "Cat",
                  Text$dPrettyPrint$dLeijen.$Doc("Char", "("),
                  Text$dPrettyPrint$dLeijen.$Doc(
                    "Cat",
                    Text$dPrettyPrint$dLeijen.$Doc(
                      "Cat",
                      Text$dPrettyPrint$dLeijen.string("default:"),
                      Text$dPrettyPrint$dLeijen.$Doc("Cat", Text$dPrettyPrint$dLeijen.$Doc("Char", " "), Text$dPrettyPrint$dLeijen.string(opt.optProps.propShowDefault._1))
                    ),
                    Text$dPrettyPrint$dLeijen.$Doc("Char", ")")
                  )
                );
                return Text$dPrettyPrint$dLeijen.$Doc("Column", k => Text$dPrettyPrint$dLeijen.$Doc("Nesting", i => Text$dPrettyPrint$dLeijen.$Doc("Nest", k - i | 0, $0)));
              }
              if (opt.optProps.propHelp.tag === "Just") {
                const $0 = Text$dPrettyPrint$dLeijen.$Doc(
                  "Cat",
                  opt.optProps.propHelp._1,
                  Text$dPrettyPrint$dLeijen.$Doc(
                    "Cat",
                    Text$dPrettyPrint$dLeijen.$Doc("Char", " "),
                    Text$dPrettyPrint$dLeijen.$Doc(
                      "Cat",
                      Text$dPrettyPrint$dLeijen.$Doc("Char", "("),
                      Text$dPrettyPrint$dLeijen.$Doc(
                        "Cat",
                        Text$dPrettyPrint$dLeijen.$Doc(
                          "Cat",
                          Text$dPrettyPrint$dLeijen.string("default:"),
                          Text$dPrettyPrint$dLeijen.$Doc("Cat", Text$dPrettyPrint$dLeijen.$Doc("Char", " "), Text$dPrettyPrint$dLeijen.string(opt.optProps.propShowDefault._1))
                        ),
                        Text$dPrettyPrint$dLeijen.$Doc("Char", ")")
                      )
                    )
                  )
                );
                return Text$dPrettyPrint$dLeijen.$Doc("Column", k => Text$dPrettyPrint$dLeijen.$Doc("Nesting", i => Text$dPrettyPrint$dLeijen.$Doc("Nest", k - i | 0, $0)));
              }
              $runtime.fail();
            })()
          )
        );
      }
      return Data$dMaybe.Nothing;
    }
    if (
      (() => {
        if (n.tag === "Nothing") { return false; }
        if (n.tag === "Just") { return true; }
        $runtime.fail();
      })() && (() => {
        if (opt.optProps.propHelp.tag === "Nothing") { return false; }
        if (opt.optProps.propHelp.tag === "Just") { return true; }
        $runtime.fail();
      })()
    ) {
      return Data$dMaybe.$Maybe(
        "Just",
        Data$dTuple.$Tuple(
          (() => {
            if (n.tag === "Nothing") { return Text$dPrettyPrint$dLeijen.Empty; }
            if (n.tag === "Just") { return n._1; }
            $runtime.fail();
          })(),
          (() => {
            if (opt.optProps.propHelp.tag === "Nothing") {
              return Text$dPrettyPrint$dLeijen.$Doc(
                "Column",
                k => Text$dPrettyPrint$dLeijen.$Doc("Nesting", i => Text$dPrettyPrint$dLeijen.$Doc("Nest", k - i | 0, Text$dPrettyPrint$dLeijen.Empty))
              );
            }
            const $0 = (() => {
              if (opt.optProps.propHelp.tag === "Nothing") { return Text$dPrettyPrint$dLeijen.Empty; }
              if (opt.optProps.propHelp.tag === "Just") { return opt.optProps.propHelp._1; }
              $runtime.fail();
            })();
            return Text$dPrettyPrint$dLeijen.$Doc("Column", k => Text$dPrettyPrint$dLeijen.$Doc("Nesting", i => Text$dPrettyPrint$dLeijen.$Doc("Nest", k - i | 0, $0)));
          })()
        )
      );
    }
    return Data$dMaybe.Nothing;
  });
  return x => Options$dApplicative$dHelp$dChunk.tabulate$p(24)(Data$dArray.mapMaybe(x$1 => x$1)($0(x)));
};
const footerHelp = chunk => (
  {
    helpFooter: chunk,
    helpBody: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpBody,
    helpError: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpError,
    helpHeader: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpHeader,
    helpSuggestions: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpSuggestions,
    helpUsage: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpUsage
  }
);
const fold_tree = v => {
  if (v.tag === "Leaf") { return v._1; }
  if (v.tag === "MultNode") { return Data$dFoldable.foldrArray(x => Options$dApplicative$dHelp$dChunk.chunkBesideOrBelow(fold_tree(x)))(chunkMonoid.mempty)(v._1); }
  if (v.tag === "AltNode") {
    const $0 = Data$dArray.filterImpl(
      x => {
        if (x.tag === "Nothing") { return false; }
        if (x.tag === "Just") { return true; }
        $runtime.fail();
      },
      Data$dFunctor.arrayMap(fold_tree)(v._1)
    );
    if ($0.length === 1) { return $0[0]; }
    const $1 = Data$dFoldable.foldrArray(v1 => v2 => {
      if (v1.tag === "Nothing") { return v2; }
      if (v2.tag === "Nothing") { return v1; }
      if (v1.tag === "Just" && v2.tag === "Just") {
        return Data$dMaybe.$Maybe(
          "Just",
          Text$dPrettyPrint$dLeijen.$Doc(
            "Cat",
            v1._1,
            Text$dPrettyPrint$dLeijen.$Doc(
              "Cat",
              Text$dPrettyPrint$dLeijen.softline,
              Text$dPrettyPrint$dLeijen.$Doc("Cat", Text$dPrettyPrint$dLeijen.$Doc("Char", "|"), Text$dPrettyPrint$dLeijen.$Doc("Cat", Text$dPrettyPrint$dLeijen.softline, v2._1))
            )
          )
        );
      }
      $runtime.fail();
    })(chunkMonoid.mempty)($0);
    if ($1.tag === "Just") {
      return Data$dMaybe.$Maybe(
        "Just",
        Text$dPrettyPrint$dLeijen.$Doc(
          "Cat",
          Text$dPrettyPrint$dLeijen.$Doc("Char", "("),
          Text$dPrettyPrint$dLeijen.$Doc("Cat", $1._1, Text$dPrettyPrint$dLeijen.$Doc("Char", ")"))
        )
      );
    }
    return Data$dMaybe.Nothing;
  }
  $runtime.fail();
};
const errorHelp = chunk => (
  {
    helpError: chunk,
    helpBody: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpBody,
    helpFooter: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpFooter,
    helpHeader: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpHeader,
    helpSuggestions: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpSuggestions,
    helpUsage: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpUsage
  }
);
const cmdDesc = /* #__PURE__ */ Options$dApplicative$dCommon.mapParser(v => opt => {
  if (opt.optMain.tag === "CmdReader") {
    return Data$dTuple.$Tuple(
      opt.optMain._1,
      Options$dApplicative$dHelp$dChunk.tabulate$p(24)(Control$dBind.arrayBind(Data$dArray.reverse(opt.optMain._2))(cmd => Control$dBind.arrayBind((() => {
        const $0 = opt.optMain._3(cmd);
        if ($0.tag === "Just") { return [$0._1.infoProgDesc]; }
        return [];
      })())(d => [
        Data$dTuple.$Tuple(
          Text$dPrettyPrint$dLeijen.string(cmd),
          (() => {
            const $0 = (() => {
              if (d.tag === "Nothing") { return Text$dPrettyPrint$dLeijen.Empty; }
              if (d.tag === "Just") { return d._1; }
              $runtime.fail();
            })();
            return Text$dPrettyPrint$dLeijen.$Doc("Column", k => Text$dPrettyPrint$dLeijen.$Doc("Nesting", i => Text$dPrettyPrint$dLeijen.$Doc("Nest", k - i | 0, $0)));
          })()
        )
      ])))
    );
  }
  return Data$dTuple.$Tuple(Data$dMaybe.Nothing, chunkMonoid.mempty);
});
const briefDesc$p = showOptional => pprefs => {
  const $0 = Options$dApplicative$dCommon.treeMapParser(optDesc(pprefs)({
    descSep: Text$dPrettyPrint$dLeijen.string("|"),
    descHidden: false,
    descOptional: showOptional,
    descSurround: true
  }));
  return x => fold_tree($0(x));
};
const missingDesc = /* #__PURE__ */ briefDesc$p(false);
const briefDesc = /* #__PURE__ */ briefDesc$p(true);
const parserUsage = pprefs => p => progn => Text$dPrettyPrint$dLeijen.hsep([
  Text$dPrettyPrint$dLeijen.string("Usage:"),
  Text$dPrettyPrint$dLeijen.string(progn),
  (() => {
    const $0 = briefDesc$p(true)(pprefs)(p);
    const $1 = (() => {
      if ($0.tag === "Nothing") { return Text$dPrettyPrint$dLeijen.Empty; }
      if ($0.tag === "Just") { return $0._1; }
      $runtime.fail();
    })();
    return Text$dPrettyPrint$dLeijen.$Doc("Column", k => Text$dPrettyPrint$dLeijen.$Doc("Nesting", i => Text$dPrettyPrint$dLeijen.$Doc("Nest", k - i | 0, $1)));
  })()
]);
const bodyHelp = chunk => (
  {
    helpBody: chunk,
    helpError: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpError,
    helpFooter: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpFooter,
    helpHeader: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpHeader,
    helpSuggestions: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpSuggestions,
    helpUsage: Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty.helpUsage
  }
);
const parserHelp = pprefs => p => bodyHelp(Options$dApplicative$dHelp$dChunk.vsepChunks([
  (() => {
    const $0 = fullDesc(pprefs)(p);
    if ($0.tag === "Just") {
      return Data$dMaybe.$Maybe(
        "Just",
        Text$dPrettyPrint$dLeijen.$Doc(
          "Cat",
          Text$dPrettyPrint$dLeijen.string("Available options:"),
          Text$dPrettyPrint$dLeijen.$Doc("Cat", Text$dPrettyPrint$dLeijen.$Doc("FlatAlt", Text$dPrettyPrint$dLeijen.Line, Text$dPrettyPrint$dLeijen.$Doc("Char", " ")), $0._1)
        )
      );
    }
    return Data$dMaybe.Nothing;
  })(),
  ...Data$dFunctor.arrayMap(arr => {
    const v = Data$dArray$dNonEmpty.uncons(arr);
    const $0 = (() => {
      if (v.head._1.tag === "Nothing") { return "Available commands:"; }
      if (v.head._1.tag === "Just") { return v.head._1._1; }
      $runtime.fail();
    })();
    const $1 = Options$dApplicative$dHelp$dChunk.vcatChunks([v.head._2, ...Data$dFunctor.arrayMap(Data$dTuple.snd)(v.tail)]);
    if ($1.tag === "Just") {
      return Data$dMaybe.$Maybe(
        "Just",
        Text$dPrettyPrint$dLeijen.$Doc(
          "Cat",
          Text$dPrettyPrint$dLeijen.string($0),
          Text$dPrettyPrint$dLeijen.$Doc("Cat", Text$dPrettyPrint$dLeijen.$Doc("FlatAlt", Text$dPrettyPrint$dLeijen.Line, Text$dPrettyPrint$dLeijen.$Doc("Char", " ")), $1._1)
        )
      );
    }
    return Data$dMaybe.Nothing;
  })(Data$dArray.groupBy(x => y => {
    if (x._1.tag === "Nothing") { return y._1.tag === "Nothing"; }
    return x._1.tag === "Just" && y._1.tag === "Just" && x._1._1 === y._1._1;
  })(cmdDesc(p)))
]));
export {
  bodyHelp,
  briefDesc,
  briefDesc$p,
  chunkMonoid,
  cmdDesc,
  errorHelp,
  fold,
  fold_tree,
  footerHelp,
  fullDesc,
  headerHelp,
  identity,
  intersperse,
  listToChunk,
  missingDesc,
  optDesc,
  parserHelp,
  parserUsage,
  suggestionsHelp,
  usageHelp
};
