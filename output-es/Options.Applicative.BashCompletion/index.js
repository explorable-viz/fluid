// | You don't need to import this module to enable bash completion.
// |
// | See [the wiki](http://github.com/pcapriotti/optparse-applicative/wiki/Bash-Completion)
// | for more information on bash completion.
import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dArray$dNonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dMonoid from "../Data.Monoid/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect from "../Effect/index.js";
import * as Options$dApplicative$dBuilder from "../Options.Applicative.Builder/index.js";
import * as Options$dApplicative$dBuilder$dInternal from "../Options.Applicative.Builder.Internal/index.js";
import * as Options$dApplicative$dCommon from "../Options.Applicative.Common/index.js";
import * as Options$dApplicative$dInternal from "../Options.Applicative.Internal/index.js";
import * as Options$dApplicative$dInternal$dUtils from "../Options.Applicative.Internal.Utils/index.js";
import * as Options$dApplicative$dTypes from "../Options.Applicative.Types/index.js";
import * as Text$dPrettyPrint$dLeijen from "../Text.PrettyPrint.Leijen/index.js";
const $Richness = (tag, _1, _2) => ({tag, _1, _2});
const fromFoldable = /* #__PURE__ */ Data$dFoldable.foldrArray(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
const identity = x => x;
const fold = /* #__PURE__ */ (() => Data$dFoldable.foldableArray.foldMap(Data$dMonoid.monoidArray)(Data$dFoldable.identity))();
const sequence = /* #__PURE__ */ (() => Data$dTraversable.traversableArray.traverse(Effect.applicativeEffect)(Data$dTraversable.identity))();
const unLines = xs => Data$dFoldable.foldlArray(v => v1 => {
  if (v.init) { return {init: false, acc: v1}; }
  return {init: false, acc: v.acc + "\n" + v1};
})({init: true, acc: ""})(xs).acc;
const fromFoldable1 = $0 => Data$dArray.fromFoldableImpl(Data$dList$dTypes.foldableList.foldr, $0);
const Standard = /* #__PURE__ */ $Richness("Standard");
const Enriched = value0 => value1 => $Richness("Enriched", value0, value1);
const zshCompletionScript = prog => progn => {
  const $0 = [
    "#compdef " + progn,
    "",
    "local request",
    "local completions",
    "local word",
    "local index=$((CURRENT - 1))",
    "",
    "request=(--bash-completion-enriched --bash-completion-index $index)",
    "for arg in ${words[@]}; do",
    "  request=(${request[@]} --bash-completion-word $arg)",
    "done",
    "",
    "IFS=$'\\n' completions=($( " + prog + " \"${request[@]}\" ))",
    "",
    "for word in $completions; do",
    "  local -a parts",
    "",
    "  # Split the line at a tab if there is one.",
    "  IFS=$'\\t' parts=($( echo $word ))",
    "",
    "  if [[ -n $parts[2] ]]; then",
    "     if [[ $word[1] == \"-\" ]]; then",
    "       local desc=(\"$parts[1] ($parts[2])\")",
    "       compadd -d desc -- $parts[1]",
    "     else",
    "       local desc=($(print -f  \"%-019s -- %s\" $parts[1] $parts[2]))",
    "       compadd -l -d desc -- $parts[1]",
    "     fi",
    "  else",
    "    compadd -f -- $word",
    "  fi",
    "done"
  ];
  return () => $0;
};
const fishCompletionScript = prog => progn => {
  const $0 = [
    " function _" + progn,
    "    set -l cl (commandline --tokenize --current-process)",
    "    # Hack around fish issue #3934",
    "    set -l cn (commandline --tokenize --cut-at-cursor --current-process)",
    "    set -l cn (count $cn)",
    "    set -l tmpline --bash-completion-enriched --bash-completion-index $cn",
    "    for arg in $cl",
    "      set tmpline $tmpline --bash-completion-word $arg",
    "    end",
    "    for opt in (" + prog + " $tmpline)",
    "      if test -d $opt",
    "        echo -E \"$opt/\"",
    "      else",
    "        echo -E \"$opt\"",
    "      end",
    "    end",
    "end",
    "",
    "complete --no-files --command " + progn + " --arguments '(_" + progn + ")'"
  ];
  return () => $0;
};
const bashCompletionScript = prog => progn => {
  const $0 = [
    "_" + progn + "()",
    "{",
    "    local CMDLINE",
    "    local IFS=$'\\n'",
    "    CMDLINE=(--bash-completion-index $COMP_CWORD)",
    "",
    "    for arg in ${COMP_WORDS[@]}; do",
    "        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)",
    "    done",
    "",
    "    COMPREPLY=( $(" + prog + " \"${CMDLINE[@]}\") )",
    "}",
    "",
    "complete -o filenames -F _" + progn + " " + progn
  ];
  return () => $0;
};
const arraySplitAt = idx => arr => {
  if (idx === 0) { return {init: [], rest: arr}; }
  return {init: Data$dArray.sliceImpl(0, idx, arr), rest: Data$dArray.sliceImpl(idx, arr.length, arr)};
};
const bashCompletionQuery = pinfo => pprefs => richness => ws => i => v => {
  const v1 = arraySplitAt(i)(ws);
  const v2 = 0 < v1.rest.length ? Data$dMaybe.$Maybe("Just", v1.rest[0]) : Data$dMaybe.Nothing;
  const is_completion = (() => {
    if (v2.tag === "Just") { return Options$dApplicative$dInternal$dUtils.startsWith(v2._1); }
    if (v2.tag === "Nothing") { return v$1 => true; }
    $runtime.fail();
  })();
  const $0 = Data$dFunctor.arrayMap(Options$dApplicative$dCommon.showOption);
  const add_opt_help1 = opt => {
    if (richness.tag === "Standard") { return identity; }
    if (richness.tag === "Enriched") {
      const $1 = richness._1;
      return Data$dFunctor.arrayMap(o => {
        if (opt.optProps.propHelp.tag === "Nothing") { return o; }
        if (opt.optProps.propHelp.tag === "Just") {
          const $2 = Text$dPrettyPrint$dLeijen.displayS(Text$dPrettyPrint$dLeijen.renderFits(Text$dPrettyPrint$dLeijen.fits1)(1.0)($1)(opt.optProps.propHelp._1));
          const $3 = $2 === "" ? [] : Data$dString$dCommon.split("\n")($2);
          return o + "\t" + (() => {
            if ($3.length > 0) {
              if (Data$dArray$dNonEmpty.uncons($3).tail.length === 0) { return Data$dArray$dNonEmpty.uncons($3).head; }
              return Data$dArray$dNonEmpty.uncons($3).head + "...";
            }
            return "";
          })();
        }
        $runtime.fail();
      });
    }
    $runtime.fail();
  };
  const v1$1 = Options$dApplicative$dCommon.runParserFully(Options$dApplicative$dInternal.completionMonadP)(pinfo.infoPolicy)(pinfo.infoParser)(fromFoldable(Data$dArray.sliceImpl(
    1,
    v1.init.length,
    v1.init
  )))(pprefs);
  const v2$1 = (() => {
    if (v1$1.tag === "ComplResult") { return Data$dMaybe.Nothing; }
    if (v1$1.tag === "ComplParser") { return Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Left", Data$dTuple.$Tuple(v1$1._1, v1$1._2))); }
    if (v1$1.tag === "ComplOption") { return Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Right", v1$1._1)); }
    $runtime.fail();
  })();
  if (v2$1.tag === "Just") {
    if (v2$1._1.tag === "Left") {
      const $1 = v2$1._1._1._2;
      const $2 = sequence(Options$dApplicative$dCommon.mapParser(hinfo => opt => {
        if (opt.optMain.tag === "OptReader") {
          if ($1 === "Intersperse" || $1 === "NoIntersperse" || $1 !== "AllPositionals") {
            const $2 = add_opt_help1(opt)(Data$dArray.filterImpl(is_completion, $0(opt.optMain._1)));
            return () => $2;
          }
          return () => [];
        }
        if (opt.optMain.tag === "FlagReader") {
          if ($1 === "Intersperse" || $1 === "NoIntersperse" || $1 !== "AllPositionals") {
            const $2 = add_opt_help1(opt)(Data$dArray.filterImpl(is_completion, $0(opt.optMain._1)));
            return () => $2;
          }
          return () => [];
        }
        if (opt.optMain.tag === "ArgReader") {
          if (hinfo.hinfoUnreachableArgs) { return () => []; }
          return opt.optMain._1.crCompleter(0 < v1.rest.length ? v1.rest[0] : "");
        }
        if (opt.optMain.tag === "CmdReader") {
          if (hinfo.hinfoUnreachableArgs) { return () => []; }
          const $2 = (() => {
            if (richness.tag === "Standard") { return identity; }
            if (richness.tag === "Enriched") {
              const $2 = richness._2;
              return Data$dFunctor.arrayMap(cmd => {
                const $3 = opt.optMain._3(cmd);
                const $4 = (() => {
                  if ($3.tag === "Just") { return $3._1.infoProgDesc; }
                  if ($3.tag === "Nothing") { return Data$dMaybe.Nothing; }
                  $runtime.fail();
                })();
                if ($4.tag === "Nothing") { return cmd; }
                if ($4.tag === "Just") {
                  const $5 = Text$dPrettyPrint$dLeijen.displayS(Text$dPrettyPrint$dLeijen.renderFits(Text$dPrettyPrint$dLeijen.fits1)(1.0)($2)($4._1));
                  const $6 = $5 === "" ? [] : Data$dString$dCommon.split("\n")($5);
                  return cmd + "\t" + (() => {
                    if ($6.length > 0) {
                      if (Data$dArray$dNonEmpty.uncons($6).tail.length === 0) { return Data$dArray$dNonEmpty.uncons($6).head; }
                      return Data$dArray$dNonEmpty.uncons($6).head + "...";
                    }
                    return "";
                  })();
                }
                $runtime.fail();
              });
            }
            $runtime.fail();
          })()(Data$dArray.filterImpl(is_completion, opt.optMain._2));
          return () => $2;
        }
        $runtime.fail();
      })(v2$1._1._1._1._1));
      return () => {
        const a$p = $2();
        return fold(a$p);
      };
    }
    if (v2$1._1.tag === "Right") { return v2$1._1._1(0 < v1.rest.length ? v1.rest[0] : ""); }
    $runtime.fail();
  }
  if (v2$1.tag === "Nothing") { return () => []; }
  $runtime.fail();
};
const bashCompletionParser = pinfo => pprefs => Options$dApplicative$dTypes.$Parser(
  "AltP",
  Options$dApplicative$dTypes.parserFunctor.map(opts => (
    {
      execCompletion: progn => {
        const $0 = opts(progn);
        return () => {
          const a$p = $0();
          return unLines(a$p);
        };
      }
    }
  ))(Options$dApplicative$dTypes.$Parser(
    "MultP",
    Options$dApplicative$dTypes.$MultPE(
      Options$dApplicative$dTypes.$Parser(
        "MultP",
        Options$dApplicative$dTypes.$MultPE(
          Options$dApplicative$dTypes.parserFunctor.map(bashCompletionQuery(pinfo)(pprefs))(Options$dApplicative$dTypes.$Parser(
            "AltP",
            Options$dApplicative$dTypes.$Parser(
              "MultP",
              Options$dApplicative$dTypes.$MultPE(
                Options$dApplicative$dTypes.$Parser(
                  "MultP",
                  Options$dApplicative$dTypes.$MultPE(
                    Options$dApplicative$dBuilder.flag$p(Enriched)(Options$dApplicative$dBuilder$dInternal.$Mod(
                      x => Options$dApplicative$dBuilder$dInternal.internal._1({
                        flagNames: [Options$dApplicative$dTypes.$OptName("OptLong", "bash-completion-enriched"), ...x.flagNames],
                        flagActive: x.flagActive
                      }),
                      Options$dApplicative$dBuilder$dInternal.$DefaultProp(
                        Options$dApplicative$dBuilder$dInternal.internal._2._1.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._1,
                        Options$dApplicative$dBuilder$dInternal.internal._2._2.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._2
                      ),
                      x => Options$dApplicative$dBuilder$dInternal.internal._3(x)
                    )),
                    Options$dApplicative$dBuilder.option(Options$dApplicative$dBuilder.int)(Options$dApplicative$dBuilder$dInternal.$Mod(
                      x => Options$dApplicative$dBuilder$dInternal.internal._1({
                        optNames: [Options$dApplicative$dTypes.$OptName("OptLong", "bash-completion-option-desc-length"), ...x.optNames],
                        optCompleter: x.optCompleter,
                        optNoArgError: x.optNoArgError
                      }),
                      Options$dApplicative$dBuilder$dInternal.$DefaultProp(
                        Data$dMaybe.$Maybe("Just", 40),
                        Options$dApplicative$dBuilder$dInternal.internal._2._2.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._2
                      ),
                      x => Options$dApplicative$dBuilder$dInternal.internal._3(x)
                    ))
                  )
                ),
                Options$dApplicative$dBuilder.option(Options$dApplicative$dBuilder.int)(Options$dApplicative$dBuilder$dInternal.$Mod(
                  x => Options$dApplicative$dBuilder$dInternal.internal._1({
                    optNames: [Options$dApplicative$dTypes.$OptName("OptLong", "bash-completion-command-desc-length"), ...x.optNames],
                    optCompleter: x.optCompleter,
                    optNoArgError: x.optNoArgError
                  }),
                  Options$dApplicative$dBuilder$dInternal.$DefaultProp(
                    Data$dMaybe.$Maybe("Just", 40),
                    Options$dApplicative$dBuilder$dInternal.internal._2._2.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._2
                  ),
                  x => Options$dApplicative$dBuilder$dInternal.internal._3(x)
                ))
              )
            ),
            Options$dApplicative$dTypes.$Parser("NilP", Standard)
          )),
          Options$dApplicative$dTypes.parserFunctor.map(fromFoldable1)(Options$dApplicative$dTypes.$Parser(
            "BindP",
            Options$dApplicative$dTypes.manyM(Options$dApplicative$dBuilder.option(Options$dApplicative$dTypes.readerAsk)(Options$dApplicative$dBuilder$dInternal.$Mod(
              x => Options$dApplicative$dBuilder$dInternal.internal._1({
                optNames: [Options$dApplicative$dTypes.$OptName("OptLong", "bash-completion-word"), ...x.optNames],
                optCompleter: x.optCompleter,
                optNoArgError: x.optNoArgError
              }),
              Options$dApplicative$dBuilder$dInternal.$DefaultProp(
                Options$dApplicative$dBuilder$dInternal.internal._2._1.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._1,
                Options$dApplicative$dBuilder$dInternal.internal._2._2.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._2
              ),
              x => Options$dApplicative$dBuilder$dInternal.internal._3(x)
            )))
          ))
        )
      ),
      Options$dApplicative$dBuilder.option(Options$dApplicative$dBuilder.int)(Options$dApplicative$dBuilder$dInternal.$Mod(
        x => Options$dApplicative$dBuilder$dInternal.internal._1({
          optNames: [Options$dApplicative$dTypes.$OptName("OptLong", "bash-completion-index"), ...x.optNames],
          optCompleter: x.optCompleter,
          optNoArgError: x.optNoArgError
        }),
        Options$dApplicative$dBuilder$dInternal.$DefaultProp(
          Options$dApplicative$dBuilder$dInternal.internal._2._1.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._1,
          Options$dApplicative$dBuilder$dInternal.internal._2._2.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._2
        ),
        x => Options$dApplicative$dBuilder$dInternal.internal._3(x)
      ))
    )
  )),
  Options$dApplicative$dTypes.$Parser(
    "AltP",
    Options$dApplicative$dTypes.parserFunctor.map(opts => (
      {
        execCompletion: progn => {
          const $0 = opts(progn);
          return () => {
            const a$p = $0();
            return unLines(a$p);
          };
        }
      }
    ))(Options$dApplicative$dTypes.parserFunctor.map(bashCompletionScript)(Options$dApplicative$dBuilder.option(Options$dApplicative$dTypes.readerAsk)(Options$dApplicative$dBuilder$dInternal.$Mod(
      x => Options$dApplicative$dBuilder$dInternal.internal._1({
        optNames: [Options$dApplicative$dTypes.$OptName("OptLong", "bash-completion-script"), ...x.optNames],
        optCompleter: x.optCompleter,
        optNoArgError: x.optNoArgError
      }),
      Options$dApplicative$dBuilder$dInternal.$DefaultProp(
        Options$dApplicative$dBuilder$dInternal.internal._2._1.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._1,
        Options$dApplicative$dBuilder$dInternal.internal._2._2.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._2
      ),
      x => Options$dApplicative$dBuilder$dInternal.internal._3(x)
    )))),
    Options$dApplicative$dTypes.$Parser(
      "AltP",
      Options$dApplicative$dTypes.parserFunctor.map(opts => (
        {
          execCompletion: progn => {
            const $0 = opts(progn);
            return () => {
              const a$p = $0();
              return unLines(a$p);
            };
          }
        }
      ))(Options$dApplicative$dTypes.parserFunctor.map(fishCompletionScript)(Options$dApplicative$dBuilder.option(Options$dApplicative$dTypes.readerAsk)(Options$dApplicative$dBuilder$dInternal.$Mod(
        x => Options$dApplicative$dBuilder$dInternal.internal._1({
          optNames: [Options$dApplicative$dTypes.$OptName("OptLong", "fish-completion-script"), ...x.optNames],
          optCompleter: x.optCompleter,
          optNoArgError: x.optNoArgError
        }),
        Options$dApplicative$dBuilder$dInternal.$DefaultProp(
          Options$dApplicative$dBuilder$dInternal.internal._2._1.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._1,
          Options$dApplicative$dBuilder$dInternal.internal._2._2.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._2
        ),
        x => Options$dApplicative$dBuilder$dInternal.internal._3(x)
      )))),
      Options$dApplicative$dTypes.parserFunctor.map(opts => (
        {
          execCompletion: progn => {
            const $0 = opts(progn);
            return () => {
              const a$p = $0();
              return unLines(a$p);
            };
          }
        }
      ))(Options$dApplicative$dTypes.parserFunctor.map(zshCompletionScript)(Options$dApplicative$dBuilder.option(Options$dApplicative$dTypes.readerAsk)(Options$dApplicative$dBuilder$dInternal.$Mod(
        x => Options$dApplicative$dBuilder$dInternal.internal._1({
          optNames: [Options$dApplicative$dTypes.$OptName("OptLong", "zsh-completion-script"), ...x.optNames],
          optCompleter: x.optCompleter,
          optNoArgError: x.optNoArgError
        }),
        Options$dApplicative$dBuilder$dInternal.$DefaultProp(
          Options$dApplicative$dBuilder$dInternal.internal._2._1.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._1,
          Options$dApplicative$dBuilder$dInternal.internal._2._2.tag === "Nothing" ? Data$dMaybe.Nothing : Options$dApplicative$dBuilder$dInternal.internal._2._2
        ),
        x => Options$dApplicative$dBuilder$dInternal.internal._3(x)
      ))))
    )
  )
);
export {
  $Richness,
  Enriched,
  Standard,
  arraySplitAt,
  bashCompletionParser,
  bashCompletionQuery,
  bashCompletionScript,
  fishCompletionScript,
  fold,
  fromFoldable,
  fromFoldable1,
  identity,
  sequence,
  unLines,
  zshCompletionScript
};
