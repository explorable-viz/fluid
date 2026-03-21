import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Options$dApplicative$dHelp$dChunk from "../Options.Applicative.Help.Chunk/index.js";
import * as Text$dPrettyPrint$dLeijen from "../Text.PrettyPrint.Leijen/index.js";
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
const ParserHelp = x => x;
const parserHelpSemigroup = {
  append: ra => rb => (
    {
      helpBody: (() => {
        if (ra.helpBody.tag === "Nothing") { return rb.helpBody; }
        if (rb.helpBody.tag === "Nothing") { return ra.helpBody; }
        if (ra.helpBody.tag === "Just" && rb.helpBody.tag === "Just") { return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", ra.helpBody._1, rb.helpBody._1)); }
        $runtime.fail();
      })(),
      helpError: (() => {
        if (ra.helpError.tag === "Nothing") { return rb.helpError; }
        if (rb.helpError.tag === "Nothing") { return ra.helpError; }
        if (ra.helpError.tag === "Just" && rb.helpError.tag === "Just") {
          return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", ra.helpError._1, rb.helpError._1));
        }
        $runtime.fail();
      })(),
      helpFooter: (() => {
        if (ra.helpFooter.tag === "Nothing") { return rb.helpFooter; }
        if (rb.helpFooter.tag === "Nothing") { return ra.helpFooter; }
        if (ra.helpFooter.tag === "Just" && rb.helpFooter.tag === "Just") {
          return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", ra.helpFooter._1, rb.helpFooter._1));
        }
        $runtime.fail();
      })(),
      helpHeader: (() => {
        if (ra.helpHeader.tag === "Nothing") { return rb.helpHeader; }
        if (rb.helpHeader.tag === "Nothing") { return ra.helpHeader; }
        if (ra.helpHeader.tag === "Just" && rb.helpHeader.tag === "Just") {
          return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", ra.helpHeader._1, rb.helpHeader._1));
        }
        $runtime.fail();
      })(),
      helpSuggestions: (() => {
        if (ra.helpSuggestions.tag === "Nothing") { return rb.helpSuggestions; }
        if (rb.helpSuggestions.tag === "Nothing") { return ra.helpSuggestions; }
        if (ra.helpSuggestions.tag === "Just" && rb.helpSuggestions.tag === "Just") {
          return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", ra.helpSuggestions._1, rb.helpSuggestions._1));
        }
        $runtime.fail();
      })(),
      helpUsage: (() => {
        if (ra.helpUsage.tag === "Nothing") { return rb.helpUsage; }
        if (rb.helpUsage.tag === "Nothing") { return ra.helpUsage; }
        if (ra.helpUsage.tag === "Just" && rb.helpUsage.tag === "Just") {
          return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", ra.helpUsage._1, rb.helpUsage._1));
        }
        $runtime.fail();
      })()
    }
  )
};
const parserHelpMonoid = /* #__PURE__ */ (() => {
  const Semigroup0 = chunkMonoid.Semigroup0();
  const Semigroup0$1 = chunkMonoid.Semigroup0();
  const Semigroup0$2 = chunkMonoid.Semigroup0();
  const Semigroup0$3 = chunkMonoid.Semigroup0();
  const Semigroup0$4 = chunkMonoid.Semigroup0();
  const Semigroup0$5 = chunkMonoid.Semigroup0();
  const semigroupRecord1 = {
    append: ra => rb => (
      {
        helpBody: Semigroup0.append(ra.helpBody)(rb.helpBody),
        helpError: Semigroup0$1.append(ra.helpError)(rb.helpError),
        helpFooter: Semigroup0$2.append(ra.helpFooter)(rb.helpFooter),
        helpHeader: Semigroup0$3.append(ra.helpHeader)(rb.helpHeader),
        helpSuggestions: Semigroup0$4.append(ra.helpSuggestions)(rb.helpSuggestions),
        helpUsage: Semigroup0$5.append(ra.helpUsage)(rb.helpUsage)
      }
    )
  };
  return {
    mempty: {
      helpBody: chunkMonoid.mempty,
      helpError: chunkMonoid.mempty,
      helpFooter: chunkMonoid.mempty,
      helpHeader: chunkMonoid.mempty,
      helpSuggestions: chunkMonoid.mempty,
      helpUsage: chunkMonoid.mempty
    },
    Semigroup0: () => semigroupRecord1
  };
})();
const newtypeParserHelp = {Coercible0: () => {}};
const helpText = v => {
  const $0 = Options$dApplicative$dHelp$dChunk.vsepChunks([v.helpError, v.helpSuggestions, v.helpHeader, v.helpUsage, v.helpBody, v.helpFooter]);
  if ($0.tag === "Nothing") { return Text$dPrettyPrint$dLeijen.Empty; }
  if ($0.tag === "Just") { return $0._1; }
  $runtime.fail();
};
const renderHelp = cols => x => Text$dPrettyPrint$dLeijen.displayS(Text$dPrettyPrint$dLeijen.renderFits(Text$dPrettyPrint$dLeijen.fits1)(1.0)(cols)(helpText(x)));
const parserHelpShow = {
  show: x => Data$dShow.showStringImpl(Text$dPrettyPrint$dLeijen.displayS(Text$dPrettyPrint$dLeijen.renderFits(Text$dPrettyPrint$dLeijen.fits1)(1.0)(80)(helpText(x))))
};
export {ParserHelp, chunkMonoid, helpText, newtypeParserHelp, parserHelpMonoid, parserHelpSemigroup, parserHelpShow, renderHelp};
