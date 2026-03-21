import * as $runtime from "../runtime.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Options$dApplicative$dTypes from "../Options.Applicative.Types/index.js";
const $DefaultProp = (_1, _2) => ({tag: "DefaultProp", _1, _2});
const $Mod = (_1, _2, _3) => ({tag: "Mod", _1, _2, _3});
const lookup = /* #__PURE__ */ Data$dFoldable.lookup(Data$dFoldable.foldableArray)(Data$dEq.eqString);
const identity = x => x;
const OptionFields = x => x;
const FlagFields = x => x;
const DefaultProp = value0 => value1 => $DefaultProp(value0, value1);
const Mod = value0 => value1 => value2 => $Mod(value0, value1, value2);
const CommandFields = x => x;
const ArgumentFields = x => x;
const optionFieldsHasValue = {hasValueDummy: v => {}};
const optionFieldsHasMetavar = {hasMetavarDummy: v => {}};
const newtypeOptionFields = {Coercible0: () => {}};
const optionFieldsHasCompleter = {modCompleter: f => p => ({optCompleter: f(p.optCompleter), optNames: p.optNames, optNoArgError: p.optNoArgError})};
const optionFieldsHasName = {name: n => fields => ({optNames: [n, ...fields.optNames], optCompleter: fields.optCompleter, optNoArgError: fields.optNoArgError})};
const newtypeFlagFields = {Coercible0: () => {}};
const newtypeCommandFields = {Coercible0: () => {}};
const newtypeArgumentFields = {Coercible0: () => {}};
const name = dict => dict.name;
const modCompleter = dict => dict.modCompleter;
const mkCommand = m => {
  const v = m._1({cmdCommands: [], cmdGroup: Data$dMaybe.Nothing});
  const $0 = v.cmdCommands;
  return Data$dTuple.$Tuple(v.cmdGroup, Data$dTuple.$Tuple(Data$dFunctor.arrayMap(Data$dTuple.fst)($0), Data$dTuple.$Tuple(v1 => lookup(v1)($0), undefined)));
};
const hasValueDummy = dict => dict.hasValueDummy;
const hasMetavarDummy = dict => dict.hasMetavarDummy;
const flagFieldsHasName = {name: n => fields => ({flagNames: [n, ...fields.flagNames], flagActive: fields.flagActive})};
const defaultPropSemigroup = {append: v => v1 => $DefaultProp(v._1.tag === "Nothing" ? v1._1 : v._1, v._2.tag === "Nothing" ? v1._2 : v._2)};
const modSemigroup = {
  append: v => v1 => $Mod(x => v1._1(v._1(x)), $DefaultProp(v1._2._1.tag === "Nothing" ? v._2._1 : v1._2._1, v1._2._2.tag === "Nothing" ? v._2._2 : v1._2._2), x => v1._3(v._3(x)))
};
const defaultPropMonoid = {mempty: /* #__PURE__ */ $DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing), Semigroup0: () => defaultPropSemigroup};
const fieldMod = f => $Mod(f, $DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing), identity);
const modMonoid = {mempty: /* #__PURE__ */ $Mod(identity, /* #__PURE__ */ $DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing), identity), Semigroup0: () => modSemigroup};
const optionMod = /* #__PURE__ */ Mod(identity)(/* #__PURE__ */ $DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing));
const internal = /* #__PURE__ */ optionMod(p => (
  {propVisibility: Options$dApplicative$dTypes.Internal, propDescMod: p.propDescMod, propHelp: p.propHelp, propMetaVar: p.propMetaVar, propShowDefault: p.propShowDefault}
));
const commandFieldsHasMetavar = {hasMetavarDummy: v => {}};
const baseProps = {
  propMetaVar: "",
  propVisibility: Options$dApplicative$dTypes.Visible,
  propHelp: Data$dMaybe.Nothing,
  propShowDefault: Data$dMaybe.Nothing,
  propDescMod: Data$dMaybe.Nothing
};
const mkProps = v => g => {
  const $0 = g(baseProps);
  return {
    propShowDefault: (() => {
      if (v._2.tag === "Just") {
        if (v._1.tag === "Just") { return Data$dMaybe.$Maybe("Just", v._2._1(v._1._1)); }
        return Data$dMaybe.Nothing;
      }
      if (v._2.tag === "Nothing") { return Data$dMaybe.Nothing; }
      $runtime.fail();
    })(),
    propDescMod: $0.propDescMod,
    propHelp: $0.propHelp,
    propMetaVar: $0.propMetaVar,
    propVisibility: $0.propVisibility
  };
};
const mkOption = d => g => rdr => ({optMain: rdr, optProps: mkProps(d)(g)});
const mkParser = v => g => rdr => {
  const o = Options$dApplicative$dTypes.$Parser("OptP", {optMain: rdr, optProps: mkProps(v)(g)});
  if (v._1.tag === "Nothing") { return o; }
  if (v._1.tag === "Just") { return Options$dApplicative$dTypes.$Parser("AltP", o, Options$dApplicative$dTypes.$Parser("NilP", v._1._1)); }
  $runtime.fail();
};
const argumentFieldsHasValue = {hasValueDummy: v => {}};
const argumentFieldsHasMetavar = {hasMetavarDummy: v => {}};
const argumentFieldsHasCompleter = {modCompleter: f => p => ({argCompleter: f(p.argCompleter)})};
export {
  $DefaultProp,
  $Mod,
  ArgumentFields,
  CommandFields,
  DefaultProp,
  FlagFields,
  Mod,
  OptionFields,
  argumentFieldsHasCompleter,
  argumentFieldsHasMetavar,
  argumentFieldsHasValue,
  baseProps,
  commandFieldsHasMetavar,
  defaultPropMonoid,
  defaultPropSemigroup,
  fieldMod,
  flagFieldsHasName,
  hasMetavarDummy,
  hasValueDummy,
  identity,
  internal,
  lookup,
  mkCommand,
  mkOption,
  mkParser,
  mkProps,
  modCompleter,
  modMonoid,
  modSemigroup,
  name,
  newtypeArgumentFields,
  newtypeCommandFields,
  newtypeFlagFields,
  newtypeOptionFields,
  optionFieldsHasCompleter,
  optionFieldsHasMetavar,
  optionFieldsHasName,
  optionFieldsHasValue,
  optionMod
};
