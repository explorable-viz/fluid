import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Val from "../Val/index.js";
const select = b => Data$dTuple.$Tuple(
  Val.functorVal.map(v => {
    if (v.tag === "Inert") { return App$dUtil.Inert; }
    if (v.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: !v._1.persistent, transient: v._1.transient}); }
    $runtime.fail();
  })(b),
  App$dUtil.Persistent
);
const linkedInputs_spec5 = {
  spec: {fluidSrcPaths: ["fluid", "test/fluid"], inputs: ["nonRenewables", "renewables"], query: Data$dMaybe.Nothing, linking: true, rowFilter: Data$dMaybe.Nothing},
  "δ_in": /* #__PURE__ */ Data$dTuple.$Tuple("nonRenewables", /* #__PURE__ */ App$dUtil$dSelector.listElement(0)(/* #__PURE__ */ App$dUtil$dSelector.dictVal("coalCap")(select))),
  in_expect: /* #__PURE__ */ (() => {
    const $0 = App$dUtil$dSelector.envVal("nonRenewables")(App$dUtil$dSelector.listElement(0)(x => App$dUtil$dSelector.dictVal("petrolCap")(select)(App$dUtil$dSelector.dictVal("nuclearCap")(select)(App$dUtil$dSelector.dictVal("gasCap")(select)(App$dUtil$dSelector.dictVal("coalCap")(select)(x)._1)._1)._1)));
    const $1 = App$dUtil$dSelector.envVal("renewables")(x => App$dUtil$dSelector.listElement(3)(App$dUtil$dSelector.dictVal("capacity")(select))(App$dUtil$dSelector.listElement(2)(App$dUtil$dSelector.dictVal("capacity")(select))(App$dUtil$dSelector.listElement(1)(App$dUtil$dSelector.dictVal("capacity")(select))(App$dUtil$dSelector.listElement(0)(App$dUtil$dSelector.dictVal("capacity")(select))(x)._1)._1)._1));
    return x => $1($0(x)._1);
  })(),
  file: "linkedInputs/mini-energyscatter.fld"
};
const linkedInputs_spec4 = {
  spec: {fluidSrcPaths: ["fluid", "test/fluid"], inputs: ["renewables", "nonRenewables"], query: Data$dMaybe.Nothing, linking: true, rowFilter: Data$dMaybe.Nothing},
  "δ_in": /* #__PURE__ */ Data$dTuple.$Tuple("renewables", /* #__PURE__ */ App$dUtil$dSelector.listElement(204)(/* #__PURE__ */ App$dUtil$dSelector.dictVal("capacity")(select))),
  in_expect: /* #__PURE__ */ (() => {
    const $0 = App$dUtil$dSelector.envVal("nonRenewables")(App$dUtil$dSelector.listElement(51)(x => App$dUtil$dSelector.dictVal("nuclearOut")(select)(App$dUtil$dSelector.dictVal("petrolCap")(select)(App$dUtil$dSelector.dictVal("nuclearCap")(select)(App$dUtil$dSelector.dictVal("gasCap")(select)(App$dUtil$dSelector.dictVal("coalCap")(select)(x)._1)._1)._1)._1)));
    const $1 = App$dUtil$dSelector.envVal("renewables")((() => {
      const $1 = App$dUtil$dSelector.listElement(204)(x => App$dUtil$dSelector.dictVal("output")(select)(App$dUtil$dSelector.dictVal("capacity")(select)(x)._1));
      const $2 = App$dUtil$dSelector.listElement(205)(x => App$dUtil$dSelector.dictVal("output")(select)(App$dUtil$dSelector.dictVal("capacity")(select)(x)._1));
      const $3 = App$dUtil$dSelector.listElement(206)(x => App$dUtil$dSelector.dictVal("output")(select)(App$dUtil$dSelector.dictVal("capacity")(select)(x)._1));
      const $4 = App$dUtil$dSelector.listElement(207)(x => App$dUtil$dSelector.dictVal("output")(select)(App$dUtil$dSelector.dictVal("capacity")(select)(x)._1));
      return x => $4($3($2($1(x)._1)._1)._1);
    })());
    return x => $1($0(x)._1);
  })(),
  file: "linkedInputs/energyscatter.fld"
};
const linkedInputs_spec3 = {
  spec: {fluidSrcPaths: ["fluid", "test/fluid"], inputs: ["renewables", "nonRenewables"], query: Data$dMaybe.Nothing, linking: true, rowFilter: Data$dMaybe.Nothing},
  "δ_in": /* #__PURE__ */ Data$dTuple.$Tuple("nonRenewables", /* #__PURE__ */ App$dUtil$dSelector.listElement(51)(/* #__PURE__ */ App$dUtil$dSelector.dictVal("coalCap")(select))),
  in_expect: /* #__PURE__ */ (() => {
    const $0 = App$dUtil$dSelector.envVal("nonRenewables")(App$dUtil$dSelector.listElement(51)(x => App$dUtil$dSelector.dictVal("petrolCap")(select)(App$dUtil$dSelector.dictVal("nuclearCap")(select)(App$dUtil$dSelector.dictVal("gasCap")(select)(App$dUtil$dSelector.dictVal("coalCap")(select)(x)._1)._1)._1)));
    const $1 = App$dUtil$dSelector.envVal("renewables")(x => App$dUtil$dSelector.listElement(207)(App$dUtil$dSelector.dictVal("capacity")(select))(App$dUtil$dSelector.listElement(206)(App$dUtil$dSelector.dictVal("capacity")(select))(App$dUtil$dSelector.listElement(205)(App$dUtil$dSelector.dictVal("capacity")(select))(App$dUtil$dSelector.listElement(204)(App$dUtil$dSelector.dictVal("capacity")(select))(x)._1)._1)._1));
    return x => $1($0(x)._1);
  })(),
  file: "linkedInputs/energyscatter.fld"
};
const linkedInputs_cases = [linkedInputs_spec3, linkedInputs_spec4, linkedInputs_spec5];
export {linkedInputs_cases, linkedInputs_spec3, linkedInputs_spec4, linkedInputs_spec5, select};
