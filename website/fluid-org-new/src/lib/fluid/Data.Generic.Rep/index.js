import * as $runtime from "../runtime.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const $NoArguments = () => ({tag: "NoArguments"});
const $Product = (_1, _2) => ({tag: "Product", _1, _2});
const $Sum = (tag, _1) => ({tag, _1});
const Inl = value0 => $Sum("Inl", value0);
const Inr = value0 => $Sum("Inr", value0);
const Product = value0 => value1 => $Product(value0, value1);
const NoArguments = /* #__PURE__ */ $NoArguments();
const Constructor = x => x;
const Argument = x => x;
const to = dict => dict.to;
const showSum = dictShow => dictShow1 => (
  {
    show: v => {
      if (v.tag === "Inl") { return "(Inl " + dictShow.show(v._1) + ")"; }
      if (v.tag === "Inr") { return "(Inr " + dictShow1.show(v._1) + ")"; }
      $runtime.fail();
    }
  }
);
const showProduct = dictShow => dictShow1 => ({show: v => "(Product " + dictShow.show(v._1) + " " + dictShow1.show(v._2) + ")"});
const showNoArguments = {show: v => "NoArguments"};
const showConstructor = dictIsSymbol => dictShow => (
  {show: v => "(Constructor @" + Data$dShow.showStringImpl(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy)) + " " + dictShow.show(v) + ")"}
);
const showArgument = dictShow => ({show: v => "(Argument " + dictShow.show(v) + ")"});
const repOf = dictGeneric => v => Type$dProxy.Proxy;
const from = dict => dict.from;
export { $Product, $Sum,     NoArguments,         };
