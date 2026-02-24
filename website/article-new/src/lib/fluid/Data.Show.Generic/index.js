import * as $runtime from "../runtime.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
import {intercalate} from "./foreign.js";
const genericShowArgsNoArguments = {genericShowArgs: v => []};
const genericShowArgsArgument = dictShow => ({genericShowArgs: v => [dictShow.show(v)]});
const genericShowArgs = dict => dict.genericShowArgs;
const genericShowArgsProduct = dictGenericShowArgs => dictGenericShowArgs1 => (
  {genericShowArgs: v => [...dictGenericShowArgs.genericShowArgs(v._1), ...dictGenericShowArgs1.genericShowArgs(v._2)]}
);
const genericShowConstructor = dictGenericShowArgs => dictIsSymbol => (
  {
    "genericShow'": v => {
      const ctor = dictIsSymbol.reflectSymbol(Type$dProxy.Proxy);
      const v1 = dictGenericShowArgs.genericShowArgs(v);
      if (v1.length === 0) { return ctor; }
      return "(" + intercalate(" ")([ctor, ...v1]) + ")";
    }
  }
);
const genericShow$p = dict => dict["genericShow'"];
const genericShowNoConstructors = {"genericShow'": a => genericShowNoConstructors["genericShow'"](a)};
const genericShowSum = dictGenericShow => dictGenericShow1 => (
  {
    "genericShow'": v => {
      if (v.tag === "Inl") { return dictGenericShow["genericShow'"](v._1); }
      if (v.tag === "Inr") { return dictGenericShow1["genericShow'"](v._1); }
      $runtime.fail();
    }
  }
);
const genericShow = dictGeneric => dictGenericShow => x => dictGenericShow["genericShow'"](dictGeneric.from(x));
export {
  
  
  
  
  genericShowArgsNoArguments,
  genericShowArgsProduct,
  genericShowConstructor,
  
  
};
export * from "./foreign.js";
