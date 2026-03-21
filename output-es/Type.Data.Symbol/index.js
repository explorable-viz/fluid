import * as Type$dProxy from "../Type.Proxy/index.js";
const uncons = () => v => ({head: Type$dProxy.Proxy, tail: Type$dProxy.Proxy});
const equalsSymbol = () => () => ({});
const equals = () => v => v1 => Type$dProxy.Proxy;
const compare = () => v => v1 => Type$dProxy.Proxy;
const append = () => v => v1 => Type$dProxy.Proxy;
export {append, compare, equals, equalsSymbol, uncons};
