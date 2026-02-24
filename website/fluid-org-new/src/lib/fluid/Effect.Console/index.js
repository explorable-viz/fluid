import {clear, debug, error, group, groupCollapsed, groupEnd, info, log, time, timeEnd, timeLog, warn} from "./foreign.js";
const warnShow = dictShow => a => warn(dictShow.show(a));
const logShow = dictShow => a => log(dictShow.show(a));
const infoShow = dictShow => a => info(dictShow.show(a));
const grouped = name => inner => {
  const $0 = group(name);
  return () => {
    $0();
    const result = inner();
    groupEnd();
    return result;
  };
};
const errorShow = dictShow => a => error(dictShow.show(a));
const debugShow = dictShow => a => debug(dictShow.show(a));
;
export * from "./foreign.js";
