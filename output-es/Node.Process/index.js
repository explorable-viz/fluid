// | Bindings to the global `process` object in Node.js. See also [the Node API documentation](https://nodejs.org/api/process.html)
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Data$dPosix$dSignal from "../Data.Posix.Signal/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Effect$dUncurried from "../Effect.Uncurried/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Node$dEventEmitter from "../Node.EventEmitter/index.js";
import * as Node$dPlatform from "../Node.Platform/index.js";
import {
  abortImpl,
  argv,
  argv0,
  channelRefImpl,
  channelUnrefImpl,
  chdirImpl,
  clearUncaughtExceptionCaptureCallback,
  config,
  connected,
  cpuUsage,
  cpuUsageDiffImpl,
  cwd,
  debugPort,
  disconnectImpl,
  execArgv,
  execPath,
  exit,
  exitImpl,
  getEnv,
  getExitCodeImpl,
  getGidImpl,
  getTitle,
  getUidImpl,
  hasUncaughtExceptionCaptureCallback,
  killImpl,
  killIntImpl,
  killStrImpl,
  memoryUsage,
  memoryUsageRss,
  nextTickCbImpl,
  nextTickImpl,
  pid,
  platformStr,
  ppid,
  process,
  resourceUsage,
  sendCbImpl,
  sendImpl,
  sendOptsCbImpl,
  sendOptsImpl,
  setEnvImpl,
  setExitCodeImpl,
  setTitleImpl,
  setUncaughtExceptionCaptureCallbackImpl,
  stderr,
  stderrIsTTY,
  stdin,
  stdinIsTTY,
  stdout,
  stdoutIsTTY,
  unsafeGetEnv,
  unsetEnvImpl,
  uptime,
  version
} from "./foreign.js";
const show = record => "{ system: " + Data$dShow.showIntImpl(record.system) + ", user: " + Data$dShow.showIntImpl(record.user) + " }";
const identity = x => x;
const showCpuUsage = {show: v => "(CpuUsage " + show(v) + ")"};
const eqCpuUsage = {eq: x => y => x.system === y.system && x.user === y.user};
const workerH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("worker", Effect$dUncurried.mkEffectFn1);
const warningH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("warning", Effect$dUncurried.mkEffectFn1);
const unsetEnv = key => () => unsetEnvImpl(key);
const unsafeSendOptsCb = () => msg => handle => opts => cb => () => sendOptsCbImpl(
  msg,
  handle,
  opts,
  err => cb(Data$dNullable.nullable(err, Data$dMaybe.Nothing, Data$dMaybe.Just))()
);
const unsafeSendOpts = () => msg => handle => opts => () => sendOptsImpl(msg, handle, opts);
const unsafeSendCb = msg => handle => cb => () => sendCbImpl(msg, handle, err => cb(Data$dNullable.nullable(err, Data$dMaybe.Nothing, Data$dMaybe.Just))());
const unsafeSend = msg => handle => () => sendImpl(msg, handle);
const unhandledRejectionH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("unhandledRejection", cb => (a, b) => cb(a)(b)());
const uncaughtExceptionH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("uncaughtException", cb => (a, b) => cb(a)(b)());
const setUncaughtExceptionCaptureCallback = cb => () => setUncaughtExceptionCaptureCallbackImpl(cb);
const setTitle = newTitle => () => setTitleImpl(newTitle);
const setExitCode = code => () => setExitCodeImpl(code);
const setEnv = key => value => () => setEnvImpl(key, value);
const rejectionHandledH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("rejectionHandled", Effect$dUncurried.mkEffectFn1);
const platform = /* #__PURE__ */ (() => {
  if (platformStr === "aix") { return Data$dMaybe.$Maybe("Just", Node$dPlatform.AIX); }
  if (platformStr === "darwin") { return Data$dMaybe.$Maybe("Just", Node$dPlatform.Darwin); }
  if (platformStr === "freebsd") { return Data$dMaybe.$Maybe("Just", Node$dPlatform.FreeBSD); }
  if (platformStr === "linux") { return Data$dMaybe.$Maybe("Just", Node$dPlatform.Linux); }
  if (platformStr === "openbsd") { return Data$dMaybe.$Maybe("Just", Node$dPlatform.OpenBSD); }
  if (platformStr === "sunos") { return Data$dMaybe.$Maybe("Just", Node$dPlatform.SunOS); }
  if (platformStr === "win32") { return Data$dMaybe.$Maybe("Just", Node$dPlatform.Win32); }
  if (platformStr === "android") { return Data$dMaybe.$Maybe("Just", Node$dPlatform.Android); }
  return Data$dMaybe.Nothing;
})();
const nextTick$p = cb => args => () => nextTickCbImpl($0 => cb($0)(), args);
const nextTick = cb => () => nextTickImpl(cb);
const mkSignalH$p = sig => Node$dEventEmitter.$EventHandle(Data$dString$dCommon.toUpper(sig), identity);
const mkSignalH = sig => Node$dEventEmitter.$EventHandle(Data$dPosix$dSignal.toString(sig), identity);
const messageH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("message", cb => (a, b) => cb(a)(Data$dNullable.nullable(b, Data$dMaybe.Nothing, Data$dMaybe.Just))());
const lookupEnv = k => () => {
  const a$p = unsafeGetEnv();
  return Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, k, a$p);
};
const killStr = p => sig => () => killStrImpl(p, sig);
const killInt = p => sig => () => killIntImpl(p, sig);
const kill = p => () => killImpl(p);
const getUid = () => {
  const a$p = getUidImpl();
  return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
};
const getGid = () => {
  const a$p = getGidImpl();
  return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
};
const getExitCode = () => {
  const a$p = getExitCodeImpl();
  return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
};
const exitH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("exit", Effect$dUncurried.mkEffectFn1);
const exit$p = code => () => exitImpl(code);
const disconnectH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("disconnect", identity);
const disconnect = /* #__PURE__ */ Data$dNullable.nullable(disconnectImpl, Data$dMaybe.Nothing, Data$dMaybe.Just);
const cpuUsageToRecord = v => v;
const cpuUsageDiff = prev => () => cpuUsageDiffImpl(prev);
const chdir = dir => () => chdirImpl(dir);
const channelUnref = /* #__PURE__ */ Data$dNullable.nullable(channelUnrefImpl, Data$dMaybe.Nothing, Data$dMaybe.Just);
const channelRef = /* #__PURE__ */ Data$dNullable.nullable(channelRefImpl, Data$dMaybe.Nothing, Data$dMaybe.Just);
const beforeExitH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("beforeExit", Effect$dUncurried.mkEffectFn1);
const abort = /* #__PURE__ */ Data$dNullable.nullable(abortImpl, Data$dMaybe.Nothing, Data$dMaybe.Just);
export {
  abort,
  beforeExitH,
  channelRef,
  channelUnref,
  chdir,
  cpuUsageDiff,
  cpuUsageToRecord,
  disconnect,
  disconnectH,
  eqCpuUsage,
  exit$p,
  exitH,
  getExitCode,
  getGid,
  getUid,
  identity,
  kill,
  killInt,
  killStr,
  lookupEnv,
  messageH,
  mkSignalH,
  mkSignalH$p,
  nextTick,
  nextTick$p,
  platform,
  rejectionHandledH,
  setEnv,
  setExitCode,
  setTitle,
  setUncaughtExceptionCaptureCallback,
  show,
  showCpuUsage,
  uncaughtExceptionH,
  unhandledRejectionH,
  unsafeSend,
  unsafeSendCb,
  unsafeSendOpts,
  unsafeSendOptsCb,
  unsetEnv,
  warningH,
  workerH
};
export * from "./foreign.js";
