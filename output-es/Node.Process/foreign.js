import process from "process";

export { process };
export const abortImpl = process.abort ? () => process.abort() : null;
export const argv = () => process.argv.slice();
export const argv0 = () => process.argv0;
export const channelRefImpl = process.channel && process.channel.ref ? () => process.channel.ref() : null;
export const channelUnrefImpl = process.channel && process.channel.unref ? () => process.channel.unref() : null;
export const chdirImpl = (dir) => process.chdir(dir);
export const config = () => Object.assign({}, process.config);
export const connected = () => process.connected;
export const cpuUsage = () => process.cpuUsage();
export const cpuUsageDiffImpl = (prevVal) => process.cpuUsage(prevVal);
export const cwd = () => process.cwd();
export const debugPort = process.debugPort;
export const disconnectImpl = process.disconnect ? () => process.disconnect() : null;
export const getEnv = () => Object.assign({}, process.env);
export const unsafeGetEnv = () => process.env;
export const setEnvImpl = (key, val) => {
  process.env[key] = val;
};
export const unsetEnvImpl = (key) => {
  delete process.env[key];
};
export const execArgv = () => process.execArgv.slice();
export const execPath = () => process.execPath;
export const exit = () => process.exit();
export const exitImpl = (code) => process.exit(code);
export const setExitCodeImpl = (code) => {
  process.exitCode = code;
};
export const getExitCodeImpl = () => process.exitCode;
export const getGidImpl = () => process.getgid();
export const getUidImpl = () => process.getuid();
export const hasUncaughtExceptionCaptureCallback = () => process.hasUncaughtExceptionCaptureCallback;
export const killImpl = (pid) => process.kill(pid);
export const killStrImpl = (pid, sig) => process.kill(pid, sig);
export const killIntImpl = (pid, int) => process.kill(pid, int);
export const memoryUsage = () => process.memoryUsage();
export const memoryUsageRss = () => process.memoryUsage.rss();
export const nextTickImpl = (cb) => process.nextTick(cb);
export const nextTickCbImpl = (cb, args) => process.nextTick(cb, args);
export const pid = process.pid;
export const platformStr = process.platform;
export const ppid = process.ppid;
export const resourceUsage = () => process.resourceUsage;
export const sendImpl = (msg, handle) => process.send(msg, handle);
export const sendOptsImpl = (msg, handle, opts) => process.send(msg, handle, opts);
export const sendCbImpl = (msg, handle, cb) => process.send(msg, handle, cb);
export const sendOptsCbImpl = (msg, handle, opts, cb) => process.send(msg, handle, opts, cb);
export const setUncaughtExceptionCaptureCallbackImpl = (cb) => process.setUncaughtExceptionCaptureCallback(cb);
export const clearUncaughtExceptionCaptureCallback = () => process.setUncaughtExceptionCaptureCallback(null);
export const stdin = process.stdin;
export const stdout = process.stdout;
export const stderr = process.stderr;
export const stdinIsTTY = process.stdinIsTTY;
export const stdoutIsTTY = process.stdoutIsTTY;
export const stderrIsTTY = process.stderrIsTTY;
export const getTitle = () => process.title;
export const setTitleImpl = (v) => {
  process.title = v;
};
export const uptime = () => process.uptime();
export const version = process.version;

