import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $Signal = tag => tag;
const SIGABRT = /* #__PURE__ */ $Signal("SIGABRT");
const SIGALRM = /* #__PURE__ */ $Signal("SIGALRM");
const SIGBUS = /* #__PURE__ */ $Signal("SIGBUS");
const SIGCHLD = /* #__PURE__ */ $Signal("SIGCHLD");
const SIGCLD = /* #__PURE__ */ $Signal("SIGCLD");
const SIGCONT = /* #__PURE__ */ $Signal("SIGCONT");
const SIGEMT = /* #__PURE__ */ $Signal("SIGEMT");
const SIGFPE = /* #__PURE__ */ $Signal("SIGFPE");
const SIGHUP = /* #__PURE__ */ $Signal("SIGHUP");
const SIGILL = /* #__PURE__ */ $Signal("SIGILL");
const SIGINFO = /* #__PURE__ */ $Signal("SIGINFO");
const SIGINT = /* #__PURE__ */ $Signal("SIGINT");
const SIGIO = /* #__PURE__ */ $Signal("SIGIO");
const SIGIOT = /* #__PURE__ */ $Signal("SIGIOT");
const SIGKILL = /* #__PURE__ */ $Signal("SIGKILL");
const SIGLOST = /* #__PURE__ */ $Signal("SIGLOST");
const SIGPIPE = /* #__PURE__ */ $Signal("SIGPIPE");
const SIGPOLL = /* #__PURE__ */ $Signal("SIGPOLL");
const SIGPROF = /* #__PURE__ */ $Signal("SIGPROF");
const SIGPWR = /* #__PURE__ */ $Signal("SIGPWR");
const SIGQUIT = /* #__PURE__ */ $Signal("SIGQUIT");
const SIGSEGV = /* #__PURE__ */ $Signal("SIGSEGV");
const SIGSTKFLT = /* #__PURE__ */ $Signal("SIGSTKFLT");
const SIGSTOP = /* #__PURE__ */ $Signal("SIGSTOP");
const SIGSYS = /* #__PURE__ */ $Signal("SIGSYS");
const SIGTERM = /* #__PURE__ */ $Signal("SIGTERM");
const SIGTRAP = /* #__PURE__ */ $Signal("SIGTRAP");
const SIGTSTP = /* #__PURE__ */ $Signal("SIGTSTP");
const SIGTTIN = /* #__PURE__ */ $Signal("SIGTTIN");
const SIGTTOU = /* #__PURE__ */ $Signal("SIGTTOU");
const SIGUNUSED = /* #__PURE__ */ $Signal("SIGUNUSED");
const SIGURG = /* #__PURE__ */ $Signal("SIGURG");
const SIGUSR1 = /* #__PURE__ */ $Signal("SIGUSR1");
const SIGUSR2 = /* #__PURE__ */ $Signal("SIGUSR2");
const SIGVTALRM = /* #__PURE__ */ $Signal("SIGVTALRM");
const SIGWINCH = /* #__PURE__ */ $Signal("SIGWINCH");
const SIGXCPU = /* #__PURE__ */ $Signal("SIGXCPU");
const SIGXFSZ = /* #__PURE__ */ $Signal("SIGXFSZ");
const toString = s => {
  if (s === "SIGABRT") { return "SIGABRT"; }
  if (s === "SIGALRM") { return "SIGALRM"; }
  if (s === "SIGBUS") { return "SIGBUS"; }
  if (s === "SIGCHLD") { return "SIGCHLD"; }
  if (s === "SIGCLD") { return "SIGCLD"; }
  if (s === "SIGCONT") { return "SIGCONT"; }
  if (s === "SIGEMT") { return "SIGEMT"; }
  if (s === "SIGFPE") { return "SIGFPE"; }
  if (s === "SIGHUP") { return "SIGHUP"; }
  if (s === "SIGILL") { return "SIGILL"; }
  if (s === "SIGINFO") { return "SIGINFO"; }
  if (s === "SIGINT") { return "SIGINT"; }
  if (s === "SIGIO") { return "SIGIO"; }
  if (s === "SIGIOT") { return "SIGIOT"; }
  if (s === "SIGKILL") { return "SIGKILL"; }
  if (s === "SIGLOST") { return "SIGLOST"; }
  if (s === "SIGPIPE") { return "SIGPIPE"; }
  if (s === "SIGPOLL") { return "SIGPOLL"; }
  if (s === "SIGPROF") { return "SIGPROF"; }
  if (s === "SIGPWR") { return "SIGPWR"; }
  if (s === "SIGQUIT") { return "SIGQUIT"; }
  if (s === "SIGSEGV") { return "SIGSEGV"; }
  if (s === "SIGSTKFLT") { return "SIGSTKFLT"; }
  if (s === "SIGSTOP") { return "SIGSTOP"; }
  if (s === "SIGSYS") { return "SIGSYS"; }
  if (s === "SIGTERM") { return "SIGTERM"; }
  if (s === "SIGTRAP") { return "SIGTRAP"; }
  if (s === "SIGTSTP") { return "SIGTSTP"; }
  if (s === "SIGTTIN") { return "SIGTTIN"; }
  if (s === "SIGTTOU") { return "SIGTTOU"; }
  if (s === "SIGUNUSED") { return "SIGUNUSED"; }
  if (s === "SIGURG") { return "SIGURG"; }
  if (s === "SIGUSR1") { return "SIGUSR1"; }
  if (s === "SIGUSR2") { return "SIGUSR2"; }
  if (s === "SIGVTALRM") { return "SIGVTALRM"; }
  if (s === "SIGWINCH") { return "SIGWINCH"; }
  if (s === "SIGXCPU") { return "SIGXCPU"; }
  if (s === "SIGXFSZ") { return "SIGXFSZ"; }
  $runtime.fail();
};
const showSignal = {show: toString};
const fromString = s => {
  if (s === "SIGABRT") { return Data$dMaybe.$Maybe("Just", SIGABRT); }
  if (s === "SIGALRM") { return Data$dMaybe.$Maybe("Just", SIGALRM); }
  if (s === "SIGBUS") { return Data$dMaybe.$Maybe("Just", SIGBUS); }
  if (s === "SIGCHLD") { return Data$dMaybe.$Maybe("Just", SIGCHLD); }
  if (s === "SIGCLD") { return Data$dMaybe.$Maybe("Just", SIGCLD); }
  if (s === "SIGCONT") { return Data$dMaybe.$Maybe("Just", SIGCONT); }
  if (s === "SIGEMT") { return Data$dMaybe.$Maybe("Just", SIGEMT); }
  if (s === "SIGFPE") { return Data$dMaybe.$Maybe("Just", SIGFPE); }
  if (s === "SIGHUP") { return Data$dMaybe.$Maybe("Just", SIGHUP); }
  if (s === "SIGILL") { return Data$dMaybe.$Maybe("Just", SIGILL); }
  if (s === "SIGINFO") { return Data$dMaybe.$Maybe("Just", SIGINFO); }
  if (s === "SIGINT") { return Data$dMaybe.$Maybe("Just", SIGINT); }
  if (s === "SIGIO") { return Data$dMaybe.$Maybe("Just", SIGIO); }
  if (s === "SIGIOT") { return Data$dMaybe.$Maybe("Just", SIGIOT); }
  if (s === "SIGKILL") { return Data$dMaybe.$Maybe("Just", SIGKILL); }
  if (s === "SIGLOST") { return Data$dMaybe.$Maybe("Just", SIGLOST); }
  if (s === "SIGPIPE") { return Data$dMaybe.$Maybe("Just", SIGPIPE); }
  if (s === "SIGPOLL") { return Data$dMaybe.$Maybe("Just", SIGPOLL); }
  if (s === "SIGPROF") { return Data$dMaybe.$Maybe("Just", SIGPROF); }
  if (s === "SIGPWR") { return Data$dMaybe.$Maybe("Just", SIGPWR); }
  if (s === "SIGQUIT") { return Data$dMaybe.$Maybe("Just", SIGQUIT); }
  if (s === "SIGSEGV") { return Data$dMaybe.$Maybe("Just", SIGSEGV); }
  if (s === "SIGSTKFLT") { return Data$dMaybe.$Maybe("Just", SIGSTKFLT); }
  if (s === "SIGSTOP") { return Data$dMaybe.$Maybe("Just", SIGSTOP); }
  if (s === "SIGSYS") { return Data$dMaybe.$Maybe("Just", SIGSYS); }
  if (s === "SIGTERM") { return Data$dMaybe.$Maybe("Just", SIGTERM); }
  if (s === "SIGTRAP") { return Data$dMaybe.$Maybe("Just", SIGTRAP); }
  if (s === "SIGTSTP") { return Data$dMaybe.$Maybe("Just", SIGTSTP); }
  if (s === "SIGTTIN") { return Data$dMaybe.$Maybe("Just", SIGTTIN); }
  if (s === "SIGTTOU") { return Data$dMaybe.$Maybe("Just", SIGTTOU); }
  if (s === "SIGUNUSED") { return Data$dMaybe.$Maybe("Just", SIGUNUSED); }
  if (s === "SIGURG") { return Data$dMaybe.$Maybe("Just", SIGURG); }
  if (s === "SIGUSR1") { return Data$dMaybe.$Maybe("Just", SIGUSR1); }
  if (s === "SIGUSR2") { return Data$dMaybe.$Maybe("Just", SIGUSR2); }
  if (s === "SIGVTALRM") { return Data$dMaybe.$Maybe("Just", SIGVTALRM); }
  if (s === "SIGWINCH") { return Data$dMaybe.$Maybe("Just", SIGWINCH); }
  if (s === "SIGXCPU") { return Data$dMaybe.$Maybe("Just", SIGXCPU); }
  if (s === "SIGXFSZ") { return Data$dMaybe.$Maybe("Just", SIGXFSZ); }
  return Data$dMaybe.Nothing;
};
const eqSignal = {
  eq: x => y => {
    if (x === "SIGABRT") { return y === "SIGABRT"; }
    if (x === "SIGALRM") { return y === "SIGALRM"; }
    if (x === "SIGBUS") { return y === "SIGBUS"; }
    if (x === "SIGCHLD") { return y === "SIGCHLD"; }
    if (x === "SIGCLD") { return y === "SIGCLD"; }
    if (x === "SIGCONT") { return y === "SIGCONT"; }
    if (x === "SIGEMT") { return y === "SIGEMT"; }
    if (x === "SIGFPE") { return y === "SIGFPE"; }
    if (x === "SIGHUP") { return y === "SIGHUP"; }
    if (x === "SIGILL") { return y === "SIGILL"; }
    if (x === "SIGINFO") { return y === "SIGINFO"; }
    if (x === "SIGINT") { return y === "SIGINT"; }
    if (x === "SIGIO") { return y === "SIGIO"; }
    if (x === "SIGIOT") { return y === "SIGIOT"; }
    if (x === "SIGKILL") { return y === "SIGKILL"; }
    if (x === "SIGLOST") { return y === "SIGLOST"; }
    if (x === "SIGPIPE") { return y === "SIGPIPE"; }
    if (x === "SIGPOLL") { return y === "SIGPOLL"; }
    if (x === "SIGPROF") { return y === "SIGPROF"; }
    if (x === "SIGPWR") { return y === "SIGPWR"; }
    if (x === "SIGQUIT") { return y === "SIGQUIT"; }
    if (x === "SIGSEGV") { return y === "SIGSEGV"; }
    if (x === "SIGSTKFLT") { return y === "SIGSTKFLT"; }
    if (x === "SIGSTOP") { return y === "SIGSTOP"; }
    if (x === "SIGSYS") { return y === "SIGSYS"; }
    if (x === "SIGTERM") { return y === "SIGTERM"; }
    if (x === "SIGTRAP") { return y === "SIGTRAP"; }
    if (x === "SIGTSTP") { return y === "SIGTSTP"; }
    if (x === "SIGTTIN") { return y === "SIGTTIN"; }
    if (x === "SIGTTOU") { return y === "SIGTTOU"; }
    if (x === "SIGUNUSED") { return y === "SIGUNUSED"; }
    if (x === "SIGURG") { return y === "SIGURG"; }
    if (x === "SIGUSR1") { return y === "SIGUSR1"; }
    if (x === "SIGUSR2") { return y === "SIGUSR2"; }
    if (x === "SIGVTALRM") { return y === "SIGVTALRM"; }
    if (x === "SIGWINCH") { return y === "SIGWINCH"; }
    if (x === "SIGXCPU") { return y === "SIGXCPU"; }
    return x === "SIGXFSZ" && y === "SIGXFSZ";
  }
};
const ordSignal = {
  compare: x => y => {
    if (x === "SIGABRT") {
      if (y === "SIGABRT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGABRT") { return Data$dOrdering.GT; }
    if (x === "SIGALRM") {
      if (y === "SIGALRM") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGALRM") { return Data$dOrdering.GT; }
    if (x === "SIGBUS") {
      if (y === "SIGBUS") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGBUS") { return Data$dOrdering.GT; }
    if (x === "SIGCHLD") {
      if (y === "SIGCHLD") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGCHLD") { return Data$dOrdering.GT; }
    if (x === "SIGCLD") {
      if (y === "SIGCLD") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGCLD") { return Data$dOrdering.GT; }
    if (x === "SIGCONT") {
      if (y === "SIGCONT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGCONT") { return Data$dOrdering.GT; }
    if (x === "SIGEMT") {
      if (y === "SIGEMT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGEMT") { return Data$dOrdering.GT; }
    if (x === "SIGFPE") {
      if (y === "SIGFPE") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGFPE") { return Data$dOrdering.GT; }
    if (x === "SIGHUP") {
      if (y === "SIGHUP") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGHUP") { return Data$dOrdering.GT; }
    if (x === "SIGILL") {
      if (y === "SIGILL") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGILL") { return Data$dOrdering.GT; }
    if (x === "SIGINFO") {
      if (y === "SIGINFO") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGINFO") { return Data$dOrdering.GT; }
    if (x === "SIGINT") {
      if (y === "SIGINT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGINT") { return Data$dOrdering.GT; }
    if (x === "SIGIO") {
      if (y === "SIGIO") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGIO") { return Data$dOrdering.GT; }
    if (x === "SIGIOT") {
      if (y === "SIGIOT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGIOT") { return Data$dOrdering.GT; }
    if (x === "SIGKILL") {
      if (y === "SIGKILL") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGKILL") { return Data$dOrdering.GT; }
    if (x === "SIGLOST") {
      if (y === "SIGLOST") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGLOST") { return Data$dOrdering.GT; }
    if (x === "SIGPIPE") {
      if (y === "SIGPIPE") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGPIPE") { return Data$dOrdering.GT; }
    if (x === "SIGPOLL") {
      if (y === "SIGPOLL") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGPOLL") { return Data$dOrdering.GT; }
    if (x === "SIGPROF") {
      if (y === "SIGPROF") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGPROF") { return Data$dOrdering.GT; }
    if (x === "SIGPWR") {
      if (y === "SIGPWR") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGPWR") { return Data$dOrdering.GT; }
    if (x === "SIGQUIT") {
      if (y === "SIGQUIT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGQUIT") { return Data$dOrdering.GT; }
    if (x === "SIGSEGV") {
      if (y === "SIGSEGV") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGSEGV") { return Data$dOrdering.GT; }
    if (x === "SIGSTKFLT") {
      if (y === "SIGSTKFLT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGSTKFLT") { return Data$dOrdering.GT; }
    if (x === "SIGSTOP") {
      if (y === "SIGSTOP") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGSTOP") { return Data$dOrdering.GT; }
    if (x === "SIGSYS") {
      if (y === "SIGSYS") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGSYS") { return Data$dOrdering.GT; }
    if (x === "SIGTERM") {
      if (y === "SIGTERM") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGTERM") { return Data$dOrdering.GT; }
    if (x === "SIGTRAP") {
      if (y === "SIGTRAP") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGTRAP") { return Data$dOrdering.GT; }
    if (x === "SIGTSTP") {
      if (y === "SIGTSTP") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGTSTP") { return Data$dOrdering.GT; }
    if (x === "SIGTTIN") {
      if (y === "SIGTTIN") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGTTIN") { return Data$dOrdering.GT; }
    if (x === "SIGTTOU") {
      if (y === "SIGTTOU") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGTTOU") { return Data$dOrdering.GT; }
    if (x === "SIGUNUSED") {
      if (y === "SIGUNUSED") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGUNUSED") { return Data$dOrdering.GT; }
    if (x === "SIGURG") {
      if (y === "SIGURG") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGURG") { return Data$dOrdering.GT; }
    if (x === "SIGUSR1") {
      if (y === "SIGUSR1") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGUSR1") { return Data$dOrdering.GT; }
    if (x === "SIGUSR2") {
      if (y === "SIGUSR2") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGUSR2") { return Data$dOrdering.GT; }
    if (x === "SIGVTALRM") {
      if (y === "SIGVTALRM") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGVTALRM") { return Data$dOrdering.GT; }
    if (x === "SIGWINCH") {
      if (y === "SIGWINCH") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGWINCH") { return Data$dOrdering.GT; }
    if (x === "SIGXCPU") {
      if (y === "SIGXCPU") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGXCPU") { return Data$dOrdering.GT; }
    if (x === "SIGXFSZ" && y === "SIGXFSZ") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqSignal
};
export {
  $Signal,
  SIGABRT,
  SIGALRM,
  SIGBUS,
  SIGCHLD,
  SIGCLD,
  SIGCONT,
  SIGEMT,
  SIGFPE,
  SIGHUP,
  SIGILL,
  SIGINFO,
  SIGINT,
  SIGIO,
  SIGIOT,
  SIGKILL,
  SIGLOST,
  SIGPIPE,
  SIGPOLL,
  SIGPROF,
  SIGPWR,
  SIGQUIT,
  SIGSEGV,
  SIGSTKFLT,
  SIGSTOP,
  SIGSYS,
  SIGTERM,
  SIGTRAP,
  SIGTSTP,
  SIGTTIN,
  SIGTTOU,
  SIGUNUSED,
  SIGURG,
  SIGUSR1,
  SIGUSR2,
  SIGVTALRM,
  SIGWINCH,
  SIGXCPU,
  SIGXFSZ,
  eqSignal,
  fromString,
  ordSignal,
  showSignal,
  toString
};
