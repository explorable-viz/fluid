import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $ExitCode = tag => tag;
const Success = /* #__PURE__ */ $ExitCode("Success");
const $$Error = /* #__PURE__ */ $ExitCode("Error");
const MisuseOfShellBuiltins = /* #__PURE__ */ $ExitCode("MisuseOfShellBuiltins");
const CLIUsageError = /* #__PURE__ */ $ExitCode("CLIUsageError");
const DataFormatError = /* #__PURE__ */ $ExitCode("DataFormatError");
const CannotOpenInput = /* #__PURE__ */ $ExitCode("CannotOpenInput");
const AddresseeUnknown = /* #__PURE__ */ $ExitCode("AddresseeUnknown");
const HostNameUnknown = /* #__PURE__ */ $ExitCode("HostNameUnknown");
const ServiceUnavailable = /* #__PURE__ */ $ExitCode("ServiceUnavailable");
const InternalSoftwareError = /* #__PURE__ */ $ExitCode("InternalSoftwareError");
const SystemError = /* #__PURE__ */ $ExitCode("SystemError");
const CriticalOSFileMissing = /* #__PURE__ */ $ExitCode("CriticalOSFileMissing");
const CannotCreateOutputFile = /* #__PURE__ */ $ExitCode("CannotCreateOutputFile");
const IOError = /* #__PURE__ */ $ExitCode("IOError");
const TemporaryFailure = /* #__PURE__ */ $ExitCode("TemporaryFailure");
const RemoteError = /* #__PURE__ */ $ExitCode("RemoteError");
const PermissionDenied = /* #__PURE__ */ $ExitCode("PermissionDenied");
const ConfigurationError = /* #__PURE__ */ $ExitCode("ConfigurationError");
const CannotExecute = /* #__PURE__ */ $ExitCode("CannotExecute");
const CommandNotFound = /* #__PURE__ */ $ExitCode("CommandNotFound");
const InvalidExitArgument = /* #__PURE__ */ $ExitCode("InvalidExitArgument");
const SIGHUP = /* #__PURE__ */ $ExitCode("SIGHUP");
const SIGINT = /* #__PURE__ */ $ExitCode("SIGINT");
const SIGQUIT = /* #__PURE__ */ $ExitCode("SIGQUIT");
const SIGILL = /* #__PURE__ */ $ExitCode("SIGILL");
const SIGABRT = /* #__PURE__ */ $ExitCode("SIGABRT");
const SIGFPE = /* #__PURE__ */ $ExitCode("SIGFPE");
const SIGKILL = /* #__PURE__ */ $ExitCode("SIGKILL");
const SIGSEGV = /* #__PURE__ */ $ExitCode("SIGSEGV");
const SIGPIPE = /* #__PURE__ */ $ExitCode("SIGPIPE");
const SIGALRM = /* #__PURE__ */ $ExitCode("SIGALRM");
const SIGTERM = /* #__PURE__ */ $ExitCode("SIGTERM");
const showExitCode = {
  show: v => {
    if (v === "Success") { return "Success"; }
    if (v === "Error") { return "Error"; }
    if (v === "MisuseOfShellBuiltins") { return "MisuseOfShellBuiltins"; }
    if (v === "CLIUsageError") { return "CLIUsageError"; }
    if (v === "DataFormatError") { return "DataFormatError"; }
    if (v === "CannotOpenInput") { return "CannotOpenInput"; }
    if (v === "AddresseeUnknown") { return "AddresseeUnknown"; }
    if (v === "HostNameUnknown") { return "HostNameUnknown"; }
    if (v === "ServiceUnavailable") { return "ServiceUnavailable"; }
    if (v === "InternalSoftwareError") { return "InternalSoftwareError"; }
    if (v === "SystemError") { return "SystemError"; }
    if (v === "CriticalOSFileMissing") { return "CriticalOSFileMissing"; }
    if (v === "CannotCreateOutputFile") { return "CannotCreateOutputFile"; }
    if (v === "IOError") { return "IOError"; }
    if (v === "TemporaryFailure") { return "TemporaryFailure"; }
    if (v === "RemoteError") { return "RemoteError"; }
    if (v === "PermissionDenied") { return "PermissionDenied"; }
    if (v === "ConfigurationError") { return "ConfigurationError"; }
    if (v === "CannotExecute") { return "CannotExecute"; }
    if (v === "CommandNotFound") { return "CommandNotFound"; }
    if (v === "InvalidExitArgument") { return "InvalidExitArgument"; }
    if (v === "SIGHUP") { return "SIGHUP"; }
    if (v === "SIGINT") { return "SIGINT"; }
    if (v === "SIGQUIT") { return "SIGQUIT"; }
    if (v === "SIGILL") { return "SIGILL"; }
    if (v === "SIGABRT") { return "SIGABRT"; }
    if (v === "SIGFPE") { return "SIGFPE"; }
    if (v === "SIGKILL") { return "SIGKILL"; }
    if (v === "SIGSEGV") { return "SIGSEGV"; }
    if (v === "SIGPIPE") { return "SIGPIPE"; }
    if (v === "SIGALRM") { return "SIGALRM"; }
    if (v === "SIGTERM") { return "SIGTERM"; }
    $runtime.fail();
  }
};
const eqExitCode = {
  eq: x => y => {
    if (x === "Success") { return y === "Success"; }
    if (x === "Error") { return y === "Error"; }
    if (x === "MisuseOfShellBuiltins") { return y === "MisuseOfShellBuiltins"; }
    if (x === "CLIUsageError") { return y === "CLIUsageError"; }
    if (x === "DataFormatError") { return y === "DataFormatError"; }
    if (x === "CannotOpenInput") { return y === "CannotOpenInput"; }
    if (x === "AddresseeUnknown") { return y === "AddresseeUnknown"; }
    if (x === "HostNameUnknown") { return y === "HostNameUnknown"; }
    if (x === "ServiceUnavailable") { return y === "ServiceUnavailable"; }
    if (x === "InternalSoftwareError") { return y === "InternalSoftwareError"; }
    if (x === "SystemError") { return y === "SystemError"; }
    if (x === "CriticalOSFileMissing") { return y === "CriticalOSFileMissing"; }
    if (x === "CannotCreateOutputFile") { return y === "CannotCreateOutputFile"; }
    if (x === "IOError") { return y === "IOError"; }
    if (x === "TemporaryFailure") { return y === "TemporaryFailure"; }
    if (x === "RemoteError") { return y === "RemoteError"; }
    if (x === "PermissionDenied") { return y === "PermissionDenied"; }
    if (x === "ConfigurationError") { return y === "ConfigurationError"; }
    if (x === "CannotExecute") { return y === "CannotExecute"; }
    if (x === "CommandNotFound") { return y === "CommandNotFound"; }
    if (x === "InvalidExitArgument") { return y === "InvalidExitArgument"; }
    if (x === "SIGHUP") { return y === "SIGHUP"; }
    if (x === "SIGINT") { return y === "SIGINT"; }
    if (x === "SIGQUIT") { return y === "SIGQUIT"; }
    if (x === "SIGILL") { return y === "SIGILL"; }
    if (x === "SIGABRT") { return y === "SIGABRT"; }
    if (x === "SIGFPE") { return y === "SIGFPE"; }
    if (x === "SIGKILL") { return y === "SIGKILL"; }
    if (x === "SIGSEGV") { return y === "SIGSEGV"; }
    if (x === "SIGPIPE") { return y === "SIGPIPE"; }
    if (x === "SIGALRM") { return y === "SIGALRM"; }
    return x === "SIGTERM" && y === "SIGTERM";
  }
};
const ordExitCode = {
  compare: x => y => {
    if (x === "Success") {
      if (y === "Success") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Success") { return Data$dOrdering.GT; }
    if (x === "Error") {
      if (y === "Error") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Error") { return Data$dOrdering.GT; }
    if (x === "MisuseOfShellBuiltins") {
      if (y === "MisuseOfShellBuiltins") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "MisuseOfShellBuiltins") { return Data$dOrdering.GT; }
    if (x === "CLIUsageError") {
      if (y === "CLIUsageError") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "CLIUsageError") { return Data$dOrdering.GT; }
    if (x === "DataFormatError") {
      if (y === "DataFormatError") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "DataFormatError") { return Data$dOrdering.GT; }
    if (x === "CannotOpenInput") {
      if (y === "CannotOpenInput") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "CannotOpenInput") { return Data$dOrdering.GT; }
    if (x === "AddresseeUnknown") {
      if (y === "AddresseeUnknown") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "AddresseeUnknown") { return Data$dOrdering.GT; }
    if (x === "HostNameUnknown") {
      if (y === "HostNameUnknown") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "HostNameUnknown") { return Data$dOrdering.GT; }
    if (x === "ServiceUnavailable") {
      if (y === "ServiceUnavailable") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "ServiceUnavailable") { return Data$dOrdering.GT; }
    if (x === "InternalSoftwareError") {
      if (y === "InternalSoftwareError") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "InternalSoftwareError") { return Data$dOrdering.GT; }
    if (x === "SystemError") {
      if (y === "SystemError") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SystemError") { return Data$dOrdering.GT; }
    if (x === "CriticalOSFileMissing") {
      if (y === "CriticalOSFileMissing") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "CriticalOSFileMissing") { return Data$dOrdering.GT; }
    if (x === "CannotCreateOutputFile") {
      if (y === "CannotCreateOutputFile") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "CannotCreateOutputFile") { return Data$dOrdering.GT; }
    if (x === "IOError") {
      if (y === "IOError") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "IOError") { return Data$dOrdering.GT; }
    if (x === "TemporaryFailure") {
      if (y === "TemporaryFailure") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "TemporaryFailure") { return Data$dOrdering.GT; }
    if (x === "RemoteError") {
      if (y === "RemoteError") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "RemoteError") { return Data$dOrdering.GT; }
    if (x === "PermissionDenied") {
      if (y === "PermissionDenied") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "PermissionDenied") { return Data$dOrdering.GT; }
    if (x === "ConfigurationError") {
      if (y === "ConfigurationError") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "ConfigurationError") { return Data$dOrdering.GT; }
    if (x === "CannotExecute") {
      if (y === "CannotExecute") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "CannotExecute") { return Data$dOrdering.GT; }
    if (x === "CommandNotFound") {
      if (y === "CommandNotFound") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "CommandNotFound") { return Data$dOrdering.GT; }
    if (x === "InvalidExitArgument") {
      if (y === "InvalidExitArgument") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "InvalidExitArgument") { return Data$dOrdering.GT; }
    if (x === "SIGHUP") {
      if (y === "SIGHUP") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGHUP") { return Data$dOrdering.GT; }
    if (x === "SIGINT") {
      if (y === "SIGINT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGINT") { return Data$dOrdering.GT; }
    if (x === "SIGQUIT") {
      if (y === "SIGQUIT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGQUIT") { return Data$dOrdering.GT; }
    if (x === "SIGILL") {
      if (y === "SIGILL") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGILL") { return Data$dOrdering.GT; }
    if (x === "SIGABRT") {
      if (y === "SIGABRT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGABRT") { return Data$dOrdering.GT; }
    if (x === "SIGFPE") {
      if (y === "SIGFPE") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGFPE") { return Data$dOrdering.GT; }
    if (x === "SIGKILL") {
      if (y === "SIGKILL") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGKILL") { return Data$dOrdering.GT; }
    if (x === "SIGSEGV") {
      if (y === "SIGSEGV") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGSEGV") { return Data$dOrdering.GT; }
    if (x === "SIGPIPE") {
      if (y === "SIGPIPE") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGPIPE") { return Data$dOrdering.GT; }
    if (x === "SIGALRM") {
      if (y === "SIGALRM") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SIGALRM") { return Data$dOrdering.GT; }
    if (x === "SIGTERM" && y === "SIGTERM") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqExitCode
};
const enumExitCode = {
  succ: v => {
    if (v === "Success") { return Data$dMaybe.$Maybe("Just", $$Error); }
    if (v === "Error") { return Data$dMaybe.$Maybe("Just", MisuseOfShellBuiltins); }
    if (v === "MisuseOfShellBuiltins") { return Data$dMaybe.$Maybe("Just", CLIUsageError); }
    if (v === "CLIUsageError") { return Data$dMaybe.$Maybe("Just", DataFormatError); }
    if (v === "DataFormatError") { return Data$dMaybe.$Maybe("Just", CannotOpenInput); }
    if (v === "CannotOpenInput") { return Data$dMaybe.$Maybe("Just", AddresseeUnknown); }
    if (v === "AddresseeUnknown") { return Data$dMaybe.$Maybe("Just", HostNameUnknown); }
    if (v === "HostNameUnknown") { return Data$dMaybe.$Maybe("Just", ServiceUnavailable); }
    if (v === "ServiceUnavailable") { return Data$dMaybe.$Maybe("Just", InternalSoftwareError); }
    if (v === "InternalSoftwareError") { return Data$dMaybe.$Maybe("Just", SystemError); }
    if (v === "SystemError") { return Data$dMaybe.$Maybe("Just", CriticalOSFileMissing); }
    if (v === "CriticalOSFileMissing") { return Data$dMaybe.$Maybe("Just", CannotCreateOutputFile); }
    if (v === "CannotCreateOutputFile") { return Data$dMaybe.$Maybe("Just", IOError); }
    if (v === "IOError") { return Data$dMaybe.$Maybe("Just", TemporaryFailure); }
    if (v === "TemporaryFailure") { return Data$dMaybe.$Maybe("Just", RemoteError); }
    if (v === "RemoteError") { return Data$dMaybe.$Maybe("Just", PermissionDenied); }
    if (v === "PermissionDenied") { return Data$dMaybe.$Maybe("Just", ConfigurationError); }
    if (v === "ConfigurationError") { return Data$dMaybe.$Maybe("Just", CannotExecute); }
    if (v === "CannotExecute") { return Data$dMaybe.$Maybe("Just", CommandNotFound); }
    if (v === "CommandNotFound") { return Data$dMaybe.$Maybe("Just", InvalidExitArgument); }
    if (v === "InvalidExitArgument") { return Data$dMaybe.$Maybe("Just", SIGHUP); }
    if (v === "SIGHUP") { return Data$dMaybe.$Maybe("Just", SIGINT); }
    if (v === "SIGINT") { return Data$dMaybe.$Maybe("Just", SIGQUIT); }
    if (v === "SIGQUIT") { return Data$dMaybe.$Maybe("Just", SIGILL); }
    if (v === "SIGILL") { return Data$dMaybe.$Maybe("Just", SIGABRT); }
    if (v === "SIGABRT") { return Data$dMaybe.$Maybe("Just", SIGFPE); }
    if (v === "SIGFPE") { return Data$dMaybe.$Maybe("Just", SIGKILL); }
    if (v === "SIGKILL") { return Data$dMaybe.$Maybe("Just", SIGSEGV); }
    if (v === "SIGSEGV") { return Data$dMaybe.$Maybe("Just", SIGPIPE); }
    if (v === "SIGPIPE") { return Data$dMaybe.$Maybe("Just", SIGALRM); }
    if (v === "SIGALRM") { return Data$dMaybe.$Maybe("Just", SIGTERM); }
    if (v === "SIGTERM") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  },
  pred: v => {
    if (v === "Success") { return Data$dMaybe.Nothing; }
    if (v === "Error") { return Data$dMaybe.$Maybe("Just", Success); }
    if (v === "MisuseOfShellBuiltins") { return Data$dMaybe.$Maybe("Just", $$Error); }
    if (v === "CLIUsageError") { return Data$dMaybe.$Maybe("Just", MisuseOfShellBuiltins); }
    if (v === "DataFormatError") { return Data$dMaybe.$Maybe("Just", CLIUsageError); }
    if (v === "CannotOpenInput") { return Data$dMaybe.$Maybe("Just", DataFormatError); }
    if (v === "AddresseeUnknown") { return Data$dMaybe.$Maybe("Just", CannotOpenInput); }
    if (v === "HostNameUnknown") { return Data$dMaybe.$Maybe("Just", AddresseeUnknown); }
    if (v === "ServiceUnavailable") { return Data$dMaybe.$Maybe("Just", HostNameUnknown); }
    if (v === "InternalSoftwareError") { return Data$dMaybe.$Maybe("Just", ServiceUnavailable); }
    if (v === "SystemError") { return Data$dMaybe.$Maybe("Just", InternalSoftwareError); }
    if (v === "CriticalOSFileMissing") { return Data$dMaybe.$Maybe("Just", SystemError); }
    if (v === "CannotCreateOutputFile") { return Data$dMaybe.$Maybe("Just", CriticalOSFileMissing); }
    if (v === "IOError") { return Data$dMaybe.$Maybe("Just", CannotCreateOutputFile); }
    if (v === "TemporaryFailure") { return Data$dMaybe.$Maybe("Just", IOError); }
    if (v === "RemoteError") { return Data$dMaybe.$Maybe("Just", TemporaryFailure); }
    if (v === "PermissionDenied") { return Data$dMaybe.$Maybe("Just", RemoteError); }
    if (v === "ConfigurationError") { return Data$dMaybe.$Maybe("Just", PermissionDenied); }
    if (v === "CannotExecute") { return Data$dMaybe.$Maybe("Just", ConfigurationError); }
    if (v === "CommandNotFound") { return Data$dMaybe.$Maybe("Just", CannotExecute); }
    if (v === "InvalidExitArgument") { return Data$dMaybe.$Maybe("Just", CommandNotFound); }
    if (v === "SIGHUP") { return Data$dMaybe.$Maybe("Just", InvalidExitArgument); }
    if (v === "SIGINT") { return Data$dMaybe.$Maybe("Just", SIGHUP); }
    if (v === "SIGQUIT") { return Data$dMaybe.$Maybe("Just", SIGINT); }
    if (v === "SIGILL") { return Data$dMaybe.$Maybe("Just", SIGQUIT); }
    if (v === "SIGABRT") { return Data$dMaybe.$Maybe("Just", SIGILL); }
    if (v === "SIGFPE") { return Data$dMaybe.$Maybe("Just", SIGABRT); }
    if (v === "SIGKILL") { return Data$dMaybe.$Maybe("Just", SIGFPE); }
    if (v === "SIGSEGV") { return Data$dMaybe.$Maybe("Just", SIGKILL); }
    if (v === "SIGPIPE") { return Data$dMaybe.$Maybe("Just", SIGSEGV); }
    if (v === "SIGALRM") { return Data$dMaybe.$Maybe("Just", SIGPIPE); }
    if (v === "SIGTERM") { return Data$dMaybe.$Maybe("Just", SIGALRM); }
    $runtime.fail();
  },
  Ord0: () => ordExitCode
};
const boundedExitCode = {bottom: Success, top: SIGTERM, Ord0: () => ordExitCode};
const boundedEnumExitCode = {
  cardinality: 32,
  toEnum: v => {
    if (v === 0) { return Data$dMaybe.$Maybe("Just", Success); }
    if (v === 1) { return Data$dMaybe.$Maybe("Just", $$Error); }
    if (v === 2) { return Data$dMaybe.$Maybe("Just", MisuseOfShellBuiltins); }
    if (v === 64) { return Data$dMaybe.$Maybe("Just", CLIUsageError); }
    if (v === 65) { return Data$dMaybe.$Maybe("Just", DataFormatError); }
    if (v === 66) { return Data$dMaybe.$Maybe("Just", CannotOpenInput); }
    if (v === 67) { return Data$dMaybe.$Maybe("Just", AddresseeUnknown); }
    if (v === 68) { return Data$dMaybe.$Maybe("Just", HostNameUnknown); }
    if (v === 69) { return Data$dMaybe.$Maybe("Just", ServiceUnavailable); }
    if (v === 70) { return Data$dMaybe.$Maybe("Just", InternalSoftwareError); }
    if (v === 71) { return Data$dMaybe.$Maybe("Just", SystemError); }
    if (v === 72) { return Data$dMaybe.$Maybe("Just", CriticalOSFileMissing); }
    if (v === 73) { return Data$dMaybe.$Maybe("Just", CannotCreateOutputFile); }
    if (v === 74) { return Data$dMaybe.$Maybe("Just", IOError); }
    if (v === 75) { return Data$dMaybe.$Maybe("Just", TemporaryFailure); }
    if (v === 76) { return Data$dMaybe.$Maybe("Just", RemoteError); }
    if (v === 77) { return Data$dMaybe.$Maybe("Just", PermissionDenied); }
    if (v === 78) { return Data$dMaybe.$Maybe("Just", ConfigurationError); }
    if (v === 126) { return Data$dMaybe.$Maybe("Just", CannotExecute); }
    if (v === 127) { return Data$dMaybe.$Maybe("Just", CommandNotFound); }
    if (v === 128) { return Data$dMaybe.$Maybe("Just", InvalidExitArgument); }
    if (v === 129) { return Data$dMaybe.$Maybe("Just", SIGHUP); }
    if (v === 130) { return Data$dMaybe.$Maybe("Just", SIGINT); }
    if (v === 131) { return Data$dMaybe.$Maybe("Just", SIGQUIT); }
    if (v === 132) { return Data$dMaybe.$Maybe("Just", SIGILL); }
    if (v === 134) { return Data$dMaybe.$Maybe("Just", SIGABRT); }
    if (v === 136) { return Data$dMaybe.$Maybe("Just", SIGFPE); }
    if (v === 137) { return Data$dMaybe.$Maybe("Just", SIGKILL); }
    if (v === 139) { return Data$dMaybe.$Maybe("Just", SIGSEGV); }
    if (v === 141) { return Data$dMaybe.$Maybe("Just", SIGPIPE); }
    if (v === 142) { return Data$dMaybe.$Maybe("Just", SIGALRM); }
    if (v === 143) { return Data$dMaybe.$Maybe("Just", SIGTERM); }
    return Data$dMaybe.Nothing;
  },
  fromEnum: v => {
    if (v === "Success") { return 0; }
    if (v === "Error") { return 1; }
    if (v === "MisuseOfShellBuiltins") { return 2; }
    if (v === "CLIUsageError") { return 64; }
    if (v === "DataFormatError") { return 65; }
    if (v === "CannotOpenInput") { return 66; }
    if (v === "AddresseeUnknown") { return 67; }
    if (v === "HostNameUnknown") { return 68; }
    if (v === "ServiceUnavailable") { return 69; }
    if (v === "InternalSoftwareError") { return 70; }
    if (v === "SystemError") { return 71; }
    if (v === "CriticalOSFileMissing") { return 72; }
    if (v === "CannotCreateOutputFile") { return 73; }
    if (v === "IOError") { return 74; }
    if (v === "TemporaryFailure") { return 75; }
    if (v === "RemoteError") { return 76; }
    if (v === "PermissionDenied") { return 77; }
    if (v === "ConfigurationError") { return 78; }
    if (v === "CannotExecute") { return 126; }
    if (v === "CommandNotFound") { return 127; }
    if (v === "InvalidExitArgument") { return 128; }
    if (v === "SIGHUP") { return 129; }
    if (v === "SIGINT") { return 130; }
    if (v === "SIGQUIT") { return 131; }
    if (v === "SIGILL") { return 132; }
    if (v === "SIGABRT") { return 134; }
    if (v === "SIGFPE") { return 136; }
    if (v === "SIGKILL") { return 137; }
    if (v === "SIGSEGV") { return 139; }
    if (v === "SIGPIPE") { return 141; }
    if (v === "SIGALRM") { return 142; }
    if (v === "SIGTERM") { return 143; }
    $runtime.fail();
  },
  Bounded0: () => boundedExitCode,
  Enum1: () => enumExitCode
};
export {
  $ExitCode,
  AddresseeUnknown,
  CLIUsageError,
  CannotCreateOutputFile,
  CannotExecute,
  CannotOpenInput,
  CommandNotFound,
  ConfigurationError,
  CriticalOSFileMissing,
  DataFormatError,
  $$Error as Error,
  HostNameUnknown,
  IOError,
  InternalSoftwareError,
  InvalidExitArgument,
  MisuseOfShellBuiltins,
  PermissionDenied,
  RemoteError,
  SIGABRT,
  SIGALRM,
  SIGFPE,
  SIGHUP,
  SIGILL,
  SIGINT,
  SIGKILL,
  SIGPIPE,
  SIGQUIT,
  SIGSEGV,
  SIGTERM,
  ServiceUnavailable,
  Success,
  SystemError,
  TemporaryFailure,
  boundedEnumExitCode,
  boundedExitCode,
  enumExitCode,
  eqExitCode,
  ordExitCode,
  showExitCode
};
