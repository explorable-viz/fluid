import * as Data$dDateTime$dInstant from "../Data.DateTime.Instant/index.js";
import * as Data$dJSDate from "../Data.JSDate/index.js";
import * as Partial from "../Partial/index.js";
import {
  accessedTimeImpl,
  accessedTimeMsImpl,
  birthTimeImpl,
  birthtimeMsImpl,
  blkSizeImpl,
  blocksImpl,
  devImpl,
  gidImpl,
  inodeImpl,
  isBlockDeviceImpl,
  isCharacterDeviceImpl,
  isDirectoryImpl,
  isFIFOImpl,
  isFileImpl,
  isSocketImpl,
  isSymbolicLinkImpl,
  modeImpl,
  modifiedTimeImpl,
  modifiedTimeMsImpl,
  nlinkImpl,
  rdevImpl,
  showStatsObj,
  sizeImpl,
  statusChangedTimeImpl,
  statusChangedTimeMsImpl,
  uidImpl
} from "./foreign.js";
const uid = s => uidImpl(s);
const statusChangedTimeMs = s => statusChangedTimeMsImpl(s);
const statusChangedTime = s => {
  const $0 = Data$dJSDate.toInstant(statusChangedTimeImpl(s));
  if ($0.tag === "Just") { return Data$dDateTime$dInstant.toDateTime($0._1); }
  return Partial._crashWith("Impossible: `statusChangedTime` returned invalid DateTime value.");
};
const size = s => sizeImpl(s);
const showStats = {show: s => "Stats " + showStatsObj(s)};
const rdev = s => rdevImpl(s);
const nlink = s => nlinkImpl(s);
const modifiedTimeMs = s => modifiedTimeMsImpl(s);
const modifiedTime = s => {
  const $0 = Data$dJSDate.toInstant(modifiedTimeImpl(s));
  if ($0.tag === "Just") { return Data$dDateTime$dInstant.toDateTime($0._1); }
  return Partial._crashWith("Impossible: `modifiedTime` returned invalid DateTime value.");
};
const mode = s => modeImpl(s);
const isSymbolicLink = s => isSymbolicLinkImpl(s);
const isSocket = s => isSocketImpl(s);
const isFile = s => isFileImpl(s);
const isFIFO = s => isFIFOImpl(s);
const isDirectory = s => isDirectoryImpl(s);
const isCharacterDevice = s => isCharacterDeviceImpl(s);
const isBlockDevice = s => isBlockDeviceImpl(s);
const inode = s => inodeImpl(s);
const gid = s => gidImpl(s);
const dev = s => devImpl(s);
const blocks = s => blocksImpl(s);
const blkSize = s => blkSizeImpl(s);
const birthtimeMs = s => birthtimeMsImpl(s);
const birthTime = s => {
  const $0 = Data$dJSDate.toInstant(birthTimeImpl(s));
  if ($0.tag === "Just") { return Data$dDateTime$dInstant.toDateTime($0._1); }
  return Partial._crashWith("Impossible: `birthTime` returned invalid DateTime value.");
};
const accessedTimeMs = s => accessedTimeMsImpl(s);
const accessedTime = s => {
  const $0 = Data$dJSDate.toInstant(accessedTimeImpl(s));
  if ($0.tag === "Just") { return Data$dDateTime$dInstant.toDateTime($0._1); }
  return Partial._crashWith("Impossible: `accessedTime` returned invalid DateTime value.");
};
export {
  accessedTime,
  accessedTimeMs,
  birthTime,
  birthtimeMs,
  blkSize,
  blocks,
  dev,
  gid,
  inode,
  isBlockDevice,
  isCharacterDevice,
  isDirectory,
  isFIFO,
  isFile,
  isSocket,
  isSymbolicLink,
  mode,
  modifiedTime,
  modifiedTimeMs,
  nlink,
  rdev,
  showStats,
  size,
  statusChangedTime,
  statusChangedTimeMs,
  uid
};
export * from "./foreign.js";
