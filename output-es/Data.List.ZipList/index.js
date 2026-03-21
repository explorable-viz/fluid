// | This module defines the type of _zip lists_, i.e. linked lists
// | with a zippy `Applicative` instance.
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dList$dLazy from "../Data.List.Lazy/index.js";
import * as Data$dList$dLazy$dTypes from "../Data.List.Lazy.Types/index.js";
import * as Partial from "../Partial/index.js";
const ZipList = x => x;
const traversableZipList = Data$dList$dLazy$dTypes.traversableList;
const showZipList = dictShow => ({show: v => "(ZipList " + Data$dList$dLazy$dTypes.showList(dictShow).show(v) + ")"});
const semigroupZipList = Data$dList$dLazy$dTypes.semigroupList;
const ordZipList = dictOrd => Data$dList$dLazy$dTypes.ordList(dictOrd);
const newtypeZipList = {Coercible0: () => {}};
const monoidZipList = Data$dList$dLazy$dTypes.monoidList;
const functorZipList = Data$dList$dLazy$dTypes.functorList;
const foldableZipList = Data$dList$dLazy$dTypes.foldableList;
const eqZipList = dictEq => ({eq: Data$dList$dLazy$dTypes.eq1List.eq1(dictEq)});
const applyZipList = {apply: v => v1 => Data$dList$dLazy.zipWith(Data$dFunction.apply)(v)(v1), Functor0: () => Data$dList$dLazy$dTypes.functorList};
const zipListIsNotBind = () => ({bind: Partial._crashWith("bind: unreachable"), Apply0: () => applyZipList});
const applicativeZipList = {pure: x => Data$dList$dLazy.repeat(x), Apply0: () => applyZipList};
const altZipList = {
  alt: v => v1 => Data$dList$dLazy$dTypes.semigroupList.append(v)(Data$dList$dLazy.drop(Data$dList$dLazy.length(v))(v1)),
  Functor0: () => Data$dList$dLazy$dTypes.functorList
};
const plusZipList = {empty: Data$dList$dLazy$dTypes.nil, Alt0: () => altZipList};
const alternativeZipList = {Applicative0: () => applicativeZipList, Plus1: () => plusZipList};
export {
  ZipList,
  altZipList,
  alternativeZipList,
  applicativeZipList,
  applyZipList,
  eqZipList,
  foldableZipList,
  functorZipList,
  monoidZipList,
  newtypeZipList,
  ordZipList,
  plusZipList,
  semigroupZipList,
  showZipList,
  traversableZipList,
  zipListIsNotBind
};
