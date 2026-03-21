const tracingDefaults = {
  runWithGraphT: false,
  graphBwdSlice: false,
  graphBwdSlice_vertexData: false,
  graphFwdSlice: false,
  checkEq: false,
  bwdSelection: false,
  fwdAfterBwd: false,
  mediatingData: false,
  mouseEvent: false,
  intermediates: true
};
const timingDefaults = {selectionResult: false};
const testingDefaults = {fwdPreservesTop: false, bwdDuals: false, fwdDuals: false, naiveFwd: false};
const checkingDefaults = {edgeListGC: false, edgeListSorted: false, inputsAreSinks: false, outputsInGraph: false, allocRoundTrip: false, mustEq: false};
export {checkingDefaults, testingDefaults, timingDefaults, tracingDefaults};
