module Test.Util.Debug where

import Test.Util.Debug.Defaults

tracing :: TracingConfig
tracing = tracingDefaults
   { intermediates = true
   }

checking :: CheckingConfig
checking = checkingDefaults
   { edgeListGC = true
   , edgeListSorted = true
   , inputsAreSinks = true
   , outputsInGraph = true
   }

testing :: TestingConfig
testing = testingDefaults
   { fwdPreservesTop = true
   , bwdDuals = true
   , fwdDuals = true
   , naiveFwd = true
   }

timing :: TimingConfig
timing = timingDefaults
