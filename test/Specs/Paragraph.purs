module Test.Specs.Paragraph where

import Test.Util.Suite (TestSpec)

paragraph_cases :: Array TestSpec
paragraph_cases =
   [ { file: "paragraph/basic"
     , fwd_expect:
          "Paragraph (Text \"Hello\" : (Text \"there,\" : (Text Paragraph (Text \"Alice\" : []) : (Text \"!\" : []))))"
     }
   , { file: "paragraph/explicit"
     , fwd_expect:
          "Paragraph (Text \"Hi \" : (Text \"Alice\" : (Text \"!\" : [])))"
     }
   ]
