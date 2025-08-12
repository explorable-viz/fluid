module Test.Specs.ParagraphComments where

import Test.Util.Suite (TestSpec)

paragraph_comments_cases :: Array TestSpec
paragraph_comments_cases =
   [ { file: "paragraph/basic"
     , imports: []
     , fwd_expect:
          "Paragraph (Text \"Hello\" : (Text \"there,\" : (Text Paragraph (Text \"Alice\" : []) : (Text \"!\" : []))))"
     }
   , { file: "paragraph/explicit"
     , imports: []
     , fwd_expect:
          "Paragraph (Text \"Hi \" : (Text \"Alice\" : (Text \"!\" : [])))"
     }
   ]