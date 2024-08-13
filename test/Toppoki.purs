module Test.Toppokki where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried as FU
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Effect.Uncurried as EU
import Foreign (Foreign)
import Node.Buffer (Buffer)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

foreign import data Puppeteer :: Type
foreign import data Browser :: Type
foreign import data Page :: Type
foreign import data ElementHandle :: Type

-- This is used when one wants to use chrome-aws-lambda version of puppeteer
foreign import data ChromeAWS :: Type

newtype URL = URL String

derive instance newtypeURL :: Newtype URL _

newtype Selector = Selector String

derive instance newtypeSelector :: Newtype Selector _

type LaunchOptions =
   ( args :: Array String
   , defaultViewport :: Record DefaultViewPort
   , devtools :: Boolean
   , dumpio :: Boolean
   , executablePath :: String
   , handleSIGHUP :: Boolean
   , handleSIGINT :: Boolean
   , handleSIGTERM :: Boolean
   , headless :: Boolean
   , ignoreDefaultArgs :: Array String
   , ignoreHTTPSErrors :: Boolean
   , pipe :: Boolean
   , slowMo :: Number
   , timeout :: Number
   , userDataDir :: String
   )

type DefaultViewPort =
   ( width :: Number
   , height :: Number
   , deviceScaleFactor :: Number
   , isMobile :: Boolean
   , hasTouch :: Boolean
   , isLandscape :: Boolean
   )

launch
   :: forall options trash
    . Row.Union options trash LaunchOptions
   => { | options }
   -> Aff Browser
launch = runPromiseAffE1 _launch

launchChromeAWS
   :: forall options trash
    . Row.Union options trash LaunchOptions
   => ChromeAWS
   -> { | options }
   -> Aff Browser
launchChromeAWS = runPromiseAffE2 _launchChromeAWS

newPage :: Browser -> Aff Page
newPage = runPromiseAffE1 _newPage

goto
   :: forall options trash
    . Row.Union options trash
         ( timeout :: Int
         , waitUntil :: WaitUntilOption
         )
   => URL
   -> { | options }
   -> Page
   -> Aff Unit
goto = runPromiseAffE3 _goto

close :: Browser -> Aff Unit
close = runPromiseAffE1 _close

content :: Page -> Aff String
content = runPromiseAffE1 _content

type ScreenshotOptions =
   ( path :: String
   , type :: String
   , quality :: Int
   , fullPage :: Boolean
   , clip ::
        { x :: Int
        , y :: Int
        , width :: Int
        , height :: Int
        }
   , omitBackground :: Boolean
   )

screenshot
   :: forall options trash
    . Row.Union options trash ScreenshotOptions
   => { | options }
   -> Page
   -> Aff Buffer
screenshot o p = runPromiseAffE2 _screenshot o p

foreign import data PDFMargin :: Type

type PDFMarginOptions =
   ( top :: String
   , right :: String
   , bottom :: String
   , left :: String
   )

makePDFMargin
   :: forall options trash
    . Row.Union options trash PDFMarginOptions
   => { | options }
   -> PDFMargin
makePDFMargin = unsafeCoerce

type PDFOptions =
   ( path :: String
   , scale :: Int
   , displayHeaderFooter :: Boolean
   , headerTemplate :: String
   , footerTemplate :: String
   , printBackground :: Boolean
   , landscape :: Boolean
   , pageRanges :: String
   , format :: String
   , width :: String
   , height :: String
   , margin :: PDFMargin
   )

pdf
   :: forall options trash
    . Row.Union options trash PDFOptions
   => { | options }
   -> Page
   -> Aff Buffer
pdf = runPromiseAffE2 _pdf

onPageError :: EU.EffectFn1 Error Unit -> Page -> Effect Unit
onPageError = EU.runEffectFn3 _on "pageerror"

onLoad :: EU.EffectFn1 Unit Unit -> Page -> Effect Unit
onLoad = EU.runEffectFn3 _on "load"

pageWaitForSelector
   :: forall options trash
    . Row.Union options trash
         ( visible :: Boolean
         , hidden :: Boolean
         , timeout :: Int
         )
   => Selector
   -> { | options }
   -> Page
   -> Aff ElementHandle
pageWaitForSelector = runPromiseAffE3 _pageWaitForSelector

waitForReadyStateComplete :: Page -> Aff Unit
waitForReadyStateComplete = runPromiseAffE1 _waitForReadyStateComplete

focus :: Selector -> Page -> Aff Unit
focus = runPromiseAffE2 _focus

-- | Select a specific option in select dropdown group.
select
   :: Selector
   -> String
   -> Page
   -> Aff Unit
select = runPromiseAffE3 _select

type_
   :: forall options trash
    . Row.Union options trash
         ( delay :: Int
         )
   => Selector
   -> String
   -> { | options }
   -> Page
   -> Aff Unit
type_ = runPromiseAffE4 _type

click :: Selector -> Page -> Aff Unit
click = runPromiseAffE2 _click

foreign import data WaitUntilOption :: Type

load :: WaitUntilOption
load = unsafeCoerce $ "load"

domContentLoaded :: WaitUntilOption
domContentLoaded = unsafeCoerce $ "domcontentloaded"

networkIdle :: WaitUntilOption
networkIdle = unsafeCoerce $ "networkidle"

networkIdle0 :: WaitUntilOption
networkIdle0 = unsafeCoerce $ "networkidle0"

networkIdle2 :: WaitUntilOption
networkIdle2 = unsafeCoerce $ "networkidle2"

waitForNavigation
   :: forall options trash
    . Row.Union options trash
         ( waitUntil :: WaitUntilOption
         )
   => { | options }
   -> Page
   -> Aff Unit
waitForNavigation = runPromiseAffE2 _waitForNavigation

getLocationRef :: Page -> Aff String
getLocationRef p = Promise.toAffE $ FU.runFn1 _getLocationHref p

-- | The function is invoked after the document was created but before any of its scripts were run.
-- |
-- | This is useful to amend the environment, e.g. to seed Math.random.
unsafeEvaluateOnNewDocument :: String -> Page -> Aff Foreign
unsafeEvaluateOnNewDocument = runPromiseAffE2 _unsafeEvaluateOnNewDocument

unsafeEvaluateStringFunction :: String -> Page -> Aff Foreign
unsafeEvaluateStringFunction = runPromiseAffE2 _unsafeEvaluateStringFunction

-- | This method runs document.querySelector within the page and passes it as the first argument to pageFunction. If there's no element matching selector, the method throws an error.
-- |
-- | Second argument is a pageFunction which should be a valid JavaScript function written as a string which we unsafely eval.
-- |
-- | If pageFunction returns a Promise, then page.$$eval would wait for the promise to resolve and return its value.
unsafePageEval :: Selector -> String -> Page -> Aff Foreign
unsafePageEval = runPromiseAffE3 _unsafePageEval

-- | This method runs Array.from(document.querySelectorAll(selector)) within the page and passes it as the first argument to pageFunction.
-- |
-- | Second argument is a pageFunction which should be a valid JavaScript function written as a string which we unsafely eval.
-- |
-- | If pageFunction returns a Promise, then page.$$eval would wait for the promise to resolve and return its value.
unsafePageEvalAll :: Selector -> String -> Page -> Aff Foreign
unsafePageEvalAll = runPromiseAffE3 _unsafePageEvalAll

runPromiseAffE1 :: forall a o. FU.Fn1 a (Effect (Promise o)) -> a -> Aff o
runPromiseAffE1 f a = Promise.toAffE $ FU.runFn1 f a

runPromiseAffE2 :: forall a b o. FU.Fn2 a b (Effect (Promise o)) -> a -> b -> Aff o
runPromiseAffE2 f a b = Promise.toAffE $ FU.runFn2 f a b

runPromiseAffE3 :: forall a b c o. FU.Fn3 a b c (Effect (Promise o)) -> a -> b -> c -> Aff o
runPromiseAffE3 f a b c = Promise.toAffE $ FU.runFn3 f a b c

runPromiseAffE4 :: forall a b c d o. FU.Fn4 a b c d (Effect (Promise o)) -> a -> b -> c -> d -> Aff o
runPromiseAffE4 f a b c d = Promise.toAffE $ FU.runFn4 f a b c d

-- | See [USKeyboardLayout](https://github.com/GoogleChrome/puppeteer/blob/v1.18.1/lib/USKeyboardLayout.js) for a list of all key names.
newtype KeyboardKey = KeyboardKey String

-- | Dispatches a keydown event.
keyboardDown
   :: forall options trash
    . Row.Union options trash (text :: String)
   => KeyboardKey
   -> { | options }
   -> Page
   -> Aff Unit
keyboardDown = runPromiseAffE3 _keyboardDown

-- | Trigger a single keypress. Shortcut for `keyboard.down` and `keyboard.up`.
keyboardPress
   :: forall options trash
    . Row.Union options trash
         ( delay :: Int
         , text :: String
         )
   => KeyboardKey
   -> { | options }
   -> Page
   -> Aff Unit
keyboardPress = runPromiseAffE3 _keyboardPress

-- | Dispatches a keypress and input event. This does not send a keydown or keyup event.
keyboardSendCharacter :: String -> Page -> Aff Unit
keyboardSendCharacter = runPromiseAffE2 _keyboardSendCharacter

-- | Sends a keydown, keypress/input, and keyup event for each character in the text.
-- | To press a special key, like Control or ArrowDown, use keyboard.press.
keyboardType
   :: forall options trash
    . Row.Union options trash (delay :: Number)
   => String
   -> { | options }
   -> Page
   -> Aff Unit
keyboardType = runPromiseAffE3 _keyboardType

-- | Dispatches a keyup event.
keyboardUp
   :: forall options trash
    . Row.Union options trash (text :: String)
   => KeyboardKey
   -> { | options }
   -> Page
   -> Aff Unit
keyboardUp = runPromiseAffE3 _keyboardUp

-- | set Viewport
setViewport :: Record DefaultViewPort -> Page -> Aff Unit
setViewport = runPromiseAffE2 _setViewport

-- | Specific user agent to use in this page
setUserAgent :: String -> Page -> Aff Unit
setUserAgent = runPromiseAffE2 _setUserAgent

-- | Brings page to front (activates tab)
bringToFront :: Page -> Aff Unit
bringToFront = runPromiseAffE1 _bringToFront

foreign import puppeteer :: Puppeteer
foreign import _launch :: forall options. FU.Fn1 options (Effect (Promise Browser))
foreign import _launchChromeAWS :: forall options. FU.Fn2 ChromeAWS options (Effect (Promise Browser))
foreign import _newPage :: FU.Fn1 Browser (Effect (Promise Page))
foreign import _goto :: forall options. FU.Fn3 URL options Page (Effect (Promise Unit))
foreign import _close :: FU.Fn1 Browser (Effect (Promise Unit))
foreign import _content :: FU.Fn1 Page (Effect (Promise String))
foreign import _screenshot :: forall options. FU.Fn2 options Page (Effect (Promise Buffer))
foreign import _pdf :: forall options. FU.Fn2 options Page (Effect (Promise Buffer))
foreign import _on :: forall a. EU.EffectFn3 String (EU.EffectFn1 a Unit) Page Unit
foreign import _pageWaitForSelector :: forall options. FU.Fn3 Selector options Page (Effect (Promise ElementHandle))
foreign import _waitForReadyStateComplete :: FU.Fn1 Page (Effect (Promise Unit))
foreign import _select :: forall options. FU.Fn3 Selector options Page (Effect (Promise Unit))
foreign import _focus :: FU.Fn2 Selector Page (Effect (Promise Unit))
foreign import _setViewport :: FU.Fn2 (Record DefaultViewPort) Page (Effect (Promise Unit))
foreign import _type :: forall options. FU.Fn4 Selector String options Page (Effect (Promise Unit))
foreign import _click :: FU.Fn2 Selector Page (Effect (Promise Unit))
foreign import _waitForNavigation :: forall options. FU.Fn2 options Page (Effect (Promise Unit))
foreign import _getLocationHref :: FU.Fn1 Page (Effect (Promise String))
foreign import _unsafeEvaluateOnNewDocument :: FU.Fn2 String Page (Effect (Promise Foreign))
foreign import _unsafeEvaluateStringFunction :: FU.Fn2 String Page (Effect (Promise Foreign))
foreign import _unsafePageEval :: FU.Fn3 Selector String Page (Effect (Promise Foreign))
foreign import _unsafePageEvalAll :: FU.Fn3 Selector String Page (Effect (Promise Foreign))
foreign import _keyboardDown :: forall options. FU.Fn3 KeyboardKey options Page (Effect (Promise Unit))
foreign import _keyboardPress :: forall options. FU.Fn3 KeyboardKey options Page (Effect (Promise Unit))
foreign import _keyboardSendCharacter :: FU.Fn2 String Page (Effect (Promise Unit))
foreign import _keyboardType :: forall options. FU.Fn3 String options Page (Effect (Promise Unit))
foreign import _keyboardUp :: forall options. FU.Fn3 KeyboardKey options Page (Effect (Promise Unit))
foreign import _setUserAgent :: FU.Fn2 String Page (Effect (Promise Unit))
foreign import _bringToFront :: FU.Fn1 Page (Effect (Promise Unit))
