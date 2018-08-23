module Logging.AppLogger where

import Control.Applicative (when)
import Control.Bind (bind, discard)
import Control.Category ((<<<))
import Data.Eq (class Eq, (==))
import Data.Function (($))
import Data.HeytingAlgebra ((&&))
import Data.Ord (class Ord, Ordering(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Effect (Effect)
import Effect.Console as EC
import Logging.BaseLogger (BaseLogger, buildLogger)
import Sentry.Raven (Raven, captureException, captureMessage, getContext)
import Simple.JSON (class WriteForeign, writeImpl, writeJSON)

------------ Log record structure ------------

-- TBD
-- type LogRecord = { message ∷ String }


---------- Importance level of a log ---------

data Level = Debug | Info | Warning | Error

instance eqLevel ∷ Eq Level where
  eq Debug Debug = true
  eq Info Info = true
  eq Warning Warning  = true
  eq Error Error = true
  eq _ _ = false

instance showLevel ∷ Show Level where
  show Debug = "debug"
  show Info = "info"
  show Warning = "warning"
  show Error = "error"

instance ordLevel ∷ Ord Level where
  compare Debug Debug = EQ
  compare Debug _ = LT
  compare Info Info = EQ
  compare Info Debug = GT
  compare Info _ = LT
  compare Warning Warning = EQ
  compare Warning Error = LT
  compare Warning _ = GT
  compare Error Error = EQ
  compare Error _ = GT

instance wrtFrgnLevel ∷ WriteForeign Level where
  writeImpl = writeImpl <<< show


--------- Additional implicit log context ----------
type SentryContext = { user ∷ Int}
type AppRaven h = Raven h SentryContext
type LogContext h = { raven ∷ AppRaven h }

--------- Application specific logger type ------------
type AppLogger h r = BaseLogger (LogContext h) Effect Level r


--------------- Simple console logger -----------------

type ConsoleLogConfig r =
 { level ∷ Level
 , filter ∷ r → Boolean
 , logContext ∷ Boolean }

consoleLogger ∷ ∀ h r
  . Show r
  ⇒ ConsoleLogConfig r
  → AppLogger h r
consoleLogger config = buildLogger config $ \ctx l r → do
  EC.log (show l <> ": " <> show r)
  when (config.logContext) $ do
    rctx ← getContext ctx.raven
    EC.log ("logged in context: " <> writeJSON rctx)

--------------- Sentry logger -----------------

type SentryLogConfig r =
 { level ∷ Level
 , filter ∷ r → Boolean
 , errorsAsExceptions ∷ Boolean }

sentryLogger ∷ ∀ h r
  . WriteForeign r
  ⇒ SentryLogConfig r
  → AppLogger h r
sentryLogger config = buildLogger config $ \ctx l r → do
  if l == Error && config.errorsAsExceptions then
      captureException ctx.raven r {level: l}
    else
      captureMessage ctx.raven r {level: l}
