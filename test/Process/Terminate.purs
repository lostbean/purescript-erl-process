module Test.Process.Terminate where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Erl.Process (PROCESS, REC, ExitMsg (..), ExitReason (..), (!))
import Erl.Process as P

start :: forall eff z. Eff (rec :: REC z Unit, process :: PROCESS, console :: CONSOLE | eff) Unit
start = do
  log "Spawning monitor & worker"
  _   <- P.spawnLink monitor
  pid <- P.spawnLink inverse
  log "testing ..."
  pid ! 1
  pid ! 0

monitor :: forall eff z. Eff (rec :: REC z Unit, process :: PROCESS, console :: CONSOLE | eff) Unit
monitor = do
  P.setTrappedExit true
  _ <- P.receiveWithTrappedMsg handler
  monitor
  where
    handler x = case x of
      ExitMsg pid Normal    -> log "Oh, That's fine!"
      ExitMsg pid Kill      -> log "You shouldn't see this!"
      ExitMsg pid (Other m) -> log "Bang! Bang!"

inverse :: forall z eff. Eff (rec :: REC z Int, process :: PROCESS, console :: CONSOLE | eff) Unit
inverse = do
  x :: Int <- P.receive
  log $ show (1 `div` x)
  inverse
