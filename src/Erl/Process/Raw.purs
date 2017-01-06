module Erl.Process.Raw
  ( Pid
  , spawn
  , spawnLink
  , PROCESS
  , send
  , receive
  , ExitMsg (..)
  , ExitReason (..)
  , receiveWithTrappedMsg
  , setTrappedExit
  ) where

import Prelude
import Control.Monad.Eff (Eff)

foreign import data Pid :: *

foreign import data PROCESS :: !

instance eqPid :: Eq Pid where
  eq = eqNative

foreign import eqNative :: forall a. a -> a -> Boolean

foreign import spawn :: forall eff. (Eff (process :: PROCESS | eff) Unit) -> Eff (process :: PROCESS | eff) Pid

foreign import spawnLink :: forall eff. (Eff (process :: PROCESS | eff) Unit) -> Eff (process :: PROCESS | eff) Pid

foreign import send :: forall eff a. Pid -> a -> Eff (process :: PROCESS | eff) Unit

foreign import receive :: forall eff a. Eff (process :: PROCESS | eff) a

data ExitMsg = ExitMsg Pid ExitReason

data ExitReason
  = Normal
  | Kill
  | Other String

foreign import receiveWithTrappedMsg :: forall eff a. (ExitMsg ->  Eff (process :: PROCESS | eff) Unit)
                                     -> Eff (process :: PROCESS | eff) a

-- TODO: Use a more general processFlag :: Flag -> Eff eff Flag
foreign import setTrappedExit :: forall eff. Boolean -> Eff eff Boolean
