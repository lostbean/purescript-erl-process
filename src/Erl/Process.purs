module Erl.Process
  ( REC
  , Process(..)
  , runProcess
  , receive
  , receiveWithTrappedMsg
  , send
  , (!)
  , spawn
  , spawn'
  , spawnLink
  , setTrappedExit
  , module RawExport
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Erl.Process.Raw as Raw
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Erl.Process.Raw (PROCESS, ExitMsg (..), ExitReason (..)) as RawExport

foreign import data REC :: * -> * -> !

newtype Process a = Process Raw.Pid

runProcess :: forall a. Process a -> Raw.Pid
runProcess (Process pid) = pid

instance eqProcess :: Eq (Process a) where
  eq a b = eq (runProcess a) (runProcess b)

receive :: forall z a eff. Eff (rec :: REC z a, process :: Raw.PROCESS | eff) a
receive = Raw.receive

receiveWithTrappedMsg :: forall z a eff. (Raw.ExitMsg ->  Eff (rec :: REC z a, process :: Raw.PROCESS | eff) Unit)
                      -> Eff (rec :: REC z a, process :: Raw.PROCESS | eff) a
receiveWithTrappedMsg = Raw.receiveWithTrappedMsg

send :: forall eff a. Process a -> a -> Eff (process :: Raw.PROCESS | eff) Unit
send p x = Raw.send (runProcess p) x

infixr 6 send as !

spawn' :: forall eff y b a. (forall z. Eff (rec :: REC z a, process :: Raw.PROCESS | eff) Unit)
       -> Eff (process :: Raw.PROCESS, rec :: REC y b | eff) (Process a)
spawn' e =  Process <$> Raw.spawn (coerce e)
  where
  coerce :: forall z r. Eff (rec :: REC z a, process :: Raw.PROCESS | eff) r
         -> Eff (process :: Raw.PROCESS, rec :: REC y b | eff) r
  coerce = unsafeCoerceEff

spawn :: forall eff a. (forall z. Eff (rec :: REC z a, process :: Raw.PROCESS | eff) Unit)
      -> Eff (process :: Raw.PROCESS | eff) (Process a)
spawn e =  Process <$> Raw.spawn (coerce e)
  where
  coerce :: forall z r. Eff (rec :: REC z a, process :: Raw.PROCESS | eff) r
         -> Eff (process :: Raw.PROCESS | eff) r
  coerce = unsafeCoerceEff

spawnLink :: forall eff y b a. (forall z. Eff (rec :: REC z a, process :: Raw.PROCESS | eff) Unit)
          -> Eff (process :: Raw.PROCESS, rec :: REC y b | eff) (Process a)
spawnLink e =  Process <$> Raw.spawnLink (coerce e)
  where
  coerce :: forall z r. Eff (rec :: REC z a, process :: Raw.PROCESS | eff) r
         -> Eff (rec :: REC y b, process :: Raw.PROCESS | eff) r
  coerce = unsafeCoerceEff

setTrappedExit :: forall eff. Boolean -> Eff eff Boolean
setTrappedExit = Raw.setTrappedExit
