module Effect.FSM
  ( Machine
  , machine
  , send
  , receive
  , connect
  , ($$)
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Signal as S
import Signal.Channel as SC
import Signal.Effect as SE

data Machine i o = Machine (SC.Channel i) (S.Signal o)

machine
   ∷ ∀ s i o
   . (i → s → Effect (Tuple o s))
  → s
  → i
  → o
  → Effect (Machine i o)
machine f is ii io = do
  ci ← SC.channel ii
  co ← SC.channel io
  let si = SC.subscribe ci
      so = SC.subscribe co
      f' i s = f i s >>= \(Tuple o s') → SC.send co o $> s'
  _ ← SE.foldEffect f' is si
  pure $ Machine ci so

send ∷ ∀ i o. i → Machine i o → Effect Unit
send i (Machine ci _) = SC.send ci i

receive ∷ ∀ i o. Machine i o → (o → Effect Unit) → Effect Unit
receive (Machine _ so) f = void $ SE.mapEffect f <@> so

connect ∷ ∀ i io o. Machine i io → Machine io o → Effect (Machine i o)
connect (Machine i so) (Machine ci o) = do
  _ ← SE.mapEffect (SC.send ci) <@> so
  pure $ Machine i o

infixr 2 connect as $$
