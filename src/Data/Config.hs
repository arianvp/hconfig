{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | Applicative configuration DSL.
module Data.Config
  ( Config
  , ConfigF(..)
  , text
  , int
  , optional
  , prefix
  ) where

import Control.Applicative.Free (Ap, liftAp)
import Data.Text (Text)

-- | A computation which produces configuration.
type Config k = Ap (ConfigF k)

-- | Part of a computation which produces configuration.
data ConfigF :: * -> * -> * where
  Text :: k -> (Text -> a) -> ConfigF k a
  Int :: k -> (Int -> a) -> ConfigF k a
  Optional :: Config k a -> ConfigF k (Maybe a)
  Prefix :: k -> Config k a -> ConfigF k a

-- | Configuration part which produces text.
text :: k -> Config k Text
text = liftAp . flip Text id

-- | Configuration part which produces an integer.
int :: k -> Config k Int
int = liftAp . flip Int id

-- | Configuration part which makes configuration optional.
optional :: Config k a -> Config k (Maybe a)
optional = liftAp . Optional

-- | Configuration part which nests other configuration.
prefix :: k -> Config k a -> Config k a
prefix = (liftAp .) . Prefix
