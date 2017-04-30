{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}

-- | Applicative configuration DSL.
module Data.Config
  ( -- * Constructing computations
    Config
  , ConfigF(..)
  , text
  , int
  , optional
  , prefix

    -- * Transforming computations
  , mapKeys
  ) where

import Control.Applicative.Free (Ap, hoistAp, liftAp)
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

-- | Change the keys of a configuration computation.
mapKeys :: (k -> l) -> Config k a -> Config l a
mapKeys f = hoistAp $ \case
  Text k next -> Text (f k) next
  Int k next -> Int (f k) next
  Optional next -> Optional (mapKeys f next)
  Prefix k next -> Prefix (f k) (mapKeys f next)
