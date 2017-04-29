{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Data.Config
  ( ConfigF(..)
  , text
  , int
  , optional
  , prefix
  ) where

import Control.Applicative.Free (Ap, liftAp)
import Data.Text (Text)

type Config k = Ap (ConfigF k)

data ConfigF :: * -> * -> * where
  Text :: k -> (Text -> a) -> ConfigF k a
  Int :: k -> (Int -> a) -> ConfigF k a
  Optional :: Config k a -> ConfigF k (Maybe a)
  Prefix :: k -> Config k a -> ConfigF k a

text :: k -> Config k Text
text = liftAp . flip Text id

int :: k -> Config k Int
int = liftAp . flip Int id

optional :: Config k a -> Config k (Maybe a)
optional = liftAp . Optional

prefix :: k -> Config k a -> Config k a
prefix = (liftAp .) . Prefix
