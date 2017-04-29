{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read configuration from environment variables.
module Data.Config.Env
  ( Error(..)
  , fromEnv
  ) where

import Control.Applicative.Free (runAp)
import Data.Config (Config, ConfigF(..))
import Data.Functor.Compose (Compose(..))
import Data.Maybe (listToMaybe)
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Text (Text, pack, toUpper, unpack)
import Data.Validation (AccValidation(..))
import Prelude hiding (error, lookup)
import System.Environment (lookupEnv)

import qualified Data.Set as Set

-- | When a config part could not be retrieved.
data Error
  = VarMissing -- ^ The environment variable was not set.
  | ParseError -- ^ The environment variable could not be parsed.
  deriving (Eq, Ord, Show)

-- | Read configuration from environment variables using some prefix. An
-- underscore is added to the prefix automatically, and all keys are
-- upper-cased.
fromEnv :: Text -> Config Text a -> IO (AccValidation (Set (Text, Error)) a)
fromEnv prefix = getCompose . runAp go
  where
  go :: ConfigF Text a -> Compose IO (AccValidation (Set (Text, Error))) a
  go (Text k next) = next <$> lookup k (Just . pack)
  go (Int k next) = next <$> lookup k (fmap fst . listToMaybe . reads)
  go (Optional c) = Compose $
    (<$> fromEnv prefix c) $ \case
      AccFailure e ->
        let e' = Set.filter ((/= VarMissing) . snd) e in
        if Set.null e' then AccSuccess Nothing else AccFailure e'
      AccSuccess a -> AccSuccess $ Just a
  go (Prefix k c) = Compose $ fromEnv (k <> "_" <> prefix) c

  lookup
    :: Text
    -> (String -> Maybe a)
    -> Compose IO (AccValidation (Set (Text, Error))) a
  lookup k p = Compose $ do
    v <- lookupEnv . unpack . toUpper $ prefix <> "_" <> k
    pure $ case p <$> v of
      Nothing -> error k VarMissing
      Just Nothing -> error k ParseError
      Just (Just v') -> AccSuccess v'

  error :: Text -> Error -> AccValidation (Set (Text, Error)) a
  error = curry $ AccFailure . Set.singleton
