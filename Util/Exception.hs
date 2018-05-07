{-# LANGUAGE ExistentialQuantification #-}

module Util.Exception where

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Trans.Control
import Data.Monoid (Alt (..))
import Util

handleJusts :: MonadBaseControl IO m => [ExceptionPredicate b] -> (b -> m a) -> m a -> m a
handleJusts = handleJust . fmap getAlt . foldMapA (fmap Alt . \ (ExceptionPredicate f) -> fromException >=> f)

data ExceptionPredicate b = ∀ e . Exception e => ExceptionPredicate (e -> Maybe b)
