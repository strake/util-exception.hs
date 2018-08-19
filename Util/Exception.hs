{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Util.Exception where

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Base.Control
import Data.Basic (Basic1 (..))
import Data.Monoid (Alt (..))
import Util

handleJusts :: (MonadBaseControl m, Base m ~ IO) => [ExceptionPredicate b] -> (b -> m a) -> m a -> m a
handleJusts = handleJust . fmap getAlt . foldMapA (fmap Alt . \ (ExceptionPredicate f) -> fromException >=> f)

data ExceptionPredicate b = ∀ e . Exception e => ExceptionPredicate (e -> Maybe b)
