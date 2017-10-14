{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module MonadGame where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Reflex

class (MonadIO m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), PostBuild t m, MonadHold t m, MonadFix m, MonadAdjust t m) => MonadGame t m

instance (MonadGame
             (SpiderTimeline Global)
             (PostBuildT
                 (SpiderTimeline Global)
                 (TriggerEventT
                     (SpiderTimeline Global)
                     (PerformEventT (SpiderTimeline Global) (SpiderHost Global)))))

holdGameMode :: MonadGame t m => m a -> Event t (m a) -> m (Dynamic t a)
holdGameMode mode newMode = do
    (res, newRes) <- runWithReplace mode newMode
    holdDyn res newRes
