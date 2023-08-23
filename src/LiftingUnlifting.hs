module LiftingUnlifting where

import Control.Monad.IO.Unlift (MonadUnliftIO (..))

-- mplus' :: (MonadTrans t, Monad m) => t m a -> t m a -> t m a
-- mplus' x y = mplus <$> x <*> y

--withFile' :: forall r m .(MonadUnliftIO m) =>  FilePath -> IOMode -> (Handle -> m r) -> m r
--withFile' fp m h = withRunInIO $ \run -> withFile fp m (run . h)



