module Control.Exception.Bracket (
  bracketChoice,
) where

import Control.Exception (mask, onException)
import Data.Kind (Type)

-- | A combination of `Control.Exception.bracket` and 
-- `Control.Exception.bracketOnError` that uses different 
-- closers when receiving an exception vs. ending normally.
bracketChoice 
  :: forall (a :: Type) (b :: Type) (b' :: Type) (c :: Type).
  IO a            -- ^ Computation to run first
  -> (a -> IO b ) -- ^ Computation to run on successful exit.
  -> (a -> IO b') -- ^ Computation to run on error.
  -> (a -> IO c ) -- ^ Main Computation
  -> IO c
bracketChoice before after onErr thing =
  mask $ \restore -> do
    x <- before
    r <- restore (thing x) `onException` onErr x
    _ <- after x
    return r
