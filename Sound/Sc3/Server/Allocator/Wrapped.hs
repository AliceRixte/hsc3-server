{-# LANGUAGE FlexibleContexts #-}
-- | Helper functions for newtype wrappers.
module Sound.Sc3.Server.Allocator.Wrapped (
  alloc
, free
, statistics
, allocRange
, freeRange
) where

import           Control.Arrow (second)
import           Control.Monad (liftM)
import           Control.Monad.Catch (MonadThrow)
import           Sound.Sc3.Server.Allocator (AllocFailure, Id, IdAllocator, RangeAllocator, Statistics)
import qualified Sound.Sc3.Server.Allocator as Alloc
import           Sound.Sc3.Server.Allocator.Range (Range)

alloc :: (MonadThrow m, IdAllocator a) =>
    (a -> a') -> a -> m (Id a, a')
alloc f = liftM (second f) . Alloc.alloc

free :: (MonadThrow m, IdAllocator a) =>
    (a -> a') -> Id a -> a -> m a'
free f i = liftM f . Alloc.free i

statistics :: IdAllocator a => a -> Statistics
statistics = Alloc.statistics

allocRange :: (MonadThrow m, RangeAllocator a) =>
    (a -> a') -> Int -> a -> m (Range (Id a), a')
allocRange f n = liftM (second f) . Alloc.allocRange n

freeRange :: (MonadThrow m, RangeAllocator a) =>
    (a -> a') -> Range (Id a) -> a -> m a'
freeRange f r = liftM f . Alloc.freeRange r
