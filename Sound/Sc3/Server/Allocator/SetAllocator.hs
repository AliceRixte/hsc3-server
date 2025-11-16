{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Sound.Sc3.Server.Allocator.SetAllocator (
    SetAllocator
  , cons
) where

import           Control.Monad.Catch (MonadThrow(..))
import qualified Data.Set as Set
import           Sound.Sc3.Server.Allocator (AllocFailure(..), IdAllocator(..), Statistics(..))
import           Sound.Sc3.Server.Allocator.Range (Range)
import qualified Sound.Sc3.Server.Allocator.Range as Range

data SetAllocator i =
    SetAllocator
        {-# UNPACK #-} !(Range i)
                       !(Set.Set i)
                       !i
        deriving (Eq, Show)

cons :: Integral i => Range i -> SetAllocator i
cons r = SetAllocator r Set.empty (Range.begin r)

-- | Convert an id to a bit index.
--
-- This is necessary to keep the BitSet size bounded between [0, numIds).
toBit :: Integral i => Range i -> i -> i
toBit r i = i - Range.begin r

findNext :: (Integral i) => SetAllocator i -> Maybe i
findNext (SetAllocator r u i)
    | fromIntegral (Range.size r) == Set.size u = Nothing
    | otherwise = loop i
    where
        wrap i = if i >= Range.end r
                    then Range.begin r
                    else i
        loop !i = let i' = wrap (i+1)
                  in if Set.member (toBit r i') u
                     then loop i'
                     else Just i'

_alloc :: (Integral i, MonadThrow m) => SetAllocator i -> m (i, SetAllocator i)
_alloc a@(SetAllocator r u i) =
    case findNext a of
        Nothing -> throwM NoFreeIds
        Just i' -> return (i, SetAllocator r (Set.insert (toBit r i) u) i')

_free :: (Integral i, MonadThrow m) => i -> SetAllocator i -> m (SetAllocator i)
_free i (SetAllocator r u n) =
    if Set.member (toBit r i) u
    then let u' = Set.delete (toBit r i) u
         in return (SetAllocator r u' n)
    else throwM InvalidId

_statistics :: (Integral i) => SetAllocator i -> Statistics
_statistics (SetAllocator r u _) =
    let k = fromIntegral (Range.size r)
        n = Set.size u
    in Statistics {
        numAvailable = k
      , numFree = k - n
      , numUsed = n }

instance (Integral i) => IdAllocator (SetAllocator i) where
    type Id (SetAllocator i) = i
    alloc                    = _alloc
    free                     = _free
    statistics               = _statistics
