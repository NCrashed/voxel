module Data.Voxel.Grid.Unbox.Internal where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Bifunctor (first)
import Data.Vector.Unboxed (Unbox, Vector)
import Data.Voxel.Grid.Unbox.Mutable.Internal (MVoxelGrid, posToIndex, indexToPos, posInBounds)
import Linear
import Prelude as P

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Voxel.Grid.Unbox.Mutable.Internal as GM

-- | Immutable square 3D grid of unboxed data.
--
-- Effectively it is fixed size wrapper around `Vector`.
data VoxelGrid a = VoxelGrid {
  -- | Size of one dimension
    voxelGridSize :: !Int
  -- | Vector storage of grid
  , voxelGridData :: !(Vector a)
  }


-- | Get size of single dimension of the grid
size :: VoxelGrid a -> Int
size = voxelGridSize
{-# INLINE size #-}

-- | Get amount of voxels inside the grid.
length :: VoxelGrid a -> Int
length g = s * s * s
  where
    s = size g
{-# INLINE length #-}

-- | /O(1)/ indexing
(!) :: Unbox a => VoxelGrid a -> V3 Int -> a
(!) (VoxelGrid s v) i
  | posInBounds s i = v V.! posToIndex s i
  | otherwise = error $ "(!) pos " <> show i <> " is out of bounds"
{-# INLINE (!) #-}

-- | /O(1)/ safe indexing
(!?) :: Unbox a => VoxelGrid a -> V3 Int -> Maybe a
(!?) (VoxelGrid s v) i
  | posInBounds s i = v V.!? posToIndex s i
  | otherwise = Nothing
{-# INLINE (!?) #-}

-- | /O(1)/ indexing without bound checks
unsafeIndex :: Unbox a => VoxelGrid a -> V3 Int -> a
unsafeIndex (VoxelGrid s v) i = V.unsafeIndex v (posToIndex s i)
{-# INLINE unsafeIndex #-}

-- | /O(1)/ Create new empty grid with given size
empty :: Unbox a => Int -> VoxelGrid a
empty s = VoxelGrid s V.empty
{-# INLINE empty #-}

-- | /O(n)/ Create new grid filled with value
replicate :: Unbox a => Int -> a -> VoxelGrid a
replicate s a = VoxelGrid s (V.replicate (s*s*s) a)
{-# INLINE replicate #-}

-- | /O(n)/ Create new grid by applying the function to each position
generate :: Unbox a => Int -> (V3 Int -> a) -> VoxelGrid a
generate s f = VoxelGrid s $ V.generate (s*s*s) (f . indexToPos s)
{-# INLINE generate #-}

-- | /O(n)/ Execute the monadic action the given number of times and store the results in a grid
replicateM :: (Monad m, Unbox a) => Int -> m a -> m (VoxelGrid a)
replicateM s f = fmap (VoxelGrid s) $ V.replicateM (s*s*s) f
{-# INLINE replicateM #-}

-- | /O(n)/ Construct grid of given length by applying monadic action to each index
generateM :: (Monad m, Unbox a) => Int -> (V3 Int -> m a) -> m (VoxelGrid a)
generateM s f = fmap (VoxelGrid s) $ V.generateM (s*s*s) (f . indexToPos s)
{-# INLINE generateM #-}

-- | Execute monadic action and freeze resulted grid.
create :: Unbox a => (forall s . ST s (MVoxelGrid s a)) -> VoxelGrid a
create m = VoxelGrid s v
  where
    v = V.create (GM.voxelGridData <$> m)
    s = round $ (fromIntegral (V.length v) :: Double) ** (1/3)
{-# INLINE create #-}

-- | /O(m+n)/ For each pair (i,a) from the list, replace the grid element at position i by a.
(//) :: Unbox a
  => VoxelGrid a -- ^ Initial grid (of length m)
  -> [(V3 Int, a)] -- ^ list of index/value (of length n)
  -> VoxelGrid a
(//) (VoxelGrid s a) is
  | P.all (posInBounds s . fst) is = VoxelGrid s $ a V.// fmap (first (posToIndex s)) is
  | otherwise = error $ "(//): out of bounds " ++ show (fmap fst $ filter (not . posInBounds s . fst) is)
{-# INLINE (//) #-}

-- | Same as (//) but without bounds checking.
unsafeUpd :: Unbox a
  => VoxelGrid a -- ^ Initial grid (of length m)
  -> [(V3 Int, a)] -- ^ list of index/value (of length n)
  -> VoxelGrid a
unsafeUpd (VoxelGrid s a) is =  VoxelGrid s $ V.unsafeUpd a $ fmap (first (posToIndex s)) is
{-# INLINE unsafeUpd #-}

-- | Apply a destructive operation to a grid. The operation will be performed
-- in place if it is safe to do so and will modify a copy of the vector otherwise.
modify :: Unbox a => (forall s . MVoxelGrid s a -> ST s ()) -> VoxelGrid a -> VoxelGrid a
modify m (VoxelGrid s a) = VoxelGrid s $ V.modify (m . GM.MVoxelGrid s) a
{-# INLINE modify #-}

-- | /O(n)/ Pair each element in a grid with its index
indexed :: Unbox a => VoxelGrid a -> VoxelGrid (V3 Int, a)
indexed (VoxelGrid s a) = VoxelGrid s $ V.map (first (indexToPos s)) . V.indexed $ a
{-# INLINE indexed #-}

-- | /O(n)/ Map a function over a grid
map :: (Unbox a, Unbox b) => (a -> b) -> VoxelGrid a -> VoxelGrid b
map f (VoxelGrid s a) = VoxelGrid s $ V.map f a
{-# INLINE map #-}

-- | /O(n)/ Apply a function to every element of a vector and its index
imap :: (Unbox a, Unbox b) => (V3 Int -> a -> b) -> VoxelGrid a -> VoxelGrid b
imap f (VoxelGrid s a) = VoxelGrid s $ V.imap (f . indexToPos s) a
{-# INLINE imap #-}

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a vector of results
mapM :: (Monad m, Unbox a, Unbox b) => (a -> m b) -> VoxelGrid a -> m (VoxelGrid b)
mapM f (VoxelGrid s a) = fmap (VoxelGrid s) $ V.mapM f a
{-# INLINE mapM #-}

-- | /O(n)/ Apply the monadic action to every element of a vector and its index, yielding a vector of results
imapM :: (Monad m, Unbox a, Unbox b) => (V3 Int -> a -> m b) -> VoxelGrid a -> m (VoxelGrid b)
imapM f (VoxelGrid s a) = fmap (VoxelGrid s) $ V.imapM (f . indexToPos s) a
{-# INLINE imapM #-}

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a vector of results
mapM_ :: (Monad m, Unbox a) => (a -> m b) -> VoxelGrid a -> m ()
mapM_ f (VoxelGrid _ a) = V.mapM_ f a
{-# INLINE mapM_ #-}

-- | /O(n)/ Apply the monadic action to every element of a vector and its index, yielding a vector of results
imapM_ :: (Monad m, Unbox a) => (V3 Int -> a -> m b) -> VoxelGrid a -> m ()
imapM_ f (VoxelGrid s a) = V.imapM_ (f . indexToPos s) a
{-# INLINE imapM_ #-}

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a vector of results. Equivalent to flip 'mapM'.
forM :: (Monad m, Unbox a, Unbox b) => VoxelGrid a -> (a -> m b) -> m (VoxelGrid b)
forM (VoxelGrid s a) f = fmap (VoxelGrid s) $ V.forM a f
{-# INLINE forM #-}

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a vector of results. Equivalent to flip 'mapM_'.
forM_ :: (Monad m, Unbox a) => VoxelGrid a -> (a -> m b) -> m ()
forM_ (VoxelGrid _ a) f = V.forM_ a f
{-# INLINE forM_ #-}

infix 4 `elem`
-- | /O(n)/ Check if the grid contains an element
elem :: (Unbox a, Eq a) => a -> VoxelGrid a -> Bool
elem a (VoxelGrid _ v) = V.elem a v
{-# INLINE elem #-}

infix 4 `notElem`
-- | /O(n)/ Check if the grid does not contain an element (inverse of 'elem')
notElem :: (Unbox a, Eq a) => a -> VoxelGrid a -> Bool
notElem a (VoxelGrid _ v) = V.notElem a v
{-# INLINE notElem #-}

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: Unbox a => (a -> Bool) -> VoxelGrid a -> Maybe a
find f (VoxelGrid _ v) = V.find f v
{-# INLINE find #-}

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: Unbox a => (a -> Bool) -> VoxelGrid a -> Maybe (V3 Int)
findIndex f (VoxelGrid s v) = fmap (indexToPos s) $ V.findIndex f v
{-# INLINE findIndex #-}

-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: Unbox a => (a -> Bool) -> VoxelGrid a -> Vector (V3 Int)
findIndices f (VoxelGrid s v) = V.map (indexToPos s) $ V.findIndices f v
{-# INLINE findIndices #-}

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the grid does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (Unbox a, Eq a) => a -> VoxelGrid a -> Maybe (V3 Int)
elemIndex a (VoxelGrid s v) = fmap (indexToPos s) $ V.elemIndex a v
{-# INLINE elemIndex #-}

-- | O(n) Yield the indices of all occurences of the given element in ascending
-- order. This is a specialised version of findIndices.
elemIndices :: (Unbox a, Eq a) => a -> VoxelGrid a -> Vector (V3 Int)
elemIndices a (VoxelGrid s v) = V.map (indexToPos s) $ V.elemIndices a v
{-# INLINE elemIndices #-}

-- | /O(n)/ Left fold
foldl :: Unbox b => (a -> b -> a) -> a -> VoxelGrid b -> a
foldl f a (VoxelGrid _ v) = V.foldl f a v
{-# INLINE foldl #-}

-- | /O(n)/ Left fold on non-empty vectors
foldl1 :: Unbox a => (a -> a -> a) -> VoxelGrid a -> a
foldl1 f (VoxelGrid _ v) = V.foldl1 f v
{-# INLINE foldl1 #-}

-- | /O(n)/ Left fold with strict accumulator
foldl' :: Unbox b => (a -> b -> a) -> a -> VoxelGrid b -> a
foldl' f a (VoxelGrid _ v) = V.foldl' f a v
{-# INLINE foldl' #-}

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator
foldl1' :: Unbox a => (a -> a -> a) -> VoxelGrid a -> a
foldl1' f (VoxelGrid _ v) = V.foldl1' f v
{-# INLINE foldl1' #-}

-- | /O(n)/ Right fold
foldr :: Unbox a => (a -> b -> b) -> b -> VoxelGrid a -> b
foldr f b (VoxelGrid _ v) = V.foldr f b v
{-# INLINE foldr #-}

-- | /O(n)/ Right fold on non-empty vectors
foldr1 :: Unbox a => (a -> a -> a) -> VoxelGrid a -> a
foldr1 f (VoxelGrid _ v) = V.foldr1 f v
{-# INLINE foldr1 #-}

-- | /O(n)/ Right fold with a strict accumulator
foldr' :: Unbox a => (a -> b -> b) -> b -> VoxelGrid a -> b
foldr' f b (VoxelGrid _ v) = V.foldr' f b v
{-# INLINE foldr' #-}

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator
foldr1' :: Unbox a => (a -> a -> a) -> VoxelGrid a -> a
foldr1' f (VoxelGrid _ v) = V.foldr1' f v
{-# INLINE foldr1' #-}

-- | /O(n)/ Left fold (function applied to each element and its index)
ifoldl :: Unbox b => (a -> V3 Int -> b -> a) -> a -> VoxelGrid b -> a
ifoldl f a0 (VoxelGrid s v) = V.ifoldl (\a i -> f a (indexToPos s i)) a0 v
{-# INLINE ifoldl #-}

-- | /O(n)/ Left fold with strict accumulator (function applied to each element
-- and its index)
ifoldl' :: Unbox b => (a -> V3 Int -> b -> a) -> a -> VoxelGrid b -> a
ifoldl' f a0 (VoxelGrid s v) = V.ifoldl' (\a i -> f a (indexToPos s i)) a0 v
{-# INLINE ifoldl' #-}

-- | /O(n)/ Right fold (function applied to each element and its index)
ifoldr :: Unbox a => (V3 Int -> a -> b -> b) -> b -> VoxelGrid a -> b
ifoldr f b0 (VoxelGrid s v) = V.ifoldr (f . indexToPos s) b0 v
{-# INLINE ifoldr #-}

-- | /O(n)/ Right fold with strict accumulator (function applied to each
-- element and its index)
ifoldr' :: Unbox a => (V3 Int -> a -> b -> b) -> b -> VoxelGrid a -> b
ifoldr' f b0 (VoxelGrid s v) = V.ifoldr' (f . indexToPos s) b0 v
{-# INLINE ifoldr' #-}

-- | /O(n)/ Check if all elements satisfy the predicate.
all :: Unbox a => (a -> Bool) -> VoxelGrid a -> Bool
all f (VoxelGrid _ v) = V.all f v
{-# INLINE all #-}

-- | /O(n)/ Check if any element satisfies the predicate.
any :: Unbox a => (a -> Bool) -> VoxelGrid a -> Bool
any f (VoxelGrid _ v) = V.any f v
{-# INLINE any #-}

-- | /O(n)/ Yield the maximum element of the grid.
maximum :: (Unbox a, Ord a) => VoxelGrid a -> a
maximum (VoxelGrid _ v) = V.maximum v
{-# INLINE maximum #-}

-- | /O(n)/ Yield the maximum element of the grid according to the given
-- comparison function.
maximumBy :: Unbox a => (a -> a -> Ordering) -> VoxelGrid a -> a
maximumBy f (VoxelGrid _ v) = V.maximumBy f v
{-# INLINE maximumBy #-}

-- | /O(n)/ Yield the minimum element of the grid.
minimum :: (Unbox a, Ord a) => VoxelGrid a -> a
minimum (VoxelGrid _ v) = V.minimum v
{-# INLINE minimum #-}

-- | /O(n)/ Yield the minimum element of the grid according to the given
-- comparison function.
minimumBy :: Unbox a => (a -> a -> Ordering) -> VoxelGrid a -> a
minimumBy f (VoxelGrid _ v) = V.minimumBy f v
{-# INLINE minimumBy #-}

-- | /O(n)/ Yield the index of the maximum element of the grid.
maxIndex :: (Unbox a, Ord a) => VoxelGrid a -> V3 Int
maxIndex (VoxelGrid s v) = indexToPos s $ V.maxIndex v
{-# INLINE maxIndex #-}

-- | /O(n)/ Yield the index of the maximum element of the grid according to
-- the given comparison function.
maxIndexBy :: Unbox a => (a -> a -> Ordering) -> VoxelGrid a -> V3 Int
maxIndexBy f (VoxelGrid s v) = indexToPos s $ V.maxIndexBy f v
{-# INLINE maxIndexBy #-}

-- | /O(n)/ Yield the index of the minimum element of the grid.
minIndex :: (Unbox a, Ord a) => VoxelGrid a -> V3 Int
minIndex (VoxelGrid s v) = indexToPos s $ V.minIndex v
{-# INLINE minIndex #-}

-- | /O(n)/ Yield the index of the minimum element of the grid according to
-- the given comparison function.
minIndexBy :: Unbox a => (a -> a -> Ordering) -> VoxelGrid a -> V3 Int
minIndexBy f (VoxelGrid s v) = indexToPos s $ V.minIndexBy f v
{-# INLINE minIndexBy #-}

-- | /O(n)/ Monadic fold
foldM :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> VoxelGrid b -> m a
foldM f a (VoxelGrid _ v) = V.foldM f a v
{-# INLINE foldM #-}

-- | /O(n)/ Monadic fold (action applied to each element and its index)
ifoldM :: (Monad m, Unbox b) => (a -> V3 Int -> b -> m a) -> a -> VoxelGrid b -> m a
ifoldM f a0 (VoxelGrid s v) = V.ifoldM (\a i -> f a (indexToPos s i)) a0 v
{-# INLINE ifoldM #-}

-- | /O(n)/ Monadic fold over non-empty vectors
fold1M :: (Monad m, Unbox a) => (a -> a -> m a) -> VoxelGrid a -> m a
fold1M f (VoxelGrid _ v) = V.fold1M f v
{-# INLINE fold1M #-}

-- | /O(n)/ Monadic fold with strict accumulator
foldM' :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> VoxelGrid b -> m a
foldM' f a (VoxelGrid _ v) = V.foldM' f a v
{-# INLINE foldM' #-}

-- | /O(n)/ Monadic fold with strict accumulator (action applied to each
-- element and its index)
ifoldM' :: (Monad m, Unbox b) => (a -> V3 Int -> b -> m a) -> a -> VoxelGrid b -> m a
ifoldM' f a0 (VoxelGrid s v) = V.ifoldM' (\a i -> f a (indexToPos s i)) a0 v
{-# INLINE ifoldM' #-}

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
fold1M' :: (Monad m, Unbox a) => (a -> a -> m a) -> VoxelGrid a -> m a
fold1M' f (VoxelGrid _ v) = V.fold1M' f v
{-# INLINE fold1M' #-}

-- | /O(n)/ Monadic fold that discards the result
foldM_ :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> VoxelGrid b -> m ()
foldM_ f a (VoxelGrid _ v) = V.foldM_ f a v
{-# INLINE foldM_ #-}

-- | /O(n)/ Monadic fold that discards the result (action applied to each
-- element and its index)
ifoldM_ :: (Monad m, Unbox b) => (a -> V3 Int -> b -> m a) -> a -> VoxelGrid b -> m ()
ifoldM_ f a0 (VoxelGrid s v) = V.ifoldM_ (\a i -> f a (indexToPos s i)) a0 v
{-# INLINE ifoldM_ #-}

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result
fold1M_ :: (Monad m, Unbox a) => (a -> a -> m a) -> VoxelGrid a -> m ()
fold1M_ f (VoxelGrid _ v) = V.fold1M_ f v
{-# INLINE fold1M_ #-}

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
foldM'_ :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> VoxelGrid b -> m ()
foldM'_ f a (VoxelGrid _ v) = V.foldM'_ f a v
{-# INLINE foldM'_ #-}

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
-- (action applied to each element and its index)
ifoldM'_ :: (Monad m, Unbox b)
         => (a -> V3 Int -> b -> m a) -> a -> VoxelGrid b -> m ()
ifoldM'_ f a0 (VoxelGrid s v) = V.ifoldM'_ (\a i -> f a (indexToPos s i)) a0 v
{-# INLINE ifoldM'_ #-}

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- that discards the result
fold1M'_ :: (Monad m, Unbox a) => (a -> a -> m a) -> VoxelGrid a -> m ()
fold1M'_ f (VoxelGrid _ v) = V.fold1M'_ f v
{-# INLINE fold1M'_ #-}

-- | /O(n)/ Convert a grid to a list
toList :: Unbox a => VoxelGrid a -> [a]
toList (VoxelGrid _ a) = V.toList a
{-# INLINE toList #-}

-- | /O(n)/ Convert a list to a grid. Will fail if the list contains less elements
-- than size^3.
fromList :: Unbox a => Int -> [a] -> VoxelGrid a
fromList s as
  | P.length as >= s*s*s = VoxelGrid s $ V.fromList as
  | otherwise = error $ "fromList: too little elements, expected "
      ++ show (s*s*s) ++ " but got " ++ show (P.length as)
{-# INLINE fromList #-}

-- | /O(1)/ Unsafe convert a mutable grid to an immutable one without
-- copying. The mutable grid may not be used after this operation.
unsafeFreeze :: (Unbox a, PrimMonad m) => MVoxelGrid (PrimState m) a -> m (VoxelGrid a)
unsafeFreeze = undefined
{-# INLINE unsafeFreeze #-}

-- | /O(1)/ Unsafely convert an immutable grid to a mutable one without
-- copying. The immutable grid may not be used after this operation.
unsafeThaw :: (Unbox a, PrimMonad m) => VoxelGrid a -> m (MVoxelGrid (PrimState m) a)
unsafeThaw = undefined
{-# INLINE unsafeThaw #-}

-- | /O(n)/ Yield a mutable copy of the immutable grid.
thaw :: (Unbox a, PrimMonad m) => VoxelGrid a -> m (MVoxelGrid (PrimState m) a)
thaw = undefined
{-# INLINE thaw #-}

-- | /O(n)/ Yield an immutable copy of the mutable grid.
freeze :: (Unbox a, PrimMonad m) => MVoxelGrid (PrimState m) a -> m (VoxelGrid a)
freeze = undefined
{-# INLINE freeze #-}

-- | /O(n)/ Copy an immutable grid into a mutable one. The two grids must
-- have the same length. This is not checked.
unsafeCopy
  :: (Unbox a, PrimMonad m) => MVoxelGrid (PrimState m) a -> VoxelGrid a -> m ()
unsafeCopy = undefined
{-# INLINE unsafeCopy #-}

-- | /O(n)/ Copy an immutable grid into a mutable one. The two grids must
-- have the same length.
copy :: (Unbox a, PrimMonad m) => MVoxelGrid (PrimState m) a -> VoxelGrid a -> m ()
copy = undefined
{-# INLINE copy #-}
