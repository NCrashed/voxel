module Data.Voxel.Grid.Unbox.Mutable.Internal where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import Data.Vector.Unboxed.Mutable (Unbox, MVector)
import Linear

import qualified Data.Vector.Unboxed.Mutable as VM

-- | Mutable square 3D grid of unboxed data. The 's' type variable is phantom
-- type that catches world state like in `ST` monad, so you can use the storage
-- in 'IO' and 'ST' contexts.
--
-- Effectively it is fixed size wrapper around `MVector`.
data MVoxelGrid s a = MVoxelGrid {
  -- | Size of one dimension
    voxelGridSize :: !(V3 Int)
  -- | Vector storage of grid
  , voxelGridData :: !(MVector s a)
  }

-- | 'MVoxelGrid' for 'IO' monad
type IOVoxelGrid = MVoxelGrid RealWorld
-- | 'MVoxelGrid' for 'ST' monad. The type synonym exists only for uniform API looking
-- with 'IOVoxelGrid'
type STVoxelGrid s = MVoxelGrid s

-- | Allocate new grid with given size of each of 3 dimensions.
new :: (PrimMonad m, Unbox a) => V3 Int -> m (MVoxelGrid (PrimState m) a)
new vs@(V3 x y z) = do
  s <- VM.new (x * y * z)
  pure $! MVoxelGrid vs s
{-# INLINE new #-}

-- | Allocate new grid with given size of each of 3 dimension with given value.
replicate :: (PrimMonad m, Unbox a) => V3 Int -> a -> m (MVoxelGrid (PrimState m) a)
replicate vs@(V3 x y z) a0 = do
  s <- VM.replicate (x * y * z) a0
  pure $! MVoxelGrid vs s
{-# INLINE replicate #-}

-- | Allocate new grid with given size of each of 3 dimension with given value.
replicateM :: (PrimMonad m, Unbox a) => V3 Int -> m a -> m (MVoxelGrid (PrimState m) a)
replicateM vs@(V3 x y z) ma = do
  s <- VM.replicateM (x * y * z) ma
  pure $! MVoxelGrid vs s
{-# INLINE replicateM #-}

-- | Copy memory from given grid to new grid.
clone :: (PrimMonad m, Unbox a) => MVoxelGrid (PrimState m) a -> m (MVoxelGrid (PrimState m) a)
clone (MVoxelGrid n a) = do
  b <- VM.clone a
  pure $! MVoxelGrid n b
{-# INLINE clone #-}

-- | Get size of single dimension of the grid
size :: MVoxelGrid s a -> V3 Int
size = voxelGridSize
{-# INLINE size #-}

-- | Get amount of voxels inside the grid.
length :: MVoxelGrid s a -> Int
length g = x * y * z
  where
    V3 x y z = size g
{-# INLINE length #-}

-- | Convert 3d position in grid to index in flat array. Doens't check for bounds.
posToIndex ::
     V3 Int -- ^ Dimension size
  -> V3 Int -- ^ Position
  -> Int -- ^ Index in array
posToIndex (V3 sx sy _) (V3 x y z) = x + y * sx + z * sx * sy
{-# INLINE posToIndex #-}

-- | Convert index in flat array to grid 3d position. Doens't check for bounds.
indexToPos ::
     V3 Int -- ^ Dimension size
  -> Int -- ^ Index in array
  -> V3 Int -- ^ Position
indexToPos (V3 sx sy _) i = V3 x y z
  where
    x = i' `mod` sx
    y = i' `div` sx
    z = i `div` (sx*sy)
    i' = i - z * sx * sy
{-# INLINE indexToPos #-}

-- | Check that given position is in bounds of array
posInBounds ::
     V3 Int -- ^ Dimension size
  -> V3 Int -- ^ Position
  -> Bool
posInBounds (V3 sx sy sz) (V3 x y z) =
  x >= 0 && x < sx && y >= 0 && y < sy && z >= 0 && z < sz
{-# INLINE posInBounds #-}

-- | Read single element in grid with bounding checks.
read :: (PrimMonad m, MonadFail m, Unbox a) => MVoxelGrid (PrimState m) a -> V3 Int -> m a
read (MVoxelGrid n g) p
  | not (posInBounds n p) = fail $ "MVoxelGrid: read " ++ show p
    ++ " is out of bounds of " ++ show n
  | otherwise = do
    let i = posToIndex n p
    VM.read g i
{-# INLINE read #-}

-- | Write single element in grid with bounding checks.
write :: (PrimMonad m, MonadFail m, Unbox a) => MVoxelGrid (PrimState m) a -> V3 Int -> a -> m ()
write (MVoxelGrid n g) p a
  | not (posInBounds n p) = fail $ "MVoxelGrid: write " ++ show p
    ++ " is out of bounds of " ++ show n
  | otherwise = do
    let i = posToIndex n p
    VM.write g i a
{-# INLINE write #-}

-- | Swap two elements in the grid
swap :: (PrimMonad m, MonadFail m, Unbox a) => MVoxelGrid (PrimState m) a -> V3 Int -> V3 Int -> m ()
swap (MVoxelGrid n g) p1 p2
  | not (posInBounds n p1) = fail $ "MVoxelGrid: swap " ++ show p1
    ++ " is out of bounds of " ++ show n
  | not (posInBounds n p2) = fail $ "MVoxelGrid: swap " ++ show p2
    ++ " is out of bounds of " ++ show n
  | otherwise = do
    let i1 = posToIndex n p1
        i2 = posToIndex n p2
    VM.swap g i1 i2
{-# INLINE swap #-}

-- | Read single element in grid without bounding checks.
unsafeRead :: (PrimMonad m, Unbox a) => MVoxelGrid (PrimState m) a -> V3 Int -> m a
unsafeRead (MVoxelGrid n g) p = do
  let i = posToIndex n p
  VM.unsafeRead g i
{-# INLINE unsafeRead #-}

-- | Write single element in grid without bounding checks.
unsafeWrite :: (PrimMonad m, Unbox a) => MVoxelGrid (PrimState m) a -> V3 Int -> a -> m ()
unsafeWrite (MVoxelGrid n g) p a = do
  let i = posToIndex n p
  VM.unsafeWrite g i a
{-# INLINE unsafeWrite #-}

-- | Swap two elements in the grid without bounding checks.
unsafeSwap :: (PrimMonad m, Unbox a) => MVoxelGrid (PrimState m) a -> V3 Int -> V3 Int -> m ()
unsafeSwap (MVoxelGrid n g) p1 p2 = do
  let i1 = posToIndex n p1
      i2 = posToIndex n p2
  VM.unsafeSwap g i1 i2
{-# INLINE unsafeSwap #-}
