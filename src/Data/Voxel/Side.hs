module Data.Voxel.Side(
    Side(..)
  , sideOffset
  , sideFromOffset
  , oppositeSide
  ) where

import GHC.Generics
import Linear

-- | Encodes 6 directions to represent faces of cube.
data Side = SideForward | SideBackward | SideRight | SideLeft | SideUp | SideDown
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)

-- | Get coordinates offset by side
sideOffset :: Side -> V3 Int
sideOffset s = case s of
  SideForward  -> V3   1   0   0
  SideBackward -> V3 (-1)  0   0
  SideRight    -> V3   0   1   0
  SideLeft     -> V3   0 (-1)  0
  SideUp       -> V3   0   0   1
  SideDown     -> V3   0   0 (-1)
{-# INLINE sideOffset #-}

-- | Revert of `sideOffset`
sideFromOffset :: V3 Int -> Maybe Side
sideFromOffset v = case signum v of
  V3   1   0   0  -> Just SideForward
  V3 (-1)  0   0  -> Just SideBackward
  V3   0   1   0  -> Just SideRight
  V3   0 (-1)  0  -> Just SideLeft
  V3   0   0   1  -> Just SideUp
  V3   0   0 (-1) -> Just SideDown
  _               -> Nothing
{-# INLINE sideFromOffset #-}


-- | Get opposite side of current
oppositeSide :: Side -> Side
oppositeSide s = case s of
  SideForward  -> SideBackward
  SideBackward -> SideForward
  SideRight    -> SideLeft
  SideLeft     -> SideRight
  SideUp       -> SideDown
  SideDown     -> SideUp
{-# INLINE oppositeSide #-}
