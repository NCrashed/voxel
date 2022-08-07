module Data.Voxel.UI.Class(
  MonadUI(..)
) where

import Data.Vector (Vector)
import Data.Voxel.Rect (Rect)
import Reflex 

class (Reflex t, Monad m) => MonadUI t m | m -> t where
  -- | Get rects that should be drawn at the current time.
  drawableRects :: m (Behavior t (Vector Rect))
  -- | Draw the given rect on the screen
  drawRect :: Dynamic t Rect -> m ()