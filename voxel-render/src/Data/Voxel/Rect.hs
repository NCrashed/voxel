module Data.Voxel.Rect(
    Region(..)
  , regionPoints
  , regionUvs
  , regionIndecies
  , Rect(..)
  , rectZPos
  ) where 

import Control.Arrow
import Control.Lens ((^.))
import Control.Monad 
import Control.Monad.IO.Class 
import Data.Vector (Vector)
import Data.Voxel.App (SpiderCtx)
import Data.Word
import GHC.Generics (Generic)
import Graphics.GPipe

import qualified Data.Vector as V 

-- | Region is set by two points
data Region = Region !(V2 Float) !(V2 Float)
  deriving (Generic, Show)

-- | Get points of region starting from top left corner 
-- and going clockwise.
--
-- 1 -- 4 
-- |    |
-- 2 -- 3 
regionPoints :: Region -> [V2 Float]
regionPoints (Region (V2 x1 y1) (V2 x2 y2)) = [
    V2 x1 y1
  , V2 x1 y2 
  , V2 x2 y2 
  , V2 x2 y1 
  ]

-- | Get points of region starting from bottom left corner 
-- and going counerclockwise.
--
-- 2 -- 3 
-- |    |
-- 1 -- 4 
regionUvs :: Region -> [V2 Float]
regionUvs (Region (V2 x1 y1) (V2 x2 y2)) = [
    V2 x1 y2
  , V2 x1 y1 
  , V2 x2 y1 
  , V2 x2 y2 
  ]

-- | Get indecies to describe 2 triangles of region 
-- 
-- 1 ----- 4 
-- | *   2 |
-- |   *   |
-- | 1   * |
-- 2 ----- 3 
regionIndecies :: [Word32]
regionIndecies = [
    0, 1, 2 
  , 0, 2, 3
  ]

-- | Rect is region of texture to draw on region of screen
data Rect = Rect {
  -- | Position in screen
  rectPos   :: !Region
  -- | Position in atlas
, rectUv    :: !Region
  -- | Overlay level (higher is closer to camera)
, rectLevel :: !Int
} deriving (Generic, Show)

-- | Convert abastract overlay level to concrete Z value
rectZPos :: Rect -> Float 
rectZPos Rect{..} = - fromIntegral rectLevel / 255.0
