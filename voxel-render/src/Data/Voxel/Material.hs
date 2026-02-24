-- | Material definitions for texture atlas rendering
module Data.Voxel.Material(
    MaterialDef(..)
  , MaterialTable
  , defaultMaterial
  , solidMaterial
  , grassMaterial
  , packMaterialsForGPU
  , materialsToPixels
  ) where

import Data.Int
import Data.Vector (Vector)

import qualified Data.Vector as V

-- | Per-face texture layer indices.
-- Order matches Side enum: +X (Forward), -X (Backward), +Y (Right), -Y (Left), +Z (Up), -Z (Down)
data MaterialDef = MaterialDef
  { matForward  :: !Int32  -- ^ +X face texture layer
  , matBackward :: !Int32  -- ^ -X face texture layer
  , matRight    :: !Int32  -- ^ +Y face texture layer
  , matLeft     :: !Int32  -- ^ -Y face texture layer
  , matUp       :: !Int32  -- ^ +Z face texture layer
  , matDown     :: !Int32  -- ^ -Z face texture layer
  } deriving (Show, Eq)

-- | Table of material definitions indexed by material ID
type MaterialTable = Vector MaterialDef

-- | Default material using texture layer 0 on all faces
defaultMaterial :: MaterialDef
defaultMaterial = solidMaterial 0

-- | Material with same texture on all faces
solidMaterial :: Int32 -> MaterialDef
solidMaterial layer = MaterialDef layer layer layer layer layer layer

-- | Grass-style material (different top/side/bottom)
-- Top face uses first layer, sides use second, bottom uses third
grassMaterial :: Int32    -- ^ Top texture layer (+Z)
              -> Int32    -- ^ Side texture layer (all horizontal faces)
              -> Int32    -- ^ Bottom texture layer (-Z)
              -> MaterialDef
grassMaterial top side bottom = MaterialDef
  { matForward  = side
  , matBackward = side
  , matRight    = side
  , matLeft     = side
  , matUp       = top
  , matDown     = bottom
  }

-- | Get texture layer for a face index (0-5 matching Side enum order)
getFaceTexture :: MaterialDef -> Int -> Int32
getFaceTexture m i = case i of
  0 -> matForward m
  1 -> matBackward m
  2 -> matRight m
  3 -> matLeft m
  4 -> matUp m
  5 -> matDown m
  _ -> matForward m
{-# INLINE getFaceTexture #-}

-- | Pack material table into GPU-uploadable format.
-- Returns a flat vector where each 6 consecutive Int32 values represent one material's face textures.
packMaterialsForGPU :: MaterialTable -> Vector Int32
packMaterialsForGPU = V.concatMap packMaterial
  where
    packMaterial m = V.fromList
      [ matForward m
      , matBackward m
      , matRight m
      , matLeft m
      , matUp m
      , matDown m
      ]

-- | Convert material table to pixel data for a lookup texture.
-- Returns (width, height, pixels) where width=6 (faces) and height=numMaterials.
-- Each pixel is a Float storing the texture layer index.
-- The texture should be created as Format RFloat.
materialsToPixels :: MaterialTable -> (Int, Int, [Float])
materialsToPixels materials = (6, numMats, pixels)
  where
    numMats = max 1 (V.length materials)  -- At least 1 row
    pixels = [getPixel idx | idx <- [0 .. 6 * numMats - 1]]
    getPixel idx =
      let matIdx = idx `div` 6
          faceIdx = idx `mod` 6
      in if matIdx < V.length materials
         then fromIntegral $ getFaceTexture (materials V.! matIdx) faceIdx
         else 0
