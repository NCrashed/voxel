module Data.Voxel.Transform(
    Transform(..)
  , notransform
  , transformMatrix
  , rotateTransform
  ) where 

import Linear 

-- | Transformation of space that are converted to M44 
-- matrix for rendering.
data Transform = Transform {
  transformTranslation :: !(V3 Float)
, transformRotation :: !(Quaternion Float)
, transformScale :: !(V3 Float)
}

-- | Transformation that does nothing
notransform :: Transform 
notransform = Transform {
    transformTranslation = V3 0 0 0
  , transformRotation = axisAngle (V3 0 0 1) 0
  , transformScale = V3 1 1 1
  }

instance Semigroup Transform where 
  a <> b = Transform {
      transformTranslation = transformTranslation a + transformTranslation b
    , transformRotation = transformRotation a * transformRotation b
    , transformScale = transformScale a * transformScale b
    }
  {-# INLINE (<>) #-}

instance Monoid Transform where 
  mempty = notransform
  {-# INLINE mempty #-}

-- | Convert transformation to 4x4 matrix
transformMatrix :: Transform -> M44 Float
transformMatrix Transform{..} = rotTrans !*! scalemat
  where 
    scalemat = scaleMat transformScale
    modelRot = fromQuaternion transformRotation
    rotTrans = mkTransformationMat modelRot transformTranslation

scaleMat :: V3 Float -> M44 Float
scaleMat (V3 x y z) = V4 
  (V4 x 0 0 0)
  (V4 0 y 0 0)
  (V4 0 0 z 0)
  (V4 0 0 0 1)

-- | Helper to apply rotation to transformation
rotateTransform :: Quaternion Float -> Transform -> Transform 
rotateTransform q t = t {
    transformRotation = q * transformRotation t
  }
{-# INLINE rotateTransform #-}