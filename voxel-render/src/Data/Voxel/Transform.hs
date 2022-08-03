module Data.Voxel.Transform(
    Transform(..)
  , notransform
  , transformMatrix
  , rotateTransform
  , translateTransform
  ) where 

import Linear 
import GHC.Generics (Generic)

-- | Transformation of space that are converted to M44 
-- matrix for rendering.
data Transform a = Transform {
  transformTranslation :: !(V3 a)
, transformRotation :: !(Quaternion a)
, transformScale :: !(V3 a)
} deriving (Show, Generic)

-- | Transformation that does nothing
notransform :: (Floating a, Epsilon a) => Transform a
notransform = Transform {
    transformTranslation = V3 0 0 0
  , transformRotation = axisAngle (V3 0 0 1) 0
  , transformScale = V3 1 1 1
  }

instance RealFloat a => Semigroup (Transform a) where 
  a <> b = Transform {
      transformTranslation = transformTranslation a + transformTranslation b
    , transformRotation = transformRotation a * transformRotation b
    , transformScale = transformScale a * transformScale b
    }
  {-# INLINE (<>) #-}

instance (RealFloat a, Epsilon a) => Monoid (Transform a) where 
  mempty = notransform
  {-# INLINE mempty #-}

-- | Convert transformation to 4x4 matrix
transformMatrix :: Num a => Transform a -> M44 a
transformMatrix Transform{..} = rotTrans !*! scalemat
  where 
    scalemat = scaleMat transformScale
    modelRot = fromQuaternion transformRotation
    rotTrans = mkTransformationMat modelRot transformTranslation

scaleMat :: Num a => V3 a -> M44 a
scaleMat (V3 x y z) = V4 
  (V4 x 0 0 0)
  (V4 0 y 0 0)
  (V4 0 0 z 0)
  (V4 0 0 0 1)
{-# INLINE scaleMat #-}

-- | Helper to apply rotation to transformation
rotateTransform :: RealFloat a => Quaternion a -> Transform a -> Transform a 
rotateTransform q t = t {
    transformRotation = q * transformRotation t
  }
{-# INLINE rotateTransform #-}

-- | Helper to apply translation to transformation
translateTransform :: Num a => V3 a -> Transform a -> Transform a 
translateTransform v t = t {
    transformTranslation = v + transformTranslation t 
  }
{-# INLINE translateTransform #-}