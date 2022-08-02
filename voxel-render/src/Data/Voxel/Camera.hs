module Data.Voxel.Camera(
    Camera(..)
  , cameraViewMat
  , cameraProjMat
  ) where 

import Linear 
import GHC.Generics (Generic)

-- | Camera that defines projection and view transformations.
-- It is the window which player uses to view the scene.
data Camera a = Camera {
  cameraPosition :: !(V3 a)
, cameraRotation :: !(Quaternion a)
, cameraAngle    :: !a 
, cameraAspect   :: !a
, cameraNear     :: !a 
, cameraFar      :: !a
} deriving (Show, Generic)

-- | Get matrix that transforms world coordinates to view volume
cameraViewMat :: Floating a => Camera a -> M44 a 
cameraViewMat Camera{..} = mkTransformation cameraRotation cameraPosition

-- | Get matrix that transforms view coordinates to window volume
cameraProjMat :: Floating a => Camera a -> M44 a 
cameraProjMat Camera{..} = perspective cameraAngle cameraAspect cameraNear cameraFar