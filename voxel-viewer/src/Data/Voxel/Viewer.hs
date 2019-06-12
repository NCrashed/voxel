module Data.Voxel.Viewer(
    runViewer
  ) where

import Control.Monad
-- import Control.Monad.IO.Class
import Control.Lens ((^.))
import Data.Maybe
import Graphics.GPipe
-- import Linear

import qualified Graphics.GPipe.Context.GLFW as GLFW

runViewer :: IO ()
runViewer = runContextT GLFW.defaultHandleConfig $ do
  let wcfg = (GLFW.defaultWindowConfig "voxel viewer") {
          GLFW.configWidth = 800
        , GLFW.configHeight = 600
        }
  win <- newWindow (WindowFormatColorDepth RGBA8 Depth16) wcfg

  -- Create vertex data buffers
  positions :: Buffer os (B2 Float) <- newBuffer 4
  normals   :: Buffer os (B3 Float) <- newBuffer 6
  tangents  :: Buffer os (B3 Float) <- newBuffer 6
  writeBuffer positions 0 [V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]
  writeBuffer normals 0 [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)]
  writeBuffer tangents 0 [V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1), V3 (-1) 0 0, V3 1 0 0]

  -- Make a Render action that returns a PrimitiveArray for the cube
  let makePrimitives = do
        pArr <- newVertexArray positions
        nArr <- newVertexArray normals
        tArr <- newVertexArray tangents
        let sideInstances = zipVertices (,) nArr tArr
        return $ toPrimitiveArrayInstanced TriangleStrip (,) pArr sideInstances

  -- Create a buffer for the uniform values
  uniform :: Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float))) <- newBuffer 1

  -- Create the shader
  shader <- compileShader $ do
    sides <- fmap makeSide <$> toPrimitiveStream primitives
    (modelViewProj, normMat) <- getUniform (const (uniform, 0))
    let projectedSides = proj modelViewProj normMat <$> sides

    fragNormalsUV <- rasterize rasterOptions projectedSides
    let litFrags = light . fst <$> fragNormalsUV
        litFragsWithDepth = withRasterizedInfo
            (\a x -> (a, rasterizedFragCoord x ^. _z)) litFrags
        colorOption = ContextColorOption NoBlending (pure True)
        depthOption = DepthOption Less True

    drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth

  -- Run the loop
  loop win shader makePrimitives uniform 0

loop :: Window os RGBAFloat Depth
  -> (ShaderEnvironment -> Render os ())
  -> Render
       os (PrimitiveArray Triangles (B2 Float, (B3 Float, B3 Float)))
  -> Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float)))
  -> Float
  -> ContextT GLFW.Handle os IO ()
loop win shader makePrimitives uniform ang = do
  -- Write this frames uniform value
  size@(V2 w h) <- getFrameBufferSize win
  let modelRot = fromQuaternion (axisAngle (V3 1 0.5 0.3) ang)
      modelMat = mkTransformationMat modelRot (pure 0)
      projMat = perspective (pi/3) (fromIntegral w / fromIntegral h) 1 100
      viewMat = mkTransformationMat identity (- V3 0 0 5)
      viewProjMat = projMat !*! viewMat !*! modelMat
      normMat = modelRot
  writeBuffer uniform 0 [(viewProjMat, normMat)]

  -- Render the frame and present the results
  render $ do
    clearWindowColor win 0 -- Black
    clearWindowDepth win 1 -- Far plane
    prims <- makePrimitives
    shader $ ShaderEnvironment prims (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  unless (fromMaybe False closeRequested) $
    loop win shader makePrimitives uniform ((ang + 0.005) `mod''` (2*pi))

data ShaderEnvironment = ShaderEnvironment
  { primitives :: PrimitiveArray Triangles (B2 Float, (B3 Float, B3 Float))
  , rasterOptions :: (Side, ViewPort, DepthRange)
  }

-- Project the sides coordinates using the instance's normal and tangent
makeSide :: Fractional a => (V2 a, (V3 a, V3 a)) -> (V3 a, V3 a, V2 a)
makeSide (p@(V2 x y), (normal, tangent)) =
  (V3 x y 1 *! V3 tangent bitangent normal, normal, uv)
  where bitangent = cross normal tangent
        uv = (p + 1) / 2

-- Project the cube's positions and normals with ModelViewProjection matrix
proj :: V4 (V4 VFloat)
  -> V3 (V3 VFloat)
  -> (V3 VFloat, V3 VFloat, b)
  -> (V4 VFloat, (V3 FlatVFloat, b))
proj modelViewProj normMat (V3 px py pz, normal, uv) =
  (modelViewProj !* V4 px py pz 1, (fmap Flat $ normMat !* normal, uv))

-- Set color from sampler and apply directional light
light :: Num a => V3 a -> V4 a
light normal = V4 1 0 0 1 * pure (normal `dot` V3 0 0 1)
