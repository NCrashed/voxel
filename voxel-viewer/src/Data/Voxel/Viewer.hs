module Data.Voxel.Viewer(
    runViewer
  ) where

import Control.Monad
-- import Control.Monad.IO.Class
import Control.Lens ((^.))
import Data.Maybe
import Graphics.GPipe
import Data.Word
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
  let faces = [
          (V3 (-1) (-1) (-1), V3 2 0 0,  V3 0 2 0)
        , (V3 (-1) (-1) (-1), V3 0 2 0,  V3 0 0 2)
        , (V3 (-1) (-1) (-1), V3 0 0 2,  V3 2 0 0)
        , (V3 1 1 1, V3 0 (-2) 0, V3 (-2) 0 0)
        , (V3 1 1 1, V3 0 0 (-2), V3 0 (-2) 0)
        , (V3 1 1 1, V3 (-2) 0 0, V3 0 0 (-2))
        ]
  let ntrigs = length faces * 2
  let nverts = length faces * 4
  positions :: Buffer os (B3 Float) <- newBuffer nverts
  normals   :: Buffer os (B3 Float) <- newBuffer nverts
  uvs       :: Buffer os (B2 Float) <- newBuffer nverts
  indecies  :: Buffer os (B Word32) <- newBuffer (ntrigs * 3)
  let mkPoses p0 u l = [ p0, p0 + l, p0 + u, p0 + u + l ]
      mkNormals u l = let n = normalize $ cross l u in replicate 4 n
      mkUvs = [ V2 0 1, V2 1 1, V2 0 0, V2 1 0 ]
      mkIndecies o = [ o, o+1, o+2, o+1, o+2, o+3 ]
  writeBuffer positions 0 $ concat [ mkPoses p0 u l | (p0, u, l) <- faces]
  writeBuffer normals 0 $ concat [ mkNormals u l | (_, u, l) <- faces]
  writeBuffer uvs 0 $ concat [ mkUvs | _ <- faces]
  writeBuffer indecies 0 $ concat [ mkIndecies (fromIntegral $ i * 4) | i <- [0 .. length faces - 1]]

  -- Make a Render action that returns a PrimitiveArray for the cube
  let makePrimitives = do
        pArr :: VertexArray () (B3 Float) <- newVertexArray positions
        nArr :: VertexArray () (B3 Float) <- newVertexArray normals
        uArr :: VertexArray () (B2 Float) <- newVertexArray uvs
        iArr :: IndexArray <- newIndexArray indecies Nothing
        let vals = zipVertices (,) pArr $ zipVertices (,) nArr uArr
        return $ toPrimitiveArrayIndexed TriangleList iArr vals

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
       os (PrimitiveArray Triangles (B3 Float, (B3 Float, B2 Float)))
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
  { primitives :: PrimitiveArray Triangles (B3 Float, (B3 Float, B2 Float))
  , rasterOptions :: (Side, ViewPort, DepthRange)
  }

-- Project the sides coordinates using the instance's normal and tangent
makeSide :: Fractional a => (V3 a, (V3 a, V2 a)) -> (V3 a, V3 a, V2 a)
makeSide (p, (normal, uv)) = (p, normal, uv)

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
