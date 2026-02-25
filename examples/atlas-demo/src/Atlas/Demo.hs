-- | Demo of texture atlas rendering with grass and dirt materials
-- Demonstrates:
--   - 4 shadow-casting point lights using dual paraboloid shadows
--   - 2 simple fill lights (no shadows) for accent lighting
module Atlas.Demo(
    runDemo
) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Int
import Data.IORef
import Data.Voxel.App
import Data.Voxel.App.Class
import Data.Voxel.Camera
import Data.Voxel.Material
import Data.Voxel.Scene
import Data.Voxel.Shader.PhongAtlas
  ( PhongAtlasMultiShadowContext, newPhongAtlasMultiShadowContext, renderModelAtlasMultiShadow
  , MultiLighting(..), PointLight(..), ShadowConfig(..), defaultShadowConfig
  )
import Data.Voxel.Texture.Atlas
import Data.Voxel.Transform
import Data.Voxel.Window
import Graphics.GPipe
import Linear
import Reflex

import qualified Data.Voxel.Grid.Unbox as G
import qualified Data.Vector as V
import qualified Graphics.GPipe.Context.GLFW as GLFW

-- | Entry point to the demo
runDemo :: IO ()
runDemo = runAppHost $ do
  -- Create window
  let initWidth = 800
  let initHeight = 600
  win <- createWindow "Texture Atlas Demo" initWidth initHeight

  -- Load texture atlas
  -- Texture layers:
  --   0 = grass_top
  --   1 = grass_side
  --   2 = dirt
  --
  -- NOTE: Paths are relative to working directory when running!
  -- Run from examples/atlas-demo/ or adjust paths accordingly.
  let texturePaths =
        [ "textures/grass_top.png"
        , "textures/grass_side.png"
        , "textures/dirt.png"
        ]
      normalPaths =
        [ "textures/grass_top_normal.png"
        , "textures/grass_side_normal.png"
        , "textures/dirt_normal.png"
        ]

  liftIO $ putStrLn "Loading textures..."
  atlasResult <- loadTextureAtlasWithNormals texturePaths Nothing -- (Just normalPaths)
  atlas <- case atlasResult of
    Left err -> do
      liftIO $ putStrLn $ "Failed to load textures: " ++ err
      liftIO $ putStrLn "Trying without normal maps..."
      -- Fall back to loading without normal maps
      atlasResult2 <- loadTextureAtlas texturePaths
      case atlasResult2 of
        Left err2 -> fail $ "Failed to load textures: " ++ err2
        Right a -> do
          liftIO $ putStrLn $ "Loaded " ++ show (atlasLayers a) ++ " texture layers (no normal maps)"
          pure a
    Right a -> do
      liftIO $ putStrLn $ "Loaded " ++ show (atlasLayers a) ++ " texture layers with normal maps"
      pure a

  -- Define materials
  -- Material 0: empty (unused, but needed as placeholder)
  -- Material 1: grass (top=0, sides=1, bottom=2)
  -- Material 2: dirt (all faces=2)
  let materials = V.fromList
        [ solidMaterial 0           -- Material 0: placeholder
        , grassMaterial 0 1 2       -- Material 1: grass
        , solidMaterial 2           -- Material 2: dirt
        ]

  -- Create 2x2x2 voxel grid
  -- Bottom layer: dirt (material 2)
  -- Top layer: grass (material 1)
  let gridData =
        [ 2, 2, 2    -- z=0, y=0: x=0,1 (dirt)
        , 2, 2, 2
        , 2, 2, 0     
        , 1, 0, 0     
        , 2, 1, 0
        , 1, 0, 0
        , 0, 0, 0
        , 1, 0, 0
        , 0, 0, 0     
        ] :: [Int32]
      grid = G.fromList (V3 3 3 3) gridData

  liftIO $ putStrLn $ "Grid size: " ++ show (G.size grid)

  -- Prepare grid for rendering
  liftIO $ putStrLn "Preparing mesh..."
  scene <- prepareGridMaterial grid
  liftIO $ putStrLn $ "Generated " ++ show (V.length scene) ++ " LOD levels"

  when (V.null scene) $ do
    liftIO $ putStrLn "ERROR: No mesh generated! Grid may be empty or invalid."
    fail "No mesh generated"

  -- Setup camera
  -- Cube center is at origin with centeredTransform
  -- Position camera to look at origin from an angle
  let camera :: Camera Float
      camera = Camera
        { cameraPosition = V3 0 0 (-4)  -- Behind and above, looking at origin
        , cameraRotation =  (axisAngle (V3 1 0 0) (-0.6)) * (axisAngle (V3 0 0 1) (-0.3))  -- Tilt down to see top
        , cameraAngle = pi/4
        , cameraAspect = fromIntegral initWidth / fromIntegral initHeight
        , cameraNear = 0.1
        , cameraFar = 100
        }

  liftIO $ putStrLn "Creating render context..."

  -- Debug: print camera matrices
  liftIO $ do
    putStrLn $ "Camera position: " ++ show (cameraPosition camera)
    putStrLn $ "View matrix:\n" ++ show (cameraViewMat camera)
    putStrLn $ "Proj matrix:\n" ++ show (cameraProjMat camera)

  -- Create rendering context with multi-light shadow support
  let shadowCfg = defaultShadowConfig
        { shadowResolution = 1024  -- Shadow map resolution per cube face
        , shadowFar = 50.0         -- Light range
        , shadowBias = 0.0005
        }
  ctx <- newPhongAtlasMultiShadowContext win atlas materials camera shadowCfg

  liftIO $ putStrLn "Starting render loop..."
  liftIO $ putStrLn $ "Atlas has " ++ show (atlasLayers atlas) ++ " layers, size " ++ show (atlasSize atlas)
  liftIO $ putStrLn "Shadow lights: 4 dual paraboloid shadow-casting lights"
  liftIO $ putStrLn "Fill lights: green (below), pulsing purple"

  -- Run application
  runApp win $ demoApp ctx (V.head scene)

-- | Create a transform that rotates around the center of a scaled unit cube.
-- The mesh spans 0-1, scaled to 0-scale. We want rotation around center (scale/2).
centeredTransform :: V3 Float -> Quaternion Float -> V3 Float -> Transform Float
centeredTransform finalPos rotation scale = Transform
  { transformTranslation = translation
  , transformRotation = rotation
  , transformScale = scale
  }
  where
    -- Center of scaled cube (mesh 0-1 scaled to 0-scale, center at scale/2)
    center = scale ^/ 2
    -- After rotation around origin, center moves to rotate(center)
    -- We want the rotated center to end up at finalPos
    -- So: rotate(center) + translation = finalPos
    rotatedCenter = rotate rotation center
    translation = finalPos - rotatedCenter

-- | Main application loop with 4 shadow-casting lights (dual paraboloid) + 2 fill lights
demoApp :: forall t m os . MonadApp t os m
  => PhongAtlasMultiShadowContext os
  -> SceneModelMaterial os
  -> m ()
demoApp ctx model = do
  -- Track time for light animation
  timeRef <- liftIO $ newIORef (0 :: Float)

  -- Rendering loop
  setRenderer $ pure $ do
    -- Update time
    time <- liftIO $ do
      modifyIORef' timeRef (+ 0.005)  -- ~60fps timing
      readIORef timeRef

    -- Static model transform (centered at origin)
    let scale = V3 1.5 1.5 1.5
        finalPos = V3 0 0 0
        transform = centeredTransform finalPos (axisAngle (V3 0 0 1) 0) scale

    -- === Shadow-casting lights (4 max with dual paraboloid) ===

    -- Shadow light 1: Warm orange, fast orbit
    let angle1 = time * 1.5  -- Fast rotation
        radius1 = 2.5
        shadowLight1 = PointLight
          { pointLightPosition = V3 (radius1 * cos angle1) (radius1 * sin angle1) 2.0
          , pointLightColor    = V3 2.5 1.2 0.3  -- Warm orange
          , pointLightPower    = 20
          }

    -- Shadow light 2: Cool blue, slow orbit (opposite direction)
    let angle2 = time * 0.7  -- Slower rotation
        radius2 = 3.0
        shadowLight2 = PointLight
          { pointLightPosition = V3 (radius2 * cos (-angle2)) (radius2 * sin (-angle2)) 1.5
          , pointLightColor    = V3 0.4 0.8 2.5  -- Cool blue
          , pointLightPower    = 20
          }

    -- Shadow light 3: Soft cyan, medium orbit
    let angle3 = time * 1.0  -- Medium rotation
        radius3 = 2.8
        shadowLight3 = PointLight
          { pointLightPosition = V3 (radius3 * sin angle3) 2.0 (radius3 * cos angle3)
          , pointLightColor    = V3 0.3 0.9 1.0  -- Cyan
          , pointLightPower    = 18
          }

    -- Shadow light 4: Warm yellow, bobbing up and down
    let bob = sin (time * 2.0) * 0.5
        angle4 = time * 0.5  -- Very slow rotation
        shadowLight4 = PointLight
          { pointLightPosition = V3 (2.0 * cos angle4) (2.0 * sin angle4) (1.0 + bob)
          , pointLightColor    = V3 1.0 0.9 0.4  -- Yellow
          , pointLightPower    = 16
          }

    -- === Simple fill lights (no shadows, 2 for accent) ===

    -- Fill light 1: Soft green from below (like bioluminescence)
    let fillLight1 = PointLight
          { pointLightPosition = V3 0 0 (-1.5)  -- Below the scene
          , pointLightColor    = V3 0.2 0.8 0.3  -- Green
          , pointLightPower    = 15
          }

    -- Fill light 2: Soft purple, pulsing intensity
    let pulse = 0.5 + 0.5 * sin (time * 3.0)  -- Pulsing 0-1
        fillLight2 = PointLight
          { pointLightPosition = V3 2.5 2.5 0.5
          , pointLightColor    = V3 (0.6 * pulse) (0.2 * pulse) (0.8 * pulse)  -- Purple, pulsing
          , pointLightPower    = 12
          }

    let lighting = MultiLighting
          { multiLightingAmbient      = 0.005  -- Very slight ambient
          , multiLightingDirectional  = Nothing  -- No directional light (cave scene)
          , multiLightingShadowLights = [shadowLight1, shadowLight2, shadowLight3, shadowLight4]
          , multiLightingSimpleLights = [fillLight1, fillLight2]
          }

    renderModelAtlasMultiShadow ctx model transform lighting
