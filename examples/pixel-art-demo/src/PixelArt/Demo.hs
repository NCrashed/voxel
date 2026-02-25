-- | Demo of pixel-art post-processing pipeline
-- Demonstrates:
--   - Rendering to off-screen texture
--   - Downscaling with point filtering for crisp pixels
--   - Upscaling with point filtering for blocky retro look
--   - Multiple shadow-casting lights
--   - CIE Lab color quantization with switchable palettes
--
-- Controls:
--   - 0: No palette (downsampling only)
--   - 1, 2, 3, 4: Switch between palettes
--   - ESC: Close window
module PixelArt.Demo(
    runDemo
) where

import Control.Monad (when, forM)
import Control.Monad.IO.Class
import Data.Int
import Data.IORef
import Data.Voxel.App
import Data.Voxel.App.Class
import Data.Voxel.Camera
import Data.Voxel.Material
import Data.Voxel.PostProcess.PixelArt
import Data.Voxel.Scene
import Data.Voxel.Shader.PhongAtlas
  ( PhongAtlasMultiShadowContext(..), newPhongAtlasMultiShadowContext
  , renderModelAtlasMultiShadowToImages
  , MultiLighting(..), PointLight(..), ShadowConfig(..), defaultShadowConfig
  )
import Data.Voxel.Texture.Atlas
import Data.Voxel.Transform
import Data.Voxel.Window
import Graphics.GPipe
import Linear
import Reflex
import System.Directory (doesFileExist)

import qualified Data.Voxel.Grid.Unbox as G
import qualified Data.Vector as V
import qualified Graphics.GPipe.Context.GLFW as GLFW

--------------------------------------------------------------------------------
-- Palettes
--------------------------------------------------------------------------------

-- | Fantasy console inspired palette (warm tones)
palette1 :: Palette
palette1 = V.fromList
  [ V3 0.07 0.07 0.12  -- Dark blue-black
  , V3 0.20 0.16 0.28  -- Dark purple
  , V3 0.35 0.24 0.35  -- Purple
  , V3 0.55 0.35 0.44  -- Mauve
  , V3 0.76 0.53 0.49  -- Salmon
  , V3 0.93 0.76 0.65  -- Peach
  , V3 0.98 0.95 0.82  -- Cream
  , V3 0.67 0.85 0.65  -- Light green
  , V3 0.38 0.65 0.52  -- Green
  , V3 0.24 0.45 0.45  -- Teal
  , V3 0.20 0.28 0.40  -- Dark teal
  , V3 0.28 0.36 0.55  -- Blue
  , V3 0.42 0.52 0.72  -- Light blue
  , V3 0.62 0.70 0.82  -- Pale blue
  , V3 0.45 0.40 0.32  -- Brown
  , V3 0.72 0.60 0.45  -- Tan
  ]

-- | Cool blue/cyan palette
palette2 :: Palette
palette2 = V.fromList
  [ V3 0.05 0.05 0.10  -- Near black
  , V3 0.10 0.12 0.20  -- Dark blue
  , V3 0.15 0.20 0.35  -- Navy
  , V3 0.20 0.30 0.50  -- Blue
  , V3 0.25 0.45 0.65  -- Medium blue
  , V3 0.35 0.60 0.75  -- Sky blue
  , V3 0.50 0.75 0.85  -- Light blue
  , V3 0.70 0.88 0.92  -- Pale cyan
  , V3 0.85 0.95 0.95  -- Near white
  , V3 0.15 0.35 0.35  -- Dark teal
  , V3 0.20 0.50 0.50  -- Teal
  , V3 0.30 0.65 0.60  -- Cyan
  , V3 0.45 0.75 0.70  -- Light cyan
  , V3 0.25 0.25 0.40  -- Slate
  , V3 0.40 0.40 0.55  -- Gray blue
  , V3 0.60 0.60 0.70  -- Light gray
  ]

-- | Warm sunset palette
palette3 :: Palette
palette3 = V.fromList
  [ V3 0.08 0.04 0.06  -- Dark brown
  , V3 0.20 0.08 0.10  -- Maroon
  , V3 0.40 0.12 0.12  -- Dark red
  , V3 0.60 0.20 0.15  -- Red
  , V3 0.80 0.35 0.20  -- Orange red
  , V3 0.95 0.55 0.25  -- Orange
  , V3 1.00 0.75 0.35  -- Gold
  , V3 1.00 0.90 0.55  -- Yellow
  , V3 1.00 0.98 0.80  -- Cream
  , V3 0.50 0.25 0.20  -- Brown
  , V3 0.65 0.40 0.30  -- Tan
  , V3 0.80 0.55 0.40  -- Light brown
  , V3 0.30 0.15 0.25  -- Dark purple
  , V3 0.50 0.25 0.45  -- Purple
  , V3 0.70 0.40 0.60  -- Pink
  , V3 0.90 0.70 0.80  -- Light pink
  ]

-- | Grayscale palette
palette4 :: Palette
palette4 = V.fromList
  [ V3 0.00 0.00 0.00  -- Black
  , V3 0.07 0.07 0.07
  , V3 0.13 0.13 0.13
  , V3 0.20 0.20 0.20
  , V3 0.27 0.27 0.27
  , V3 0.33 0.33 0.33
  , V3 0.40 0.40 0.40
  , V3 0.47 0.47 0.47
  , V3 0.53 0.53 0.53
  , V3 0.60 0.60 0.60
  , V3 0.67 0.67 0.67
  , V3 0.73 0.73 0.73
  , V3 0.80 0.80 0.80
  , V3 0.87 0.87 0.87
  , V3 0.93 0.93 0.93
  , V3 1.00 1.00 1.00  -- White
  ]

-- | All available palettes with names (fallback if files not found)
defaultPalettes :: [(String, Palette)]
defaultPalettes =
  [ ("Fantasy (warm)", palette1)
  , ("Cool blue", palette2)
  , ("Sunset", palette3)
  , ("Grayscale", palette4)
  ]

-- | Palette file paths to try loading
paletteFiles :: [(String, FilePath)]
paletteFiles =
  [ ("Palette 1", "palettes/palette1.png")
  , ("Palette 2", "palettes/palette2.png")
  , ("Palette 3", "palettes/palette3.png")
  , ("Palette 4", "palettes/palette4.png")
  ]

-- | Load palettes from files, falling back to defaults if not found
loadPalettes :: IO [(String, Palette)]
loadPalettes = do
  results <- forM (zip paletteFiles defaultPalettes) $ \((name, path), (defName, defPalette)) -> do
    exists <- doesFileExist path
    if exists
      then do
        result <- loadPaletteFromImage path
        case result of
          Right palette -> do
            putStrLn $ "Loaded palette from: " ++ path ++ " (" ++ show (V.length palette) ++ " colors)"
            pure (name, palette)
          Left err -> do
            putStrLn $ "Failed to load " ++ path ++ ": " ++ err ++ ", using default"
            pure (defName, defPalette)
      else do
        putStrLn $ "Palette file not found: " ++ path ++ ", using default: " ++ defName
        pure (defName, defPalette)
  pure results

--------------------------------------------------------------------------------
-- Demo
--------------------------------------------------------------------------------

-- | Entry point to the demo
runDemo :: IO ()
runDemo = do
  -- Load palettes from files (or use defaults)
  putStrLn "Loading palettes..."
  allPalettes <- loadPalettes

  runAppHost $ do
    -- Create window
    let initWidth = 800
    let initHeight = 600
    win <- createWindow "Pixel Art Demo" initWidth initHeight

    -- Load texture atlas
    -- Texture layers:
    --   0 = grass_top
    --   1 = grass_side
    --   2 = dirt
    --
    -- NOTE: Paths are relative to working directory when running!
    -- Run from examples/pixel-art-demo/ or adjust paths accordingly.
    let texturePaths =
          [ "textures/grass_top.png"
          , "textures/grass_side.png"
          , "textures/dirt.png"
          ]

    liftIO $ putStrLn "Loading textures..."
    atlasResult <- loadTextureAtlas texturePaths
    atlas <- case atlasResult of
      Left err -> fail $ "Failed to load textures: " ++ err
      Right a -> do
        liftIO $ putStrLn $ "Loaded " ++ show (atlasLayers a) ++ " texture layers"
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

    -- Create 3x3x3 voxel grid
    -- Bottom layer: dirt (material 2)
    -- Top layer: grass (material 1)
    let gridData =
          [ 2, 2, 2    -- z=0, y=0: x=0,1,2 (dirt)
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
    let camera :: Camera Float
        camera = Camera
          { cameraPosition = V3 0 0 (-4)
          , cameraRotation = (axisAngle (V3 1 0 0) (-0.6)) * (axisAngle (V3 0 0 1) (-0.3))
          , cameraAngle = pi/4
          , cameraAspect = fromIntegral initWidth / fromIntegral initHeight
          , cameraNear = 0.1
          , cameraFar = 100
          }

    liftIO $ putStrLn "Creating render context..."

    -- Create rendering context with multi-light shadow support
    let shadowCfg = defaultShadowConfig
          { shadowResolution = 1024
          , shadowFar = 50.0
          , shadowBias = 0.0005
          }
    ctx <- newPhongAtlasMultiShadowContext win atlas materials camera shadowCfg

    -- Create pixel-art post-processing pipeline
    -- Try different downscale factors: 2, 4, 8 for varying pixel sizes
    let firstPalette = snd (head allPalettes)
    let paConfig = PixelArtConfig
          { paDownscaleFactor = 4      -- 4x downscale for chunky pixels
          , paHighResSize = V2 initWidth initHeight
          , paPalette = Just firstPalette  -- Start with first palette
          , paDitherStrength = 0.0     -- Disable ordered dithering
          }
    paCtx <- newPixelArtContext win paConfig

    -- Track current palette index
    paletteIdxRef <- liftIO $ newIORef (0 :: Int)

    -- Track dither strength
    ditherStrengthRef <- liftIO $ newIORef (paDitherStrength paConfig)

    -- Convert to vector for indexed access
    let palettesVec = V.fromList allPalettes

    liftIO $ putStrLn "Starting render loop..."
    liftIO $ putStrLn $ "Atlas has " ++ show (atlasLayers atlas) ++ " layers, size " ++ show (atlasSize atlas)
    liftIO $ putStrLn "Shadow lights: 4 dual paraboloid shadow-casting lights"
    liftIO $ putStrLn "Fill lights: green (below), pulsing purple"
    liftIO $ putStrLn $ "Pixel-art mode: " ++ show (paDownscaleFactor paConfig) ++ "x downscale"
    liftIO $ putStrLn $ "Effective resolution: " ++ show (computeLowResSize paConfig)
    liftIO $ putStrLn ""
    liftIO $ putStrLn "=== Palettes ==="
    liftIO $ V.imapM_ (\i (name, _) -> putStrLn $ "  " ++ show (i+1) ++ ": " ++ name) palettesVec
    liftIO $ putStrLn ""
    liftIO $ putStrLn "Controls:"
    liftIO $ putStrLn "  0     - No palette (downsampling only)"
    liftIO $ putStrLn "  1-4   - Switch palettes"
    liftIO $ putStrLn "  +/=   - Increase dither strength"
    liftIO $ putStrLn "  -     - Decrease dither strength"

    -- Run application
    runApp win $ demoApp ctx paCtx paletteIdxRef ditherStrengthRef palettesVec (V.head scene)

-- | Create a transform that rotates around the center of a scaled unit cube.
centeredTransform :: V3 Float -> Quaternion Float -> V3 Float -> Transform Float
centeredTransform finalPos rotation scale = Transform
  { transformTranslation = translation
  , transformRotation = rotation
  , transformScale = scale
  }
  where
    center = scale ^/ 2
    rotatedCenter = rotate rotation center
    translation = finalPos - rotatedCenter

-- | Main application loop with pixel-art post-processing
demoApp :: forall t m os . MonadApp t os m
  => PhongAtlasMultiShadowContext os
  -> PixelArtContext os
  -> IORef Int    -- ^ Current palette index
  -> IORef Float  -- ^ Current dither strength
  -> V.Vector (String, Palette)  -- ^ Available palettes
  -> SceneModelMaterial os
  -> m ()
demoApp ctx paCtx paletteIdxRef ditherStrengthRef palettes model = do
  bindEscapeToClose

  -- Track time for light animation
  timeRef <- liftIO $ newIORef (0 :: Float)

  -- Track last palette index to detect changes
  lastPaletteRef <- liftIO $ newIORef (-1 :: Int)

  -- Track last dither strength to detect changes
  lastDitherRef <- liftIO $ newIORef (-1 :: Float)

  let viewportSize = paHighResSize (paConfig paCtx)

  -- Handle palette switching with number keys
  -- Index -1 means "no palette" (downsampling only)
  keyE <- keyInput
  performEvent_ $ ffor keyE $ \ke -> do
    when (_keyEvent_state ke == GLFW.KeyState'Pressed) $ do
      -- Handle palette switching
      let mNewIdx = case _keyEvent_key ke of
            GLFW.Key'0 -> Just (-1)  -- No palette
            GLFW.Key'1 -> Just 0
            GLFW.Key'2 -> Just 1
            GLFW.Key'3 -> Just 2
            GLFW.Key'4 -> Just 3
            _ -> Nothing
      case mNewIdx of
        Just (-1) -> liftIO $ do
          writeIORef paletteIdxRef (-1)
          putStrLn "Switched to: No palette (downsampling only)"
        Just idx -> liftIO $ when (idx < V.length palettes) $ do
          writeIORef paletteIdxRef idx
          let (name, _) = palettes V.! idx
          putStrLn $ "Switched to palette: " ++ name
        Nothing -> pure ()

      -- Handle dither strength adjustment
      let ditherDelta = case _keyEvent_key ke of
            GLFW.Key'Equal -> Just 0.1      -- + key (shared with =)
            GLFW.Key'Minus -> Just (-0.1)   -- - key
            _ -> Nothing
      case ditherDelta of
        Just delta -> liftIO $ do
          oldStrength <- readIORef ditherStrengthRef
          let s = max 0 (oldStrength + delta)  -- Clamp to >= 0
          writeIORef ditherStrengthRef s
          putStrLn $ "Dither strength: " ++ show s
        Nothing -> pure ()

  -- Rendering loop
  setRenderer $ pure $ do
    -- Check if palette changed and update
    currentIdx <- liftIO $ readIORef paletteIdxRef
    lastIdx <- liftIO $ readIORef lastPaletteRef
    currentDither <- liftIO $ readIORef ditherStrengthRef
    when (currentIdx /= lastIdx && currentIdx >= 0 && currentIdx < V.length palettes) $ do
      let (_, newPalette) = palettes V.! currentIdx
      updatePixelArtPalette paCtx newPalette
      -- Also restore current dither strength after palette update
      updatePixelArtDitherStrength paCtx currentDither
      liftIO $ writeIORef lastPaletteRef currentIdx

    -- Check if dither strength changed and update
    lastDither <- liftIO $ readIORef lastDitherRef
    when (currentDither /= lastDither) $ do
      updatePixelArtDitherStrength paCtx currentDither
      liftIO $ writeIORef lastDitherRef currentDither

    -- Update time
    time <- liftIO $ do
      modifyIORef' timeRef (+ 0.005)
      readIORef timeRef

    -- Static model transform (centered at origin)
    let scale = V3 1.5 1.5 1.5
        finalPos = V3 0 0 0
        transform = centeredTransform finalPos (axisAngle (V3 0 0 1) 0) scale

    -- === Shadow-casting lights (4 max with dual paraboloid) ===

    -- Shadow light 1: Warm orange, fast orbit
    let angle1 = time * 1.5
        radius1 = 2.5
        shadowLight1 = PointLight
          { pointLightPosition = V3 (radius1 * cos angle1) (radius1 * sin angle1) 2.0
          , pointLightColor    = V3 2.5 1.2 0.3
          , pointLightPower    = 1
          }

    let lighting = MultiLighting
          { multiLightingAmbient      = 0.0001
          , multiLightingDirectional  = Nothing
          , multiLightingShadowLights = [shadowLight1]
          , multiLightingSimpleLights = []
          }

    -- Step 1: Render scene to high-res off-screen textures (color + depth)
    let textures = getHighResTextures paCtx
    renderModelAtlasMultiShadowToImages ctx textures viewportSize model transform lighting

    -- Step 2: Downsample and upscale to window using pixel-art pipeline
    -- Apply quantization only if a palette is selected (index >= 0)
    let useQuantization = currentIdx >= 0
    executePixelArtPipelineEx paCtx (pmsWindow ctx) useQuantization
