module Data.Voxel.UI.Button(
    ButtonSkin(..)
  , ButtonStyle(..)
  , ButtonCfg(..)
  , Button(..)
  , button
  ) where
  
import Data.Text (Text) 
import Data.Voxel.Rect 
import Data.Voxel.UI.Class
import Data.Voxel.App.Class
import GHC.Generics 
import Reflex
import Linear 

-- | Region of texture to define a button
data ButtonSkin = ButtonSkin {
  _buttonSkin_leftUpCorner    :: !Region 
, _buttonSkin_rightUpCorner   :: !Region 
, _buttonSkin_leftDownCorner  :: !Region 
, _buttonSkin_rightDownCorner :: !Region 
, _buttonSkin_horizontalBar   :: !Region 
, _buttonSkin_verticalBar     :: !Region
} deriving (Generic)

-- | Defines how the button looks like
data ButtonStyle = ButtonStyle {
  _buttonStyle_main  :: !ButtonSkin
, _buttonStyle_hover :: !ButtonSkin
, _buttonStyle_press :: !ButtonSkin
} deriving (Generic)

data ButtonCfg t = ButtonCfg {
  _buttonCfg_style :: !ButtonStyle
, _buttonCfg_text  :: Dynamic t Text
, _buttonCfg_pos   :: Dynamic t (V2 Float)
, _buttonCfg_size  :: Dynamic t (V2 Float)
} deriving (Generic)

data Button t = Button {
  _button_click :: Event t ()
} deriving (Generic)

button :: (MonadUI t m, MonadGPipe t os m) => ButtonCfg t -> m (Button t)
button _ = do 
  pure $ Button {
      _button_click = never
    }