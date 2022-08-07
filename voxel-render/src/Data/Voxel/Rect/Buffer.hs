{-# LANGUAGE Arrows #-}
module Data.Voxel.Rect.Buffer(
    RectBuffers(..)
  , RectVertexArr(..)
  , RectVertex(..)
  , newRectBuffers
  , writeRectBuffers
  , rectBuffers
  , rectBufferArray
  ) where 

import Control.Arrow
import Control.Lens ((^.))
import Control.Monad 
import Control.Monad.IO.Class 
import Data.Vector (Vector)
import Data.Voxel.App (SpiderCtx)
import Data.Voxel.Rect 
import Data.Word
import GHC.Generics (Generic)
import Graphics.GPipe

import qualified Data.Vector as V 

-- | Structure that holds all buffers for rects
data RectBuffers os = RectBuffers {
-- | Holds triangles vertecies
  rectBuffPositions :: !(Buffer os (B3 Float))
-- | Holds triangles uv coords
, rectBuffUvs       :: !(Buffer os (B2 Float))
-- | Holds triples of indecies, each index corresponds to elements
-- from the previous buffers.
, rectBuffIndecies  :: !(Buffer os (B Word32))
} deriving (Eq, Generic)

-- | Structure that is used inside shader primitive array
data RectVertexArr = RectVertexArr {
  rectArrPosition :: !(B3 Float)
, rectArrUv       :: !(B2 Float)
}

-- | Structure that is used inside vertex shader
data RectVertex = RectVertex {
  rectPrimPosition :: !(V3 VFloat)
, rectPrimUv       :: !(V2 VFloat)
}

instance VertexInput RectVertexArr where
  type VertexFormat RectVertexArr = RectVertex
  toVertex = proc ~(RectVertexArr p u) -> do
    p' <- toVertex -< p
    u' <- toVertex -< u
    returnA -< RectVertex p' u'

-- | Create new buffers with given capacity (count of rects)
newRectBuffers :: (MonadIO m, ContextHandler ctx)
  => Int -- ^ Count of rects
  -> ContextT ctx os m (RectBuffers os)
newRectBuffers n = RectBuffers
  <$> newBuffer nverts
  <*> newBuffer nverts
  <*> newBuffer (ntrigs * 3)
  where 
    nverts = 4 * n 
    ntrigs = 2 * n 

-- | Write contents of rects to the given buffer. Note that size of buffer should
-- match the vector.
writeRectBuffers :: (MonadIO m, ContextHandler ctx)
  => Vector Rect
  -> RectBuffers os
  -> ContextT ctx os m ()
writeRectBuffers vs RectBuffers{..} = V.forM_ (V.indexed vs) $ \(i, r@Rect{..}) -> do
  let z = rectZPos r
  let poses = (\(V2 x y) -> V3 x y z) <$> regionPoints rectPos 
  guardedWrite "positions" (i*4) rectBuffPositions poses
  guardedWrite "uvs" (i*4) rectBuffUvs $ regionUvs rectUv
  guardedWrite "indicies" (i*2*3) rectBuffIndecies regionIndecies
  where 
    guardedWrite name offset buff vec = do
      when (bufferLength buff < offset + length vec) $ 
        fail $ name ++ " buffer size " ++ 
          show (bufferLength buff) ++ 
          ", but expected " ++ (show $ offset + length vec)
      writeBuffer buff offset vec

-- | Create new buffers and fill it with vertecies from rects
rectBuffers :: forall m ctx os . (MonadIO m, ContextHandler ctx)
  => Vector Rect
  -> ContextT ctx os m (RectBuffers os)
rectBuffers vs = do
  when (V.null vs) $ fail "Cannot fill rect buffers with no vertecies!"
  bs <- newRectBuffers (V.length vs)
  writeRectBuffers vs bs
  pure bs

-- | Convert rect buffers to rect vertex arrays
rectBufferArray :: RectBuffers os -> Render os (PrimitiveArray Triangles RectVertexArr)
rectBufferArray RectBuffers{..} = do
  p :: VertexArray () (B3 Float) <- newVertexArray rectBuffPositions
  u :: VertexArray () (B2 Float) <- newVertexArray rectBuffUvs
  i :: IndexArray <- newIndexArray rectBuffIndecies Nothing
  pure $ toPrimitiveArrayIndexed TriangleList i $ zipVertices RectVertexArr p u

