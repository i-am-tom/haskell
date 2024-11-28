{-# LANGUAGE BlockArguments #-}

-- |
-- Take snapshots of the OpenGL buffer.
module Graphics.Rendering.OpenGL.Snapshot where

import Codec.Picture (Image, PixelRGB8)
import Codec.Picture qualified as Codec
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy qualified as ByteString
import Data.Vector.Storable qualified as Vector
import Foreign.ForeignPtr qualified as Ptr
import Foreign.Marshal.Array (peekArray)
import Graphics.Rendering.OpenGL qualified as GL
import SDL (V2 (V2), Window)
import SDL qualified
import System.Process

-- | Snapshot the current state of the OpenGL buffer. Note that this is the
-- buffer to which you /write/, not the buffer being /displayed/.
snapshot :: forall m. (MonadIO m) => Window -> (Image PixelRGB8 -> m ()) -> m ()
snapshot window handle = do
  V2 width height <- SDL.get (SDL.windowSize window)

  image <- liftIO do
    buffer <- Ptr.mallocForeignPtrArray do
      3 * fromIntegral width * fromIntegral height

    Ptr.withForeignPtr buffer \pointer -> do
      let size :: GL.Size
          size = GL.Size (fromIntegral width) (fromIntegral height)

      GL.readPixels (GL.Position 0 0) size do
        GL.PixelData GL.RGB GL.UnsignedByte pointer

      stream <- peekArray (3 * fromIntegral width * fromIntegral height) pointer
      pure (Vector.fromList stream)

  handle (Codec.Image (fromIntegral width) (fromIntegral height) image)

-- | Create a 'snapshot' handler that writes frames to a video file.
record :: forall m. (MonadFail m, MonadIO m) => FilePath -> m (Image PixelRGB8 -> m ())
record path = do
  let arguments :: [String]
      arguments =
        [ "-y",
          "-f",
          "image2pipe",
          "-c:v",
          "png",
          "-r",
          "60",
          "-i",
          "-",
          "-an",
          "-vcodec",
          "libx264",
          "-crf",
          "1",
          "-pix_fmt",
          "yuv420p",
          path
        ]

  (Just stdin, _, _, _) <- liftIO do
    createProcess
      (proc "ffmpeg" arguments)
        { std_in = CreatePipe
        }

  pure (liftIO . ByteString.hPut stdin . Codec.encodePng)
