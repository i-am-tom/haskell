# `Paparazzi` OpenGL snapshotting

I want to take screenshots of an OpenGL buffer, and I'd like to stream them out
into an MP4 file. This package lets me do that.

## How?

```haskell
snapshot :: forall m. MonadIO m => Window -> (Image PixelRGB8 -> m ()) -> m ()
```

If your SDL `Window` has an active OpenGL context, then you can call `snapshot`
at any moment to get access to the OpenGL buffer as a `JuicyPixels` image.

```haskell
record :: forall m. (MonadFail m, MonadIO m) => FilePath -> m (Image PixelRGB8 -> m ())
```

With the help of `ffmpeg`, this function produces a handler for `snapshot` that
adds the image as a frame in an MP4 at the given file path. This means you can
turn an OpenGL animation into a video file by adding `snapshot recordHandle` in
your rendering loop. See the `tests` directory for an example.
