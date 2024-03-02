{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL qualified as GL
import Graphics.Rendering.OpenGL.Snapshot (record, snapshot)
import SDL hiding (Vector, angle)

main :: IO ()
main = do
  initializeAll

  window <-
    createWindow "test" defaultWindow
      { windowGraphicsContext =
          OpenGLContext defaultOpenGL
            { glProfile = Core Normal 4 1
            }
      , windowInitialSize = V2 800 800
      , windowVisible = True
      }

  context <- glCreateContext window
  glMakeCurrent window context

  showWindow window

  vertex <- GL.createShader GL.VertexShader
  ByteString.readFile "./tests/shader.vert" >>= \source ->
    GL.shaderSourceBS vertex $= source
  GL.compileShader vertex
  GL.shaderInfoLog vertex >>= print

  fragment <- GL.createShader GL.FragmentShader
  ByteString.readFile "./tests/shader.frag" >>= \source ->
    GL.shaderSourceBS fragment $= source
  GL.compileShader fragment
  GL.shaderInfoLog fragment >>= print

  program <- GL.createProgram
  GL.attachShader program vertex
  GL.attachShader program fragment

  vertexArrayObject <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vertexArrayObject

  GL.linkProgram program
  GL.validateProgram program

  position <- GL.get (GL.attribLocation program "position")
  angle <- GL.get (GL.uniformLocation program "angle")

  arrayBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just arrayBuffer

  withArray [ 0.0 :: GL.GLfloat, 0.8, -0.8, 0.8, -0.8 ] \pointer -> do
    GL.bufferData GL.ArrayBuffer $= (12, pointer, GL.StaticDraw)

    GL.vertexAttribPointer position $=
      (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr)

  GL.viewport $= (GL.Position 0 0, GL.Size 800 800)
  GL.currentProgram $= Just program

  GL.vertexAttribArray position $= GL.Enabled
  recorder <- record "./tests/example.mp4"

  let loop :: GL.GLfloat -> IO ()
      loop count = unless (count <= 0) do
        _ <- SDL.pollEvents
        GL.uniform angle $= count

        GL.clear [GL.ColorBuffer]
        GL.drawArrays GL.Triangles 0 3

        snapshot window recorder

        glSwapWindow window
        loop (count - 1)

  loop 1000
  glDeleteContext context

