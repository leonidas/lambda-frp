{-# LANGUAGE RecordWildCards #-}

module Rendering (render) where

import Control.Monad
import Graphics.Rendering.OpenGL

import Lambda.Vector

import ViewModel

render :: ViewModel -> IO ()
render (ViewModel {..}) = drawCircle ballPos ballRadius
    where Ball{..} = ball

drawCircle :: Vec2 -> GLfloat -> IO ()
drawCircle (Vec2 x y) radius = do
    currentColor $= Color4 0.9 0.9 0.9 1.0
    renderPrimitive TriangleFan $ do
        vertex $ Vertex2 x y
        forM_ radians $ \r ->
            let vx = x + cos r * radius
                vy = y - sin r * radius
            in vertex $ Vertex2 vx vy
    where
        radians = map (\a -> a * pi / 16) [0..32]