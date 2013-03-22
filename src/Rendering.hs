{-# LANGUAGE RecordWildCards #-}

module Rendering (render) where

import Control.Monad
import Graphics.Rendering.OpenGL

import Lambda.Vector

import ViewModel

render :: ViewModel -> IO ()
render (ViewModel {..}) = do
    renderTurret turret
    mapM_ renderBullet bullets
    mapM_ renderInvader invaders

renderTurret :: Turret -> IO ()
renderTurret (Turret{..}) = do
    setColor 0.2 0.8 0.2
    drawRect (tPos - Vec2 0 8) (Vec2 50 16)
    drawRect (tPos - Vec2 0 18) (Vec2 6 10)

renderBullet :: Bullet -> IO ()
renderBullet = undefined

renderInvader :: Invader -> IO ()
renderInvader (Invader{..}) = do
    setColor 0.9 0.9 0.9
    drawRect iPos (Vec2 24 24)
    setColor 0.9 0.1 0.1
    drawRotated (iPos - Vec2 16 0) (pi/3) 160 160

    setColor 0 0 0
    drawRect (iPos + Vec2 6 0) (Vec2 4 8)
    drawRect (iPos - Vec2 6 0) (Vec2 4 8)

setColor :: GLfloat -> GLfloat -> GLfloat -> IO ()
setColor r g b = currentColor $= Color4 r g b 1.0

drawRotated :: Vec2 -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawRotated (Vec2 x y) rot len w = do
    let w' = w / 2
        r' = rot + pi
        x' = x + (cos rot * len)
        y' = y - (sin rot * len)
        dx = cos r' * w'
        dy = -sin r' * w'

    print (x,y)
    print (dx, dy)
    print (x', y')

    -- drawCircle (Vec2 x y + Vec2 (cos r' * w') (-sin r' * w')) 3
    -- drawCircle (Vec2 x' y') 3

    {-
    renderPrimitive Quads $ mapM_ (vertex . uncurry Vertex2)
        [ (x - dx, y - dy)
        , (x' + dx, y' + dy)
        , (x + dx, y + dy)
        , (x' - dx, y' - dy)
        ]
    -}

    renderPrimitive Quads $ mapM_ (vertex . uncurry Vertex2)
        [ (fromIntegral $ truncate (x - dx), y - dy)
        , (x + 10, y + 10)
        , (x' + 10, y' + 10)
        , (x' - 10, y' - 10)
        ]


drawRect :: Vec2 -> Vec2 -> IO ()
drawRect (Vec2 x y) (Vec2 w h) = do
    let w' = w / 2
        h' = h / 2

    renderPrimitive Quads $ mapM_ (vertex . uncurry Vertex2)
            [(x-w',y-h'), (x+w',y-h'), (x+w',y+h'), (x-w',y+h')]

drawCircle :: Vec2 -> GLfloat -> IO ()
drawCircle (Vec2 x y) radius = do
    renderPrimitive TriangleFan $ do
        vertex $ Vertex2 x y
        forM_ radians $ \r ->
            let vx = x + cos r * radius
                vy = y - sin r * radius
            in vertex $ Vertex2 vx vy
    where
        radians = map (\a -> a * pi / 16) [0..32]
