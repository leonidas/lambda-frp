{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Logic (logic) where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Arrow
import Control.Category

import Control.Coroutine
import Control.Coroutine.FRP

import Graphics.UI.GLUT

import Lambda.OpenGL (KeyEvent(..))
import Lambda.Vector
import ViewModel

collides :: Bullet -> Invader -> Bool
collides (Bullet{..}) (Invader{..}) = dx < 29 && dy < 39
    where
        dx = abs $ vx bPos - vx iPos
        dy = abs $ vy bPos - vy iPos

logic :: Coroutine [KeyEvent] ViewModel
logic = proc keyEvents -> do

    -- Controls
    keyLeft  <- keyPressed (SpecialKey KeyLeft)  -< keyEvents
    keyRight <- keyPressed (SpecialKey KeyRight) -< keyEvents
    keyFire  <- keyPressed (Char ' ') -< keyEvents

    returnA -< ViewModel
        { turret   = Turret { tPos = Vec2 400 550 }
        , bullets  = Bullet { bPos = Vec2 410 370 } : []
        , invaders = zipWith Invader invaderPos invaderFrame
        }

    where
        invaderPos   = Vec2 <$> [64,128..10*64] <*> [64,128..4*64]
        invaderFrame = cycle [Walk1, Walk2, Death]

        keyPressed :: Key -> Coroutine [KeyEvent] Bool
        keyPressed k = filterE isInteresting >>> mapE isKeyDown >>> stepE False where
            isInteresting (KeyEvent{..}) = key == k
            isKeyDown (KeyEvent{..})     = keyState == Down
