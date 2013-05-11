{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Logic (logic) where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Arrow
import Control.Category

import Control.Coroutine
import Control.Coroutine.FRP
import Control.Coroutine.FRP.Collections

import Lambda.OpenGL (KeyEvent(..))
import Lambda.Vector
import ViewModel

collides :: Bullet -> Invader -> Bool
collides (Bullet{..}) (Invader{..}) = dx < 29 && dy < 39
    where
        dx = abs $ vx bPos - vx iPos
        dy = abs $ vy bPos - vy iPos

invader :: Invader -> Coroutine () (Maybe Invader)
invader (Invader{..}) = proc _ -> do
    let pos   = iPos
        frame = iFrame
    returnA -< Just $ Invader { iPos = pos, iFrame = frame }

logic :: Coroutine [KeyEvent] ViewModel
logic = proc _ -> do

    invaders <- collection initialInvaders -< ((), [])

    returnA -< ViewModel
        { turret   = Turret { tPos = Vec2 400 550 }
        , bullets  = Bullet { bPos = Vec2 410 370 } : []
        , invaders = invaders
        }

    where
        initialInvaders = map invader $ zipWith Invader invaderPos invaderFrame
        invaderPos   = Vec2 <$> [64,128..10*64] <*> [64,128..4*64]
        invaderFrame = cycle [Walk1, Walk2, Death]
