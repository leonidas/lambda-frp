{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Logic (logic) where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Arrow
-- import Control.Category

import Control.Coroutine
import Control.Coroutine.FRP
import Control.Coroutine.FRP.Collections

import Graphics.UI.GLUT

import Lambda.OpenGL (KeyEvent(..))
import Lambda.Vector
import ViewModel

throttle :: Int -> Coroutine (Event a) (Event a)
throttle minTicks = proc evs -> do
    rec ticksSinceLast <- restartWhen (integrate 0) <<< delay (1, [])  -< (1, evs')
        let evs' = if ticksSinceLast > minTicks then take 1 evs else []
    returnA -< evs'

collides :: Bullet -> Invader -> Bool
collides (Bullet{..}) (Invader{..}) = dx < 29 && dy < 39
    where
        dx = abs $ vx bPos - vx iPos
        dy = abs $ vy bPos - vy iPos

bulletC :: Bullet -> Coroutine [Invader] (Maybe Bullet)
bulletC (Bullet{..}) = proc invaders -> do
    y' <- integrate (vy bPos) -< -1

    let bullet = Bullet (Vec2 (vx bPos) y')
        doesCollide = any (collides bullet) invaders
        b
            | doesCollide || y' < 0 = Nothing
            | otherwise   = Just bullet
    returnA -< b

turretC :: Coroutine [KeyEvent] (Turret, Event Bullet)
turretC = proc keyEvents -> do
    -- Controls
    keyLeft  <- keyPressed (SpecialKey KeyLeft)  -< keyEvents
    keyRight <- keyPressed (SpecialKey KeyRight) -< keyEvents
    keyFire  <- keyPressed (Char ' ') -< keyEvents

    rec let xspeed
                | keyLeft  && x' > 30  = -1
                | keyRight && x' < 770 = 1
                | otherwise            = 0

        x' <- delay 400 -< x
        x <- integrate 400 -< xspeed

    let pos = Vec2 x y
        newBullet
            | keyFire   = [Bullet $ pos + Vec2 0 (-20)]
            | otherwise = []

    returnA <<< second (throttle 140) -< (Turret pos, newBullet)
    where
        y = 550

logic :: Coroutine [KeyEvent] ViewModel
logic = proc keyEvents -> do
    (turret, newBullet)  <- turretC       -< keyEvents
    bullets              <- collection [] -< (invaders, map bulletC newBullet)

    returnA -< ViewModel turret bullets invaders
    where
        invaders = zipWith Invader invaderPos invaderFrame
        invaderPos   = Vec2 <$> [64,128..10*64] <*> [64,128..4*64]
        invaderFrame = cycle [Walk1, Walk2, Death]


keyPressed :: Key -> Coroutine [KeyEvent] Bool
keyPressed k = filterE isInteresting >>> mapE isKeyDown >>> stepE False where
    isInteresting (KeyEvent{..}) = key == k
    isKeyDown (KeyEvent{..})     = keyState == Down
