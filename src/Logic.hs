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

import Graphics.UI.GLUT

import Lambda.OpenGL (KeyEvent(..))
import Lambda.Vector
import ViewModel

import Debug.Trace

data Collision = Collision

throttle :: Int -> Coroutine (Event a) (Event a)
throttle minTicks = proc evs -> do
    rec ticksSinceLast <- restartWhen (integrate 0) -< (1, evs')
        let evs' = if ticksSinceLast > minTicks then take 1 evs else []
    returnA -< evs'

collides :: Bullet -> Invader -> Bool
collides (Bullet{..}) (Invader{..}) = dx < 19 && dy < 29
    where
        dx = abs $ vx bPos - vx iPos
        dy = abs $ vy bPos - vy iPos

bulletC :: Bullet -> Coroutine [Tagged Invader] (Maybe Bullet, TEvent Collision)
bulletC (Bullet{..}) = trace "newBullet" $ proc invaders -> do
    y' <- integrate (vy bPos) -< -1

    let bullet = Bullet (Vec2 (vx bPos) y')
        collisions
            = map (\(recvId, _) -> (recvId, Collision))
            $ filter (collides bullet . snd)
            $ filter (isNotDead . snd) invaders
        doesCollide = not . null $ collisions
        isNotDead (Invader _ Death) = False
        isNotDead _                 = True
        b
            | doesCollide || y' < 0 = Nothing
            | otherwise   = Just bullet
    returnA -< (b, collisions)

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

invaderC :: Invader -> Coroutine ((), Event Collision) (Maybe Invader)
invaderC (Invader{..}) = proc ((), evs) -> do
    frameEv <- every 240 () -< ()
    frame   <- switchWith (const $ pure Death) frameCycle -< (frameEv, evs)

    returnA -< Just $ Invader iPos frame
    where
        frameCycle = stepE iFrame <<< mapC (cycleC frames)
        frames = case iFrame of
            Walk1 -> [Walk2, Walk1]
            Walk2 -> [Walk1, Walk2]
            _     -> [Death]


logic :: Coroutine [KeyEvent] ViewModel
logic = proc keyEvents -> do
    (turret, newBullet)   <- turretC       -< keyEvents
    rec invaders              <- receivers initialInvaders -< ((), ([], collisions))
        (bullets, collisions) <- second (delay []) <<< senders [] -< (invaders, map bulletC newBullet)

    returnA -< ViewModel turret bullets (untag invaders)
    where
        initialInvaders = map invaderC $ zipWith Invader invaderPos invaderFrame
        invaderPos   = Vec2 <$> [64,128..10*64] <*> [64,128..4*64]
        invaderFrame = cycle [Walk1, Walk2, Walk1]


keyPressed :: Key -> Coroutine [KeyEvent] Bool
keyPressed k = filterE isInteresting >>> mapE isKeyDown >>> stepE False where
    isInteresting (KeyEvent{..}) = key == k
    isKeyDown (KeyEvent{..})     = keyState == Down
