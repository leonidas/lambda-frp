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

import Data.Set (Set)
import qualified Data.Set as Set

import Graphics.UI.GLUT

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

    newFrame <- mapC frames <<< every 240 () -< ()

    frame <- stepE iFrame -< newFrame


    returnA -< Just $ Invader { iPos = pos, iFrame = frame }
    where
        frames = cycleC $ case iFrame of
            Walk2 -> [Walk1, Walk2]
            Walk1 -> [Walk2, Walk1]

keysDown :: Coroutine [KeyEvent] (Set Key)
keysDown = updateE Set.empty <<< mapE insertKey where
    insertKey (KeyEvent{..}) = case keyState of
        Down -> Set.insert key
        Up   -> Set.delete key

turret_ :: Turret -> Coroutine [KeyEvent] Turret
turret_ (Turret{..}) = proc evs -> do
    keys <- keysDown -< evs
    let xvel
            | (SpecialKey KeyLeft `Set.member` keys) = -1
            | (SpecialKey KeyRight `Set.member` keys) = 1
            | otherwise    = 0
    xpos <- integrate 400 -< xvel
    returnA -< Turret { tPos = Vec2 xpos 550 }
    where
        keyToDelta (KeyEvent (SpecialKey KeyLeft) Down _) = -1
        keyToDelta (KeyEvent (SpecialKey KeyLeft) Up _) = 1
        keyToDelta (KeyEvent (SpecialKey KeyRight) Down _) = 1
        keyToDelta (KeyEvent (SpecialKey KeyRight) Up _) = -1


logic :: Coroutine [KeyEvent] ViewModel
logic = proc evs -> do

    invaders <- collection initialInvaders -< ((), [])
    turret'  <- turret_ initialTurret -< evs

    returnA -< ViewModel
        { turret   = turret'
        , bullets  = Bullet { bPos = Vec2 410 370 } : []
        , invaders = invaders
        }

    where
        initialTurret   = Turret { tPos = Vec2 400 550 }
        initialInvaders = map invader $ zipWith Invader invaderPos invaderFrame
        invaderPos   = Vec2 <$> [64,128..10*64] <*> [64,128..4*64]
        invaderFrame = cycle [Walk1, Walk2]
