{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Logic (logic) where

import Prelude hiding (id, (.))

import Control.Category
import Control.Arrow

import Control.Coroutine
import Control.Coroutine.FRP

import Graphics.UI.GLUT

import Lambda.OpenGL (KeyEvent(..))
import Lambda.Vector
import ViewModel

logic :: Coroutine [KeyEvent] ViewModel
logic = proc keyEvents -> do

    keyLeft  <- keyPressed (SpecialKey KeyLeft)  -< keyEvents
    keyRight <- keyPressed (SpecialKey KeyRight) -< keyEvents

    let xspeed
            | keyLeft   = -1
            | keyRight  =  1
            | otherwise = 0

    rec switchEvent <- watch (> 500) <<< delay 1 -< ypos
        yspeed <- switchCurrent ySwitch 0 -< (gravity, switchEvent)
        ypos   <- integrate 100 -< yspeed

    xpos   <- integrate 100 -< xspeed

    returnA -< ViewModel
        { ball = Ball
            { ballPos    = Vec2 xpos ypos
            , ballRadius = 25
            }
        }

    where
        gravity = 0.01

        ySwitch currentYSpeed = integrate (-0.8 * currentYSpeed)

        keyPressed :: Key -> Coroutine [KeyEvent] Bool
        keyPressed k = scanE handleKeyEvent False where
            handleKeyEvent old (KeyEvent{..})
                | key == k  = keyState == Down
                | otherwise = old
