{-# LANGUAGE Arrows #-}

module Logic (logic) where

import Prelude hiding (id, (.))

import Control.Category
import Control.Arrow

import Control.Coroutine
import Control.Coroutine.FRP

import Lambda.OpenGL (KeyEvent(..))
import Lambda.Vector
import ViewModel

logic :: Coroutine [KeyEvent] ViewModel
logic = proc _ -> do

    ballX  <- integrate 100 -< xSpeed
    rec ballY  <- switchCurrent yFunc 100 -< (ySpeed, bounce)
        ySpeed <- switchCurrent ySpeedFunc 0 -< (gravity, bounce)
        bounce <- watch (> floorY) <<< delay 1 -< ballY

    returnA -< ViewModel
        { ball = Ball
            { ballPos    = Vec2 ballX ballY
            , ballRadius = 25
            }
        }

    where
        ySpeedFunc old = integrate (old * (-0.8))
        yFunc old      = integrate (newY) where
            newY
                | old > 500 = 500 - (old - 500)
                | otherwise = old

        xSpeed  = 0.3
        gravity = 0.01
        floorY  = 500
