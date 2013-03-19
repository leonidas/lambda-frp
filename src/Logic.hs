
module Logic (logic) where

import Control.Arrow
import Control.Applicative

import Control.Coroutine
import Control.Coroutine.FRP

import Lambda.OpenGL (KeyEvent(..))
import Lambda.Vector
import ViewModel

logic :: Coroutine [KeyEvent] ViewModel
logic = (ballX &&& ballY) >>^ \(x, y) -> ViewModel
    { ball = Ball
        { ballPos    = Vec2 x y
        , ballRadius = 25
        }
    }
    where
        ballX = ballSpeed >>> integrate 100
        ballSpeed = pure 0.3
        ballY = gravity >>> integrate 0 >>> integrate 100
        gravity = pure 0.01