
module Logic (logic) where

import Control.Applicative
import Control.Coroutine

import Lambda.OpenGL (KeyEvent(..))
import Lambda.Vector
import ViewModel

logic :: Coroutine [KeyEvent] ViewModel
logic = pure $ ViewModel
    { ball = Ball
        { ballPos    = Vec2 400 300
        , ballRadius = 25
        }
    }