{-# LANGUAGE Arrows #-}

module Logic (logic) where

import Prelude hiding (id, (.))

import Control.Applicative (pure)
import Control.Category
import Control.Arrow

import Control.Coroutine
import Control.Coroutine.FRP

import Lambda.OpenGL (KeyEvent(..))
import Lambda.Vector
import ViewModel

logic :: Coroutine [KeyEvent] ViewModel
logic = pure $ ViewModel
        { ball = Ball
            { ballPos    = Vec2 100 100
            , ballRadius = 25
            }
        }