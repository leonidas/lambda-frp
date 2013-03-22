{-# LANGUAGE Arrows #-}

module Logic (logic) where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Arrow
import Control.Category

import Control.Coroutine
import Control.Coroutine.FRP

import Lambda.OpenGL (KeyEvent(..))
import Lambda.Vector
import ViewModel

logic :: Coroutine [KeyEvent] ViewModel
logic = proc _ -> do

    returnA -< ViewModel
        { turret   = Turret { tPos = Vec2 400 550 }
        , bullets  = []
        , invaders = zipWith Invader invaderPos invaderFrame
        }

    where
        invaderPos   = Vec2 <$> [64,128..12*64] <*> [64,128..4*64]
        invaderFrame = cycle [Walk1, Walk2]
