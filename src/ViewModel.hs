
module ViewModel (ViewModel(..), Ball(..)) where

import Graphics.Rendering.OpenGL (GLfloat)
import Lambda.Vector

data ViewModel = ViewModel
    { ball :: Ball
    }

data Ball = Ball
    { ballPos    :: Vec2
    , ballRadius :: GLfloat
    }