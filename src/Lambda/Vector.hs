
module Lambda.Vector where

import Graphics.Rendering.OpenGL (GLfloat)

data Vec2 = Vec2 !GLfloat !GLfloat deriving (Eq, Show)

instance Num Vec2 where
    Vec2 x y + Vec2 i j = Vec2 (x+i) (y+j)
    Vec2 x y - Vec2 i j = Vec2 (x-i) (y-j)
    negate (Vec2 x y) = Vec2 (negate x) (negate y)
    fromInteger i = Vec2 f f where f = fromInteger i

    signum = error "signum not defined for Vec2"
    (*)    = error "(*) not defined for Vec2"
    abs    = error "abs not defined for Vec2"


