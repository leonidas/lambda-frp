
module ViewModel
    ( ViewModel(..)
    , Turret(..)
    , Bullet(..)
    , Invader(..)
    , AnimationFrame(..)
    ) where

import Lambda.Vector

data ViewModel = ViewModel
    { turret   :: Turret
    , bullets  :: [Bullet]
    , invaders :: [Invader]
    }

data Turret = Turret
    { tPos :: Vec2
    }

data Bullet = Bullet
    { bPos :: Vec2
    }

data Invader = Invader
    { iPos   :: Vec2
    , iFrame :: AnimationFrame
    }

data AnimationFrame
    = Walk1
    | Walk2
    | Death