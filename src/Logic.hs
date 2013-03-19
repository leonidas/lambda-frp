
module Logic (logic) where

import Control.Coroutine
import Lambda.OpenGL (KeyEvent(..))
import ViewModel

logic :: Coroutine [KeyEvent] ViewModel
logic = undefined