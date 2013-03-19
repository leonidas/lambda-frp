
module Lambda.OpenGL
    ( glInteract
    , KeyEvent(..)
    ) where

import Data.IORef
import Control.Concurrent.MVar

import System.Clock

import Graphics.UI.GLUT hiding (accum)

import Control.Coroutine


initializeWindow :: IO ()
initializeWindow = do
    initialWindowSize  $= Size 800 600
    initialDisplayMode $= [DoubleBuffered]
    _ <- getArgsAndInitialize
    createWindow "Lambda"
    windowSize $= Size 800 600

-- | Set up the initial OpenGL parameters
initializeOpenGL :: IO ()
initializeOpenGL = do
    -- Disable depth checking as we won't be needing it in 2D
    depthMask $= Disabled

    -- Nicer line drawing
    lineSmooth  $= Enabled
    blend       $= Enabled
    blendFunc   $= (SrcAlpha, OneMinusSrcAlpha)
    lineWidth   $= 2.0

    -- Set up viewport
    viewport   $= (Position 0 0, Size 800 600)

    -- Set up an orthogonal projection for 2D rendering
    matrixMode $= Projection
    loadIdentity
    ortho 0 800 600 0 (-1) 1
    matrixMode $= Modelview 0
    loadIdentity

    -- Set background color to dark bluish black
    clearColor $= Color4 0.0 0.0 0.1 1.0

data KeyEvent = KeyEvent
    { key          :: Key
    , keyState     :: KeyState
    , keyModifiers :: Modifiers
    }

nanoDelta :: TimeSpec -> TimeSpec -> Int
nanoDelta (TimeSpec s ns) (TimeSpec s' ns')
    = (s' - s) * 1000000 + ns' - ns

glInteract :: Coroutine [KeyEvent] viewmodel -> (viewmodel -> IO ()) -> IO ()
glInteract logic renderFunc = do
    -- Initialization
    initializeWindow
    initializeOpenGL

    keyEvents <- newMVar []
    prevTime  <- getTime Monotonic >>= newIORef
    timeAccum <- newIORef 0

    coroutine <- newIORef logic
    viewModel <- newIORef undefined

    keyboardMouseCallback $= Just (\k ks mods _ ->
        modifyMVar_ keyEvents $ return . (KeyEvent k ks mods :))

    let tick = do
            co  <- readIORef coroutine
            evs <- swapMVar keyEvents []
            let (vm, co') = runC co evs
            writeIORef coroutine co'
            writeIORef viewModel vm

    let tickLength = 1000000 `div` 120  -- 120 ticks per second in nanoseconds
    displayCallback $= do
        prev    <- readIORef prevTime
        current <- getTime Monotonic
        accum   <- readIORef timeAccum

        let delta  = nanoDelta prev current
            accum' = accum + delta

            exhaust act t
                | t < 0    = return t
                | otherwise = act >> exhaust act (t - tickLength)

        writeIORef prevTime current
        exhaust tick accum' >>= writeIORef timeAccum

        clear [ColorBuffer]
        readIORef viewModel >>= renderFunc
        swapBuffers

    mainLoop
