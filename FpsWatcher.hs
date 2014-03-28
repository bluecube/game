module FpsWatcher where

import qualified Graphics.UI.SDL as SDL
import Data.Word

data FpsWatcherState = FpsWatcherState !Word32 !Integer

initFpsWatcher :: IO FpsWatcherState
initFpsWatcher = do
    time <- SDL.getTicks
    return (FpsWatcherState time 0)

stepFpsWatcher :: Int -> FpsWatcherState -> IO FpsWatcherState
stepFpsWatcher targetFps (FpsWatcherState prevTime _) = do
    time <- SDL.getTicks
    let elapsed = time - prevTime
    let targetFrameTime = (1000::Word32) `div` (fromIntegral targetFps)
    if targetFrameTime > elapsed then do
        SDL.delay (targetFrameTime - elapsed)
        newTime <- SDL.getTicks
        let realFps = 1000 `div` (newTime - prevTime)
        return (FpsWatcherState newTime (toInteger realFps))
    else do
        let realFps = 1000 `div` (time - prevTime)
        return (FpsWatcherState time (toInteger realFps))

fpsWatcherFps :: FpsWatcherState -> Integer
fpsWatcherFps (FpsWatcherState _ fps) = fps
