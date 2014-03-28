{-# OPTIONS -Wall #-}

module FpsWatcher where

import qualified Graphics.UI.SDL as SDL
import Data.Word

data FpsWatcherState = FpsWatcherState !Word32 !Word32

initFpsWatcher :: IO FpsWatcherState
initFpsWatcher = do
    time <- SDL.getTicks
    return (FpsWatcherState time 0)

updateFpsWatcher :: Int -> FpsWatcherState -> IO FpsWatcherState
updateFpsWatcher targetFps (FpsWatcherState prevTime _) = do
    time <- SDL.getTicks
    let elapsed = time - prevTime
    let targetFrameTime = (1000 `div` (fromIntegral targetFps)) :: Word32
    if targetFrameTime > elapsed then do
        SDL.delay (targetFrameTime - elapsed)
        newTime <- SDL.getTicks
        let realFps = 1000 `div` (newTime - prevTime)
        return (FpsWatcherState newTime realFps)
    else do
        let realFps = 1000 `div` (time - prevTime)
        return (FpsWatcherState time realFps)

