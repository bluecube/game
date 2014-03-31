#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import Particles
import ParticleTypes
import Util.Geometry
import Util
import FpsWatcher
import World

import Debug.Trace

data InputState = QuitGame | InputState

windowWidth :: Int
windowWidth = 800
windowHeight :: Int
windowHeight = 600
framerate :: Int
framerate = 50
background :: SDL.Color
background = SDL.Color 0x00 0x00 0x14
white :: SDL.Color
white = SDL.Color 0xcc 0xcc 0xcc
fontPath :: String
fontPath = "/usr/share/fonts/dejavu/DejaVuSans.ttf"

initialParticles :: [Particle]
initialParticles = [
    (Particle
        (Vector2D (fromIntegral ((i * 43221) `mod` 251) - 200) (fromIntegral ((i * 537) `mod` 251) - 200))
        (Vector2D 0.1 0.5)
        (particleTypeByName "t1r"))
    | i <- [0..200 :: Int]] ++
    [(Particle
        (Vector2D (fromIntegral ((i * 43221) `mod` 251) + 200) (fromIntegral ((i * 537) `mod` 251) + 200))
        (Vector2D 0.6 (-1.4))
        (particleTypeByName "t1g"))
    | i <- [0..200 :: Int]] ++
    [(Particle
        (Vector2D (fromIntegral ((i * 43221) `mod` 133) + 200) (fromIntegral ((i * 537) `mod` 101) - 200))
        (Vector2D (-1) (1))
        (particleTypeByName "t2"))
    | i <- [0..50 :: Int]]

main :: IO ()
main = do
    SDL.init [SDL.InitEverything]
    _ <- TTF.init

    fpsFont <- TTF.openFont fontPath 70

    surface <- SDL.setVideoMode windowWidth windowHeight 24 [SDL.HWSurface, SDL.DoubleBuf]
    SDL.setCaption "Game" ""

    fpsWatcher <- initFpsWatcher

    gameLoop surface fpsFont fpsWatcher (initWorldState initialParticles)

    SDL.quit

gameLoop :: SDL.Surface -> TTF.Font -> FpsWatcherState -> WorldState -> IO ()
gameLoop oldSurface fpsFont oldFpsWatcher oldWorldState = do
    (surface, inputState) <- processEvents oldSurface
    case inputState of
        QuitGame -> return ()
        InputState -> do
            let worldState = updateWorldState oldWorldState

            bg <- mapColor surface background
            _ <- SDL.fillRect surface Nothing bg
            drawFps fpsFont surface oldFpsWatcher
            drawWorld surface worldState

            SDL.flip surface

            fpsWatcher <- updateFpsWatcher framerate oldFpsWatcher

            gameLoop surface fpsFont fpsWatcher worldState

processEvents :: SDL.Surface -> IO (SDL.Surface, InputState)
processEvents surface = do
    event <- SDL.pollEvent
    case event of
        SDL.NoEvent -> return (surface, InputState)
        SDL.Quit -> return (surface, QuitGame)
        SDL.VideoResize w h -> do
            newSurface <- SDL.setVideoMode w h 24 [SDL.HWSurface, SDL.DoubleBuf]
            processEvents newSurface
        _ -> processEvents surface

drawFps :: TTF.Font -> SDL.Surface -> FpsWatcherState -> IO()
drawFps fpsFont surface (FpsWatcherState _ fps) = do
    let string = (show fps) ++ " fps"
    let w = SDL.surfaceGetWidth surface
    let color = mixColors 0.1 white background
    rendered <- TTF.renderTextShaded fpsFont string color background
    let renderedW = SDL.surfaceGetWidth rendered
    _ <- (SDL.blitSurface rendered Nothing
                          surface (Just (SDL.Rect (w - renderedW) 0 0 0)))
    return ()
