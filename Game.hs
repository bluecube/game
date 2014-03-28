#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import Particles
import ParticleTypes
import ParticleTree
import Physics
import Util
import FpsWatcher

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
        (Vector2D 0 1)
        (particleTypeByName "t1r"))
    | i <- [0..200 :: Int]] ++
    [(Particle
        (Vector2D (fromIntegral ((i * 43221) `mod` 251) + 200) (fromIntegral ((i * 537) `mod` 251) + 200))
        (Vector2D 0 (-2.5))
        (particleTypeByName "t1g"))
    | i <- [0..200 :: Int]] ++
    [(Particle
        (Vector2D (fromIntegral ((i * 43221) `mod` 251) + 200) (fromIntegral ((i * 537) `mod` 251) - 200))
        (Vector2D (-1) (1))
        (particleTypeByName "t2"))
    | i <- [0..50 :: Int]]

main :: IO ()
main = do
    SDL.init [SDL.InitEverything]
    _ <- TTF.init

    fpsFont <- TTF.openFont fontPath 70

    SDL.setCaption "Game" ""
    surface <- SDL.setVideoMode windowWidth windowHeight 24 [SDL.HWSurface, SDL.DoubleBuf]

    fpsWatcher <- initFpsWatcher
    let particleTree = buildParticleTree initialParticles

    gameLoop surface fpsFont fpsWatcher particleTree

    SDL.quit

gameLoop :: SDL.Surface -> TTF.Font -> FpsWatcherState -> ParticleTree -> IO ()
gameLoop oldSurface fpsFont oldFpsWatcher oldParticles = do
    (surface, inputState) <- processEvents oldSurface
    case inputState of
        QuitGame -> return ()
        InputState -> do
            let particles = simulationStep oldParticles

            bg <- mapColor surface background
            _ <- SDL.fillRect surface Nothing bg
            drawFps fpsFont surface (fpsWatcherFps oldFpsWatcher)
            --particleTreeMapM_ (drawParticle surface) (drawBox surface) particles
            particleTreeMapM_ (drawParticle surface) (\_ -> return()) particles
            SDL.flip surface

            fpsWatcher <- stepFpsWatcher framerate oldFpsWatcher

            gameLoop surface fpsFont fpsWatcher particles

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

projectToScreen :: SDL.Surface -> V2F -> Vector2D Int
projectToScreen surface (Vector2D x y) = let
    w = SDL.surfaceGetWidth surface
    h = SDL.surfaceGetHeight surface
    in
    Vector2D (w `div` 2 + (truncate x)) (h `div` 2 + (truncate y))

drawFps :: TTF.Font -> SDL.Surface -> Integer -> IO()
drawFps fpsFont surface fps = do
    let string = (show fps) ++ " fps"
    let w = SDL.surfaceGetWidth surface
    let color = mixColors 0.1 white background
    rendered <- TTF.renderTextShaded fpsFont string color background
    let renderedW = SDL.surfaceGetWidth rendered
    _ <- (SDL.blitSurface rendered Nothing
                          surface (Just (SDL.Rect (w - renderedW) 0 0 0)))
    return ()

drawParticle :: SDL.Surface -> Particle -> IO ()
drawParticle surface p = do
    let sourceSurface = particleSurface p
    let w = SDL.surfaceGetWidth sourceSurface
    let h = SDL.surfaceGetHeight sourceSurface
    let Vector2D x y = projectToScreen surface (particlePosition p)
    _ <- (SDL.blitSurface sourceSurface Nothing
                          surface (Just (SDL.Rect (x - w `div` 2) (y - h `div` 2) 0 0)))
    return ()

drawBox :: SDL.Surface -> ParticleTree -> IO ()
drawBox surface (InnerNode (BoundingBox v1 v2) _ _ _ _) = do
    let Vector2D x1 y1 = projectToScreen surface v1
    let Vector2D x2 y2 = projectToScreen surface v2

    c <- mapColor surface white

    _ <- SDL.fillRect surface (Just (SDL.Rect x1 y1 1 (abs (y2 - y1)))) c
    _ <- SDL.fillRect surface (Just (SDL.Rect x1 y1 (abs (x2 - x1)) 1)) c
    _ <- SDL.fillRect surface (Just (SDL.Rect x2 y1 1 (abs (y2 - y1)))) c
    _ <- SDL.fillRect surface (Just (SDL.Rect x1 y2 (abs (x2 - x1)) 1)) c
    return ()
drawBox _ _ = error "blah"

mixColors :: Float -> SDL.Color -> SDL.Color -> SDL.Color
mixColors ratio (SDL.Color r1 g1 b1) (SDL.Color r2 g2 b2) = let
    mix a b = round (ratio * (fromIntegral a) + (1 - ratio) * (fromIntegral b))
    in
    SDL.Color (mix r1 r2) (mix g1 g2) (mix b1 b2)

mapColor :: SDL.Surface -> SDL.Color -> IO SDL.Pixel
mapColor surface (SDL.Color r g b) =
    SDL.mapRGB (SDL.surfaceGetPixelFormat surface) r g b
