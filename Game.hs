#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import qualified Graphics.UI.SDL as SDL
import Data.Word

import Particles
import ParticleTypes
import ParticleTree
import Physics
import Util

data InputState = QuitGame | InputState

windowWidth :: Int
windowWidth = 800
windowHeight :: Int
windowHeight = 600
framerate :: Int
framerate = 50
background :: SDL.Pixel
background = SDL.Pixel 0x000014
white :: SDL.Pixel
white = SDL.Pixel 0xcccccc

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

    SDL.setCaption "Game" ""
    surface <- SDL.setVideoMode windowWidth windowHeight 24 [SDL.HWSurface, SDL.DoubleBuf]

    gameLoop surface (buildParticleTree initialParticles)

    SDL.quit

gameLoop :: SDL.Surface -> ParticleTree -> IO ()
gameLoop oldSurface oldParticles = do
    timeBefore <- SDL.getTicks
    (surface, inputState) <- processEvents oldSurface
    case inputState of
        QuitGame -> return ()
        InputState -> do
            let particles = simulationStep oldParticles

            _ <- SDL.fillRect surface Nothing background
            --particleTreeMapM_ (drawParticle surface) (drawBox surface) particles
            particleTreeMapM_ (drawParticle surface) (\_ -> return()) particles
            SDL.flip surface

            timeAfter <- SDL.getTicks
            let timePerFrame = (round ((1000.0::Float) / (fromIntegral framerate)))::Word32
            print (timeAfter - timeBefore)
            if timeAfter - timeBefore < timePerFrame then
                SDL.delay (timePerFrame - (timeAfter - timeBefore))
            else
                return ()
            gameLoop surface particles

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

projectToScreen :: V2F -> Vector2D Int
projectToScreen (Vector2D x y) = Vector2D (windowWidth `div` 2 + (truncate x)) (windowHeight `div` 2 + (truncate y))

drawParticle :: SDL.Surface -> Particle -> IO ()
drawParticle surface p = do
    let sourceSurface = particleSurface p
    let w = SDL.surfaceGetWidth sourceSurface
    let h = SDL.surfaceGetHeight sourceSurface
    let Vector2D x y = projectToScreen (particlePosition p)
    _ <- (SDL.blitSurface sourceSurface Nothing
                          surface (Just (SDL.Rect (x - w `div` 2) (y - h `div` 2) 0 0)))
    return ()

drawBox :: SDL.Surface -> ParticleTree -> IO ()
drawBox surface (InnerNode (BoundingBox v1 v2) _ _ _ _) = do
    let Vector2D x1 y1 = projectToScreen v1
    let Vector2D x2 y2 = projectToScreen v2

    _ <- SDL.fillRect surface (Just (SDL.Rect x1 y1 1 (abs (y2 - y1)))) white
    _ <- SDL.fillRect surface (Just (SDL.Rect x1 y1 (abs (x2 - x1)) 1)) white
    _ <- SDL.fillRect surface (Just (SDL.Rect x2 y1 1 (abs (y2 - y1)))) white
    _ <- SDL.fillRect surface (Just (SDL.Rect x1 y2 (abs (x2 - x1)) 1)) white
    return ()
drawBox _ _ = error "blah"

