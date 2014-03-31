{-# OPTIONS -Wall #-}

module World where

import qualified Graphics.UI.SDL as SDL
import Debug.Trace

import Particles
import ParticleTree
import ParticleTree.Building
import Physics
import Util

type WorldState = (ParticleTree, Int)

initWorldState :: [Particle] -> WorldState
initWorldState particles = ((buildParticleTree particles), 0)

updateWorldState :: WorldState -> WorldState
updateWorldState (particleTree, 0) = simulationStep particleTree
updateWorldState (particleTree, baseOpenCount) = let
    (newParticleTree, count) = simulationStep particleTree
    in
    if count > baseOpenCount + (baseOpenCount `div` 2) then
        trace "Rebuilding" (simulationStep (rebuildParticleTree particleTree))
    else
        trace ("ok (" ++ (show count) ++ ", " ++ (show baseOpenCount) ++ ")") (newParticleTree, baseOpenCount)

drawWorld :: SDL.Surface -> WorldState -> IO ()
drawWorld surface (particles, _) = do
    --particleTreeMapM_ (drawParticle surface) (\_ -> return()) particles
    particleTreeMapM_ (drawParticle surface) (drawBox surface) particles

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

    c <- mapColor surface (SDL.Color 255 255 255)

    _ <- SDL.fillRect surface (Just (SDL.Rect x1 y1 1 (abs (y2 - y1)))) c
    _ <- SDL.fillRect surface (Just (SDL.Rect x1 y1 (abs (x2 - x1)) 1)) c
    _ <- SDL.fillRect surface (Just (SDL.Rect x2 y1 1 (abs (y2 - y1)))) c
    _ <- SDL.fillRect surface (Just (SDL.Rect x1 y2 (abs (x2 - x1)) 1)) c
    return ()
drawBox _ _ = error "blah"

projectToScreen :: SDL.Surface -> V2F -> Vector2D Int
projectToScreen surface (Vector2D x y) = let
    w = SDL.surfaceGetWidth surface
    h = SDL.surfaceGetHeight surface
    in
    Vector2D (w `div` 2 + (truncate x)) (h `div` 2 + (truncate y))

