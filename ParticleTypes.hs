{-# OPTIONS -Wall #-}

module ParticleTypes where

import qualified Graphics.UI.SDL as SDL
import System.IO.Unsafe

data ParticleType = ParticleType {
    particleTypeName :: String,

    -- Display properties
    particleTypeSurface :: SDL.Surface,

    -- Physics properties
    -- | Mass used for gravitation
    particleTypeMg :: Float,
    -- | Mass used when converting force to acceleration
    particleTypeMa :: Float,
    -- | Repulsive force at zero distance
    particleTypeF0 :: Float,
    -- | Distance where the gravitational pull switches to repulsion
    -- | (distance with maximal gravitational pull)
    particleTypeD0 :: Float
    } deriving Show

particleTypeByName :: String -> ParticleType
particleTypeByName name = head (filter ((== name) . particleTypeName) particleTypes)

particleTypes :: [ParticleType]
particleTypes = [
    (newParticleType "t1r" (SDL.Color 255 0 0) 2.5 2.5 0.2 10),
    (newParticleType "t1g" (SDL.Color 0 255 0) 2.5 2.5 0.2 10),
    (newParticleType "t2" (SDL.Color 0 0 255) 10 10.0 1 20)
    ]

makeSurfaceForParticleType :: SDL.Color -> Float -> IO SDL.Surface
makeSurfaceForParticleType color d0 = do
    let size = (round (d0 * 1.5)) :: Int
    surfaceRaw <- SDL.createRGBSurface [SDL.HWSurface, SDL.SrcAlpha] size size 32 0xff000000 0x00ff0000 0x0000ff00 0x000000ff
    surface <- SDL.displayFormatAlpha surfaceRaw

    mapM_ (drawParticleTypePixel surface color size) [(x, y) | x <- [0..(size - 1)], y <- [0..(size - 1)]]
    return surface

drawParticleTypePixel :: SDL.Surface -> SDL.Color -> Int -> (Int, Int) -> IO ()
drawParticleTypePixel surface (SDL.Color r g b) size (x, y) = do
    let halfSize = size `div` 2
    let xx = (fromIntegral (x - halfSize)) :: Float
    let yy = (fromIntegral (y - halfSize)) :: Float
    let distance = sqrt (xx^(2::Int) + yy^(2::Int))
    let weight = max 0 (1 - (distance / (fromIntegral halfSize)))
    let pixelFormat = SDL.surfaceGetPixelFormat surface
    pixel <- SDL.mapRGBA pixelFormat r g b (round (255 * weight * weight))
    _ <- SDL.fillRect surface (Just (SDL.Rect x y 1 1)) pixel
    return ()

newParticleType :: String -> SDL.Color -> Float -> Float -> Float -> Float -> ParticleType
newParticleType name color mg ma f0 d0 = (ParticleType
    name
    (unsafePerformIO (makeSurfaceForParticleType color d0))
    mg ma f0 d0)
