{-# OPTIONS -Wall #-}

module Util where

import qualified Graphics.UI.SDL as SDL

deg2rad :: Float -> Float
deg2rad = (* (pi / 180))

mixColors :: Float -> SDL.Color -> SDL.Color -> SDL.Color
mixColors ratio (SDL.Color r1 g1 b1) (SDL.Color r2 g2 b2) = let
    mix a b = round (ratio * (fromIntegral a) + (1 - ratio) * (fromIntegral b))
    in
    SDL.Color (mix r1 r2) (mix g1 g2) (mix b1 b2)

mapColor :: SDL.Surface -> SDL.Color -> IO SDL.Pixel
mapColor surface (SDL.Color r g b) =
    SDL.mapRGB (SDL.surfaceGetPixelFormat surface) r g b
