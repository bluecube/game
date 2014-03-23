{-# OPTIONS -Wall #-}

module Particles where

import qualified Graphics.UI.SDL as SDL

import ParticleTypes
import Util

data Particle = Particle {
    particlePosition :: !V2F,
    particleVelocity :: !V2F,
    particleType :: !ParticleType
    } deriving Show

particleSurface :: Particle -> SDL.Surface
particleSurface = particleTypeSurface . particleType

particleMg :: Particle -> Float
particleMg = particleTypeMg . particleType

particleMa :: Particle -> Float
particleMa = particleTypeMa . particleType

particleF0 :: Particle -> Float
particleF0 = particleTypeF0 . particleType

particleD0 :: Particle -> Float
particleD0 = particleTypeD0 . particleType
