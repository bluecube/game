{-# OPTIONS -Wall #-}

module Physics (simulationStep) where

import ParticleTree
import Particles
import Util

-- | Gravitational constant
g :: Float
g = 2
-- | Maximum angular size of a box that is not expanded in degrees
maxAngle :: Float
maxAngle = 5

simulationStep :: ParticleTree -> (ParticleTree, Int)
simulationStep particles = particleTreeUpdateAutoPar (simulateParticle particles) particles

-- | Update a particle by a step of the simulation
simulateParticle :: ParticleTree -> Particle -> (Particle, Int)
simulateParticle particles p = let
    (forces, count) = force p particles (Vector2D 0 0) 0
    newVelocity = (particleVelocity p) |+| (forces |/| (particleMa p))
    newPosition = (particlePosition p) |+| newVelocity
    in
    ((p { particlePosition = newPosition, particleVelocity = newVelocity }), count)

-- | What force particle p1 gets from interaction with a given particle tree
-- | Approximates far enough subtrees with a single particle
-- | Return the force and a count of inner nodes that had to be expanded
force :: Particle -> ParticleTree -> V2F -> Int -> (V2F, Int)
force p1 (LeafNode p2) addForce addCount = addForce `seq` addCount `seq` let
    direction = (particlePosition p2) |-| (particlePosition p1)
    distanceSquared = absSquared direction
    distance = sqrt distanceSquared
    mg = (particleMg p1) * (particleMg p2)
    f0 = (particleF0 p1) + (particleF0 p2)
    d0 = (particleD0 p1) + (particleD0 p2)
    in
    if distanceSquared == 0 then
        (addForce, addCount + 1)
    else if distance < d0 then let
        attractionScalar = distance * (f0 + g * mg / (d0 * d0)) / d0 - f0
        velocityDifference = (particleVelocity p2) |-| (particleVelocity p1)
        frictionScalar = 0.005 * ((velocityDifference `dot` direction) / distance)
        in
        (addForce |+| (direction |*| ((attractionScalar + frictionScalar) / distance)), addCount + 1)
    else
        (addForce |+| (direction |*| (g * mg / (distance * distanceSquared))), addCount+ 1)
force p (InnerNode bbox treeMg maxD0 t1 t2) addForce addCount = addForce `seq` addCount `seq` let
    direction = (boundingBoxCenter bbox) |-| (particlePosition p)
    distanceSquared = absSquared direction
    boxRadiusSquared = ((boundingBoxDiameter bbox) / 2)^(2::Int)
    tanAlphaSquared = boxRadiusSquared / distanceSquared
    tanAlphaSquaredLimit = (tan (deg2rad maxAngle))^(2::Int)
    d0Squared = (maxD0 + (particleD0 p))^(2::Int)
    in
    if (distanceSquared > d0Squared) && tanAlphaSquared < tanAlphaSquaredLimit then let
        distance = sqrt distanceSquared
        mg = (particleMg p) * treeMg
        in
        (addForce |+| (direction |*| (g * mg / (distance * distanceSquared))), addCount + 1)
    else let
        (newAddForce, newAddCount) = force p t2 addForce addCount
        in
        force p t1 newAddForce newAddCount
