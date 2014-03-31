module ParticleTree.Building (buildParticleTree, rebuildParticleTree) where

import ParticleTree
import Particles
import Util

type CandidateBox = ([Particle], Int, BBoxF)

rebuildParticleTree :: ParticleTree -> ParticleTree
rebuildParticleTree = buildParticleTree . particleTreeToList

buildParticleTree :: [Particle] -> ParticleTree
buildParticleTree [] = error "buildParticleTree empty argument"
buildParticleTree [p] = LeafNode p
buildParticleTree ps = let
    (ps1, ps2, bbox, mg, maxD0) = split ps
    in InnerNode bbox mg maxD0 (buildParticleTree ps1) (buildParticleTree ps2)

-- | Greedy splitting algorithm for lists of Particles
-- | Attempts to put each element into one of the candidate boxes,
-- | comparing weight functions to find out which one to use.
split :: [Particle] -> ([Particle], [Particle], BBoxF, Float, Float)
split particles = let
    pos1 = particlePosition (head particles)
    pos2 = findFurthest pos1 particles pos1 0
    pos3 = findFurthest pos2 particles pos2 0
    in
    split' particles ([], 0, (BoundingBox pos2 pos2)) ([], 0, (BoundingBox pos3 pos3)) 0 0
split' :: [Particle] -> CandidateBox -> CandidateBox -> Float -> Float -> ([Particle], [Particle], BBoxF, Float, Float)
split' [] l r mg maxD0 = let
    (lList, _, lBBox) = l
    (rList, _, rBBox) = r
    bbox = updateBoundingBox lBBox rBBox
    in (lList, rList, bbox , mg, maxD0)
split' (p:ps) l r mg maxD0 = let
    (lList, lCount, lBBox) = l
    (rList, rCount, rBBox) = r

    pos = particlePosition p

    lUpdated = ((p:lList), lCount + 1, (updateBoundingBoxVector lBBox pos))
    rUpdated = ((p:rList), rCount + 1, (updateBoundingBoxVector rBBox pos))

    lCost = splitCost lUpdated r
    rCost = splitCost l rUpdated

    newMg = mg + (particleMg p)
    newMaxD0 = max maxD0 (particleD0 p)
    in
    if lCost < rCost then
        split' ps lUpdated r newMg newMaxD0
    else
        split' ps l rUpdated newMg newMaxD0

splitCost :: CandidateBox -> CandidateBox -> Float
splitCost l r = let
    (_, lCount, lBBox) = l
    (_, rCount, rBBox) = r
    in (fromIntegral (abs (lCount - rCount))) + (boundingBoxDiameter lBBox) + (boundingBoxDiameter rBBox)

findFurthest :: V2F -> [Particle] -> V2F -> Float -> V2F
findFurthest _ [] pos _ = pos
findFurthest pos1 (p:ps) best bestDistance = let
    pos2 = particlePosition p
    distance = absSquared (pos1 |-| pos2)
    in
    if distance > bestDistance then
        findFurthest pos1 ps pos2 distance
    else
        findFurthest pos1 ps best bestDistance
