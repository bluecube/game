module ParticleTree.Building (buildParticleTree, rebuildParticleTree) where

import ParticleTree
import Particles
import Util.Geometry

-- | Scale for velocity in the 4D build vectors.
-- | Higher value means more grouping by velocity, lower value means more
-- | grouping by position
velocityScaling :: Float
velocityScaling = 1

type BuildVector = V4F
type BuildBBox = BoundingBox V4F
type CandidateBox = ([Particle], Int, BuildBBox)

rebuildParticleTree :: ParticleTree -> ParticleTree
rebuildParticleTree = buildParticleTree . particleTreeToList

buildParticleTree :: [Particle] -> ParticleTree
buildParticleTree [] = error "buildParticleTree empty argument"
buildParticleTree [p] = LeafNode p
buildParticleTree ps = let
    (ps1, ps2, bbox, mg, maxD0) = split ps
    in InnerNode bbox mg maxD0 (buildParticleTree ps1) (buildParticleTree ps2)

particleBuildVector :: Particle -> BuildVector
particleBuildVector p = let
    (Vector2D x y) = particlePosition p
    (Vector2D z w) = (particleVelocity p) |*| velocityScaling
    in Vector4D x y z w

-- | Greedy splitting algorithm for lists of Particles
-- | Attempts to put each element into one of the candidate boxes,
-- | comparing weight functions to find out which one to use.
split :: [Particle] -> ([Particle], [Particle], BBoxF, Float, Float)
split particles = let
    pos1 = particleBuildVector (head particles)
    pos2 = findFurthest pos1 particles pos1 0
    pos3 = findFurthest pos2 particles pos2 0
    in
    split' particles ([], 0, (BoundingBox pos2 pos2)) ([], 0, (BoundingBox pos3 pos3)) emptyBoundingBox 0 0
split' :: [Particle] -> CandidateBox -> CandidateBox -> BBoxF -> Float -> Float -> ([Particle], [Particle], BBoxF, Float, Float)
split' [] l r bbox mg maxD0 = let
    (lList, _, lBBox) = l
    (rList, _, rBBox) = r
    in (lList, rList, bbox , mg, maxD0)
split' (p:ps) l r bbox mg maxD0 = let
    (lList, lCount, lBBox) = l
    (rList, rCount, rBBox) = r

    buildPos = particleBuildVector p

    lUpdated = ((p:lList), lCount + 1, (updateBoundingBoxVector lBBox buildPos))
    rUpdated = ((p:rList), rCount + 1, (updateBoundingBoxVector rBBox buildPos))

    lCost = splitCost lUpdated r
    rCost = splitCost l rUpdated

    newBBox = updateBoundingBoxVector bbox (particlePosition p)
    newMg = mg + (particleMg p)
    newMaxD0 = max maxD0 (particleD0 p)
    in
    if lCost < rCost then
        split' ps lUpdated r newBBox newMg newMaxD0
    else
        split' ps l rUpdated newBBox newMg newMaxD0

splitCost :: CandidateBox -> CandidateBox -> Float
splitCost l r = let
    (_, lCount, lBBox) = l
    (_, rCount, rBBox) = r
    in (fromIntegral (abs (lCount - rCount))) + (boundingBoxDiameter lBBox) + (boundingBoxDiameter rBBox)

findFurthest :: BuildVector -> [Particle] -> BuildVector -> Float -> BuildVector
findFurthest _ [] pos _ = pos
findFurthest pos1 (p:ps) best bestDistance = let
    pos2 = particleBuildVector p
    distance = absSquared (pos1 |-| pos2)
    in
    if distance > bestDistance then
        findFurthest pos1 ps pos2 distance
    else
        findFurthest pos1 ps best bestDistance
