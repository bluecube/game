{-# OPTIONS -Wall #-}

module ParticleTree where

import Control.Parallel
import Debug.Trace

import Particles
import Util

maxUpdateThreads :: Int
maxUpdateThreads = 8

-- | Bounding volume hierarchy of particles. Strict in all operations (unless I forgot some)
data ParticleTree = InnerNode !BBoxF !Float !Float !ParticleTree !ParticleTree | LeafNode !Particle deriving Show

particleTreeBBox :: ParticleTree -> BBoxF
particleTreeBBox (InnerNode bbox _ _ _ _) = bbox
particleTreeBBox (LeafNode p) = BoundingBox (particlePosition p) (particlePosition p)

particleTreeMg :: ParticleTree -> Float
particleTreeMg (InnerNode _ mg _ _ _) = mg
particleTreeMg (LeafNode p) = particleMg p

particleTreeMaxD0 :: ParticleTree -> Float
particleTreeMaxD0 (InnerNode _ _ maxD0 _ _) = maxD0
particleTreeMaxD0 (LeafNode p) = particleD0 p

particleTreeMapM_ :: (Particle -> IO a) -> (ParticleTree -> IO b) -> ParticleTree -> IO ()
particleTreeMapM_ f _ (LeafNode p) = do
    _ <- f p
    return ()
particleTreeMapM_ f g t@(InnerNode _ _ _ t1 t2) = do
    _ <- particleTreeMapM_ f g t1
    _ <- particleTreeMapM_ f g t2
    _ <- g t
    return ()

particleTreeUpdateAutoPar :: (Particle -> (Particle, Int)) -> ParticleTree -> (ParticleTree, Int)
particleTreeUpdateAutoPar = particleTreeUpdateAutoPar' (truncate (logBase (2::Float) (fromIntegral maxUpdateThreads)))

particleTreeUpdateAutoPar' :: Int -> (Particle -> (Particle, Int)) -> ParticleTree -> (ParticleTree, Int)
particleTreeUpdateAutoPar' 0 f t = particleTreeUpdate f t
particleTreeUpdateAutoPar' level f (InnerNode _ mg maxD0 subtree1 subtree2) = let
    (newSubtree1, count1) = particleTreeUpdateAutoPar' (level - 1) f subtree1
    (newSubtree2, count2) = particleTreeUpdateAutoPar' (level - 1) f subtree2
    in
        newSubtree2 `par` (newSubtree1 `pseq` (
            (InnerNode (updateBoundingBox (particleTreeBBox newSubtree1)
                       (particleTreeBBox newSubtree2))
                       mg
                       maxD0
                       newSubtree1
                       newSubtree2), count1 + count2))
particleTreeUpdateAutoPar' _ f t = particleTreeUpdate f t

particleTreeUpdate :: (Particle -> (Particle, Int)) -> ParticleTree -> (ParticleTree, Int)
particleTreeUpdate f (LeafNode p) = let
    (newP, count) = f p
    in
    ((LeafNode newP), count)
particleTreeUpdate f (InnerNode _ mg maxD0 subtree1 subtree2) = let
    (newSubtree1, count1) = particleTreeUpdate f subtree1
    (newSubtree2, count2) = particleTreeUpdate f subtree2
    in ((InnerNode (updateBoundingBox (particleTreeBBox newSubtree1)
                   (particleTreeBBox newSubtree2))
                   mg
                   maxD0
                   newSubtree1
                   newSubtree2), count1 + count2)

particleTreeFoldl' :: (a -> Particle -> a) -> a -> ParticleTree -> a
particleTreeFoldl' f z (LeafNode p) = z `seq` (f z p)
particleTreeFoldl' f z (InnerNode _ _ _ subtree1 subtree2) = let
    folded1 = z `seq` particleTreeFoldl' f z subtree1
    folded2 = folded1 `seq` particleTreeFoldl' f folded1 subtree2
    in folded2 `seq` folded2

particleTreeToList :: ParticleTree -> [Particle]
particleTreeToList tree = particleTreeToListAccum tree []
    where
        particleTreeToListAccum :: ParticleTree -> [Particle] -> [Particle]
        particleTreeToListAccum (LeafNode p) append = (p:append)
        particleTreeToListAccum (InnerNode _ _ _ t1 t2) append = particleTreeToListAccum t1 $ particleTreeToListAccum t2 append

rebuildParticleTree :: ParticleTree -> ParticleTree
rebuildParticleTree = buildParticleTree . particleTreeToList

type CandidateBox = ([Particle], Int, BBoxF)

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

    lUpdated = ((p:lList), lCount + 1, (updateBoundingBoxVector2D lBBox pos))
    rUpdated = ((p:rList), rCount + 1, (updateBoundingBoxVector2D rBBox pos))

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
