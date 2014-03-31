{-# OPTIONS -Wall #-}

module ParticleTree where

import Control.Parallel

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

particleTreeInsert :: ParticleTree -> Particle -> ParticleTree
particleTreeInsert (LeafNode p1) p2 = let
    pos1 = particlePosition p1
    pos2 = particlePosition p2
    bbox = BoundingBox (vectorMin pos1 pos2) (vectorMax pos1 pos2)
    mg = (particleMg p1) + (particleMg p2)
    maxD0 = max (particleD0 p1) (particleD0 p2)
    in
    InnerNode bbox mg maxD0 (LeafNode p1) (LeafNode p2)
particleTreeInsert (InnerNode bbox mg maxD0 t1 t2) p = let
    pos = particlePosition p
    lBox = particleTreeBBox t1
    rBox = particleTreeBBox t2
    lSizeInc = (boundingBoxDiameter (updateBoundingBoxVector lBox pos)) - (boundingBoxDiameter lBox)
    rSizeInc = (boundingBoxDiameter (updateBoundingBoxVector rBox pos)) - (boundingBoxDiameter rBox)
    newBBox = updateBoundingBoxVector bbox pos
    newMg = mg + (particleMg p)
    newMaxD0 = max maxD0 (particleD0 p)
    in
    if lSizeInc < rSizeInc then
        InnerNode newBBox newMg newMaxD0 (particleTreeInsert t1 p) t2
    else
        InnerNode newBBox newMg newMaxD0 t1 (particleTreeInsert t2 p)

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
