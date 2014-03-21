{-# OPTIONS -Wall #-}

module Util where

data Vector2D a = Vector2D !a !a deriving (Eq, Show)

(|+|) :: (Num a) => Vector2D a -> Vector2D a -> Vector2D a
(Vector2D x1 y1) |+| (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
(|-|) :: (Num a) => Vector2D a -> Vector2D a -> Vector2D a
(Vector2D x1 y1) |-| (Vector2D x2 y2) = Vector2D (x1 - x2) (y1 - y2)
(|*|) :: (Num a) => Vector2D a -> a -> Vector2D a
(Vector2D x1 y1) |*| k = Vector2D (x1 * k) (y1 * k)
(|/|) :: (Fractional a) => Vector2D a -> a -> Vector2D a
(Vector2D x1 y1) |/| k = Vector2D (x1 / k) (y1 / k)
dot :: (Num a) => Vector2D a -> Vector2D a -> a
Vector2D x1 y1 `dot` Vector2D x2 y2 = x1 * x2 + y1 * y2
vectorMax :: (Ord a) => Vector2D a -> Vector2D a -> Vector2D a
vectorMax (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (max x1 x2) (max y1 y2)
vectorMin :: (Ord a) => Vector2D a -> Vector2D a -> Vector2D a
vectorMin (Vector2D x1 y1) (Vector2D x2 y2) =  Vector2D (min x1 x2) (min y1 y2)

absSquared :: (Num a) => Vector2D a -> a
absSquared (Vector2D x y) = x * x + y * y
vectorAbs :: (Floating a) => Vector2D a -> a
vectorAbs = sqrt . absSquared

normalized :: (Floating a) => Vector2D a -> Vector2D a
normalized v = v |/| (vectorAbs v)

data BoundingBox a = BoundingBox {
    topleft :: !(Vector2D a),
    bottomright :: !(Vector2D a)
    } deriving (Eq, Show)

emptyBoundingBox :: Fractional a => BoundingBox a
emptyBoundingBox = BoundingBox (Vector2D (1/0) (1/0)) (Vector2D (-1/0) (-1/0))

updateBoundingBoxVector2D :: Ord a => BoundingBox a -> Vector2D a -> BoundingBox a
updateBoundingBoxVector2D (BoundingBox v1 v2) v = BoundingBox (vectorMin v1 v) (vectorMax v2 v)

updateBoundingBox :: Ord a => BoundingBox a -> BoundingBox a -> BoundingBox a
updateBoundingBox (BoundingBox v1 v2) (BoundingBox v3 v4) = BoundingBox (vectorMin v1 v3) (vectorMax v2 v4)

boundingBoxCenter :: Fractional a => BoundingBox a -> Vector2D a
boundingBoxCenter (BoundingBox v1 v2) = (v1 |+| v2) |/| 2

boundingBoxArea :: Num a => BoundingBox a -> a
boundingBoxArea (BoundingBox (Vector2D x1 y1) (Vector2D x2 y2)) = abs ((x2 - x1) * (y2 - y1))

boundingBoxDiameter :: Floating a => BoundingBox a -> a
boundingBoxDiameter (BoundingBox v1 v2) = vectorAbs (v2 |-| v1)

deg2rad :: Float -> Float
deg2rad = (* (pi / 180))

type V2F = Vector2D Float
type BBoxF = BoundingBox Float
