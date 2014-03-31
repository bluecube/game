{-# OPTIONS -Wall #-}
module Util.Geometry where

class Vector a b | a -> b where
    vectorOneByOne :: (b -> b -> b) -> a -> a -> a
    vectorOneByConst :: (b -> b -> b) -> a -> b -> a
    (|+|) :: Num b => a -> a -> a
    (|+|) = vectorOneByOne (+)
    (|-|) :: Num b => a -> a -> a
    (|-|) = vectorOneByOne (-)
    (|*|) :: Num b => a -> b -> a
    (|*|) = vectorOneByConst (*)
    (|/|) :: Fractional b => a -> b -> a
    (|/|) = vectorOneByConst (/)
    dot :: Num b => a -> a -> b
    vectorMax :: Ord b => a -> a -> a
    vectorMax = vectorOneByOne max
    vectorMin :: Ord b => a -> a -> a
    vectorMin = vectorOneByOne min

    vectorZero :: Num b => a
    vectorInf :: Fractional b => a

    absSquared :: Num b => a -> b
    absSquared v = v `dot` v
    vectorAbs :: Floating b => a -> b
    vectorAbs = sqrt . absSquared

    normalized :: (Floating b) => a -> a
    normalized v = v |/| (vectorAbs v)

data Vector2D a = Vector2D !a !a deriving (Eq, Show)
instance Vector (Vector2D a) a where
    vectorOneByOne f (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (f x1 x2) (f y1 y2)
    vectorOneByConst f (Vector2D x1 y1) k = Vector2D (f x1 k) (f y1 k)
    Vector2D x1 y1 `dot` Vector2D x2 y2 = x1 * x2 + y1 * y2
    vectorInf = Vector2D (1/0) (1/0)
    vectorZero = Vector2D 0 0

data Vector4D a = Vector4D !a !a !a !a deriving (Eq, Show)
instance Vector (Vector4D a) a where
    vectorOneByOne f (Vector4D x1 y1 z1 w1) (Vector4D x2 y2 z2 w2) = Vector4D (f x1 x2) (f y1 y2) (f z1 z2) (f w1 w2)
    vectorOneByConst f (Vector4D x1 y1 z1 w1) k = Vector4D (f x1 k) (f y1 k) (f z1 k) (f w1 k)
    Vector4D x1 y1 z1 w1 `dot` Vector4D x2 y2 z2 w2 = x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2
    vectorInf = Vector4D (1/0) (1/0) (1/0) (1/0)
    vectorZero = Vector4D 0 0 0 0

data BoundingBox a = BoundingBox {
    topleft :: !a,
    bottomright :: !a
    } deriving (Eq, Show)

emptyBoundingBox :: (Vector a b, Fractional b) => BoundingBox a
emptyBoundingBox = BoundingBox vectorInf (vectorZero |-| vectorInf)

updateBoundingBoxVector :: (Vector a b, Ord b) => BoundingBox a -> a -> BoundingBox a
updateBoundingBoxVector (BoundingBox v1 v2) v = BoundingBox (vectorMin v1 v) (vectorMax v2 v)

updateBoundingBox :: (Vector a b, Ord b) => BoundingBox a -> BoundingBox a -> BoundingBox a
updateBoundingBox (BoundingBox v1 v2) (BoundingBox v3 v4) = BoundingBox (vectorMin v1 v3) (vectorMax v2 v4)

boundingBoxCenter :: (Vector a b, Fractional b) => BoundingBox a -> a
boundingBoxCenter (BoundingBox v1 v2) = (v1 |+| v2) |/| 2

boundingBoxDiameter :: (Vector a b, Floating b) => BoundingBox a -> b
boundingBoxDiameter (BoundingBox v1 v2) = vectorAbs (v2 |-| v1)

type V2F = Vector2D Float
type V4F = Vector4D Float
type BBoxF = BoundingBox V2F
