module Util.Geometry where

class Vector a b | a -> b where
    (|+|) :: Num b => a -> a -> a
    (|-|) :: Num b => a -> a -> a
    (|*|) :: Num b => a -> b -> a
    (|/|) :: Fractional b => a -> b -> a
    dot :: Num b => a -> a -> b
    vectorMax :: Ord b => a -> a -> a
    vectorMin :: Ord b => a -> a -> a

    vectorZero :: Num b => a
    vectorInf :: Fractional b => a

    absSquared :: Num b => a -> b
    vectorAbs :: Floating b => a -> b
    vectorAbs = sqrt . absSquared

    normalized :: (Floating b) => a -> a
    normalized v = v |/| (vectorAbs v)

data Vector2D a = Vector2D !a !a deriving (Eq, Show)
instance Vector (Vector2D a) a where
    (Vector2D x1 y1) |+| (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
    (Vector2D x1 y1) |-| (Vector2D x2 y2) = Vector2D (x1 - x2) (y1 - y2)
    (Vector2D x1 y1) |*| k = Vector2D (x1 * k) (y1 * k)
    (Vector2D x1 y1) |/| k = Vector2D (x1 / k) (y1 / k)
    Vector2D x1 y1 `dot` Vector2D x2 y2 = x1 * x2 + y1 * y2
    vectorMax (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (max x1 x2) (max y1 y2)
    vectorMin (Vector2D x1 y1) (Vector2D x2 y2) =  Vector2D (min x1 x2) (min y1 y2)
    vectorInf = Vector2D (1/0) (1/0)
    vectorZero = Vector2D 0 0
    absSquared (Vector2D x y) = x * x + y * y

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
type BBoxF = BoundingBox V2F
