module CGVector where

import Data.List (intercalate)

data Vec a = Vec [a] deriving (Eq, Ord)

instance (Show a) => Show (Vec a) where
  show (Vec []) = "{}"
  show (Vec a) = "{" ++ (intercalate ", " $ map show a) ++ "}"

-- Vector sum
(.+) :: (Num a) => Vec a -> Vec a -> Vec a
(Vec a) .+ (Vec b) = Vec $ zipWith (+) a b

-- Vector subtraction
(.-) :: (Num a) => Vec a -> Vec a -> Vec a
(Vec a) .- (Vec b) = Vec $ zipWith (-) a b

-- Vector scalar product
(.^) :: (Num a) => Vec a -> Vec a -> a
(Vec a) .^ (Vec b) = sum $ zipWith (*) a b

-- Vector * scalar
(.*) :: (Num a) => Vec a -> a -> Vec a
(Vec a) .* b = Vec $ map (*b) a

-- Access element at i, indexed from 1
(|.) :: Vec a -> Int -> a
(Vec a) |. i = a !! (i-1)

-- Get the dimension of the vector
dim :: Vec a -> Int
dim (Vec a) = length a

--The magnitude squared of the vector (useful if comparing lengths of vectors)
magSq :: (Num a) => Vec a -> a
magSq v = v.^v

--The magnitude of the vector
mag :: (Floating a) => Vec a -> a
mag v = sqrt $ magSq v

--The unit vector pointing in the same direction of the given vector
normed :: (Floating a) => Vec a -> Vec a
normed v = v.*(1/(mag v))

--Angle between two vectors
angle :: (Floating a) => Vec a -> Vec a -> a
angle v1 v2 = acos $ (v1 .^ v2) / (mag v1 * mag v2)

--Angle between two vectors given by 3 points p1->p2, p2->p3
angleP :: (Floating a) => Vec a -> Vec a -> Vec a -> a
angleP p1 p2 p3 = let v1 = p2 .- p1
                      v2 = p3 .- p2
                  in angle v1 v2

fromRadian :: Floating a => a -> a
fromRadian a = 180 * a / pi

fromDegree :: Floating a => a -> a
fromDegree a = pi * a / 180
