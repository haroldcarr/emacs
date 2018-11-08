
module Matrix where

import qualified Data.List as L

-- | Takes a matrix (a list of lists of ints) and returns its transposition.
transpose :: [[Int]] -> [[Int]]
transpose = L.transpose

-- | Returns an identity matrix of size n.
identity :: Int -> [[Int]]
identity n
  | n > 1 = L.nub $ L.permutations $ 1 : replicate (n-1) 0
  | otherwise = [[1]]

-- | Check whether a given matrix is a identity matrix.
isIdentity :: [[Int]] -> Bool
isIdentity xs = xs == identity (length xs)

-- | Compute the dyadic product of two vectors.
dyadic :: [Int] -> [Int] -> [[Int]]
dyadic xs ys = map (\x -> map (x*) ys) xs
