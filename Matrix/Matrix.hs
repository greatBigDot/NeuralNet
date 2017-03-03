module Matrix where

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

--zipWith, except the lists can't be of different lengths.
dotZip :: (a -> b -> c) -> [a] -> [b] -> [c]
dotZip _ [] []         = []
dotZip _ [] _          = error "Matrix.dotZip: Lists are of different lengths."
dotZip _ _ []          = error "Matrix.dotZip: Lists are of different lengths."
dotZip f (x:xs) (y:ys) = (f x y):(dotZip f xs ys)

--concat
{-
transpose :: [[a]] -> [[a]]
transpose [[]]   = [[]]
transpose [v]    =
transpose (v:vs) = 
-}

isMatrix :: (Num a) => [[a]] -> Bool
isMatrix mat = (all (\v -> length v == len) mat) && (not . null $ mat)
  where len = if not . null $ mat then length . head $ mat else 0

add :: (Num a) => [a] -> [a] -> [a]
add v1 v2 = if length v1 == length v2
              then dotZip (+) v1 v2
              else error "Matrix.add: Vectors are of different dimensions."

sMult :: (Num a) => a -> [a] -> [a]
sMult x ys = map (*x) ys

dotProd :: (Num a) => [a] -> [a] -> a
dotProd v1 v2 = if length v1 == length v2
                  then sum (dotZip (*) v1 v2)
                  else error "Matrix.dotProd: Vectors are of different dimensions."

toMatrix :: [a] -> [[a]]
toMatrix v = [v]

toMatrix' :: [a] -> [[a]]
toMatrix' v = map (:[]) v

row :: Int -> [[a]] -> [a]
row n mat = if 0 <= n && n < length mat
              then mat!!n
              else error "Matrix.row: Index is out of range."

column :: Int -> [[a]] -> [a]
column n mat = if ((0 <= n) && (n < (length(head(mat)))))
                 then map (\v -> v!!n) mat
                 else error "Matrix.column: Index is out of range."

--crossProd :: (Num a) => [a] -> [a] -> [a]

mult :: (Num a) => [[a]] -> [[a]] -> [[a]]
mult m1 m2 = if (length . head $ m1) == length m2
               then funcToMat (\(x,y) -> dotProd (row x m1) (column y m2)) (length m1,length $ head m2)
               else error "Matrix.mult: The width of matrix one is not equal to the height of matrix two."

matToFunc :: [[a]] -> (Int,Int) -> a
matToFunc m (x,y) = m !! x !! y

funcToMat :: ((Int, Int) -> a) -> (Int,Int) -> [[a]]
funcToMat f (x,y) = map (map f) (coMatrix (x,y))

--Produces a matrix that contains the value (x,y) at the coordinates (x,y). Length is x, height is y.
coMatrix :: (Int, Int) -> [[(Int,Int)]]
coMatrix (0,_) = []
coMatrix (_,0) = []
coMatrix (x,y) = (coMatrix ((x-1),y)) ++ [map (\b -> (x-1,b)) [0..(y-1)]]
