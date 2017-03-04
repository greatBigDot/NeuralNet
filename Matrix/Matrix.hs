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
add :: (Num a) => [a] -> [a] -> [a]
add v1 v2 = dotZip (+) v1 v2

sMult :: (Num a) => a -> [a] -> [a]
sMult x ys = map (*x) ys

dotProd :: (Num a) => [a] -> [a] -> a
dotProd v1 v2 = sum (dotZip (*) v1 v2)

toMatrix :: [a] -> [[a]]
toMatrix v = [v]

toMatrix' :: [a] -> [[a]]
toMatrix' v = map (:[]) v

row :: [[a]] -> Int -> [a]
row mat = (mat!!)

column :: [[a]] -> Int -> [a]
column mat n = map (\v -> v!!n) mat

--crossProd :: (Num a) => [a] -> [a] -> [a]

mult :: (Num a) => [[a]] -> [[a]] -> [[a]]
mult m1 m2 = funcToMat (\(x,y) -> dotProd (row m1 x) (column m2 y)) (length m1,length $ head m1)

matToFunc :: [[a]] -> (Int,Int) -> a
matToFunc m (x,y) = m !! x !! y

funcToMat :: ((Int, Int) -> a) -> (Int,Int) -> [[a]]
funcToMat f (x,y) = map (map f) (coMatrix (x,y))

--Produces a matrix that contains the value (x,y) at the coordinates (x,y). Length is x, height is y.
coMatrix :: (Int, Int) -> [[(Int,Int)]]
coMatrix (0,_) = []
coMatrix (_,0) = []
coMatrix (x,y) = (coMatrix ((x-1),y)) ++ [map (\b -> (x-1,b)) [0..(y-1)]]
