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

concat

transpose :: [[a]] -> [[a]]
transpose [[]]   = [[]]
transpose [v]    =
transpose (v:vs) = 

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

row :: (Intgeral b) => [[a]] -> b -> [a]
row = (\m -> (m!!))

column :: (Integral b) => [[a]] -> b -> [a]
column 

--crossProd :: (Num a) => [a] -> [a] -> [a]

mult :: (Num a) -> [[a]] -> [[a]] -> [[a]]
mult m1 m2 = 

