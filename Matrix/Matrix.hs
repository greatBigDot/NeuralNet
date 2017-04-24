module Matrix.Matrix where

type Vector a = [a]
type Matrix a = [[a]]

--zipWith, except the lists can't be of different lengths.
dotZip :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
dotZip _ [] []         = []
dotZip _ [] _          = error "Matrix.dotZip: Lists are of different lengths."
dotZip _ _ []          = error "Matrix.dotZip: Lists are of different lengths."
dotZip f (x:xs) (y:ys) = (f x y):(dotZip f xs ys)

(.$.) = flip dotZip

--Matrix functions:
height :: Matrix a -> Int
height = length

width :: Matrix a -> Int
width = length . head

dim :: Matrix a -> (Int, Int)
dim mat = (height mat, width mat)

--concat

transpose :: Matrix a -> Matrix a
transpose mat = map (flip column mat) [0..(width mat)-1]

--Since all is lazy, this returns true for []. I may want to change that later, but currently coMatrix treats [] is a matrix. Hopefully this won't be a source of bugs...
isMatrix :: (Num a) => Matrix a -> Bool
isMatrix mat = (all (\v -> length v == len) mat)
  where len = length . head $ mat

add :: (Num a) => Vector a -> Vector a -> Vector a
add v1 v2 = if length v1 == length v2
              then dotZip (+) v1 v2
              else error "Matrix.add: Vectors are of different dimensions."

sMult :: (Num a) => a -> Vector a -> Vector a
sMult x ys = map (*x) ys

dotProd :: (Num a) => Vector a -> Vector a -> a
dotProd v1 v2 = if length v1 == length v2
                  then sum (dotZip (*) v1 v2)
                  else error "Matrix.dotProd: Vectors are of different dimensions."

len :: (Floating a) => Vector a -> a
len = sqrt . sum . map (^2)

toMatrix :: Vector a -> Matrix a
toMatrix v = [v]

toMatrix' :: Vector a -> Matrix a
toMatrix' v = map (:[]) v

row :: Int -> Matrix a -> Vector a
row n mat = if 0 <= n && n < length mat
              then mat!!n
              else error "Matrix.row: Index is out of range."

column :: Int -> Matrix a -> Vector a
column n mat = if 0 <= n && n < (length . head $ mat)
                 then map (\v -> v!!n) mat
                 else error "Matrix.column: Index is out of range."

--crossProd :: (Num a) => Vector a -> Vector a -> Vector a

mult :: (Num a) => Matrix a -> Matrix a -> Matrix a
mult m1 m2 = if (length . head $ m1) == length m2
               then funcToMat (\(x,y) -> dotProd (row x m1) (column y m2)) (length m1,length $ head m2)
               else error "Matrix.mult: The width of matrix one is not equal to the height of matrix two."

matToFunc :: Matrix a -> (Int,Int) -> a
matToFunc m (x,y) = m !! x !! y

funcToMat :: ((Int, Int) -> a) -> (Int,Int) -> Matrix a
funcToMat f (x,y) = map (map f) $ coMatrix (x,y)

funcTo3DMat :: ((Int, Int, Int) -> a) -> (Int, Int, Int) -> [Matrix a]
funcTo3DMat f (l,i,j) = map (map (map f)) $ co3DMatrix (l,i,j)

co3DMatrix :: (Int, Int, Int) -> [Matrix (Int,Int,Int)]
co3DMatrix (0,_,_) = []
co3DMatrix (_,0,_) = []
co3DMatrix (_,_,0) = []
co3DMatrix (l,i,j) = co3DMatrix (l-1,i,j) ++ [map (map  (\(b,c) -> (l-1,b,c))) (coMatrix (i,j))]

--map (map {:: Int -> Int -> (Int, Int, Int)}) {:: [[(Int,Int)]]}
--map {:: } {:: [[(Int,Int)]]}

--Produces a matrix that contains the value (x,y) at the coordinates (x,y). Length is x, height is y.
coMatrix :: (Int, Int) -> Matrix (Int,Int)
coMatrix (0,_) = []
coMatrix (_,0) = []
coMatrix (x,y) = coMatrix (x-1,y) ++ [map (\b -> (x-1,b)) [0..(y-1)]]

--determinant :: (Num a) => Matrix a -> a
elemMult :: (Num a) => Matrix a -> Matrix a -> Matrix a
elemMult = dotZip (dotZip (*))

distribute :: (Num a) => (a -> a) -> Matrix a -> Matrix a
distribute = map . map

fill :: a -> (Int, Int) -> Matrix a
fill _ (0,_) = []
fill _ (1,0) = [[]]
fill x (1,1) = [[x]]
fill x (1,j) = map (x:) (fill x (1,j-1))
fill x (i,j) = fill x (1,j) ++ fill x (i-1,j)

