module NeuralNet.NeuralNet where

import Matrix.Matrix hiding (Matrix)

{-Preliminaries-}

type Matrix = [[Double]]               --A matrix is a list of lists. See NeuralNet/Matrix/Matrix.hs for matrix and vector functions.
type Activator = Double -> Double      --An activation function refers here to a unary function over the reals; typically, this should "normalize" a neural signal. Example: a sigmoid function.
data Net = Net [Matrix] Activator      --A Neural Network in this code is uniquely defined by (1) a list of weight matrices; and (2) An activation function. Note that more complex neural networks than those used here would require more; this is just a bare-bones net (at least, for now).
type Data = (Matrix, Matrix)           --An example data set is a set of inputs paired with corresponding outputs. This will be used to refine a randomly generated net into a net that actually does what we want it to do. Hopefully.


--A 3D matrix containing all synaptic weights. The first parameter specifies the source layer, the second the source neuron, and the third the destination neuron.
wts :: Net -> [Matrix]
wts (Net ws _) = ws

--The activation function to normalize signals passing through the net. Typically maps (-inf,+inf) onto (-1,+1) or (0,1); for example, a sigmoid function or an arctangent function.
act :: Net -> Activator
act (Net _ a) = a

--Given a layer, this outputs the 2D matrix containing the synaptic weights of that layer. The first parameter specifies the source neuron, and the second specifies the destination neuron. It is significantly easier to deal with these 2D matrices instead of the 3D matrix of all the weights (for example, note that the lists in the 3D matrix do not all have the same dimensions).
weights :: Net -> Int -> Matrix
weights (Net ws _) = (\n -> ws!!n)

--The number of layers in a given neural network.
numLayers :: Net -> Int
numLayers (Net ws _) = length ws + 1

--The number of neurons in a given layer of a given neural network.
numNeurons :: Net -> (Int -> Int)
numNeurons net = height . (weights net) . fl
  where fl x = if x==0 then x else x-1

--The number of sets of inputs in the given data set.
numInputs :: Data -> Int
numInputs = length . fst

--Standard activation functions; given a real, this outputs a sigmoid function with that real as the scaling factor. Maps (-inf,+inf) onto (0,+1).
stdAct :: Double -> (Double -> Double)
stdAct a x = e**(a*x) / (e**(a*x) + 1)


--Given an example data set (though really only the inputs are needed), a neural net, and a layer, this gives the neurons in that layer that are generated from the input. Layer 0 is just the input data; layer n is the activation function applied elementwise to the matrix product of the neurons in layer (n-1) and the weights in layer (n-1).
neurons :: Data -> Net -> Int -> Matrix
neurons (input,_) _ 0     = input
neurons dat net lyr = (matrify . act $ net) ((neurons dat net (lyr-1) |*| weights net (lyr-1)))

result :: Net -> [Double] -> Double
result net input = unwrap $ neurons ([input],[]) net (numLayers net - 1)

--unsafe
unwrap :: Matrix -> Double
unwrap [[x]] = x

--A 3D matrix containing the values of all neurons. The first paramter specifies the layer, the second specifies the set of inputs, and the third specifies the particular neuron. I.e., neurons[5,3,1] represents the 1st node in the 5th layer originating from the 3rd input. This is just for completeness; as mentioned above, it's much easier to deal with (:: Int -> Matrix) than (:: [Matrix]).
allNeurons :: Data -> Net -> [Matrix]
allNeurons dat net = map (neurons dat net) [0..numLayers net - 1]


----------------------------------------------TODO: ORGANIZE-------------------------------------


{-Foundational Functionality-}

--Measures how accurate the neural network is by comparing its output to a set of training data; other loss functions are possible, but this, a supervised learning paradigm, is easiest.
trainLoss :: Data -> Net -> Double
trainLoss dat net =
  let targetOut = snd dat
      netOut = neurons dat net (numLayers net - 1)
      lftSqDf = dotZip . dotZip $ (\x y -> (x-y)^2)
  in 0.5 * sum (map (\[q] -> q) (lftSqDf targetOut netOut))


--grad :: (Num a) => (a,a,a) -> a
--grad (l,i,j) = sum $ map ((\k -> (column k out) - (column k targetOut)) [0..numInputs - 1] )

--gradient :: 


--Euler's constant
e :: Double
e = exp 1

--kinda like a backwards map; instead of distributing a function over a list of inputs, it distributes an input over a list of functions (i.e., abs. {100-01})
(<==) :: [a -> b] -> a -> [b]
fs <== x = map ($x) fs




--The derivative of the nth layer neural matrix with respect to weight (l,i,j).
netDer :: Data -> Net -> (Int, Int, Int) -> Int -> Matrix
netDer dat net (l,i,j) n
  | n <= l    = fill 0 (dim $ neurons dat net n)
  | n == l+1  = (neurons' |*| indicate (dim weights') (i,j)) |.| (act' (neurons' |*| weights'))
  | otherwise = (netDer'  |*| weights')                      |.| (act' (neurons' |*| weights'))
  where netDer'  = netDer dat net (l,i,j) (n-1)
        neurons' = neurons dat net (n-1)
        weights' = weights net (n-1)
        act'     = lift1 . (derivative stdDel) . act $ net

indicate :: (Int, Int) -> (Int, Int) -> Matrix
indicate dimen (i,j) = lift1 (\(x,y) -> if (x,y) == (i,j) then 1 else 0) (coMatrix dimen)

--The partial derivative of the loss function with respect to weight (l,i,j).
grad :: Data -> Net -> (Int, Int, Int) -> Double
grad dat net (l,i,j) = sum $ for [0..numInputs dat - 1] (\k -> (netOut |-| targetOut)!!k!!0 * (netDer dat net (l,i,j) (numLayers net - 1))!!k!!0)
  where targetOut = snd dat
        netOut    = neurons dat net (numLayers net - 1)

gradient :: Data -> Net -> [Matrix]
gradient dat net = toUnit $
  (lift1'
    (grad dat net)
    (cntMap
      (\d -> (coMatrix d))
      (map
        (dim . weights net)
        [0..numLayers net - 2]
      )
    )
  )



--to unit vector
toUnit :: [Matrix] -> [Matrix]
toUnit v = (lift1') (/ scale) v
  where scale = len . concat . concat $ v

--Descends down the gradient, given a step size.
stepDescend :: Double -> Data -> Net -> Double
stepDescend step dat net = trainLoss dat $ Net (wts net ||-|| lift1' (*step) (gradient dat net)) (act net)

--The Armijo-Goldstein condition.
armijo :: Double -> Data -> Net -> Bool
armijo step dat net = (stepDescend step dat net - trainLoss dat net) / step <= _k * (len . concat . concat $ gradient dat net)

updateStep :: Double -> Data -> Net -> Double
updateStep step dat net = if armijo step dat net then step else updateStep (step / phi) dat net

_k, phi :: Double
_k = 0.25; phi = (1+sqrt 5)/2

--Starts at a step length of 1, then shrinks it until Armijo-Goldstein is satisfied.
findStep :: Data -> Net -> Double
findStep = updateStep 1



descend :: Data -> Net -> Net
descend dat net = Net (wts net ||-|| lift1' (*step) (gradient dat net)) (act net)
  where step = findStep dat net

learn :: Data -> Net -> Int -> Net
learn dat net 0 = net
learn dat net n = descend dat (learn dat net (n-1))

lift1' :: (a -> b) -> [[[a]]] -> [[[b]]]
lift1' = map . lift1

--map, except (1) its domain is restricted to only certain kinds of functions; and (2) instead of returing a 3D matrix of pairs, it returns a 3D matrix of triples, where the new first element is a counter (hence the name).
cntMap :: ((Int,Int) -> [[(Int,Int)]]) -> [(Int,Int)] -> [[[(Int,Int,Int)]]]
cntMap f ps = cMap 0 f ps

--subroutine for countMap
cMap :: Int -> ((Int, Int) -> [[(Int,Int)]]) -> [(Int,Int)] -> [[[(Int,Int,Int)]]]
cMap n f []     = []
cMap n f (p:ps) = lift1 (n<:) (f p) : cMap (n+1) f ps

tCon :: a -> (b,c) -> (a,b,c)
tCon x (y,z) = (x,y,z)

(<:) :: a -> (b,c) -> (a,b,c)
(<:) = tCon

infix 5 <:

for :: [a] -> (a -> b) -> [b]
for = flip map

lift1 :: (Functor t) => (a -> b) -> t (t a) -> t (t b)
lift1 = fmap . fmap

matrify = lift1

--lift2 :: (Applicative t) => (a -> b -> c) -> t (t a) -> t (t b) -> t (t c)
--lift2 = (<^>) . (<^>)

--This just turns some binary function into a binary function over applicative types.
--(<^>) :: (Applicative t) => (a -> b -> c) -> (t a -> t b -> t c)
--(<^>) f = (<*>) . (f <$>)


derivative :: Double -> (Double -> Double) -> (Double -> Double)
derivative del f = \x -> (f (x + del) - f x) / del

stdDel :: Double
stdDel = 1.0e-12

(|*|) :: Matrix -> Matrix -> Matrix
(|*|) = mult

(|.|) :: Matrix -> Matrix -> Matrix
(|.|) =  dotZip . dotZip $ (*)

(|+|) :: Matrix -> Matrix -> Matrix
(|+|) = dotZip . dotZip $ (+)

(|-|) :: Matrix -> Matrix -> Matrix
(|-|) = dotZip . dotZip $ (-)

(||-||) :: [Matrix] -> [Matrix] -> [Matrix]
(||-||) = dotZip (|-|)
