module NeuralNet.NeuralNet where

import System.Random
import Matrix.Matrix hiding (Matrix)

type Matrix = [[Double]]               --A matrix is a list of lists. See NeuralNet/Matrix/Matrix.hs for matrix and vector functions.
type Activator = Double -> Double      --An activation function refers here to a unary function over the reals; typically, this should "normalize" a neural signal. Example: a sigmoid function.
type Data = (Matrix, Matrix)           --An example data set is a set of inputs paired with corresponding outputs. This will be used to refine a randomly generated net into a net that actually does what we want it to do. Hopefully.
type Pair   = (Int, Int)               --A Pair is a pair of integers; useful when representing dimensions or coordinates as tuples.
type Triple = (Int, Int, Int)          --A Triple is a triple of integers; useful when representing dimensions or coordinates as tuples.


{- | Preliminaries | -}

--A Neural Network in this code is uniquely defined by (1) a list of weight matrices; and (2) an activation function. Note that more complex neural networks than those used here would require more; this is just a bare-bones net (at least, for now).
data Net = Net [Matrix] Activator

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

--Given a real, this outputs a sigmoid function with that real as the scaling factor. Maps (-inf,+inf) onto (0,+1).
sigmoid :: Double -> Activator
sigmoid a x = e**(a*x) / (e**(a*x) + 1)

--The "standard" activation function; maps (-inf,+inf) onto (-1,+1).
stdAct :: Activator
stdAct = \x -> 2 * sigmoid 1 x - 1


{- | Foundatinal Functionality | -}

--Given an example data set (though really only the inputs are needed), a neural net, and a layer, this gives the neurons in that layer that are generated from the input. Layer 0 is just the input data; layer n is the activation function applied elementwise to the matrix product of the neurons in layer (n-1) and the weights in layer (n-1).
neurons :: Data -> Net -> Int -> Matrix
neurons (input,_) _ 0 = input
neurons dat net lyr   = (lift1 . act $ net) (neurons dat net (lyr-1) |*| weights net (lyr-1))

--A 3D matrix containing the values of all neurons. The first paramter specifies the layer, the second specifies the set of inputs, and the third specifies the particular neuron. I.e., neurons[5,3,1] represents the 1st node in the 5th layer originating from the 3rd input. This is just for completeness; as mentioned above, it's much easier to deal with (:: Int -> Matrix) than (:: [Matrix]).
allNeurons :: Data -> Net -> [Matrix]
allNeurons dat net = map (neurons dat net) [0..numLayers net - 1]

--Runs the given input through the net.
result :: Net -> [Double] -> Double
result net input = unwrap $ neurons ([input],[]) net (numLayers net - 1)


{- | Loss Function and Derivatives | -}

--Measures how accurate the neural network is by comparing its output to a set of training data; other loss functions are possible, but this, a supervised learning paradigm, is easiest. Making the neural net learn is equivalent to minimizing the loss function.
trainLoss :: Data -> Net -> Double
trainLoss dat net =
  let targetOut = snd dat
      netOut = toMatrix' $ map (result net) (fst dat)
      lftSqDf = lift2 (\x y -> (x-y)^2)
  in 0.5 * sum (map (\[q] -> q) (lftSqDf targetOut netOut))

--The partial derivative of the nth layer neural matrix with respect to weight (l,i,j).
netDer :: Data -> Net -> (Int, Int, Int) -> Int -> Matrix
netDer dat net (l,i,j) n
  | n <= l    = fill 0 (dim $ neurons dat net n)
  | n == l+1  = (neurons' |*| indicate (dim weights') (i,j)) |.| (act' (neurons' |*| weights'))
  | otherwise = (netDer'  |*| weights')                      |.| (act' (neurons' |*| weights'))
  where netDer'  = netDer dat net (l,i,j) (n-1)
        neurons' = neurons dat net (n-1)
        weights' = weights net (n-1)
        act'     = lift1 . (derivative stdDel) . act $ net

--An indicator function; given dimensions for a matrix and a pair of coordinates, this returns a matrix with those dimensions containing all 0's, except at the given coordinates, at which is a 1. Used for calculating the partial derivative of the l-th layer weight matrix with respect to the (l,_,_)-th weight.
indicate :: (Int, Int) -> (Int, Int) -> Matrix
indicate dimen (i,j) = lift1 (\(x,y) -> if (x,y) == (i,j) then 1 else 0) (coMatrix dimen)

--The partial derivative of the loss function with respect to weight (l,i,j).
grad :: Data -> Net -> (Int, Int, Int) -> Double
grad dat net (l,i,j) = sum $ for [0..ins - 1] (\k -> mElem k 0 $ (netOut |-| targetOut) |.| (netDer' $ lyrs - 1))
  where targetOut = snd dat
        netOut    = neurons dat net (numLayers net - 1)
        netDer'   = netDer dat net (l,i,j)
        lyrs      = numLayers net
        ins       = numInputs dat

--The gradient of the loss function (i.e., the list of all partial derivatives of the loss function).
gradient :: Data -> Net -> [Matrix]
gradient dat net = xLift1 grad' (cntMap coMatrix (map dims [0..lyrs - 2]))
  where dims     = dim . weights net
        grad'    = grad dat net
        lyrs     = numLayers net


{- | Gradient Descent | -}

--Descends down the gradient, given a step size.
stepDescend :: Double -> Data -> Net -> Net
stepDescend step dat net = newWts net $ wts net ||-|| xLift1 (*step) (xToUnit $ gradient dat net)

--The Armijo-Goldstein condition: returns true if the step size is small enough to have a reasonably large slope of the descent; returns false otherwise. The higher c is in (0,1), the stricter this function is. With too large a c, the step size will be too small (despite having a large slope); with too small a c, the slope will be too small (despite having a large step size).
armijo :: Double -> Double -> Data -> Net -> Bool
armijo c step dat net =
  let secant   = (trainLoss dat (stepDescend step dat net) - trainLoss dat net) / step
      tangent' = -1 * c * (xMatLen $ gradient dat net)
  in  secant <= tangent'

--Shrinks step by a certain factor in (0,1) (currently fixed at phi-1 == 1/phi) iff the Armijo-Goldstein condition is unfulfilled.
updateStep :: Double -> Double -> Data -> Net -> Double
updateStep c step dat net =
  if armijo c step dat net
    then step
    else updateStep c (step / phi) dat net

--Starts at a step length of 1, then shrinks it until Armijo-Goldstein is satisfied.
findStep :: Data -> Net -> Double
findStep = updateStep stdC 1

--A standard value for c in the Armijo-Goldstein condition.
stdC = 0.1

--Provides a convenient way to change the weights without changing the activator.
newWts :: Net -> [Matrix] -> Net
newWts net ws = Net ws (act net)

--The 3D weight matrix descends in the direction given by the gradient with the best step size found.
descend :: Data -> Net -> Net
descend dat net = stepDescend (findStep dat net) dat net

--Descends down the gradient n times. Ideally, learn dat net (infinity) would yield the actual optimal solution.
learn :: Data -> Net -> Int -> Net
learn dat net 0 = net
learn dat net n = descend dat (learn dat net (n-1))


{- | Random Weight Initialization | -}

-- Uses an approximation of the inverse of the cumalative distribution function of a Gaussian distribution with the given mean and standard deviation to run an inverse transform sample on a random real in (0,1), thereby simulating random samples from a normally distributed random variable.
gaussian :: (Floating a, Random a) => a -> a -> a -> a
gaussian mean stdDev rand = mean + signum (rand - 0.5) * sqrt (stdDev^2 * k * log (1 - (2*rand - 1)^2))
  where k = -1.5707963

--Same as above, except infinitely many are produced.
gaussians :: (Floating a, Random a, RandomGen g) => g -> a -> a -> [a]
gaussians gen mean stdDev = map (gaussian mean stdDev) (randoms gen)

--Initializes the weights with values drawn from a gaussian distribution and with the given structure (sizes specifies the number of neurons per layer).
initialize :: RandomGen g => g -> Double -> [Int] -> [Matrix]
initialize gen stdDev sizes = zipWith divide (tail sizes) (splits (biMap (*) sizes) (gaussians gen 0 stdDev))

--Normalizes the initialization; the more weights go into a given neuron, the less impact those neurons should have.
normalizeInit :: RandomGen g => g -> Double -> [Int] -> [Matrix]
normalizeInit gen stdDev sizes = zipWith (lift1 . flip (/) . sqrt) (map fromIntegral (init sizes)) (initialize gen stdDev sizes)

--Given an activator, the standard deviation, a random number generator, and the integer array specifying the net's structure, this given an initial, randomly generated, normalized net to be trained.
initNet :: RandomGen g => Activator -> Double -> g -> [Int] -> Net
initNet activator stdDev gen sizes = Net ws activator
  where ws = normalizeInit gen stdDev sizes

--Same as initNet, except the activator is set to stdAct and the standard deviation is set to 3.
stdInitNet :: RandomGen g => g -> [Int] -> Net
stdInitNet = initNet stdAct 3



{- | Utility Functions and Miscellaneous | -}

{- Lifting -}

--Lifts a unary function to the matrix level (i.e., it just lifts it twice).
lift1 :: (Functor t) => (a -> b) -> t (t a) -> t (t b)
lift1 = fmap . fmap

--Lifts a binary function to the matrix level.
lift2 :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
lift2 = dotZip . dotZip

--Lifts a unary function to the 3D martix level (i.e., it just lifts it thrice).
xLift1 :: Functor t => (a -> b) -> t (t (t a)) -> t (t (t b))
xLift1 = fmap . lift1

--Lifts a binary function to the 3D matrix level.
xLift2 :: (a -> b -> c) -> [[[a]]] -> [[[b]]] -> [[[c]]]
xLift2 = dotZip . lift2

--Infix matrix multiplication.
(|*|) :: Matrix -> Matrix -> Matrix
(|*|) = mult

--Infix elementwise arithmetic on matrices.
(|.|), (|+|), (|-|) :: Matrix -> Matrix -> Matrix
(|.|) = lift2 (*)
(|+|) = lift2 (+)
(|-|) = lift2 (-)

--Infix elementwise subtraction on 3D matrices.
(||-||) :: [Matrix] -> [Matrix] -> [Matrix]
(||-||) = xLift2 (-)

--Lifts matrix length function to the 3D matrix level.
xMatLen :: [Matrix] -> Double
xMatLen = matLen . concat

--Lifts conversion to a unit vector to the 3D matrix level.
xToUnit :: [Matrix] -> [Matrix]
xToUnit m = xLift1 (/xMatLen m) m


{- Miscellaneous List Processing -}

--Splits into lists of the sizes in ns. Does *not* keep extra elements, so it can safely be used on infinite lists.
splits :: [Int] -> [a] -> [[a]]
splits [] xs     = []
splits (n:ns) xs = first : splits ns rest
  where (first,rest) = splitAt n xs

--Divide a list into lists of size n. *Does* keep extra elements (note that therefore the last list may not be of size n); do not use on infinite lists.
divide :: Int -> [a] -> [[a]]
divide n [] = []
divide n xs = first : divide n rest
  where (first, rest) = splitAt n xs

--Puts n copies of x in a list. (I know there's a Prelude function for this, but I don't remember what its called and I have no internet connection at the moment).
make :: Int -> a -> [a]
make 0 _ = []
make n x = x : make (n-1) x

--Combines elements at n and n+1 with binary function. Note that the output list has one fewer elements than the input list (except in the degenerate case of []).
biMap :: (a -> a -> b) -> [a] -> [b]
biMap _ []        = []
biMap _ [x]       = []
biMap f (x:x':xs) = f x x' : biMap f (x':xs)

--Takes a matrix holding 1 element and extracts it; unsafe.
unwrap :: Matrix -> Double
unwrap [[x]] = x

--Converts an arbitrary vector into a unit vector pointing in the same direction.
toUnit :: [Double] -> [Double]
toUnit v = map (/scale) v
  where scale = len v


{- Count Mapping -}

--map, except (1) its domain is restricted to only certain kinds of functions (specifically, pairs to matrices of pairs); and (2) instead of returing a 3D matrix of pairs, it returns a 3D matrix of triples, where the new first element is a counter (hence the name).
cntMap :: (Pair -> [[Pair]]) -> [Pair] -> [[[Triple]]]
cntMap = cMap 0

--subroutine for countMap
cMap :: Int -> (Pair -> [[Pair]]) -> [Pair] -> [[[Triple]]]
cMap n f []     = []
cMap n f (p:ps) = (lift1 (n<:) (f p)) : (cMap (n+1) f ps)

--cons for tuples (or at least, 2-tuples to 3-tuples).
(<:) :: a -> (b,c) -> (a,b,c)
x <: (y,z) = (x,y,z)
infix 5 <:


{- Function to Data -}

--Subroutine for funcToData.
fnToDt :: ([Double] -> [Double]) -> [[Double]] -> Data
fnToDt f domain = (domain, map f domain)

--The cartesian product of a list with itself.
cSquare :: [a] -> [[a]]
cSquare xs = (\x y -> [x,y]) <$> xs <*> xs

--Turns a binary function into a function that acts on two element lists.
listify :: (a -> a -> b) -> [a] -> [b]
listify f [x,y] = [f x y]

--Turns a binary function on doubles and a list specifying the domain into a pair of matrices. Useful when playing around in ghci, since it can turn a function into a set of training data.
funcToData :: (Double -> Double -> Double) -> [Double] -> Data
funcToData f domain = fnToDt (listify f) (cSquare domain)


{- Other -}

--Euler's constant
e :: Double
e = exp 1

--The Golden Ratio (1 + phi = phi^2; phi > 0).
phi :: Double
phi = (1 + sqrt 5)/2

--map, but flipped
for :: [a] -> (a -> b) -> [b]
for = flip map

--Approximation of the derivative; equality holds in the limit as del approaches 0. Note that this has no sense of when the derivative doesn't exist.
derivative :: Double -> (Double -> Double) -> (Double -> Double)
derivative del f = \x -> (f (x + del) - f x) / del

--A number that is 0 for all intents and purposes.
stdDel :: Double
stdDel = 1.0e-12

--The exclusive or; provides a simple test case for the net.
xor :: Double -> Double -> Double
xor x y = x + y - 2*x*y


dat :: Data
dat = funcToData xor [0,1]

loss :: Net -> Double
loss = trainLoss dat

structure :: [Int]
structure = [2,4,3,1]

results :: Net -> [Double]
results net = map (result net) (fst dat)
