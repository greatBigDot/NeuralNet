import Matrix

{-Preliminaries-}

type Matrix = [[Double]]               --A matrix is a list of lists. See NeuralNet/Matrix/Matrix.hs for matrix and vector functions.
type Data   = [([Double],Double)]      --An example data set is a list of pairs; the first element of each pair is the list of inputs, the second is the corresponding output.

data Net = Net [Matrix] Data Double    --A Neural Network in this code is uniquely defined by (1) a list of weight matrices; (2) a set of training data; and (3) the parameter for the sigmoid activation function. Note that more complex neural networks than those used here would require much more; this is just a bare-bones net (at least, for now).

--A 3D matrix containing all synaptic weights. The first parameter specifies the source layer, the second the source neuron, and the third the destination neuron.
wts :: Net -> [Matrix]
wts Net ws _ _ = ws
--The training data. Essentially, what the neural network should do is find a way to predict the outputs of some fucntion from inputs, given some example input-output pairs. Note that currently, there can be multiple inputs, but only one output; i.e., the function is from R^n to R. Later this may be updated to from R^n to R^m.
dat :: Net -> Data
dat Net _ dt _ = dt
--Determines the steepness of the sigmoid activation function; will usually just be 1.
act :: Net -> Double
act Net _ _  a = a

--Given a layer, this outputs the 2D matrix containing the synaptic weights of that layer. The first parameter specifies the source neuron, and the second specifies the destination neuron. It is significantly easier to deal with these 2D matrices instead of the 3D matrix of all the weights (for example, note that the lists in the 3D matrix do not all have the same dimensions).
weights :: Net -> (Int -> Matrix)
weights Net ws _ _ = (\n -> ws!!n)

--the number of layers in the neural network
numLayers :: Net -> (Int)
numLayers Net ws _ _ = length ws

--the number of example input-output pairs
numInputs :: Net -> (Int)
numInputs Net _ dt _ = length dt

--the number of neurons in the given layer
numNeurons :: Net -> (Int -> Int)
numNeurons Net ws _ _ = height . (weights ws) . fl
  where fl x = if x==0 then x else x-1

--maps (-infinity, +infinity) to (0,1); "normalizes" the signal travelling through the network
activate :: Net -> (Double -> Double)
activate Net _ _ a = (\x -> e**(a*x) / (e**(a*x) + 1))


{-Foundational Functionality-}

--Given a layer, this returns the 2D matrix containing the neuron values in that layer. The first parameter specifies which set of inputs it originated from, and the second specifies which neuron in the given layer is being referenced.
neurons :: Net -> Int -> Matrix
neurons net =
  \l -> if l==0
          then map fst $ dat net
          else (map . map . activate net) . (foldr1 mult . [neurons, weights] <== net <== . subtract 1)

--A 3D matrix containing the values of all neurons. The first paramter specifies the layer, the second specifies the set of inputs, and the third specifies the particular neuron. I.e., neurons[5,3,1] represents the 1st node in the 5th layer originating from the 3rd input. This is just for completeness; as mentioned above, it's much easier to deal with (:: Int -> Matrix) than (:: [Matrix]).
allNeurons :: Net -> [Matrix]
allNeurons net = map (neurons net) [0..numLayers - 1]

--Measures how accurate the neural network is by comparing its output to the training data.
loss :: Net -> Double
loss net = sum $ map (\k -> 1/2 * (diff (column k) [targetOut, actualOut]) ^ 2) [0..numInputs - 1]
  where targetOut = transpose $ map snd $ dat net
        actualOut = neurons net $ numLayers - 1
        diff f xs = foldl (-) (map f xs)

--grad :: (Num a) => (a,a,a) -> a
--grad (l,i,j) = sum $ map ((\k -> (column k out) - (column k targetOut)) [0..numInputs - 1]

--gradient :: ( 


--Euler's constant
e :: Double
e = exp 1

--kinda like a backwards map; instead of distributing a function over a list of inputs, it distributes an input over a list of functions (i.e., abs. {100-01})
(<==) :: [a -> b] -> a -> [b]
fs <== x = map ($x) fs
