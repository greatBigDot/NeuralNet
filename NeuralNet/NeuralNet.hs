import Matrix

--Euler's constant
e :: (Floating a) => a
e = exp 1

numLayers :: Int

--the number of neurons in the given layer
numNeurons :: Int -> Int

numInputs :: Int
numInputs = length initData

--maps (-infinity, +infinity) to (0,1); "normalizes" the signal travelling through the network.
activate :: (Num a) => a -> a -> a
activate a = (\x -> e**(a*x) / (e**(a*x) + 1))

--The training data. Essentially, what the neural network should do is find a way to predict the outputs of some fucntion from inputs, given some example input-output pairs. Note that currently, there can be multiple inputs, but only one input; i.e., the function is from R^n to R. Later this may be updated to from R^n to R^m.
initData :: (Num a) => [([a],a)]

--A 3D matrix containing all synaptic weights. The first parameter specifies the source layer, the second the source neuron, and the third the destination neuron.
allWeights :: (Num a) => [[[a]]]

--Given a layer, this outputs the 2D matrix containing the synaptic weights of that layer. The first parameter specifies the source neuron, and the second specifies the destination neuron. It is significantly easier to deal with these 2D matrices instead of the 3D matrix of all the weights.
weights :: Int -> [[a]]
weights n = allWeights!!n

--Given a layer, this returns the 2D matrix containing the neuron values in that layer. The first parameter specifies which set of inputs it originated from, and the second specifies which neuron in the given layer is being referenced.
neurons :: (Num a) => Int -> [[a]]
neurons 0 = map fst initData
neurons n = activate $ mult (neurons (n-1)) (weights (n-1))

--A 3D matrix containing the values of all neurons. The first paramter specifies the layer, the second specifies the set of inputs, and the third specifies the particular neuron. I.e., neurons[5,3,1] represents the 1st node in the 5th layer originating from the 3rd input.
allNeurons :: (Num a) => [[[a]]]
allNeurons = map neurons [0..numLayers-1]

--A set of synaptic weights uniquely specifies a neural network, which is a function that takes in a set of inputs and returns an output (currently only one output; returning a list of outputs is functinality I may add later). Using math, one can try to figure out what weights result in a neural network whose behavior most closely matches the training data (an set of example input/output pairs).
neuralNet :: (Num a) => ([a] -> a)
neuralNet = matToFunc (neurons !! (numLayers)) . toPair
  where toPair = (\x -> (x, 0))




loss :: (Num a) => [[[a]]] -> a
loss = sum $ map (\k -> 1/2 * ((column k targetOut) - (column k out)) ^ 2) [0..numInputs - 1]
  where targetOut = transpose $ map snd initData
        out       = neurons (numLayers - 1)

grad :: (Num a) => (a,a,a) -> a
grad (l,i,j) = sum $ map ((\k -> (column k out) - (column k targetOut)) [0..numInputs - 1]

--gradient :: ( 
