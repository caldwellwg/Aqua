{-# LANGUAGE DataKinds #-}

import Control.Arrow
import Aqua

myCircuit :: QCirc 3
myCircuit = hGate ⊗ idGate
        >>> cnotGate ⊗ idGate
        >>> idGate ⊗ cnotGate


main :: IO ()
main = do
    putStrLn "Creating Bell state by cnot and hadamard gates..."
    print $ runCircuit myCircuit
