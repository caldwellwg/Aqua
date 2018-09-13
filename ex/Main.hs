{-# LANGUAGE DataKinds #-}

import Control.Arrow
import Aqua

toffoliGate :: QCirc 3
toffoliGate = idGate ⊗ hGate
          >>> idGate ⊗ cnotGate
          >>> swapGate ⊗ rotGate (-pi / 4)
          >>> idGate ⊗ cnotGate
          >>> swapGate ⊗ rotGate (pi / 4)
          >>> idGate ⊗ cnotGate
          >>> swapGate ⊗ rotGate (-pi / 4)
          >>> idGate ⊗ cnotGate
          >>> swapGate ⊗ idGate
          >>> idGate ⊗ rotGate (-pi / 4) ⊗ rotGate (pi / 4)
          >>> cnotGate ⊗ hGate
          >>> idGate ⊗ rotGate (-pi / 4) ⊗ (idGate :: QCirc 1)
          >>> cnotGate ⊗ idGate
          >>> rotGate (pi / 4) ⊗ rotGate (pi / 2) ⊗ idGate

myCircuit :: QCirc 3
myCircuit = hGate ⊗ idGate
        >>> cnotGate ⊗ idGate
        >>> idGate ⊗ cnotGate

main :: IO ()
main = do
    putStrLn "Creating Bell state by cnot and hadamard gates..."
    print $ runCircuit toffoliGate
