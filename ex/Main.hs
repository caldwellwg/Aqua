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

halfAdder :: QCirc 3
halfAdder = toffoliGate
        >>> cnotGate ⊗ idGate

main :: IO ()
main = do
    let a = False
        b = True
    putStrLn "Adding a and b, bit 2 is sum and bit 3 is carry"
    print $ runCircuit $
        (if a then notGate else idGate)
            ⊗ (if b then notGate else idGate)
            ⊗ idGate
        >>> halfAdder
