{-# LANGUAGE DataKinds #-}

import Aqua

myCircuit :: Aqua 3
myCircuit = do
    apply $ hGate ⊗ idGate
    apply $ cnotGate ⊗ idGate
    apply $ idGate ⊗ cnotGate


main :: IO ()
main = do
    let initialReg = initQReg :: QReg 3
    putStrLn "Creating Bell state by cnot and hadamard gates..."
    print $ runCircuit myCircuit initialReg
