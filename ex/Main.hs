{-# LANGUAGE DataKinds #-}

import Aqua

myCircuit :: Aqua 2
myCircuit = do
    apply (hGate ⊗ idGate)
    apply cnotGate


main :: IO ()
main = do
    let initialReg = initQReg :: QReg 2
    putStrLn "Creating Bell state by cnot and hadamard gates..."
    print $ runCircuit myCircuit initialReg
