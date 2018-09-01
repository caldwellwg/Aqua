{-# LANGUAGE DataKinds #-}

import Aqua

main :: IO ()
main = do
    let reg = initQReg :: QReg 2
        circ = cnotGate `compose` (hGate `pair` idGate)
    putStrLn "Creating Bell state by cnot and hadamard gates..."
    print $ apply circ reg
