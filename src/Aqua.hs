{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , KindSignatures
           , ScopedTypeVariables
           , TypeOperators
           , TypeApplications
           , NoImplicitPrelude #-}
module Aqua
    (
      QReg (..)
    , QCirc (..)
    , initQReg
    , notGate, idGate, hGate, cnotGate
    , pair, compose, apply
    ) where

import Prelude hiding ((<>))
import Data.List (intercalate)
import Data.Proxy
import GHC.TypeNats
import Numeric.LinearAlgebra 
import Text.Printf

-- |Size of register is controlled by the value of n
newtype QReg (n :: Nat) = UnsafeMkQReg { getReg :: Vector C }
    deriving (Eq)

instance KnownNat n => Show (QReg n) where
    show = intercalate " + " . f' 0 . toList . getReg
        where l = fromIntegral $ natVal $ Proxy @n
              f' m ((r :+ c):rest)  | r /= 0 && c == 0 = printf (printf "%%f|%%0%db>" l) r m : f' (m + 1) rest
                                    | r == 0 && c /= 0 = printf (printf "%%fi|%%0%db>" l) c m : f' (m + 1) rest
                                    | r == 0 && c == 0 = f' (m + 1) rest
                                    | otherwise        = printf (printf "(%%f + %%fi)|%%0%db>" l) r c m : f' (m + 1) rest
              f' _ [] = []

-- |QCirc are fundamentally morphisms between registers,
-- but are represented by unitary matrices of size nxn.
newtype QCirc (n :: Nat) = UnsafeMkQCirc { getCirc :: Matrix C }
    deriving (Eq, Show)

-- |Take 2 registers and append them together.
combine :: QReg m -> QReg n -> QReg (m + n)
combine reg1 reg2 = UnsafeMkQReg $ head . toColumns $ asColumn (getReg reg1) `kronecker` asColumn (getReg reg2)

-- |Default register state, all qbits initialized to |0>.
initQReg :: forall n. KnownNat n => QReg n
initQReg = UnsafeMkQReg $ (2^l) |> (1 : repeat 0)
    where l = natVal $ Proxy @n

-- |Not gate on 1 qubit
notGate :: QCirc 1
notGate = UnsafeMkQCirc $ (2><2) [0,1,1,0]

-- |Identity on n qubits
idGate :: forall n. KnownNat n => QCirc n
idGate = UnsafeMkQCirc $ ident (2^l)
    where l = fromIntegral $ natVal $ Proxy @n

-- |Hadamard gate on 1 qubit
hGate :: QCirc 1
hGate = UnsafeMkQCirc $ (2><2) [1,1,1,-1] / sqrt 2

-- |CNot gate on 2 qubits
cnotGate :: QCirc 2
cnotGate = UnsafeMkQCirc $ (4><4)
                    [1,0,0,0,
                     0,1,0,0,
                     0,0,0,1,
                     0,0,1,0]

-- |If `circ1` is the circuit on the first `m` qubits, and
-- `circ2` is the circuit on the last `n` qubits, then
-- `pair circ1 circ2` is the circuit on the entire combined register.
pair :: QCirc m -> QCirc n -> QCirc (m + n)
pair circ1 circ2 = UnsafeMkQCirc $ getCirc circ1 `kronecker` getCirc circ2

-- |Compose two circuits of the same size.
compose :: QCirc n -> QCirc n -> QCirc n
compose circ1 circ2 = UnsafeMkQCirc $ getCirc circ1 <> getCirc circ2

-- |Apply the circuit to the register.
apply :: QCirc n -> QReg n -> QReg n
apply circ reg = UnsafeMkQReg $ getCirc circ #> getReg reg