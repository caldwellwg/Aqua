{-# LANGUAGE DataKinds
           , GADTs
           , TypeOperators #-}
module Aqua
    (
      module Aqua
    ) where

import Control.Arrow
import Data.Complex
import GHC.TypeLits

import QBasis
import Vect

type QReg n = Vect C (QBasis n)
type QCirc n = Vect C (QBasis n) -> Vect C (QBasis n)

runCircuit :: KnownNat n => QCirc n -> QReg n
runCircuit = ($ unit)

tensor :: QReg m -> QReg n -> QReg (m + n)
tensor vec1 vec2 = vec1 `te` vec2

(⊗) :: KnownNat m => QCirc m -> QCirc n -> QCirc (m + n)
c1 ⊗ c2 = c1 `tf` c2

unit :: KnownNat n => QReg n
unit = V [(unit', 1)]

idGate :: QCirc n
idGate = id

notGate :: QCirc 1
notGate = linear n
    where n :: QBasis 1 -> QReg 1
          n (Cons False Nil) = V [(Cons True Nil, 1)]
          n (Cons True Nil) = V [(Cons False Nil, 1)]

hGate :: QCirc 1
hGate = linear h
    where h :: QBasis 1 -> QReg 1
          h (Cons False Nil) = V [(Cons False Nil, 1 / sqrt 2)
                                 ,(Cons True Nil, 1 / sqrt 2)]
          h (Cons True Nil) = V [(Cons False Nil, 1 / sqrt 2)
                                ,(Cons True Nil, -1 / sqrt 2)]

rotGate :: Double -> QCirc 1
rotGate t = linear r
    where r :: QBasis 1 -> QReg 1
          r (Cons False Nil) = V [(Cons False Nil, 1)]
          r (Cons True Nil) = V [(Cons True Nil, cis t)]

cnotGate :: QCirc 2
cnotGate = linear n
    where n :: QBasis 2 -> QReg 2
          n (Cons False (Cons False Nil)) = V [(Cons False (Cons False Nil), 1)]
          n (Cons False (Cons True Nil)) = V [(Cons False (Cons True Nil), 1)]
          n (Cons True (Cons False Nil)) = V [(Cons True (Cons True Nil), 1)]
          n (Cons True (Cons True Nil)) = V [(Cons True (Cons False Nil), 1)]

swapGate :: QCirc 2
swapGate = linear s
    where s :: QBasis 2 -> QReg 2
          s (Cons False (Cons False Nil)) = V [(Cons False (Cons False Nil), 1)]
          s (Cons False (Cons True Nil)) = V [(Cons True (Cons False Nil), 1)]
          s (Cons True (Cons False Nil)) = V [(Cons False (Cons True Nil), 1)]
          s (Cons True (Cons True Nil)) = V [(Cons True (Cons True Nil), 1)]