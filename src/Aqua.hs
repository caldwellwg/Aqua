{-# LANGUAGE DataKinds
           , GADTs
           , KindSignatures
           , TypeOperators #-}
module Aqua
    (
      module Aqua
    ) where

import Data.Complex
import GHC.TypeLits

import QBasis
import Vect

newtype QReg (n :: Nat) = QReg (Vect C (QBasis n)) deriving Show
newtype QCirc (n :: Nat) = QCirc (Vect C (QBasis n) -> Vect C (QBasis n))

apply :: QCirc n -> QReg n -> QReg n
apply (QCirc f) (QReg x) = QReg $ f x

compose :: QCirc n -> QCirc n -> QCirc n
compose (QCirc c1) (QCirc c2) = QCirc $ c2 . c1

tensor :: QReg m -> QReg n -> QReg (m + n)
tensor (QReg vec1) (QReg vec2) = QReg $ vec1 `te` vec2

tensorC :: KnownNat m => QCirc m -> QCirc n -> QCirc (m + n)
tensorC (QCirc c1) (QCirc c2) = QCirc $ c1 `tf` c2


unit :: QReg 1
unit = QReg $ V [(Cons False Nil, 1)]

idGate :: QCirc n
idGate = QCirc id

notGate :: QCirc 1
notGate = QCirc $ \vec -> linear n vec
    where n :: QBasis 1 -> Vect C (QBasis 1)
          n (Cons False Nil) = V [(Cons True Nil, 1)]
          n (Cons True Nil) = V [(Cons False Nil, 1)]

hGate :: QCirc 1
hGate = QCirc $ \vec -> linear h vec
    where h :: QBasis 1 -> Vect C (QBasis 1)
          h (Cons False Nil) = V [(Cons False Nil, (1 :+ 0) / sqrt 2)
                                 ,(Cons True Nil, (1 :+ 0) / sqrt 2)]
          h (Cons True Nil) = V [(Cons False Nil, (1 :+ 0) / sqrt 2)
                                ,(Cons True Nil, -(1 :+ 0) / sqrt 2)]

cnotGate :: QCirc 2
cnotGate = QCirc $ \vec -> linear n vec
    where n :: QBasis 2 -> Vect C (QBasis 2)
          n (Cons False (Cons False Nil)) = V [(Cons False (Cons False Nil), 1)]
          n (Cons False (Cons True Nil)) = V [(Cons False (Cons True Nil), 1)]
          n (Cons True (Cons False Nil)) = V [(Cons True (Cons True Nil), 1)]
          n (Cons True (Cons True Nil)) = V [(Cons True (Cons False Nil), 1)]