{-# LANGUAGE DataKinds
           , GADTs
           , KindSignatures
           , ScopedTypeVariables
           , TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module QBasis
    ( module QBasis
    ) where

import Data.Proxy
import GHC.TypeLits

import LiftedNats

data QBasis (n :: Nat) where
    Nil :: QBasis 0
    Cons :: Bool -> QBasis n -> QBasis (n + 1)

instance Show (QBasis n) where
    show x = "|" ++ show' x ++ ">"
show' :: QBasis n -> String
show' Nil = ""
show' (Cons True x) = "1" ++ show' x
show' (Cons False x) = "0" ++ show' x

instance Eq (QBasis n) where
    Nil == Nil = True
    (Cons _ _) == Nil = False
    Nil == (Cons _ _) = False
    (Cons a xs) == (Cons b ys) = (a == b) || (xs == ys)

instance Ord (QBasis n) where
    compare Nil Nil = EQ
    compare Nil (Cons _ _) = LT
    compare (Cons _ _) Nil = GT
    compare (Cons a xs) (Cons b ys) = if a == b
                                        then compare xs ys
                                        else compare a b

unit' :: KnownNat n => QBasis n
unit' = withSNat (unitU . toUNat)

unitU :: UNat n -> QBasis n
unitU UZero = Nil
unitU (USucc u) = Cons False $ unitU u

head' :: QBasis (n + 1) -> QBasis 1
head' (Cons b _) = Cons b Nil

tail' :: QBasis (n + 1) -> QBasis n
tail' (Cons _ x) = x

evolve' :: QBasis n -> [QBasis (n + 1)]
evolve' Nil = [Cons False Nil, Cons True Nil]
evolve' (Cons b x) = map (Cons b) $ evolve' x

tb :: QBasis m -> QBasis n -> QBasis (m + n)
tb Nil y = y
tb (Cons b x) y = Cons b $ tb x y

splitAt' :: SNat m -> QBasis (m + n) -> (QBasis m, QBasis n)
splitAt' n = splitAtU (toUNat n)

splitAtU :: UNat m -> QBasis (m + n) -> (QBasis m, QBasis n)
splitAtU UZero ys = (Nil, ys)
splitAtU (USucc s) (Cons b ys) = let (as, bs) = splitAtU s ys
                                 in (Cons b as, bs)

bt :: KnownNat m => QBasis (m + n) -> (QBasis m, QBasis n)
bt = withSNat splitAt'