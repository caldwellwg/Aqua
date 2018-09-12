{-# LANGUAGE TypeOperators #-}
module Vect
    ( module Vect
    ) where

import Control.Arrow (first, second)
import Control.Monad (ap)
import Data.Complex
import Data.List (intercalate, sortOn)
import GHC.TypeLits

import QBasis

type C = Complex Double

-- |Free vector space over field
newtype Vect k b = V [(b, k)] deriving (Eq, Ord)

instance (Show k, Show b) => Show (Vect k b) where
    show (V xs) = intercalate " + " $ map (\(e, c) -> show c ++ show e) xs

instance Functor (Vect k) where
    fmap f (V xs) = V $ map (first f) xs

instance Num k => Applicative (Vect k) where
    pure = return
    (<*>) = ap

instance Num k => Monad (Vect k) where
    return a = V [(a, 1)]
    V ts >>= f = V $ concat [ [(b, y*x) | let V us = f a, (b, y) <- us] | (a, x) <- ts]

-- |Convert an element of Vect k b into normal form. Normal form consists in having the basis elements in ascending order,
-- with no duplicates, and all coefficients non-zero
nf :: (Ord b, Eq k, Num k) => Vect k b -> Vect k b
nf (V ts) = V $ nf' $ sortOn fst ts where
    nf' ((b1,x1):(b2,x2):ts) =
        case compare b1 b2 of
        LT -> if x1 == 0 then nf' ((b2,x2):ts) else (b1,x1) : nf' ((b2,x2):ts)
        EQ -> if x1+x2 == 0 then nf' ts else nf' ((b1,x1+x2):ts)
        GT -> error "nf': not pre-sorted"
    nf' [(b,x)] = if x == 0 then [] else [(b,x)]
    nf' [] = []

-- |The zero vector
zero :: Vect k b
zero = V []

-- |Vector addition
add :: (Ord b, Num k, Eq k) => Vect k b -> Vect k b -> Vect k b
add (V ts) (V us) = V $ addmerge ts us
    where addmerge axs@((a,x):xs) bxs@((b,y):ys) =
            case compare a b of
                LT -> (a,x) : addmerge xs bxs
                EQ -> if x+y == 0
                        then addmerge xs ys
                        else (a, x+y) : addmerge xs ys
                GT -> (b,y) : addmerge axs ys
          addmerge xs [] = xs
          addmerge [] ys = ys

-- |(Left) Scalar multiplication
smult :: (Eq k, Num k) => k -> Vect k b -> Vect k b
smult 0 _ = zero
smult k (V xs) = V $ map (second (k*)) xs

-- |Linear application to basis elements
linear :: (Ord b, Eq k, Num k) => (a -> Vect k b) -> Vect k a -> Vect k b
linear f v = nf $ v >>= f

-- |Tensor product of two free vector spaces
te :: (Num k, Eq k) => Vect k (QBasis m) -> Vect k (QBasis n) -> Vect k (QBasis (m+n))
te (V u) (V v) = V [(a `tb` b, x*y) | (a,x) <- u, (b,y) <- v]

-- |Tensor product of two functions between free vector spaces
tf :: (Num k, Eq k, KnownNat m) =>
            (Vect k (QBasis m) -> Vect k (QBasis m)) ->
            (Vect k (QBasis n) -> Vect k (QBasis n)) ->
            Vect k (QBasis (m+n))-> Vect k (QBasis (m+n))
tf f g (V ts) = sum [let (a,b) = bt basis in smult x (f (return a) `te` g (return b)) | (basis,x) <- ts]
        where sum = foldl add zero