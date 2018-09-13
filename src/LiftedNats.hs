{-# LANGUAGE DataKinds
           , GADTs
           , KindSignatures
           , ScopedTypeVariables
           , TypeOperators #-}
module LiftedNats
    ( module LiftedNats
    ) where

import GHC.TypeLits
import Unsafe.Coerce

-- |Singleton type for type-level naturals
data SNat (n :: Nat) where
    SNat :: KnownNat n => SNat n

-- |Unary type for type-level naturals. Used for pattern matching
data UNat :: Nat -> * where
    UZero :: UNat 0
    USucc :: UNat n -> UNat (n + 1)

toUNat :: SNat n -> UNat n
toUNat p@SNat = fromI $ natVal p
    where fromI :: Integer -> UNat m
          fromI 0 = unsafeCoerce UZero
          fromI n = unsafeCoerce (USucc $ fromI (n - 1))

withSNat :: KnownNat n => (SNat n -> a) -> a
withSNat f = f SNat