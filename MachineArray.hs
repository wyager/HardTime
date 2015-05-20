{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures, DataKinds #-}

module MachineArray (
	Array(..),
	length,
	arrayLength
) where

import GHC.TypeLits (Nat, natVal, KnownNat)
import Data.Proxy (Proxy(Proxy))
import Prelude hiding (length)

import MachineTypes (MachineType, TypeRep(..), typeRep, Name(..), name)

data Array a (n :: Nat) = Array

data Length a = Length {lengthOf :: Integer}

length :: forall t n . KnownNat n => Length (Array t n)
length = Length (natVal (Proxy :: Proxy n))

arrayLength :: forall a n . KnownNat n => Array a n -> Integer
arrayLength _ = natVal (Proxy :: Proxy n)

instance (MachineType t, KnownNat n) => MachineType (Array t n) where
	name = Name $ "Array<" ++ tName ++ ">[" ++ show len ++ "]"
		where
		tName = nameOf (name :: Name t)
		len = lengthOf (length :: Length (Array t n))
	typeRep = TypeRep $ tType ++ "[" ++ show len ++ "]"
		where
		tType = textOf (typeRep :: TypeRep t)
		len = lengthOf (length :: Length (Array t n))