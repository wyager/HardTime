{-# LANGUAGE TypeOperators #-} -- For infix types, a la :->
{-# LANGUAGE GADTs #-} -- For restricting (:->) to types
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, KindSignatures #-}

import GHC.TypeLits (Nat, natVal, KnownNat)
import Data.Proxy (Proxy(Proxy))
import Prelude hiding (length)


data Procedure io where
	Primitive :: Code (a :-> b) -> Procedure (a :-> b)
	Chain :: Procedure (a :-> b) -> Procedure (b :-> c) -> Procedure (a :-> c)


instance Show (Procedure io) where
	show (Primitive code) = "Primitive procedure with code " ++ show code
	show (Chain p1 p2) = (show p1) ++ " --> " ++ (show p2)

(-->) :: Procedure (a :-> b) -> Procedure (b :-> c) -> Procedure (a :-> c)
p1 --> p2 = Chain p1 p2

data Code io where
	Nop :: (MachineType a) => Code (a :-> a)

instance Show (Code io) where
	show Nop = "No-op"

--
data (:->) a b where
	(:->) :: (MachineType a, MachineType b) => a -> b -> (a :-> b)

data TypeRep a = TypeRep {textOf :: String}
data Name a = Name {nameOf :: String}
data Length a = Length {lengthOf :: Integer}

instance Show (Name a) where
	show (Name name) = name

instance (MachineType a, MachineType b) => Show (a :-> b) where
	show (_ :: a :-> b) = show (name :: Name a) ++ " :-> " ++ show (name :: Name b)

class MachineType a where
	typeRep :: TypeRep a
	name :: Name a


data IntRep a = IntRep String

class MachineType a => IntType a where
	intRep :: Integer -> IntRep a

data Int8 = Int8 deriving Show
instance MachineType Int8 where
	typeRep = TypeRep "uint8_t"
	name = Name "Int8"

data Int16 = Int16 deriving Show
instance MachineType Int16 where
	typeRep = TypeRep "uint16_t"
	name = Name "Int16"

data Array a (n :: Nat) = Array

class HasLength t where
	length :: Length t

instance KnownNat n => HasLength (Array a n) where
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


instance forall a b . (MachineType a, MachineType b) => MachineType (a,b) where
	typeRep = TypeRep $ concat ["(", aType, ", ", bType, ")"]
		where
		aType = textOf (typeRep :: TypeRep a)
		bType = textOf (typeRep :: TypeRep b)
	name = Name $ concat ["(", aName, ", ", bName, ")"]
		where
		aName = nameOf (name :: Name a)
		bName = nameOf (name :: Name b)

valueType :: forall a . MachineType a => a -> String
valueType _ = textOf (typeRep :: TypeRep a)

valueName :: forall a . MachineType a => a -> String
valueName _ = nameOf (name :: Name a)

delOne :: Procedure (Array Int8 5 :-> Array Int8 5)
delOne = Primitive Nop

main = do
	print (Int8 :-> (Int16, Int8))
	print $ valueName (Array :: Array Int8 5)
	print $ valueType (Array :: Array (Array Int16 3) 4)
	print (Int8 :-> (Array :: (Array Int8 5)))
	print delOne

