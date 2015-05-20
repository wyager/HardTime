{-# LANGUAGE ScopedTypeVariables #-}

module MachineTypes (
	MachineType, typeRep, name,
	Name(..),
	TypeRep(..),
	Int8(..), Int16(..),
	valueType, valueName
) where


class MachineType a where
	typeRep :: TypeRep a
	name :: Name a

data TypeRep a = TypeRep {textOf :: String}
data Name a = Name {nameOf :: String}

instance Show (Name a) where
	show (Name name) = name

data Int8 = Int8 deriving Show
instance MachineType Int8 where
	typeRep = TypeRep "uint8_t"
	name = Name "Int8"

data Int16 = Int16 deriving Show
instance MachineType Int16 where
	typeRep = TypeRep "uint16_t"
	name = Name "Int16"

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
