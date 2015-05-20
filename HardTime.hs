{-# LANGUAGE TypeOperators #-} -- For infix types, a la :->
{-# LANGUAGE GADTs #-} -- For restricting (:->) to types
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}


import Prelude hiding (length)
import MachineTypes (MachineType, name, Name, valueName, valueType, Int8(..), Int16(..))
import MachineArray (Array(..))

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

instance (MachineType a, MachineType b) => Show (a :-> b) where
	show _ = show (name :: Name a) ++ " :-> " ++ show (name :: Name b)



delOne :: Procedure (Array Int8 5 :-> Array Int8 5)
delOne = Primitive Nop

main = do
	print (Int8 :-> (Int16, Int8))
	print $ valueName (Array :: Array Int8 5)
	print $ valueType (Array :: Array (Array Int16 3) 4)
	print (Int8 :-> (Array :: (Array Int8 5)))
	print delOne

