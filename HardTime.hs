{-# LANGUAGE TypeOperators #-} -- For infix types, a la :->
{-# LANGUAGE DataKinds #-}

import Prelude hiding (length)
import MachineTypes (MachineType, name, Name, valueName, valueType, Int8(..), Int16(..))
import MachineArray (Array(..))
import Procedure (Procedure(..))
import Function ((:->)(..))
import Code (Code(Nop))
--

delOne :: Procedure (Array Int8 5 :-> Array Int8 5)
delOne = Primitive Nop

main = do
	print (Int8 :-> (Int16, Int8))
	print $ valueName (Array :: Array Int8 5)
	let myArray = (Array :: Array (Array Int16 3) 4)
	print $ valueType myArray
	print $ valueName myArray
	print (Int8 :-> (Array :: (Array Int8 5)))
	print delOne
