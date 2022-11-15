{-# Language NegativeLiterals #-}
{-# Language NoImplicitPrelude #-}

module MixComputer where

import Relude
import Control.Monad.State.Lazy as S

----------------------------------
-- MIX TYPES & MISC DEFINITIONS --
----------------------------------

type Byte = Int
byteSize = 64 -- TODO: the byte size should be configurable

data Sign = Pos | Neg
  deriving (Eq, Show)

type Word' = (Sign, Byte, Byte, Byte, Byte, Byte)
type Index = (Sign, Byte, Byte)
type Jump = (Byte, Byte)
type FieldSpec = (Int, Int)

-- Knuth refers to these with the little 'r' in front: (rA, rX, etc ...)
-- However, Haskell wants types to begin with caps.
type Ar = Word'
type Xr = Word'
type I1r = Index
type I2r = Index
type I3r = Index
type I4r = Index
type I5r = Index
type I6r = Index
type Jr = Jump
data Registers = Registers
  { rA :: Ar
  , rX :: Xr
  , rI1 :: I1r
  , rI2 :: I2r
  , rI3 :: I3r
  , rI4 :: I4r
  , rI5 :: I5r
  , rI6 :: I6r
  , rJ :: Jr
  }

type MemLoc = Int
type MemCell = Word'
type Memory = [MemCell]

memSize :: Int
memSize = 4000

type OverflowToggle = Bool
data ComparisonIndicator = E | L | G

data MixComputer = MixComputer
  { registers :: Registers
  , memory :: Memory
  , overflowToggle :: OverflowToggle
  , compIndicator :: ComparisonIndicator
  }

-- TODO input-output devices (Magnetic Tapes, Drums, etc.)

-- TODO - Check if Knuth defines intial values for these things

--------------------
-- MIX INIT STATE --
--------------------
initA :: Ar
initA = (Pos, 0, 0, 0, 0, 0)

initX :: Xr
initX = (Pos, 0, 0, 0, 0, 0)

initIndexReg :: Index
initIndexReg = (Pos, 0, 0)

initJumpReg :: Jump
initJumpReg = (0, 0)

initRegisters :: Registers
initRegisters = Registers
  { rA = initA
  , rX = initX
  , rI1 = initIndexReg
  , rI2 = initIndexReg
  , rI3 = initIndexReg
  , rI4 = initIndexReg
  , rI5 = initIndexReg
  , rI6 = initIndexReg
  , rJ = initJumpReg
  }

initMemoryCell :: MemCell
initMemoryCell = (Pos, 0, 0, 0, 0, 0)

initMemory :: Memory
initMemory = replicate memSize initMemoryCell

initOverflow :: Bool
initOverflow = False

initCompIndicator :: ComparisonIndicator
initCompIndicator = E

initComputer :: MixComputer
initComputer = MixComputer
  { registers = initRegisters
  , memory = initMemory
  , overflowToggle = initOverflow
  , compIndicator = initCompIndicator
  }

---------------------
-- Auxiliary Funcs --
---------------------

-- A field spec (L:R) is encoded in a Word' as 8L + R
fieldSpecLeftWeight = 8

decodeFieldSpec :: Byte -> FieldSpec
decodeFieldSpec byte = (left, right)
  where left = byte `div` fieldSpecLeftWeight
        right = byte `mod` fieldSpecLeftWeight
        

encodeFieldSpec :: FieldSpec -> Byte
encodeFieldSpec (left, right) = fieldSpecLeftWeight * left + right

--------------------
-- MIX Operations --
--------------------

idxReg :: Byte -> S.State MixComputer Index
idxReg i = do
  comp <- S.get
  let regs = registers comp
  case i of
    1 -> return $ rI1 regs
    2 -> return $ rI2 regs
    3 -> return $ rI3 regs
    4 -> return $ rI4 regs
    5 -> return $ rI5 regs
    6 -> return $ rI6 regs

-- TODO: Factor in index specification
address :: Word' -> MemLoc
address (sign, a1, a2, _, _, _) = s * (a1 * byteSize + a2)
  where s = if sign == Pos then 1 else -1

memContents :: MemLoc -> S.State MixComputer (Maybe MemCell)
memContents loc = do
  computer <- S.get
  return $ (memory computer) !!? loc
