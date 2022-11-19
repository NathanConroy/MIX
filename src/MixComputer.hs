{-# Language NegativeLiterals  #-}
{-# Language NoImplicitPrelude #-}

module MixComputer where

import Relude
import Control.Monad.State.Lazy as S

----------------------------------
-- MIX TYPES & MISC DEFINITIONS --
----------------------------------

type Byte = Int

byteSize :: Int
byteSize = 64 -- TODO: the byte size should be configurable

data Sign = Pos | Neg
  deriving (Eq, Show)

-- The parts of a word are numbered thusly:
---------------------------------------------
-- |   0  |   1  |   2  |   3  |   4  |   5  |
---------------------------------------------
-- |  +-  | Byte | Byte | Byte | Byte | Byte |
---------------------------------------------
type Word' = (Sign, Byte, Byte, Byte, Byte, Byte)
type Index = (Sign, Byte, Byte)
type Jump = (Byte, Byte)

-- A field specification denotes the bytes of a Word'
-- an instruction should concern itself with.
-- It is denoted in the book as (L:R) and is typically encoded
-- in the 4th byte of a Word' as the sum 8L + R.
-- L and R are respectively the left and right bounds of the Word'
-- inclusive. So (3:5) denotes the 3rd, 4th, and 5th bytes.
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

-------------------------------
-- Instruction Reading Funcs --
-------------------------------

fieldSpecLeftWeight :: Int
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

-------------------------------
-- Test Helpers              --
-------------------------------
-- This functions allow us to update the computer's state
-- directly - and not through MIX instructions.

-- TODO: Use lens library here
-- TODO: Use Template Haskell for similar funcs
updateA :: Word' -> S.State MixComputer ()
updateA w = do
  comp <- S.get
  let regs = registers comp
      newRegs = regs { rA = w }
  S.put comp { registers = newRegs }

updateX :: Word' -> S.State MixComputer ()
updateX w = do
  comp <- S.get
  let regs = registers comp
      newRegs = regs { rX = w }
  S.put comp { registers = newRegs }

updateI1 :: Index -> S.State MixComputer ()
updateI1 idx = do
  comp <- S.get
  let regs = registers comp
      newRegs = regs { rI1 = idx }
  S.put comp { registers = newRegs }
