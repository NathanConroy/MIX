module MixComputer where

import Control.Monad.State.Lazy as S

----------------------------------
-- MIX TYPES & MISC DEFINITIONS --
----------------------------------

type Byte = Int
data Sign = Pos | Neg deriving (Eq, Show)

type Word' = (Sign, Byte, Byte, Byte, Byte, Byte)
type Index = (Sign, Byte, Byte)
type Jump = (Byte, Byte)

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

--------------------
-- MIX Operations --
--------------------

contents :: Int -> S.State MixComputer MemCell
contents idx = do
  computer <- S.get
  return $ (memory computer) !! idx
