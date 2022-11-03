module MixComputer where

type Byte = Int
data Sign = Pos | Neg

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

type Memory = [Word']
type OverflowToggle = Bool
data ComparisonIndicator = E | L | G

memSize :: Int
memSize = 4000

initMemoryCell :: Word'
initMemoryCell = (Pos, 0, 0, 0, 0, 0)

initialMemory :: Memory
initialMemory = replicate memSize initMemoryCell

-- TODO input-output devices (Magnetic Tapes, Drums, etc.)
