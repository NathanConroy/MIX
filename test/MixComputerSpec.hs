module MixComputerSpec where

import Test.Hspec
import Control.Monad.State.Lazy as S

import qualified MixComputer as MIX

spec :: Spec
spec = do
  describe "memory accessors" $ do
    it "can get an initial memory location" $ do
      S.evalState (MIX.contents 0) MIX.initComputer `shouldBe` MIX.initMemoryCell

  describe "instruction readers" $ do
    it "can get an unindexed address" $ do
      let w = (MIX.Pos, 1, 1, 0, 0, 0)
          expectedResult = MIX.byteSize * 1 + 1
      MIX.address w `shouldBe` expectedResult
