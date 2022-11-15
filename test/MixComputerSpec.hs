module MixComputerSpec where

import Test.Hspec
import Control.Monad.State.Lazy as S

import qualified MixComputer as MIX

spec :: Spec
spec = do
  describe "memory accessors" $ do
    it "can get an initial memory location" $ do
      S.evalState (MIX.contents 0) MIX.initComputer `shouldBe` Just MIX.initMemoryCell

  describe "instruction readers" $ do
    it "can get an unindexed address" $ do
      let w = (MIX.Pos, 1, 1, 0, 0, 0)
          expectedResult = MIX.byteSize + 1
      MIX.address w `shouldBe` expectedResult

  describe "index reg getter" $ do
    it "can get the proper index" $ do
      -- TODO: should probably distinguish all the registers
      -- during the test setup so we make sure we get the right one.
      S.evalState (MIX.idxReg 1) MIX.initComputer `shouldBe` MIX.initIndexReg

  describe "field specification helpers" $ do
    it "can encode as a number" $ do
      let fieldSpec = (3, 5)
      MIX.encodeFieldSpec fieldSpec `shouldBe` 29

    it "can decode from a number" $ do
      let encodedFieldSpec = 29
      MIX.decodeFieldSpec encodedFieldSpec `shouldBe` (3, 5)
