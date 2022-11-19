module MixComputerSpec where

import Test.Hspec
import Control.Monad.State.Lazy as S

import qualified MixComputer as MIX

testWord :: MIX.Word'
testWord = (MIX.Neg, 3, 21, 9, 0, 22)

testIndex :: MIX.Index
testIndex = (MIX.Pos, 2, 19)

-- Exec a stateful computation on a blank computer.
execInitComp :: State MIX.MixComputer a -> MIX.MixComputer
execInitComp = flip S.execState MIX.initComputer

spec :: Spec
spec = do
  describe "memory accessors" $ do
    it "can get an initial memory location" $ do
      S.evalState (MIX.memContents 0) MIX.initComputer `shouldBe` Just MIX.initMemoryCell

  describe "instruction readers" $ do
    it "can calculate an unindexed address" $ do
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

  describe "test helpers" $ do
    it "can update rA" $ do
      let comp = execInitComp (MIX.updateA testWord)
      MIX.rA (MIX.registers comp) `shouldBe` testWord

    it "can update rX" $ do
      let comp = execInitComp (MIX.updateX testWord)
      MIX.rX (MIX.registers comp) `shouldBe` testWord

    it "can update rI1" $ do
      let comp = execInitComp (MIX.updateI1 testIndex)
      MIX.rI1 (MIX.registers comp) `shouldBe` testIndex
