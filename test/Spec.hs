{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec

import qualified MixComputerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MixComputer" MixComputerSpec.spec
