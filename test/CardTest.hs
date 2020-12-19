{-# LANGUAGE TypeApplications #-}

module CardTest where

import Test.Tasty.Hspec
import Test.Validity

import Data.Card
import Data.Containers.ListUtils (nubOrd)

spec_Card :: Spec
spec_Card = do
  eqSpec @Card
  ordSpec @Card
  genValidSpec @Card

spec_fullDeck :: Spec
spec_fullDeck =
  describe "fullDeck" $ do
    it "has 52 cards" $
      length fullDeck `shouldBe` 52

    it "contains only unique cards" $
      nubOrd fullDeck `shouldBe` fullDeck
