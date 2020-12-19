{-# LANGUAGE TypeApplications #-}

module CardTest where

import Test.Tasty.Hspec
import Test.Validity

import Data.Card

allCards :: [Card]
allCards = [minBound..maxBound]

spec_Card :: Spec
spec_Card = do
  eqSpec @Card
  genValidSpec @Card
