{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Card
  ( -- Types
    Card(..)
  , Suit(..)
  , Rank(..)
  , fullDeck

    -- Functions
  , cardValue
  , cardName
  , suitName
  , rankName
  ) where

import Data.Bifunctor (bimap)
import Data.String (IsString, fromString)
import Data.Validity
import Data.GenValidity
import GHC.Generics (Generic)

data Card = Card
  { cardSuit :: Suit
  , cardRank :: Rank
  } deriving (Show, Eq, Ord, Bounded, Generic)

instance Enum Card where
  fromEnum Card{..} =
    fromEnum cardRank + rankBase * fromEnum cardSuit

  toEnum n =
    let (cardSuit, cardRank) = bimap toEnum toEnum (n `divMod` rankBase)
    in Card{..}

-- | The 'Suit' of a card is the category into which they are divided.
data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

-- | The 'Rank' of a card is the number or face on the card.
data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance Validity Card
instance Validity Suit
instance Validity Rank

instance GenUnchecked Card
instance GenUnchecked Suit
instance GenUnchecked Rank

instance GenValid Card
instance GenValid Suit
instance GenValid Rank

cardValue :: Card -> Int
cardValue = succ . fromEnum . cardRank

-- | The name of a 'Card'. This is a pretty version of 'show'.
--
-- This overloads as your favourite string type.
cardName :: (IsString s, Semigroup s) => Card -> s
cardName Card{..} =
  suitName cardSuit <> " of " <> rankName cardRank

-- | The name of a 'Suit'.
--
-- This overloads as your favourite string type.
suitName :: IsString s => Suit -> s
suitName = fromString . show

-- | The name of a 'Rank'.
--
-- This overloads as your favourite string type.
rankName :: IsString s => Rank -> s
rankName = fromString . show

-- | The number of 'Rank's ('Two', 'Three', etc.)
rankBase :: Int
rankBase = fromEnum (maxBound :: Rank) + 1

-- | Provides a list of 'Card's in New Deck Order:
--
-- https://ambitiouswithcards.com/new-deck-order/
fullDeck :: [Card]
fullDeck = concat
  [ map (Card Spades) aceToKing
  , map (Card Diamonds) aceToKing
  , map (Card Clubs) kingToAce
  , map (Card Hearts) kingToAce
  ]

aceToKing :: [Rank]
aceToKing =
  [ Ace
  , Two
  , Three
  , Four
  , Five
  , Six
  , Seven
  , Eight
  , Nine
  , Ten
  , Jack
  , Queen
  , King
  ]

kingToAce :: [Rank]
kingToAce = reverse aceToKing
