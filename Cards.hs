module Cards where

-- import Data.Random.List
import Control.Monad
import Control.Monad.State
import System.Random
import System.Random.Shuffle

data Card = Card { value :: Int -- "King" > "Queen" is false... how to do this?
                 , suit :: String } deriving (Eq, Show)

type Deck = [Card]
type Hand = Deck -- more or less the same operations are applied

data Game = None | Organizing [Player] [Deck] | Game [Player] [Deck]
          | Suspended [Player] [Deck] deriving (Eq, Show)

type Player = (String, Hand, Int) -- Name, hand, points

-- instance Eq Card where
--     (Card a _) == (Card b _) = a == b

-- instance Ord Card where
--     compare (Card a _) (Card b _) = compare a b

-- instance Show Card where
--     show (Card a b) = show a ++ " of " ++ show b

-- use shuffle' to shuffle deck ; liftM (shuffle' list (length list)) $ getStdGen

-- okay maaaaaybe type Deck = ( Card, [Card] ) where fst isn't exposed unless game logic calls for it
-- drawing a card will look like (A, [...]) -> (B, A:[...]) because hands are decks?
-- firm notion of 'next' card but popping arbitrary indices is messy
-- and this isn't really useful in hands afaik
--
-- Consider type Deck = (Card, [Card]); game logic can choose to hide fst
-- push :: Card -> (A, [...]) -> (Card, A:[...])
-- pop :: Int -> (A, [...]) ->

pop :: Int -> State Deck Card -- pop arbitrary index (eg to play a card)
pop i = state $ \(xs) -> (xs !! i, take i xs ++ drop (i + 1) xs)

push :: Card -> State Deck () -- add card to hand or deck
push c = state $ \(xs) -> ((),c:xs)

-- let f = do pop 3; push (Card 4 "Spades"); pop 3; pop 3; in runState f [Card a b | a <- [1..10], b <- ["Spades","Clubs","Hearts","Diamonds"]]
-- okay so next up i want to figure out how to deal with hands, etc
-- maybe Players = [(Name, Hand)]; Game = ([Player], [Deck])
-- how might one remove players, or check if game is in progress
-- uhh maybe attach a nonzero value eg poker chips or card number or something
--
--
--
-- should bot be one game over all or one game per channel?
-- -- if one user is playing from 2 channels there may be confusion
--
-- okay i'm just doing fucking notekeeping here
-- stuff to do:
-- finish utility functions guesswork
-- -- draw card from deck to hand
-- -- maintain a sub deck for 'played' cards or whatever
-- -- consider the river or whatever it is for texas holdem
-- reorganize, put types and associated functions in files?
--
-- problems to solve:
-- game architecture
-- -- is game a type? how to track if one is in progress? extend Bot type?
-- -- how does the game flow functionally? this aint no imperative language
-- track players - type Player = (Name, [Card], pts)?
-- -- pts is to be implemented in game logic; chips, # of cards left, etc
-- -- how to remove player from play? put cards back in deck?
-- play game... asynchronously? game shouldn't halt other functions...?
-- validation - in game logic... ante validation, proper suit, etc
-- representation of Card; want generic, but want ordered etc.
-- -- two strings allows for any suits, any values... but no guaranteed order
-- -- can i... rename integers? ie 13 henceforth referred to as Ace?
-- what is ReaderT... do I hook CardState into Net or Bot?
--
-- oh i think i have an idea.
-- Bot or Net or whatever type includes card game state or lack thereof
-- functions modify the card game state and return new Net or Bot
-- can still do non-card game functions, only modified if called for
