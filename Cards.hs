module Cards where

-- import Data.Random.List
import Data.List
import Control.Monad
import Control.Monad.State
import System.Random
import System.Random.Shuffle

data Card = Card { value :: Int -- "King" > "Queen" is false... how to do this?
                 , suit :: String } deriving (Eq, Show)

type Deck = [Card]
type Hand = Deck -- more or less the same operations are applied

-- Do I need suspended? 
data Game = None | Organizing [Player] [Deck] | Game [Player] [Deck]
          | Suspended [Player] [Deck] deriving (Eq, Show)

type Player = (String, Hand, Int) -- Name, hand, points

-- use shuffle' to shuffle deck ; liftM (shuffle' list (length list)) $ getStdGen

pop :: Int -> State Deck Card -- pop arbitrary index (eg to play a card)
pop i = state $ \(xs) -> (xs !! i, take i xs ++ drop (i + 1) xs)

push :: Card -> State Deck () -- add card to hand or deck
push c = state $ \(xs) -> ((),c:xs)

-- Add player to game, only before it has started.
-- I believe this works as a stateful computation?
addPlayer :: Player -> Game -> ((), Game)
addPlayer player None = ((), Organizing (player:[]) [])
addPlayer player (Organizing players decks) = ((), Organizing (player:players) decks)
addPlayer _ g = ((), g)

-- Add a deck to the game - be it a deck, scrap pile, or 'river' or w/e
-- I believe this works as a stateful computation?
addDeck :: Deck -> Game -> ((), Game)
addDeck deck None = ((), Organizing [] (deck:[]))
addDeck deck (Organizing players decks) = ((), Organizing players (deck:decks))
addDeck deck (Game players decks) = ((), Game players (deck:decks))
-- Does this need a Suspended case? Do I even need Suspended?

-- This still needs to be tested!
-- Pop player by name, so game logic can appropriately deal with cards, points
removePlayer :: String -> Game -> (Player, Game)
removePlayer name (Organizing players decks) = ((head (fst part)), (Organizing (tail (snd part)) decks))
                  where part = partition (\(player, _, _) -> (name /= player)) players
removePlayer name (Game players decks) = ((head (fst part)), (Game (tail (snd part)) decks))
                  where part = partition (\(player, _, _) -> (name /= player)) players
removePlayer name (Suspended players decks) = ((head (fst part)), (Suspended (tail (snd part)) decks))
                  where part = partition (\(player, _, _) -> (name /= player)) players

-- let f = do pop 3; push (Card 4 "Spades"); pop 3; pop 3; in runState f [Card a b | a <- [1..10], b <- ["Spades","Clubs","Hearts","Diamonds"]]
