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
-- consider actually monadizing -> ((), Game)
addPlayer :: Player -> Game -> Game
addPlayer player None = Organizing player:[] []
addPlayer player (Organizing players decks) = Organizing player:players decks
addPlayer _ g = g

-- Add a deck to the game - be it a deck, scrap pile, or 'river' or w/e
-- consider actually monadizing -> ((), Game)
addDeck :: Deck -> Game -> Game
addDeck deck None = Organizing players deck:[]
addDeck deck (Organizing players decks) = Organizing players deck:decks
addDeck deck (Game players decks) = Game players deck:decks

-- This isn't so simple - what to with their points and cards? Game logic?
-- perhaps pop player -> (player, Game) and game logic can distribute
removePlayer :: String -> Game -> Game
removePlayer name (Organizing players decks) = Organizing (filter target players) decks
                  where target = (\(player, _, _) -> (name /= player))
removePlayer name (Game players decks) = Game (filter target players) decks
                  where target = (\(player, _, _) -> (name /= player))
removePlayer name (Suspended players decks) = Suspended (filter target players) decks
                  where target = (\(player, _, _) -> (name /= player))

-- let f = do pop 3; push (Card 4 "Spades"); pop 3; pop 3; in runState f [Card a b | a <- [1..10], b <- ["Spades","Clubs","Hearts","Diamonds"]]
