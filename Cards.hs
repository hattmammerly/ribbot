module Cards where

-- import Data.Random.List
import Data.List
import Control.Monad
import Control.Monad.State
import System.Random
import System.Random.Shuffle

-- use fromIntegral to convert eg 10 to Jack?
data Card = Card { value :: Int -- "King" > "Queen" is false... how to do this?
                 , suit :: String } deriving (Eq, Show)
type Deck = [Card]
type Hand = Deck -- more or less the same operations are applied
type Player = (String, Hand, Int) -- Name, hand, points

-- Do I need suspended? 
data Game = None | Organizing [Player] [Deck] [(String, String)]
                 | Game [Player] [Deck] [(String, String)]
                 | Suspended [Player] [Deck] [(String, String)] deriving (Eq, Show)

pop :: Int -> State Deck Card -- pop arbitrary index (eg to play a card)
pop i = state $ \(xs) -> (xs !! i, take i xs ++ drop (i + 1) xs)

push :: Card -> State Deck () -- add card to hand or deck
push c = state $ \(xs) -> ((),c:xs)

-- Add player to game, only before it has started.
-- I believe this works as a stateful computation?
addPlayer :: Player -> Game -> ((), Game)
addPlayer player None = ((), Organizing (player:[]) [] [])
addPlayer player (Organizing players decks msgs) = ((), Organizing (player:players) decks msgs)
addPlayer _ g = ((), g)

-- Add a deck to the game - be it a deck, scrap pile, or 'river' or w/e
-- I believe this works as a stateful computation?
addDeck :: Deck -> Game -> ((), Game)
addDeck deck None = ((), Organizing [] (deck:[]) [])
addDeck deck (Organizing players decks msgs) = ((), Organizing players (deck:decks) msgs)
addDeck deck (Game players decks msgs) = ((), Game players (deck:decks) msgs)
-- Does this need a Suspended case? Do I even need Suspended?

-- This still needs to be tested!
-- Pop player by name, so game logic can appropriately deal with cards, points
removePlayer :: String -> Game -> (Player, Game)
removePlayer name (Organizing players decks msgs) = ((head (fst part)), (Organizing (tail (snd part)) decks msgs))
                  where part = partition (\(player, _, _) -> (name /= player)) players
removePlayer name (Game players decks msgs) = ((head (fst part)), (Game (tail (snd part)) decks msgs))
                  where part = partition (\(player, _, _) -> (name /= player)) players
removePlayer name (Suspended players decks msgs) = ((head (fst part)), (Suspended (tail (snd part)) decks msgs))
                  where part = partition (\(player, _, _) -> (name /= player)) players

-- Draw n cards from deck
-- actually must return hand AND deck minus those cards
-- rework - sequence pop 0s against shuffleDeck deck somehow
-- looks like I get to actually use monad stuff!!
-- take gen as parameter to return [Card] sans IO
drawCards :: Int -> Deck -> IO [Card]
drawCards n deck = liftM (take n) $ liftM (shuffle' deck (length deck)) $ getStdGen

-- exportable pure shuffle function
shuffleDeck :: (RandomGen g) => Deck -> g -> Deck
shuffleDeck deck g = shuffle' deck (length deck) g

-- Add a message to the queue to be sent
-- addMsg :: Deck -> Game -> ((), Game)
-- addMsg msg None = ((), Organizing [] [] msg:[])
-- addMsg msg (Organizing players decks msgs) = ((), Organizing players decks msg:msgs)
-- addMsg msg (Game players decks msgs) = ((), Game players decks msg:msgs)
-- addMsg msg (Suspended players decks msgs) = ((), Suspended players decks msg:msgs)


-- let f = do pop 3; push (Card 4 "Spades"); pop 3; pop 3; in runState f [Card a b | a <- [1..10], b <- ["Spades","Clubs","Hearts","Diamonds"]]




-- THINKING
-- then over in ribbot i guess I'll do gets game and send IO Game to cards
-- the game state inside the bot will be IO Game instead of just Game
-- except rename execute to be the game in question!!! ohhh!!!!!!!
-- okay I know how to organize this stuff I think
--

-- TODO
-- should this be stateful computations inside of IO?
-- -- maybe this sort of thing is solved by io alias in ribbot?
