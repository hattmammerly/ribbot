module Uno where

import Cards
import System.Random
import System.Random.Shuffle
import System.IO

-- Okay I don't feel like thinking more tonight
-- Order doesn't matter in Uno, just need equality
-- Create this card, make Game and Player use generic lists
-- Create a full uno deck, put in game on creation
-- Keep a played-cards deck, reshuffle that when out of cards
-- Draw seven cards on player creation - concat more decks for more players
-- how to check for winners/empty decks? Make necessary changes and repass to uno?

-- TURNS OUT I NEED TO REDO ALL TYPES EVERYWHEREEEEE
-- for example Game must be defined data Game a = Game [Players] [[a]] [(String, String)]
-- Same deal for player. And since game takes a type arg...
-- Bot also needs one, so Net needs one, and weird shit happens
data Color = Red | Blue | Yellow | Green | Misc deriving (Show)
data Value = Wild | WildDrawFour | Skip | Reverse |
             DrawTwo | DrawFour | One | Two | Three |
             Four | Five | Six | Seven | Eight | Nine deriving (Show, Eq)
type UnoCard = (Color, Value)

instance Eq Color where
    (==) _ _ = True


-- Routing, so we can be a little cleaner!
-- Different game states will have different options available, but case is ugly
-- then again, case would allow me to have those recurring 'where's only once
uno :: [String] -> Game -> IO Game
uno xs game = do
    case game of None -> unoNone xs game
                 (Organizing ps ds ms) -> unoOrganizing xs game
                 (Game ps ds ms) -> unoGame xs game
    where tokens = words $ last xs
          user = head xs
          chan = last $ init xs

-- Consider giving None space for messages so I can say a game must be started.
unoNone :: [String] -> Game -> IO Game
unoNone xs game = do
    case tokens of ("organize":xs) -> return $ addMsg (chan, "Organizing a game! Join with '!uno aye'") game
                   ("aye":xs) -> return $ addPlayer (user, [], 0) $ addMsg (chan, "Organizing a game! Join with '!uno aye'") game
                   xs -> return game                   
    where tokens = words $ last xs
          user = head xs
          chan = last $ init xs

unoOrganizing :: [String] -> Game -> IO Game
unoOrganizing xs game@(Organizing ps ds ms) = do
    case tokens of ("organize":xs) -> return $ addMsg (chan, "We're already organizing one! Join with '!uno aye'") game
                   ("aye":xs) -> return $ addPlayer (user, [], 0) $ addMsg (chan, user ++ " has joined the game.") game
                   ("start":xs) -> if (length ps > 0)
                                   then
                                     return $ addMsg (chan, "And here we go! First up is " ++ getName (head ps)) $
                                       tellNext (Game ps ds ms)
                                   else return $ addMsg (chan, "Try adding some players first.") game
                   ("forfeit":xs) -> return $ snd $ removePlayer user $ addMsg (chan, user ++ " is a quitter!") game
                   xs -> return game
    where tokens = words $ last xs
          user = head xs
          chan = last $ init xs

-- Oh gosh, multiple uses of xs is confusing!
unoGame :: [String] -> Game -> IO Game
unoGame xs game@(Game ps ds ms) = do
    case tokens of ("organize":xs) -> return $ addMsg (chan, "Game in progress.") game
                   ("aye":xs) -> return $ addMsg (chan, "Sorry, " ++ user ++ ", you missed your chance.") game
                   ("start":xs) -> return $ addMsg (chan, "Game in progress, bub.") game
                   ("forfeit":xs) -> return $ snd $ removePlayer user $ addMsg (chan, user ++ " is a quitter!") game
    where tokens = words $ last xs
          user = head xs
          chan = last $ init xs


-- get name of player for convenience
getName :: Player -> String
getName player@(name, _, _) = name
getHand :: Player -> Hand
getHand player@(_, hand, _) = hand
getScore :: Player -> Int
getScore player@(_, _, score) = score

tellNext game@(Game ps ds ms) = addMsg (getName (head ps), show (getHand (head ps))) game

-- TODO
-- Deck should be generic list, cards defined on per-game basis
-- Read others' code so I can see how to deal with these longass lines
-- I waste a lot of space indenting to case blocks, etc
-- Add game termination
-- Give players hands when they join
-- Put cards back in deck when forfeit
-- Add in playing cards, etc - don't bother until card type is worked out!
