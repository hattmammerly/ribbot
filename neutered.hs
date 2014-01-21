-- ONLY WRITES INITIAL SETUP AND PINGS - NO LINKS POSTED, ETC

import Data.Either
import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception as Ex
import Text.Printf
import Prelude hiding (catch)
import Cards -- Cards.hs module in the same directory as this file
import Title -- title.hs module in same directory


-- Net monad, wrapper over IO, carrying the bot's immutable state
data Bot = Nope | Bot { socket :: Handle -- Nope is the most bullshit way to solve my problem
                      , game :: Game }
type Net = StateT Bot IO
-- type Net = StateT Game (ReaderT Bot IO)

server = "irc.freenode.org"
port = 6667
chan = "#testmattbot"
nick = "testmattbot"

main = Ex.bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = (runStateT run st) `Ex.catch` (\(SomeException _) -> return ((), Nope))
    -- state monad expected a ((), Bot) so I redefined bot. Nope = dummy type

-- connect to server and return initial bot state
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber port)
    hSetBuffering h NoBuffering
    return (Bot h None) -- initially no game will be in progress
  where
    notify a = Ex.bracket_
      (printf "Connecting to %s ... " server >> hFlush stdout)
      (putStrLn "done.")
      a

run :: Net ()
run = do
    write' "NICK" nick
    write' "USER" (nick ++ " 0 * :matt testing a bot")
    write' "JOIN" chan
    gets socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
  s <- init `fmap` io (hGetLine h)
  io (putStrLn s)
  if ping s
    then pong s
  else do
    scan (tokenize s)
    eval (clean s)
  where
    forever a = a >> forever a
    clean = drop 1 . dropWhile (/= ':') . drop 1 -- gonna need to rework
    ping x = "PING :" `isPrefixOf` x
    pong x = write' "PONG" (':' : drop 6 x)

tokenize s = words (fst tmp) ++ (snd tmp : [])
    where tmp = span (/=':') $ drop 1 s

-- Scan for passive bot cues
scan :: [String] -> Net () -- This function is messy!
scan xs = do
    a <- io $ liftM rights $ sequence $ fmap getTitle (findURLs s) -- Extract [String] from [IO (Either ParserError String)] 
    mapM_ (\x -> privmsg ("Link Title: [ " ++ x ++ " ]")) a;
    where s = drop 1  (last xs)

-- Evaluate active bot commands
eval :: String -> Net ()
eval "!quit" = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
       | "!uno " `isPrefixOf` x = do
           g <- gets game
           if g == None
               then privmsg "Feature coming soon!" -- If no game, start one
               else privmsg "There's already a game in progress, let's not get too crazy in here!"
       | "!show" `isPrefixOf` x = showGame
eval _ = return ()

-- Send a privmsg to current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a message to the server
-- -- -- Command -> Value
write' :: String -> String -> Net ()
write' s t = do
  h <- gets socket
  io $ hPrintf h "%s %s\r\n" s t
  io $ printf   ">%s %s\n"   s t

write :: String -> String -> Net ()
write s t = do
  h <- gets socket
  io $ printf   ">%s %s\n"   s t

-- Convenience -- haskellwiki had this. Personally I see no value.
io :: IO a -> Net a
io = liftIO

-- Print game for debugging purposes I suppose
showGame :: Net ()
showGame = do
    g <- gets game
    privmsg (show g)

-- returns the bot with the updated game state
updateGame :: Game -> Net ()
updateGame g = do
    h <- gets socket
    put $ Bot h g 

-- TODO
-- reorganize code
-- -- move uno stuff to another file
-- -- -- has to import main file to return Net () in functions
-- figure out how to limit time when addPlayer can be called
-- -- implement in the game module, not here!
-- -- don't want people joining midgame or when none is started
-- -- could write a stopwatch w/ state monad
-- -- -- but I have to lug it around in Bot always?
-- -- track state - None - Organizing - In Progress - Suspended
-- get username of person sending message for addPlayer
-- -- clean function strips that out
-- redo clean function!
-- -- tokenize irc line (include username & message at least)
--
-- Tentative Plan
-- limit addPlayer function via game state in Cards module
