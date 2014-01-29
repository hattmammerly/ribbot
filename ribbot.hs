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
                      , game :: IO Game }
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
    return (Bot h (return None)) -- initially no game will be in progress
    -- nested return is probably really stupid
  where
    notify a = Ex.bracket_
      (printf "Connecting to %s ... " server >> hFlush stdout)
      (putStrLn "done.")
      a

run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick ++ " 0 * :matt testing a bot")
    write "JOIN" chan
    gets socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
  s <- init `fmap` io (hGetLine h)
  io (putStrLn s)
  if ping s
    then pong s
  else do
    scan (tokenize s)
    eval (tokenize s)
  where
    forever a = a >> forever a
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

-- Separate into name, command, target, whatever else, and message
tokenize s = words (fst tmp) ++ ((drop 1 (snd tmp)) : [])
    where tmp = span (/=':') $ drop 1 s

-- Scan for passive bot cues
scan :: [String] -> Net () -- This function is messy!
scan xs = do
    a <- io $ liftM rights $ sequence $ fmap getTitle (findURLs s) -- Extract [String] from [IO (Either ParserError String)] 
    mapM_ (\x -> privmsg chan ("Link Title: [ " ++ x ++ " ]")) a;
    where s = drop 1 (last xs)

-- Evaluate active bot commands
eval :: [String] -> Net ()
eval (x:xs) | "!id" `isPrefixOf` msg && "hattmammerly" == user = privmsg chan $ drop 4 msg
            | ("!quit" == msg) && ("hattmammerly" == user)
                 = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
            | "!uno " `isPrefixOf` msg = do -- send game and line to uno
                 g <- gets game
                 updateGame $ uno (user: init xs ++ [(drop 5 msg)]) g
--          | "!show" == msg = do
--               g <- gets game
--               showGame g
        where 
            msg = last xs
            user = takeWhile (/='!') x
eval _ = return ()

-- Send a privmsg to given chan + server
privmsg :: String -> String -> Net ()
privmsg ch s = write "PRIVMSG" (ch ++ " :" ++ s)

-- Send a message to the server
-- -- -- Command -> Value
write :: String -> String -> Net ()
write s t = do
  h <- gets socket
  io $ hPrintf h "%s %s\r\n" s t
  io $ printf   ">%s %s\n"   s t

-- Convenience -- haskellwiki had this. I see no value.
io :: IO a -> Net a
io = liftIO

-- Print game for debugging purposes I suppose
showGame :: Game -> Net ()
showGame g = do
    privmsg chan (show g)

-- returns the bot with the updated game state
updateGame :: IO Game -> Net ()
updateGame g = do
    h <- gets socket
    put $ Bot h g 

-- uno game logic - own file, import ribbot.hs and random stuff?
-- need to work out how to send messages out to users
-- Game encased in IO encased in Bot inside a Net
uno :: [String] -> IO Game -> IO Game
uno xs iogame = do
--  gen <- getStdGen
  game <- iogame
  -- privmsg chan "playing uno!" -- can't do this
  -- printf "%s" "test!" -- no type error but also no action taken...?
  -- added complication of requiring the handle. can't gets inside here, not stateT
  return game -- tentative, obviously


-- TODO
-- reorganize code
-- -- move uno stuff to another file
-- -- -- has to import main file to return Net () in functions?
-- figure out how to limit time when addPlayer can be called
-- -- implement in the game module, not here!
-- -- don't want people joining midgame or when none is started
