import Data.Either
import Data.List
import Network
import System.IO
import System.IO.Unsafe
import System.Exit
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception as Ex
import Text.Printf
import Prelude hiding (catch)
import Title -- Title.hs module in same directory


-- Net monad, wrapper over IO, carrying the bot's immutable state
data Bot = Nope | Bot { socket :: Handle } -- Nope is the most bullshit way to solve my problem
type Net = StateT Bot IO

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
    return (Bot h) -- initially no game will be in progress
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
    where s = last xs

-- Evaluate active bot commands
eval :: [String] -> Net ()
eval (x:xs) | "!id " `isPrefixOf` msg && "mathu" == user = privmsg chan $ drop 4 msg
            | "!imp " `isPrefixOf` msg && "mathu" == user = let (ch, m) = break (==' ') (drop 5 msg)
                                                                  in privmsg ch (drop 1 m)
            | "!quit" == msg && "mathu" == user =
                write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
            where 
                msg = last xs
                user = takeWhile (/='!') x
eval _ = return ()

-- Send a privmsg to given channel on freenode
-- Break longer messages to send in more than one burst
privmsg :: String -> String -> Net ()
privmsg ch s = if (len > 500) then do
        write "PRIVMSG" (ch ++ " :" ++ (take (400 - length ch) s))
        privmsg ch (drop (400 - length ch) s) -- write the rest of s
    else do
        write "PRIVMSG" (ch ++ " :" ++ s)
    where 
        len = length ("PRIVMSG " ++ ch ++ " :" ++ s)

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

-- List of (Target, msg) -- execute all writes
privmsgSeq ((ch, msg) : msgs) = do
    privmsg ch msg
    privmsgSeq msgs
privmsgSeq [] = return ()

-- eventually add channel management interface
-- track joined chans in bot
-- take join chan from eval, write join chan and add chan to list
-- list is print array
-- part chan is pop chan from array, write part chan if exists
-- broadcast msg is privmsgSeq [(ch, msg) | ch <- chans]


-- a total rewrite is in order in a few months i think
-- after i clear my head
