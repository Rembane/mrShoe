-- | The main program! \o/
-- The IRC part here is heavily inspired by:
-- https://wiki.haskell.org/Roll_your_own_IRC_bot
{-# LANGUAGE DeriveGeneric, FlexibleContexts, OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Exception (bracket, bracket_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (get, gets, evalStateT, put, runState, StateT)
import Control.Monad.Random (runRand)
import Data.List (init, isInfixOf, isPrefixOf)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Yaml (FromJSON, decodeFileEither)
import GHC.Generics (Generic)
import Network (PortID(..), connectTo)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (BufferMode(..), Handle, hClose, hFlush, hGetLine, hSetBuffering, stdout)
import System.Random (StdGen, getStdGen, randomR)
import Text.Printf (hPrintf, printf)

import Intelligence
import ParseLog

data Config = Config {
  -- This is for the IRC-bot.
  server :: String,
  port   :: Int,
  chan   :: String,
  nick   :: String,

  -- This is for the Markov chains.
  prefixLength  :: Int,
  logFile       :: String,
  maxLineLength :: Int
} deriving Generic

instance FromJSON Config

type Net = StateT Bot IO
data Bot = Bot {
  socket    :: Handle,
  generator :: StdGen,
  db        :: DB,
  config    :: Config
}

-- | Command line arguments:
-- 1: Length of the prefix in words. Five is a reasonable default.
-- 2: The max length of a generated line, in words.
-- 3: The path to the IRC-log to seed the bot with.
main :: IO ()
main = do
  bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = evalStateT run st

readConfig :: IO Config
readConfig = do
  either (error . show) id <$> decodeFileEither "settings.yaml"

-- | Initialize global state, connect to the server
--   and return the initial bot state.
connect :: IO Bot
connect = do
  config <- readConfig
  g      <- getStdGen
  db     <- createDatabase (Main.prefixLength config) <$> (parseLog $ logFile config)
  printf "Connecting to %s ... " $ server config
  hFlush stdout
  h      <- connectTo (server config) (PortNumber (fromIntegral $ port config))
  hSetBuffering h NoBuffering
  putStrLn "Done."

  return (Bot h g db config)

-- | We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
  config <- gets config
  write "NICK" (nick config)
  write "USER" $ (nick config) ++ " 0 * :mrShoe, the bot"
  write "JOIN" $ chan config
  gets socket >>= listen

-- Process each line from the server
-- TODO: Convert all strings to Data.Text.Text
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init <$> liftIO (hGetLine h)
    liftIO (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

-- | Dispatch a command on a certain input
eval :: String -> Net ()
eval     "!quit"               = write "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval     s                     = do
  -- We have a 10% chance for every line that is received of saying something.
  -- Except for when spoken to or about, then it is 100%.
  bot <- get
  if isInfixOf (nick $ config bot) s
     then saySomething
     else do
       let (v, g') = randomR (0,9 :: Int) $ generator bot
       put $ bot { generator = g' }
       if v == 0
          then saySomething
          else return ()
  where
    saySomething = do
      bot <- get
      let (line, g') = runRand (generateLine (Main.prefixLength $ config bot) (db bot)) (generator bot)
      put $ bot { generator = g' }
      privmsg $ T.unpack line

-- | Send a message to the current chan + server
privmsg :: String -> Net ()
privmsg s = do
  config <- gets config
  write "PRIVMSG" ((chan config) ++ " :" ++ s)

-- | Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- gets socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t

