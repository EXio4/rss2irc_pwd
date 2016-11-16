{-# LANGUAGE PatternGuards, BangPatterns, DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}
{- |

IRC stuff

Copyright (c) Don Stewart 2008-2009, Simon Michael 2009-2014
License: BSD3.

-}

module Irc where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 as B8 (pack, unpack)
import Data.List
import Data.Maybe
import Data.Time.Clock (getCurrentTime,diffUTCTime)
import Network (PortID(PortNumber), connectTo)
import Network.IRC (Message(Message),msg_command,msg_params,decode,encode,joinChan,privmsg)
import Prelude hiding (log)
import System.IO (BufferMode(NoBuffering),stdout,hSetBuffering,hFlush,hClose,hGetLine,hPutStr)
import Text.Printf

import Base
import Utils


-- | Connect to the irc server.
connect :: App -> IO App
connect !app@App{aOpts=opts, aBot=bot@Bot{server=srv,port=p,password=pwd,channel=c,botnick=n}} = do
  unless (quiet opts) $
    log $ n ++ " connecting to " ++
            (if null srv then "(simulated)" else printf "%s, channel %s" srv c)
  bot' <- if null srv
          then return bot
          else do
            h <- connectTo srv (PortNumber $ fromIntegral p)
            hSetBuffering h NoBuffering
            return bot{socket=h}
  case pwd of
      Nothing   -> return ()
      Just pwd' -> ircWrite opts bot' pwd'
  ircWrite opts bot' n
  ircWrite opts bot' $ if null (ident opts) then defusername else ident opts
  (connected,err) <- if null srv then return (True,"")
                                 else ircWaitForConnectConfirmation opts bot' -- some servers require this
  unless connected $ throw $ IrcException err
  ircWrite opts bot' $ B8.unpack $ encode $ joinChan $ B8.pack c
  unless (quiet opts) $ log "connected."
  return app{aBot=bot'}

-- | Disconnect from the irc server, if connected.
disconnect :: App -> IO ()
disconnect App{aBot=Bot{server=srv,socket=s}}
    | s == stdout = return ()
    | otherwise = log (printf "disconnecting from %s" srv) >> hClose s

-- | Wait for server connection confirmation.
ircWaitForConnectConfirmation :: Opts -> Bot -> IO (Bool,String)
ircWaitForConnectConfirmation _ Bot{server=""} = return (True,"")
ircWaitForConnectConfirmation !opts !bot@Bot{socket=h} = do
  s <- hGetLine h
  when (debug_irc opts) $ log $ printf "<-%s" s
  if isPing s
    then ircPong opts bot s >> ircWaitForConnectConfirmation opts bot
    else if isResponseOK s
         then return (True, chomp s)
         else if isNotice s
              then ircWaitForConnectConfirmation opts bot
              else return (False, chomp s)
  where
    parseRespCode x = if length (words x) > 1 then (words x) !! 1 else "000" 
    isResponseOK x = (parseRespCode x) `elem` [ "001", "002", "003", "004" ]
    isNotice     x = (head $ parseRespCode x) `elem` ('0':['a'..'z']++['A'..'Z'])

{-
2011-10-18 13:28:20 PDT: <-PING :niven.freenode.net
2011-10-18 13:28:20 PDT: ->PONG niven.freenode.net
hGetIRCLine :: Handle -> IO MsgString      Read an IRC message string.
hGetMessage :: Handle -> IO Message        Read the next valid IRC message.
hPutCommand :: Handle -> Command -> IO ()  Write an IRC command with no origin.
hPutMessage :: Handle -> Message -> IO ()  Write an IRC message.
-}
-- | Run forever, responding to irc PING commands to keep the bot connected.
-- Also keeps track of the last time a message was sent, for --idle.
ircResponder :: Shared App -> IO ()
ircResponder !appvar = do
  app@App{aOpts=opts,aBot=bot@Bot{server=srv,socket=h}} <- getSharedVar appvar
  if null srv
   then threadDelay (maxBound::Int)
   else do
    s <- hGetLine h
    let s' = init s
    when (debug_irc opts) $ log $ printf "<-%s" s'
    let respond | isMessage s = do t <- getCurrentTime
                                   putSharedVar appvar app{aBot=bot{lastmsgtime=t}}
                | isPing s    = ircPong opts bot s'
                | otherwise   = return ()
    respond
  ircResponder appvar

-- | Run forever, printing announcements appearing in the bot's announce
-- queue to its irc channel, complying with bot and irc server policies.
-- Specifically:
--
-- - no messages until --idle minutes of silence on the channel
--
-- - no more than 400 chars per message
--
-- - no more than one message per 2s
--
-- - no more than --max-items feed items announced per polling interval
--
-- - no more than --max-items messages per polling interval, except a
--   final item split across multiple messages will be completed.

-- XXX On freenode, six 400-char messages in 2s can still cause a flood.
-- Try limiting chars-per-period, or do ping-pong ?
ircAnnouncer :: Shared App -> IO ()
ircAnnouncer !appvar = do
    -- wait for something to announce
    App{aBot=Bot{announcequeue=q}} <- getSharedVar appvar
    ann <- atomically $ readTChan q
    -- re-read bot to get an up-to-date idle time
    app@App{aOpts=opts, aBot=bot@Bot{server=srv,batchindex=i}} <- getSharedVar appvar
    idletime <- channelIdleTime bot
    let batchsize    = max_items opts
        requiredidle = idle opts                   -- minutes
        pollinterval = interval opts               -- minutes
        sendinterval = if null srv then 0 else 2 -- seconds
        iscontinuation = continuationprefix `isPrefixOf` ann
        go | i >= batchsize && not iscontinuation = do
               -- reached max batch size, sleep
               when (debug_irc opts) $
                    log $ printf "sent %d messages in this batch, max is %d, sleeping for %dm" i batchsize pollinterval
               threadDelay $ pollinterval * minutes
               atomically $ unGetTChan q ann
               putSharedVar appvar app{aBot=bot{batchindex=0}}
               ircAnnouncer appvar
           | requiredidle > 0 && (idletime < requiredidle) = do
               -- not yet at required idle time, sleep
               let idleinterval = requiredidle - idletime
               when (debug_irc opts) $ log $
                 printf "channel has been idle %dm, %dm required, sleeping for %dm" idletime requiredidle idleinterval
               threadDelay $ idleinterval * minutes
               atomically $ unGetTChan q ann
               ircAnnouncer appvar
           | otherwise = do
               -- ok, announce it
               when (debug_irc opts) $ do
                 let s | requiredidle == 0 = "" :: String
                       | otherwise = printf " and channel has been idle %dm" idletime
                 log $ printf "sent %d messages in this batch%s, sending next" i s
               let (a,rest) = splitAnnouncement ann
               when (not $ null rest) $ atomically $ unGetTChan q rest
               ircPrivmsg opts bot a
               threadDelay $ sendinterval * seconds
               putSharedVar appvar app{aBot=bot{batchindex=i+1}}
               ircAnnouncer appvar
    go

-- | The time in minutes since the last message on this bot's channel, or
-- otherwise since joining the channel. Leap seconds are ignored.
channelIdleTime :: Bot -> IO Int
channelIdleTime (Bot{lastmsgtime=t1}) = do
  t <- getCurrentTime
  return $ round (diffUTCTime t t1) `div` 60

-- IRC utils

-- | Send a response to the irc server's ping.
ircPong :: Opts -> Bot -> String -> IO ()
ircPong opts b x  = ircWrite opts b $ printf "PONG :%s" (drop 6 x)

-- | Send a privmsg to the bot's irc server & channel, and to stdout unless --quiet is in effect.
ircPrivmsg :: Opts -> Bot -> String -> IO ()
ircPrivmsg opts bot@(Bot{channel=c}) msg = do
  ircWrite opts bot $ B8.unpack $ encode $ privmsg (B8.pack c) (B8.pack msg')
  unless (quiet opts) $ putStrLn msg >> hFlush stdout
 where
  msg' | use_actions opts = "\1ACTION " ++ msg ++ "\1"
       | otherwise        = msg

-- | Send a message to the bot's irc server, and log to the console if --debug-irc is in effect.
ircWrite :: Opts -> Bot -> String -> IO ()
ircWrite opts (Bot{server=srv,socket=h}) s = do
  when (debug_irc opts) $ log $ printf "->%s" s -- (B8.unpack $ showCommand c)
  unless (null srv) $ hPutStr h (s++"\r\n")

isMessage :: String -> Bool
isMessage s = isPrivmsg s && not ("VERSION" `elem` (maybe [] msg_params $ decode $ B8.pack s))

isPrivmsg :: String -> Bool
isPrivmsg s = case decode $ B8.pack s of Just Message{msg_command="PRIVMSG"} -> True
                                         _ -> False

isPing :: String -> Bool
isPing s = case decode $ B8.pack s of Just Message{msg_command="PING"} -> True
                                      _ -> False

