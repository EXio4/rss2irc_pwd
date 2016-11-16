#!/usr/bin/env runhaskell
{-# LANGUAGE PatternGuards, BangPatterns, DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}
--------------------------------------------------------------------
{- |
rss2irc - watches an RSS/Atom feed and writes it to an IRC channel.

Copyright (c) Don Stewart 2008-2009, Simon Michael 2009-2014
License: BSD3.
-}
--------------------------------------------------------------------

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (when,unless)
import Data.Maybe
import Data.Time.Clock (getCurrentTime)
import Prelude hiding (log)
import Network (withSocketsDo)
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit (newManager)
import System.Console.CmdArgs
import System.Exit (ExitCode(ExitSuccess), exitFailure, exitSuccess)
import System.IO
import System.IO.Storage
import Text.Printf (printf)

import Base
import Utils
import Feed
import Irc


-- | Get parsed and checked options and a reader and bot ready to connect.
getRss2IrcArgs :: IO (Opts, Reader, Bot)
getRss2IrcArgs = do
  opts <- cmdArgs defopts >>= \opts -> do
    unless (interval opts > 0 || (maybe False (<=10) $ num_iterations opts)) $
      opterror "--interval 0 requires --num-iterations 10 or less"
    seq (applyReplacements opts "") $ return () -- report any bad --replace regexp
    return opts
  q <- atomically $ newTChan
  t <- getCurrentTime
  let reader = Reader{httpManager=Nothing
                     ,iterationsleft=num_iterations opts
                     }
      bot = Bot{socket        = stdout
               ,server        = ""
               ,port          = defport
               ,channel       = ""
               ,botnick       = ""
               ,password      = Nothing
               ,announcequeue = q
               ,batchindex    = 0
               ,lastmsgtime   = t
               }
      bot' = case ircAddressFromOpts opts of
              Nothing -> bot
              Just (IrcAddress s p pwd c n) ->
                bot{server  = s
                   ,port    = fromMaybe defport p
                   ,channel = c
                   ,botnick = n
                   ,password = pwd
                   }
  return (opts, reader, bot')

-- | Process arguments, join the irc channel, start worker threads,
-- disconnect and report when there is a problem.
main :: IO ()
main =
 withStore "globals" $ do -- for readFeedFile
  -- http-conduit stuff
  withSocketsDo $ do -- for ms windows
   manager <- io $ newManager defaultManagerSettings
   (opts,reader,bot) <- getRss2IrcArgs
   let app = App{aOpts=opts,aReader=reader{httpManager=Just manager},aBot=bot}
   when (delay opts > 0) $ threadDelay $ (delay opts) * minutes
   runThreads app

runThreads :: App -> IO ()
runThreads app@App{aOpts=opts} = do
  -- catch any termination or error in sub-threads and handle it here
  r <- try $ bracket (connect app) disconnect $ \a -> do
    appvar <- newSharedVar a
    -- 1. the feed reader thread polls forever or until it reaches max
    -- iterations or raises a non-transient exception
    _ <- forkMonitoredIO $ feedReader appvar
    -- 2. the irc announcer runs until it raises an exception or is killed
    _ <- forkMonitoredIO $ ircAnnouncer appvar
    -- 3. the main thread becomes the irc responder, keeping the
    -- connection alive until it raises or receives an exception
    ircResponder appvar
  -- exit after an exception
  preExitDelay
  case r of
    Right _                                      -> unless (quiet opts) (log "normal termination.") >> exitSuccess -- shouldn't happen
    Left e | Just ExitSuccess <- fromException e -> unless (quiet opts) (log "normal termination") >> exitSuccess
    Left e                                       -> unless (quiet opts) (putStr "\n" >> log (show e)) >> exitFailure

-- | Spawn a thread which will throw any exception or termination (as
-- ExitSuccess) to us. Also log error exceptions.
forkMonitoredIO :: IO () -> IO ThreadId
forkMonitoredIO action = do
  me <- myThreadId
  forkIO $ do
    ex <- action >> return (toException ExitSuccess)
          `Control.Exception.catch` \e ->
            case fromException e of
              Just ExitSuccess -> return e
              _ -> log (printf "Error: %s" (show (e::SomeException))) >> return e
    throwTo me ex

-- hack: give announcer a chance to announce items from final poll
preExitDelay :: IO ()
preExitDelay = threadDelay $ 500000
