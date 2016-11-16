{-# LANGUAGE PatternGuards, BangPatterns, DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
-- DeriveGeneric, StandaloneDeriving
{- |

Types and settings.

Copyright (c) Don Stewart 2008-2009, Simon Michael 2009-2014
License: BSD3.

-}

module Base where

import Control.Concurrent.STM (TChan)
-- import Control.DeepSeq (NFData)
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import GHC.Generics
import Data.Time ()
import Data.Time.Clock
import Data.Typeable
import Network.HTTP.Conduit (Manager)
import System.Console.CmdArgs
import System.IO (Handle)
-- import Text.Feed.Types
import Text.Printf (printf)

  
progname            = "rss2irc"
version             = "1.1"  -- sync with rss2irc.cabal
progversion         = progname ++ " " ++ version :: String
defport             = 6667
defusername         = progname
defrealname         = progname ++ " feed announcer"
defuseragent :: String
defuseragent        = progversion
definterval         = 5
defidle             = 0
defmaxitems         = 5
maxmessagelength    = 400
-- | Maximum size of each part of our irc announcements.
-- The max announcement length will be the sum of these, plus typically 15
-- due to prettification, plus any length increase due to --replace. The
-- defaults below should keep most announcements within maxmessagelength
-- and all announcements within maxmessagelength * 2 or so.
maxtitlelength      = 100
maxdesclength       = 300
maxauthorlength     = 50
maxdatelength       = 50
maxlinklength       = 200

progname, version, progversion, defusername, defrealname :: String
defport, definterval, defidle, defmaxitems, maxmessagelength, maxtitlelength,
 maxdesclength, maxauthorlength, maxdatelength, maxlinklength :: Int
defopts :: Opts

defopts = Opts {
     ident             = defrealname &= typ "STR" &= help "set the bot's identity string (useful for contact info)"
    ,uagent            = defuseragent &= typ "STR" &= help "set the bot's Http UserAgent string "
    ,delay             = def &= help "wait for N minutes before starting (helps avoid mass joins)"
    ,interval          = definterval &= name "i" &= help ("polling and announcing interval in minutes (default "++(show definterval)++")")
    ,cache_control     = def &= explicit &= name "cache-control" &= typ "STR" &= help ("set a HTTP cache-control header when polling")
    ,idle              = defidle &= help ("announce only when channel has been idle N minutes (default "++(show defidle)++")")
    ,max_items         = defmaxitems &= help ("announce at most N items per interval (default "++(show defmaxitems)++")")
    ,recent            = def &= name "r" &= help "announce up to N recent items at startup (default 0)"
    ,ignore_ids_and_times = def &= help "ignore feed item IDs and timestamps (use for feeds with bad ones)"
    ,allow_duplicates  = def &= help "turn off duplicate announcement protection (enabled by default)"
    ,use_actions       = def &= help "use CTCP ACTIONs instead of normal IRC messages"
    ,no_title          = def &= help ("don't show item title (shown by default, up to "++(show maxtitlelength)++" chars)")
    ,author            = def &= name "a" &= help ("show author (up to "++(show maxauthorlength)++" chars)")
    ,description       = def &= name "d" &= help ("show description (up to "++(show maxdesclength)++" chars)")
    ,link_             = def &= help ("show link URL (up to "++(show maxlinklength)++" chars)")
    ,time              = def &= help ("show timestamp (up to "++(show maxdatelength)++" chars)")
    ,email             = def &= help "show email addresses (stripped by default)"
    ,html              = def &= help "show HTML tags and entities (stripped by default)"
    ,replace           = def &= typ "OLD/NEW" &= help "replace OLD with NEW (regexpr patterns)"
    ,num_iterations    = def &= name "n" &= help "exit after N iterations"
    ,quiet             = def &= help "silence normal console output"
    ,debug_irc         = def &= help "log irc activity"
    ,debug_feed        = def &= help "log feed items and polling stats"
    ,debug_xml         = def &= help "log feed content"
    ,feed              = def &= argPos 0 &= typ "FEEDURL"
    ,irc_address       = def &= argPos 1 &= opt ("" :: String) &= typ "IRCSERVER[:PORT]/CHANNEL/NICK"
    }
    &= program progname
    &= groupname progname
    &= summary progversion

data Opts = Opts {
     ident                :: String
    ,uagent               :: String
    ,delay                :: Int
    ,interval             :: Int
    ,cache_control        :: String
    ,idle                 :: Int
    ,max_items            :: Int
    ,recent               :: Int
    ,ignore_ids_and_times :: Bool
    ,allow_duplicates     :: Bool
    ,use_actions          :: Bool
    ,no_title             :: Bool
    ,author               :: Bool
    ,description          :: Bool
    ,link_                :: Bool
    ,time                 :: Bool
    ,email                :: Bool
    ,html                 :: Bool
    ,replace              :: [String]
    ,num_iterations       :: (Maybe Int)
    ,quiet                :: Bool
    ,debug_irc            :: Bool
    ,debug_feed           :: Bool
    ,debug_xml            :: Bool
    ,feed                 :: String
    ,irc_address          :: String
    } deriving (Show, Data, Typeable)

data Reader = Reader { httpManager :: !(Maybe Manager)
                     , iterationsleft :: !(Maybe Int)
                     } deriving (Show)

instance Show Manager where show = const "(Manager)"

data Bot = Bot { socket        :: !Handle      -- ^ the bot's active IRC connection, or stdout indicating none
               , server        :: !String      -- ^ the IRC server's hostname or IP address, or "" indicating none
               , port          :: !Int         -- ^ the IRC server's port number
               , channel       :: !String      -- ^ the IRC channel to join
               , password      :: !(Maybe String)  -- ^ the IRC server's password
               , botnick       :: !String      -- ^ the bot's IRC nickname
               , announcequeue :: !(TChan String) -- ^ a shared queue of messages to be announced
               , batchindex    :: !Int         -- ^ how many announcements have been made in the current batch
               , lastmsgtime   :: !UTCTime     -- ^ the last time somebody spoke on the IRC channel
               }
instance Show Bot where
  show Bot{socket=s,server=srv,port=p,channel=c,botnick=n,lastmsgtime=t} =
    printf "Bot{botnick=%s, socket=%s, server=%s, port=%d, channel=%s, lastmsgtime=%s}"
      n (show s) srv p c (show t)

-- | rss2irc's application state, shared by all threads.
data App = App {aOpts   :: !Opts   -- ^ initial command-line options
               ,aReader :: !Reader -- ^ the feed reader's state
               ,aBot    :: !Bot    -- ^ the irc bot's state
               } deriving (Show)

type FeedAddress = String

data FeedParseException = FeedParseException String deriving (Typeable)
instance Exception FeedParseException
instance Show FeedParseException where show (FeedParseException url) = printf "could not parse feed content from %s" url

data IrcAddress  = IrcAddress {ircaddrServer  :: !String,
                               ircaddrPort    :: !(Maybe Int),
                               ircaddrPass    :: !(Maybe String),
                               ircaddrChannel :: !String,
                               ircaddrNick    :: !String
                              } deriving (Show)

data IrcException = IrcException String deriving (Typeable)
instance Exception IrcException
instance Show IrcException where show (IrcException msg) = printf "IRC error (%s)" msg

-- deriving instance Generic Item
-- instance NFData Item

io :: MonadIO m => IO a -> m a
io = liftIO
