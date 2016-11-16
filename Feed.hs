{-# LANGUAGE PatternGuards, BangPatterns, DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}
{- |

Feed stuff.

Copyright (c) Don Stewart 2008-2009, Simon Michael 2009-2014
License: BSD3.

-}

module Feed where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Maybe
import Data.List
import System.IO.Storage
import Network.HTTP.Conduit
import Network.HTTP.Types (hCacheControl, hUserAgent)
import Network.URI
import Prelude hiding (log)
import Safe
import System.IO (stdout,hFlush)
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.Printf (printf)
import Text.RegexPR

import Base
import Utils


-- deriving instance Eq Item
instance Eq Item where
    (==) a b = let match f = f a == f b in
               all match [getItemTitle
                         ,getItemLink
                         ,getItemPublishDateString
                         ,getItemDate
                         ,getItemAuthor
                         ,getItemCommentLink
                         ,getItemFeedLink
                         ,getItemRights
                         ,getItemSummary
                         ,getItemDescription
                         ]
                 && match getItemCategories
                 && match getItemEnclosure
                 && match getItemId

-- | Poll the feed every interval minutes, ignoring transient IO
-- errors, detecting announceable items and sending them to the
-- announcer thread, forever or until the specified maximum number of
-- iterations is reached.
--
-- New item detection: this must be done carefully to avoid spamming
-- IRC users with useless messages. The content fetched from real-world
-- feeds may jitter due to http caching, unstable item ordering,
-- unpredictable or missing item dates, etc. We support several strategies:
--
-- - @topnew@: announce new unseen items at the top.
--   In more detail: assume that feeds provide items sorted newest first.
--   Then, announceable items are the new (newer pub date than the last
--   announced item) and unseen (id not among the last N ids seen since
--   startup) items at the top of the feed.  This is the default strategy,
--   best for most feeds.
--
-- - @allnew@: announce new unseen items appearing anywhere in the feed.
--   Good for feeds with unreliable item ordering, or to notice the items of
--   feeds newly added to a planet (aggregator).
--
-- - @top@: announce items appearing above the previous top item, new or not.
--   Good for feeds not ordered by date, eg a darcs repo's.

-- XXX none of these work for announcing recent-but-not-newest items from a blog added to a planet
feedReader :: Shared App -> IO ()
feedReader !appvar = do
  -- first poll - prime the pump
  app@App{aOpts=opts@Opts{feed=url}, aReader=Reader{httpManager=mmanager,iterationsleft=numleft}, aBot=Bot{announcequeue=q}} <- getSharedVar appvar
  case numleft of
   Just 0 -> return ()
   _ -> do
         unless (quiet opts) $ log $ printf "Polling %s %s" url (everyMinutesString $ interval opts)
         fetched <- fetchItems (fromJust mmanager) url opts
         let polls = 1
         -- with --recent N, send last N (non-duplicate) items to announcer thread
         let unique = (if allow_duplicates opts then id else (elideDuplicates [])) fetched
             announceable = take (recent opts) unique
             numannounced = fromIntegral $ length announceable
         writeList2Chan q $ map (toAnnouncement opts) $ reverse announceable
         when (debug_feed opts) $ logPoll fetched announceable polls numannounced
         -- start iterating
         let seen = map (\i -> (itemId i, fromMaybe "" $ getItemTitle i)) fetched
             lastpubdate = maybe Nothing getItemPublishDateString $ headMay unique
         putSharedVar appvar $ maybeDecrementIterationsLeft app
         feedReaderPoll appvar polls seen lastpubdate numannounced

feedReaderPoll :: Shared App -> Integer -> [(String,String)] -> Maybe String -> Integer -> IO ()
feedReaderPoll !appvar !polls !seen !lastpubdate !numannounced = do
  -- second & subsequent polls - wait interval then look for new items
  app@App{aOpts=opts@Opts{feed=url}, aReader=Reader{httpManager=mmanager,iterationsleft=numleft}, aBot=Bot{announcequeue=q}} <- getSharedVar appvar
  case numleft of
   Just 0 -> return ()
   _ -> do
         threadDelay $ (interval opts) * minutes
         when (debug_feed opts) $ log $ printf "polling %s" url
         fetched <- fetchItems (fromJust mmanager) url opts
         -- detect announceable items
         let seenids = map fst seen
             hasunseenid = (`notElem` seenids).itemId
             hasnewerdate = (`isNewerThan` lastpubdate).getItemPublishDateString
             isunseenandnewer i = hasnewerdate i && hasunseenid i
             isprevioustop = (== head seenids).itemId
             announceable = (if allow_duplicates opts then id else (elideDuplicates seen)) $
                            reverse $
                            (if ignore_ids_and_times opts then takeWhile (not . isprevioustop)
                                                          else filter isunseenandnewer) $
                            fetched
         -- send to announcer thread and iterate
         writeList2Chan q $ map (toAnnouncement opts) announceable
         let polls' = polls + 1
             seen' = take windowsize $ (map (\i -> (itemId i, fromMaybe "" $ getItemTitle i)) fetched)
                                       ++ seen where windowsize = 200
             lastpubdate' = maybe lastpubdate getItemPublishDateString $ headMay announceable
             numannounced' = numannounced + fromIntegral (length announceable)
         putSharedVar appvar $ maybeDecrementIterationsLeft app
         when (debug_feed opts) $ logPoll fetched announceable polls' numannounced'
         feedReaderPoll appvar polls' seen' lastpubdate' numannounced'

maybeDecrementIterationsLeft :: App -> App
maybeDecrementIterationsLeft app@App{aReader=reader@Reader{iterationsleft=n}} =
    app{aReader=reader{iterationsleft=decrementMaybe n}}

-- | Log debug info for a poll.
logPoll :: [Item] -> [Item] -> Integer -> Integer -> IO ()
logPoll fetched announceable polls numannounced = do
  printItemDetails "feed items, in feed order" fetched
  printItemDetails "announceable items, oldest first" announceable
  _ <- printf "successful consecutive polls, items announced: %10d %10d\n" polls numannounced
  hFlush stdout

-- | Fetch a feed's items, or the empty list in case of transient IO
-- errors (and log those).
fetchItems :: Manager -> FeedAddress -> Opts -> IO [Item]
fetchItems manager url opts =
  runResourceT (feedItems `fmap` readFeed manager url opts)
   `catches`
   [Handler $ \(e :: IOException) -> handleFetchError e
   ,Handler $ \(e :: HttpException) -> handleFetchError e
   ,Handler $ \(e :: FeedParseException) -> handleFetchError e
   ]
  where
    handleFetchError e =  do
      log $ printf "Error (%s), retrying %s" (show e) (inMinutesString $ interval opts)
      return []

-- | Fetch and parse a feed's content, or raise an exception.
readFeed :: Manager -> FeedAddress -> Opts -> ResourceT IO Feed
readFeed manager url opts = do
  s <- readUri manager url opts
  when (debug_xml opts) $ io $ log $ labelledText (printf "FEED CONTENT FROM %s " url) s
  case parseFeedString s of
    Nothing          -> io $ throwIO $ FeedParseException url
    Just (XMLFeed _) -> io $ throwIO $ FeedParseException url
    Just f           -> return f

-- | Fetch the contents of a uri, which must be an ascii string.
-- Redirects, authentication, https: and file: uris are supported.
readUri :: Manager -> String -> Opts -> ResourceT IO String
readUri manager s opts =
    case parseURI' s of
      Just URI{uriScheme="file:",uriPath=f} -> io $ readFeedFile f
      Just _ -> do
        -- LB8.unpack `fmap` simpleHttp s
        -- http-conduit is complex, cf https://github.com/snoyberg/http-conduit/issues/97
        r <- parseUrlThrow s
        let cachecontrol = cache_control opts
            r' | null cachecontrol = r
               | otherwise = r{requestHeaders=(hCacheControl, B8.pack cachecontrol):requestHeaders r}
            r'' = r'{requestHeaders=(hUserAgent, B8.pack $ uagent opts):requestHeaders r'}
        rsp <- httpLbs r'' manager
        return $ LB8.unpack $ responseBody rsp
      Nothing -> opterror $ "could not parse URI: " ++ s

-- | Parse a string to a URI, ensuring a simple filename is assigned the file: scheme.
parseURI' :: String -> Maybe URI
parseURI' s =
  case parseURIReference s of
    Just u  -> Just $ u `relativeTo` nullURI{uriScheme="file:",uriPath="."}
    Nothing -> Nothing

-- | A hacky stateful readFile to assist testing: this reads one or
-- more concatenated copies of the feed from the file and returns one
-- on each call, or the empty string when there are none left.
-- Reads from stdin if the file path is "-".
readFeedFile :: FilePath -> IO String
readFeedFile f = do
  v <- getValue "globals" "feedfile"
  case v of
    Nothing -> do
      s <- case f of "-" -> getContents
                     _   -> readFile f
      let (first:rest) = splitFeedCopies s
      putValue "globals" "feedfile" rest
      return first
    Just (first:rest) -> do
      putValue "globals" "feedfile" rest
      return first
    Just [] -> return ""
  where
    splitFeedCopies = initDef [""] . map (++"</feed>\n") . splitRegexPR "(?i)</(feed|rdf:RDF)\n? *>\n*"

-- | Check if the first date is newer than the second, where dates (from
-- feed items) can be Nothing, a parseable date string or unparseable.  In
-- the (likely) event we can't parse two dates, return True.
isNewerThan :: Maybe String -> Maybe String -> Bool
isNewerThan _ Nothing = True
isNewerThan Nothing _ = True
isNewerThan (Just s2) (Just s1) =
    case (parseDateTime s2, parseDateTime s1) of
      (Just d2, Just d1) -> d2 > d1
      _ -> True

-- | Remove any items from the list which duplicate another item in
-- this or the second list (the last N fetched items), where
-- "duplicates" means "would generate a similar irc message", ie it
-- has the same item title. This is a final de-duplication pass before
-- announcing on irc.
elideDuplicates :: [(String,String)] -> [Item] -> [Item]
elideDuplicates seen new =
  filter (\a -> not $ fromMaybe "" (getItemTitle a) `elem` seentitles) $
  nubBy (\a b -> getItemTitle a == getItemTitle b)
  new
    where
      seentitles = map snd seen

-- | Get the best available unique identifier for a feed item.
itemId :: Item -> String
itemId i = case getItemId i of 
             Just (_,s) -> s
             Nothing    -> case getItemTitle i of
                             Just s  -> s
                             Nothing -> case getItemDate i of
                                          Just s  -> s
                                          Nothing -> show i

-- | Dump item details to the console for debugging.
printItemDetails :: String -> [Item] -> IO ()
printItemDetails hdr is = printf "%s: %d\n%s" hdr count items >> hFlush stdout
    where
      items = unlines [printf " %-29s%s  %-*s" d p twidth t | (d,p,t,_) <- fields]
      twidth = maximum $ map (length.fromMaybe "".getItemTitle) is
      -- subhdr = "(date, (publish date if different), title)\n"
      -- subhdr' = if null is then "" else subhdr
      count = length is
      fields = [(d, if p==d then "" :: String else printf "  pubdate:%-29s" p, t, i) | item <- is
               ,let d = fromMaybe "" $ getItemDate item
               ,let p = fromMaybe "" $ getItemPublishDateString item
               ,let t = fromMaybe "" $ getItemTitle item
               ,let i = maybe "" show $ getItemId item
               ]

writeList2Chan :: TChan a -> [a] -> IO ()
writeList2Chan q as = do
  atomically $ forM as $ \a -> writeTChan q a
  return ()
