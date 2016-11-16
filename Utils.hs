{-# LANGUAGE PatternGuards, BangPatterns, DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}
{- |

Common utilities.

Copyright (c) Don Stewart 2008-2009, Simon Michael 2009-2014
License: BSD3.

-}

module Utils (
  module Utils,
  module Debug.Trace
  )
where
import Codec.Binary.UTF8.String as UTF8 (decodeString, encodeString, isUTF8Encoded)
import Control.Concurrent.MSampleVar
import Control.Monad
import Data.List
import Data.Maybe
import Data.Time.Clock (UTCTime,getCurrentTime)
import Data.Time.Format (parseTimeM)
import Data.Time.LocalTime (LocalTime,getCurrentTimeZone,utcToLocalTime)
import Prelude hiding (log)
import System.Info
import System.IO (stdout,hFlush)
import Data.Time.Format (defaultTimeLocale)
import Text.Feed.Query
import Text.Feed.Types (Item)
import Text.ParserCombinators.Parsec hiding (label)
import Text.Printf (printf)
import Text.RegexPR (splitRegexPR,gsubRegexPR)

import Base

import Debug.Trace
-- | trace a showable expression
strace :: Show a => a -> a
strace a = trace (show a) a
-- | labelled trace - like strace, with a label prepended
ltrace :: Show a => String -> a -> a
ltrace l a = trace (l ++ ": " ++ show a) a
-- | monadic trace - like strace, but works as a standalone line in a monad
mtrace :: (Monad m, Show a) => a -> m a
mtrace a = strace a `seq` return a
-- | trace an expression using a custom show function
tracewith :: (a -> String) -> a -> a
tracewith f e = trace (f e) e


-- Light abstraction layer for thread-safe mutable data

type Shared a = MSampleVar a

newSharedVar :: a -> IO (MSampleVar a)
newSharedVar = newSV

getSharedVar :: MSampleVar a -> IO a
getSharedVar v = do
  x <- readSV v
  writeSV v x
  return x

putSharedVar :: MSampleVar a -> a -> IO ()
putSharedVar v x = writeSV v x

-- Option parsing helpers

ircAddressFromOpts :: Opts -> Maybe IrcAddress
ircAddressFromOpts Opts{irc_address=""} = Nothing
ircAddressFromOpts Opts{irc_address=a} = Just $ parseIrcAddress a

parseIrcAddress :: String -> IrcAddress
parseIrcAddress a =
  either (\e -> opterror $ printf "could not parse IRC address \"%s\"\n%s\n" a (show e))
         id
         $ parse ircaddrp "" a
  where
    ircaddrp = choice' $ [
      do
        -- pre 0.5 syntax: [irc://]NICK@IRCSERVER[:PORT]/[#]CHANNEL
        optional $ choice' $ map string ["irc://", "irc:"]
        n <- many1 $ noneOf "@"
        char '@'
        s <- many1 $ noneOf ":/"
        p <- optionMaybe $ char ':' >> read `fmap` many1 digit >>= return
        char '/'
        optional $ char '#'
        c <- many1 $ noneOf "/ \t"
        eof
        return $ IrcAddress s p Nothing ('#':c) n
      ,
      do
        -- magical new syntax with pwd
        -- [irc://]NICK:PWD@IRCSERVER:PORT/[#]CHANNEL
        optional $ choice' $ map string ["irc://", "irc:"]
        n <- many1 $ noneOf ":@"
        pwd <- optional (char ':' >> many1 (noneOf '@'))
        s <- many1 $ noneOf ':'
        p <- optionMaybe $ char ':' >> read `fmap` many1 digit >>= return
        char '/'
        optional $ char '#'
        c <- many1 $ noneOf "/ \t"
        eof
        return $ IrcAddress s p pwd ('#':c) n
      ,
      do
        -- new easier syntax: [irc://]IRCSERVER[:PORT]/[#]CHANNEL/NICK
        optional $ choice' $ map string ["irc://", "irc:"]
        s <- many1 $ noneOf ":/"
        p <- optionMaybe $ char ':' >> read `fmap` many1 digit >>= return
        char '/'
        optional $ char '#'
        c <- many1 $ noneOf "/"
        char '/'
        n <- many1 $ noneOf "/ \t"
        eof
        return $ IrcAddress s p Nothing ('#':c) n
      ]

-- | A version of error' that suggests --help.
opterror :: String -> a
opterror = error' . (++ " (see --help for usage)")

-- | A version of error that's better at displaying unicode.
error' :: String -> a
error' = error . toPlatformString

-- | Convert a feed item to a string for the bot to announce on irc.
-- The announcement is likely but not guaranteed to fit within a
-- single irc message.
toAnnouncement:: Opts -> Item -> String
toAnnouncement opts i = applyReplacements opts $ printf "%s%s%s%s%s" title desc author' date link'
    where
      title = unlessopt no_title $ maybe "" (truncateWordsAt maxtitlelength "..." . clean) (getItemTitle i)
      desc = ifopt description $ maybe "" ((" - "++) . truncateWordsAt maxdesclength "..." . clean) (getItemDescription i)
      author' = ifopt author $ maybe "" ((" "++) . parenthesise . truncateWordsAt maxauthorlength "..." . clean) (getItemAuthor i)
      date = ifopt time $ maybe "" ((" "++) . truncateAt maxdatelength "..." . clean) (getItemDate i)
      link' = ifopt link_ $ maybe "" (("  "++) . truncateAt maxlinklength "..." . clean) (getItemLink i)

      clean = oneline . trimwhitespace . striphtml . stripemail
      ifopt o = if o opts then id else const ""
      unlessopt o = if not $ o opts then id else const ""
      oneline = intercalate "  " . map strip . lines -- two spaces to hint at newlines & brs
      trimwhitespace = gsubRegexPR "[ \t][ \t]+" " "
      striphtml = if html opts then id else stripHtml . brtonewline
      brtonewline = gsubRegexPR "(<|&lt;) *br */?(>|&gt;)" "\n"
      stripemail = if email opts then id else stripEmails
      parenthesise = (++")").("("++)

-- | Split an announcement into one or more suitably truncated and
-- formatted irc messages. Each call returns the next message and
-- the remainder of the announcement.
-- XXX n must be > length continuationsuffix
splitAnnouncement :: String -> (String,String)
splitAnnouncement a
    | length a <= maxmessagelength = (a,"")
    | otherwise =
        case splitAtWordBefore n a of
          (m,rest@(_:_)) -> (m++continuationsuffix, continuationprefix++rest)
          (m,"")         -> (m, "")
    where
      n = maxmessagelength - length continuationsuffix

continuationprefix, continuationsuffix :: String
continuationprefix = "... "
continuationsuffix = " ..."

-- | Truncate a string, if possible at a word boundary, at or before
-- the specified position, and indicate truncation with the specified
-- suffix. The length of the returned string will be in the range
-- n, n+length suffix.
truncateWordsAt :: Int -> String -> String -> String
truncateWordsAt n suffix s
    | s' == s   = s
    | otherwise = s' ++ suffix
    where
      s' = fst $ splitAtWordBefore n s

-- | Truncate a string at the specified position, and indicate
-- truncation with the specified suffix. The length of the returned
-- string will be in the range n, n+length suffix.
truncateAt :: Int -> String -> String -> String
truncateAt n suffix s
    | s' == s   = s
    | otherwise = s' ++ suffix
    where
      s' = take n s

-- | Split a string at or before the specified position, on a word boundary if possible.
splitAtWordBefore :: Int -> String -> (String,String)
splitAtWordBefore n s
    | null a || (null b) = (rstrip a, lstrip b)
    | last a == ' ' || (head b == ' ') || (not $ ' ' `elem` a) = (rstrip a, lstrip b)
    | otherwise = (rstrip $ take (length a - length partialword) a, partialword ++ lstrip b)
    where (a,b) = splitAt n s
          partialword = reverse $ takeWhile (/= ' ') $ reverse a


-- | Apply all --replace substitutions to a string, in turn.
-- Warning, will fail at runtime if there is a bad regexp.
applyReplacements :: Opts -> String -> String
applyReplacements opts = foldl' (.) id (reverse substitutions)
    where substitutions = map replaceOptToSubst $ replace opts 
          replaceOptToSubst s = case splitRegexPR "(?<!\\\\)/" s of
                     (pat:sub:[]) -> gsubRegexPR pat sub
                     _ -> id

-- | Replace any HTML tags or entities in a string with a single space.
stripHtml :: String -> String
stripHtml = gsubRegexPR "(&[^ \t]*?;|<.*?>)" " "

-- | Remove any email addresses from a string.
stripEmails :: String -> String
stripEmails = gsubRegexPR "(?i) ?(<|&lt;)?\\b[-._%+a-z0-9]+@[-.a-z0-9]+\\.[a-z]{2,4}\\b(>|&gt;)?" ""

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, _)] -> Just x
    _        -> Nothing

decrementMaybe :: Enum a => Maybe a -> Maybe a
decrementMaybe = maybe Nothing (Just . pred)

-- | Parse a datetime string if possible, trying at least the formats
-- likely to be used in RSS/Atom feeds.
parseDateTime :: String -> Maybe UTCTime
parseDateTime s = firstJust [parseTimeM True defaultTimeLocale f s' | f <- formats]
    where
      s' = adaptForParseTime s
      adaptForParseTime = gsubRegexPR "(....-..-..T..:..:..[\\+\\-]..):(..)" "\\1\\2" -- 2009-09-22T13:10:56+00:00
      formats = -- http://hackage.haskell.org/packages/archive/time/1.1.4/doc/html/Data-Time-Format.html#v%3AformatTime
          [
           "%a, %d %b %Y %T %z" -- Fri, 18 Sep 2009 12:42:07 -0400
          ,"%a, %d %b %Y %T %Z" -- Fri, 25 Sep 2009 11:01:23 UTC
          ,"%Y-%m-%dT%T%z"      -- 2009-09-22T13:10:56+0000
          ]

firstJust :: [Maybe a] -> Maybe a
firstJust ms = case dropWhile isNothing ms of (m:_) -> m
                                              _     -> Nothing

-- | Grammatically correct "every N minutes".
everyMinutesString :: Int -> String
everyMinutesString 1 = "every minute"
everyMinutesString i = "every " ++ show i ++ " minutes"

-- | Grammatically correct "in N minutes".
inMinutesString :: Int -> String
inMinutesString 1 = "in 1 minute"
inMinutesString i = "in " ++ show i ++ " minutes"

-- | Log some text to the console with a timestamp.
log :: String -> IO ()
log s = do
  t <- getTimeStamp
  putStrLn $ printf "%s: %s" t s
  hFlush stdout

-- | Decorate some multi-line text with a label and start/end separators.
labelledText :: String -> String -> String
labelledText label s = printf "========== %s:\n%s\n=============================================\n" label s

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToLocalTime tz t

getTimeStamp :: IO String
getTimeStamp = do
  t <- getCurrentLocalTime
  tz <- getCurrentTimeZone
  return $ printf "%s %s" (take 19 $ show t) (show tz)

hours, minutes, seconds :: Int
hours = 60 * minutes
minutes = 60 * seconds
seconds = 10^(6::Int)

strip, lstrip, rstrip, dropws :: String -> String
strip = lstrip . rstrip
lstrip = dropws
rstrip = reverse . dropws . reverse
dropws = dropWhile (`elem` (" \t"::String))

chomp :: String -> String
chomp = reverse . dropWhile (`elem` ("\n\r"::String)) . reverse

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- platform strings

-- | A platform string is a string value from or for the operating system,
-- such as a file path or command-line argument (or environment variable's
-- name or value ?). On some platforms (such as unix) these are not real
-- unicode strings but have some encoding such as UTF-8. This alias does
-- no type enforcement but aids code clarity.
type PlatformString = String

-- | Convert a possibly encoded platform string to a real unicode string.
-- We decode the UTF-8 encoding recommended for unix systems
-- (cf http://www.dwheeler.com/essays/fixing-unix-linux-filenames.html)
-- and leave anything else unchanged.
fromPlatformString :: PlatformString -> String
fromPlatformString s = if UTF8.isUTF8Encoded s then UTF8.decodeString s else s

-- | Convert a unicode string to a possibly encoded platform string.
-- On unix we encode with the recommended UTF-8
-- (cf http://www.dwheeler.com/essays/fixing-unix-linux-filenames.html)
-- and elsewhere we leave it unchanged.
toPlatformString :: String -> PlatformString
toPlatformString = case os of
                     "unix" -> UTF8.encodeString
                     "linux" -> UTF8.encodeString
                     "darwin" -> UTF8.encodeString
                     _ -> id

-- | Backtracking choice, use this when alternatives share a prefix.
-- Consumes no input if all choices fail.
choice' :: [GenParser tok st a] -> GenParser tok st a
choice' = choice . map Text.ParserCombinators.Parsec.try

