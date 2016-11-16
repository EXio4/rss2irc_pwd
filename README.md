rss2irc is an IRC bot that polls an RSS or Atom feed and announces
updates to an IRC channel, with options for customizing output and
behavior.  It aims to be an easy-to-use, reliable, well-behaved bot.

Usage: `rss2irc [OPTIONS] FEEDURL [IRCSERVER[:PORT]/[#]CHANNEL/NICK]`

Example:

    $ rss2irc http://hackage.haskell.org/packages/archive/recent.rss irc.freenode.net/mychannel/mybot

Known limitations:

- If the feed goes down for a while and then comes back up, the bot may re-announce all items.

- Memory is leaked on each poll, causing rss2irc bots to consume more memory over time.
  This will be more noticeable when feeds have large content and are polled frequently.
  Restarting rss2irc bots daily is recommended.
