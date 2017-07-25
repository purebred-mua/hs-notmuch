-- This file is part of hs-notmuch - Haskell Notmuch binding
-- Copyright (C) 2014, 2017  Fraser Tweedale
--
-- hs-notmuch is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-|

High-level interface to the /notmuch/ mail indexer.

Some functions that operate on 'Message' objects cause a file
descriptor to be opened (indicated below).  The file descriptor is
automatically closed when the data gets GC'd but when the RTS is
using a multi-generation collector (the default) it becomes more
likely to hit /max open files/ limits.  Approaches to avoid this
scenario include:

- Avoid using these functions; if you need to open the mail file
  from Haskell and close it promptly.

- Use a single-generation collector (build with @-rtsopts@ and run
  with @+RTS -G1@).  This incurs the cost of scanning the entire
  heap on every GC run.

- In an interactive program, build with @-threaded@ to enable
  parallel GC.  By default, major GC will be triggered when the
  program is idle for a certain time.

- Manually execute 'System.Mem.performMajorGC' at appropriate times
  to ensure that older generations get cleaned up.

The functions that may open file descriptors are:

- 'messageHeader'

-}
module Notmuch
  (
    Tag
  , MessageId
  , ThreadId

  , Database
  , databaseOpen
  , databaseVersion

  , Query
  , query
  , queryCountMessages
  , queryCountThreads

  , Thread

  , Message
  , messageId
  , messageHeader
  , messageFilename

  , HasTags(..)
  , HasMessages(..)
  , HasThreads(..)
  ) where

import Notmuch.Binding
import Notmuch.Search

--
-- PUBLIC API
--

--
-- Classes
--

class HasTags a where
  tags :: a -> IO [Tag]

instance HasTags Database where
  tags = database_get_all_tags

instance HasTags Thread where
  tags = thread_get_tags

instance HasTags Messages where
  tags = messages_collect_tags

instance HasTags Message where
  tags = message_get_tags


class HasMessages a where
  messages :: a -> IO [Message]

instance HasMessages Query where
  messages = query_search_messages

instance HasMessages Thread where
  messages = thread_get_messages

instance HasMessages Message where
  messages = message_get_replies
  -- replies!


class HasThreads a where
  threads :: a -> IO [Thread]


class HasThread a where
  threadId :: a -> IO ThreadId

instance HasThread Thread where
  threadId = thread_get_thread_id

instance HasThread Message where
  threadId = message_get_thread_id


databaseOpen :: String -> IO (Either Status Database)
databaseOpen = database_open

databaseVersion :: Database -> IO Int
databaseVersion = database_get_version

query :: Database -> SearchTerm -> IO Query
query db = query_create db . show

queryCountMessages :: Query -> IO Int
queryCountMessages = query_count_messages

queryCountThreads :: Query -> IO Int
queryCountThreads = query_count_threads

messageId :: Message -> IO String
messageId = message_get_message_id

-- | Get the named header.  Empty string if header is
-- missing or 'Nothing' on error.
--
-- /May open a file descriptor./
--
messageHeader :: String -> Message -> IO (Maybe String)
messageHeader = flip message_get_header

messageFilename :: Message -> IO String
messageFilename = message_get_filename
