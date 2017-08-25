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

- Avoid using these functions; if you need to open the mail file,
  open it from Haskell, do the thing, then close it promptly.

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
  , RO
  , RW
  , databaseOpen
  , databaseVersion
  , findMessage

  , Query
  , query
  , queryCountMessages
  , queryCountThreads

  , Thread

  , Message
  , messageId
  , messageDate
  , messageHeader
  , messageFilename

  , HasTags(..)
  , HasMessages(..)
  , HasThreads(..)
  , HasThread(..)
  ) where

import qualified Data.ByteString as B
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

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

instance HasTags (Database a) where
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


databaseOpen :: FilePath -> IO (Either Status (Database RO))
databaseOpen = database_open

databaseVersion :: Database a -> IO Int
databaseVersion = database_get_version

findMessage :: Database a -> MessageId -> IO (Either Status (Maybe Message))
findMessage = database_find_message

query :: Database a -> SearchTerm -> IO Query
query db = query_create db . show

queryCountMessages :: Query -> IO Int
queryCountMessages = query_count_messages

queryCountThreads :: Query -> IO Int
queryCountThreads = query_count_threads

messageId :: Message -> IO MessageId
messageId = message_get_message_id

messageDate :: Message -> IO (UTCTime)
messageDate = fmap (posixSecondsToUTCTime . realToFrac) . message_get_date

-- | Get the named header as a UTF-8 encoded string.
-- Empty string if header is missing or @Nothing@ on error.
--
-- /May open a file descriptor./
--
messageHeader :: B.ByteString -> Message -> IO (Maybe B.ByteString)
messageHeader = flip message_get_header

messageFilename :: Message -> IO FilePath
messageFilename = message_get_filename
