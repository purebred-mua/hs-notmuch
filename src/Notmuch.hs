-- This file is part of hs-notmuch - Haskell Notmuch binding
-- Copyright (C) 2014  Fraser Tweedale
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

messageHeader :: String -> Message -> IO String
messageHeader = flip message_get_header
