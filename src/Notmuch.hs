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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

{-|

High-level interface to the /notmuch/ mail indexer.

Example program to add/remove a tag on all messages matching a query:

@
main :: IO ()
main = getArgs >>= \\args -> case args of
  [path, expr, \'+\':tag] -> go path expr tag 'messageAddTag'
  [path, expr, \'-\':tag] -> go path expr tag 'messageRemoveTag'
  _ -> 'die' "usage: hs-notmuch-tag-set DB-DIR SEARCH-TERM +TAG|-TAG"
  where
    go path expr tag f =
      'runExceptT' (do
        db <- 'databaseOpen' path
        'query' db ('Bare' expr) >>= 'messages' >>= traverse_ (f ('fromString' tag))
      ) >>= either (die . (show :: 'Status' -> String)) pure
@

== File descriptor exhaustion

Some 'Message' operations cause the message file to be opened (and
remain open until the object gets garbage collected):

- 'messageHeader' will open the file to read the headers, except for the
  @From@, @Subject@ and @Message-Id@ headers which are indexed.

If the RTS is using a multi-generation collector (the default), and if
you are working with lots of messages, you may hit /max open files/
limits.  The best way to avoid this is to avoid the scenarios outlined
above.  Alternative approaches that could help include:

- Use a single-generation collector (build with @-rtsopts@ and run
  with @+RTS -G1@).  This incurs the cost of scanning the entire
  heap on every GC run.

- In an interactive program, build with @-threaded@ to enable
  parallel GC.  By default, major GC will be triggered when the
  program is idle for a certain time.

- Manually execute 'System.Mem.performMajorGC' at relevant times
  to ensure that older generations get cleaned up.

-}
module Notmuch
  (
  -- * Opening a database
    databaseOpen
  , databaseOpenReadOnly
  , databasePath
  , databaseVersion
  , databaseRevision
  , Database
  -- ** Database modes
  , Mode
  , DatabaseMode(..)
  , RO
  , RW

  -- * Querying the database
  , Query
  , query
  , queryCountMessages
  , queryCountThreads

  -- ** Search expressions
  , SearchTerm(..)

  -- * Working with threads
  , HasThread(..)
  , Thread
  , threadToplevelMessages
  , threadNewestDate
  , threadSubject
  , threadAuthors
  , threadTotalMessages

  -- ** Thread ID
  , ThreadId
  , HasThreads(..)

  -- ** Thread authors
  , ThreadAuthors
  , Author
  , matchedAuthors
  , unmatchedAuthors

  -- * Working with messages
  , findMessage
  , HasMessages(..)
  , Message
  -- ** Headers
  , MessageId
  , messageId
  , messageDate
  , messageHeader
  -- ** Tags
  , messageSetTags
  , messageAddTag
  , messageRemoveTag
  , withFrozenMessage
  -- ** Files
  , messageFilename
  , indexFile
  , removeFile
  , RemoveResult(..)

  -- * Tags
  , HasTags(..)
  , Tag
  , mkTag
  , getTag
  , tagMaxLen

  -- * Errors
  , Status(..)
  , AsNotmuchError(..)

  -- * Library information
  , libnotmuchVersion
  ) where

import Control.Exception (bracket)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (traverse_)
import Foreign.C.Types (CULong)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Notmuch.Tag
import Notmuch.Binding
import Notmuch.Binding.Constants (libnotmuchVersion)
import Notmuch.Search
import Notmuch.Util

-- | Objects with tags
class HasTags a where
  tags :: MonadIO m => a -> m [Tag]

-- | Get all tags used in the database
instance HasTags (Database a) where
  tags = liftIO . database_get_all_tags

-- | Get all tags used in a thread
instance HasTags (Thread a) where
  tags = liftIO . thread_get_tags

-- | Get the tags of a single message
instance HasTags (Message n a) where
  tags = liftIO . message_get_tags


-- | Objects with associated messages.
class HasMessages a where
  messages
    :: (AsNotmuchError e, MonadError e m, MonadIO m)
    => a mode -> m [Message 0 mode]

-- | Retrieve all messages matching a 'Query'
instance HasMessages Query where
  messages = query_search_messages

-- | Retrieve the messages in a 'Thread'
instance HasMessages Thread where
  messages = thread_get_messages

-- | Retrieve the replies to a 'Message'
instance HasMessages (Message n) where
  messages = message_get_replies

-- | Objects with associated threads
class HasThreads a where
  threads
    :: (AsNotmuchError e, MonadError e m, MonadIO m)
    => a mode -> m [Thread mode]

-- | Retrieve the threads matching a 'Query'
instance HasThreads Query where
  threads = query_search_threads

-- | Objects with an associated 'ThreadId'
class HasThread a where
  threadId :: MonadIO m => a -> m ThreadId

-- | Get the 'ThreadId' of a 'Thread'
instance HasThread (Thread a) where
  threadId = liftIO . thread_get_thread_id

-- | Get the 'ThreadId' of a 'Message'
instance HasThread (Message n a) where
  threadId = liftIO . message_get_thread_id

-- | Open a database.  The database will be closed and associated
-- resources freed upon garbage collection.
--
-- The mode is determined by usage.  Because read-only functions
-- also work on read-write databases, 'databaseOpenReadOnly' is also
-- provided for convenience.
--
databaseOpen
  :: (Mode a, AsNotmuchError e, MonadError e m, MonadIO m)
  => FilePath -> m (Database a)
databaseOpen = database_open

-- | Convenience function for opening a database read-only
databaseOpenReadOnly
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
   => FilePath -> m (Database RO)
databaseOpenReadOnly = database_open

-- | Database format version of the given database.
databaseVersion :: MonadIO m => Database a -> m Int
databaseVersion = liftIO . database_get_version

-- | Get the revision and UUID of the database.
--
-- The revision number increases monotonically with each commit to
-- the database (although rollover is possible).  The "UUID" is an
-- __opaque__ string that persists until e.g. database compaction.
-- Revision numbers are only comparable where the UUID strings are
-- equal.
--
databaseRevision :: MonadIO m => Database a -> m (CULong, String)
databaseRevision = liftIO . database_get_revision

-- | Look for a particular message in the database.
findMessage
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
  => Database a -> MessageId -> m (Maybe (Message 0 a))
findMessage = database_find_message

-- | Query the database.  To retrieve results from a @Query@, use
-- 'threads' or 'messages'.
--
query :: (MonadIO m) => Database a -> SearchTerm -> m (Query a)
query db = liftIO . query_create db . show

-- | Count the number of messages matching a query.
--
-- Complexity: same as the underlying Xapian search…
--
queryCountMessages
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
  => Query a -> m Int
queryCountMessages = query_count_messages

-- | Count the number of threads matching a query.
--
-- __/Θ(n)/ in the number of messages__!
queryCountThreads
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
  => Query a -> m Int
queryCountThreads = query_count_threads

-- | Get the message ID.
messageId :: MonadIO m => Message n a -> m MessageId
messageId = liftIO . message_get_message_id

-- | Get the date the message was sent.
messageDate :: MonadIO m => Message n a -> m UTCTime
messageDate = liftIO . fmap (posixSecondsToUTCTime . realToFrac) . message_get_date

-- | Get the named header as a UTF-8 encoded string.
-- Empty string if header is missing or @Nothing@ on error.
--
-- __May open a file descriptor__ that will not be closed until the
-- message gets garbage collected.
--
messageHeader :: MonadIO m => B.ByteString -> Message n a -> m (Maybe B.ByteString)
messageHeader k = liftIO . flip message_get_header k

-- | Get the filename of the message.
messageFilename :: MonadIO m => Message n a -> m FilePath
messageFilename = liftIO . message_get_filename

-- | Freeze the message, run the given computation
-- and return the result.  The message is always thawed at the end.
--
-- Have to start with @Message 0 RW@ due to GHC type system limitation
-- (type-level Nat is not inductive).
--
withFrozenMessage :: (forall n. Message n RW -> IO a) -> Message 0 RW -> IO a
withFrozenMessage k msg = bracket (message_freeze msg) message_thaw k

-- | Set tags for the message.  Atomic.
--
messageSetTags :: (MonadIO m, Foldable t) => t Tag -> Message 0 RW -> m ()
messageSetTags l = liftIO . withFrozenMessage (\msg ->
  message_remove_all_tags msg *> traverse_ (message_add_tag msg) l)

-- | Add the tag to a message.  If adding/removing multiple tags,
-- use 'messageSetTags' to set the whole tag list atomically, or use
-- 'withFrozenMessage' to avoid inconsistent states when
-- adding/removing tags.
--
messageAddTag :: (MonadIO m) => Tag -> Message n RW -> m ()
messageAddTag tag msg = liftIO $ message_add_tag msg tag

-- | Remove the tag from a message.  If adding/removing multiple
-- tags, use 'messageSetTags' to set the whole tag list atomically,
-- or use 'withFrozenMessage' to avoid inconsistent states when
-- adding/removing tags.
--
messageRemoveTag :: (MonadIO m) => Tag -> Message n RW -> m ()
messageRemoveTag tag msg = liftIO $ message_remove_tag msg tag

-- | Returns only messages in a thread which are not replies to other messages in the thread.
threadToplevelMessages :: (MonadIO m) => Thread a -> m [Message 0 a]
threadToplevelMessages = thread_get_toplevel_messages

-- | /O(1)/ Date of the newest message in a 'Thread'.
threadNewestDate :: MonadIO m => Thread a -> m UTCTime
threadNewestDate = liftIO . fmap (posixSecondsToUTCTime . realToFrac) . thread_get_newest_date

-- | Returns the subject of the first message in the query results that belongs to this thread.
threadSubject :: MonadIO m => Thread a -> m B.ByteString
threadSubject = liftIO . thread_get_subject

-- | Author of a message.
type Author = T.Text

-- | Authors belonging to messages in a query result of a thread ordered by date.
data ThreadAuthors = ThreadAuthors
    { _matchedAuthors :: [Author]
    -- ^ authors matching the query
    , _unmatchedAuthors :: [Author]
    -- ^ non-matched authors
    } deriving (Show, Generic, NFData)

-- | Lens to matched authors.  See also 'threadAuthors'.
matchedAuthors :: Lens' ThreadAuthors [Author]
matchedAuthors f (ThreadAuthors a b) = fmap (\a' -> ThreadAuthors a' b) (f a)
{-# ANN matchedAuthors ("HLint: ignore Avoid lambda using `infix`" :: String) #-}

-- | Lens to unmatched authors.  See also 'threadAuthors'.
unmatchedAuthors :: Lens' ThreadAuthors [Author]
unmatchedAuthors f (ThreadAuthors a b) = fmap (\b' -> ThreadAuthors a b') (f b)
{-# ANN unmatchedAuthors ("HLint: ignore Avoid lambda" :: String) #-}

-- | Return authors of a thread.  These are split into:
--
-- * Authors of messages matching the query (accessible via 'matchedAuthors').
-- * Authors of non-matching messages (accessible via 'unmatchedAuthors').
--
threadAuthors :: MonadIO m => Thread a -> m ThreadAuthors
threadAuthors t = do
  a <- liftIO $ thread_get_authors t
  pure $ maybe (ThreadAuthors [] []) (convertAuthors . T.decodeUtf8) a

convertAuthors :: T.Text -> ThreadAuthors
convertAuthors raw =
  let t = T.breakOn (T.pack "|") raw
      matched = T.strip <$> T.splitOn (T.pack ",") (fst t)
      unmatched = filter (not . T.null) (T.splitOn (T.pack "|") $ snd t)
  in ThreadAuthors matched unmatched

-- | /O(1)/ count of messages in the thread.
threadTotalMessages :: MonadIO m => Thread a -> m Int
threadTotalMessages = liftIO . thread_get_total_messages
