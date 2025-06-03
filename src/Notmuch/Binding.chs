-- Copyright (C) 2014-2019  Fraser Tweedale
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Notmuch.Binding where

import Control.Monad ((>=>), (<=<), void, when)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.List (isPrefixOf)
import Data.Proxy
import GHC.TypeLits (Nat, type (<=), type (+), type (-))
import System.FilePath (addTrailingPathSeparator, isAbsolute, splitPath)

import Notmuch.Tag
import Notmuch.Util

#include <notmuch.h>
{#context prefix = "notmuch" #}

import Foreign
  ( ForeignPtr, Ptr, castForeignPtr
  , alloca, newForeignPtr, nullFunPtr, nullPtr, peek
  , touchForeignPtr
  )
import Foreign.C
import Foreign.Storable (Storable)
import qualified System.IO.Unsafe

import qualified Data.ByteString as B

import Notmuch.Talloc


-- | @Message-Id@ header value.
type MessageId = B.ByteString

-- | Thread identifier generated and used by /libnotmuch/.
type ThreadId = B.ByteString

--
-- BINDING API
--

{#enum status_t as Status {underscoreToCase} deriving (Eq) #}
{#enum database_mode_t as DatabaseMode {underscoreToCase} #}
{#enum sort_t as Sort {underscoreToCase} #}
{#enum message_flag_t as MessageFlag {underscoreToCase} #}
{#pointer *database_t as DatabaseHandle foreign finalizer database_destroy newtype #}
{#pointer *query_t as QueryHandle foreign finalizer query_destroy newtype #}
{#pointer *threads_t as Threads newtype #}
{#pointer *thread_t as ThreadHandle foreign finalizer thread_destroy newtype #}
{#pointer *messages_t as Messages newtype #}
{#pointer *message_t as MessageHandle foreign finalizer message_destroy newtype #}
{#pointer *tags_t as Tags newtype #}
{#pointer *directory_t as Directory foreign newtype #}
{#pointer *filenames_t as Filenames foreign newtype #}

deriving instance Storable Threads
deriving instance Storable Messages

instance Show Status where
  show a = System.IO.Unsafe.unsafePerformIO $
    {#call unsafe status_to_string #} (fromEnum' a) >>= peekCString

-- | Classy prism for injecting a /libnotmuch/ status code.
class AsNotmuchError s where
  _NotmuchError :: Prism' s Status

instance AsNotmuchError Status where
  _NotmuchError = id

throwOr :: (AsNotmuchError e, MonadError e m) => (a -> m b) -> Either Status a -> m b
throwOr = either (throwError . review _NotmuchError)

-- | Convenience synonym for the promoted 'DatabaseModeReadOnly' constructor.
type RO = 'DatabaseModeReadOnly

-- | Convenience synonym for the promoted 'DatabaseModeReadWrite' constructor.
type RW = 'DatabaseModeReadWrite

-- | A database handle.  The database will be closed and freed when
-- it is garbage collected.
--
-- Use 'query' to perform a search or 'findMessage' to search for a
-- particular message.
--
-- The @Database@ type carries a phantom for the database mode, which
-- is propgated to derived 'Query', 'Thread' and 'Message' objects.
-- This is used to prevent write operations being performed against
-- a read-only database.
--
newtype Database (a :: DatabaseMode) = Database DatabaseHandle

withDatabase :: Database a -> (Ptr DatabaseHandle -> IO b) -> IO b
withDatabase (Database dbh) = withDatabaseHandle dbh

-- | Message object.  Cleaned up when garbage collected.
--
-- The @Message@ type carries a phantom for the database mode, so that
-- write operations are restricted to read/write database sessions.
--
-- There is also a phantom type parameter for the degree of frozenness
-- of the message.  Tag operations on a frozen message are atomic, only
-- becoming visible to other threads/processes after the thaw.  The
-- freeze/thaw behaviour is available via 'withFrozenMessage'.
--
data Message (n :: Nat) (a :: DatabaseMode) = Message
                 ![ForeignPtr () {- owners -}]
  {-# UNPACK #-} !MessageHandle

withMessage :: Message n a -> (Ptr MessageHandle -> IO b) -> IO b
withMessage (Message owners a) k = do
  r <- withMessageHandle a k
  traverse_ touchForeignPtr owners
  pure r

-- | Thread object.  Cleaned up when garbage collected.
--
-- Use 'messages' to get the messages of a thread.
--
-- The @Thread@ type carries a phantom for the database mode, so that
-- write operations on messages derived from it are restricted to
-- read/write database sessions.
--
data Thread (a :: DatabaseMode) = Thread
  {-# UNPACK #-} !(ForeignPtr DatabaseHandle {- owner -})
  {-# UNPACK #-} !(ForeignPtr QueryHandle {- owner -})
  {-# UNPACK #-} !ThreadHandle

withThread :: Thread a -> (Ptr ThreadHandle -> IO b) -> IO b
withThread (Thread dfp qfp a) k = do
  r <- withThreadHandle a k
  touchForeignPtr dfp
  touchForeignPtr qfp
  pure r

-- | Query object.  Cleaned up when garbage collected.
--
-- Use 'messages' or 'threads' to get the results.
--
-- The @Query@ type carries a phantom for the database mode, so that
-- write operations on messages derived from it are restricted to
-- read/write database sessions.
--
data Query (a :: DatabaseMode) = Query
  {-# UNPACK #-} !(ForeignPtr DatabaseHandle)
  {-# UNPACK #-} !QueryHandle

withQuery :: Query a -> (Ptr QueryHandle -> IO b) -> IO b
withQuery (Query owner a) f = do
  r <- withQueryHandle a f
  touchForeignPtr owner
  pure r

fromEnum' :: (Enum a, Integral b) => a -> b
fromEnum' = fromIntegral . fromEnum

-- | If @e == StatusSuccess@ then apply @Right@ in the given effect,
-- otherwise return @pure (Left e)@
--
status :: (Applicative f) => f a -> Status -> f (Either Status a)
status = status' (== StatusSuccess)

-- | Like 'status'' but with predicate parameter (False if status
-- should be raised as an error, otherwise True).
status'
  :: (Applicative f)
  => (Status -> Bool) -> f a -> Status -> f (Either Status a)
status' f a e
  | f e = Right <$> a
  | otherwise = pure $ Left e

toStatus :: (Integral a, Enum b) => a -> b
toStatus = toEnum . fromIntegral


-- | This is an internal class whose instances are the promoted
-- 'DatabaseMode' constructors.
class Mode a where
  getMode :: Proxy a -> DatabaseMode
  upgrade :: (AsNotmuchError e, MonadError e m, MonadIO m) => Database a -> m (Database a)

instance Mode 'DatabaseModeReadOnly where
  getMode _ = DatabaseModeReadOnly
  upgrade = pure

instance Mode 'DatabaseModeReadWrite where
  getMode _ = DatabaseModeReadWrite
  upgrade db =
    liftIO (toEnum . fromIntegral <$> withDatabase db (\dbPtr ->
      {#call unsafe database_upgrade #} dbPtr nullFunPtr nullPtr))
    >>= \rv -> case rv of
      StatusSuccess -> pure db
      e -> throwError $ review _NotmuchError e

-- | Open a Notmuch database
--
-- The database will be closed and resources freed when it gets
-- garbage collected.
--
database_open
  :: forall a e m. (AsNotmuchError e, Mode a, MonadError e m, MonadIO m)
  => FilePath
  -> m (Database a)
database_open s =
  liftIO (
    withCString s (\s' -> (fmap . fmap) runIdentity $
      constructF
        Identity
        (fmap (Database . DatabaseHandle) . newForeignPtr notmuch_database_destroy)
        ({#call database_open #} s' (fromEnum' (getMode (Proxy :: Proxy a))))
    ))
  >>= throwOr upgrade

-- notmuch_status_t notmuch_database_compact(path, backup_path, status_cb, closure)

database_get_path :: Database a -> IO FilePath
database_get_path db =
  withDatabase db {#call unsafe database_get_path #} >>= peekCString

-- | Get the path of the database
databasePath :: Database a -> FilePath
databasePath = System.IO.Unsafe.unsafePerformIO . database_get_path

database_get_version :: Database a -> IO Int
database_get_version db =
  fromIntegral <$> withDatabase db {#call unsafe database_get_version #}

-- | Index a file with the default indexing options.
-- (This binding does not yet provide a way to change
-- the indexing options.)  Returns the indexed message.
--
-- If message has same message ID as another message in the
-- database, the new filename will be added to the message
-- and the existing message is returned.
--
-- Possible errors include:
--
-- * 'StatusPathError' if file path is not absolute or is not an
--    extension of the database path.  This check is performed in
--    this binding, not in the foreign /libnotmuch/ code.
-- * 'StatusFileError' when file does not exist or cannot be opened
-- * 'StatusFileNotEmail' when file does not look like an email
--
indexFile
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
  => Database RW -> FilePath -> m (Message 0 RW)
indexFile db@(Database (DatabaseHandle dfp)) path =
  when (not validPath) (throwError (review _NotmuchError StatusPathError))
  *> liftIO (withDatabase db $ \db' ->
    withCString path $ \path' ->
      constructF'
        (\r -> r == StatusSuccess || r == StatusDuplicateMessageId)
        Identity
        (fmap (Message [castForeignPtr dfp] . MessageHandle)
          . (newForeignPtr notmuch_message_destroy <=< detachPtr))
        ({#call unsafe database_index_file #} db' path' nullPtr)
  )
  >>= throwOr (pure . runIdentity)
  where
    validPath =
      isAbsolute path
      && splitPath (addTrailingPathSeparator (databasePath db))
          `isPrefixOf` splitPath path

-- | Result of a 'removeFile' operation.
data RemoveResult = MessageRemoved | MessagePersists
  deriving (Eq, Show)

-- | Remove a message filename.  If the message has no more
-- filenames return 'MessageRemoved', otherwise 'MessagePersists'.
--
-- The underlying routine (as of notmuch v0.28) returns
-- @NOTMUCH_STATUS_SUCCESS@ even when the given path does not
-- exist, is not an internet message, or is not recorded in the
-- database.  Therefore @removeFile@ also returns 'MessageRemoved'
-- in this scenario.  This is particularly confusing when the
-- @Message-Id@ of the given file is known, but the the file itself
-- is unknown.
--
removeFile
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
  => Database RW -> FilePath -> m RemoveResult
removeFile db path =
  liftIO (withDatabase db $ \db' ->
    withCString path $ \path' ->
      toStatus <$> {#call unsafe database_remove_message #} db' path'
  )
  >>= \e -> case e of
    StatusSuccess -> pure MessageRemoved
    StatusDuplicateMessageId -> pure MessagePersists
    _ -> throwError $ review _NotmuchError e

-- notmuch_database_needs_upgrade ## do automatically for updates
-- notmuch_database_upgrade ## do automatically for updates
-- notmuch_database_begin_atomic ## do automatically for updates
-- notmuch_database_end_atomic ## do automatically for updates

-- notmuch_database_get_directory

database_find_message
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
  => Database a
  -> MessageId
  -> m (Maybe (Message 0 a))
database_find_message =
  database_find_message_x B.useAsCString {#call unsafe database_find_message #}

database_find_message_by_filename
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
  => Database a -- ^ Database
  -> FilePath   -- ^ Filename
  -> m (Maybe (Message 0 a))
database_find_message_by_filename =
  database_find_message_x withCString {#call unsafe database_find_message_by_filename #}

database_find_message_x
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
  => (s -> (s' -> IO (Either Status (Maybe (Message 0 a)))) -> IO (Either Status (Maybe (Message 0 a))))
  -> (Ptr DatabaseHandle -> s' -> Ptr (Ptr MessageHandle) -> IO CInt)
  -> Database a -- ^ Database
  -> s
  -> m (Maybe (Message 0 a))
database_find_message_x prep f db@(Database (DatabaseHandle dfp)) s =
  liftIO (withDatabase db $ \db' ->
    prep s $ \s' ->
      constructF
        (\ptr -> if ptr /= nullPtr then Just ptr else Nothing)
        (fmap (Message [castForeignPtr dfp] . MessageHandle)
          . (newForeignPtr notmuch_message_destroy <=< detachPtr))
        (f db' s')
  )
  >>= throwOr pure

-- Tags are always copied from the libnotmuch-managed memory,
-- and all the copying takes place under 'withDatabase', so
-- we don't need to detach the pointer or use a ForeignPtr.
--
-- TODO: check for NULL, indicating error
--
database_get_all_tags :: Database a -> IO [Tag]
database_get_all_tags ptr = withDatabase ptr $ \ptr' ->
  {#call unsafe database_get_all_tags #} ptr' >>= tagsToList

-- TODO: check for NULL, indicating error
query_create :: Database a -> String -> IO (Query a)
query_create db@(Database (DatabaseHandle dfp)) s = withCString s $ \s' ->
  withDatabase db $ \db' ->
    {#call unsafe notmuch_query_create #} db' s'
      >>= detachPtr
      >>= fmap (Query dfp . QueryHandle) . newForeignPtr notmuch_query_destroy

query_get_query_string :: Query a -> IO String
query_get_query_string ptr =
  withQuery ptr ({#call unsafe query_get_query_string #} >=> peekCString)

query_set_sort :: Query a -> Sort -> IO ()
query_set_sort ptr x = withQuery ptr $ \ptr' ->
  {#call unsafe query_set_sort #} ptr' (fromEnum' x)

query_get_sort :: Query a -> IO Sort
query_get_sort ptr = withQuery ptr $
  fmap (toEnum . fromIntegral) . {#call unsafe query_get_sort #}

query_add_tag_exclude :: Query a -> Tag -> IO ()
query_add_tag_exclude ptr tag = void $
  withQuery ptr $ \ptr' ->
    tagUseAsCString tag $ \s' ->
      {#call unsafe query_add_tag_exclude #} ptr' s'

query_search_threads
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
  => Query a -> m [Thread a]
query_search_threads q@(Query dfp (QueryHandle qfp)) =
  liftIO ( withQuery q $ \qPtr ->
    constructF
      Identity
      (threadsToList dfp qfp)
      ({#call unsafe query_search_threads #} qPtr)
  )
  >>= throwOr (pure . runIdentity)

query_search_messages
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
  => Query a -> m [Message 0 a]
query_search_messages q@(Query dfp (QueryHandle qfp)) =
  liftIO ( withQuery q $ \qPtr ->
    constructF
      Identity
      (messagesToList [castForeignPtr dfp, castForeignPtr qfp])
      ({#call unsafe query_search_messages #} qPtr)
  )
  >>= throwOr (pure . runIdentity)

query_count_x
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
  => (Ptr QueryHandle -> Ptr CUInt -> IO CInt)
  -> Query a -> m Int
query_count_x f q = fmap fromIntegral $
  liftIO (
    withQuery q $ \qPtr ->
      alloca $ \intPtr ->
        toStatus <$> f qPtr intPtr
        >>= status (peek intPtr)
  ) >>= throwOr pure

query_count_messages, query_count_threads
  :: (AsNotmuchError e, MonadError e m, MonadIO m) => Query a -> m Int
query_count_messages = query_count_x {#call unsafe query_count_messages #}
query_count_threads = query_count_x {#call unsafe query_count_threads #}


thread_get_thread_id :: Thread a -> IO ThreadId
thread_get_thread_id ptr =
  withThread ptr ({#call unsafe thread_get_thread_id #} >=> B.packCString)

thread_get_total_messages :: Thread a -> IO Int
thread_get_total_messages ptr =
  fromIntegral <$> withThread ptr ({#call unsafe thread_get_total_messages #})

thread_get_toplevel_messages :: MonadIO m => Thread a -> m [Message 0 a]
thread_get_toplevel_messages t@(Thread dfp qfp (ThreadHandle tfp))
  = liftIO $ withThread t $ \ptr ->
    {#call unsafe thread_get_toplevel_messages #} ptr
    >>= messagesToList [castForeignPtr dfp, castForeignPtr qfp, castForeignPtr tfp]

thread_get_messages :: MonadIO m => Thread a -> m [Message 0 a]
thread_get_messages t@(Thread dfp qfp (ThreadHandle tfp))
  = liftIO $ withThread t $ \ptr ->
    {#call unsafe thread_get_messages #} ptr
    >>= messagesToList [castForeignPtr dfp, castForeignPtr qfp, castForeignPtr tfp]

-- notmuch_thread_get_matched_messages -> Int

thread_get_authors :: Thread a -> IO (Maybe B.ByteString)
thread_get_authors ptr = withThread ptr $ \ptr' -> do
  r <- {#call unsafe thread_get_authors #} ptr'
  if r == nullPtr
     then pure Nothing
     else Just <$> B.packCString r

thread_get_subject :: Thread a -> IO B.ByteString
thread_get_subject ptr = withThread ptr ({#call unsafe thread_get_subject #} >=> B.packCString)
-- notmuch_thread_get_oldest_date
thread_get_newest_date :: Thread a -> IO CLong
thread_get_newest_date = flip withThread {#call unsafe thread_get_newest_date #}

-- Tags are always copied from the libnotmuch-managed memory,
-- and all the copying takes place under 'withThread', so
-- we don't need to detach the pointer or use a ForeignPtr.
thread_get_tags :: Thread a -> IO [Tag]
thread_get_tags ptr = withThread ptr $
  {#call unsafe thread_get_tags #} >=> tagsToList

message_get_message_id :: Message n a -> IO MessageId
message_get_message_id ptr =
  withMessage ptr ({#call unsafe message_get_message_id #} >=> B.packCString)

message_get_thread_id :: Message n a -> IO ThreadId
message_get_thread_id ptr =
  withMessage ptr ({#call unsafe message_get_thread_id #} >=> B.packCString)

message_get_replies :: MonadIO m => Message n a -> m [Message 0 a]
message_get_replies msg@(Message owners (MessageHandle mfp))
  = liftIO $ withMessage msg $ \ptr ->
    {#call unsafe message_get_replies #} ptr
      >>= messagesToList (castForeignPtr mfp : owners)
          -- have to keep this message alive, as well as its owners

message_get_filename :: Message n a -> IO FilePath
message_get_filename ptr =
  withMessage ptr ({#call unsafe message_get_filename #} >=> peekCString)

message_get_flag :: Message n a -> MessageFlag -> IO Bool
message_get_flag ptr flag = withMessage ptr $ \ptr' -> do
  (/= 0) <$> {#call unsafe message_get_flag #} ptr' (enumToCInt flag)

-- DB NEEDS TO BE WRITABLE???
message_set_flag :: Message n a -> MessageFlag -> Bool -> IO ()
message_set_flag ptr flag v = withMessage ptr $ \ptr' ->
  {#call unsafe message_set_flag #} ptr' (enumToCInt flag) (enumToCInt v)

message_get_date :: Message n a -> IO CLong
message_get_date = flip withMessage {#call unsafe message_get_date #}

-- returns EMPTY STRING on missing header,
-- NOTHING on error (I know, confusing)
--
message_get_header :: Message n a -> B.ByteString -> IO (Maybe B.ByteString)
message_get_header ptr s =
  B.useAsCString s $ \s' ->
    withMessage ptr $ \ptr' -> do
      r <- {#call unsafe message_get_header #} ptr' s'
      if r == nullPtr
        then pure Nothing
        else Just <$> B.packCString r

-- Tags are always copied from the libnotmuch-managed memory,
-- and all the copying takes place under 'withMessage', so
-- we don't need to detach the pointer or use a ForeignPtr.
message_get_tags :: Message n a -> IO [Tag]
message_get_tags ptr = withMessage ptr $
  {#call unsafe message_get_tags #} >=> tagsToList

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message n RW@)
--
-- Therefore assume everything worked!
--
message_remove_all_tags :: Message n RW -> IO ()
message_remove_all_tags msg = void $ withMessage msg $
  {#call unsafe message_remove_all_tags #}

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_NULL_POINTER (excluded by @Tag@ type)
-- * NOTMUCH_STATUS_TAG_TOO_LONG (excluded by @Tag@ type)
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message RW@)
--
-- Therefore assume everything worked!
--
message_add_tag :: Message n RW -> Tag -> IO ()
message_add_tag msg tag = void $ withMessage msg $
  tagUseAsCString tag . {#call unsafe message_add_tag #}

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_NULL_POINTER (excluded by @Tag@ type)
-- * NOTMUCH_STATUS_TAG_TOO_LONG (excluded by @Tag@ type)
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message RW@)
--
-- Therefore assume everything worked!
--
message_remove_tag :: Message n RW -> Tag -> IO ()
message_remove_tag msg tag = void $ withMessage msg $
  tagUseAsCString tag . {#call unsafe message_remove_tag #}

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message n RW@)
--
-- Therefore assume everything worked!
--
message_freeze :: Message n RW -> IO (Message (n + 1) RW)
message_freeze msg = withMessage msg {#call unsafe message_freeze #} $> coerce msg

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message n RW@)
-- * NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW
--     (excluded by @(CmpNat n 0 ~ 'GT) => Message n RW@)
--
-- Therefore assume everything worked!
--
message_thaw :: (1 <= n) => Message n RW -> IO (Message (n - 1) RW)
message_thaw msg = withMessage msg {#call unsafe message_thaw #} $> coerce msg

-- message_maildir_flags_to_tags
-- message_tags_to_maildir_flags

-- directory functions

-- filenames functions


--
-- UTILITY FUNCTIONS
--

enumToCInt :: Enum a => a -> CInt
enumToCInt = fromIntegral . fromEnum

constructF
  :: (Storable ptr, Traversable t)
  => (ptr -> t ptr)
  -- ^ Inspect received pointer and lift it into a Traversable
  -> (ptr -> IO r)
  -- ^ Wrap pointer, including attaching finalizers
  -> (Ptr ptr -> IO CInt)
  -- ^ C double-pointer-style constructor
  -> IO (Either Status (t r))
constructF = constructF' (== StatusSuccess)

constructF'
  :: (Storable ptr, Traversable t)
  => (Status -> Bool)
  -- ^ Which statuses are considered a "success"?
  -> (ptr -> t ptr)
  -- ^ Inspect received pointer and lift it into a Traversable
  -> (ptr -> IO r)
  -- ^ Wrap pointer, including attaching finalizers
  -> (Ptr ptr -> IO CInt)
  -- ^ C double-pointer-style constructor
  -> IO (Either Status (t r))
constructF' test mkF dcon constructor =
  alloca $ \ptr ->
    toEnum . fromIntegral <$> constructor ptr
    >>= status' test (traverse dcon . mkF =<< peek ptr)


type PtrToList =
  forall a b ptr.
     (ptr -> IO CInt) -- ^ Iterator predicate function
  -> (ptr -> IO a)    -- ^ Iterator get function
  -> (ptr -> IO ())   -- ^ Iterator next function
  -> (a -> IO b)      -- ^ Item mapper
  -> ptr              -- ^ Iterator
  -> IO [b]

-- | Strictly read a C iterator into a list.
--
ptrToList :: PtrToList
ptrToList = ptrToListIO id

-- | Lazily read a C iterator into a list.
--
lazyPtrToList :: PtrToList
lazyPtrToList = ptrToListIO System.IO.Unsafe.unsafeInterleaveIO

ptrToListIO :: (forall r. IO r -> IO r) -> PtrToList
ptrToListIO tweakIO test get next f ptr = go
  where
  go = test ptr >>= \valid -> case valid of
    0 -> pure []
    _ -> (:)
          <$> (get ptr >>= f >>= \x -> next ptr $> x)
          <*> tweakIO go

-- It's assumed that tag lists are short and that it doesn't make
-- sense to read the list lazily.  Tere
tagsToList :: Tags -> IO [Tag]
tagsToList = ptrToList
  {#call unsafe tags_valid #}
  {#call unsafe tags_get #}
  {#call unsafe tags_move_to_next #}
  tagFromCString

threadsToList
  :: ForeignPtr DatabaseHandle
  -> ForeignPtr QueryHandle
  -> Threads
  -> IO [Thread mode]
threadsToList dfp qfp = lazyPtrToList
  {#call unsafe threads_valid #}
  {#call unsafe threads_get #}
  {#call unsafe threads_move_to_next #}
  (fmap (Thread dfp qfp . ThreadHandle) . newForeignPtr notmuch_thread_destroy <=< detachPtr)

messagesToList :: [ForeignPtr ()] -> Messages -> IO [Message 0 mode]
messagesToList owners = lazyPtrToList
  {#call unsafe messages_valid #}
  {#call unsafe messages_get #}
  {#call unsafe messages_move_to_next #}
  (fmap (Message owners . MessageHandle) . newForeignPtr notmuch_message_destroy <=< detachPtr)
