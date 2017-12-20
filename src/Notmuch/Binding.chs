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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Notmuch.Binding where

import Control.Monad ((>=>), (<=<), void)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.Proxy
import GHC.TypeLits

import Notmuch.Tag
import Notmuch.Util

#include <notmuch.h>
{#context prefix = "notmuch" #}

import Foreign
  ( FinalizerPtr, ForeignPtr, Ptr, castForeignPtr
  , alloca, newForeignPtr, newForeignPtr_, nullFunPtr, nullPtr, peek
  , touchForeignPtr
  )
import Foreign.C
import Foreign.Storable (Storable)
import qualified System.IO.Unsafe

import qualified Data.ByteString as B

import Notmuch.Talloc


--
-- Types and type synonyms
--
type MessageId = B.ByteString
type ThreadId = B.ByteString

--
-- BINDING API
--

{#enum status_t as Status {underscoreToCase} deriving (Eq) #}
{#enum database_mode_t as DatabaseMode {underscoreToCase} #}
{#enum sort_t as Sort {underscoreToCase} #}
{#enum message_flag_t as MessageFlag {underscoreToCase} #}
{#pointer *database_t as DatabaseHandle foreign newtype #}
{#pointer *query_t as QueryHandle foreign newtype #}
{#pointer *threads_t as Threads newtype #}
{#pointer *thread_t as ThreadHandle foreign newtype #}
{#pointer *messages_t as Messages newtype #}
{#pointer *message_t as MessageHandle foreign newtype #}
{#pointer *tags_t as Tags newtype #}
{#pointer *directory_t as Directory foreign newtype #}
{#pointer *filenames_t as Filenames foreign newtype #}

deriving instance Storable Threads
deriving instance Storable Messages

instance Show Status where
  show a = System.IO.Unsafe.unsafePerformIO $
    {#call unsafe status_to_string #} (fromEnum' a) >>= peekCString

class AsNotmuchError s where
  _NotmuchError :: Prism' s Status

instance AsNotmuchError Status where
  _NotmuchError = id

throwOr :: (AsNotmuchError e, MonadError e m) => (a -> m b) -> Either Status a -> m b
throwOr = either (throwError . review _NotmuchError)

-- | Read-only database mode
type RO = 'DatabaseModeReadOnly

-- | Read-write database mode
type RW = 'DatabaseModeReadWrite

newtype Database (a :: DatabaseMode) = Database DatabaseHandle

withDatabase :: Database a -> (Ptr DatabaseHandle -> IO b) -> IO b
withDatabase (Database dbh) = withDatabaseHandle dbh

data Message (n :: Nat) (a :: DatabaseMode) = Message
                 ![ForeignPtr () {- owners -}]
  {-# UNPACK #-} !MessageHandle

withMessage :: Message n a -> (Ptr MessageHandle -> IO b) -> IO b
withMessage (Message owners a) k = do
  r <- withMessageHandle a k
  traverse_ touchForeignPtr owners
  pure r

data Thread (a :: DatabaseMode) = Thread
  {-# UNPACK #-} !(ForeignPtr QueryHandle {- owner -})
  {-# UNPACK #-} !ThreadHandle

withThread :: Thread a -> (Ptr ThreadHandle -> IO b) -> IO b
withThread (Thread fp a) k = do
  r <- withThreadHandle a k
  touchForeignPtr fp
  pure r

newtype Query (a :: DatabaseMode) = Query QueryHandle

withQuery :: Query a -> (Ptr QueryHandle -> IO b) -> IO b
withQuery (Query a) = withQueryHandle a

fromEnum' :: (Enum a, Integral b) => a -> b
fromEnum' = fromIntegral . fromEnum

-- | If @StatusSuccess@ then @Right a@ else @Left status@.
--
status :: Functor f => f a -> Status -> f (Either Status a)
status a StatusSuccess = Right <$> a
status a e = Left e <$ a

toStatus :: (Integral a, Enum b) => a -> b
toStatus = toEnum . fromIntegral


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
-- The database has no finaliser and will remain open even if GC'd.
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
        (fmap (Database . DatabaseHandle) . newForeignPtr_)
        ({#call unsafe database_open #} s' (fromEnum' (getMode (Proxy :: Proxy a))))
    ))
  >>= throwOr upgrade

database_destroy :: (AsNotmuchError e, MonadError e m, MonadIO m) => Database a -> m ()
database_destroy db =
#if LIBNOTMUCH_CHECK_VERSION(4,1,0)
  liftIO (
    toStatus <$> withDatabase db {#call unsafe database_destroy #}
    >>= status (pure ())
  ) >>= throwOr pure
#else
  liftIO (withDatabase db {#call unsafe database_destroy #})
#endif

-- notmuch_status_t notmuch_database_compact(path, backup_path, status_cb, closure)

database_get_path :: Database a -> IO FilePath
database_get_path db =
  withDatabase db {#call unsafe database_get_path #} >>= peekCString

database_get_version :: Database a -> IO Int
database_get_version db =
  fromIntegral <$> withDatabase db {#call unsafe database_get_version #}

-- notmuch_database_needs_upgrade ## do automatically for updates
-- notmuch_database_upgrade ## do automatically for updates
-- notmuch_database_begin_atomic ## do automatically for updates
-- notmuch_database_end_atomic ## do automatically for updates

-- notmuch_database_get_directory

-- notmuch_database_add_message

-- notmuch_database_remove_message

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
          . (newForeignPtr message_destroy <=< detachPtr))
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
query_create db s = withCString s $ \s' ->
  withDatabase db $ \db' ->
    {#call unsafe notmuch_query_create #} db' s'
      >>= detachPtr
      >>= fmap (Query . QueryHandle) . newForeignPtr query_destroy

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
query_search_threads q@(Query (QueryHandle qfp)) =
#if LIBNOTMUCH_CHECK_VERSION(5,0,0)
  liftIO ( withQuery q $ \qPtr ->
    constructF
      Identity
      (threadsToList qfp)
      ({#call unsafe query_search_threads #} qPtr)
  )
  >>= throwOr (pure . runIdentity)
#else
  liftIO $ withQuery q $ \qPtr ->
    {#call unsafe query_search_threads #} qPtr >>= threadsToList qfp
#endif

query_search_messages
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
  => Query a -> m [Message 0 a]
query_search_messages q@(Query (QueryHandle qfp)) =
#if LIBNOTMUCH_CHECK_VERSION(5,0,0)
  liftIO ( withQuery q $ \qPtr ->
    constructF
      Identity
      (messagesToList [castForeignPtr qfp])
      ({#call unsafe query_search_messages #} qPtr)
  )
  >>= throwOr (pure . runIdentity)
#else
  liftIO $ withQuery q $ \qPtr ->
    {#call unsafe query_search_messages #} qPtr >>= messagesToList [castForeignPtr qfp]
#endif

query_count_x
  :: (AsNotmuchError e, MonadError e m, MonadIO m)
#if LIBNOTMUCH_CHECK_VERSION(5,0,0)
  => (Ptr QueryHandle -> Ptr CUInt -> IO CInt)
#else
  => (Ptr QueryHandle -> IO CUInt)
#endif
  -> Query a -> m Int
query_count_x f q = fmap fromIntegral $
#if LIBNOTMUCH_CHECK_VERSION(5,0,0)
  liftIO (
    withQuery q $ \qPtr ->
      alloca $ \intPtr ->
        toStatus <$> f qPtr intPtr
        >>= status (peek intPtr)
  ) >>= throwOr pure
#else
  liftIO $ withQuery q f
#endif

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
thread_get_toplevel_messages t@(Thread _ (ThreadHandle tfp))
  = liftIO $ withThread t $ \ptr ->
    {#call unsafe thread_get_toplevel_messages #} ptr
      >>= messagesToList [castForeignPtr tfp]

thread_get_messages :: MonadIO m => Thread a -> m [Message 0 a]
thread_get_messages t@(Thread _ (ThreadHandle tfp))
  = liftIO $ withThread t $ \ptr ->
    {#call unsafe thread_get_messages #} ptr
      >>= messagesToList [castForeignPtr tfp]

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
thread_get_tags ptr = withThread ptr $ \ptr' ->
  {#call unsafe thread_get_tags #} ptr' >>= tagsToList

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
message_get_tags ptr = withMessage ptr $ \ptr' ->
  {#call unsafe message_get_tags #} ptr' >>= tagsToList

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message n RW@)
--
-- Therefore assume everything worked!
--
message_remove_all_tags :: Message n RW -> IO ()
message_remove_all_tags msg = withMessage msg $ \ptr ->
  void $ {#call unsafe message_remove_all_tags #} ptr

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_NULL_POINTER (excluded by @Tag@ type)
-- * NOTMUCH_STATUS_TAG_TOO_LONG (excluded by @Tag@ type)
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message RW@)
--
-- Therefore assume everything worked!
--
message_add_tag :: Message n RW -> Tag -> IO ()
message_add_tag msg tag = withMessage msg $ \ptr ->
  tagUseAsCString tag $ \s ->
    void $ {#call unsafe message_add_tag #} ptr s

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_NULL_POINTER (excluded by @Tag@ type)
-- * NOTMUCH_STATUS_TAG_TOO_LONG (excluded by @Tag@ type)
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message RW@)
--
-- Therefore assume everything worked!
--
message_remove_tag :: Message n RW -> Tag -> IO ()
message_remove_tag msg tag = withMessage msg $ \ptr ->
  tagUseAsCString tag $ \s ->
    void $ {#call unsafe message_remove_tag #} ptr s

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message n RW@)
--
-- Therefore assume everything worked!
--
message_freeze :: Message n RW -> IO (Message (n + 1) RW)
message_freeze msg = withMessage msg $ \ptr ->
  coerce msg <$ {#call unsafe message_freeze #} ptr

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message n RW@)
-- * NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW
--     (excluded by @(CmpNat n 0 ~ 'GT) => Message n RW@)
--
-- Therefore assume everything worked!
--
message_thaw :: (CmpNat n 0 ~ 'GT) => Message n RW -> IO (Message (n - 1) RW)
message_thaw msg = withMessage msg $ \ptr ->
  coerce msg <$ {#call unsafe message_thaw #} ptr

-- message_maildir_flags_to_tags
-- message_tags_to_maildir_flags

-- directory functions

-- filenames functions

--
-- Destructors
--

foreign import ccall unsafe "&notmuch_query_destroy"
  query_destroy :: FinalizerPtr a

foreign import ccall unsafe "&notmuch_thread_destroy"
  thread_destroy :: FinalizerPtr a

foreign import ccall unsafe "&notmuch_message_destroy"
  message_destroy :: FinalizerPtr a


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
constructF mkF dcon constructor =
  alloca $ \ptr ->
    toEnum . fromIntegral <$> constructor ptr
    >>= status (traverse dcon . mkF =<< peek ptr)


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
-- The touch effect is used to keep relevant objects alive for the
-- duration of the iteration.
--
ptrToList :: PtrToList
ptrToList = ptrToListIO id

-- | Lazily read a C iterator into a list.
--
-- The touch effect is used to keep relevant objects alive for the
-- duration of the iteration.
--
lazyPtrToList :: PtrToList
lazyPtrToList = ptrToListIO System.IO.Unsafe.unsafeInterleaveIO

ptrToListIO :: (forall r. IO r -> IO r) -> PtrToList
ptrToListIO tweakIO test get next f = go
  where
  go ptr = test ptr >>= \valid -> case valid of
    0 -> pure []
    _ -> (:)
          <$> (get ptr >>= f >>= \x -> next ptr $> x)
          <*> tweakIO (go ptr)

-- It's assumed that tag lists are short and that it doesn't make
-- sense to read the list lazily.  Tere
tagsToList :: Tags -> IO [Tag]
tagsToList = ptrToList
  {#call unsafe tags_valid #}
  {#call unsafe tags_get #}
  {#call unsafe tags_move_to_next #}
  tagFromCString

threadsToList :: ForeignPtr QueryHandle -> Threads -> IO [Thread mode]
threadsToList owner = lazyPtrToList
  {#call unsafe threads_valid #}
  {#call unsafe threads_get #}
  {#call unsafe threads_move_to_next #}
  (fmap (Thread owner . ThreadHandle) . newForeignPtr thread_destroy <=< detachPtr)

messagesToList :: [ForeignPtr ()] -> Messages -> IO [Message 0 mode]
messagesToList owners = lazyPtrToList
  {#call unsafe messages_valid #}
  {#call unsafe messages_get #}
  {#call unsafe messages_move_to_next #}
  (fmap (Message owners . MessageHandle) . newForeignPtr message_destroy <=< detachPtr)
