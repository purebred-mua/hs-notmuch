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
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Notmuch.Binding where

import Control.Applicative (liftA2)
import Control.Monad ((>=>), (<=<), void)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity(..))
import Data.Proxy
import GHC.TypeLits

#include <notmuch.h>
{#context prefix = "notmuch" #}

import Foreign
  ( FinalizerPtr, ForeignPtr, Ptr
  , alloca, newForeignPtr, newForeignPtr_, nullFunPtr, nullPtr, peek
  )
import Foreign.C
import qualified System.IO.Unsafe

import qualified Data.ByteString as B

import Notmuch.Talloc


--
-- Types and type synonyms
--
type MessageId = B.ByteString
type ThreadId = B.ByteString

newtype Tag = Tag { getTag :: B.ByteString }

-- | @Just@ a tag, or @Nothing@ if the string is too long

mkTag :: B.ByteString -> Maybe Tag
mkTag s
  | B.length s > {#const NOTMUCH_TAG_MAX #} = Nothing
  | otherwise = Just (Tag s)

--
-- BINDING API
--

{#enum status_t as Status {underscoreToCase} deriving (Eq) #}
{#enum database_mode_t as DatabaseMode {underscoreToCase} #}
{#enum sort_t as Sort {underscoreToCase} #}
{#enum message_flag_t as MessageFlag {underscoreToCase} #}
{#pointer *database_t as DatabaseHandle foreign newtype #}
{#pointer *query_t as QueryHandle foreign newtype #}
{#pointer *threads_t as Threads foreign newtype #}
{#pointer *thread_t as ThreadHandle foreign newtype #}
{#pointer *messages_t as Messages foreign newtype #}
{#pointer *message_t as MessageHandle foreign newtype #}
{#pointer *tags_t as Tags foreign newtype #}
{#pointer *directory_t as Directory foreign newtype #}
{#pointer *filenames_t as Filenames foreign newtype #}

instance Show Status where
  show a = System.IO.Unsafe.unsafePerformIO $
    {#call status_to_string #} (fromEnum' a) >>= peekCString

-- | Read-only database mode
type RO = 'DatabaseModeReadOnly

-- | Read-write database mode
type RW = 'DatabaseModeReadWrite

newtype Database (a :: DatabaseMode) = Database DatabaseHandle

withDatabase :: Database a -> (Ptr DatabaseHandle -> IO b) -> IO b
withDatabase (Database dbh) = withDatabaseHandle dbh

newtype Message (n :: Nat) (a :: DatabaseMode) = Message MessageHandle

withMessage :: Message n a -> (Ptr MessageHandle -> IO b) -> IO b
withMessage (Message a) = withMessageHandle a

newtype Thread (a :: DatabaseMode) = Thread ThreadHandle

withThread :: Thread a -> (Ptr ThreadHandle -> IO b) -> IO b
withThread (Thread a) = withThreadHandle a

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


class Mode a where
  getMode :: Proxy a -> DatabaseMode
  upgrade :: Database a -> IO (Either Status (Database a))

instance Mode 'DatabaseModeReadOnly where
  getMode _ = DatabaseModeReadOnly
  upgrade = pure . Right

instance Mode 'DatabaseModeReadWrite where
  getMode _ = DatabaseModeReadWrite
  upgrade db = do
    toEnum . fromIntegral <$> withDatabase db (\dbPtr ->
      {#call database_upgrade #} dbPtr nullFunPtr nullPtr)
    >>= status (pure db)

-- | Open a Notmuch database
--
-- The database has no finaliser and will remain open even if GC'd.
--
database_open :: forall a. Mode a => FilePath -> IO (Either Status (Database a))
database_open s = withCString s (\s' -> (fmap . fmap) runIdentity $
  constructF
    (pure . Identity)
    (Database . DatabaseHandle)
    ({#call database_open #} s' (fromEnum' (getMode (Proxy :: Proxy a))))
    Nothing  -- no destructor
  ) >>= either (pure . Left) upgrade

database_destroy :: Database a -> IO Status
database_destroy db =
  toEnum . fromIntegral <$> withDatabase db {#call database_destroy #}

-- notmuch_status_t notmuch_database_compact(path, backup_path, status_cb, closure)

database_get_path :: Database a -> IO FilePath
database_get_path db =
  withDatabase db {#call database_get_path #} >>= peekCString

database_get_version :: Database a -> IO Int
database_get_version db =
  fromIntegral <$> withDatabase db {#call database_get_version #}

-- notmuch_database_needs_upgrade ## do automatically for updates
-- notmuch_database_upgrade ## do automatically for updates
-- notmuch_database_begin_atomic ## do automatically for updates
-- notmuch_database_end_atomic ## do automatically for updates

-- notmuch_database_get_directory

-- notmuch_database_add_message

-- notmuch_database_remove_message

database_find_message
  :: Database a
  -> MessageId
  -> IO (Either Status (Maybe (Message 0 a)))
database_find_message db s =
  withDatabase db $ \db' ->
    B.useAsCString s $ \s' ->
      constructF
        (\ptr -> if ptr /= nullPtr then Just <$> detachPtr ptr else pure Nothing)
        (Message . MessageHandle)
        ({#call database_find_message #} db' s')
        (Just message_destroy)

database_find_message_by_filename
  :: Database a -- ^ Database
  -> FilePath   -- ^ Filename
  -> IO (Either Status (Maybe (Message 0 a)))
database_find_message_by_filename db s =
  withDatabase db $ \db' ->
    withCString s $ \s' ->
      constructF
        (\ptr -> if ptr /= nullPtr then Just <$> detachPtr ptr else pure Nothing)
        (Message . MessageHandle)
        ({#call database_find_message_by_filename #} db' s')
        (Just message_destroy)

-- TODO: check for NULL, indicating error
database_get_all_tags :: Database a -> IO [Tag]
database_get_all_tags ptr = withDatabase ptr $ \ptr' ->
  {#call database_get_all_tags #} ptr'
    >>= detachPtr
    >>= newForeignPtr tags_destroy
    >>= tagsToList . Tags

-- TODO: check for NULL, indicating error
query_create :: Database a -> String -> IO (Query a)
query_create db s = withCString s $ \s' ->
  withDatabase db $ \db' ->
    {#call notmuch_query_create #} db' s'
      >>= detachPtr
      >>= fmap (Query . QueryHandle) . newForeignPtr query_destroy

query_get_query_string :: Query a -> IO String
query_get_query_string ptr =
  withQuery ptr ({#call query_get_query_string #} >=> peekCString)

query_set_sort :: Query a -> Sort -> IO ()
query_set_sort ptr x = withQuery ptr $ \ptr' ->
  {#call query_set_sort #} ptr' (fromEnum' x)

query_get_sort :: Query a -> IO Sort
query_get_sort ptr = withQuery ptr $
  fmap (toEnum . fromIntegral) . {#call query_get_sort #}

query_add_tag_exclude :: Query a -> Tag -> IO ()
query_add_tag_exclude ptr (Tag s) =
  withQuery ptr $ \ptr' ->
    B.useAsCString s $ \s' ->
      {#call query_add_tag_exclude #} ptr' s'

query_search_threads :: Query a -> IO [Thread a]
query_search_threads ptr = withQuery ptr $ \ptr' ->
  {#call query_search_threads #} ptr'
     >>= detachPtr
     >>= newForeignPtr threads_destroy
     >>= threadsToList . Threads

query_search_messages :: Query a -> IO [Message 0 a]
query_search_messages ptr = withQuery ptr $ \ptr' ->
  {#call query_search_messages #} ptr'
    >>= detachPtr
    >>= newForeignPtr messages_destroy
    >>= messagesToList . Messages

query_count_messages :: Query a -> IO Int
query_count_messages query =
  fromIntegral <$> withQuery query {#call query_count_messages #}

query_count_threads :: Query a -> IO Int
query_count_threads query =
  fromIntegral <$> withQuery query {#call query_count_threads #}

thread_get_thread_id :: Thread a -> IO ThreadId
thread_get_thread_id ptr =
  withThread ptr ({#call thread_get_thread_id #} >=> B.packCString)

-- notmuch_thread_get_total_messages
-- notmuch_thread_get_toplevel_messages -> Messages

thread_get_messages :: Thread a -> IO [Message 0 a]
thread_get_messages ptr = withThread ptr $ \ptr' ->
  {#call thread_get_messages #} ptr'
    >>= detachPtr
    >>= newForeignPtr messages_destroy
    >>= messagesToList . Messages

-- notmuch_thread_get_matched_messages -> Int
-- notmuch_thread_get_authors -> String
-- notmuch_thread_get_subject
-- notmuch_thread_get_oldest_date
-- notmuch_thread_get_newest_date

thread_get_tags :: Thread a -> IO [Tag]
thread_get_tags ptr = withThread ptr $ \ptr' ->
  {#call thread_get_tags #} ptr'
    >>= detachPtr
    >>= newForeignPtr tags_destroy
    >>= tagsToList . Tags

messages_collect_tags :: Messages -> IO [Tag]
messages_collect_tags ptr = withMessages ptr $ \ptr' ->
  {#call messages_collect_tags #} ptr'
    >>= detachPtr
    >>= newForeignPtr tags_destroy
    >>= tagsToList . Tags

message_get_message_id :: Message n a -> IO MessageId
message_get_message_id ptr =
  withMessage ptr ({#call message_get_message_id #} >=> B.packCString)

message_get_thread_id :: Message n a -> IO ThreadId
message_get_thread_id ptr =
  withMessage ptr ({#call message_get_thread_id #} >=> B.packCString)

message_get_replies :: Message n a -> IO [Message 0 a]
message_get_replies ptr = withMessage ptr $ \ptr' ->
  {#call message_get_replies #} ptr'
    >>= detachPtr
    >>= newForeignPtr messages_destroy
    >>= messagesToList . Messages

message_get_filename :: Message n a -> IO FilePath
message_get_filename ptr =
  withMessage ptr ({#call message_get_filename #} >=> peekCString)

message_get_flag :: Message n a -> MessageFlag -> IO Bool
message_get_flag ptr flag = withMessage ptr $ \ptr' -> do
  (/= 0) <$> {#call message_get_flag #} ptr' (enumToCInt flag)

-- DB NEEDS TO BE WRITABLE???
message_set_flag :: Message n a -> MessageFlag -> Bool -> IO ()
message_set_flag ptr flag v = withMessage ptr $ \ptr' ->
  {#call message_set_flag #} ptr' (enumToCInt flag) (enumToCInt v)

message_get_date :: Message n a -> IO CLong
message_get_date = flip withMessage {#call message_get_date #}

-- returns EMPTY STRING on missing header,
-- NOTHING on error (I know, confusing)
--
-- possible optimisation: detachPtr the returned string and
-- B.unsafePackCStringFinalizer to turn it into a ByteString
-- with talloc_free finaliser.  This will be O(n) because
-- must use strlen(3) to learn string length, and avoids
-- malloc.  Measure carefully to see if this would be worth it,
-- because it couples to both notmuch internals and unstable
-- parts of bytestring API (Data.ByteString.Unsafe).
--
message_get_header :: Message n a -> B.ByteString -> IO (Maybe B.ByteString)
message_get_header ptr s =
  B.useAsCString s $ \s' ->
    withMessage ptr $ \ptr' -> do
      r <- {#call message_get_header #} ptr' s'
      if r == nullPtr
        then pure Nothing
        else Just <$> B.packCString r

message_get_tags :: Message n a -> IO [Tag]
message_get_tags ptr = withMessage ptr $ \ptr' ->
  {#call message_get_tags #} ptr'
    >>= detachPtr
    >>= newForeignPtr tags_destroy
    >>= tagsToList . Tags

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message n RW@)
--
-- Therefore assume everything worked!
--
message_remove_all_tags :: Message n RW -> IO ()
message_remove_all_tags msg = withMessage msg $ \ptr ->
  void $ {#call message_remove_all_tags #} ptr

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_NULL_POINTER (excluded by @B.useAsCString@)
-- * NOTMUCH_STATUS_TAG_TOO_LONG (excluded by @Tag@ smart constructor)
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message RW@)
--
-- Therefore assume everything worked!
--
message_add_tag :: Message n RW -> Tag -> IO ()
message_add_tag msg (Tag s) = withMessage msg $ \ptr ->
  B.useAsCString s $ \s' ->
    void $ {#call message_add_tag #} ptr s'

-- According to the header file, possible errors are:
--
-- * NOTMUCH_STATUS_READ_ONLY_DATABASE (excluded by @Message n RW@)
--
-- Therefore assume everything worked!
--
message_freeze :: Message n RW -> IO (Message (n + 1) RW)
message_freeze msg = withMessage msg $ \ptr ->
  coerce msg <$ {#call message_freeze #} ptr

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
  coerce msg <$ {#call message_thaw #} ptr

-- message_remove_tag
-- message_maildir_flags_to_tags
-- message_tags_to_maildir_flags

-- directory functions

-- filenames functions

--
-- Destructors
--

foreign import ccall "&notmuch_query_destroy"
  query_destroy :: FinalizerPtr a

foreign import ccall "&notmuch_threads_destroy"
  threads_destroy :: FinalizerPtr a

foreign import ccall "&notmuch_thread_destroy"
  thread_destroy :: FinalizerPtr a

foreign import ccall "&notmuch_messages_destroy"
  messages_destroy :: FinalizerPtr a

foreign import ccall "&notmuch_message_destroy"
  message_destroy :: FinalizerPtr a

foreign import ccall "&notmuch_tags_destroy"
  tags_destroy :: FinalizerPtr a


--
-- UTILITY FUNCTIONS
--

enumToCInt :: Enum a => a -> CInt
enumToCInt = fromIntegral . fromEnum

constructF
  :: Traversable t
  => (Ptr p -> IO (t (Ptr p)))
  -- ^ Inspect received pointer and lift it into a Traversable
  -> (ForeignPtr p -> r)
  -- ^ Haskell data constructor
  -> (Ptr (Ptr p) -> IO CInt)
  -- ^ C double-pointer-style constructor
  -> Maybe (FinalizerPtr p)
  -- ^ Optional destructor
  -> IO (Either Status (t r))
constructF mkF dcon constructor destructor =
  let mkForeignPtr = maybe newForeignPtr_ newForeignPtr destructor
  in alloca $ \ptr ->
    toEnum . fromIntegral <$> constructor ptr
    >>= status (peek ptr >>= mkF >>= traverse (fmap dcon . mkForeignPtr))

-- | Turn a C iterator into a list
--
ptrToList
  :: (p -> (Ptr p -> IO [b]) -> IO [b])
  -- ^ Pointer unwrapper function (e.g. `withMessages`)
  -> (Ptr p -> IO (CInt))
  -- ^ Predicate on iterator
  -> (Ptr p -> IO a)
  -- ^ Iterater getter function
  -> (Ptr p -> IO ())
  -- ^ Function to advance iterator
  -> (a -> IO b)
  -- ^ Item mapper
  -> p
  -- ^ Pointer
  -> IO [b]
ptrToList withFObj test get next f fObj = withFObj fObj ptrToList'
  where
  ptrToList' ptr = test ptr >>= \valid -> if valid == 0
    then pure []
    else liftA2 (:)
      (get ptr >>= f >>= \x -> next ptr >> pure x)
      (ptrToList' ptr)

tagsToList :: Tags -> IO [Tag]
tagsToList = ptrToList
  withTags
  {#call tags_valid #}
  {#call tags_get #}
  {#call tags_move_to_next #}
  (fmap Tag . B.packCString)

threadsToList :: Threads -> IO [Thread a]
threadsToList = ptrToList
  withThreads
  {#call threads_valid #}
  {#call threads_get #}
  {#call threads_move_to_next #}
  (fmap (Thread . ThreadHandle) . newForeignPtr thread_destroy <=< detachPtr)

messagesToList :: Messages -> IO [Message 0 a]
messagesToList = ptrToList
  withMessages
  {#call messages_valid #}
  {#call messages_get #}
  {#call messages_move_to_next #}
  (fmap (Message . MessageHandle) . newForeignPtr message_destroy <=< detachPtr)
