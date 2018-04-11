-- Copyright (C) 2017  Fraser Tweedale
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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Notmuch.Tag
  (
    Tag
  , getTag
  , mkTag
  , tagUseAsCString
  , tagFromCString
  , tagMaxLen
  ) where

import Data.Hashable (Hashable, hash, hashWithSalt, hashPtrWithSalt)
import Data.Interned
import Data.Interned.Internal
import Data.Maybe (fromJust)
import Data.String (IsString(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Foreign.C (CInt(CInt), CSize(CSize), CString, CStringLen)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Notmuch.Binding.Constants (tagMaxLen)

-- | A pointer to tag data.  The length INCLUDES the null terminator.
--
data TagPtr
  = TagForeign {-# UNPACK #-} !B.ByteString
  | TagBare    {-# UNPACK #-} !Int {-# UNPACK #-} !CString

-- | Do a thing with the pointer of a tag.  For pinned tags this
-- uses withForeignPtr, otherwise it uses the bare pointer and it is
-- caller's responsibility to keep that memory live.
--
withTagPtr :: TagPtr -> (CStringLen -> IO b) -> IO b
withTagPtr (TagForeign bs) = B.unsafeUseAsCStringLen bs
withTagPtr (TagBare n ptr) = \k -> k (ptr, n)

instance Eq TagPtr where
  t1 == t2 = unsafeDupablePerformIO $
    withTagPtr t1 $ \(ptr1, n1) ->
      withTagPtr t2 $ \(ptr2, n2) ->
        if n1 == n2
          then (== 0) <$> c_strncmp ptr1 ptr2 (fromIntegral n1)
          else pure False

instance Hashable TagPtr where
  hashWithSalt salt t = unsafeDupablePerformIO $
    withTagPtr t (\(ptr, n) -> hashPtrWithSalt ptr (n - 1) salt)

instance Interned Tag where
  type Uninterned Tag = TagPtr
  newtype Description Tag = TagDescription TagPtr deriving (Eq, Hashable)
  describe = describeTagPtr
  identify i = Tag i . pinTag
  cacheWidth _ = 32
  cache = tagCache

-- First check for the presense of the key in the cache.
--
-- If absent, we need to pin the string (so that the key
-- data will not be in foreign memory).
--
-- If present, we do not
--
describeTagPtr :: TagPtr -> Description Tag
describeTagPtr tagptr =
  maybe (TagDescription . TagForeign . pinTag $ tagptr) (const $ TagDescription tagptr)
  $ unsafeDupablePerformIO (recover (TagDescription tagptr))

-- | Return a pinned version of a tag.  If the tag is pinned it is
-- returned unchanged.  Otherwise it is only safe to use if the
-- referenced memory is live.
--
pinTag :: TagPtr -> B.ByteString
pinTag (TagForeign bs) = bs
pinTag (TagBare n ptr) =
  unsafeDupablePerformIO $ B.packCStringLen (ptr, n)

tagCache :: Cache Tag
tagCache = mkCache
{-# NOINLINE tagCache #-}

data Tag = Tag
  {-# UNPACK #-} !Id
  {-# UNPACK #-} !B.ByteString

instance Eq Tag where
  Tag i _ == Tag j _ = i == j

instance Ord Tag where
  Tag _ s1 `compare` Tag _ s2 = s1 `compare` s2

instance Show Tag where
  show = show . getTag

instance Hashable Tag where
  hashWithSalt salt (Tag i _) = hashWithSalt salt i
  hash (Tag i _) = i

-- | Throws exception if not a valid tag.
instance IsString Tag where
  fromString = fromJust . mkTag . fromString

-- | /O(1)/
getTag :: Tag -> B.ByteString
getTag (Tag _ s) = B.init s  -- trim null byte

-- | /O(n)/ @Just@ a tag, or @Nothing@ if the string is too long
mkTag :: B.ByteString -> Maybe Tag
mkTag s =
  if w < 1 || w > tagMaxLen
    then Nothing
    else Just $ intern (TagForeign (s `B.snoc` 0))
  where
    w = B.length s

-- | /O(1)/
tagUseAsCString :: Tag -> (CString -> IO a) -> IO a
tagUseAsCString (Tag _ bs) = B.unsafeUseAsCString bs
{-# INLINE tagUseAsCString #-}

-- | /O(n)/ @CString@ must be null-terminated and non-empty.
-- We must copy the tag into pinned memory so that it can be
-- used again as a CString without copying.
--
tagFromCString :: CString -> IO Tag
tagFromCString ptr = do
  n <- B.c_strlen ptr
  let !t = intern $ TagBare (fromIntegral n + 1) ptr
  pure t

foreign import ccall unsafe "string.h strncmp" c_strncmp
  :: Ptr a -> Ptr b -> CSize -> IO CInt
