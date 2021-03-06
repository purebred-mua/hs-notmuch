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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Notmuch.Tag
  (
    Tag
  , getTag
  , mkTag
  , tagUseAsCString
  , tagFromCString
  , tagMaxLen
  ) where

import Control.DeepSeq (NFData)
import Data.Maybe (fromJust)
import Data.String (IsString(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Foreign.C (CString)

import Notmuch.Binding.Constants (tagMaxLen)

-- | Message tag.  Use 'mkTag' to construct a tag.  Or use
-- @-XOverloadedStrings@, but beware that the @IsString@ instance
-- is non-total.
--
-- This data type avoid copying when passing tags to /libnotmuch/.
-- But copies do occur when reading tags from a database.
--
-- A previous experiment with interning showed no benefit.  Tags
-- are typically very short so the overhead erodes any advantage.
--
newtype Tag = Tag B.ByteString
  deriving (Eq, Ord, NFData)

instance Show Tag where
  show = show . getTag

-- | Throws exception if the tag is empty or too long.
instance IsString Tag where
  fromString = fromJust . mkTag . fromString

-- | /O(1)/
getTag :: Tag -> B.ByteString
getTag (Tag s) = B.init s  -- trim null byte

-- | /O(n)/ @Just@ a tag, or @Nothing@ if the string is too long
--
-- Use UTF-8 encoding to include non-ASCII characters in a tag.
--
mkTag :: B.ByteString -> Maybe Tag
mkTag s =
  if w < 1 || w > tagMaxLen
    then Nothing
    else Just $ Tag (s `B.snoc` 0)
  where
    w = B.length s

-- | /O(1)/
tagUseAsCString :: Tag -> (CString -> IO a) -> IO a
tagUseAsCString (Tag bs) = B.unsafeUseAsCString bs
{-# INLINE tagUseAsCString #-}

-- | /O(n)/ @CString@ must be null-terminated and non-empty.
-- We must copy the tag into pinned memory so that it can be
-- used again as a CString without copying.
--
tagFromCString :: CString -> IO Tag
tagFromCString cstr = Tag <$> do
  len <- B.c_strlen cstr
  B.packCStringLen (cstr, fromIntegral len + 1 {- include null byte -})
