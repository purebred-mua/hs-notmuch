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

module Notmuch.Tag where

import qualified Data.ByteString as B

import Notmuch.Binding.Constants (tagMaxLen)

newtype Tag = Tag { getTag :: B.ByteString }

-- | @Just@ a tag, or @Nothing@ if the string is too long
mkTag :: B.ByteString -> Maybe Tag
mkTag s =
  let
    w = B.length s
  in
    if w < 1 || w > tagMaxLen
      then Nothing
      else Just (Tag s)
