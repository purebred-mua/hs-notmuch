-- This file is part of hs-notmuch - Haskell Notmuch binding
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

{-

Search a notmuch database and print the filenames of each email found.

-}

{-# LANGUAGE LambdaCase #-}

import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Char8 as C
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.Exit (die)

import Notmuch
import Notmuch.Search


main :: IO ()
main = getArgs >>= \case
  [dbDir, search, '+':tag@(_:_)] -> go dbDir search messageAddTag tag
  [dbDir, search, '-':tag@(_:_)] -> go dbDir search messageRemoveTag tag
  _ -> putStrLn "usage: hs-notmuch-tag-set DB-DIR SEARCH-TERM +TAG|-TAG"
  where
    go dbDir searchTerm f tag =
      let
        tag' = fromJust $ mkTag $ C.pack tag
      in
        runExceptT (do
          db <- databaseOpen dbDir
          query db (Bare searchTerm) >>= messages >>= traverse_ (f tag')
          databaseDestroy db
        ) >>= either (die . (show :: Status -> String)) pure
