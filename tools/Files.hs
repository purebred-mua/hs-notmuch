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

import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import System.Environment (getArgs)
import System.Exit (die)

import Notmuch


main :: IO ()
main = getArgs >>= \case
  [dbDir, search] -> go dbDir search
  _ -> putStrLn "usage: hs-notmuch-files DB-DIR SEARCH-TERM"

go :: String -> String -> IO ()
go dbDir searchTerm = runExceptT (
    databaseOpenReadOnly dbDir
    >>= (flip query (Bare searchTerm)
          >=> messages
          >=> traverse_ (messageFilename >=> (liftIO . putStrLn)))
  ) >>= either (die . (show :: Status -> String)) pure
