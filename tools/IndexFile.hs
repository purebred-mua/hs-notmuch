-- This file is part of hs-notmuch - Haskell Notmuch binding
-- Copyright (C) 2019  Fraser Tweedale
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

Index or remove the given file.

-}

{-# LANGUAGE LambdaCase #-}

import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import System.Environment (getArgs)
import System.Exit (die)

import Notmuch

main :: IO ()
main = getArgs >>= \case
  [dbDir, '-':path] -> remove dbDir path
  [dbDir, path] -> index dbDir path
  _ -> die "usage: hs-notmuch-index-file DB-DIR [-]PATH"

index :: FilePath -> FilePath -> IO ()
index dbDir path =
  runExceptT (databaseOpen dbDir >>= flip indexFile path)
  >>= either (die . (show :: Status -> String)) (messageId >=> print)

remove :: FilePath -> FilePath -> IO ()
remove dbDir path =
  runExceptT (databaseOpen dbDir >>= flip removeFile path)
  >>= either (die . (show :: Status -> String)) print
