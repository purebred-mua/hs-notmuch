-- This file is part of hs-notmuch - Haskell Notmuch binding
-- Copyright (C) 2022  Yikai Zhao
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

Print version and revision info for a database.

-}

{-# LANGUAGE LambdaCase #-}

import Control.Monad.Except (runExceptT)
import System.Environment (getArgs)
import System.Exit (die)

import Notmuch


main :: IO ()
main = getArgs >>= \case
  [dbDir] -> go dbDir
  _ -> putStrLn "usage: hs-notmuch-dbinfo DB-DIR"

go :: String -> IO ()
go dbDir = do
  (version, (revision, uuid)) <- runExceptT (
    do
      db <- databaseOpenReadOnly dbDir
      (,) <$> databaseVersion db <*> databaseRevision db
    ) >>= either (die . (show :: Status -> String)) pure
  putStr "Path: "     *> putStrLn dbDir
  putStr "Version: "  *> print version
  putStr "Revision: " *> print revision
  putStr "UUID: "     *> putStrLn uuid
