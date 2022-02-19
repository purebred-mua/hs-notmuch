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

Print version info for a db.

-}

{-# LANGUAGE LambdaCase #-}

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import System.Exit (die)

import Notmuch


main :: IO ()
main = getArgs >>= \case
  [dbDir] -> go dbDir
  _ -> putStrLn "usage: hs-notmuch-dbinfo DB-DIR"

go :: String -> IO ()
go dbDir = runExceptT (
  do
    db <- databaseOpenReadOnly dbDir
    version <- databaseVersion db
    (revision, uuid) <- databaseRevision db
    liftIO $ putStrLn $
      "Path: " ++ databasePath db ++ "\n" ++
      "Version: " ++ show version ++ "\n" ++
      "Revision: " ++ show revision ++ "\n" ++
      "UUID: " ++ uuid
  ) >>= either (die . (show :: Status -> String)) pure
