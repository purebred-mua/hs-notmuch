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

Set the tags on a specific message

-}

{-# LANGUAGE LambdaCase #-}

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (die)

import qualified Data.ByteString.Char8 as C

import Notmuch


main :: IO ()
main = getArgs >>= \case
  (dbDir : msgId : l) -> go dbDir msgId l
  _ -> putStrLn "usage: hs-notmuch-tag-message DB-DIR MESSAGE-ID TAG..."

go :: String -> String -> [String] -> IO ()
go dbDir msgId l = bracket
  (runExceptT (databaseOpen dbDir) >>= either (die . show) pure)
  (void . runExceptT . databaseDestroy)
  (\db -> runExceptT (
    let l' = fromMaybe (error "Bad tag(s)") (traverse (mkTag . C.pack) l)
    in findMessage db (C.pack msgId)
      >>= \case
        Nothing -> liftIO $ putStrLn "Message not found"
        Just msg -> messageSetTags l' msg
    ) >>= either (die . show) pure
  )
