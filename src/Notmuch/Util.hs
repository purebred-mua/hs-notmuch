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

{-# LANGUAGE RankNTypes #-}

{-|

Stuff that we don't want to export by default, but that we
do want to expose in the library interface.

-}

module Notmuch.Util where

import Control.Exception (bracket)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity(..))
import Data.Profunctor (Choice)
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.Tagged (Tagged(..))

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
type Prism' s a = Prism s s a a

review :: MonadReader b m => Prism' t b -> m t
review p = asks (runIdentity #. unTagged #. p .# Tagged .# Identity)
{-# INLINE review #-}

-- | Variant of 'bracket' that works with ExceptT and allows
-- resource acquisition to fail, propagating the error.  If
-- resource finalisation fails, the error is discarded.
--
bracketT
  :: (MonadError e m, MonadIO m)
  => ExceptT e IO a
  -> (a -> ExceptT e IO b)
  -> (a -> ExceptT e IO c)
  -> m c
bracketT acq rel go = liftIO (
  bracket
    (runExceptT acq)
    (runExceptT . traverse_ rel)
    (runExceptT . either throwError go)
  ) >>= either throwError pure
