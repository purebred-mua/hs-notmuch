module Notmuch.Search where

import qualified Data.ByteString.Char8 as C

import Notmuch.Binding (Tag, MessageId, ThreadId)

data SearchTerm
  = FreeForm String
  | From String
  | To String
  | Subject String
  | Attachment String -- <word>
  | Tag Tag
  | Id MessageId
  | Thread ThreadId
  | Folder String -- <maildir-folder>
  | Path String -- <directory-path>  ...  FilePath?
  | Date String String -- <since>..<until>
  | Asterisk
  | And SearchTerm SearchTerm
  | Or SearchTerm SearchTerm
  | Xor SearchTerm SearchTerm
  | Not SearchTerm
  | Bare String

instance Show SearchTerm where
  show (FreeForm s) = s -- TODO quote
  show (From s) = "from:" ++ s
  show (To s) = "to:" ++ s
  show (Subject s) = "subject:" ++ s
  show (Attachment s) = "attachment:" ++ s
  show (Tag s) = "tag:" ++ C.unpack s
  show (Id s) = "id:" ++ C.unpack s
  show (Thread s) = "thread:" ++ C.unpack s
  show (Folder s) = "folder:" ++ s
  show (Path s) = "path:" ++ s
  show (Date t u) = "date:" ++ t ++ ".." ++ u
  show Asterisk = "*"
  show (And a b)  = "( " ++ show a ++ " and " ++ show b ++ " )"
  show (Or a b)   = "( " ++ show a ++  " or " ++ show b ++ " )"
  show (Xor a b)  = "( " ++ show a ++ " xor " ++ show b ++ " )"
  show (Not a)    = "( not " ++ show a ++ " )"
  show (Bare s)   = s
