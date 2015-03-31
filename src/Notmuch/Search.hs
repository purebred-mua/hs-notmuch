module Notmuch.Search where

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

instance Show SearchTerm where
  show (FreeForm s) = s -- TODO quote
  show (From s) = "from:" ++ s
  show (To s) = "to:" ++ s
  show (Subject s) = "subject:" ++ s
  show (Attachment s) = "attachment:" ++ s
  show (Tag s) = "tag:" ++ s
  show (Id s) = "id:" ++ s
  show (Thread s) = "thread:" ++ s
  show (Folder s) = "folder:" ++ s
  show (Path s) = "path:" ++ s
  show (Date t u) = "date:" ++ t ++ ".." ++ u
  show Asterisk = "*"

data Search
  = Term SearchTerm
  | And Search Search
  | Or Search Search
  | Xor Search Search
  | Not Search

instance Show Search where
  show (Term a)   = show a
  show (And a b)  = "( " ++ show a ++ " and " ++ show b ++ " )"
  show (Or a b)   = "( " ++ show a ++  " or " ++ show b ++ " )"
  show (Xor a b)  = "( " ++ show a ++ " xor " ++ show b ++ " )"
  show (Not a)    = "( not " ++ show a ++ " )"
