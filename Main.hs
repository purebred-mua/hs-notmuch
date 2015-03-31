import Control.Monad
import Data.Function (on)
import Data.List (maximumBy)
import System.Environment

import Notmuch
import Notmuch.Search

main :: IO ()
main = do
  args <- getArgs
  db' <- databaseOpen $ head args
  case db' of
    Left status -> putStrLn $ "Error: " ++ show status
    Right db -> do
      tagList <- tags db
      mapM_ (\s -> putStrLn ("tag:"++s) >> tagQuery db s >>= printQueryInfo) tagList
      msgCounts <- mapM (tagQuery db >=> queryCountMessages) tagList
      let mostMsgsTag = fst $ maximumBy (compare `on` snd) (zip tagList msgCounts)
      putStrLn $ "tag with most msgs: " ++ mostMsgsTag
      putStrLn "five subject lines from this tag (below):"
      do
        q <- tagQuery db mostMsgsTag
        msgs <- messages q
        (mapM_ (messageHeader "Subject" >=> putStrLn) . take 5) msgs
      putStrLn "END"

tagQuery :: Database -> Tag -> IO Query
tagQuery db tag = query db (Tag tag)

printQueryInfo :: Query -> IO ()
printQueryInfo q = do
  queryCountThreads q
    >>= \n -> putStrLn $ "  " ++ show n ++ " threads"
  queryCountMessages q
    >>= \n -> putStrLn $ "  " ++ show n ++ " messages"
