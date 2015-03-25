{-# LANGUAGE OverloadedStrings #-}
module Blame where

import Prelude hiding (mapM_, sum)
import GHC.IO.Handle (Handle, hIsEOF)
import Control.Applicative ((<$>))
import Control.Monad (unless, when)
import Pipes
import qualified Pipes.Prelude as P
import System.Process
import System.IO (hGetLine)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>), takeFileName)
import Data.Foldable
import Data.List (isPrefixOf, filter, sortBy, intercalate, dropWhile)
import Data.Function (on)
import qualified Data.ByteString.Char8 as B 
import qualified Data.Map as Map
import qualified IrcBot (Message)       
import IrcBot

data BlameModule = BlameModule FilePath FilePath
instance BotModule BlameModule where
  initForConnection m = return (return (), consumer m)
  
consumer :: BlameModule -> Consumer IrcBot.Message Bot ()
consumer mod@(BlameModule gitPath repoPath) = do
  m <- await
  case parseCommand "blame" m of
    Just (chan, body) -> do
      o <- B.pack <$> liftIO (blame gitPath repoPath $ B.unpack body)
      lift $ privmsg chan o
    Nothing -> return ()
  consumer mod

blame :: FilePath -> FilePath -> String -> IO String
blame gitPath repoPath file = do
    let sfile = takeFileName file
    e <- doesFileExist (repoPath </> sfile) 
    if e then do
      m <- sortBy (flip compare `on` snd) . Map.toList <$> blameMap sfile
      let total = sum $ map snd m
      let percentage = filter ((>0) . snd) $ map (\(s, k) -> (s, k * 100 `div` total)) m
      let formatted = map (\(s, k) -> s ++ " (" ++ show k ++ "%)") percentage
      return $ sfile ++ ": " ++ intercalate ", " (take 3 formatted)
    else return "No such file"
  where
    addAuthor :: Map.Map String Int -> String -> Map.Map String Int
    addAuthor m a = Map.insertWith (+) a 1 m
    blameMap :: FilePath -> IO (Map.Map String Int)
    blameMap sfile = P.fold addAuthor Map.empty id (blameProducer sfile gitPath repoPath >-> extractAuthors)

extractAuthors :: Pipe String String IO ()
extractAuthors = do s <- await
                    when ("author " `isPrefixOf` s) (yield $ drop 7 s)
                    extractAuthors

blameProducer :: FilePath -> FilePath -> FilePath -> Producer String IO ()
blameProducer f git repo = do 
  (hin, hout, herr, h) <- lift $ runInteractiveProcess git ["blame", "--line-porcelain", repo </> f] (Just repo) Nothing
  blameProducer' hout

blameProducer' :: Handle -> Producer String IO ()
blameProducer' h = do eof <- lift $ hIsEOF h
                      unless eof $ do
                      s <- lift $ hGetLine h
                      yield s
                      blameProducer' h
