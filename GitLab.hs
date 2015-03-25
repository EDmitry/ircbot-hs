{-# LANGUAGE OverloadedStrings #-}
module GitLab (GitLabModule (GitLabModule)) where

import qualified IrcBot (Message)       
import IrcBot
import Data.Aeson
import Control.Applicative
import Data.Monoid
import Data.Char (chr)
import Network.HTTP.Types (ok200)
import Web.Scotty
import Web.Scotty.TLS (scottyTLS)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.STM
import Control.Concurrent.STM.TQueue
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.Async (async, wait, waitAnyCatch, waitAny, cancel)
import Control.Monad (forever)
import Pipes (Consumer, Pipe, Producer, yield, await, (>->), runEffect, cat)
import Control.Monad.Reader (runReaderT, ReaderT, ask, asks)

data Commit = Commit { id :: String
                     , message :: String
                     , author :: String
                     , email :: String  
                     , url :: String  
                     } deriving (Eq)

instance Show Commit where
  show x = mconcat [author x, " (", email x, ") committed: ", message x]

instance FromJSON Commit where
  parseJSON (Object o) = Commit <$>
                         o .: "id" <*>
                         o .: "message" <*>
                         ((o .: "author") >>= (.: "name")) <*>
                         ((o .: "author") >>= (.: "email")) <*>
                         o .: "url"
                         
data CommitHook = CommitHook { ref :: String
                             , projectId :: Int
                             , commits :: [Commit]
                             } deriving (Show, Eq) 

instance FromJSON CommitHook where
  parseJSON (Object o) = CommitHook <$>
                         o .: "ref" <*>
                         o .: "project_id" <*>
                         (o .: "commits" >>= parseJSON)


data GitLabModule = GitLabModule
instance BotModule GitLabModule where
  initForConnection m = startWebServer
  
startWebServer :: Bot (IO (), Consumer IrcBot.Message Bot ())
startWebServer = do queue <- lift $ atomically newTQueue
                    scottyThread <- lift $ async $ startScottyWithQueue queue
                    botSettings <- ask
                    writerThread <- lift $ async $ runReaderT (processQueue queue) botSettings
                    return (shutdownScotty [scottyThread, writerThread], return ())

processQueue :: TQueue CommitHook -> Bot ()
processQueue queue = forever $ do hook <- liftIO $ atomically $ readTQueue queue
                                  case commits hook of
                                    [] -> liftIO $ putStrLn ("Malformed hook: " <> show hook)
                                    otherwise -> asks (chan . botSettings) >>= (`privmsg` (formatGray . formatHook $ hook))
                                  return ()
  where formatHook h = B.pack $ mconcat [author topCommit, " committed to ", ref h, ": ", message topCommit]
          where topCommit = head $ commits h

        formatGray :: B.ByteString -> B.ByteString
        formatGray s = B.pack (chr 3 : "14") <> s

shutdownScotty threads = do mapM_ cancel threads
                            putStrLn "Shutting down the web server"
                            return ()

startScottyWithQueue :: TQueue CommitHook -> IO ()
startScottyWithQueue queue = scotty 25000 $
  post "/" $ do
    b <- body
    let j = decode b :: Maybe CommitHook
    case j of
      (Just a) -> liftIO $ atomically $ writeTQueue queue a
      Nothing -> return ()
    status ok200                                                                  
