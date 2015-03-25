{-# LANGUAGE OverloadedStrings #-}
module Topic (TopicModule (TopicModule)) where

import qualified IrcBot (Message)       
import IrcBot
import Data.Monoid ((<>))
import Pipes
import Control.Monad.Trans (lift, liftIO)
 
data TopicModule = TopicModule
instance BotModule TopicModule where
  initForConnection m = return (return (), consumer)
  
consumer :: Consumer IrcBot.Message Bot ()
consumer = do
  m <- await
  case parseCommand "topic" m of
    Just (chan, body) -> lift $ write ("TOPIC " <> chan <> " :" <> body)
    Nothing -> return ()
  consumer
