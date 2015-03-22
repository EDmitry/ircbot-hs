{-# LANGUAGE OverloadedStrings #-}
import Network.Connection
  (Connection
  , TLSSettings (TLSSettingsSimple)
  , ConnectionParams (ConnectionParams, connectionHostname, connectionPort, connectionUseSecure, connectionUseSocks)
  , initConnectionContext
  , connectTo
  , ConnectionParams
  , connectionGetLine
  , connectionPut
  , connectionClose
  )
import System.IO 
import qualified GitLab
import Text.Printf
import qualified Network.IRC as IRC
import Web.Scotty
import Pipes (Consumer, Pipe, Producer, yield, await, (>->), runEffect, cat)

import Control.Monad.STM
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TQueue

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import Control.Exception.Base (bracket)
import Control.Monad (when, forever)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (StateT, evalStateT, lift)
import Control.Monad.Reader (runReaderT, ReaderT, ask)
import qualified Control.Monad.State as S
import Data.List (isPrefixOf, isInfixOf)
import Data.Monoid (mconcat)
import Data.Default (def)

server = "irc.freenode.org"
port   = 6697
nick   = "CoolestBot123048"
chan   = "#bottest"
nickPassword = "password123"

connectToServer ::  IO Connection
connectToServer = do
  ctx <- initConnectionContext
  con <- connectTo ctx ConnectionParams
                  { connectionHostname = server
                  , connectionPort = port                       
                  , connectionUseSecure = Just $ TLSSettingsSimple True False False
                  , connectionUseSocks = Nothing
                  }                       
  return con

type Bot = ReaderT Connection IO

processQueue :: TQueue String -> Bot ()
processQueue queue = forever $ do newData <- liftIO $ atomically $ readTQueue queue
                                  privmsg chan newData

privmsg :: String -> String -> Bot ()
privmsg dest s = write ("PRIVMSG " ++ dest ++ " :" ++ s)

joinChannel :: String -> Bot ()
joinChannel c = write ("JOIN " ++ c)
           
write :: String -> Bot ()
write s = do
  con <- ask 
  liftIO $ connectionPut con $ B.pack (s ++ "\r\n")
  liftIO $ printf    "> %s\n" s

runIrcBot :: TQueue String -> IO ()
runIrcBot queue = do bracket connect disconnect loop
  where
    connect = connectToServer
    disconnect con = connectionClose con >> putStrLn "Closing the connection"
    loop con = do forkIO $ runReaderT (processQueue queue) con
                  runReaderT (do write ("NICK " ++ nick)
                                 write ("USER " ++ nick ++ " 0 * :test bot")
                                 runEffect (messages >-> handlePing >-> sequence_ [nickNegotiation, nickServId, forever await])
                              ) con

messages :: Producer IRC.Message Bot ()
messages = do con <- ask
              message <- liftIO $ fmap IRC.decode (connectionGetLine 512 con)
              case message of
                Just a -> yield a
                Nothing -> return ()
              messages

handlePing :: Pipe IRC.Message IRC.Message Bot ()
handlePing = do m <- await
                if IRC.msg_command m == "PING"
                  then (pong . mconcat . map B.unpack) (IRC.msg_params m)
                  else yield m
                handlePing
  where
    pong x = lift $ write ("PONG :" ++ x) 
   
nickNegotiation :: Consumer IRC.Message Bot ()
nickNegotiation = do m <- await
                     if (IRC.msg_command m == "376")
                       then do liftIO $ putStrLn "connected!"
                               lift $ privmsg "NickServ" ("identify " ++ nickPassword)
                       else nickNegotiation

nickServId :: Consumer IRC.Message Bot ()
nickServId = do m <- await
                if (identified m)
                  then do liftIO $ putStrLn "Identified!"
                          lift $ joinChannel chan
                  else nickServId
  where
    isNickServ (Just (IRC.NickName "NickServ" _ _)) = True
    isNickServ _ = False
    identified m = isNickServ (IRC.msg_prefix m) && "now identified" `isInfixOf` (mconcat . map show) (IRC.msg_params m)

main =  do queue <- atomically newTQueue
           forkIO $ runIrcBot queue
  -- web_service queue

-- web_service :: TQueue String -> IO ()
-- web_service queue = scotty 25000 $ do
--   get "/" $ do
--     liftIO $ atomically $ writeTQueue queue "test passed"    
--     status ok200
