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

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TQueue

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import Control.Exception.Base (bracket)
import Control.Monad (when, forever)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (StateT, evalStateT, lift)
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
  write con ("NICK " ++ nick)
  write con ("USER " ++ nick ++ " 0 * :test bot")
  return con

listen :: Connection -> IO ()
listen ssl = evalStateT (forever (liftIO (connectionGetLine 512 ssl) >>= processLine ssl)) Initial >> return ()

data ConnectionState = Initial | NickIdentification | Connected

processQueue :: Connection -> TQueue String -> IO ()
processQueue ssl queue = forever $ do newData <- atomically $ readTQueue queue
                                      privmsg ssl chan newData

processLine :: Connection -> B.ByteString -> StateT ConnectionState IO ()
processLine ssl line = case message of
                         Just a -> processMessage ssl a
                         Nothing -> return ()
  where message = IRC.decode line

processMessage :: Connection -> IRC.Message -> StateT ConnectionState IO ()
processMessage ssl m = do
    state <- S.get
    case state of
      Initial -> when (IRC.msg_command m == "376") $ do
        liftIO $ do putStrLn "connected!"
                    privmsg ssl "NickServ" ("identify " ++ nickPassword)
        S.put NickIdentification
      NickIdentification ->
        when (identified m) $ do
          liftIO $ do putStrLn "identified!"
                      joinChannel ssl chan
          S.put Connected
      Connected -> return ()
    liftIO $ print m
    when (IRC.msg_command m == "PING") $
      (liftIO . pong . mconcat . map show) (IRC.msg_params m)
  where
    pong x = write ssl ("PONG :" ++ x) 
    isNickServ (Just (IRC.NickName "NickServ" _ _)) = True
    isNickServ _ = False
    identified m = isNickServ (IRC.msg_prefix m) && "now identified" `isInfixOf` (mconcat . map show) (IRC.msg_params m)

privmsg :: Connection -> String -> String -> IO ()
privmsg h dest s = write h ("PRIVMSG " ++ dest ++ " :" ++ s)

joinChannel :: Connection -> String -> IO ()
joinChannel s c = write s ("JOIN " ++ c)
           
write :: Connection -> String -> IO ()
write h s = do
    connectionPut h $ B.pack (s ++ "\r\n")
    printf    "> %s\n" s

-- web_service :: TQueue String -> IO ()
-- web_service queue = scotty 25000 $ do
--   get "/" $ do
--     liftIO $ atomically $ writeTQueue queue "test passed"    
--     status ok200

runIrcBot :: TQueue String -> IO ()
runIrcBot queue = do bracket connect disconnect botLoop
  where
    connect = connectToServer
    disconnect con = connectionClose con >> putStrLn "Closing the connection"
    botLoop con = do forkIO $ processQueue con queue
                     listen con

main =  do queue <- atomically newTQueue
           forkIO $ runIrcBot queue
  -- web_service queue
