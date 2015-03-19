{-# LANGUAGE OverloadedStrings #-}
import Network.Socket hiding (listen, Connected)
import OpenSSL
import qualified OpenSSL.Session as SSL
import System.IO 
import qualified GitLab as GitLab
import Text.Printf
import qualified Network.IRC as IRC
import Web.Scotty
import Network.HTTP.Types.Status

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TQueue

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import Control.Monad (when, forever)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (StateT, evalStateT, lift)
import qualified Control.Monad.State as S
import Data.List (isPrefixOf, isInfixOf)
import Data.Monoid (mconcat)

server = "irc.freenode.org"
port   = 6697
nick   = "CoolestBot123048"
chan   = "#bottest"
nick_password = "password123"

connectToServer :: AddrInfo -> IO SSL.SSL
connectToServer saddr = do
  sServer <- socket (addrFamily saddr) Stream defaultProtocol
  putStrLn "connecting"
  connect sServer (addrAddress saddr)
  putStrLn "establishing ssl context"
  ctx <- SSL.context
  putStrLn "setting ciphers"
  SSL.contextSetCiphers ctx "DEFAULT"
  putStrLn "setting verfication mode"
  SSL.contextSetVerificationMode ctx SSL.VerifyNone
  putStrLn "making ssl connection"
  sslServer <- SSL.connection ctx sServer
  putStrLn "doing handshake"
  SSL.connect sslServer
  putStrLn "connected"
  write sslServer ("NICK " ++ nick)
  write sslServer ("USER " ++ nick ++ " 0 * :test bot")
  return sslServer

listen :: SSL.SSL -> IO ()
listen ssl = do
  lines <- BL.lines `fmap` SSL.lazyRead ssl
  evalStateT (mapM_ (processLine ssl) lines) Initial

data ConnectionState = Initial | NickIdentification | Connected

processQueue :: SSL.SSL -> TQueue String -> IO ()
processQueue ssl queue = forever $ do newData <- atomically $ readTQueue queue
                                      privmsg ssl chan newData

processLine :: SSL.SSL -> BL.ByteString -> StateT ConnectionState IO ()
processLine ssl line = case message of
                         Just a -> processMessage ssl a
                         Nothing -> return ()
  where message = IRC.decode $ BL.toStrict line

processMessage :: SSL.SSL -> IRC.Message -> StateT ConnectionState IO ()
processMessage ssl m = do
    state <- S.get
    case state of
      Initial -> when (IRC.msg_command m == "376") $ do
        liftIO $ do putStrLn "connected!"
                    privmsg ssl "NickServ" ("identify " ++ nick_password)
        S.put NickIdentification
      NickIdentification -> do
        when (identified m) $ do
          liftIO $ do putStrLn "identified!"
                      joinChannel ssl chan
          S.put Connected
      Connected -> return ()
    liftIO $ print m
    -- let s = (tail . init . show) line
    -- putStrLn s
    when (IRC.msg_command m == "PING") $
      (liftIO . pong . mconcat . map show) (IRC.msg_params m)
  where
    pong x = write ssl ("PONG :" ++ x) 
    isNickServ (Just (IRC.NickName "NickServ" _ _)) = True
    isNickServ _ = False
    identified m = isNickServ (IRC.msg_prefix m) && "now identified" `isInfixOf` (mconcat . map show) (IRC.msg_params m)

privmsg :: SSL.SSL -> String -> String -> IO ()
privmsg h dest s = write h ("PRIVMSG " ++ dest ++ " :" ++ s)

joinChannel :: SSL.SSL -> String -> IO ()
joinChannel s c = write s ("JOIN " ++ c)
           
write :: SSL.SSL -> String -> IO ()
write h s = do
    SSL.write h $ B.pack (s ++ "\r\n")
    printf    "> %s\n" s

-- web_service :: TQueue String -> IO ()
-- web_service queue = scotty 25000 $ do
--   get "/" $ do
--     liftIO $ atomically $ writeTQueue queue "test passed"    
--     status ok200

runIrcBot :: [AddrInfo] -> TQueue String -> IO ()
runIrcBot addrs queue = do ssl <- connectToServer (head addrs)
                           forkIO $ listen ssl
                           processQueue ssl queue

main = withOpenSSL $ do
  let hints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
  addrs <- getAddrInfo (Just hints) (Just "irc.freenode.com") (Just "6697")
  queue <- atomically newTQueue
  forkIO $ runIrcBot addrs queue
  -- web_service queue
