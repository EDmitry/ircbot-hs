{-# LANGUAGE OverloadedStrings #-}
import Network.Socket hiding (listen)
import OpenSSL
import qualified OpenSSL.Session as SSL
import System.IO 
import Text.Printf
  
import Web.Scotty
import Network.HTTP.Types.Status

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TQueue

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import Control.Monad (when, forever)
import Control.Monad.Trans (liftIO)
import Data.List (isPrefixOf)

server = "irc.freenode.org"
port   = 6667
nick   = "CoolestBot123048"
chan   = "#bottest"

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
  write sslServer ("JOIN " ++ chan)
  return sslServer

listen :: SSL.SSL -> IO ()
listen ssl = do
  lines <- BL.lines `fmap` SSL.lazyRead ssl
  mapM_ (processLine ssl . show) lines

processQueue :: SSL.SSL -> TQueue String -> IO ()
processQueue ssl queue = forever $ do newData <- atomically $ readTQueue queue
                                      privmsg ssl newData

processLine :: SSL.SSL -> String -> IO ()
processLine ssl line = do
    let s = (tail . init) line
    putStrLn s
    when (ping s) $ pong s 
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write ssl ("PONG :" ++ drop 6 x) 

privmsg :: SSL.SSL -> String -> IO ()
privmsg h s = write h ("PRIVMSG " ++ chan ++ " :" ++ s)

write :: SSL.SSL -> String -> IO ()
write h s = do
    SSL.write h $ B.pack (s ++ "\r\n")
    printf    "> %s\n" s

web_service :: TQueue String -> IO ()
web_service queue = scotty 25000 $ do
  get "/" $ do
    liftIO $ atomically $ writeTQueue queue "test passed"    
    status ok200

irc_bot :: [AddrInfo] -> TQueue String -> IO ()
irc_bot addrs queue = do ssl <- connectToServer (head addrs)
                         forkIO $ listen ssl
                         processQueue ssl queue

main = withOpenSSL $ do
  let hints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
  addrs <- getAddrInfo (Just hints) (Just "irc.freenode.com") (Just "6697")
  queue <- atomically newTQueue
  forkIO $ irc_bot addrs queue
  web_service queue
