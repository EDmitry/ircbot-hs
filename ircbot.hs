{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import System.Exit(exitWith, ExitCode (ExitSuccess)) 
import qualified GitLab
import Text.Printf
import qualified Network.IRC as IRC
import Web.Scotty
import Pipes (Consumer, Pipe, Producer, yield, await, (>->), runEffect, cat)
import Pipes.Concurrent (spawn, unbounded, Buffer(Unbounded), toOutput, fromInput)

import Control.Monad.STM
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (async, wait, waitAnyCatch, waitAny, cancel)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar, newTVar)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import Control.Exception (throwIO, catch, try, throw, SomeException, AsyncException(ThreadKilled, UserInterrupt))
import Control.Exception.Base (bracket, finally)
import Control.Monad (when, forever, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (StateT, evalStateT, lift)
import Control.Monad.Reader (runReaderT, ReaderT, ask)
import qualified Control.Monad.State as S
import Data.List (isPrefixOf, isInfixOf)
import Data.Monoid (mconcat, (<>))
import Data.Default (def)
import Data.Function (on)

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

type Bot = ReaderT (TQueue String) IO

class BotModule m where
  messageHandler :: m -> Consumer IRC.Message Bot ()
  
data ModuleBox = forall m. BotModule m => ModuleBox m
instance BotModule ModuleBox where
  messageHandler (ModuleBox s) = messageHandler s
  
botModules :: [ModuleBox]
botModules = [ModuleBox IdModule]
             
--------------------------------------------------------
data IdModule = IdModule 
instance BotModule IdModule where
  messageHandler = idConsumer

idConsumer :: IdModule -> Consumer IRC.Message Bot ()
idConsumer mod = do m <- await
                    case parseCommand "id" m of
                      Just (chan, body) -> lift $ privmsg chan body
                      Nothing -> return ()
                    case parseJoin m of
                      Just (nick, chan) -> lift $ privmsg chan ("Greetings, " ++ nick ++ ".")
                      Nothing -> return ()
                    idConsumer mod
            
--------------------------------------------------------
                 
parseCommand :: String -> IRC.Message -> Maybe (String, String)
parseCommand cmd m
  | IRC.msg_command m /= "PRIVMSG" = Nothing
  | (a:b:xs) <- IRC.msg_params m = let (first:rest) = words (B.unpack b)
                                   in if (tail first == cmd) then Just (B.unpack a, unwords rest) else Nothing
                                                             
  | otherwise = Nothing

parseJoin :: IRC.Message -> Maybe (String, String)
parseJoin m
  | IRC.msg_command m /= "JOIN" = Nothing
  | (Just (IRC.NickName nick _ _)) <- IRC.msg_prefix m = Just (B.unpack nick, B.unpack $ head $ IRC.msg_params m)
  | otherwise = Nothing

processQueue :: TQueue String -> Bot ()
processQueue queue = forever $ do newData <- liftIO $ atomically $ readTQueue queue
                                  privmsg chan newData

privmsg :: String -> String -> Bot ()
privmsg dest s = write ("PRIVMSG " ++ dest ++ " :" ++ s)

joinChannel :: String -> Bot ()
joinChannel c = write ("JOIN " ++ c)
           
write :: String -> Bot ()
write s = do
  queue <- ask 
  liftIO $ atomically $ writeTQueue queue s

register = do
  write ("NICK " ++ nick)                     
  write ("USER " ++ nick ++ " 0 * :test bot")

writeThread :: TQueue String -> Connection -> IO ()
writeThread queue con = forever $ do newData <- atomically (readTQueue queue)
                                     connectionPut con $ B.pack (newData ++ "\r\n") 
                                     printf    "> %s\n" newData

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny m f = Control.Exception.catch m onExc
  where
    onExc e
      | shouldCatch e = f e
      | otherwise = throwIO e
    shouldCatch e
      | show e == "user interrupt" = False
      | otherwise = True
                    
runIrcBot :: TQueue String -> IO ()
runIrcBot queue = forever $ do catchAny (connectToIRCNetwork queue) $ \e -> do
                                 putStrLn $ "Got an exception " ++ show e
                               threadDelay $ 1000000 * timeout
                               putStrLn "Retrying to connect..."
  where timeout = 30
                     
-- forkIO $ runReaderT (processQueue queue) writeQueue
connectToIRCNetwork :: TQueue String -> IO ()
connectToIRCNetwork queue =  do bracket connectToServer disconnect loop
  where
    disconnect con = do
      connectionClose con                     
      putStrLn "Closing the connection"

    loop con = bracket (runThreads con) cleanupThreads waitForThreads

    runThreads con = do
      writeQueue <- atomically newTQueue
      watchdogIndicator <- atomically $ newTVar False
      writerThread <- async $ writeThread writeQueue con
      watchDogThread <- async $ runReaderT (watchdog watchdogIndicator) writeQueue
      modulesWithBoxes <- mapM (\m -> do (output, input) <- spawn unbounded
                                         return (m, output, input)) botModules
      let modulesOutputs = foldl1 (<>) (map (\(_, b, _) -> b) modulesWithBoxes)
      modulesThreads <- mapM (\(m, _, input) -> async $ runReaderT (runEffect (fromInput input  >-> messageHandler m)) writeQueue) modulesWithBoxes
      readThread <- async $ runReaderT (do register
                                           runEffect (messages con >->
                                                      handlePing watchdogIndicator >->
                                                      sequence_ [nickNegotiation, nickServId, toOutput modulesOutputs])
                                       ) writeQueue
      return ([readThread, watchDogThread, writerThread], modulesThreads)

    waitForThreads (coreThreads, modulesThreads) = void $ waitAnyCatch coreThreads

    cleanupThreads (coreThreads, modulesThreads) = do
      mapM cancel $ coreThreads <> modulesThreads
      putStrLn "We are done"

watchdog :: TVar Bool -> Bot ()
watchdog var = runEffect (produceUpdates >-> checkUpdates)
  where produceUpdates = do liftIO $ threadDelay (1000000 * timeout)
                            update <- liftIO $ atomically $ do v <- readTVar var
                                                               writeTVar var False
                                                               return v
                            yield update
                            produceUpdates
        checkUpdates = do a <- await
                          if a
                            then checkUpdates 
                            else do lift $ write ("PING :CoolestBot")
                                    b <- await
                                    when b checkUpdates
        timeout = 30
                          
 
messages :: Connection -> Producer IRC.Message Bot ()
messages con = do message <- liftIO $ fmap IRC.decode (connectionGetLine 512 con)
                  case message of
                    Just a -> yield a
                    Nothing -> return ()
                  messages con

handlePing :: TVar Bool -> Pipe IRC.Message IRC.Message Bot ()
handlePing indicator = do m <- await
                          liftIO $ atomically $ writeTVar indicator True
                          if IRC.msg_command m == "PING"
                            then (pong . mconcat . map B.unpack) (IRC.msg_params m)
                            else yield m
                          handlePing indicator
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
           runIrcBot queue
  -- web_service queue

-- web_service :: TQueue String -> IO ()
-- web_service queue = scotty 25000 $ do
--   get "/" $ do
--     liftIO $ atomically $ writeTQueue queue "test passed"    
--     status ok200
