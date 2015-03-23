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
import Network.HTTP.Types (ok200)
import System.IO 
import System.Exit(exitWith, ExitCode (ExitSuccess)) 
import GitLab
import Text.Printf
import qualified Network.IRC as IRC
import Web.Scotty
import Web.Scotty.TLS (scottyTLS)
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

server = "irc.freenode.org"
port   = 6697

nick :: B.ByteString
nick   = "CoolestBot123048"

chan :: B.ByteString
chan   = "#bottest"

nickPassword :: B.ByteString
nickPassword = "password123"

connectToServer ::  IO Connection
connectToServer = do
  ctx <- initConnectionContext
  connectTo ctx ConnectionParams
                { connectionHostname = server
                , connectionPort = port                       
                , connectionUseSecure = Just $ TLSSettingsSimple True False False
                , connectionUseSocks = Nothing
                }                       

type Bot = ReaderT (TQueue B.ByteString) IO

class BotModule m where
  initForConnection :: m -> Bot (IO (), Consumer IRC.Message Bot ())
  
data ModuleBox = forall m. BotModule m => ModuleBox m
instance BotModule ModuleBox where
  initForConnection (ModuleBox s) = initForConnection s
  
botModules :: [ModuleBox]
botModules = [ModuleBox IdModule, ModuleBox GitLabModule]
             
--------------------------------------------------------
data IdModule = IdModule 
instance BotModule IdModule where
  initForConnection m = return (return (), idConsumer)

idConsumer :: Consumer IRC.Message Bot ()
idConsumer = do m <- await
                case parseCommand "id" m of
                  Just (chan, body) -> lift $ privmsg chan body
                  Nothing -> return ()
                -- case parseJoin m of
                --   Just (nick, chan) -> lift $ privmsg chan ("Greetings, " <> nick <> ".")
                --   Nothing -> return ()
                idConsumer
            
--------------------------------------------------------
data GitLabModule = GitLabModule
instance BotModule GitLabModule where
  initForConnection m = startWebServer
  
startWebServer :: Bot (IO (), Consumer IRC.Message Bot ())
startWebServer = do queue <- lift $ atomically newTQueue
                    scottyThread <- lift $ async $ startScottyWithQueue queue
                    botSettings <- ask
                    writerThread <- lift $ async $ runReaderT (processQueue queue) botSettings
                    return (shutdownScotty [scottyThread, writerThread], return ())

processQueue :: TQueue CommitHook -> Bot ()
processQueue queue = forever $ do hook <- liftIO $ atomically $ readTQueue queue
                                  privmsg chan (formatHook hook)
                                  return ()
  where formatHook h = B.pack $ mconcat [author topCommit, " committed to ", ref h, ": ", message topCommit]
          where topCommit = head $ commits h

shutdownScotty threads = do mapM_ cancel threads
                            putStrLn "Shutting down the web server"
                            return ()

startScottyWithQueue :: TQueue CommitHook -> IO ()
startScottyWithQueue queue = scotty 25000 $ do
  post "/" $ do
    b <- body
    let j = decode b :: Maybe CommitHook
    case j of
      (Just a) -> liftIO $ atomically $ writeTQueue queue a
      Nothing -> return ()
    status ok200                                                                  
--------------------------------------------------------
                 
parseCommand :: B.ByteString -> IRC.Message -> Maybe (B.ByteString, B.ByteString)
parseCommand cmd m
  | IRC.msg_command m /= "PRIVMSG" = Nothing
  | (a:b:xs) <- IRC.msg_params m = let (first:rest) = B.words b
                                   in if B.tail first == cmd then Just (a, B.unwords rest) else Nothing
                                                             
  | otherwise = Nothing

parseJoin :: IRC.Message -> Maybe (B.ByteString, B.ByteString)
parseJoin m
  | IRC.msg_command m /= "JOIN" = Nothing
  | (Just (IRC.NickName nick _ _)) <- IRC.msg_prefix m = Just (nick, head $ IRC.msg_params m)
  | otherwise = Nothing

privmsg :: B.ByteString -> B.ByteString -> Bot ()
privmsg dest s = write $ mconcat ["PRIVMSG ", dest, " :", s]

joinChannel :: B.ByteString -> Bot ()
joinChannel c = write ("JOIN " <> c)
           
write :: B.ByteString -> Bot ()
write s = do
  queue <- ask 
  liftIO $ atomically $ writeTQueue queue s

register = do
  write ("NICK " <> nick)                     
  write ("USER " <> nick <> " 0 * :test bot")

writeThread :: TQueue B.ByteString -> Connection -> IO ()
writeThread queue con = forever $ do newData <- atomically (readTQueue queue)
                                     connectionPut con $ newData <> "\r\n" 
                                     printf    "> %s\n" (B.unpack newData)

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny m f = Control.Exception.catch m onExc
  where
    onExc e
      | shouldCatch e = f e
      | otherwise = throwIO e
    shouldCatch e
      | show e == "user interrupt" = False
      | otherwise = True
                    
runIrcBot :: IO ()
runIrcBot = forever $ do catchAny connectToIRCNetwork $ \e ->
                           putStrLn $ "Got an exception " ++ show e
                         threadDelay $ 1000000 * timeout
                         putStrLn "Retrying to connect..."
  where timeout = 30
                     
connectToIRCNetwork :: IO ()
connectToIRCNetwork = bracket connectToServer disconnect loop
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
                                         (deinit, consumer) <- runReaderT (initForConnection m) writeQueue
                                         return (deinit, consumer, output, input)) botModules
      let modulesOutputs = foldl1 (<>) (map (\(_, _, b, _) -> b) modulesWithBoxes)
      modulesThreads <- mapM (\(_, consumer, _, input) -> async $ runModulesConsumer consumer input writeQueue) modulesWithBoxes
      let modulesDeinits = map (\(a, _, _, _) -> a) modulesWithBoxes
      readThread <- async $ flip runReaderT writeQueue $ do
        register
        runEffect (messages con >->
                   handlePing watchdogIndicator >->
                   sequence_ [nickNegotiation, nickServId, toOutput modulesOutputs])
      return ([readThread, watchDogThread, writerThread], modulesThreads, modulesDeinits)

    waitForThreads (coreThreads, modulesThreads, _) = void $ waitAnyCatch coreThreads

    cleanupThreads (coreThreads, modulesThreads, modulesDeinits) = do
      mapM_ cancel $ coreThreads <> modulesThreads
      sequence_ modulesDeinits
      putStrLn "We are done"

    runModulesConsumer consumer input = runReaderT (runEffect (fromInput input  >-> consumer))

watchdog :: TVar Bool -> Bot ()
watchdog var = runEffect (produceUpdates >-> checkUpdates)
  where produceUpdates = forever $ do liftIO $ threadDelay (1000000 * timeout)
                                      update <- liftIO $ atomically $ do v <- readTVar var
                                                                         writeTVar var False
                                                                         return v
                                      yield update
        checkUpdates = do a <- await
                          if a
                            then checkUpdates 
                            else do lift $ write "PING :CoolestBot"
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
                            then (pong . mconcat) (IRC.msg_params m)
                            else yield m
                          handlePing indicator
  where
    pong x = lift $ write ("PONG :" <> x) 
   
nickNegotiation :: Consumer IRC.Message Bot ()
nickNegotiation = do m <- await
                     if IRC.msg_command m == "376"
                       then do liftIO $ putStrLn "connected!"
                               lift $ privmsg "NickServ" ("identify " <> nickPassword)
                       else nickNegotiation

nickServId :: Consumer IRC.Message Bot ()
nickServId = do m <- await
                if identified m
                  then do liftIO $ putStrLn "Identified!"
                          lift $ joinChannel chan
                          lift $ privmsg "ChanServ" ("op " <> chan <> " " <> nick)
                  else nickServId
  where
    isNickServ (Just (IRC.NickName "NickServ" _ _)) = True
    isNickServ _ = False
    identified m = isNickServ (IRC.msg_prefix m) &&
                   "now identified" `B.isInfixOf` mconcat (IRC.msg_params m)

main =  do runIrcBot

