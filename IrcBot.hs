{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module IrcBot (BotSettings (BotSettings
                           , server
                           , port
                           , nick
                           , chan
                           , chanPassword
                           , nickPassword
                           , modules)
               , runIrcBot
               , BotModule (initForConnection)
               , ModuleBox (ModuleBox)
               , Bot
               , botSettings
               , privmsg
               , IRC.Message
               , parseCommand
               , write
               ) where

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
import Network.Socket (PortNumber)
import System.IO 
import System.Exit(exitWith, ExitCode (ExitSuccess)) 
import Text.Printf
import qualified Network.IRC as IRC
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
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (runReaderT, ReaderT, ask, asks)
import qualified Control.Monad.State as S
import Data.List (isPrefixOf, isInfixOf)
import Data.Monoid (mconcat, (<>))
import Data.Default (def)

connectToServer :: String -> PortNumber -> IO Connection
connectToServer server port = do
  ctx <- initConnectionContext
  connectTo ctx ConnectionParams
                { connectionHostname = server
                , connectionPort = port                       
                , connectionUseSecure = Just $ TLSSettingsSimple True False False
                , connectionUseSocks = Nothing
                }                       

type Bot = ReaderT BotData IO

data BotSettings =  BotSettings
                    { server :: String
                    , port :: PortNumber 
                    , nick :: B.ByteString
                    , nickPassword :: B.ByteString
                    , chan :: B.ByteString
                    , chanPassword :: B.ByteString
                    , modules :: [ModuleBox]
                    }

data BotData = BotData
               { botSettings :: BotSettings
               , writeQueue :: TQueue B.ByteString
               }

class BotModule m where
  initForConnection :: m -> Bot (IO (), Consumer IRC.Message Bot ())
  
data ModuleBox = forall m. BotModule m => ModuleBox m
instance BotModule ModuleBox where
  initForConnection (ModuleBox s) = initForConnection s
  
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

joinChannel :: B.ByteString -> B.ByteString -> Bot ()
joinChannel c p = write ("JOIN " <> c <> " " <> p)
           
write :: B.ByteString -> Bot ()
write s = do
  queue <- asks writeQueue 
  liftIO $ atomically $ writeTQueue queue s

register = do
  n <- asks $ nick . botSettings
  write ("NICK " <> n)                     
  write ("USER " <> n <> " 0 * :test bot")

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
                    
runIrcBot :: BotSettings -> IO ()
runIrcBot settings = forever $ do catchAny (connectToIRCNetwork settings) $ \e ->
                                    putStrLn $ "Got an exception " ++ show e
                                  threadDelay $ 1000000 * timeout
                                  putStrLn "Retrying to connect..."
  where timeout = 30
                     
connectToIRCNetwork :: BotSettings -> IO ()
connectToIRCNetwork settings = bracket (connectToServer (server settings) (port settings)) disconnect loop
  where
    disconnect con = do
      connectionClose con                     
      putStrLn "Closing the connection"

    loop con = bracket (runThreads con) cleanupThreads waitForThreads

    runThreads con = do
      writeQueue' <- atomically newTQueue
      let botData = BotData { writeQueue = writeQueue', botSettings = settings }
      watchdogIndicator <- atomically $ newTVar False
      writerThread <- async $ writeThread writeQueue' con
      watchDogThread <- async $ runReaderT (watchdog watchdogIndicator) botData
      modulesWithBoxes <- mapM (\m -> do (output, input) <- spawn unbounded
                                         (deinit, consumer) <- runReaderT (initForConnection m) botData
                                         return (deinit, consumer, output, input)) (modules settings)
      let modulesOutputs = foldl1 (<>) (map (\(_, _, b, _) -> b) modulesWithBoxes)
      modulesThreads <- mapM (\(_, consumer, _, input) -> async $ runModulesConsumer consumer input botData) modulesWithBoxes
      let modulesDeinits = map (\(a, _, _, _) -> a) modulesWithBoxes
      readThread <- async $ flip runReaderT botData $ do
        register
        runEffect (messages con >->
                   handlePing watchdogIndicator >->
                   sequence_ [nickNegotiation, nickServId, toOutput modulesOutputs])
      return ([readThread, watchDogThread, writerThread], modulesThreads, modulesDeinits)

    waitForThreads (coreThreads, modulesThreads, _) = do (_, c) <- waitAnyCatch coreThreads
                                                         case c of
                                                           Left e -> putStrLn ("EXCEPTION: " <> show e)
                                                           Right _ -> putStrLn "Exited correctly"


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
                               np <- asks $ nickPassword . botSettings
                               lift $ privmsg "NickServ" ("identify " <> np)
                       else nickNegotiation

nickServId :: Consumer IRC.Message Bot ()
nickServId = do m <- await
                if identified m
                  then do liftIO $ putStrLn "Identified!"
                          c <- asks $ chan . botSettings
                          n <- asks $ nick . botSettings
                          p <- asks $ chanPassword . botSettings
                          lift $ joinChannel c p
                          lift $ privmsg "ChanServ" ("op " <> c <> " " <> n)
                  else nickServId
  where
    isNickServ (Just (IRC.NickName "NickServ" _ _)) = True
    isNickServ _ = False
    identified m = isNickServ (IRC.msg_prefix m) &&
                   "now identified" `B.isInfixOf` mconcat (IRC.msg_params m)
