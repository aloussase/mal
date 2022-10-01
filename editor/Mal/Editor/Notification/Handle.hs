module Mal.Editor.Notification.Handle
(
    Handle
  , close
  , new
  , send
  , start
)
where

import           Mal.Editor.Notification.Renderer

import           Control.Concurrent               (Chan, MVar, ThreadId, forkIO,
                                                   killThread, newChan,
                                                   newEmptyMVar, putMVar,
                                                   readChan, readMVar,
                                                   threadDelay, writeChan)
import           Control.Monad                    (forever, void)
import qualified GI.GLib                          as GLib

data SinkMessage a = Notification a | Close deriving Show

data Handle a msg =
  Handle
  { _sink     :: Chan (SinkMessage msg)
  , _renderer :: a
  , _threadId :: MVar ThreadId
  }

new :: (NotificationRenderer a) => a -> IO (Handle a msg)
new renderer = Handle <$> newChan <*> pure renderer <*> newEmptyMVar

start :: (NotificationRenderer a) => Handle a (Message a) -> IO ()
start h = do
  thid <- forkIO $ forever $ do
            message <- readChan (_sink h)
            case message of
              Close -> readMVar (_threadId h) >>= killThread
              Notification m -> do
                let renderer = _renderer h
                showNotification renderer m
                void $ forkIO $ do
                  threadDelay $ 10^6
                  void $ GLib.idleAdd GLib.PRIORITY_HIGH_IDLE $
                    hideNotification renderer >> pure False

  putMVar (_threadId h) thid

send :: Handle a msg -> msg -> IO ()
send h message = writeChan (_sink h) (Notification message)

close :: Handle a msg -> IO ()
close h = writeChan (_sink h) Close
