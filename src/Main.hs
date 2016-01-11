{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Trans (liftIO)
import DBus.Mpris
import Data.Default
import Data.Monoid ((<>))
import System.IO
import Format (formatMetadata)

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust a f = maybe (return ()) f a

myPlaybackStatusHook :: Callback PlaybackStatus
myPlaybackStatusHook = do
  bus <- bus
  value <- value
  whenJust value $ \s ->
    when (s == Playing) $
      liftIO $ hPutStrLn stderr $ "Current bus is now " ++ show bus

loop :: Mpris ()
loop = do
  c <- current
  whenJust c $ \cur -> do
    meta <- metadata cur
    pos <- position cur
    status <- playbackStatus cur
    liftIO $ do
      threadDelay 800000
      putStrLn $ formatMetadata meta pos status

main :: IO ()
main = do
  let config = def {
        playbackStatusHook = playbackStatusHook def <> myPlaybackStatusHook
        }
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  mpris config $ forever loop
