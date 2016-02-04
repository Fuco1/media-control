{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.RWS (gets)
import Control.Monad.Trans (liftIO)
import DBus
import DBus.Mpris
import Data.Default
import Data.IORef
import Data.Map as M
import Data.Monoid ((<>))
import System.IO
import Format (formatMetadata)

data Player = Player { previousStatus :: PlaybackStatus
                     , previousVolume :: Double } deriving Show

setPreviousVolume :: Player -> Double -> Player
setPreviousVolume p v = p { previousVolume = v }

type PlayerData = Map BusName Player

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust a f = maybe (return ()) f a

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM a f = a >>= \v -> whenJust v f

myPlaybackStatusHook :: Callback PlaybackStatus
myPlaybackStatusHook = do
  bus <- bus
  whenJustM value $ \s ->
    when (s == Playing) $
      liftIO $ hPutStrLn stderr $ "Current bus is now " ++ show bus

stopCurrentOnPlaybackStatusChange :: IORef PlayerData -> Callback PlaybackStatus
stopCurrentOnPlaybackStatusChange playerdata = do
  b <- bus
  whenJustM value $ \bstatus -> liftMpris $ do
    pl <- gets players
    whenJustM current $ \c ->
      when (bstatus == Playing && notElem b pl) $ do
        liftIO $ hPutStrLn stderr $ "old current was " ++ show c
        whenJustM (volume c) $ \vol ->
          whenJustM (playbackStatus c) $ \cstatus -> do
            liftIO $ do
              players <- readIORef playerdata
              let p = Player { previousVolume = vol, previousStatus = cstatus }
              let newp = insert c p players
              writeIORef playerdata newp
              hPutStrLn stderr $ "stats: " ++ show newp
            when (cstatus == Playing) $ forkMpris $ do
              fade c
              pause c

restartPlayer :: IORef PlayerData -> Callback ()
restartPlayer playerdata = do
  bus <- bus
  liftIO $ hPutStrLn stderr $ "Bus quit. " ++ show bus
  liftMpris $ whenJustM current $ \cur -> do
    players <- liftIO $ readIORef playerdata
    whenJust (M.lookup cur players) $ \(Player { previousVolume = vol, previousStatus = cstatus }) -> do
      liftIO $ writeIORef playerdata (delete cur players)
      when (cstatus == Playing) $ do
         -- the player should be paused/stopped, so this should always
         -- restart it.  We use pausePlay over play because stupid
         -- spotify doesn't fully implement mpris.
        playPause cur
        unfade cur vol

fade :: BusName -> Mpris ()
fade bus = whenJustM (volume bus) $ \cv -> do
  let vol = (takeWhile (>= 0) . iterate (flip (-) 0.05) $ cv) ++ [0]
  forM_ vol $ \v -> do
    setVolume bus v
    liftIO $ threadDelay 65000

unfade :: BusName -> Double -> Mpris ()
unfade bus target = do
  let vol = (takeWhile (<= target) . iterate (0.05 +) $ 0) ++ [target]
  forM_ vol $ \v -> do
    setVolume bus v
    liftIO $ threadDelay 65000

loop :: IORef PlayerData -> Mpris ()
loop playerdata = do
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
  playerdata <- newIORef (M.empty :: PlayerData)
  let config = def { playbackStatusHook = stopCurrentOnPlaybackStatusChange playerdata
                                       <> playbackStatusHook def
                                       <> myPlaybackStatusHook
                   , playerQuitHook = playerQuitHook def <> restartPlayer playerdata
                   }
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  mpris config $ do
    names <- gets players
    liftIO $ print names
    forever (loop playerdata)
