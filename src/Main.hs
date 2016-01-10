{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Trans (liftIO)
import DBus.Mpris
import Data.Default
import Data.Maybe
import Data.List as L
import qualified Data.Text as T
import Data.Monoid ((<>))
import Text.Printf (printf)
import System.IO

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

formatMetadata :: Maybe Metadata -> Maybe Integer -> Maybe PlaybackStatus -> String
formatMetadata (Just meta) (Just pos) (Just status) = name ++ playback ++ duration
  where name = case title meta of
          Just t -> "<fc=#888a85>" ++ t ++
                    "</fc><fc=#729fcf>" ++ fromMaybe "" (artist meta) ++ "</fc>"
          Nothing -> case url meta of
            Just f -> "<fc=#888a85>" ++ formatURL f ++ "</fc>"
            Nothing -> "Unknown"
        seekPos = formatDuration pos
        playback = case status of
          Playing -> "<fc=#8ae234>" ++ seekPos ++ "</fc>"
          Paused -> "<fc=#edd400>" ++ seekPos ++ "</fc>"
          Stopped -> "<fc=#ef2929>" ++ seekPos ++ "</fc>"
        duration = case len meta of
          Just l -> if l < 1000 then "" else formatDuration l
          Nothing -> ""
formatMetadata _ _ _ = "Metadata not available"

formatDuration :: Integer -> String
formatDuration dur = (if h > 0 then show h ++ ":" else "") ++ printf "%02d:%02d" m s
  where sec = dur `div` 1000000
        h = sec `div` 3600
        m = (sec `mod` 3600) `div` 60
        s = (sec `mod` 3600) `mod` 60

-- | Return file portion of URL if file:///, otherwise do nothing
formatURL :: String -> String
formatURL url = if "file:///" `isPrefixOf` url
                 then T.unpack . T.replace "%20" " " . T.pack . reverse . takeWhile (/= '/') . reverse $ url
                 else url
