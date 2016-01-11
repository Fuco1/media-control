{-# LANGUAGE OverloadedStrings #-}

module Format
       ( formatMetadata
       ) where

import Data.Maybe (fromMaybe)
import Data.List as L
import qualified Data.Text as T
import DBus.Mpris (Metadata(..), PlaybackStatus(..))
import Text.Printf (printf)

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
