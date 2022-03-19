{-# LANGUAGE OverloadedStrings #-}

module Main where

import BirthdayNotifier (sendDailyBirthdayEmail)
import Control.Concurrent
import Data.Time (defaultTimeLocale)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format (formatTime)
import System.Cron
import System.IO (hPutStrLn, stderr, stdout)

main :: IO ()
main = do
  tids <- execSchedule $ do
    addJob job "0 8 * * *"
  hPutStrLn stderr ("Scheduled " ++ show (length tids) ++ " Job(s)")
  -- TODO: not sure if there is a better way to wait while threads execute, also not sure on cleanup
  threadDelay maxBound
  mapM_ killThread tids

job :: IO ()
job = do
  currentTime <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%FT%T%QZ" currentTime
  hPutStrLn stderr ("Executing Scheduled Email at " ++ formattedTime)
  sendDailyBirthdayEmail
