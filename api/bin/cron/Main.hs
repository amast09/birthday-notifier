{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Data.Time (defaultTimeLocale)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format (formatTime)
import System.Cron

main :: IO ()
main = do
  tids <- execSchedule $ do
    addJob job "* * * * *"
  putStrLn ("Scheduled " ++ show (length tids) ++ " Job(s)")
  -- Not sure how else to suspend thread in order to continue to allow jobs to be scheduled.
  -- This was the simplest way I could find without possibly bringing in other libraries
  _ <- getLine
  mapM_ killThread tids

job :: IO ()
job = do
  currentTime <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%FT%T%QZ" currentTime
  putStrLn ("Executing Scheduled Email at " ++ formattedTime)