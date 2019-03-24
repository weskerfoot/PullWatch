{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import PullWatch.PullWatch (getLatestPRs, getLatest, toNote, prID)
import PullWatch.Environment (getPAT)

import Control.Applicative

main :: IO ()
main = do
  -- Set up authentication token from environment
  pat <- getPAT
  let ?pat = pat

  prIDs <- getLatestPRs [("racket", "racket")]
  print prIDs

  return ()
