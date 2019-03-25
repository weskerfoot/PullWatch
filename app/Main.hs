{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import PullWatch.PullWatch
  ( monitorPRs
  , getLatestPRs
  , getLatest
  , prID
  )

import PullWatch.Environment (getPAT)

import Data.Default (def)
import Control.Applicative

main :: IO ()
main = do
  -- Set up authentication token from environment
  pat <- getPAT
  let ?pat = pat

  --prIDs <- getLatestPRs [("racket", "racket")]
  --print prIDs
  monitorPRs def [("weskerfoot", "PullWatch")]

  return ()
