{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import PullWatch.PullWatch
import PullWatch.Environment (getPAT)
import qualified Data.Default as Default (def)

doMonitor :: RepoArgs -> IO ()
doMonitor (RepoArgs owner repo) = do
  -- Set up authentication token from environment
  pat <- getPAT
  let ?pat = pat

  monitorPRs Default.def [(owner, repo)]

  return ()

main = parseRepoArgs doMonitor
