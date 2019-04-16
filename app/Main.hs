{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import PullWatch.PullWatch
import PullWatch.Types
import PullWatch.Environment (getPAT, getRepoConfig)
import qualified Data.Default as Default (def)

doMonitor :: RepoArgs -> IO ()
doMonitor (RepoArgs owner repo) = do
  -- Set up authentication token from environment
  pat <- getPAT
  let ?pat = pat

  repoConfig <- getRepoConfig

  case repoConfig of
    Nothing -> monitorPRs Default.def [Repo owner repo]
    (Just repos) -> monitorPRs Default.def $ [Repo owner repo] ++ repos


  return ()

main = parseRepoArgs doMonitor
