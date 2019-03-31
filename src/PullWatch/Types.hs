{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module PullWatch.Types where

import Data.Default
import GitHub.Data.Name
import System.Console.ArgParser.QuickParams (RawRead, rawParse)

import qualified GitHub.Endpoints.PullRequests as PR
import qualified Data.IntMap as IntMap
import qualified Data.Text as T

-- Type definitions

type PullRequests = IntMap.IntMap PullRequest

data PullRequest = PR {
                    prText :: T.Text
                  , prTitle :: T.Text
                  , prRepo :: T.Text
                  , prOwner :: T.Text
                  , prID :: Integer
                  }

                  deriving (Show)

data RepoArgs = RepoArgs (PR.Name PR.Owner)
                         (PR.Name PR.Repo)
                  deriving (Show)

instance Default PullRequest where
  def = PR {
            prText = ""
          , prTitle = ""
          , prRepo = ""
          , prOwner = ""
          , prID = 0
  }

instance RawRead (PR.Name a) where
  rawParse x = Just (N $ T.pack x, x)
