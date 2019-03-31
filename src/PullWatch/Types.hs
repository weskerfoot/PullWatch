{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module PullWatch.Types
    (
      Repo(..)
    , PullRequest(..)
    , PullRequests
    , RepoArgs(..)
    ) where

import Data.Default
import GitHub.Data.Name
import Data.Yaml (FromJSON, withObject, (.:))
import System.Console.ArgParser.QuickParams (RawRead, rawParse)

import qualified GitHub.Endpoints.PullRequests as G
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Data.Yaml as Y

-- Type definitions

type PullRequests = IntMap.IntMap PullRequest

data Repo = Repo (G.Name G.Owner)
                 (G.Name G.Repo)
                deriving (Show, Eq)

data PullRequest = PR {
                    prText :: T.Text
                  , prTitle :: T.Text
                  , prRepo :: T.Text
                  , prOwner :: T.Text
                  , prID :: Integer
                  }

                  deriving (Show, Eq)

data RepoArgs = RepoArgs (G.Name G.Owner)
                         (G.Name G.Repo)
                  deriving (Show)

instance Default PullRequest where
  def = PR {
            prText = ""
          , prTitle = ""
          , prRepo = ""
          , prOwner = ""
          , prID = 0
  }

instance RawRead (G.Name a) where
  rawParse x = Just (N $ T.pack x, x)

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v -> makeRepo
      <$> v .: "owner"
      <*> v .: "repo"

-- Smart constructor for Repo type
makeRepo owner repo = Repo (c owner)
                           (c repo) where
    c = N . T.pack
