{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

module PullWatch.PullWatch
    (  getLatest
     , getLatestAsync
     , getLatestPRs
     , prText
     , prTitle
     , prID
     , toNote
    ) where

import GitHub.Data.PullRequests
    (  simplePullRequestId
     , simplePullRequestTitle
     , simplePullRequestBody
    )

import Prelude.Compat
import GitHub.Data.Id (untagId)
import GitHub.Data.Name (untagName)
import System.Environment (lookupEnv)
import Data.Vector ((!))
import DBus.Notify
import Control.Concurrent.Async
import Control.Applicative
import Data.Maybe
import Data.Monoid

import qualified GitHub.Endpoints.PullRequests as PR
import qualified GitHub.Auth as Auth
import qualified Data.Text as T

data PullRequest = PR {
                    prText :: T.Text
                  , prTitle :: T.Text
                  , prRepo :: T.Text
                  , prOwner :: T.Text
                  , prID :: Integer }

                  deriving (Show)


fiveMinutes = 300000000

getPRId = Just . fromIntegral . untagId . simplePullRequestId . (! 0)

getPRTitle = Just . simplePullRequestTitle . (! 0)

getPRBody = simplePullRequestBody . (! 0)

-- Converts a pull request into a dbus notification
toNote :: Maybe PullWatch.PullWatch.PullRequest -> Maybe Note

toNote Nothing = Nothing
toNote (Just pr) =
  Just $ blankNote {
      summary = T.unpack $ prRepo pr
    , body = (Just $ Text $ T.unpack $ prTitle pr)
    , appImage = (Just $ Icon "dialog-information")
  }

getLatest :: (?pat :: (Maybe Auth.Auth)) =>
             (PR.Name PR.Owner) ->
             (PR.Name PR.Repo) ->
             IO (Maybe PullRequest)

getLatest owner repo = do
  prs <- PR.pullRequestsFor' ?pat owner repo
  let pr = case prs of
            (Left _) -> Nothing
            (Right pullreqs) -> do
              id <- getPRId pullreqs
              title <- getPRTitle pullreqs
              body <- getPRBody pullreqs
              let repoName = untagName repo
              let repoOwner = untagName owner
              return $ PR body title repoName repoOwner id
  return pr

getLatestAsync owner repo = do
    client <- connectSession

    async $ poll client where
      poll client = do
        latest <- getLatest owner repo
        notification <- maybe undefined (notify client) (toNote latest)
        return $ prID <$> latest

getLatestPRs :: (?pat :: (Maybe Auth.Auth)) =>
                [(PR.Name PR.Owner, PR.Name PR.Repo)] ->
                IO [Maybe (T.Text, T.Text, Integer)]

getLatestPRs repos = do
  prs <- mapConcurrently (uncurry getLatest) repos
  return $ map ((\pr -> (prRepo pr, prOwner pr, prID pr)) <$>) prs
