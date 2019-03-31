{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

module PullWatch.PullWatch
    (  getLatest
     , getLatestPRs
     , prText
     , prTitle
     , prID
     , monitorPRs
     , parseRepoArgs
     , RepoArgs(..)
    ) where

import GitHub.Data.PullRequests
    (  simplePullRequestId
     , simplePullRequestTitle
     , simplePullRequestBody
    )

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Pool
import DBus.Notify
import Data.Maybe
import Data.Vector ((!?))
import GitHub.Data.Id (untagId)
import GitHub.Data.Name (untagName)
import Prelude.Compat
import System.Console.ArgParser
import PullWatch.Types
import Data.IntMap ((\\))

import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified GitHub.Auth as Auth
import qualified GitHub.Endpoints.PullRequests as PR

-- Argument parser
parseRepos :: ParserSpec RepoArgs

parseRepos = RepoArgs
  `parsedBy` reqPos "owner"
  `andBy` reqPos "repo"

parseRepoArgs = withParseResult parseRepos


-- Helper functions
tenMinutes = 300000000*2

getPRId = Just . fromIntegral . untagId . simplePullRequestId

getPRTitle = Just . simplePullRequestTitle

getPRBody = simplePullRequestBody

-- Converts a pull request into a dbus notification
toNote :: PullRequest -> Note

toNote pr =
  blankNote {
      summary = T.unpack $ prRepo pr
    , body = (Just $ Text $ T.unpack $ prTitle pr)
    , appImage = (Just $ Icon "dialog-information")
  }


notifyPR client pr = (print $ (summary pr) ++ " was opened") >>
                     notify client pr

getLatest :: (?pat :: (Maybe Auth.Auth)) =>
             Repo ->
             IO (Maybe PullRequest)

getLatest (Repo owner repo) = do
  prs <- PR.pullRequestsFor' ?pat owner repo
  let pr = case prs of
            (Left _) -> Nothing
            (Right pullreqs) -> do
              firstPR <- pullreqs !? 0

              id <- getPRId firstPR
              title <- getPRTitle firstPR
              body <- getPRBody firstPR

              let repoName = untagName repo
              let repoOwner = untagName owner

              return $ PR body title repoName repoOwner id
  return pr

getLatestPRs :: (?pat :: (Maybe Auth.Auth)) =>
                [Repo] ->
                IO (Maybe PullRequests)

getLatestPRs repos = withTaskGroup 2 $ \g -> do
  prs <- mapConcurrently g getLatest repos
  return $ maybesToMap prs

monitorPRs :: (?pat :: (Maybe Auth.Auth)) =>
              Maybe PullRequests ->
              [Repo] ->
              IO ()
monitorPRs previous repos = do
  currentPRs <- getLatestPRs repos
  client <- connectSession

  -- print currentPRs
  let difference = do {
    currentPRs' <- currentPRs;
    previousPRs' <- previous;
    return $ IntMap.elems (currentPRs' \\ previousPRs');
  }

  maybe (return ())
        (mapM_ (notifyPR client))
        ((map toNote) <$> difference)

  threadDelay tenMinutes
  monitorPRs currentPRs repos

-- Helper functions for converting to an IntMap
prToTuple :: PullRequest -> (Int, PullRequest)

prToTuple pr = (fromIntegral $ prID pr, pr)

maybesToMap :: [Maybe PullRequest] -> Maybe PullRequests

maybesToMap = ((IntMap.fromList .
                map prToTuple ) <$>) .
                sequence
