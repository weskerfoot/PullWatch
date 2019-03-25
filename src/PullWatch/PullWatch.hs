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
import Control.Concurrent (threadDelay)
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.IntMap (IntMap, (\\))
import Data.Default

import qualified GitHub.Endpoints.PullRequests as PR
import qualified GitHub.Auth as Auth
import qualified Data.Text as T
import qualified Data.IntMap as IntMap


type PullRequests = IntMap PullRequest

data PullRequest = PR {
                    prText :: T.Text
                  , prTitle :: T.Text
                  , prRepo :: T.Text
                  , prOwner :: T.Text
                  , prID :: Integer }

                  deriving (Show)

instance Default PullRequest where
  def = PR {
            prText = ""
          , prTitle = ""
          , prRepo = ""
          , prOwner = ""
          , prID = 0
  }

fiveMinutes = 3000000

getPRId = Just . fromIntegral . untagId . simplePullRequestId . (! 0)

getPRTitle = Just . simplePullRequestTitle . (! 0)

getPRBody = simplePullRequestBody . (! 0)

-- Converts a pull request into a dbus notification
toNote :: PullWatch.PullWatch.PullRequest -> Note

toNote pr =
  blankNote {
      summary = T.unpack $ prRepo pr
    , body = (Just $ Text $ T.unpack $ prTitle pr)
    , appImage = (Just $ Icon "dialog-information")
  }

notifyPR client pr = notify client pr

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

getLatestPRs :: (?pat :: (Maybe Auth.Auth)) =>
                [(PR.Name PR.Owner, PR.Name PR.Repo)] ->
                IO (Maybe PullRequests)

getLatestPRs repos = do
  prs <- mapConcurrently (uncurry getLatest) repos
  return $ maybesToMap prs

monitorPRs :: (?pat :: (Maybe Auth.Auth)) =>
              Maybe PullRequests ->
              [(PR.Name PR.Owner, PR.Name PR.Repo)] ->
              IO ()

monitorPRs previous repos = do
  currentPRs <- getLatestPRs repos
  client <- connectSession

  -- print currentPRs
  let difference = do {
    currentPRs' <- currentPRs;
    previousPRs' <- previous;
    return $ IntMap.elems (previousPRs' \\ currentPRs');
  }

  maybe (return ()) (mapM_ (notifyPR client)) ((map toNote) <$> difference)

  threadDelay fiveMinutes
  monitorPRs currentPRs repos

-- Helpers functions for converting to an IntMap
prToTuple :: PullRequest -> (Int, PullRequest)

prToTuple pr = (fromIntegral $ prID pr, pr)

maybesToMap :: [Maybe PullRequest] -> Maybe PullRequests

maybesToMap = ((IntMap.fromList .
                map prToTuple ) <$>) .
                sequence
