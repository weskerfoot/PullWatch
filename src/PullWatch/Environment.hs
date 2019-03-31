{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

module PullWatch.Environment
    ( getPAT
    , getRepoConfig
    ) where

import System.Environment
import Filesystem.Path

import qualified Data.Yaml as Y
import qualified GitHub.Auth as Auth
import qualified Data.ByteString.Char8 as C

getPAT = do
  (Just pat) <- lookupEnv "PERSONAL_ACCESS_TOKEN"
  return $ Just $ Auth.OAuth $ C.pack pat

getRepoConfig = do
  config <- lookupEnv "PULLWATCH_CONFIG"
  case config of
    Nothing -> return "~/.config/pullwatch.yml"
    Just configPath -> return configPath
