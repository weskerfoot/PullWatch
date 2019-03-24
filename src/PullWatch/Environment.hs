{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

module PullWatch.Environment
    ( getPAT
    ) where

import System.Environment

import qualified GitHub.Auth as Auth
import qualified Data.ByteString.Char8 as C

getPAT = do
  (Just pat) <- lookupEnv "PERSONAL_ACCESS_TOKEN"
  return $ Just $ Auth.OAuth $ C.pack pat
