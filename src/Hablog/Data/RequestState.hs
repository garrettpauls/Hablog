module Hablog.Data.RequestState
( RequestState(..)
) where

import Hablog.Data.Config (Config)

data RequestState = RequestState
  { requestConfig :: Config
  } deriving Show

