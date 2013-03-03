module Hablog.Data
( Page, PageT
, getConfig, runDatabase
) where

import Hablog.Data.RequestState
import Hablog.Data.Page

type Page = PageT RequestState IO

