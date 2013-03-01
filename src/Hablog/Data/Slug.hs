{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Hablog.Data.Slug
( Slug(..)
, slugify
) where

import Data.Data        (Data, Typeable)
import Database.Persist (PersistField(..))
import Web.Routes       (PathInfo(..))

newtype Slug = Slug { unSlug :: String }
               deriving (Eq, Ord, Read, Show, Data, Typeable, PathInfo, PersistField)

slugChars :: [Char]
slugChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['-']

slugify :: String -> Slug
slugify = Slug . (mkSlug True)
  where
    mkSlug _ [] = []
    mkSlug replace (x:xs)
      | x `elem` slugChars = x:mkSlug True xs
      | otherwise          = (if replace then ['-'] else []) ++ mkSlug False xs

