{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Hablog.Data.Slug
( Slug(..)
, slugify
) where

import Data.Char        (toLower)
import Data.Data        (Data, Typeable)
import Database.Persist (PersistField(..))
import Web.Routes       (PathInfo(..))

newtype Slug = Slug { unSlug :: String }
               deriving (Eq, Ord, Read, Show, Data, Typeable, PathInfo, PersistField)

slugChars :: [Char]
slugChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

slugify :: String -> Slug
slugify = Slug . (mkSlug True)
  where
    mkSlug _ [] = []
    mkSlug replace (x:xs)
      | toLower x `elem` slugChars = toLower x:mkSlug True xs
      | otherwise                  = (if replace then ['-'] else []) ++ mkSlug False xs

