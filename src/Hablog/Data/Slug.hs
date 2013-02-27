{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hablog.Data.Slug
( Slug(..)
, slugify
) where

import Database.Persist (PersistField(..))

newtype Slug = Slug { unSlug :: String }
               deriving (Show, PersistField)

slugChars :: [Char]
slugChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['-']

slugify :: String -> Slug
slugify = Slug . (mkSlug True)
  where
    mkSlug _ [] = []
    mkSlug replace (x:xs)
      | x `elem` slugChars = x:mkSlug True xs
      | otherwise          = (if replace then ['-'] else []) ++ mkSlug False xs

