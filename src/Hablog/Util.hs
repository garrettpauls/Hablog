module Hablog.Util
( mkTwo
, mkUTCTime
) where

import Data.Time

mkTwo :: a -> b -> (a, b)
mkTwo a b = (a, b)

mkUTCTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
mkUTCTime year month day hour minute second =
  let utcDay  = fromGregorian year month day
      utcTime = secondsToDiffTime $ toInteger $ (hour * 60 * 60) + (minute * 60) + second
   in UTCTime utcDay utcTime
