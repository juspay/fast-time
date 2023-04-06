{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Time
import Data.Time
import qualified Data.Time.Format as DTF
import Data.Text as T

main :: IO ()
main = do
  let format = "%Y-%m-%dT%k:%M:%SZ"
      jformat = "%Y-%m-%dT%k:%M:%SZ"
      jformatq = "%Y-%m-%dT%k:%M:%S%QZ"
      ezFormat = "%Y-%m-%dT%k:%M:%S%Q%Ez"
      dformat = "%d%m%Y"
      dformat2 = "%Y-%m-%d"
      dformat3 = "%Y/%-m/%-d"
      b str = parseTimeM True DTF.defaultTimeLocale (T.unpack jformatq) str :: Maybe UTCTime
      c str = parseTimeM True DTF.defaultTimeLocale (T.unpack ezFormat) str :: Maybe LocalTime
      x str = parseTimeM True DTF.defaultTimeLocale dformat str :: Maybe UTCTime
      y str = parseTimeM True DTF.defaultTimeLocale dformat2 str :: Maybe UTCTime 
      z str = parseTimeM True DTF.defaultTimeLocale dformat3 str :: Maybe UTCTime
  defaultMain [ bench "fast parse time to Maybe UTCTime" $ nf (Time.parseTime jformat) "2022-10-02T12:22:32Z"
              , bench "parse time to Maybe UTCTime using Data.Time package" $ nf b "2022-10-02T12:22:32Z"
              , bench "fast parse time with millis to Maybe UTCTime" $ nf (Time.parseTime jformatq) "2022-10-02T12:22:32.223Z"
              , bench "parse time to Maybe millis UTCTime using Data.Time package" $ nf b "2022-10-02T12:22:32.223Z"
              , bench "fast parse day  to Maybe UTCTime" $ nf (Time.parseTime "%Y-%m %d") "2022-10-02"
              , bench "parse day using the Data.Time package" $ nf y "2022-10-02"
              , bench "fast parse time to Maybe LocalTime" $ nf (Time.parseLTime ezFormat)  "2020-09-10T16:01:41.395+05:30"
              , bench "parse time to Maybe LocalTime using Data.Time package" $ nf c "2020-09-10T16:01:41.395+05:30"
              , bench "fast parse day to Maybe UTCTime(%d%m%Y)" $ nf (Time.parseTime (T.pack dformat)) "25012023"
              , bench "parse day to Maybe UTCTime using Data.Time package" $ nf x "25012023"
              , bench "fast parse day to Maybe UTCTime(%Y/%-m/%-d)" $ nf (Time.parseTime "%Y/%-m/%-d") "2023/12/09"
              , bench "parse day using Data.Time package" $ nf z "2023/12/09"
              ]
