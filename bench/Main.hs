{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main

import Time
import Data.Time
import qualified Data.Time.Format as DTF


main :: IO ()
main = do
  let format = "%Y-%m-%dT%k:%M:%SZ"
  let jformat = "%Y-%m-%dT%k:%M:%SZ"
  let jformatq = "%Y-%m-%dT%k:%M:%S%QZ"
  let b str = parseTimeM True DTF.defaultTimeLocale format str :: Maybe UTCTime
  defaultMain [ bench "juspay parse time to Maybe UTCTime" $ nf (Time.parseTime jformat) "2022-10-02T12:22:32Z"
              , bench "parse time to Maybe UTCTime" $ nf b "2022-10-02T12:22:32Z"
              , bench "parse time with millis to Maybe UTCTime" $ nf (Time.parseTime jformatq) "2022-10-02T12:22:32.223Z"
              , bench "parse day  to Maybe UTCTime" $ nf (Time.parseTime "%Y-%m %d") "2022-10-02"
              ]
