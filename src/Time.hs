{-# LANGUAGE OverloadedStrings #-}

module Time
  ( module X
  , parseTime
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import FlatParse.Basic (Result(..), runParserS)
import Time.Parser as X

parseTime :: Text -> String -> Maybe UTCTime
parseTime format stringifiedDate = go $ runParserS parser stringifiedDate
  where
    parser
      | format == "%Y-%m-%dT%k:%M:%SZ" ||
          format == "%Y-%m-%dT%k:%M:%S%QZ" ||
          format == "%Y-%m-%d %k:%M:%S%Q" || format == "%Y-%m-%d %k:%M:%S" =
        basicParser
      | format == "%Y-%m-%d" ||
          format == "%Y %m %d" || format == "%Y-%m %d" || format == "%Y %m-%d" =
        dayParser
      | otherwise = basicParser
    go (OK r _) = Just r
    go _ = Nothing
