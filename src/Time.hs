{-# LANGUAGE OverloadedStrings #-}

module Time
  ( module X
  , parseTime
  , parseLTime
  ) where

import Data.Text (Text)
import Data.Time (UTCTime, LocalTime)
import FlatParse.Basic (Result(..), runParser)
import Time.Parser as X
import qualified Data.ByteString.Char8 as B

parseTime :: Text -> String -> Maybe UTCTime
parseTime format stringifiedDate = go $ runParser parser (B.pack stringifiedDate)
  where
    parser
      | format == "%Y-%m-%dT%k:%M:%SZ" ||
          format == "%Y-%m-%dT%k:%M:%S%QZ" ||
          format == "%Y-%m-%d %k:%M:%S%Q" || format == "%Y-%m-%d %k:%M:%S" =
        parserUTCTime
      | format == "%Y-%m-%d" ||
          format == "%Y %m %d" || format == "%Y-%m %d" || format == "%Y %m-%d" || format == "%Y/%-m/%-d" =
        dayParser
      | format == "%d%m%Y" = dayParser'
      | otherwise = parserUTCTime
    go (OK r _) = Just r
    go _ = Nothing

parseLTime :: Text -> String -> Maybe LocalTime 
parseLTime format stringifiedDate = go $ runParser parser (B.pack stringifiedDate)
  where
    parser 
      |  format == "%Y-%m-%dT%k:%M:%S%Q%Ez" =
        parserLocalTime
      | otherwise = parserLocalTime
    go (OK r _) = Just r
    go _ = Nothing
