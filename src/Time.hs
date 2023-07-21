{-# LANGUAGE OverloadedStrings #-}

module Time
  ( module X
  , parseTime
  , parseLTime
  ) where
 
import Data.Text (Text)
import Data.Time (UTCTime, LocalTime)
import qualified FlatParse.Basic as FB
import Time.Parser as X

parseTime :: Text -> String -> Maybe UTCTime
parseTime format stringifiedDate =  go $ FB.runParser parser $ FB.strToUtf8 stringifiedDate
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
    go (FB.OK r _) = Just r
    go _ = Nothing

parseLTime :: Text -> String -> Maybe LocalTime 
parseLTime format stringifiedDate =  go $ FB.runParser parser  $ FB.strToUtf8 stringifiedDate
  where
    parser 
      |  format == "%Y-%m-%dT%k:%M:%S%Q%Ez" =
        parserLocalTime
      | otherwise = parserLocalTime
    go (FB.OK r _) = Just r
    go _ = Nothing
