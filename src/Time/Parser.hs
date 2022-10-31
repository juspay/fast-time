{-# LANGUAGE TemplateHaskell #-}
module Time.Parser
  ( basicParser
  , dayParser
  ) where

import Data.Time
import FlatParse.Basic

-- | Custom parser for the format
-- %Y-%m-%dT%k:%M:%SZ
-- %Y-%m-%dT%k:%M:%S%QZ
basicParser :: Parser e UTCTime
basicParser = do
  (year, month, day) <- dateParser
  satisfy_ (\x -> x == '-' || x == ' ' || x == 'T')
  many_ $(char ' ')
  hr <- readInteger
  $(char ':')
  minutes <- readInteger
  $(char ':')
  sec <- readInteger
  _ <- ($(char '.') *> readInt) <|> (pure 0)
  (try $ $(char 'Z') <|> pure ())
  pure $
    UTCTime
      (fromGregorian year month day)
      (secondsToDiffTime $ (hr * 60 * 60) + (minutes * 60) + sec)

dayParser :: Parser e UTCTime
dayParser = do
  (year, month, day) <- dateParser
  pure $
    UTCTime
      (fromGregorian year month day)
      (secondsToDiffTime 0)

-- | utils
dateParser :: Parser e (Integer, Int, Int)
dateParser = do
  year <- readInteger
  satisfy_ (\x -> x == '-' || x == ' ')
  month <- readInt
  satisfy_ (\x -> x == '-' || x == ' ')
  day <- readInt
  pure $ (year, month, day)
