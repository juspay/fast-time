{-# LANGUAGE TemplateHaskell #-}
module Time.Parser
  ( parserLocalTime
  , parserUTCTime
  , dayParser
  , dayParser'
  ) where

import Data.Time
import FlatParse.Basic
import Data.Fixed (Pico)

-- | Custom parser for the format
-- %Y-%m-%dT%k:%M:%SZ
-- %Y-%m-%dT%k:%M:%S%QZ
-- %Y-%m-%dT%k:%M:%S%Q%Ez

parserLocalTime :: Parser e LocalTime
parserLocalTime = do 
  (year, month, day, hr, minutes, sec) <- basicParserUtil
  pure $
    LocalTime
      (fromGregorian year month day)
      (TimeOfDay (fromIntegral hr :: Int) (fromIntegral minutes :: Int) (fromIntegral sec :: Pico))

parserUTCTime :: Parser e UTCTime
parserUTCTime = do 
  (year, month, day, hr, minutes, sec) <- basicParserUtil
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

-- Added parser for handling date for this format --> "%d%m%Y" 
dayParser' :: Parser e UTCTime
dayParser' = do 
  day <- isolate 2 readInt 
  month <- isolate 2 readInt
  year <- isolate 4 readInteger 
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

-- common function for parsing time
basicParserUtil :: Parser e (Integer, Int, Int, Integer, Integer, Integer)
basicParserUtil = do
  (year, month, day) <- dateParser
  satisfy_ (\x -> x == '-' || x == ' ' || x == 'T')
  many_ $(char ' ')
  hr <- readInteger
  $(char ':')
  minutes <- readInteger
  $(char ':')
  sec <- readInteger
  _ <- ($(char '.') *> readInt) <|> pure 0 
  (try $ $(char 'Z') <|> pure ())
  pure (year, month, day, hr, minutes, sec)
