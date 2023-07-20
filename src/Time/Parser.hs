{-# LANGUAGE TemplateHaskell #-}
module Time.Parser
  ( parserLocalTime
  , parserUTCTime
  , dayParser
  , dayParser'
  ) where

import Data.Time
import qualified FlatParse.Basic as FB
import Data.Fixed (Pico)

-- | Custom parser for the format
-- %Y-%m-%dT%k:%M:%SZ
-- %Y-%m-%dT%k:%M:%S%QZ
-- %Y-%m-%dT%k:%M:%S%Q%Ez

parserLocalTime :: FB.Parser e LocalTime
parserLocalTime = do
  (year, month, day, hr, minutes, sec, msec, curLen) <- basicParserUtil
  let curLen' = if curLen < 0 then 0
                else curLen
      sec'    = (fromIntegral sec :: Pico) + ((fromIntegral msec :: Pico) / (10 ^ curLen' :: Pico))
  pure $
    LocalTime
      (fromGregorian year month day)
      (TimeOfDay (fromIntegral hr :: Int) (fromIntegral minutes :: Int) sec')

parserUTCTime :: FB.Parser e UTCTime
parserUTCTime = do
  (year, month, day, hr, minutes, sec, msec, curLen) <- basicParserUtil
  pure $
   UTCTime
      (fromGregorian year month day)
      (picosecondsToDiffTime (((hr * 60 * 60)  + (minutes * 60) + sec) * (10 ^ 12) + (msec * (10 ^ (12 - curLen)))))

dayParser :: FB.Parser e UTCTime
dayParser = do
  (year, month, day) <- dateParser
  pure $
    UTCTime
      (fromGregorian year month day)
      (secondsToDiffTime 0)

-- Added parser for handling date for this format --> "%d%m%Y" 
dayParser' :: FB.Parser e UTCTime
dayParser' = do
  day <-  FB.isolate 2 FB.anyAsciiDecimalInt
  month <-  FB.isolate 2 FB.anyAsciiDecimalInt
  year <- FB.isolate 4 FB.anyAsciiDecimalInteger
  pure $
   UTCTime
      (fromGregorian year month day)
      (secondsToDiffTime 0)

-- | utils
dateParser :: FB.Parser e (Integer, Int, Int)
dateParser = do
  year <- FB.anyAsciiDecimalInteger
  _ <- FB.satisfy (\x -> x == '-' || x == ' ' || x == '/')
  month <- FB.anyAsciiDecimalInt
  _ <- FB.satisfy (\x -> x == '-' || x == ' ' || x == '/')
  day <- FB.anyAsciiDecimalInt
  pure (year, month, day)

-- common function for parsing time
basicParserUtil :: FB.Parser e (Integer, Int, Int, Integer, Integer, Integer, Integer, Int)
basicParserUtil = do
  (year, month, day) <- dateParser
  _ <- FB.satisfy (\x -> x == '-' || x == ' ' || x == 'T') 
  _ <- FB.many $(FB.char ' ')
  hr <- FB.anyAsciiDecimalInteger
  $(FB.char ':')
  minutes <- FB.anyAsciiDecimalInteger
  $(FB.char ':')
  sec <- FB.anyAsciiDecimalInteger
  FB.Pos curS <- FB.getPos
  msec <- ($(FB.char '.') *> FB.anyAsciiDecimalInteger) FB.<|> pure 0
  FB.Pos curE <- FB.getPos
  FB.try $ $(FB.char 'Z') FB.<|> pure ()
  pure (year, month, day, hr, minutes, sec, msec, curS - curE -1)
