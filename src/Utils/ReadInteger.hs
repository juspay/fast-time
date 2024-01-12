
module Utils.ReadInteger where

import FlatParse.Basic as FB
import Data.Time.Calendar ( DayOfMonth, MonthOfYear, Year )

readInt :: Parser e Int
readInt     = FB.anyAsciiDecimalInt

readInteger :: Parser e Integer
readInteger = FB.anyAsciiDecimalInteger

readDayOfMonth :: Parser e DayOfMonth
readDayOfMonth = FB.anyAsciiDecimalInt

readMonthOfYear :: Parser e MonthOfYear
readMonthOfYear = FB.anyAsciiDecimalInt

readYear :: Parser e Year
readYear = FB.anyAsciiDecimalInteger
