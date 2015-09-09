module Layout00001 (
    everyNth
) where

import Data.Either
import Data.Time.Calendar

everyNth :: Int -> Either WeekDay Int -> Day -> [Day]
everyNth n tp start = case tp of
    WeekDay -> start : everyNth tp (addDays n start)
    Int     -> let (y, m, d) = toGregorian(start) start : everyNth tp ()
