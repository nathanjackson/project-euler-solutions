-- Project Euler
-- Problem 19
-- Nathan Jackson

-- Calculating the day of week via the Gregorian calendar is described here:
-- http://calendars.wikia.com/wiki/Calculating_the_day_of_the_week

import Data.Time

-- Look up the century term.
centuryTerm :: Integer -> Integer
centuryTerm year
    | year `div` 100 == 17  = 4
    | year `div` 100 == 18  = 2
    | year `div` 100 == 19  = 0
    | year `div` 100 == 20  = 6
    | year `div` 100 == 21  = 4
    | otherwise             = error "Unhandled year."

-- Look up the month term.  The year is required because January and February's
-- values change depending on whether or not we are in a leap year.
monthTerm :: Integer -> Integer -> Integer
monthTerm year month
    | month == 1 && isLeapYear year         = 6
    | month == 1 && not (isLeapYear year)   = 0
    | month == 2 && isLeapYear year         = 2
    | month == 2 && not (isLeapYear year)   = 3
    | month == 3                            = 3
    | month == 4                            = 6
    | month == 5                            = 1
    | month == 6                            = 4
    | month == 7                            = 6
    | month == 8                            = 2
    | month == 9                            = 5
    | month == 10                           = 0
    | month == 11                           = 3
    | month == 12                           = 5
    | otherwise                             = error "Invalid month."

-- Calculates the day of the week for the given gregorian date.
-- Returns 0-6, Sunday-Saturday.
dayOfWeek :: (Integer, Int, Int) -> Integer
dayOfWeek (year, month, day) = ((centuryTerm year) + (year `mod` 100)
    + ((year `mod` 100) `div` 4) + (monthTerm year (toInteger month)) + (toInteger day)) `mod` 7

-- Returns the day of the month from a gregorian format date of
-- (year, month, day).
dayOfMonth :: (Integer,Int,Int) -> Int
dayOfMonth (_,_,day) = day 

-- Count the Sundays that fall on the first of the month between an inclusive
-- range of dates.
countFirstOfMonthSundays :: Day -> Day -> Integer
countFirstOfMonthSundays begin end = recSundays' begin
    where recSundays' day =
            if day == end
                then countSundays' day
                else (countSundays' day) + recSundays' (addDays 1 day)
          countSundays' day =
            let gregorian = toGregorian day
            in if dayOfWeek gregorian == 0 && dayOfMonth gregorian == 1
               then 1
               else 0

main :: IO ()
main = do
    putStrLn $ show $ countFirstOfMonthSundays (fromGregorian 1901 1 1) (fromGregorian 2000 12 31)
