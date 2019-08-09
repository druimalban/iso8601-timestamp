-- file src/Data/Time/ISO8601/Validate.hs
-- Functions for validating dates and times and the like.

{-# LANGUAGE RecordWildCards #-}

module Data.Time.ISO8601.Validate where
{-
import Data.Time
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Clock.System
-}

import Data.Time.LocalTime

import Data.Time.ISO8601.Format
import Data.Time.ISO8601.Internal
import Data.Time.ISO8601.Parse

validYear = (<=) 1

validDay leap m d = case (getMonthLength m leap) of
  Nothing -> False
  Just max -> between 1 max d

validDayGivenYear y m d  = validYear y && validDay (isLeapYear y) m d
validDaySansYear m d = (validDay True m d) || (validDay False m d)

validHour, validMin, validSec :: Ord a => Num a => a -> Bool
validHour = between 0 24
validMin  = between 0 60
validSec  = between 0 60

evalDate :: NominalDate -> Bool
evalDate (CalendarDate {..})  = (not $ (not . isLeapYear) year && (month == 2) && (day == 29)) && (validDayGivenYear year month day) 
evalDate (SpecificMonth {..}) = (monthCodeValid specificMonth) && (validYear yearOfMonth) 
evalDate (SpecificWeek {..})  = (between 1 52 specificWeek) && (validYear yearOfWeek) 
evalDate (WeekDate {..})      = (between 1 7 weekDay) && (between 1 52 week) && (validYear weekYear)
evalDate (OrdinalDate {..})   = ((isLeapYear ordinalYear) && (between 1 366 ordinalDay)) || ((not . isLeapYear) ordinalYear && (between 1 365 ordinalDay)) &&
                                (validYear ordinalYear)
evalDate (SpecificYear {..})  = (validYear specificYear)
evalDate (Century {..})       = (validYear century)

evalTime :: NominalTime -> Bool
evalTime (NominalTime {..})   = validHour hours && validMin mins && validSec secs &&
                                not (hours == 24 && (mins > 0 || secs > 0))
evalTime (SpecificHour {..})  = validHour specificHour
evalTime (SpecificMin {..})   = validHour hourOfMin && validMin specificMin && not (hourOfMin == 24 && specificMin > 0)

{-evalTimeZone (PosOffset {..}) = between 0 24 hoursPlus  && between 0 59 minsPlus  && not (hoursPlus == 24  && minsPlus  > 0)
evalTimeZone (NegOffset {..}) = between 0 24 hoursMinus && between 0 59 minsMinus && not (hoursMinus == 24 && minsMinus > 0)
evalTimeZone _                = True -}

evalTimeZone :: TimeZone -> Bool
evalTimeZone x = True

evalZonedTime :: NominalZonedTime -> Bool
evalZonedTime (NominalZonedTime t tz) = evalTime t && evalTimeZone tz

evalTimeStamp :: TimeStamp -> Bool
evalTimeStamp (TimeStamp {..}) = evalDate date && evalTime time

evalZonedTimeStamp :: ZonedTimeStamp -> Bool
evalZonedTimeStamp (ZonedTimeStamp ts tz) = evalTimeStamp ts && evalTimeZone tz

evalYMD :: PeriodYMD -> Bool
evalYMD (PeriodYMD y m d) = not (y == 0 && m == 0 && d == 0) && y >= 0 && m >= 0 && d >= 0 
evalYMD (PeriodYM y m)    = not (y == 0 && m == 0) && y >= 0 && m >= 0
evalYMD (PeriodY y)       = y > 0   

evalHMS :: PeriodHMS -> Bool
evalHMS (PeriodHMS h m s) = not (h == 0 && m == 0 && s == 0) && h >= 0 && m >= 0 && s >= 0
evalHMS (PeriodHM h m)    = not (h == 0 && m == 0) && h >= 0 && m >= 0
evalHMS (PeriodH h)       = h > 0

evalDuration :: Duration -> Bool
evalDuration (Weeks {..})       = between 0 53 periodWeeks
evalDuration (Duration ymd hms) = evalYMD ymd && evalHMS hms
evalDuration (ShortDur ymd)     = evalYMD ymd
evalDuration (Dated {..}) = evalDate (CalendarDate Regular periodYears periodMonths periodDays) &&
                            evalTime (NominalTime periodHours periodMins (toEnum periodSecs))

evalInterval :: Interval -> Bool
evalInterval (Interval {..}) = evalTimeStamp start && evalTimeStamp end
evalInterval (ToDur {..})    = evalTimeStamp fromStart && evalDuration toDur
evalInterval (FromDur {..})  = evalDuration fromDur && evalTimeStamp toEnd
evalInterval (JustDur dur)   = evalDuration dur

evalRepeat :: Repeat -> Bool
evalRepeat (Repeat {..}) = evalRepeatFor repeatFor && evalInterval interval

evalRepeatFor :: Maybe Int -> Bool
evalRepeatFor (Nothing) = True
evalRepeatFor (Just n)  = n > 0
