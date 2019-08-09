-- file src/Data/Time/ISO8601/Time.hs
-- Bindings to the time package's API

{-# LANGUAGE RecordWildCards #-}

module Data.Time.ISO8601.Time where

import Data.Time
import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Clock.System
import Data.Time.LocalTime

import Data.Time.ISO8601.Internal

{-
 UTCTime for actual times
 NominalDiffTime for differences between times, i.e. durations
 Day for something like June 27th 2017
 DayOfWeek for something like Tuesday 
 TimeOfDay for something like 5pm
 LocalTime for a Day with a TimeOfDay
 TimeZone for a time zone offset (not actually the time zone itself) like -0700
 ZonedTime for a LocalTime with a TimeZone
 CalendarDiffDays for something like 6 years, 1 month and 5 days
 CalendarDiffTime for something like 6 years, 1 month, 5 days, 3 hours, 7 minutes and 25.784 seconds
-}

-- UTCTime - an absolute point in time.
toUTCTime :: TimeStamp -> Maybe UTCTime
toUTCTime (TimeStamp {..}) = case toDay date of
  Just utctDay -> return $ UTCTime { utctDayTime = getNumPicosecs time, ..}
  Nothing -> Nothing
  
-- Day - a modified Julian day
toDay :: NominalDate -> Maybe Day
toDay (CalendarDate {..})
  | expCalendarDate == Regular || expCalendarDate == CE = return $ fromJulian (fromIntegral year) month day
  | otherwise = Nothing
toDay (WeekDate {..})
  | expWeekDate == Regular || expWeekDate == CE = return $ fromWeekDate (fromIntegral weekYear) week weekDay   
  | otherwise = Nothing
toDay (OrdinalDate {..})
  | expOrdinalDate == Regular || expOrdinalDate == CE = return $ fromOrdinalDate (fromIntegral ordinalYear) ordinalDay
  | otherwise = Nothing
toDay _ = Nothing         

fromDay :: Day -> (NominalDate, NominalDate, NominalDate)
fromDay d = do
  let
    (year', month, day)        = toJulian d
    (weekYear', week, weekDay) = toWeekDate d
    (ordinalYear', ordinalDay) = toOrdinalDate d

    expCalendarDate = Regular
    expWeekDate     = Regular
    expOrdinalDate  = Regular
    
  ( CalendarDate { year = fromIntegral year', .. },
    WeekDate { weekYear = fromIntegral weekYear', .. }, 
    OrdinalDate { ordinalYear = fromIntegral ordinalYear', .. } )

-- TimeOfDay - some given time of the day
-- make this return Maybe TimeOfDay if we need to change toDiffTime like this
toTimeOfDay :: NominalTime -> TimeOfDay
toTimeOfDay = timeToTimeOfDay . getNumPicosecs

fromTimeOfDay :: TimeOfDay -> NominalTime
fromTimeOfDay = undefined

getNumPicosecs :: NominalTime -> DiffTime
getNumPicosecs (NominalTime {..}) = do
  let picosecondsD = secs * toEnum (10 ^ 12)
  let picoseconds  = fromEnum picosecondsD :: Int
  let totalPicoseconds = fromIntegral (3600 * hours) + fromIntegral (60 * mins) + fromIntegral picoseconds
  picosecondsToDiffTime totalPicoseconds

-- LocalTime
toLocalTime :: ZonedTimeStamp -> LocalTime
toLocalTime (ZonedTimeStamp ts tz) = undefined

fromLocalTime :: LocalTime -> ZonedTimeStamp
fromLocalTime = undefined

-- ZonedTime - LocalTime + TimeZone
toZonedTime :: ZonedTimeStamp -> ZonedTime
toZonedTime = undefined

fromZonedTime :: ZonedTime -> ZonedTimeStamp
fromZonedTime = undefined

-- CalendarDiffDays - a duration without a time in the duration
-- e.g. a period where T onwards is omitted
toCalendarDiffDays :: Duration -> Maybe CalendarDiffDays -- can't guarantee this
toCalendarDiffDays = undefined

fromCalendarDiffDays :: CalendarDiffDays -> Duration
fromCalendarDiffDays = undefined

--CalendarDiffTime - a full duration
toCalendarDiffTime :: Duration -> Maybe CalendarDiffTime
toCalendarDiffTime = undefined

fromCalendarDiffTime :: CalendarDiffTime -> Duration
fromCalendarDiffTime = undefined
