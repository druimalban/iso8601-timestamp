{-|
Module      : Data.Time.ISO8601.Format
Description : Reasonable formatting for our various datatypes
Copyright   : (c) Duncan Guthrie, 2019
License     : BSD 3-Clause
Maintainer  : dguthrie@posteo.net
Stability   : experimental

Reasonable formatting for our various datatypes
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide, not-home #-}

module Data.Time.ISO8601.Format where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.LocalTime
import Lens.Micro.Extras (view)
import Text.Regex.Applicative (match)

import Data.Time.ISO8601.Internal
import Data.Time.ISO8601.Parse

dash, colon :: Extension -> String
dash Basic = ""
dash Extended = "-"
colon Basic = ""
colon Extended = ":"

-- | Given a year, print the appropriate sign if we're considering an ext reresentation.
showAltSign :: Integral a => Representation -> a -> String
showAltSign (Regular _ _) _ = ""
showAltSign (Alt _ _ _) y
  | y >= 0    = "+"
  | otherwise = ""

-- | Pretty-print the properties of the profile wrapping a given type, as well as its ISO 8601 representation.
showProfilePretty :: ISO8601 (Profile t) => Show (Profile t) => Show t => Profile t -> String
showProfilePretty profile =
  mconcat [ "Profile level: Profile | Any format extenstion: ", show (extension profile), " | "
          , "Representation: ", show (representation profile), " | Wrapped type: ", show (value profile), " | "
          , "ISO 8601-1 representation: ", show profile ]

instance Show (Profile TimeZone) where
  show (Profile ext repr TimeZone {..})
    | timeZoneMinutes == 0 = "Z"
    | otherwise = do
        let (h, m) = getTzHrsMins timeZoneMinutes
        mconcat [ getPrefix timeZoneMinutes
                , padTo 2 (abs h)
                , colon ext
                , padTo 2 m ]
          where
            getPrefix m | m > 0 = "+" | otherwise = "-"

instance Show (Profile NominalDate) where
  show (Profile ext repr d) = case d of
    SpecificWeek {..} -> mconcat [ showAltSign repr year
                                 , padTo (4 + getAgreedLength repr) year
                                 , dash ext, "W"
                                 , padTo 2 week ]
    
    SpecificMonth {..} -> mconcat [ showAltSign repr year
                                  , padTo (4 + getAgreedLength repr) year
                                  , dash ext
                                  , padTo 2 month ]
  
    SpecificYear {..} -> mconcat [ showAltSign repr year
                                 , padTo (4 + getAgreedLength repr) year ]

    SpecificCentury {..} -> mconcat [ showAltSign repr century
                                    , padTo (2 + getAgreedLength repr) century ]

    CalendarDate {..} -> mconcat [ showAltSign repr y
                                 , padTo (4 + getAgreedLength repr) y
                                 , dash ext
                                 , padTo 2 m
                                 , dash ext
                                 , padTo 2 d ] where (y, m, d) = toGregorian date
                                                   
    WeeklyDate {..} -> mconcat [ showAltSign repr y
                               , padTo (4 + getAgreedLength repr) y
                               , dash ext
                               , "W"
                               , padTo 2 w
                               , dash ext
                               , padTo 1 d ] where (y, w, d) = toWeekDate date

    OrdinalDate {..} -> mconcat [ showAltSign repr y
                                , padTo (4 + getAgreedLength repr) y
                                , dash ext
                                , padTo 3 d ] where (y, d) = toOrdinalDate date
                                  
instance Show (Profile NominalTime) where
  show (Profile ext repr t) = case t of
    SpecificMin {..} ->
      mconcat [ padTo 2 hourOfMin
              , colon ext
              , padFixedTo 2 fixedLength delim specificMin ]
      where fracLength = abs (fractionalLength repr)
            delim      = delimiter repr
            fixedLength | abs fracLength > 12 = 12 | otherwise = abs fracLength
    SpecificHour {..} ->
      padFixedTo 2 fixedLength delim specificHour
      where
        fracLength = abs (fractionalLength repr)
        delim      = delimiter repr
        fixedLength | abs fracLength > 12 = 12 | otherwise = abs fracLength
    NominalTime TimeOfDay {..} ->
      show (Profile ext repr TimeOfDay {..})

instance Show (Profile TimeOfDay) where
  show (Profile ext repr TimeOfDay {..}) =
    mconcat [ padTo 2 todHour
            , colon ext
            , padTo 2 todMin
            , colon ext
            , padFixedTo 2 fixedLength delim todSec ]
    where fracLength = abs (fractionalLength repr)
          delim      = delimiter repr
          fixedLength | abs fracLength > 12 = 12 | otherwise = abs fracLength

instance Show (Profile LocalTime) where
  show (Profile ext repr LocalTime {..}) =
    mconcat [ show (Profile ext repr (CalendarDate localDay))
            , "T"
            , show (Profile ext repr localTimeOfDay) ]

instance Show (Profile ZonedTime) where
  show (Profile ext repr ZonedTime {..}) =
    mconcat [ show (Profile ext repr zonedTimeToLocalTime)
            , show (Profile ext repr zonedTimeZone) ]


instance Show (Profile TimeStamp) where
  show (Profile ext repr TimeStamp {..}) =
    mconcat [ show (Profile ext repr nominalDate)
            , "T"
            , show (Profile ext repr nominalTime) ]

instance Show (Profile ZonedTimeStamp) where
  show (Profile ext repr (ZonedTimeStamp ts tz)) =
    mconcat [ show (Profile ext repr ts)
            , show (Profile ext repr tz) ]

instance Show (Profile Duration) where
  show (Profile Basic repr (DiffWeeks {..})) =
    mconcat [ "P"
            , show diffWeeks
            , "W" ]
  show (Profile Basic repr (ShortDuration {..})) = do
    let ymd = showAcrossYMD diffYMD
    let hms = showAcrossHMS diffHMS
    mconcat [ "P"
            , ymd
            , checkIgnored hms ]
      where
        showAcrossYMD :: AcrossYMD -> String
        showAcrossYMD (DiffYears {..})  = mconcat [ show diffYears, "Y" ]
        showAcrossYMD (DiffMonths {..}) = mconcat [ omitDesignator diffYears "Y", show diffMonths, "M" ]
        showAcrossYMD (DiffDays {..})   = mconcat [ omitDesignator y "Y"
                                                  , omitDesignator m "M"
                                                  , show d         , "D" ]
          where (y, m) = (getYearsMonths . cdMonths) diffDays
                d      = cdDays diffDays 

        showAcrossHMS :: AcrossHMS -> String
        showAcrossHMS (DiffHours {..}) = mconcat [ show diffHours, "H" ]
        showAcrossHMS (DiffMins {..})  = mconcat [ omitDesignator diffHours "H", show diffMins, "M" ]
        showAcrossHMS (DiffSecs {..})  = mconcat [ omitDesignator h "H"
                                                 , omitDesignator m "M"
                                                 , show s         , "S" ]
          where (h, m, s) = fromNominalDiffTime' diffSecs

        checkIgnored :: String -> String
        checkIgnored ""  = ""
        checkIgnored str = mconcat [ "T", str ]

  show (Profile ext repr (Duration CalendarDiffTime {..})) =
    do
      let (yr, mo) = getYearsMonths ctMonths
          (d, h, m, s) = fromNominalDiffTime ctTime
      case repr of
        (Regular _ _) -> do
          mconcat [ "P"
                  , omitZero 4 "Y" yr
                  , omitZero 2 "M" mo
                  , omitZero 2 "D" d
                  , "T"
                  , omitZero 2 "H" h
                  , omitZero 2 "M" m
                  , omitZero 2 "S" s ]

        (Alt agreedLength _ _) -> do
          mconcat ["P"
                  , padTo 4 yr, dash ext
                  , padTo 2 mo, dash ext
                  , padTo 2 d
                  , "T"
                  , padTo 2 h, colon ext
                  , padTo 2 m, colon ext
                  , padTo 2 s ]
        where
          omitZero :: Show a => Integral a => Int -> String -> a -> String
          omitZero agreedLength designator n
            | n == 0    = ""
            | otherwise = show n ++ designator

instance Show (Profile Interval) where
  show (Profile ext repr i) =
    case i of
      Interval {..} ->
        mconcat [ show (Profile ext repr fromTime)
                , "/"
                , show (Profile ext repr toTime) ]
      FromTime {..} ->
        mconcat [ show (Profile ext repr fromTime)
                , "/"
                , show (Profile ext repr overDuration) ]
      ToTime {..} ->
        mconcat [ show (Profile ext repr overDuration)
                , "/"
                , show (Profile ext repr toTime) ]
      OverDuration {..} -> show (Profile ext repr overDuration)

instance Show (Profile Repeat) where
  show (Profile ext repr r) = case r of
    Repeat {..} -> "R" ++ show repeatBy ++ "/" ++ show (Profile ext repr interval)
    RepeatForever {..} -> "R/" ++ show (Profile ext repr interval)

instance ISO8601 (Profile TimeZone) where
  iso8601Parse = match . byTimeZone
instance ISO8601 (Profile NominalDate) where
  iso8601Parse = match . byNominalDate
instance ISO8601 (Profile NominalTime) where
  iso8601Parse = match . byNominalTime
instance ISO8601 (Profile TimeOfDay) where
  iso8601Parse = match . byTimeOfDay
instance ISO8601 (Profile TimeStamp) where
  iso8601Parse = match . byTimeStamp
instance ISO8601 (Profile ZonedTimeStamp) where
  iso8601Parse = match . byZonedTimeStamp
instance ISO8601 (Profile Duration) where
  iso8601Parse = match . byDuration
instance ISO8601 (Profile Interval) where
  iso8601Parse = match . byInterval
instance ISO8601 (Profile Repeat) where
  iso8601Parse = match . byRepeat
