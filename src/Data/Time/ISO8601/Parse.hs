{-|
Module      : Data.Time.ISO8601.Parse
Description : Parsing functions, built using regex-applicative
Copyright   : (c) Duncan Guthrie, 2019
License     : BSD 3-Clause
Maintainer  : dguthrie@posteo.net
Stability   : experimental

Parsing functions, built using regex-applicative.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide, not-home #-}

module Data.Time.ISO8601.Parse where

import Control.Monad (replicateM)
import Data.Digits
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.LocalTime
import Lens.Micro.Extras (view)
import Text.Regex.Applicative
import Text.Regex.Applicative.Common

import Data.Time.ISO8601.Internal
import Data.Time.ISO8601.Lens

takeDigits :: Integral a => Int -> RE Char a
takeDigits n = unDigits 10 <$> replicateM n digit

withExtension :: Extension -> Char -> RE Char Char
withExtension Extended c = sym c
withExtension Basic    c = pure c

-- If we are providing a given Representation, check it actually matches.
-- There's not much point parsing w.r.t. Alternative format if the string
-- to parse isn't in the desired format.
-- Return value to multiply against read digits.
checkRepresentation :: Representation -> RE Char Int 
checkRepresentation (Alt _ _ _)   = (sym '+' *> pure 1) <|> (sym '-' *> pure (-1)) 
checkRepresentation (Regular _ _) = pure 1

-- | Parse by year of a given length, depending on the representation.
byYear :: Int -> Representation -> RE Char Integer
byYear nominalLength (Regular _ _) = toInteger <$> takeDigits nominalLength
byYear nominalLength (Alt {..}) =
  (sym '-' *> (negate . toInteger <$> takeDigits (nominalLength + abs additionalLeading))) <|>
  (sym '+' *> (toInteger <$> takeDigits (nominalLength + abs additionalLeading)))

-- | Parse a value of type 'Data.Fixed.Pico'. This is up to 12 digits long. Additional digits will be clipped
takePico :: Int -> Int -> RE Char Pico
takePico agreedLength fracLength = withTail agreedLength fracLength <|> sansTail agreedLength where
  withTail agreedLength fracLength = do
    leading <- takeDigits (abs agreedLength)
    sym '.' <|> sym ','
    frac <- takeDigits (takeLength fracLength)
    pure $ buildPico leading frac (takeLength fracLength)

  sansTail agreedLength = do
    r <- takeDigits (takeLength agreedLength)
    pure $ MkFixed (toInteger (r * 10^12))

  takeLength :: Int -> Int
  takeLength x | abs x > 12 = 12 | otherwise = abs x
  
-- | Parse a value of type 'Data.Fixed.Pico', of arbitrary, unspecified length up to 12 digits as above.
takePico' :: RE Char Pico
takePico' = withTail <|> sansTail where
  withTail = do
    leading <- decimal
    sym '.' <|> sym ','
    fracDigits <- some digit
    pure $ buildPico leading (firstTwelve fracDigits) (conditionalLength fracDigits)
  sansTail = do
    r <- decimal
    pure $ MkFixed (toInteger (r * 10^12))
 
  conditionalLength :: [Int] -> Int
  conditionalLength fd
    | length fd > 12 = 12
    | otherwise = length fd

  firstTwelve :: [Int] -> Int
  firstTwelve fd
    | length fd > 12 = unDigits 10 (take 12 fd)
    | otherwise = unDigits 10 fd

-- | Build a 'Data.Fixed.Pico' value from a given leading integer and a given fractional integer.
buildPico :: Int -> Int -> Int -> Pico
buildPico leading frac fracLength = 
  do let
       leading' = (MkFixed . toInteger) (leading * 10^12) :: Pico
       frac'    = (MkFixed . toInteger) (frac * 10^(12 - fracLength)) :: Pico
     leading' + frac'

-- | Parse a time-zone according to ISO8601-1, section 4.2.5. ('Local time and Coordinated Universal Time (UTC)').
byTimeZone :: Representation -> RE Char (Profile TimeZone)
byTimeZone repr = utc <|> pos <|> neg where
  utc = sym 'Z' *> pure (Profile Basic repr (TimeZone 0 False ""))
  pos = runp Basic <|> runp Extended
  neg = runq Basic <|> runq Extended
  runp ext = do
    sym '+'
    h <- takeDigits 2
    withExtension ext ':'
    m <- takeDigits 2
    pure $ Profile ext repr (TimeZone (60 * h + m) False "")
  runq ext = do
    sym '-'
    h <- takeDigits 2
    withExtension ext ':'
    m <- takeDigits 2
    pure $ Profile ext repr (TimeZone ((-1) * (60 * h + m)) False "")

byTimeOfDay :: Representation -> RE Char (Profile TimeOfDay)
byTimeOfDay repr = run Basic <|> run Extended where
  run ext = do
    todHour <- takeDigits 2
    withExtension ext ':'
    todMin  <- takeDigits 2
    withExtension ext ':'
    todSec  <- takePico 2 (fractionalLength repr)
    pure $ Profile ext repr TimeOfDay {..}

-- | Parse a time of day, according to ISO8601-1, section 4.2.2. ('Complete representations').
byNominalTimeOfDay :: Representation -> RE Char (Profile NominalTime)
byNominalTimeOfDay repr = run Basic <|> run Extended where
  run ext = do
    todHour <- takeDigits 2
    withExtension ext ':'
    todMin  <- takeDigits 2
    withExtension ext ':'
    todSec  <- takePico 2 (fractionalLength repr)
    pure $ Profile ext repr (NominalTime TimeOfDay {..})

-- | Parse a specific minute, according to ISO8601-1, section 4.2.3. ('Representations with reduced precision').
bySpecificMin :: Representation -> RE Char (Profile NominalTime)
bySpecificMin repr = run Basic <|> run Extended where
  run ext = do
    hourOfMin <- takeDigits 2
    withExtension ext ':'
    specificMin <- takePico 2 (fractionalLength repr)
    pure $ Profile ext repr (SpecificMin {..})

-- | Parse a specific hour, according to ISO8601-1, section 4.2.3. ('Representations with reduced precision').
bySpecificHour :: Representation -> RE Char (Profile NominalTime)
bySpecificHour repr = Profile <$> pure Basic <*> pure repr <*> (SpecificHour <$> takePico 2 (fractionalLength repr))

-- | Parse a nominal time representation as in ISO8601-1, section 4.2. ('Time of day').
byNominalTime :: Representation -> RE Char (Profile NominalTime)
byNominalTime repr = byNominalTimeOfDay repr <|> bySpecificHour repr <|> bySpecificMin repr
--

-- | Parse a time-stamp according to ISO8601-1, section 4.3. ('Date and time of a day').
byTimeStamp :: Representation -> RE Char (Profile TimeStamp)
byTimeStamp repr = do
  someDate <- byNominalDate repr
  sym 'T'
  someTime <- byNominalTime repr
  pure $ Profile (getExt (extension someDate) (extension someTime)) repr (TimeStamp (value someDate) (value someTime))

byZonedTimeStamp :: Representation -> RE Char (Profile ZonedTimeStamp)
byZonedTimeStamp repr = do
  someTimeStamp <- byTimeStamp repr
  someTimeZone  <- byTimeZone  repr
  pure $ Profile (getExt (extension someTimeStamp) (extension someTimeZone)) repr (ZonedTimeStamp (value someTimeStamp) (value someTimeZone))

-- | Parse a time-stamp of some number of weeks, according to ISO8601-1, section 4.4.3.2. ('Format with designators').
byCalendarDiffWeeks :: Representation -> RE Char (Profile Duration)
byCalendarDiffWeeks repr = Profile <$> pure Basic <*> pure repr <*> (DiffWeeks <$> (sym 'P' *> decimal <* sym 'W'))

-- | Parse a full time-stamp with yearly, monthly, daily, hourly, minutely, and secondly components, according to ISO8601-1, section 4.4.3.2. ('Format with designators').
byCalendarDiffTime :: Representation -> RE Char (Profile Duration)
byCalendarDiffTime repr
  | isRegular repr = do
      sym 'P'
      numYears  <- toInteger <$> (decimal <* sym 'Y')
      numMonths <- toInteger <$> (decimal <* sym 'M')
      numDays   <- decimal <* sym 'D'
      sym 'T'
      numHours  <- decimal <* sym 'H'
      numMins   <- decimal <* sym 'M'
      numSecs   <- decimal <* sym 'S'
      pure $ Profile Basic repr (Duration (CalendarDiffTime (getMonths numYears numMonths) (getNominalDiffTime numDays numHours numMins numSecs)))
  | not (isRegular repr) = run Basic <|> run Extended where
      run ext = do
        sym 'P'
        numYears  <- toInteger <$> takeDigits 4
        withExtension ext '-'
        numMonths <- toInteger <$> takeDigits 2
        withExtension ext '-'
        numDays   <- takeDigits 2 
        sym 'T'
        numHours   <- takeDigits 2
        withExtension ext ':'
        numMins    <- takeDigits 2
        withExtension ext ':'
        numSecs    <- takeDigits 2
        pure $ Profile ext repr (Duration (CalendarDiffTime (getMonths numYears numMonths) (getNominalDiffTime numDays numHours numMins numSecs)))

-- | Parse a representation of yearly, monthly and daily values where these may be ommited in certain circumstances.
byDiffYearsMonths :: Representation -> RE Char (Profile AcrossYMD)
byDiffYearsMonths repr = ymd repr <|> ym repr <|> y repr where
  ymd repr = do
    numYears  <- toInteger <$> ((decimal <* sym 'Y') <|> pure 0)
    numMonths <- toInteger <$> ((decimal <* sym 'M') <|> pure 0)
    numDays   <- toInteger <$> (decimal <* sym 'D')
    pure $ Profile Basic repr (DiffDays (CalendarDiffDays (getMonths numYears numMonths) numDays))
  ym repr = do
    diffYears  <- toInteger <$> ((decimal <* sym 'Y') <|> pure 0)
    diffMonths <- toInteger <$> decimal <* sym 'M'
    pure $ Profile Basic repr DiffMonths {..}
  y repr = do
    diffYears <- toInteger <$> decimal <* sym 'Y'
    pure $ Profile Basic repr DiffYears {..}

-- | Parse a representation of hourly, minutely and secondly values where these may be ommited in certain circumstances.
byDiffHoursMins :: Representation -> RE Char (Profile AcrossHMS)
byDiffHoursMins repr = hms repr <|> hm repr <|> h repr where
  hms repr = do
    numHours <- (decimal <* sym 'H') <|> pure 0
    numMins  <- (decimal <* sym 'M') <|> pure 0
    numSecs  <- decimal <* sym 'S'
    pure $ Profile Basic repr (DiffSecs (getNominalDiffTime 0 numHours numMins numSecs))
  hm repr = do
    diffHours <- toInteger <$> ((decimal <* sym 'H') <|> pure 0)
    diffMins  <- toInteger <$> decimal <* sym 'M'
    pure $ Profile Basic repr DiffMins {..}
  h repr = do
    diffHours <- toInteger <$> decimal <* sym 'H'
    pure $ Profile Basic repr DiffHours {..}

-- | Parse a reduced accuracy duration representation. Please see ISO8601-1, section 4.4.3.2. ('Format with designators'), rule a).
byShortDuration :: Representation -> RE Char (Profile Duration)
byShortDuration repr = full repr <|> demi repr where
  full repr = do
    sym 'P'
    diffYMD <- value <$> byDiffYearsMonths repr
    sym 'T'
    diffHMS <- value <$> byDiffHoursMins repr
    pure $ Profile Basic repr ShortDuration {..}
  demi repr = do
    sym 'P'
    diffYMD <- value <$> byDiffYearsMonths repr
    pure $ Profile Basic repr (ShortDuration { diffHMS = DiffHours 0, .. })
    
-- | Parse a duration according to ISO8601-1, section 4.4.3. ('Duration').
byDuration :: Representation -> RE Char (Profile Duration)
byDuration repr = byCalendarDiffWeeks repr <|> byCalendarDiffTime repr <|> byShortDuration repr
------

-- | Parse a calendar date according to ISO8601-1, section 4.1.2.2. ('Complete representations').
byCalendarDate :: Representation -> RE Char (Profile NominalDate)
byCalendarDate repr = run Basic <|> run Extended where
  run ext = do
    someYear  <- byYear 4 repr
    withExtension ext '-'
    someMonth <- takeDigits 2
    withExtension ext '-'
    someDay   <- takeDigits 2
    pure $ Profile ext repr (CalendarDate (fromGregorian someYear someMonth someDay))

-- | Parse a weekly date according to ISO8601-1, section 4.1.3. ('Ordinal date').
byWeeklyDate :: Representation -> RE Char (Profile NominalDate)
byWeeklyDate repr = run Basic <|> run Extended where
  run ext = do
    someYear <- byYear 4 repr
    withExtension ext '-'
    sym 'W'
    someWeek <- takeDigits 2
    withExtension ext '-' 
    someWeekDay <- digit
    pure $ Profile ext repr (WeeklyDate (fromWeekDate (someYear) someWeek someWeekDay))

-- | Parse an ordinal date according to ISO8601-1, section 4.1.4 ('Week date').
byOrdinalDate :: Representation -> RE Char (Profile NominalDate)
byOrdinalDate repr = run Basic <|> run Extended where
  run ext = do
    someYear <- byYear 4 repr
    withExtension ext '-'
    someOrdDay <- takeDigits 3
    pure $ Profile ext repr (OrdinalDate (fromOrdinalDate (someYear) someOrdDay))

-- | Parse a specific month according to ISO8601-1, section 4.1.2.3. ('Representations with reduced precision').
bySpecificMonth :: Representation -> RE Char (Profile NominalDate)
bySpecificMonth repr = run Basic <|> run Extended where
  run ext = do
    someYear <- byYear 4 repr
    withExtension ext '-'
    someMonth <- takeDigits 2
    pure $ Profile ext repr (SpecificMonth (someYear) someMonth)

-- | Parse a specific week according to ISO8601-1, section 4.1.2.3. ('Representations with reduced precision').
bySpecificWeek :: Representation -> RE Char (Profile NominalDate)
bySpecificWeek repr = run Basic <|> run Extended where
  run ext = do
    someYear <- byYear 4 repr
    withExtension ext '-'
    sym 'W'
    someWeek <- takeDigits 2
    pure $ Profile ext repr (SpecificWeek (someYear) someWeek)

-- | Parse a specific year accoridng to ISO8601-1, section 4.1.2.3. ('Representations with reduced precision').
bySpecificYear :: Representation -> RE Char (Profile NominalDate)
bySpecificYear repr = Profile <$> pure Basic <*> pure repr <*>
                                (SpecificYear <$> byYear 4 repr)

-- | Parse a specific year according to ISO8601-1, section 4.1.2.3. ('Representations with reduced precision').
bySpecificCentury :: Representation -> RE Char (Profile NominalDate)
bySpecificCentury repr = Profile <$> pure Basic <*> pure repr <*>
                                   (SpecificCentury <$> byYear 2 repr)

-- | Parse any of the nominal date representations defined in ISO8601-1, section 4.1. ('Date').
byNominalDate :: Representation -> RE Char (Profile NominalDate)
byNominalDate repr = bySpecificCentury repr <|> bySpecificYear repr <|> bySpecificMonth repr <|>
                     byOrdinalDate repr <|> byWeeklyDate repr <|> byCalendarDate repr <|>
                     bySpecificWeek repr
------

--byLocalTime :: Int -> Int -> RE Char LocalTime
--byLocalTime agreedLength fracLength = LocalTime <$> (byCalendarDate agreedLength <|> byOrdinalDate agreedLength <|> byWeeklyDate agreedLength) <* sym 'T' <*> byTimeOfDay fracLength

byLocalTime :: Representation -> RE Char (Profile LocalTime)
byLocalTime repr = undefined

--byZonedTime :: Int -> Int -> RE Char ZonedTime
--byZonedTime agreedLength fracLength = ZonedTime <$> byLocalTime agreedLength fracLength <*> byTimeZone

byZonedTime :: Representation -> RE Char (Profile ZonedTime)
byZonedTime repr = undefined

-- | Parse an interval representation according to ISO8601-1, section 4.4. ('Time interval') and ISO8601-1, section 4.4.4. ('Complete representations').
byInterval :: Representation -> RE Char (Profile Interval)
byInterval repr = byInterval' <|> byFromTime <|> byToTime <|> byOverDuration where
  getExt Extended _  = Extended
  getExt _ Extended  = Extended
  getExt Basic Basic = Basic

  byInterval' = do
    someTimeStamp  <- byTimeStamp repr
    sym '/'
    someTimeStamp' <- byTimeStamp repr
    
    pure $ Profile (getExt (extension someTimeStamp) (extension someTimeStamp')) repr
           (Interval (value someTimeStamp) (value someTimeStamp'))

  byFromTime = do
    someTimeStamp <- byTimeStamp repr
    sym '/'
    someDuration  <- byDuration repr
    pure $ Profile (getExt (extension someTimeStamp) (extension someDuration)) repr
           (FromTime (value someTimeStamp) (value someDuration))

  byToTime = do
    someDuration  <- byDuration repr
    sym '/'
    someTimeStamp <- byTimeStamp repr
    pure $ Profile (getExt (extension someDuration) (extension someTimeStamp)) repr
           (ToTime (value someDuration) (value someTimeStamp))

  byOverDuration = do
    someDuration  <- byDuration repr
    pure $ Profile (extension someDuration) repr (OverDuration (value someDuration))
           
-- | Parse a recurring time interval according to ISO8601-1, section 4.5. ('Recurring time interval').
byRepeat :: Representation -> RE Char (Profile Repeat)
byRepeat repr = repeatN <|> repeatForever where
  repeatN = do
    sym 'R'
    repeatBy <- decimal
    sym '/'
    someInterval <- byInterval repr
    pure $ Profile (extension someInterval) repr (Repeat { interval = value someInterval, ..})
  repeatForever = do
    string "R/"
    someInterval <- byInterval repr
    pure $ Profile (extension someInterval) repr (RepeatForever (value someInterval))
