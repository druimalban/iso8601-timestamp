{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide, not-home #-}

module Data.Time.ISO8601.Internal where

import Control.Applicative
import Data.Digits (digits, unDigits)
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Text.Regex.Applicative (RE)

import Data.Time.ISO8601.TH 

class ISO8601 t where
  iso8601Parse :: Representation -> String -> Maybe t

-- | Basic or extended format.
data Extension =
   Basic | Extended
  deriving (Show, Eq)

-- | Regular or alternative representation.
data Representation =
  -- | Regular format which specificies only the minimum information required for parsing and serialisation.
    Regular { fractionalLength :: Int, delimiter :: Delim }
  {- | Alternative format which specifies agreed additional digits. This can be zero, of course.
       This is specified in /ISO 8601-1/ section 3.5, /Expansion/ and section 3.7, /Mutual agreement/.
  -}
  | Alt { additionalLeading :: Int, fractionalLength :: Int, delimiter :: Delim }
  deriving (Show, Eq)

-- | Profile used for serilisation and parsing so that we know what the agreed profile is.
data Profile t = Profile { extension :: Extension, representation :: Representation, value :: t }

{-instance Show t => Show (Profile t) where
  show (Profile ext repr val) =
    mconcat [ "Profile { extension = "
            , show ext
            , ", representation = "
            , show repr
            , ", value = "
            , show val , " }" ]
  -}  
instance Eq t => Eq (Profile t) where
  (==) (Profile extension representation value) (Profile extension' representation' value') =
    extension == extension' && representation == representation' && value == value'

{- | Delimiters permitted: comma or point.
     A comma the preferred representation, according to /ISO 8601-1/, section 4.2.2.4 /Representations with decimal fraction/. This is assumed to hold where appropriate.
-}

data Delim = Comma | Point deriving (Eq)

instance Show Delim where show Comma = ","
                          show Point = "."


{- | Nominal date: A given date. Full representations may be expressed in terms of 'Data.Time.Calendar.Day'.
     Please see /ISO 8601-1/, section 4.1, /Date/.
-}
data NominalDate
  -- | Full representation year, month, day. Please see /ISO 8601-1/, section 4.1.2.2, /Complete representations/.
  = CalendarDate    { date :: Day }
  -- | Full representation year, week, week-day, where a week is between 1 and 52, and the day is between 1 and 7. Please see /ISO 8601-1/, section 4.1.3, /Ordinal date/.
  | WeeklyDate      { date :: Day }
  -- | Full representation year, ordinal day, where ordinal day is between 1 and 365 (or 366 for a leap year). Please see /ISO 8601-1/ section 4.1.4, /Week date/.
  | OrdinalDate     { date :: Day }
  -- | A specific week without consideration of the week-day. Please see /ISO 8601-1/, section 4.1.4.3, /Representations with reduced precision/.
  | SpecificWeek    { year :: Integer, week :: Int }
  -- | A specific month without consideration of the day. Please see /ISO 8601-1/ section 4.1.2.3, /Representations with reduced precision/.
  | SpecificMonth   { year :: Integer, month :: Int }
  -- | A specific year. Please see /ISO 8601-1/ section 4.1.2.3, /Representations with reduced precision'/.
  | SpecificYear    { year    :: Integer }
  -- | A specific century. Please see /ISO 8601-1/, section 4.1.2.3, /Representations with reduced precision/.
  | SpecificCentury { century :: Integer }
  deriving (Show, Eq)

{- | Nominal time: Some point in time, not considering on which day it occured or in which time-zone. Thus, this is a local time.
     Please see /ISO 8601-1/, section 4.2 /Time of day/.
-}
data NominalTime 
  -- | A full representation may be expressed in terms of time of day 'Data.Time.LocalTime.TimeOfDay'. Please see /ISO 8601-1/, section 4.2.2, /Complete representations/.
  = NominalTime { timeOfDay :: TimeOfDay }
  -- | A specific minute, not at a particular second in that minute. Please see /ISO 8601-1/ section 4.2.3, /Representations with reduced precision/.
  | SpecificMin { hourOfMin :: Int, specificMin :: Pico }
  -- | A specific hour, without consideration of a minute or second in that hour. Please see /ISO 8601-1/, section 4.2.3, /Representations with reduced precision/.
  | SpecificHour { specificHour :: Pico }
  deriving (Show, Eq)

-- | 'Data.Time.ISO8601.NominalTime' annotated with a 'Data.Time.LocalTime.TimeZone'. Please see /ISO 8601-1/, section 4.2.2.5, /Representations with time designator/.
data NominalZonedTime = NominalZonedTime NominalTime TimeZone
  deriving (Show, Eq)

{- | A time-stamp, that is a representation with a date 'Data.Time.ISO8601.NominalDate'
     and time 'Data.Time.ISO8601.NominalTime' component.
     Please see /ISO 8601-1/, section 4.3, /Date and time of a day/
-}
data TimeStamp = TimeStamp { nominalDate :: NominalDate, nominalTime :: NominalTime }
  deriving (Show, Eq)

-- | 'Data.Time.ISO8601.TimeStamp' annotated with a 'Data.Time.LocalTime.TimeZone'. Please see /ISO 8601-1/, section 4.3, /Date and time of a day/.
data ZonedTimeStamp = ZonedTimeStamp { timeStamp :: TimeStamp, timeZone :: TimeZone }
  deriving (Show, Eq)

{-|
   AcrossYMD is some duration across a given number of days, months, and/or years.
   We express these in terms of three components of varying accuracy.
   Having these three components reduces any ambiguities that may arise from reading.
-}
data AcrossYMD
  -- | In some cases these may be ommited. We represent a recording with, at minimum, a day component with 'Data.Time.Calendar.CalendarDiffDays'.
  = DiffDays   { diffDays  :: CalendarDiffDays }
  -- | When this is ommited but a months component is included, we may represent this with, at minimum a months component.
  | DiffMonths { diffYears :: Integer, diffMonths :: Integer }
  -- | Finally, we may have only a years component.
  | DiffYears  { diffYears :: Integer }
  deriving (Show, Eq)

-- | AcrossHMS is some duration across a given number of seconds, minutes and/or hours. The principle behind this is identical to AcrossYMD.
data AcrossHMS = DiffSecs  { diffSecs  :: NominalDiffTime }
               | DiffMins  { diffHours :: Integer, diffMins :: Integer }
               | DiffHours { diffHours :: Integer }
               deriving (Show, Eq)

-- | Represent a duration or period. Please see /ISO 8601-1/, section 4.4.3, /Duration/.
data Duration
  {- | A full representation in terms of 'Data.Time.LocalTime.CalendarDiffTime'.
  May be according to /ISO 8601-1/, section 4.4.3.2, /Format with designators/,
  or /ISO 8601-1/, section 4.4.4.2.2, /Alternative format/.
  -}
  = Duration      { duration  :: CalendarDiffTime }
  -- | A duration of a number of weeks. Please see /ISO 8601-1/, section 4.4.3.2, /Format with designators/.
  | DiffWeeks     { diffWeeks :: Integer }
  {- | A representation where we may omit certain components, that is, with reduced accuracy.
       Please see /ISO 8601-1/, section 4.4.3.2, /Format with designators/.
  -}
  | ShortDuration { diffYMD   :: AcrossYMD
                  , diffHMS   :: AcrossHMS }
              deriving (Show, Eq)

-- | A time interval. Please see /ISO 8601-1/, section 4.4, /Time interval/, and /ISO 8601-1/, section 4.4.4, /Complete representations/.
data Interval
  -- | An interval, in terms of being between two points in time. Please see /ISO 8601-1/, section 4.4.4.1, /Representations of time intervals identified by start and end/.
  = Interval     { fromTime     :: TimeStamp, toTime       :: TimeStamp }
  -- | An interval, over a given duration. Please see /ISO 8601-1/, section 4.4.4.2, /Representations of time intervals by duration and context information/.
  | OverDuration { overDuration :: Duration }
  -- | An interval from a certain point in time over a given duration. Please see /ISO 8601-1/, section 4.4.4.3, /Representations of time interval identified by start and duration/.
  | FromTime     { fromTime     :: TimeStamp, overDuration :: Duration }
  -- | An interval to a certain point in time over a given duration. Please see /ISO 8601-1/, section 4.4.4.4, /Representations of time interval identified by duration and end/.
  | ToTime       { overDuration :: Duration,  toTime       :: TimeStamp }
  deriving (Show, Eq)

-- | We may repeat a given interval some number of times, or indefinitely. Please see /ISO 8601-1/, section 4.5, /Recurring time interval/.
data Repeat = Repeat { repeatBy :: Int, interval :: Interval }
            | RepeatForever { interval :: Interval }
            deriving (Show, Eq)

-- | Produce NominalDiffTime from a given number of days, hours, minutes and seconds.
getNominalDiffTime :: Int -> Int -> Int -> Int -> NominalDiffTime
getNominalDiffTime numDays numHours numMins numSecs = do
  let numSecs'  = toEnum (numSecs * 10^12)
      numMins'  = toEnum (numMins * 60 * 10^12)
      numHours' = toEnum (numHours * 60 * 60 * 10^12)
      numDays'  = toEnum (numDays * 10^12) * nominalDay
  numDays' + numHours' + numMins' + numSecs'

-- | Produce days, hours, minutes and seconds from a given 'Data.Time.LocalTime.NominalDiffTime'
fromNominalDiffTime :: NominalDiffTime -> (Int, Int, Int, Int)
fromNominalDiffTime t = (numDays, numHours, numMins, numSecs)
  where
    totalSecs = fromEnum (nominalDiffTimeToSeconds t) `div` (10^12)
    (numDays,  afterDays)  = quotRem totalSecs 86400
    (numHours, afterHours) = quotRem afterDays 3600
    (numMins,  numSecs)    = quotRem afterHours 60

-- | As above but don't consider the number of days.
fromNominalDiffTime' :: NominalDiffTime -> (Int, Int, Int)
fromNominalDiffTime' t = (numHours, numMins, numSecs) where
  totalSecs = fromEnum (nominalDiffTimeToSeconds t) `div` (10^12)
  (numHours, afterHours) = quotRem totalSecs 3600
  (numMins,  numSecs)    = quotRem afterHours 60

-- | Check a given representation is regular or otherwise.
isRegular :: Representation -> Bool
isRegular (Regular _ _) = True
isRegular _           = False

-- | Get the agreed number of additional leading digits from a given representation. Always assume none have been agreed for a 'regular' representation.
getAgreedLength :: Representation -> Int
getAgreedLength (Alt agreedLength _ _) = abs agreedLength
getAgreedLength _ = 0

-- | We may omit both designator and value for a given zero-value. Show the value and designator only if the value is non-zero.
omitDesignator :: (Eq a, Num a, Show a) => a -> String -> String
omitDesignator val designator
  | val == 0  = ""
  | otherwise = mconcat [ show val, designator ]

-- | Calculate the number of hours, and remaining minutes in some number of minutes.
getTzHrsMins :: Int -> (Int, Int)
getTzHrsMins m = (m `div` 60, m `mod` 60)

-- | Calculate the total number of months in some number of years and months.
getMonths :: Integral a => a -> a -> a
getMonths y m = 12 * y + m

-- | Calculate the number of years, and remaining months in some number of months.
getYearsMonths :: Integral a => a -> (a, a)
getYearsMonths m = (m `div` 12, m `mod` 12)

-- | Check a number is within or at the bounds given.
between :: Num a => Ord a => a -> a -> a -> Bool
between lower upper x = x >= lower && x <= upper

-- | Round half-up a given value.
round' :: RealFrac a => Integral b => a -> b
round' x = floor (x + 0.5)

-- | Round a fixed value to some number of digits after the decimal point.
roundFixedTo :: HasResolution a => Int -> Fixed a -> Fixed a
roundFixedTo fracLength p = do
  let res = (round . logBase 10 . fromIntegral . resolution) p
      n   = res - abs fracLength
      p'  = fromEnum p
      n' | n < 0 = 0 | otherwise = n
      re  = toEnum p' / toEnum 10^n'
  toEnum (round' re * 10^n')


padTo :: Show a => Integral a => Int -> a -> String
padTo n toPad = padTo' n '0' toPad

-- | Pad an integer such that there are at least a given number of leading digits. Will pad with zeroes if applicable.
padTo' :: Show a => Integral a => Int -> Char -> a -> String
padTo' n ch toPad
  | n <= leading (abs toPad) = show toPad
  | otherwise = mconcat [ sign toPad
                        , replicate (n - leading (abs toPad)) ch
                        , show (abs toPad) ] where
      leading :: Integral a => a -> Int
      leading 0 = 1
      leading n = (length . digits 10) n

      sign :: Integral a => a -> String
      sign d | d < 0 = "-"
             | otherwise = ""

{- | Pad a fixed value such that:

1. The portion before the decimal point is padded as for integers above.
2. The portion after the decimal point is rounded to a given fractional length and padded thereafter with zeroes as appropriate.
3. The delimiter is as specificed.

The function gets the following and produces the above result:
1. Round the number we want to pad to the appropriate number of digits.
2. Get the leading length, so we know what to pad.
3. Substitute the delimiter as appropriate.

-}
padFixedTo :: HasResolution a => Int -> Int -> Delim -> Fixed a -> String
padFixedTo leadingDigits fracLength delim toPad = do
  let rounded       = roundFixedTo (abs fracLength) (abs toPad)
      sign          = getSign toPad
      leadingLength = getLeadingLength rounded
      padFrontBy    = getNumToPad leadingDigits leadingLength rounded
      replacedDelim = getRepr leadingDigits fracLength delim rounded

  mconcat [ sign
          , replicate padFrontBy '0'
          , replacedDelim ]
      
  where
    getSign :: HasResolution a => Fixed a -> String
    getSign x | x < 0 = "-" | otherwise = ""

    getLeadingLength :: HasResolution a => Fixed a -> Int
    getLeadingLength = length . digits 10 . truncate . abs
    
    getNumToPad :: HasResolution a => Int -> Int -> Fixed a -> Int
    getNumToPad leadingDigits leadingLength toPad
      | getLeadingLength toPad == 0 = abs leadingDigits - 1
      | getLeadingLength toPad > abs leadingDigits = 0
      | otherwise = abs leadingDigits - getLeadingLength toPad
          
    getRepr :: HasResolution a => Int -> Int -> Delim -> Fixed a -> String
    getRepr leadingDigits fracLength delim d
      | length res <= abs leadingDigits || leadingDigits == 0 = res 
      | getLeadingLength d == 0 =
          uncurry (\x y -> x ++ show delim ++ padStrEnd fracLength (tail y)) splitted
      | otherwise =
          uncurry (\x y -> x ++ show delim ++ padStrEnd fracLength (tail y)) splitted'
      where
        res       = showFixed True (abs d)
        splitted  = splitAt 1 res
        splitted' = splitAt (getLeadingLength d) res

    padStrEnd :: Int -> String -> String
    padStrEnd n str = str ++ replicate (abs n - length str) '0'

-- | Find the 'best' representation, such that it would superceded any 'shorter' representation and not discard any information about the length of the leading digits.
bestRepresentation :: Representation -> Representation -> Representation
bestRepresentation repr repr' = case (repr, repr') of
  (Regular fracLength delim, Regular fracLength' delim') ->
    Regular (bestOf fracLength fracLength') (bestDelimOf delim delim')
    
  (Regular fracLength delim, Alt leadingLength fracLength' delim') ->
    Alt leadingLength (bestOf fracLength fracLength') (bestDelimOf delim delim')

  (Alt leadingLength fracLength delim, Regular fracLength' delim') ->
    Alt leadingLength (bestOf fracLength fracLength') (bestDelimOf delim delim')

  (Alt leadingLength fracLength delim, Alt leadingLength' fracLength' delim') ->
    Alt (bestOf leadingLength leadingLength') (bestOf fracLength fracLength') (bestDelimOf delim delim')

  where
    bestOf :: Int -> Int -> Int
    bestOf x y | abs x == abs y = abs x
               | abs x > abs y  = abs x
               | abs x < abs y  = abs y
    bestDelimOf :: Delim -> Delim -> Delim
    bestDelimOf Comma _     = Comma
    bestDelimOf _     Comma = Comma
    bestDelimOf Point Point = Point

getExt :: Extension -> Extension -> Extension
getExt Basic Basic = Basic
getExt Extended _  = Extended
getExt _ Extended  = Extended
