{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide, not-home, ignore-exports #-}

module Base where

import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Lens.Micro
import Lens.Micro.Extras
import Test.QuickCheck hiding (Fixed)
import Test.QuickCheck.All

import Data.Time.ISO8601
import Data.Time.ISO8601.Lens

between i j x = x >= i && x <= j

{-

We start by defining instances of Arbitrary for our various datatypes.

-}

instance Arbitrary Delim where
  arbitrary = elements [Comma, Point]

instance Arbitrary Extension where
  arbitrary = elements [Basic, Extended]

instance Arbitrary Representation where
  arbitrary = oneof [ Regular <$> arbitrary <*> arbitrary
                    , Alt <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Arbitrary TimeZone where
  arbitrary = do
    someHour <- choose (0, 23 :: Int)
    someMin  <- choose (0, 59 :: Int)
    let totalMinutes = someHour + someMin
    TimeZone <$> pure totalMinutes <*> arbitrary <*> pure ""

instance Arbitrary (Profile TimeZone) where
  arbitrary = Profile <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Day where
  arbitrary = suchThat arbitrary (\x -> x >= 0) >>= return . ModifiedJulianDay

instance Arbitrary TimeOfDay where
  arbitrary = do
    someHour <- choose (1, 23 :: Int)
    someMin <- choose (1, 59 :: Int)
    someSec <- MkFixed <$> choose (1, (61 * 10^12) :: Integer)
    TimeOfDay <$> pure someHour <*> pure someMin <*> pure someSec

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

instance Arbitrary ZonedTime where
  arbitrary = ZonedTime <$> arbitrary <*> arbitrary

instance Arbitrary NominalDate where
  arbitrary = oneof [
      CalendarDate    <$> arbitrary
    , WeeklyDate      <$> arbitrary
    , OrdinalDate     <$> arbitrary
    , SpecificYear    <$> arbitraryYear
    , SpecificCentury <$> arbitraryYear
    , SpecificWeek    <$> arbitraryYear <*> arbitraryWeek
    , SpecificMonth   <$> arbitraryYear <*> arbitraryMonth
    ]
    where
      arbitraryWeek  = suchThat arbitrary (between 1 53)
      arbitraryMonth = suchThat arbitrary (between 1 12)
      arbitraryYear  = suchThat arbitrary (/= 0)
    
instance Arbitrary (Profile NominalDate) where
  arbitrary = arbitrary >>=
    (\case
        Regular {..} ->
          oneof [ Profile <$> pure Basic <*> pure Regular {..} <*> years arbitraryYear arbitraryCentury
                , Profile <$> arbitrary  <*> pure Regular {..} <*> extendedYears arbitraryYear
                ]
        Alt {..} ->
          oneof [ Profile <$> pure Basic <*> pure Alt {..} <*> years arbitraryYear' arbitraryYear'
                , Profile <$> arbitrary  <*> pure Alt {..} <*> extendedYears arbitraryYear'
                ]
    )
    where
      years :: Gen Integer -> Gen Integer -> Gen NominalDate
      years arbYear arbCent = oneof [
          SpecificYear    <$> arbYear
        , SpecificCentury <$> arbCent
        ]
      extendedYears :: Gen Integer -> Gen NominalDate
      extendedYears arbYear = oneof [
          CalendarDate  <$> arbitrary
        , WeeklyDate    <$> arbitrary
        , OrdinalDate   <$> arbitrary
        , SpecificWeek  <$> arbYear <*> arbitraryWeek
        , SpecificMonth <$> arbYear <*> arbitraryMonth
        ]
      arbitraryYear    = suchThat arbitrary (between 1 9999)
      arbitraryYear'   = suchThat arbitrary (/= 0)
      arbitraryCentury = suchThat arbitrary (between 1 99)
      arbitraryWeek    = suchThat arbitrary (between 1 53)
      arbitraryMonth   = suchThat arbitrary (between 1 12)

instance Arbitrary NominalTime where
  arbitrary = oneof [ NominalTime  <$> arbitrary
                    , SpecificMin  <$> suchThat arbitrary (between 0 59) <*> (MkFixed <$> choose (1, (61 * 10^12) :: Integer) >>= pure)
                    , SpecificHour <$> suchThat arbitrary (between 0 23) ]

instance Arbitrary (Profile NominalTime) where
  arbitrary = oneof
    [ Profile <$> arbitrary  <*> arbitrary <*>
      (SpecificMin <$> suchThat arbitrary (between 0 59) <*> (MkFixed <$> choose (1, (61 * 10^12) :: Integer) >>= pure))
    , Profile <$> arbitrary  <*> arbitrary <*> (NominalTime <$> arbitrary)
    , Profile <$> pure Basic <*> arbitrary <*> (SpecificHour <$> suchThat arbitrary (between 0 23)) ]

instance Arbitrary CalendarDiffDays where
  arbitrary = do
    diffMonths <- suchThat arbitrary (\x -> x > 0)
    diffDays <- suchThat arbitrary (\x -> x >= 0)
    CalendarDiffDays <$> pure diffMonths <*> pure diffDays

instance Arbitrary CalendarDiffTime where
  arbitrary = do
    diffMonths <- suchThat arbitrary (\x -> x > 0)
    someSecs <- MkFixed <$> choose (0, (61* 10^12) :: Integer)
    CalendarDiffTime <$> pure diffMonths <*> pure (secondsToNominalDiffTime someSecs)

instance Arbitrary NominalDiffTime where
  arbitrary = do
    diffHours <- suchThat arbitrary (\x -> x >= 0)
    diffMins  <- suchThat arbitrary (\x -> x >= 0)
    diffSecs  <- suchThat arbitrary (\x -> x > 0)

    pure $ getNominalDiffTime 0 diffHours diffMins diffSecs

instance Arbitrary (Profile AcrossYMD) where
  arbitrary = choose (1, 3 :: Int) >>=
    (\case 1 -> do
             numYears <- suchThat arbitrary (\x -> x > 0)
             Profile <$> pure Basic <*> (Regular <$> arbitrary <*> arbitrary) <*> (DiffYears <$> pure numYears)
           2 -> do
             numYears  <- suchThat arbitrary (\x -> x >= 0)
             numMonths <- suchThat arbitrary (\x -> x > 0)
             Profile <$> pure Basic <*> (Regular <$> arbitrary <*> arbitrary) <*> (DiffMonths <$> pure numYears <*> pure numMonths)
           3 -> do
             numYears  <- suchThat arbitrary (\x -> x >= 0)
             numMonths <- suchThat arbitrary (between 1 11) -- we will end up with 00YnnMnnD here.
             numDays   <- suchThat arbitrary (\x -> x > 0)

             let numMonths' = getMonths numYears 0          -- we will end up with nnY00MnnD here.
            
             oneof [ Profile <$> pure Basic <*> (Regular <$> arbitrary <*> arbitrary) <*>
                     (DiffDays <$> (CalendarDiffDays <$> pure numMonths <*> pure numDays))
                   , Profile <$> pure Basic <*> (Regular <$> arbitrary <*> arbitrary) <*>
                     (DiffDays <$> (CalendarDiffDays <$> pure numMonths' <*> pure numDays)) ]
    )
instance Arbitrary (Profile AcrossHMS) where
  arbitrary = choose (1, 3 :: Int) >>=
    (\case 1 -> do
             numHours <- suchThat arbitrary (\x -> x > 0)
             Profile <$> pure Basic <*> (Regular <$> arbitrary <*> arbitrary) <*> (DiffHours <$> pure numHours)
           2 -> do
             numHours  <- suchThat arbitrary (\x -> x >= 0)
             numMins <- suchThat arbitrary (\x -> x > 0)
             Profile <$> pure Basic <*> (Regular <$> arbitrary <*> arbitrary) <*> (DiffMins <$> pure numHours <*> pure numMins)
           3 -> do
             numHours <- suchThat arbitrary (\x -> x >= 0)
             numMins  <- suchThat arbitrary (between 1 59)
             numSecs  <- suchThat arbitrary (\x -> x > 0)

             let totalSecs  = getNominalDiffTime 0 0 numMins numSecs -- 00HnnMnnS
             let totalSecs' = getNominalDiffTime 0 numHours 0 numSecs -- nnH00MnnS
             
             oneof [ Profile <$> pure Basic <*> (Regular <$> arbitrary <*> arbitrary) <*> (DiffSecs <$> pure totalSecs)
                   , Profile <$> pure Basic <*> (Regular <$> arbitrary <*> arbitrary) <*> (DiffSecs <$> pure totalSecs') ]
    )
            
instance Arbitrary (Profile Duration) where
  arbitrary = (arbitrary :: Gen Representation) >>=
    (\case
        Regular {..} ->
          oneof [ someShortDur (Regular {..}), someDiffWeeks (Regular {..})
                , someDuration (Regular {..}) ]
        Alt {..} -> someDuration (Alt {..})
    )
    where
      someDiffWeeks (Regular {..}) =
        Profile <$> pure Basic <*> pure Regular {..} <*> (DiffWeeks <$> suchThat arbitrary (\x -> x> 0))
      
      someShortDur (Regular {..}) = do
        pYMD <- arbitrary :: Gen (Profile AcrossYMD)
        pHMS <- arbitrary :: Gen (Profile AcrossHMS)
        
        Profile <$> pure Basic <*> pure Regular {..} <*> (ShortDuration <$> (value <$> arbitrary) <*> (value <$> arbitrary))

      someDuration repr = do
        diffYears  <- suchThat arbitrary (\x -> x > 0)
        diffMonths <- suchThat arbitrary (\x -> x > 0 && (x `mod` 12) /= 0)

        let ctMonths = getMonths diffYears diffMonths
      
        diffDays  <- getDiffDays repr
        diffHours <- suchThat arbitrary (between 1 23)
        diffMins  <- suchThat arbitrary (between 1 59)
        diffSecs  <- suchThat arbitrary (between 1 59)
    
        let ctTime = getNominalDiffTime diffDays diffHours diffMins diffSecs

        Profile <$> getExt repr <*> pure repr <*> (Duration <$> pure CalendarDiffTime {..})
          
          where
            getDiffDays :: Representation -> Gen Int
            getDiffDays (Regular {..})     = suchThat arbitrary (\x -> x > 0)
            getDiffDays (Alt {..}) = suchThat arbitrary (between 1 99)

            getExt :: Representation -> Gen Extension
            getExt (Regular {..})     = pure Basic
            getExt (Alt {..}) = arbitrary
                                     
            nonZeroAll :: Integer -> Bool
            nonZeroAll x = x > 0 &&
              (x `mod` 86400) == 0 &&
              (x `mod` 3600)  == 0 &&
              (x `mod` 60)    == 0
  
instance Arbitrary (Profile TimeStamp) where
  arbitrary = do
    someDate <- arbitrary 
    let ext = extension someDate
    someTime <- suchThat arbitrary (\x -> extension x == ext)
    let repr = bestRepresentation (representation someDate) (representation someTime)
    Profile <$> pure ext <*> pure repr <*> (TimeStamp <$> pure (value someDate) <*> pure (value someTime))

instance Arbitrary (Profile Interval) where
  arbitrary = oneof [ genInterval
                    , genFromTime
                    , genToTime
                    , genOverDuration ]
    where
      genInterval = do       
        someTimeStamp  <- arbitrary :: Gen (Profile TimeStamp)
        let ext  = extension someTimeStamp
        someTimeStamp' <- suchThat arbitrary (\x -> extension x == ext) :: Gen (Profile TimeStamp)
        let repr = bestRepresentation (representation someTimeStamp) (representation someTimeStamp')
        Profile <$> pure ext <*> pure repr <*> (Interval <$> pure (value someTimeStamp) <*> pure (value someTimeStamp))

      genFromTime = do
        someTimeStamp <- arbitrary :: Gen (Profile TimeStamp)
        let ext  = extension someTimeStamp
        someDuration  <- suchThat arbitrary (\x -> extension x == ext) :: Gen (Profile Duration)
        let repr = bestRepresentation (representation someDuration) (representation someTimeStamp)
        Profile <$> pure ext <*> pure repr <*> (FromTime <$> pure (value someTimeStamp) <*> pure (value someDuration))

      genToTime = do
        someDuration <- arbitrary :: Gen (Profile Duration)
        let ext  = extension someDuration
        someTimeStamp <- suchThat arbitrary (\x -> extension x == ext) :: Gen (Profile TimeStamp)
        let repr = bestRepresentation (representation someDuration) (representation someTimeStamp)
        Profile <$> pure ext <*> pure repr <*> (ToTime <$> pure (value someDuration) <*> pure (value someTimeStamp))
        
      genOverDuration = do
        someDuration <- arbitrary :: Gen (Profile Duration)
        let ext  = extension someDuration
        let repr = representation someDuration
        Profile <$> pure ext <*> pure repr <*> (OverDuration <$> pure (value someDuration))  

instance Arbitrary (Profile Repeat) where
  arbitrary = do
    someInterval <- arbitrary :: Gen (Profile Interval)
    let ext  = extension someInterval
    let repr = representation someInterval

    oneof [ Profile <$> pure ext <*> pure repr <*>
            (Repeat <$> suchThat arbitrary (\x -> x > 0) <*> pure (value someInterval))
          , Profile <$> pure ext <*> pure repr <*>
            (RepeatForever <$> pure (value someInterval)) ]
{-

Testable properties.

-}
-- | Test that serialising a given timezone value will produce a compliant value that we may read back.
prop_TimeZone :: Profile TimeZone -> Bool
prop_TimeZone tz = case iso8601Parse (representation tz) (show tz) of
  Nothing  -> False
  Just tz' -> tzMins tz' == tzMins tz
    where tzMins = timeZoneMinutes . value

-- | Test that serialising a given nominal date value will produce a compliant value that we may read back.
prop_NominalDate :: Profile NominalDate -> Bool
prop_NominalDate d = case iso8601Parse (representation d) (show d) of
  Nothing -> False
  Just d' -> d == d'

-- | Test that serialising a given nominal time value will produce a compliant value that we may read back.
prop_NominalTime :: Profile NominalTime -> Bool
prop_NominalTime t = case iso8601Parse (representation t) (show t) of
  Nothing -> False
  Just t' -> roundProfile roundNominalTime t == roundProfile roundNominalTime t'

-- | Test that serialising a given time-stamp value will produce a compliant value that we may read back.
prop_TimeStamp :: Profile TimeStamp -> Bool
prop_TimeStamp ts = case iso8601Parse (representation ts) (show ts) of
  Nothing  -> False
  Just ts' -> roundProfile roundTimeStamp ts == roundProfile roundTimeStamp ts'

-- | Test that serialising a given duration value will produce a compliant value that we may read back.
prop_Duration :: Profile Duration -> Bool
prop_Duration dur = case iso8601Parse (representation dur) (show dur) of
  Nothing ->  False
  Just dur' -> roundProfile roundDuration dur == roundProfile roundDuration dur'

-- | Test that serialising a given interval value will produce a compliant value that we may read back.
prop_Interval :: Profile Interval -> Bool
prop_Interval i = case iso8601Parse (representation i) (show i) of
  Nothing -> False
  Just i' -> roundProfile roundInterval i == roundProfile roundInterval i'

-- | Test that serialising a given repeating interval value will produce a compliant value that we may read back.
prop_Repeat :: Profile Repeat -> Bool
prop_Repeat r = case iso8601Parse (representation r) (show r) of
  Nothing -> False
  Just r' -> roundProfile roundRepeat r == roundProfile roundRepeat r'
{-

  Rounding functions and wrapper for rounding a given Profile of a type

-}
-- | Round a given value wrapped with a Profile.
roundProfile :: (Representation -> a -> a) -> Profile a -> Profile a
roundProfile f x = over _value (f $ view _representation x) x

-- | Round a nominal time value to the given fractional length specified in the representation.
roundNominalTime :: Representation -> NominalTime -> NominalTime
roundNominalTime repr val = case val of
  NominalTime  {..} -> over (_timeOfDay . _todSec) roundFixedTo' val
  SpecificHour {..} -> over _specificHour roundFixedTo' val
  SpecificMin  {..} -> over _specificMin  roundFixedTo' val
  where
    roundFixedTo' :: HasResolution a => Fixed a -> Fixed a
    roundFixedTo' = roundFixedTo (fractionalLength repr)

-- | Round a components of a duration value to the values specified in the representation.
roundDuration :: Representation -> Duration -> Duration
roundDuration repr val = case val of
  Duration {..}  -> over (_duration . _ctTime) roundCalendarDiffTime val
  dur -> dur
  where
    roundFixedTo' :: HasResolution a => Fixed a -> Fixed a
    roundFixedTo' = roundFixedTo (fractionalLength repr)

    roundCalendarDiffTime :: NominalDiffTime -> NominalDiffTime
    roundCalendarDiffTime = secondsToNominalDiffTime . roundFixedTo' . nominalDiffTimeToSeconds

-- | Round the time component of a time-stamp to the fractional length specified in the representation.
roundTimeStamp :: Representation -> TimeStamp -> TimeStamp
roundTimeStamp repr = over _nominalTime (roundNominalTime repr)

-- | Round the duration and time-stamp specified in the interval.
roundInterval :: Representation -> Interval -> Interval
roundInterval repr val = case val of
  Interval {..} -> over _fromTime (roundTimeStamp repr) (over _toTime (roundTimeStamp repr) val)
  FromTime {..} -> over _fromTime (roundTimeStamp repr) (over _overDuration (roundDuration repr) val)
  ToTime   {..} -> over _toTime   (roundTimeStamp repr) (over _overDuration (roundDuration repr) val)
  OverDuration {..} -> over _overDuration (roundDuration repr) val

-- | Round the interval component specified in the repeating interval value.
roundRepeat :: Representation -> Repeat -> Repeat
roundRepeat repr = over _interval (roundInterval repr)
