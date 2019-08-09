{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Extended where

import Control.Monad
import Data.Time.Calendar
import Test.QuickCheck
import Test.QuickCheck.All

import Data.Time.ISO8601.Extended

between i j x = x >= i && x <= j
between' i j  = suchThat arbitrary (between i j)

instance Arbitrary NominalDate' where
  arbitrary = oneof [ y, m, d ] where
    y = do
      year  <- between' 1 9999
      month <- between' 1 12
      day   <- between' 1 (gregorianMonthLength year month)
      pure (CalendarDate' {..})
    m = SpecificMonth' <$> between' 1 9999 <*> between' 1 12
    d = SpecificYear'  <$> between' 1 9999

-- Probably not that useful
instance Arbitrary t => Arbitrary (Provenance t) where
  arbitrary = oneof [ Reliable    <$> arbitrary
                    , Uncertain   <$> arbitrary
                    , Approximate <$> arbitrary
                    , Unreliable  <$> arbitrary ]

instance Arbitrary t => Arbitrary (Context t) where
  arbitrary = oneof [ Context <$> arbitrary
                    , pure Unknown
                    , pure Open ]

instance (Integral t, Arbitrary t) => Arbitrary (Unspecified t) where
  arbitrary = oneof [ pure Unspecified, Specified <$> between' 1 9 ]

instance Arbitrary NominalDate2 where
  arbitrary = oneof [ y, m, d ] where
    d = do
      year2 <- oneof [ Reliable    <$> someYear
                     , Uncertain   <$> someYear
                     , Approximate <$> someYear
                     , Unreliable  <$> someYear ]
      month2 <- oneof [ Reliable    <$> someMonth
                      , Uncertain   <$> someMonth
                      , Approximate <$> someMonth
                      , Unreliable  <$> someMonth ]
      day2 <- oneof [ Reliable <$> someDay year2 month2
                    , Uncertain <$> someDay year2 month2
                    , Approximate <$> someDay year2 month2
                    , Unreliable <$> someDay year2 month2]
      pure (CalendarDate2 {..})
    m = do
      year2 <- oneof [ Reliable    <$> someYear
                     , Uncertain   <$> someYear
                     , Approximate <$> someYear
                     , Unreliable  <$> someYear ]
      month2 <- oneof [ Reliable    <$> someMonth
                      , Uncertain   <$> someMonth
                      , Approximate <$> someMonth
                      , Unreliable  <$> someMonth ]
      pure (SpecificMonth2 {..})
    y = SpecificYear2 <$> oneof [ Reliable    <$> someYear
                                , Uncertain   <$> someYear
                                , Approximate <$> someYear
                                , Unreliable  <$> someYear ]
    someYear = between' 1 9999
    someMonth = between' 1 12
    someDay y m = between' 1 (gregorianMonthLength (provenanceOf y) (provenanceOf m))

instance Arbitrary UnspecifiedDate1 where
  arbitrary = oneof [ y, m, d ] where
    d = oneof [ UnspecifiedDay1   <$> pure Unspecified <*> pure Unspecified <*> pure Unspecified   -- XXXX-XX-XX
              , UnspecifiedDay1   <$> between'' 1 9999 <*> pure Unspecified <*> pure Unspecified   -- YYYY-XX-XX
              , UnspecifiedDay1   <$> between'' 1 9999 <*> between'' 1 12   <*> pure Unspecified ] -- YYYY-MM-XX
    m = oneof [ UnspecifiedMonth1 <$> pure Unspecified <*> pure Unspecified 
              , UnspecifiedMonth1 <$> between'' 1 9999 <*> pure Unspecified ] 
    y = oneof [ UnspecifiedYear1  <$> pure [ Unspecified, Unspecified, Unspecified, Unspecified ] -- XXXX
              , UnspecifiedYear1  <$> someYear          -- YYXX
              , UnspecifiedYear1  <$> someYear' ] -- YYYX
    someYear = do
      y1 <- Specified <$> between' 1 9
      y2 <- Specified <$> between' 1 9
      pure [ y1, y2, Unspecified, Unspecified ]
    someYear' = do
      y1 <- Specified <$> between' 1 9
      y2 <- Specified <$> between' 1 9
      y3 <- Specified <$> between' 1 9
      pure [ y1, y2, y3, Unspecified ]
    between'' i j = Specified <$> between' i j

instance Arbitrary UnspecifiedDate2 where
  arbitrary =
    oneof [ UnspecifiedDay2   <$> replicateM 4 arbitrary <*> replicateM 2 arbitrary <*> replicateM 2 arbitrary
          , UnspecifiedMonth2 <$> replicateM 4 arbitrary <*> replicateM 2 arbitrary
          , UnspecifiedYear2  <$> replicateM 4 arbitrary ]

instance Arbitrary t => Arbitrary (BeforeOrAfter t) where
  arbitrary =
    oneof [ BeforeOrOn  <$> arbitrary
          , AfterOrOn   <$> arbitrary
          , CertainlyOn <$> arbitrary ]

instance Arbitrary Interval1 where
  arbitrary = Interval1 <$> arbitrary <*> arbitrary

instance Arbitrary Context' where
  arbitrary =
    oneof [ Context' <$> arbitrary
          , Tagged   <$> arbitrary ]

instance Arbitrary Interval2 where
  arbitrary = Interval2 <$> arbitrary <*> arbitrary

instance Arbitrary Year1 where
  arbitrary = Year1 <$> arbitrary

instance Arbitrary Year2 where
  arbitrary = Year2 <$> arbitrary <*> suchThat arbitrary (\x -> x > 0)

instance Arbitrary SignificandYear2 where
  arbitrary =
    oneof [ SignificandYear <$> between' 1 9999 <*> suchThat arbitrary (\x -> x > 0)
          , SignificandYearE      <$> arbitrary <*> suchThat arbitrary (\x -> x > 0)
          , SignificandYearLong   <$> arbitrary <*> suchThat arbitrary (\x -> x > 0) ]

instance Arbitrary Season where
  arbitrary = oneof [ pure Spring, pure Summer, pure Autumn, pure Winter ]

instance Arbitrary YearDivision1 where
  arbitrary = Seasonal <$> between' 1 9999 <*> arbitrary

instance Arbitrary YearDivision2 where
  arbitrary =
    oneof [ SouthernHemisphere <$> between' 1 9999 <*> arbitrary
          , NorthernHemisphere <$> between' 1 9999 <*> arbitrary
          , Quarter            <$> between' 1 9999 <*> between' 1 4
          , Quadrimester       <$> between' 1 9999 <*> between' 1 3
          , Semester           <$> between' 1 9999 <*> between' 1 2 ]

instance Arbitrary SetOfDates2 where
  arbitrary = do
    q <- oneof [ BeforeOrOn  <$> suchThat arbitrary (\x -> length x > 0)
               , AfterOrOn   <$> suchThat arbitrary (\x -> length x > 0)
               , CertainlyOn <$> suchThat arbitrary (\x -> length x > 0) ]
    oneof [ ExclusiveSet <$> pure q
          , InclusiveSet <$> pure q ]

instance Arbitrary Decade2 where
  arbitrary = oneof [ Decade <$> between' 1 999
                    , ApproxDecade <$> between' 1 999 ]

prop_ISO8601Profile1 :: ISO8601Profile1 a => Show a => Eq a => a -> Bool
prop_ISO8601Profile1 d = case iso8601Profile1Parse (show d) of
  Nothing -> False
  Just d' -> d == d'
prop_ISO8601Profile2 :: ISO8601Profile2 a => Show a => Eq a => a -> Bool
prop_ISO8601Profile2 d = case iso8601Profile2Parse (show d) of
  Nothing -> False
  Just d' -> d == d'
  
prop_NominalDate1 :: NominalDate1 -> Bool
prop_NominalDate1 = prop_ISO8601Profile1

prop_NominalDate2 :: NominalDate2 -> Bool
prop_NominalDate2 = prop_ISO8601Profile2

prop_UnspecifiedDate1 :: UnspecifiedDate1 -> Bool
prop_UnspecifiedDate1 = prop_ISO8601Profile1

prop_UnspecifiedDate2 :: UnspecifiedDate2 -> Bool
prop_UnspecifiedDate2 = prop_ISO8601Profile2

prop_BeforeOrAfter2 :: BeforeOrAfter2 -> Bool
prop_BeforeOrAfter2 = prop_ISO8601Profile2

prop_Interval1 :: Interval1 -> Bool
prop_Interval1 = prop_ISO8601Profile1

prop_Interval2 :: Interval2 -> Bool
prop_Interval2 = prop_ISO8601Profile2

prop_Year1 :: Year1 -> Bool
prop_Year1 = prop_ISO8601Profile1

prop_Year2 :: Year2 -> Bool
prop_Year2 = prop_ISO8601Profile2

prop_SignificandYear2 :: SignificandYear2 -> Bool
prop_SignificandYear2 = prop_ISO8601Profile2

prop_YearDivision1 :: YearDivision1 -> Bool
prop_YearDivision1 = prop_ISO8601Profile1

prop_YearDivision2 :: YearDivision2 -> Bool
prop_YearDivision2 = prop_ISO8601Profile2

prop_SetOfDates2 :: SetOfDates2 -> Bool
prop_SetOfDates2 = prop_ISO8601Profile2

prop_Decade2 :: Decade2 -> Bool
prop_Decade2 = prop_ISO8601Profile2
