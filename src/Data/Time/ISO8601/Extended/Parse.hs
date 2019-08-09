{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Time.ISO8601.Extended.Parse where

import Control.Monad (replicateM)
import Data.Digits (digits, unDigits)
import Lens.Micro
import Text.Regex.Applicative
import Text.Regex.Applicative.Common

import Data.Time.ISO8601.Extended.Internal

instance ISO8601Profile1 NominalDate1 where
  iso8601Profile1Parse = match byNominalDate1
instance ISO8601Profile1 UnspecifiedDate1 where
  iso8601Profile1Parse = match byUnspecifiedDate1
instance ISO8601Profile1 Interval1 where
  iso8601Profile1Parse = match byInterval1
instance ISO8601Profile1 Year1 where
  iso8601Profile1Parse = match byYear1
instance ISO8601Profile1 YearDivision1 where
  iso8601Profile1Parse = match byYearDivision1

instance ISO8601Profile2 NominalDate2 where
  iso8601Profile2Parse = match byNominalDate2
instance ISO8601Profile2 UnspecifiedDate2 where
  iso8601Profile2Parse = match byUnspecifiedDate2
instance ISO8601Profile2 BeforeOrAfter2 where
  iso8601Profile2Parse = match byBeforeOrAfter2
instance ISO8601Profile2 Interval2 where
  iso8601Profile2Parse = match byInterval2
instance ISO8601Profile2 Year2 where
  iso8601Profile2Parse = match byYear2
instance ISO8601Profile2 SignificandYear2 where
  iso8601Profile2Parse = match bySignificandYear2
instance ISO8601Profile2 YearDivision2 where
  iso8601Profile2Parse = match byYearDivision2
instance ISO8601Profile2 SetOfDates2 where
  iso8601Profile2Parse = match bySetOfDates2
instance ISO8601Profile2 Decade2 where
  iso8601Profile2Parse = match byDecade2

takeDigits :: Integral a => Int -> RE Char a
takeDigits n = unDigits 10 <$> replicateM n digit

takeUnspecifiedDigits :: Integral a => Int -> RE Char [Unspecified a]
takeUnspecifiedDigits n = replicateM n digit' where
  digit' = sym 'X' *> pure Unspecified <|> Specified <$> digit

takeUnspecified :: Integral a => Int -> RE Char (Unspecified a)
takeUnspecified n = do
  q <- takeUnspecifiedDigits n
  pure $ case sequence q of
    Unspecified  -> Unspecified
    Specified xs -> Specified (unDigits 10 xs)

byContext :: RE Char t -> RE Char (Context t)
byContext byWrapped = context byWrapped <|> unknown byWrapped <|> open byWrapped where
  context byWrapped = Context <$> byWrapped
  unknown byWrapped = sym '*' *> pure Unknown
  open    byWrapped = pure Open

byNominalDate' :: RE Char NominalDate'
byNominalDate' = d <|> m <|> y where
  d = CalendarDate'  <$> takeDigits 4 <* sym '-' <*> takeDigits 2 <* sym '-' <*> takeDigits 2
  m = SpecificMonth' <$> takeDigits 4 <* sym '-' <*> takeDigits 2
  y = SpecificYear'  <$> takeDigits 4

-- | Parse a nominal date representation at level 1.
byNominalDate1 :: RE Char NominalDate1
byNominalDate1 = reliable <|> uncertain <|> approx <|> unreliable
  where
    reliable   = Reliable    <$> byNominalDate'
    uncertain  = Uncertain   <$> byNominalDate' <* sym '?'
    approx     = Approximate <$> byNominalDate' <* sym '~'
    unreliable = Unreliable  <$> byNominalDate' <* sym '%'

byAnnotation :: RE Char t -> RE Char (Annotation t)
byAnnotation byWrapped = Annotation <$> someAnnotation <*> byWrapped <*> someAnnotation
  where someAnnotation =
          pure Reliable' <|>
          (sym '?' *> pure Uncertain') <|>
          (sym '~' *> pure Approximate') <|>
          (sym '%' *> pure Unreliable')

-- | Parse a nominal date representation at level 2.
byNominalDate2 :: RE Char NominalDate2
byNominalDate2 = d <|> m <|> y where
  d = do
    yearAnno  <- byAnnotation (takeDigits 4)
    sym '-'
    monthAnno <- byAnnotation (takeDigits 2)
    sym '-'
    dayAnno   <- byAnnotation (takeDigits 2)
    pure $ toNominalDate2 yearAnno monthAnno dayAnno
  m = do
    yearAnno  <- byAnnotation (takeDigits 4)
    sym '-'
    monthAnno <- byAnnotation (takeDigits 2)
    pure $ toNominalDate2' yearAnno monthAnno
  y = SpecificYear2 <$> (toProvenance <$> byAnnotation (takeDigits 4))

-- | Parse an unspecified date representation at level 1.
byUnspecifiedDate1 :: RE Char UnspecifiedDate1
byUnspecifiedDate1 = d <|> m <|> y where
  d = UnspecifiedDay1   <$> takeUnspecified 4 <* sym '-' <*> takeUnspecified 2 <* sym '-' <*>  takeUnspecified 2
  m = UnspecifiedMonth1 <$> takeUnspecified 4 <* sym '-' <*> takeUnspecified 2
  y = UnspecifiedYear1  <$> takeUnspecifiedDigits 4

-- | Parse an unspecified date representation at level 2.
byUnspecifiedDate2 :: RE Char UnspecifiedDate2
byUnspecifiedDate2 = d <|> m <|> y where
  d = UnspecifiedDay2   <$> takeUnspecifiedDigits 4 <* sym '-' <*> takeUnspecifiedDigits 2 <* sym '-' <*> takeUnspecifiedDigits 2
  m = UnspecifiedMonth2 <$> takeUnspecifiedDigits 4 <* sym '-' <*> takeUnspecifiedDigits 2
  y = UnspecifiedYear2  <$> takeUnspecifiedDigits 4

byBeforeOrAfter :: RE Char t -> RE Char (BeforeOrAfter t)
byBeforeOrAfter byWrapped = certainlyOn byWrapped <|>
                            beforeOrOn  byWrapped <|>
                            afterOrOn   byWrapped
  where
    certainlyOn byWrapped = CertainlyOn <$> byWrapped
    beforeOrOn  byWrapped = BeforeOrOn  <$  string ".." <*> byWrapped
    afterOrOn   byWrapped = AfterOrOn   <$> byWrapped <* string ".."

-- | Parse a representation of some nominal date before or after the specified value.
byBeforeOrAfter2 :: RE Char BeforeOrAfter2
byBeforeOrAfter2 = byBeforeOrAfter byNominalDate'

-- | Parse an interval between two times at level 1.
byInterval1 :: RE Char Interval1
byInterval1 = Interval1 <$> byContext byNominalDate1 <* sym '/' <*> byContext byNominalDate1

-- | Parse an interval between two times at level 2.
byInterval2 :: RE Char Interval2
byInterval2 = Interval2 <$> byContextL <* sym '/' <*> byContextR where
  byContextL = context <|> (string ".." *> tagged)
  byContextR = context <|> (tagged <* string "..")
  context = Context' <$> byContext byNominalDate2
  tagged  = Tagged   <$> byContext byNominalDate2

-- | Parse a single year representation at level 1.
byYear1 :: RE Char Year1
byYear1 = Year1 <$ sym 'Y' <*> signed decimal

-- | Parse a single year representation at level 2.
byYear2 :: RE Char Year2
byYear2 = Year2 <$ sym 'y' <*> signed decimal <* sym 'E' <*> decimal

-- | Parse a year specified by a given number of significant digits (the significand) after 'S'.
bySignificandYear2 :: RE Char SignificandYear2
bySignificandYear2 = y <|> e <|> l where
  y = SignificandYear     <$> takeDigits 4 <* sym 'S' <*> decimal
  e = SignificandYearE    <$> byYear2      <* sym 'S' <*> decimal
  l = SignificandYearLong <$> byYear1      <* sym 'S' <*> decimal

-- | Parse a division of the year at level 1 - values 21-24 denoting the seasons spring to winter repsectively.
byYearDivision1 :: RE Char YearDivision1
byYearDivision1 = Seasonal <$> takeDigits 4 <* sym '-' <*>
  ((string "21" *> pure Spring) <|> (string "22" *> pure Summer) <|>
   (string "23" *> pure Autumn) <|> (string "24" *> pure Winter))

{- | Parse a division of the year at level 2 using additional values from 25-41:

* Seasons in the southern hemisphere denoted by values 25-28 as above;
* Seasons in the northern hemisphere denoted by values 29-32 as above;
* A quarter of the year denoted by values 33-36 respectively;
* A quadrimester (third) of the year denoted by values 37-39 respectively;
* A semester (half) of the year denoted by values 40-41 respectively.
-}
byYearDivision2 :: RE Char YearDivision2
byYearDivision2 = sh <|> nh <|> q <|> qm <|> sm where
  sh = SouthernHemisphere <$> takeDigits 4 <* sym '-' <*>
    ((string "25" *> pure Spring) <|> (string "26" *> pure Summer) <|>
     (string "27" *> pure Autumn) <|> (string "28" *> pure Winter))
  nh = NorthernHemisphere <$> takeDigits 4 <* sym '-' <*>
    ((string "29" *> pure Spring) <|> (string "30" *> pure Summer) <|>
     (string "31" *> pure Autumn) <|> (string "32" *> pure Winter))
  q = Quarter <$> takeDigits 4 <* sym '-' <*>
    ((string "33" *> pure 1) <|> (string "34" *> pure 2) <|>
     (string "35" *> pure 3) <|> (string "36" *> pure 4))
  qm = Quadrimester <$> takeDigits 4 <* sym '-' <*>
    ((string "37" *> pure 1) <|> (string "38" *> pure 2) <|>
     (string "39" *> pure 3))
  sm = Semester <$> takeDigits 4 <* sym '-' <*>
    ((string "40" *> pure 1) <|> (string "41" *> pure 2))

bySomeSet :: String -> String -> RE Char (BeforeOrAfter [NominalDate'])
bySomeSet leftBracket rightBracket = certainlyOn <|> beforeOrOn <|> afterOrOn where
  certainlyOn = do
    string leftBracket
    init' <- many (byNominalDate' <* string ", ")
    last' <- byNominalDate'
    string rightBracket
    pure $ CertainlyOn (init' ++ [last'])
  beforeOrOn = do
    string (leftBracket ++ "..")
    init' <- many (byNominalDate' <* string ", ")
    last' <- byNominalDate'
    string rightBracket
    pure $ BeforeOrOn (init' ++ [last'])
  afterOrOn = do
    string leftBracket
    init' <- many (byNominalDate' <* string ", ")
    last' <- byNominalDate'
    string (".." ++ rightBracket)
    pure $ AfterOrOn (init' ++ [last'])

-- | A set of dates at level 2: that is, an inclusive set (consider all) of dates enclosed by curly brackets { }
-- or an exclusive set (consider one) of dates enclosed by square brackets.
bySetOfDates2 :: RE Char SetOfDates2
bySetOfDates2 = InclusiveSet <$> bySomeSet "{" "}" <|> ExclusiveSet <$> bySomeSet "[" "]"

-- | A given decade from 1-999, naturally being up to and including three digits in length.
byDecade2 :: RE Char Decade2
byDecade2 = (Decade <$> takeDigits 3) <|> (ApproxDecade <$> takeDigits 3 <* sym '~')
