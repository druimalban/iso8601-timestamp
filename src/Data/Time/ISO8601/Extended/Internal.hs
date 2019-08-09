{-|
Module      : Data.Time.ISO8601.Extended.Internal
Description : Implementation of ISO 8601-2:2019
Copyright   : (c) Duncan Guthrie, 2019
License     : BSD 3-Clause
Maintainer  : dguthrie@posteo.net
Stability   : experimental

= Implementation of ISO 8601-2:2019

Functions here are equivalent to those defined Data.Time.ISO8601 and have a suffix '1' or '2' as appropriate
to the profile they implement.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Time.ISO8601.Extended.Internal where

import Control.Monad
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Lens.Micro
import Lens.Micro.TH

import Data.Time.ISO8601.Format
import Data.Time.ISO8601.Internal hiding (year, month, day)
import Data.Time.ISO8601.Parse
import Data.Time.ISO8601.TH

{- | Define profile level 1 as a typeclass. Analogous to 'Data.Time.ISO8601.ISO8601'.
See /ISO 8601-2/, section 4.1.2, /Levels/.
-}
class ISO8601Profile1 t where
  iso8601Profile1Parse :: String -> Maybe t

-- | Define profile level 2 as above.
class ISO8601Profile2 t where
  iso8601Profile2Parse :: String -> Maybe t

data Anno
  = Reliable'
  | Uncertain'
  | Approximate'
  | Unreliable'
  deriving Eq

instance Show Anno where
  show (Reliable')    = ""
  show (Uncertain')   = "?"
  show (Approximate') = "~"
  show (Unreliable')  = "%"

-- | An annotated value has a prefix to the left which applies to /just/ that value.
-- It also has a suffix which applies to all values to the left.
data Annotation t = Annotation { prefix :: Anno
                               , annotatedValue  :: t
                               , suffix :: Anno }

instance Show t => Show (Annotation t) where
  show (Annotation {..}) =
    mconcat [ show prefix
            , show annotatedValue
            , show suffix ]

makeLensesWith customRules ''Annotation

{- | Given two annotations, what happens if we combine them?
That is, certain items supercede others.

Since % supercedes ?, combining these of course returns %.
If we combine ? and ~, they become %, a combination of both.
And if we combine anything with the empty string, of course
we end up with the same thing we started off with.

Naturally, combining two of the same value produces that value.
-}
combine :: Anno -> Anno -> Anno
combine Reliable'    n            = n
combine n            Reliable'    = n
combine Uncertain'   Approximate' = Unreliable'
combine Approximate' Uncertain'   = Unreliable'
combine Unreliable'  _            = Unreliable'
combine _            Unreliable'  = Unreliable'
combine Approximate' Approximate' = Approximate'
combine Uncertain'   Uncertain'   = Uncertain'

{- | compareAnno: Let's compare two annotations on two different values,
with a view to moving any common item to the rightmost value's suffix,
since this is the preffered form and it is simpler.

Here is an example: %2015-~02.

We can remove ~ from both items (approximation) and we are left with an
uncertainty on 2015.

> compareAnno (Unreliable' 2015) (Approximation' 02)
("?", ("", "~"))

In this case, the suffix of our second item didn't change. 
-}
compareAnno :: Anno -> Anno -> (Anno, (Anno, Anno))
compareAnno Unreliable'  Uncertain'   = (Approximate', (Reliable'   , Uncertain')) -- %2000~-?02 => ~2000~-02?
compareAnno Unreliable'  Approximate' = (Uncertain'  , (Reliable'   , Approximate')) -- %2000-~02 => ?2000-02~
compareAnno Uncertain'   Unreliable'  = (Reliable'   , (Approximate', Uncertain')) -- ?2000-%02 => 2000-~02?
compareAnno Approximate' Unreliable'  = (Reliable'   , (Uncertain'  , Approximate')) -- ~2000-%02 => 2000-?02~
compareAnno p q 
  | p == q = (Reliable', (Reliable', q)) -- ~2000-~02 => 2000-02~
  | otherwise      = (p, (q, Reliable'))

-- | If we consider that the suffix implies a property of the item and anything to the left of it,
-- we may remove certain prefixes if they are already implicit.
collapsePrefix :: Annotation t -> Annotation t
collapsePrefix (Annotation Unreliable'  val Uncertain'  ) = Annotation Approximate' val Uncertain'   -- %02? => ~02?
collapsePrefix (Annotation Unreliable'  val Approximate') = Annotation Uncertain'   val Approximate' -- %02~ => ?02~
collapsePrefix (Annotation Uncertain'   val Unreliable' ) = Annotation Reliable'    val Unreliable'  -- ?02% =>  02%
collapsePrefix (Annotation Approximate' val Unreliable' ) = Annotation Reliable'    val Unreliable'  -- ~02% =>  02%
collapsePrefix Annotation {..}
  | prefix == suffix = Annotation Reliable' annotatedValue suffix
  | otherwise        = Annotation {..}

-- | Move an annotation that is found to be common to the prefix as appropriate.
shiftLeft :: Annotation s -> Annotation t -> (Annotation s, Annotation t)
shiftLeft (Annotation prefix val suffix) (Annotation prefix' val' suffix') =
  (Annotation prefix val (combine suffix suffix'),
   Annotation (combine prefix' suffix') val' Reliable')

-- | Pad the value that is annotated by some n digits.
padAnnotation :: Integral t => Show t => Int -> Annotation t -> String
padAnnotation n (Annotation {..}) =
  mconcat [ show prefix
          , padTo n annotatedValue
          , show suffix ]
           
{-| Provenance: To what extent is a given value uncertain or approximate?
Please see /ISO 8601-2/, section 4.2, /Uncertain and\/or approximate date/.
-}
data Provenance t
  -- | A value is reliable if it is /neither/ uncertain /nor/ approximate. No mark.
  = Reliable    { provenanceOf :: t }
  -- | A value in this case is /just/ uncertain. Marked with a question mark (?).
  | Uncertain   { provenanceOf :: t }
  -- | A value in this case is /just/ approximate. Marked with a tilde (~).
  | Approximate { provenanceOf :: t }
  -- | A value is unreliable if it is both uncertain /and/ approximate. Marked with a percentage sign (%).
  | Unreliable  { provenanceOf :: t }
  deriving Eq

makeLensesWith customRules ''Provenance

-- | Convert a given annotation to a provenance using 
-- its prefix. Ignore the suffix in all cases.
toProvenance :: Annotation t -> Provenance t
toProvenance (Annotation Reliable'    val _) = Reliable    val
toProvenance (Annotation Uncertain'   val _) = Uncertain   val
toProvenance (Annotation Approximate' val _) = Approximate val
toProvenance (Annotation Unreliable'  val _) = Unreliable  val

-- | Convert a given provenance to an annotation.
fromProvenance :: Provenance t -> Annotation t
fromProvenance (Reliable    val) = Annotation Reliable'    val Reliable'
fromProvenance (Uncertain   val) = Annotation Uncertain'   val Reliable'
fromProvenance (Approximate val) = Annotation Approximate' val Reliable'
fromProvenance (Unreliable  val) = Annotation Unreliable'  val Reliable'

instance Show t => Show (Provenance t) where
  show (Reliable p)    = show p
  show (Uncertain p)   = show p ++ "?"
  show (Approximate p) = show p ++ "~"
  show (Unreliable p)  = show p ++ "%"

{-| Note that these apply only to Gregorian dates. Other representations are not considered.
Please see /ISO 8601-2/, section 4.2.1. --}
data NominalDate'
  -- | Full calendar date with a year, month and day representation.
  = CalendarDate' { year :: Integer, month :: Int, day :: Int }
  -- | Specific month in a year.
  | SpecificMonth' { year :: Integer, month :: Int }
  -- | A year only.
  | SpecificYear'  { year :: Integer }
  deriving Eq

instance Show NominalDate' where
  show (CalendarDate' {..})
    = mconcat [ padTo 4 year, "-", padTo 2 month, "-", padTo 2 day ]
  show (SpecificMonth' {..})
    = mconcat [ padTo 4 year, "-", padTo 2 month ]
  show (SpecificYear' {..}) = padTo 4 year

-- | At level 1, express the entire date in terms of its reliability.
type NominalDate1 = Provenance NominalDate'

-- | Same structure as level 1, albeit with the reliability of each individual component being considered.
data NominalDate2
  = CalendarDate2  { year2 :: Provenance Integer, month2 :: Provenance Int, day2 :: Provenance Int }
  | SpecificMonth2 { year2 :: Provenance Integer, month2 :: Provenance Int }
  | SpecificYear2  { year2 :: Provenance Integer }
  deriving Eq

instance Show NominalDate2 where
  show = fromNominalDate2

-- | Convert three annotations to a nominal date at level 2. This function is for parsers.
toNominalDate2 :: Annotation Integer -> Annotation Int -> Annotation Int -> NominalDate2
toNominalDate2 y m d = do
  let (m', d')  = shiftLeft m d
      (y', m'') = shiftLeft y m'

      pre = combine (prefix y') (suffix y')
      val = annotatedValue y'
  
      y'' = Annotation pre val Reliable'
  CalendarDate2 (toProvenance y'') (toProvenance m'') (toProvenance d')

-- | As above but concerning two annotations, that is ommiting the day.
toNominalDate2' :: Annotation Integer -> Annotation Int -> NominalDate2
toNominalDate2' y m = do
  let (y', m') = shiftLeft y m
      pre = combine (prefix y') (suffix y')
      val = annotatedValue y'

      y'' = Annotation pre val Reliable'
  SpecificMonth2 (toProvenance y'') (toProvenance m')

-- | Convert a nominal date representation to a pretty string.
fromNominalDate2 :: NominalDate2 -> String
fromNominalDate2 (SpecificYear2 y) = padAnnotation 4 (fromProvenance y)
fromNominalDate2 (SpecificMonth2 {..}) = do
  let Annotation yearPrefix  year  yearSuffix  = fromProvenance year2
      Annotation monthPrefix month monthSuffix = fromProvenance month2
      
      -- Move any common parts into the suffix of month.
      (yearPrefix', (monthPrefix', monthSuffix')) = compareAnno yearPrefix monthPrefix

      -- Build final annotations...
      yearAnno  = Annotation yearPrefix'  year  Reliable'
      monthAnno = Annotation monthPrefix' month monthSuffix'

  -- ...and print them nicely
  mconcat [ padAnnotation 4 yearAnno, "-"
          , padAnnotation 2 monthAnno ]
fromNominalDate2 (CalendarDate2 {..}) = do
  let annoY = fromProvenance year2
      annoM = fromProvenance month2
      annoD = fromProvenance day2

  -- compareAnno the prefixes of year and month to find a common item.
  let (yearPrefix, (monthPrefix, monthSuffix)) = compareAnno (prefix annoY) (prefix annoM)

  -- compareAnno the suffix of month, and prefix of day to find a common item.
  let (monthSuffix', (dayPrefix, daySuffix)) = compareAnno monthSuffix (prefix annoD)

  -- now try and remove spurious items from each item
  let yearPrefix'  = prefix $ collapsePrefix (Annotation yearPrefix  0 daySuffix)
  let yearPrefix'' = prefix $ collapsePrefix (Annotation yearPrefix' 0 monthSuffix')

  let monthPrefix' = prefix $ collapsePrefix (Annotation monthPrefix 0 daySuffix)

  -- Build final representations
  let finalYearAnno  = set _prefix (combine yearPrefix' yearPrefix'') annoY
  let finalMonthAnno = collapsePrefix $ Annotation monthPrefix' (annotatedValue annoM) monthSuffix'
  let finalDayAnno   = collapsePrefix $ Annotation dayPrefix    (annotatedValue annoD) daySuffix
  
  mconcat [ padAnnotation 4 finalYearAnno , "-"
          , padAnnotation 2 finalMonthAnno, "-"
          , padAnnotation 2 finalDayAnno ]
    
{- | Unspecified: A given value or any o
f its components may be unspecified.
Please see /ISO 8601-2/, section 4.3, /Unspecified/.

Obviously, we don't have any access to the thing that is unspecified,
so we can't e.g. print some number of digits. Therefore, when showing an
unspecified thing, the only thing that can be shown is an X.
-}
data Unspecified t
  = Unspecified
  | Specified t
  deriving Eq

instance Show t => Show (Unspecified t) where
  show (Specified t) = show t
  show (Unspecified) = "X"

instance Functor Unspecified where
  fmap = liftM
instance Applicative Unspecified where
  pure  = return
  (<*>) = ap
instance Monad Unspecified where
  return p = Specified p
  (>>=) Unspecified   f = Unspecified
  (>>=) (Specified p) f = f p

{- | Unspecified date: at level 1, we consider the possiblity that the lowest-ranked component may be unspecified.
Please see /ISO 8601-2/, section 4.3.1.
-}
data UnspecifiedDate1
  = UnspecifiedDay1   { unspecifiedYear1  :: Unspecified Integer
                      , unspecifiedMonth1 :: Unspecified Int
                      , unspecifiedDay1   :: Unspecified Int } -- YYYY-MM-XX or YYYY-XX-XX or XXXX-XX-XX
  | UnspecifiedMonth1 { unspecifiedYear1  :: Unspecified Integer
                      ,  unspecifiedMonth1 :: Unspecified Int }  -- XXXX-XX or YYYY-XX
  | UnspecifiedYear1  { unspecifiedYear1' :: [Unspecified Int] } -- XX 
  deriving Eq

instance Show UnspecifiedDate1 where
  show (UnspecifiedDay1 {..})
    = mconcat [ padU 4 unspecifiedYear1, "-", padU 2 unspecifiedMonth1, "-", padU 2 unspecifiedDay1 ]
  show (UnspecifiedMonth1 {..})
    = mconcat [ padU 4 unspecifiedYear1, "-", padU 2 unspecifiedMonth1 ]
  show (UnspecifiedYear1 {..})
    = showU 4 unspecifiedYear1'

{- | At level 2, we further consider the possibility that /any digit/ of any component may be unspecified.
The interpetation is implementation-specific.
Please see /ISO 8601-2/, section 4.3.2.
-}
data UnspecifiedDate2
  = UnspecifiedDay2   { unspecifiedYear2 :: [Unspecified Int], unspecifiedMonth2 :: [Unspecified Int], unspecifiedDay2 :: [Unspecified Int] }
  | UnspecifiedMonth2 { unspecifiedYear2 :: [Unspecified Int], unspecifiedMonth2 :: [Unspecified Int] }
  | UnspecifiedYear2  { unspecifiedYear2 :: [Unspecified Int] }
  deriving Eq

instance Show UnspecifiedDate2 where
  show = \case
    UnspecifiedDay2 {..} ->
      mconcat [ showU 4 unspecifiedYear2, "-"
              , showU 2 unspecifiedMonth2, "-"
              , showU 2 unspecifiedDay2 ]
    UnspecifiedMonth2 {..} ->
      mconcat [ showU 4 unspecifiedYear2, "-"
              , showU 2 unspecifiedMonth2 ]
    UnspecifiedYear2 {..} -> showU 4 unspecifiedYear2

showU :: Show a => Integral a => Int -> [Unspecified a] -> String
showU n xs
  | length xs <= abs n =
      mconcat [ replicate (abs n - length xs) 'X'
              , foldr ((++) . show) [] xs ]
  | otherwise = foldr ((++) . show) [] xs

padU :: Show a => Integral a => Int -> Unspecified a -> String
padU n (Unspecified) = replicate (abs n) 'X'
padU n (Specified p) = padTo n p

{- | Before or after: Qualify a value (either a date or a group of dates) and indicate if it was "before or on", or "after or on".
Please see /ISO 8601-2/, section 4.4, /Before or after/.
-}
data BeforeOrAfter t
  -- | If an event was before or on a given date or group of dates, prepend a double-dot (..)
  = BeforeOrOn t
  -- | Likewise, if an event was after or on a value, append a double dot (..).
  | AfterOrOn t
  -- | We need to handle the case where no clarification of this sort is required.
  | CertainlyOn t

instance Show t => Show (BeforeOrAfter t) where
  show (BeforeOrOn  p) = ".." ++ show p
  show (AfterOrOn   p) = show p ++ ".."
  show (CertainlyOn p) = show p

instance Eq t => Eq (BeforeOrAfter t) where
  (==) (BeforeOrOn  p) (BeforeOrOn  q) = p == q
  (==) (AfterOrOn   p) (AfterOrOn   q) = p == q
  (==) (CertainlyOn p) (CertainlyOn q) = p == q
  (==) _ _                             = False

-- | Type alias: Before or after for some date. (Level 2 only)
type BeforeOrAfter2 = BeforeOrAfter NominalDate'

{- | Context: Allows us to consider whether a given start or end date is:

* Unknown; indicate with asterisk (*). The opposite of this is of course a known date, so we wrap the type with the Known constructor.
* Open start or end; in this case, we leave something blank. The opposite of this is a closed end, so we wrap the type with the Closed constructor.
-}
data Context t = Open | Unknown | Context t
  deriving Eq

instance Show t => Show (Context t) where
  show (Context t) = show t
  show Open        = ""
  show Unknown     = "*"

{- | Enhanced time interval:
This allows us to indicate whether the start or end of a given date is /unknown/;
and\/or whether the start or end of a given date has an open start or similar.

Please see /ISO 8601-2/, section 4.5, /Enhanced time interval/.

At level 1, we consider the possibility a given date may be inaccurate or similar. We use NominalDate1 (recall this is defined above) to represent this case.
-}
data Interval1 = Interval1 { fromDate1 :: Context NominalDate1, toDate1 :: Context NominalDate1 }
  deriving (Eq)

instance Show Interval1 where
  show (Interval1 {..}) = mconcat [ show fromDate1, "/", show toDate1 ]

{- | At level 2, we consider dates of the same format as NominalDate2 as above.
However, we also consider the possibility that a date might occur before or on, or after or on, dependent on accuracy.
-}
data Context'
  -- | Nominal date for profile 2, in context.
  = Context' (Context NominalDate2)
  -- | Nominal date as above, albeit with consideration that something happened after or before.
  | Tagged   (Context NominalDate2)
    deriving Eq

-- | Finally, we define our main structure for an interval at profile level 2. Please see /ISO 8601-2/, section 4.5.2.
data Interval2 = Interval2 { fromDate2 :: Context', toDate2 :: Context' }
  deriving Eq

instance Show Interval2 where
  show (Interval2 {..}) = mconcat [ showL fromDate2, "/", showR toDate2 ] where
    showL (Tagged c)   = ".." ++ show c
    showL (Context' c) = show c
    showR (Tagged c)   = show c ++ ".."
    showR (Context' c) = show c

{- | Year exceeding 4 digits: An alternative method which does not require agreement -
by the party receiving the data, and the party sending the data -
about additional leading digits in the format.

At level 1, prepend /Y/ to a number exceeding four digits. At level 2, prepend /y/ to an exponential value.

Please see /ISO 8601-2/, section 4.6, /Year exceeding four digits/.

For now, type alias to Integer.
-}
newtype Year1 = Year1 Integer
  deriving Eq
instance Show Year1 where
  show (Year1 p) = "Y" ++ show p

-- | At level 2, prepend lower-case /y/ to an exponent, e.g. y2E3 ~= 2000.
data Year2 = Year2 { year2 :: Integer, exponent :: Integer }
  deriving Eq

instance Show Year2 where
  show (Year2 {..})
    = mconcat [ "y", show year2, "E", show exponent ]

{- | Signficiant digits: number of significand digits for the expressed year.

-}
data SignificandYear2
  -- | Some four digit year followed by S and the signficand.
  = SignificandYear { significandYear :: Integer, significand :: Int }
  -- | As above albeit with an exponential year.
  | SignificandYearE { significandYearE :: Year2, significand :: Int }
  -- | As above albeit with a long year starting with a Y.
  | SignificandYearLong { significandYearLong :: Year1, significand :: Int }
  deriving Eq

instance Show SignificandYear2 where
  show (SignificandYear {..})
    = mconcat [ padTo 4 significandYear
              , "S"
              , show significand ]
  show (SignificandYearE {..})
    = mconcat [ show significandYearE
              , "S"
              , show significand ]
  show (SignificandYearLong {..})
    = mconcat [ show significandYearLong
              , "S"
              , show significand ]

-- | Some named season.
data Season = Spring | Summer | Autumn | Winter
  deriving (Show, Eq)

seasonCode :: Season -> Int
seasonCode Spring = 21
seasonCode Summer = 22
seasonCode Autumn = 23
seasonCode Winter = 24

{- | YearDivision: Specify different codes for different parts of the year.
Please see /ISO 8601-2/, section 4.8, /Divisions of a year/.

At level 1, only consider the seasons.
-}
data YearDivision1
  -- | Values 21-24: spring, summer, autumn, winter respectively
  = Seasonal { year1 :: Integer, season :: Season }
  deriving Eq

instance Show YearDivision1 where
  show (Seasonal {..})
    = mconcat [ padTo 4 year1, "-", padTo 2 (seasonCode season) ]

-- | At level 2, consider other portions of the year.
data YearDivision2
  -- | Values 25-28: seasons for the southern hemisphere
  = SouthernHemisphere { year2 :: Integer, season       :: Season }
  -- | Values 29-32: seasons for the northern hemisphere
  | NorthernHemisphere { year2 :: Integer, season       :: Season }
  -- | Values 33-36: quarter portion of the year. Values will be clipped during reading/writing.
  | Quarter            { year2 :: Integer, quarter      :: Int }
  -- | Values 37-39: third portion of the year. Values will be clipped during reading/writing.
  | Quadrimester       { year2 :: Integer, quadrimester :: Int }
  -- | Values 40-41: half portion of the year. Values will be clipped during reading/writing.
  | Semester           { year2 :: Integer, semester     :: Int }
  deriving Eq

instance Show YearDivision2 where
  show (SouthernHemisphere {..})
    = mconcat [ padTo 4 year2, "-", padTo 2 (seasonCode season + 4) ]
  show (NorthernHemisphere {..})
    = mconcat [ padTo 4 year2, "-", padTo 2 (seasonCode season + 8) ]
  show (Quarter {..})
    = mconcat [ padTo 4 year2, "-", padTo 2 (quarter + 32) ]
  show (Quadrimester {..})
    = mconcat [ padTo 4 year2, "-", padTo 2 (quadrimester + 36) ]
  show (Semester {..})
    = mconcat [ padTo 4 year2, "-", padTo 2 (semester + 39) ]

{- | Set of dates: a list enclosed by brackets. The feature is not specified at level 1.
At level 2, additionally we can use the two dots to mean:

* If the double-dot is at the start of the list, it means that the dates start before or on the first date.
* If the double-dot is at the end of the list, it means that the dates end after or on the last date.
* If the double-dot is between any two dates, it means that this is an enumerating list.
   E.g. 1992..1998 means "1992 to 1998" i.e. 1992, 1993, ..., 1997, 1998.

    * This particular construction is not represented in the final data structure. When parsing, we expand this with "getInclusiveList".
    * We could choose to fold together when serialising, but this should be optional as it doesn't affect the data.

Unfortunately, it's not possible to enumerate dates easily in standard Haskell. The Enum class must, at a minimum, implement toEnum (transform Int to the type) and fromEnum (transform the type into an Int). We can't, for instance, convert both to a Gregorian "Data.Time.Calendar.Day". For instance, let's say we used a value of zero to represent a missing element (month or day). We would then end up with situations where, for instance, 1992-02-0 and 1992-02-1 can only really represent the same thing as they represent the number of days since the start of the Gregorian date.

I may yet find another way to say this. Help and input would naturally be appreciated.
-}
data SetOfDates2
  -- | Exclusive set: Consider /one of/ these dates.
  = ExclusiveSet { setOfDates :: BeforeOrAfter [NominalDate'] }
  -- | Inclusive set" Consider /all of/ these dates.
  | InclusiveSet { setOfDates :: BeforeOrAfter [NominalDate'] }
  deriving Eq

instance Show SetOfDates2 where
  show (ExclusiveSet {..}) = showSetOfDates2 "[" "]" setOfDates
  show (InclusiveSet {..}) = showSetOfDates2 "{" "}" setOfDates

showSetOfDates2 :: String -> String -> BeforeOrAfter [NominalDate'] -> String
showSetOfDates2 leftBracket rightBracket (CertainlyOn d)
  = mconcat [ leftBracket
            , listFilling d
            , rightBracket ]
showSetOfDates2 leftBracket rightBracket (BeforeOrOn d)
  = mconcat [ leftBracket
            , ".."
            , listFilling d
            , rightBracket ]
showSetOfDates2 leftBracket rightBracket (AfterOrOn d)
  = mconcat [ leftBracket
            , listFilling d
            , ".."
            , rightBracket ]

listFilling :: Show a => [a] -> String
listFilling xs
  = mconcat [ foldr (\x y -> show x ++ ", " ++ y) [] (init xs)
            , show (last xs) ]
            
-- | Build an inclusive list. Analogous to e.g. [1..5], but accounting for all three constructors.
getInclusiveList :: (NominalDate', NominalDate') -> [NominalDate']
getInclusiveList (CalendarDate' y m d, CalendarDate' y' m' d')
  = map buildDate [ fromGregorian y m d .. fromGregorian y' m' d' ]
  where
    buildDate :: Day -> NominalDate'
    buildDate = (\(year, month, day) -> CalendarDate' {..}) . toGregorian
getInclusiveList (SpecificMonth' y m, SpecificMonth' y' m')
  = map buildDate  [ getMonths' y m .. getMonths' y' m' ]
  where
    getMonths' y m   = getMonths y (toInteger m)
    buildDate = (\(year, month') -> SpecificMonth' { month = fromIntegral month', ..}) . getYearsMonths
getInclusiveList (SpecificYear' y, SpecificYear' y') = map SpecificYear' [y .. y']
getInclusiveList (_, _) = []

{- | Decade: three-digit integer, that is ommision of the fourth digit of a given group of ten years.
May also be approximate. Only implemented at level 2.
-}
data Decade2
  = Decade       { decade :: Integer }
  | ApproxDecade { decade :: Integer }
  deriving Eq

instance Show Decade2 where
  show (Decade {..})       = padTo 3 decade
  show (ApproxDecade {..}) = padTo 3 decade ++ "~"
  

{- Recurring dates and times.
This is technically based on /IETF RFC 5545 (2009)/, /Internet Calendaring and Scheduling Core Object Specification (iCalendar)/, section 3.3.10, /Recurrence Rule/.

This is to allow for interoprability and compatibility.

Note - according to /ISO 8601-2/, section 5:

/All features in this section are defined at level 1 for the purpose of profiles, which may refer to the levels when specifying conformance to the profile. (Profiles are described in Annex B.)/

Not implemented yet.
-}
