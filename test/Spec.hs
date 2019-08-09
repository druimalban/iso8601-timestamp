import Test.Hspec
import Test.Hspec.Core.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.All
import Text.Regex.Applicative (match)

import Base
import Extended

main :: IO ()
main = hspec $ parallel $ do
  describe "Testing properties for base ISO 8601-1." $
    modifyMaxSuccess (const 1000) $
    do it "prop_TimeZone"    $ property prop_TimeZone
       it "prop_NominalDate" $ property prop_NominalDate
       it "prop_NominalTime" $ property prop_NominalTime
       it "prop_TimeStamp"   $ property prop_TimeStamp
       it "prop_Duration"    $ property prop_Duration
       it "prop_Interval"    $ property prop_Interval
       it "prop_Repeat"      $ property prop_Repeat
  describe "Testing properties for profile level 1 of extended ISO 8601-2." $
    modifyMaxSuccess (const 1000) $
    do it "prop_NominalDate1"     $ property prop_NominalDate1
       it "prop_UnspecifiedDate1" $ property prop_UnspecifiedDate1
       it "prop_Interval1"        $ property prop_Interval1
       it "prop_Year1"            $ property prop_Year1
       it "prop_YearDivision1"    $ property prop_YearDivision1
  describe "Testing properties for profile level 2 of extended ISO 8601-2." $
    modifyMaxSuccess (const 1000) $
    do it "prop_NominalDate2"     $ property prop_NominalDate2
       it "prop_UnspecifiedDate2" $ property prop_UnspecifiedDate2
       it "prop_BeforeOrAfter2"   $ property prop_BeforeOrAfter2
       it "prop_Interval2"        $ property prop_Interval2
       it "prop_Year2"            $ property prop_Year2
       it "prop_SignificandYear2" $ property prop_SignificandYear2
       it "prop_YearDivision2"    $ property prop_YearDivision2
       it "prop_SetOfDates2"      $ property prop_SetOfDates2
       it "prop_Decade2"          $ property prop_Decade2
