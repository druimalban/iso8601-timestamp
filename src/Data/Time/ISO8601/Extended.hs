{-|
Module      : Data.Time.ISO8601.Extended
Description : Implementation of ISO 8601-2:2019
Copyright   : (c) Duncan Guthrie, 2019
License     : BSD 3-Clause
Maintainer  : dguthrie@posteo.net
Stability   : experimental

= Implementation of ISO 8601-2:2019

This module implements most of the extended section of ISO 8601,
that is, ISO 8601-2:2019.

This is a set of alternative representations that mostly pertain to accuracty and approximation of values.
There are two profile levels, 1 and 2, which a given sender and/or receiver can choose between.

It should be noted that only the extended format is considered, because this simplifies parsing.

Furthermore, for dates and such, only the simplest calendar dates are considred.
This is because, for instance, ordinal and weekly dates can be computed from calendar dates, and vice versa.

In order to relate these functions to ISO 8601-1, two classes, one for each profile level, are provided.
Instances of these will have a 1 or 2 appended to them as appropriate.
-}
{-# OPTIONS_HADDOCK prune #-} 

module Data.Time.ISO8601.Extended (
    module Data.Time.ISO8601.Extended.Internal
  , module Data.Time.ISO8601.Extended.Parse
  ) where

import Data.Time.ISO8601.Extended.Internal hiding
  ( Anno (..)
  , combine
  , compareAnno
  , collapsePrefix
  , shiftLeft
  , Annotation (..)
  , toProvenance
  , fromProvenance
  , padAnnotation
  , toNominalDate2
  , toNominalDate2'
  , fromNominalDate2
  , showU
  , padU
  , showSetOfDates2
  , listFilling
  , getInclusiveList )
    
import Data.Time.ISO8601.Extended.Parse
  ( byNominalDate1
  , byUnspecifiedDate1
  , byInterval1
  , byYear1
  , byYearDivision1
  , byNominalDate2
  , byUnspecifiedDate2
  , byBeforeOrAfter2
  , byInterval2
  , byYear2
  , bySignificandYear2
  , byYearDivision2
  , bySetOfDates2
  , byDecade2 )
