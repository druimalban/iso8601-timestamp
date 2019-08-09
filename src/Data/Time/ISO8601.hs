{-|
Module      : Data.Time.ISO8601
Description : Implementation of ISO 8601-1:2019
Copyright   : (c) Duncan Guthrie, 2019
License     : BSD 3-Clause
Maintainer  : dguthrie@posteo.net
Stability   : experimental

= Implementation of ISO 8601-1:2019

* In this module, we implement ISO 8601-1:

    * Basic and extended formats - these are very simple representations. E.g. in dates whether to have hyphens or dashes, and in times to have colons.
    * Alternative formats - these extend __both__ the basic and extended formats. A good example of this is an additional agreed number of digits and padding as appropriate. E.g. prefixes of ± for CE or BCE of a given date.

* In "Data.Time.ISO8601.Extended", we further implement ISO 8601-2:

    * All of the above, but extends the original standard e.g. specifying further levels of ambiguity.
    * Profile levels - there are in fact two profile levels, levels 1 and 2. (An aside; it does not seem unreasonble to consider /ISO 8601-1/ as /level 0/ but probably not appropriate.

== Profiles

A given type must be wrapped in by the type 'Data.Time.ISO8601.Profile'. The reasons for this are as follows and are our interpretation of the standard:

* We need certain context information when parsing. At the very minimum, we need to know the number of permitted decimal points (this is not specified).
* When serialising, we need this information, and additionally whether to use a comma or decimal point (a comma should be preferred, it should be noted).
* We should not parse strings in an alternative representation by default. If we intend to, we need even more additional information, especially the number of leading digits.
* We need to have a way to preserve the above information, and wrapping the actual type parsed or serialised in this context information preserves it. Preserving this information is the only
  way to guarantee that if we write a given piece of data, then read it back, then we read back the same result.

== Parsing functions

>>> :module Data.Time.ISO8601

Declare a regular representation with three digits permitted after the decimal point, and using a comma as the delimiter:

>>> let r = Regular { fractionalLength = 3, delimiter = Comma }

Match time of day according to /ISO 8601-1/, using this representation:

>>> iso8601Parse r "12:23:44,330" :: Maybe (Profile TimeOfDay)
Just Main { extension = Extended, representation = Regular {fractionalLength = 3, delimiter = ,}, value = 12:32:34.33 }

Because TimeOfDay is also expressed as a NominalTime, it is also possible to parse it as such:

>>> iso8601Parse r "12:23:44,330" :: Maybe (Profile NominalTime)
Just Main { extension = Extended, representation = Regular {fractionalLength = 3, delimiter = ,}, value = NominalTime {timeOfDay = 12:32:13.330} }

As an aside, when parsing we don't actually check the comma. That's because either are permitted when parsing. However, when serialising, obviously we need to make a choice!

The parsers are built on [regex-applicative](https://hackage.haskell.org/package/regex-applicative). Thus, these parsers are instances of Applicative and Alternative, and can be used to
produce bigger parsers.

== Formatting functions

>>> :module Data.Time.LocalTime

Let's use our example from above:

>>> let val = NominalTime { timeOfDay = 12 32 13.330 }
>>> let ext = Extended
>>> let q = Main { extension = ext, representation = r, value = val }

Show the time of day according to /ISO 8601-1/:

>>> iso8601Show q
"12:32:13,330"

There's one more function we can use to show the profile being wrapped:

>>> showProfilePretty q
"Profile level: Main | Any format extenstion: Extended | Representation: Regular {fractionalLength = 3, delimiter = ,} | Wrapped type: NominalTime {timeOfDay = 12:32:34.33} | ISO 8601-1 representation: 12:32:34,330"
-}

module Data.Time.ISO8601 (
    module Data.Time.ISO8601.Internal
  , module Data.Time.ISO8601.Format
  , module Data.Time.ISO8601.Parse
  ) where

import Data.Time.ISO8601.Internal hiding (between, isRegular, omitDesignator, padFixedTo, padTo, getExt)
import Data.Time.ISO8601.Format hiding (dash, colon, showAltSign)
import Data.Time.ISO8601.Parse hiding (takeDigits, withExtension, byYear, takePico, takePico', buildPico, checkRepresentation)
