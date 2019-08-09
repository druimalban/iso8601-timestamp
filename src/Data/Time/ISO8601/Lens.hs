{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : Data.Time.ISO8601.Lens
Description : Define various lenses for the various datatypes.
Stability   : experimental
Portability : POSIX

Define various lenses for the various datatypes in Data.Time.ISO8601.Internal, exported as appropriate by Data.Time.ISO8601.
-}

module Data.Time.ISO8601.Lens where

import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Lens.Micro
import Lens.Micro.TH

import Data.Time.ISO8601.Internal
import Data.Time.ISO8601.TH

-- | Define a lens for 'Data.Time.LocalTime.ctMonths' component of 'Data.Time.LocalTime.CalendarDiffTime'.
_ctMonths :: Lens' CalendarDiffTime Integer
_ctMonths = lens ctMonths (\cdt newCtMonths -> cdt { ctMonths = newCtMonths })

-- | Define a lens for 'Data.Time.LocalTime.ctTime' component of 'Data.Time.LocalTime.CalendarDiffTime'.
_ctTime :: Lens' CalendarDiffTime NominalDiffTime
_ctTime = lens ctTime (\cdt newCtTime -> cdt { ctTime = newCtTime })

-- | Define a lens for 'Data.Time.Calendar.cdMonths' component of 'Data.Time.Calendar.CalendarDiffDays'.
_cdMonths :: Lens' CalendarDiffDays Integer
_cdMonths = lens cdMonths (\cd newCdMonths -> cd { cdMonths = newCdMonths })
-- | Define a lens for 'Data.Time.Calendar.cdDays' component of 'Data.Time.Calendar.CalendarDiffDays'.
_cdDays :: Lens' CalendarDiffDays Integer
_cdDays = lens cdDays (\cd newCdDays -> cd { cdDays = newCdDays })

-- | Define a lens for 'Data.Time.LocalTime.todHour' component of 'Data.Time.LocalTime.TimeOfDay'.
_todHour :: Lens' TimeOfDay Int
_todHour = lens todHour (\tod newHour -> tod { todHour = newHour })
-- | Define a lens for 'Data.Time.LocalTime.todMin' component of 'Data.Time.LocalTime.TimeOfDay'.
_todMin :: Lens' TimeOfDay Int
_todMin = lens todMin (\tod newMin -> tod { todMin = newMin })

-- | Define a lens for 'Data.Time.LocalTime.todSec' component of 'Data.Time.LocalTime.TimeOfDay'.
_todSec :: Lens' TimeOfDay Pico
_todSec = lens todSec (\tod newSec -> tod { todSec = newSec })

makeLensesWith customRules ''Profile
makeLensesWith customRules ''Representation
makeLensesWith customRules ''NominalDate
makeLensesWith customRules ''NominalTime
makeLensesWith customRules ''NominalZonedTime
makeLensesWith customRules ''TimeStamp
makeLensesWith customRules ''ZonedTimeStamp
makeLensesWith customRules ''AcrossYMD
makeLensesWith customRules ''AcrossHMS
makeLensesWith customRules ''Duration
makeLensesWith customRules ''Interval
makeLensesWith customRules ''Repeat
