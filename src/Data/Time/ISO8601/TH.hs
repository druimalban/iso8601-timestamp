{-# LANGUAGE TemplateHaskell #-}

module Data.Time.ISO8601.TH where

import Lens.Micro (set)
import Lens.Micro.TH (LensRules, lensField, lensRules, makeLensesWith, DefName (TopName))
import Language.Haskell.TH (mkName, nameBase)

customRules = set lensField (\_ _ n -> [TopName (mkName ("_" ++ nameBase n))]) lensRules
