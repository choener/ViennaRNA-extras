
module BioInf.ViennaRNA.Types where

import Control.Lens
import Control.DeepSeq
import GHC.Generics (Generic)

import Biobase.Types.Energy
import Biobase.Types.Structure



-- | Holds a pair of energy and structure.

data Folded = Folded
  { _foldedEnergy     ∷ !DG
  , _foldedStructure  ∷ !RNAss
  }
  deriving (Read,Show,Eq,Ord,Generic)
makeLensesWith (lensRules & generateUpdateableOptics .~ False) ''Folded

absentFolded = Folded
  { _foldedEnergy    = DG (-1/0)
  , _foldedStructure = mempty
  }
{-# Inlinable absentFolded #-}

instance NFData Folded

