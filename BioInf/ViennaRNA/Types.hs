
module BioInf.ViennaRNA.Types where

import Control.Lens
import Control.DeepSeq
import GHC.Generics (Generic)

import Biobase.Types.Energy
import Biobase.Types.Structure



-- | Holds a pair of energy and structure.

data Folded = Folded
  { _foldedStructure  ∷ !RNAss
  , _foldedEnergy     ∷ !DG
  }
  deriving (Read,Show,Eq,Ord,Generic)
makeLensesWith (lensRules & generateUpdateableOptics .~ False) ''Folded

absentFolded = Folded
  { _foldedStructure = mempty
  , _foldedEnergy    = DG (-1/0)
  }
{-# Inlinable absentFolded #-}

instance NFData Folded

