
-- | Data types for the different @ViennaRNA@ outputs.

module BioInf.ViennaRNA.Parsers.Types where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)

import Biobase.Types.Sequence

-- | Space-efficient RNAfold structure

data RNAfoldResult = RNAfoldResult
  { rnaFoldSequence       ∷ !ByteString
  , rnaFoldMFEStruc       ∷ !ByteString
  , rnaFoldMFEEner        ∷ !Double
  , rnaFoldMfeFrequency   ∷ !Double
  , rnaFoldEnsembleStruc  ∷ !ByteString
  -- ^ uses special syntax with unpaired, weakly paired, somewhat paired,
  -- somewhat paired up or down, strongly paired up or down for the ensemble
  , rnaFoldEnsembleEner   ∷ !Double
  -- ^ this *is* the ensemble free energy
  , rnaFoldCentroidStruc  ∷ !ByteString
  , rnaFoldCentroidEner   ∷ !Double
  , rnaFoldDiversity      ∷ !Double
  }
  deriving (Read,Show,Eq,Ord,Generic)

