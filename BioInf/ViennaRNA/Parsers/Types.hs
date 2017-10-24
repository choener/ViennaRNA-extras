
-- | Data types for the different @ViennaRNA@ outputs.
--
-- TODO I think these should be more generic types to be filled in by
-- high-level functions...

module BioInf.ViennaRNA.Parsers.Types where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Control.Lens

import Biobase.Types.Sequence
import Biobase.Types.Structure
import Biobase.Types.Energy



-- | Space-efficient RNAfold structure. RNAfold allows a DNA input, but this is
-- not allowed in this data structure. If you want that, keep the original
-- input string around.
--
-- Missing parts are sloppily encoded by bogus values and empty strings.
--
-- TODO newtype the sequence id.

data RNAfolded = RNAfolded
  { _sequenceID           ∷ !ByteString
  -- ^ Set to @not . null@ if the sequence was given a name. This is (likely) a
  -- fasta-style identifier.
  , _rnaFoldSequence      ∷ !RNAseq
  , rnaFoldMFEStruc       ∷ !RNAss
  , rnaFoldMFEEner        ∷ !DG
  , rnaFoldMfeFrequency   ∷ !Double
  -- ^ TODO newtype wrapper?
  , rnaFoldEnsembleStruc  ∷ !ByteString
  -- ^ uses special syntax with unpaired, weakly paired, somewhat paired,
  -- somewhat paired up or down, strongly paired up or down for the ensemble
  , rnaFoldEnsembleEner   ∷ !Double
  -- ^ this *is* the ensemble free energy.
  -- TODO Needs own newtype?
  , rnaFoldCentroidStruc  ∷ !RNAss
  , rnaFoldCentroidEner   ∷ !DG
  , rnaFoldDiversity      ∷ !Double
  -- ^ TODO Needs own newtype?
  }
  deriving (Read,Show,Eq,Ord,Generic)
makeLenses ''RNAfolded

