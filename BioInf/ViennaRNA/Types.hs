
module BioInf.ViennaRNA.Types where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Control.Lens

import Biobase.Types.Sequence
import Biobase.Types.Structure
import Biobase.Types.Energy

import Data.Attoparsec.ByteString.Char8 as A



-- | Holds a pair of energy and structure.
--
-- These should be getters as well!

data Folded = Folded
  { _foldedEnergy     ∷ !DG
  , _foldedStructure  ∷ !RNAss
  }
  deriving (Read,Show,Eq,Ord,Generic)
makeLensesWith (lensRules & generateUpdateableOptics .~ False) ''Folded

-- | Space-efficient RNAfold structure. RNAfold allows a DNA input, but this is
-- not allowed in this data structure. If you want that, keep the original
-- input string around.
--
-- This structure provides only @Getter@'s. Only the @sequenceID@ can be
-- updated via @sequenceIDlens@. This is to prevent accidental updates of
-- fields that are actually interdependent.
--
-- Missing parts are sloppily encoded by bogus values and empty strings.
--
-- TODO newtype the sequence id.
--
-- TODO temperature. How to encode? Kelvin? In BiobaseTypes! Could use (Nat)SciTypes!
--
-- TODO complete BP probability array

data RNAfold = RNAfold
  { _sequenceID   ∷ !ByteString
  -- ^ Set to @not . null@ if the sequence was given a name. This is (likely) a
  -- fasta-style identifier.
  , _input        ∷ !RNAseq
  -- ^ The input sequence, converting into an RNA string.
  , _mfe          ∷ !Folded
  -- ^ Minimum-free energy and corresponding structure.
  , _mfeFrequency ∷ !Double
  -- ^ TODO newtype wrapper?
  , _ensemble     ∷ !Folded
  -- ^ Uses special syntax with unpaired, weakly paired, somewhat paired,
  -- somewhat paired up or down, strongly paired up or down for the ensemble.
  -- The energy is the *ensemble free energy*.
  , _centroid     ∷ !Folded
  -- ^ Centroid energy and structure.
  , _diversity    ∷ !Double
  -- ^ TODO Needs own newtype?
  }
  deriving (Read,Show,Eq,Ord,Generic)
makeLensesWith (lensRules & generateUpdateableOptics .~ False) ''RNAfold
makeLensesFor [("_sequenceID", "sequenceIDlens")] ''RNAfold



-- | Parsing for 'RNAfold'. This should parse all variants that @RNAfold@
-- produces.
--
-- TODO Move into submodule.
--
-- TODO pipes-based streaming parser
--
-- TODO how to handle parsing the BP probability array, if known?
--
-- TODO I think it is possible to figure the line type based on the energy and
-- the brackets around the energy.

pRNAfold ∷ Parser RNAfold
pRNAfold = do
  _sequenceID ← error "parse sequence id if given"
  _input ← error "parse RNAseq or RNAseq which is then converted"
  _mfe ← error "parse mfe structure and energy"
  _mfeFrequency ← error "parse mfe frequency"
  _ensemble ← error "parse ensemble"
  _centroid ← error "parse centroid"
  _diversity ← error "parse diversity"
  return RNAfold{..}

