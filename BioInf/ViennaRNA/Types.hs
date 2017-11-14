
module BioInf.ViennaRNA.Types where

import Control.Lens
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BS
import Data.Char (toUpper)
import GHC.Generics (Generic)

import Biobase.Types.Energy
import Biobase.Types.Sequence
import Biobase.Types.Structure

import Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.ByteString as A



-- | Holds a pair of energy and structure.
--
-- These should be getters as well!

data Folded = Folded
  { _foldedStructure  ∷ !RNAss
  , _foldedEnergy     ∷ !DG
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



data RNArewrite
  = NoRewrite
  | ForceRNA

-- | Parsing for 'RNAfold'. This should parse all variants that @RNAfold@
-- produces.
--
-- The parser for a 'Folded' structure has to deal with different "energy"
-- types. The different energies are bracketed by different types of brackets.
--
-- @
-- mfe        (((...))) ( -1.20)
-- ensemble   (((...))) [ -1.41]
-- centroid   (((...))) { -1.20 d=1.06}
-- @
--
-- TODO Move into submodule.
--
-- TODO pipes-based streaming parser
--
-- TODO how to handle parsing the BP probability array, if known?
--
-- TODO I think it is possible to figure the line type based on the energy and
-- the brackets around the energy.

pRNAfold
  ∷ RNArewrite
  → Parser RNAfold
pRNAfold r = do
  let endedLine = A.takeTill isEndOfLine <* endOfLine
  let s2line = A8.takeWhile1 (`BS.elem` "(.)") <* skipSpace
  let nope = Folded (RNAss "") (DG 0)
  let rewrite = case r of
        NoRewrite → id
        ForceRNA  → BS.map (go . toUpper) where
          go x
            | x `BS.elem` "ACGUacgu" = x
          go x   = 'N'
  _sequenceID ← option "" $ char '>' *> endedLine
  _input      ← RNAseq <$> rewrite <$> endedLine
  let l = _input^.rnaseq.to BS.length
  -- mfe is always present ?!
  _mfe ← do s2 ← s2line
            guard (BS.length s2 == l) <?> "s2 line length /= _input length"
            skipSpace
            e  ← char '(' *> skipSpace *> signed double <* char ')'
            let _foldedStructure = RNAss s2
            let _foldedEnergy    = DG e
            return Folded{..}
            <?> "mfe"
  -- from here on, things are optional, including the newline after the mfe
  -- energy!
  _ensemble ← option nope $
    (do endOfLine
        s2 ← s2line
        skipSpace
        e  ← char '[' *> skipSpace *> signed double <* char ']'
        let _foldedStructure = RNAss s2
        let _foldedEnergy    = DG e
        return Folded{..}
        <?> "ensemble")
  _centroid ← option nope $
    (do endOfLine
        s2 ← s2line
        e ← char '{' *> skipSpace *> signed double
        -- TODO handle @d@ values
        d ← skipSpace *> "d=" *> skipSpace *> double
        skipSpace *> char '}'
        let _foldedStructure = RNAss s2
        let _foldedEnergy    = DG e
        return Folded{..}
        <?> "centroid")
  (_mfeFrequency, _diversity) ← option (0, 0) $
    (do endOfLine -- previous line ending
        skipSpace
        "frequency of mfe structure in ensemble"
        skipSpace
        f ← double
        "; ensemble diversity"
        skipSpace
        ed ← double
        skipSpace
        return (f, ed)
        <?> "mfe frequency / diversity")
  return RNAfold{..}

