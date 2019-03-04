
-- | Everything needed to wrap @RNAfold@ results, parse lines with @RNAfold@
-- input, fold a sequence with @rnafold@, and more.

module BioInf.ViennaRNA.RNAfold
  ( module BioInf.ViennaRNA.RNAfold
  , module BioInf.ViennaRNA.Types
  ) where

import           Control.Arrow
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad (guard)
import           Data.Array.IArray as AI
import qualified Data.Array.Unboxed as AU
import           Data.Attoparsec.ByteString as A
import           Data.Attoparsec.ByteString.Char8 as A8
import           Data.ByteString.Builder
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 as BS
import           Data.Char (toUpper)
import           Data.Default.Class
import           Data.Monoid
import           Data.Tuple (swap)
import           Debug.Trace
import           GHC.Generics (Generic)
import           Prelude
import           Test.QuickCheck as QC
import           Text.Printf

import           Biobase.Types.Energy
import           Biobase.Types.BioSequence
import           Biobase.Types.Structure
import qualified BioInf.ViennaRNA.Bindings as Bindings

import           BioInf.ViennaRNA.Internal
import           BioInf.ViennaRNA.Types



-- | Space-efficient RNAfold structure. RNAfold allows a DNA input, but this is
-- not allowed in this data structure. If you want that, keep the original
-- input string around.
--
-- This structure provides only @Getter@'s. Only the @sequenceID@ can be
-- updated via @sequenceIDlens@. This is to prevent accidental updates of
-- fields that are actually interdependent.
--
-- Missing parts are sloppily encoded by bogus values and empty strings. All
-- @Folded@ structures and derived values are lazy. In case @rnafold@ was used
-- to construct the structure, calculations are deferred until needed.
--
-- TODO newtype the sequence id.
--
-- TODO temperature. How to encode? Kelvin? In BiobaseTypes! Could use (Nat)SciTypes!
--
-- TODO complete BP probability array
--
-- TODO lazy fields for computation on demand!
--
-- TODO Wrapped via @Maybe@?

data RNAfold = RNAfold
  { _sequenceID   ∷ !ByteString
  -- ^ Set to @not . null@ if the sequence was given a name. This is (likely) a
  -- fasta-style identifier.
  , _input        ∷ !(BioSequence RNA)
  -- ^ The input sequence, converting into an RNA string.
  , _mfe          ∷ !Folded
  -- ^ Minimum-free energy and corresponding structure.
  , _mfeFrequency ∷ !Double
  -- ^ The mfe frequency can be calculated as follows: @exp ((ensemble energy -
  -- mfe energy) / kT)@.
  --
  -- ^ TODO newtype wrapper?
  , _ensemble     ∷ !Folded
  -- ^ Uses special syntax with unpaired, weakly paired, somewhat paired,
  -- somewhat paired up or down, strongly paired up or down for the ensemble.
  -- The energy is the *ensemble free energy*.
  , _basepairProbs  ∷ !(AU.UArray (Int,Int) Double)
  -- ^ If the array dimensions are more than (0,0), this array holds the
  -- probability that @(i,j)@ pair.
  --
  -- TODO use @Prob@ instead of Double
  , _centroid     ∷ !Folded
  -- ^ Centroid energy and structure.
  , _centroidDistance ∷ !Double
  -- ^ Centroid distance to ensemble.
  , _diversity        ∷ !Double
  -- ^ Average basepair distance between all structures in the Boltzmann
  -- ensemble
  --
  -- TODO Needs own newtype?
  , _temperature      ∷ !Double
  -- ^ Temperature in Celsius
  --
  -- TODO own newtype Celsius
  }
  deriving (Show,Eq,Ord,Generic)
makeLensesWith (lensRules & generateUpdateableOptics .~ False) ''RNAfold
makeLensesFor [("_sequenceID", "sequenceIDlens")] ''RNAfold

instance NFData RNAfold where
  rnf RNAfold{..} = ()

-- | Getter that returns @Maybe (Input,MFE)@ as a pair. @Just@ only if @_mfe@
-- is not @absentFolded@.

getMFE ∷ IndexPreservingGetter RNAfold (Maybe (BioSequence RNA, Folded))
getMFE = to (\rna → if _mfe rna == absentFolded then Nothing else Just (_input rna, _mfe rna))
{-# Inline getMFE #-}



-- | Fold a sequence.
--
-- This function is threadsafe via the viennaRNA-mutex.
--
-- TODO add temperature parameter
--
-- TODO consider creating a "super-lens" that updates whenever @_input@ or
-- @_temperature@ change.

rnafold ∷ Bindings.RNAfoldOptions → BioSequence RNA → RNAfold
rnafold o _input = unsafePerformIO $! do
  let _temperature = Bindings._fotemperature o
  let _sequenceID = ""
  (mfeT,ensembleT,centroidT) ← Bindings.rnafold o (_input^._BioSequence)
  let _mfe = maybe absentFolded (uncurry Folded . (DG *** RNAss)) mfeT
  let (_centroid, _centroidDistance) = maybe (absentFolded, 1/0) (\(e,s,d) → (Folded (DG e) (RNAss s), d)) centroidT
  -- fucked up from here
  let k0 = 273.15
  let gasconst = 1.98717 -- in kcal * (K^(-1)) * (mol^(-1))
  let kT = (k0 + 37) * gasconst * 1000
  -- TODO still single-threaded!
  --
  -- TODO we should return the array of pair probabilities as well.
  let (_ensemble, _basepairProbs) = maybe (absentFolded, AU.array ((0,0),(0,0)) [((0,0),0)])
                                    (\(e,s,arr) → (Folded (DG e) (RNAss s), arr)) ensembleT
  let _diversity = 999999
  -- TODO the energy of the mfe structure calculated with @dangles=1@ model,
  -- otherwise we get different mfe frequency values compared to rnafold. Still
  -- valid?
  let _mfeFrequency = exp $ (_centroid^.foldedEnergy.to dG - _mfe^.foldedEnergy.to dG) / kT
  return $! RNAfold {..}
{-# NoInline rnafold #-}



data RNArewrite
  = NoRewrite
  | ForceRNA
  deriving (Read,Show,Eq,Ord,Generic)

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
  → Double
  → Parser RNAfold
pRNAfold r celsius = do
  let _temperature = celsius
  let endedLine = A.takeTill isEndOfLine <* endOfLine
  let s2line = A8.takeWhile1 (`BS.elem` "(.)") <* skipSpace
  let ensline = A8.takeWhile1 (`BS.elem` "(){}.,|") <* skipSpace
  let rewrite = case r of
        NoRewrite → id
        ForceRNA  → BS.map (go . toUpper) where
          go x
            | x `BS.elem` "ACGUacgu" = x
          go x   = 'N'
  _sequenceID ← option "" $ char '>' *> endedLine
  _input      ← BioSequence <$> rewrite <$> endedLine
  let l = _input^._BioSequence.to BS.length
  let lenGuard s2 = guard (BS.length s2 == l)
        <?> ("s2 line length /= _input length: " ++ show (s2,_input) ++ "")
  -- mfe is always present ?!
  _mfe ← do s2 ← s2line
            lenGuard s2
            skipSpace
            e  ← char '(' *> skipSpace *> signed double <* char ')'
            let _foldedStructure = RNAss s2
            let _foldedEnergy    = DG e
            return Folded{..}
            <?> "mfe"
  -- from here on, things are optional, including the newline after the mfe
  -- energy!
  _ensemble ← option absentFolded $
    (do endOfLine
        s2 ← ensline
        lenGuard s2
        skipSpace
        e  ← char '[' *> skipSpace *> signed double <* char ']'
        let _foldedStructure = RNAss s2
        let _foldedEnergy    = DG e
        return Folded{..}
        <?> "ensemble")
  let _basepairProbs = AU.array ((0,0),(0,0)) [((0,0),0)]
  (_centroid, _centroidDistance) ← option (absentFolded, 0) $
    (do endOfLine
        s2 ← s2line
        lenGuard s2
        e ← char '{' *> skipSpace *> signed double
        -- TODO handle @d@ values
        d ← skipSpace *> "d=" *> skipSpace *> double
        skipSpace *> char '}'
        let _foldedStructure  = RNAss s2
        let _foldedEnergy     = DG e
        return (Folded{..}, d)
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



builderRNAfold ∷ RNAfold → Builder
builderRNAfold RNAfold{..} = mconcat [hdr, sqnce, emfe, nsmbl, cntrd]
  where
    -- SLOW!
    dbl d = byteString . BS.pack $ printf "%.2f" (d∷Double)
    addFolded l r Folded{..} = if BS.null (_rnass _foldedStructure)
                               then mempty
                               else nl <> byteString (_rnass _foldedStructure) <> char7 ' '
                                    <> char7 l <> dbl (dG _foldedEnergy) <> char7 r
    nl = char7 '\n'
    hdr = if BS.null _sequenceID then mempty else char7 '>' <> byteString _sequenceID <> nl
    sqnce = byteString (_input^._BioSequence)
    emfe = addFolded '(' ')' _mfe
    nsmbl = addFolded '[' ']' _ensemble
    cntrd = let s = _centroid^.foldedStructure.rnass
            in if BS.null (_centroid^.foldedStructure.rnass)
                then mempty
                else nl <> byteString s <> " {"
                     <> dbl (_centroid^.foldedEnergy.to dG)
                     <> " d=" <> dbl _diversity <> "}"



-- ** QuickCheck generator for 'RNAfold'. This is slightly unusual but allows
-- us to generate random sequence/structure pairs easily.
--
-- This should only be used for quickcheck testing, not anything involving
-- properties of random RNAs.
--
-- Generation is simple: A sequence with @[0,100]@ characters from @ACGU@ is
-- generated and the 'RNAfold' structure is filled by 'rnafold'. We test for
-- size @0@ explicitly, too!

instance Arbitrary RNAfold where
  -- Generate a sequence and calculate the structure for it.
  arbitrary =  choose (0,100)
            >>= \k → rnafold def <$> mkRNAseq <$> BS.pack <$> vectorOf k (QC.elements "ACGU")
  -- Try with all sequences missing one character.
  shrink rna = [ rnafold def $ mkRNAseq $ BS.pack s | s ← shrink $ rna^.input._BioSequence.to unpack ]

