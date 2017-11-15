
-- | Everything needed to wrap @RNAfold@ results, parse lines with @RNAfold@
-- input, fold a sequence with @rnafold@, and more.

module BioInf.ViennaRNA.RNAfold where

import           Control.Arrow
import           Control.Lens
import           Control.Monad (guard)
import           Data.Attoparsec.ByteString as A
import           Data.Attoparsec.ByteString.Char8 as A8
import           Data.ByteString.Builder
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 as BS
import           Data.Char (toUpper)
import           Data.Monoid
import           Data.Tuple (swap)
import           GHC.Generics (Generic)
import           Test.QuickCheck as QC
import           Text.Printf
import           Debug.Trace
import           Control.DeepSeq

import           Biobase.Types.Energy
import           Biobase.Types.Sequence (mkRNAseq, RNAseq(..), rnaseq)
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
  , _input        ∷ !RNAseq
  -- ^ The input sequence, converting into an RNA string.
  , _mfe          ∷ Folded
  -- ^ Minimum-free energy and corresponding structure.
  , _mfeFrequency ∷ Double
  -- ^ The mfe frequency can be calculated as follows: @exp ((ensemble energy -
  -- mfe energy) / kT)@.
  --
  -- ^ TODO newtype wrapper?
  , _ensemble     ∷ Folded
  -- ^ Uses special syntax with unpaired, weakly paired, somewhat paired,
  -- somewhat paired up or down, strongly paired up or down for the ensemble.
  -- The energy is the *ensemble free energy*.
  , _centroid     ∷ Folded
  -- ^ Centroid energy and structure.
  , _centroidDistance ∷ Double
  -- ^ Centroid distance to ensemble.
  , _diversity        ∷ Double
  -- ^ Average basepair distance between all structures in the Boltzmann
  -- ensemble
  --
  -- TODO Needs own newtype?
  , _temperature      ∷ !Double
  -- ^ Temperature in Celsius
  --
  -- TODO own newtype Celsius
  }
  deriving (Read,Show,Eq,Ord,Generic)
makeLensesWith (lensRules & generateUpdateableOptics .~ False) ''RNAfold
makeLensesFor [("_sequenceID", "sequenceIDlens")] ''RNAfold

instance NFData RNAfold



-- | Fold a sequence.
--
-- This function is threadsafe via the viennaRNA-mutex.
--
-- TODO add temperature parameter
--
-- TODO consider creating a "super-lens" that updates whenever @_input@ or
-- @_temperature@ change.

rnafold ∷ ByteString → RNAfold
rnafold inp' = unsafePerformIO . withMutex $ do
  let _temperature = 37
  let _sequenceID = ""
  let _input = mkRNAseq inp'
  _mfe      ← uncurry Folded . swap <$> (DG *** RNAss) <$> Bindings.mfe (_input^.rnaseq)
  (_centroid, _centroidDistance) ← (\(e,s,d) → (Folded (RNAss s) (DG e), d)) <$> Bindings.centroidTemp _temperature (_input^.rnaseq)
  -- fucked up from here
  let k0 = 273.15
  let gasconst = 1.98717
  let _temperature = 37
  let kT = (k0 + _temperature) * gasconst * 1000
  let _ensemble = Folded (RNAss "DO NOT USE ME") (DG 999999)
  let _diversity = 999999
  -- the energy of the mfe structure calculated with @dangles=1@ model,
  -- otherwise we get different mfe frequency values compared to rnafold.
  let d1mfeenergy = 0
  let _mfeFrequency = exp $ (_centroid^.foldedEnergy.to dG - d1mfeenergy) / kT
  return $ RNAfold {..}



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
pRNAfold r _temperature = do
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
  _ensemble ← option nope $
    (do endOfLine
        s2 ← s2line
        lenGuard s2
        skipSpace
        e  ← char '[' *> skipSpace *> signed double <* char ']'
        let _foldedStructure = RNAss s2
        let _foldedEnergy    = DG e
        return Folded{..}
        <?> "ensemble")
  (_centroid, _centroidDistance) ← option (nope, 0) $
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
    sqnce = byteString (_input^.rnaseq)
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
            >>= \k → rnafold <$> BS.pack <$> vectorOf k (QC.elements "ACGU")
  -- Try with all sequences missing one character.
  shrink rna = [ rnafold $ BS.pack s | s ← shrink $ rna^.input.rnaseq.to unpack ]

