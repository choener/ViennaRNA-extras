
module BioInf.ViennaRNA.RNAfold where

import           Control.Arrow
import           Control.Lens
import           Data.ByteString (ByteString)
import           Data.Tuple (swap)

import           Biobase.Types.Energy
import           Biobase.Types.Sequence (mkRNAseq, RNAseq, rnaseq)
import           Biobase.Types.Structure
import qualified BioInf.ViennaRNA.Bindings as Bindings

import           BioInf.ViennaRNA.Internal
import           BioInf.ViennaRNA.Types



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
  _centroid ← uncurry Folded . swap <$> (DG *** RNAss) <$> Bindings.centroidTemp _temperature (_input^.rnaseq)
  -- fucked up from here
  let _mfeFrequency = -1
  let _ensemble = Folded (RNAss "DO NOT USE ME") (DG 999999)
  let _diversity = 0
  return $ RNAfold {..}

