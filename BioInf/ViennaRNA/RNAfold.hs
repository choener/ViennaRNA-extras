
module BioInf.ViennaRNA.RNAfold where

import           Data.ByteString (ByteString)
import           Control.Lens
import           Control.Arrow

import qualified BioInf.ViennaRNA.Bindings as Bindings
import           Biobase.Types.Sequence (mkRNAseq, RNAseq, rnaseq)
import           Biobase.Types.Structure
import           Biobase.Types.Energy

import           BioInf.ViennaRNA.Types
import           BioInf.ViennaRNA.Internal



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
  _mfe      ← uncurry Folded <$> (DG *** RNAss) <$> Bindings.mfe (_input^.rnaseq)
  _centroid ← uncurry Folded <$> (DG *** RNAss) <$> Bindings.centroidTemp _temperature (_input^.rnaseq)
  -- fucked up from here
  let _mfeFrequency = -1
  let _ensemble = Folded (DG 999999) (RNAss "DO NOT USE ME")
  let _diversity = 0
  return $ RNAfold {..}

