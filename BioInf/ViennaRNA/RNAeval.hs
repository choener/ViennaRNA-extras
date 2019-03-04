
module BioInf.ViennaRNA.RNAeval where

import Biobase.Types.Energy (DG(..))
import Biobase.Types.BioSequence
import Biobase.Types.Structure (RNAss(..))
import BioInf.ViennaRNA.Bindings (eosTemp)

import BioInf.ViennaRNA.Internal



-- | High-level wrapper for energy-of-struct calculations similar to the
-- command line @RNAeval@.

rnaeval ∷ Double → BioSequence RNA → RNAss → DG
rnaeval t (BioSequence s1) (RNAss s2)
  = unsafePerformIO . withMutex
  $! DG <$> eosTemp t s1 s2
{-# NoInline rnaeval #-}

