
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



