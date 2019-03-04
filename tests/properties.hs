
-- |
--
-- TODO golden tests of known good RNAfold output as parser input
--
-- TODO re-enable the build/parse paired tests.

module Main where

import           Control.Lens
import           Control.Parallel.Strategies
import           Data.Attoparsec.ByteString.Char8 as A8
import           Data.ByteString.Builder
import           Data.ByteString.Lazy.Char8 (toStrict)
import           Data.Default.Class
import           Data.List (intersperse)
import           Debug.Trace
import qualified Data.ByteString as BS
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.TH

import           Biobase.Types.BioSequence
import           BioInf.ViennaRNA.RNAfold

-- prop_build ∷ RNAfold → Bool
-- prop_build r
--   | True = traceShow lbs False
--   where
--     bld = builderRNAfold r
--     lbs = toStrict $ toLazyByteString bld
-- 
-- prop_build_parse ∷ RNAfold → Bool
-- prop_build_parse r
--   | Left err ← ap = traceShow (err,r) False
--   | Right pr ← ap = pr == r
--   where
--     -- the final builder of all @RNAfold@ structures
--     bld = builderRNAfold r
--     lbs = toStrict $ toLazyByteString bld
--     ap  = parseOnly ((pRNAfold NoRewrite 37) <* endOfInput) lbs
-- 
-- 
-- prop_builds_parses ∷ [RNAfold] → Bool
-- prop_builds_parses rs
--   | Left err  ← ap = traceShow err False
--   | Right prs ← ap = prs == rs
--   where
--     -- the final builder of all @RNAfold@ structures
--     bld = mconcat $ intersperse "\n" $ map builderRNAfold rs
--     lbs = toStrict $ toLazyByteString bld
--     ap  = parseOnly (many' (pRNAfold NoRewrite 37) <* endOfInput) lbs

-- | Run RNAfold in parallel.
--
-- NOTE being tested in the ViennaRNA-bindings already!

prop_parallel_RNAfold ∷ [BioSequence RNA] → Bool
prop_parallel_RNAfold ss' = ps == ns
  where
    o = def
    ss = [ s | s ← ss', BS.length (s^._BioSequence) > 0 ]
    ps = parMap rdeepseq (rnafold o) ss
    ns = map (rnafold o) ss

main ∷ IO ()
main = $(defaultMainGenerator)

