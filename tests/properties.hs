
module Main where

import Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.List (intersperse)
import Debug.Trace
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import BioInf.ViennaRNA.RNAfold

prop_build ∷ RNAfold → Bool
prop_build r
  | True = traceShow lbs False
  where
    bld = builderRNAfold r
    lbs = toStrict $ toLazyByteString bld

prop_build_parse ∷ RNAfold → Bool
prop_build_parse r
  | Left err ← ap = traceShow (err,r) False
  | Right pr ← ap = pr == r
  where
    -- the final builder of all @RNAfold@ structures
    bld = builderRNAfold r
    lbs = toStrict $ toLazyByteString bld
    ap  = parseOnly ((pRNAfold NoRewrite 37) <* endOfInput) lbs


prop_builds_parses ∷ [RNAfold] → Bool
prop_builds_parses rs
  | Left err  ← ap = traceShow err False
  | Right prs ← ap = prs == rs
  where
    -- the final builder of all @RNAfold@ structures
    bld = mconcat $ intersperse "\n" $ map builderRNAfold rs
    lbs = toStrict $ toLazyByteString bld
    ap  = parseOnly (many' (pRNAfold NoRewrite 37) <* endOfInput) lbs

main ∷ IO ()
main = $(defaultMainGenerator)

