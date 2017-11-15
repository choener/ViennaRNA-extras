
module Main where

import Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.List (intersperse)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import BioInf.ViennaRNA.RNAfold



prop_build_parse ∷ [RNAfold] → Bool
prop_build_parse rs
  | Left err  ← ap = False
  | Right prs ← ap = prs == rs
  where
    -- the final builder of all @RNAfold@ structures
    bld = mconcat $ intersperse "\n" $ map builderRNAfold rs
    lbs = toStrict $ toLazyByteString bld
    ap  = parseOnly (many' (pRNAfold NoRewrite) <* endOfInput) lbs

main ∷ IO ()
main = $(defaultMainGenerator)

