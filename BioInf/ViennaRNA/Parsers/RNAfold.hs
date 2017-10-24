
-- | An efficient pipes-based parser for @RNAfold@ output.

module BioInf.ViennaRNA.Parsers.RNAfold where

{-
-- | Lazily read @RNAfold@ structures.
--
-- TODO use @pipes/machines@! we need lazy reading of files and live in an exceptt transformer stack!
-- TODO generalize transformer stack

readRNAfoldFiles
  ∷ FilePath
  → IO [RNAfoldResult]
readRNAfoldFiles fp = do
  let go [] = return []
      go (f:fs) = do
        bs ← unsafeInterleaveIO (putStrLn ("#" ++ f) >> decompress <$> BSL.readFile f)
        let rs = either error id $ runExcept (bslToRNAfoldResult bs)
        rss ← go fs
        return $ rs ++ rss
      go ∷ [FilePath] → IO [RNAfoldResult]
  FP.find FP.always (FP.extension FP.==? ".gz")  (fp </> "structures") >>= go
{-# NoInline readRNAfoldFiles #-}

-- |

bslToRNAfoldResult ∷ (Monad m) ⇒ BSL.ByteString → ExceptT String m [RNAfoldResult]
bslToRNAfoldResult bs = do
  case A.eitherResult $ A.parse pRNAfold bs of
    Left e  → throwE e
    Right r → return r
{-# Inline bslToRNAfoldResult #-}

-- | Parser for @RNAfold@ output. @RNAfold@ can have between 2 and 5 output lines.
--
-- TODO Extend the parser to deal with all cases. Our best hint is probably if
-- there is whitespace in a line.
--
-- @
-- echo "CCCAAAGGG\nCCCAAAGGG" | ./RNAfold -p
-- CCCAAAGGG
-- (((...))) ( -1.20)
-- (((...))) [ -1.41]
-- (((...))) { -1.20 d=1.06}
--  frequency of mfe structure in ensemble 0.707288; ensemble diversity 1.67  
-- CCCAAAGGG
-- (((...))) ( -1.20)
-- (((...))) [ -1.41]
-- (((...))) { -1.20 d=1.06}
--  frequency of mfe structure in ensemble 0.707288; ensemble diversity 1.67  
-- @

pRNAfold ∷ A.Parser [RNAfoldResult]
pRNAfold = A.many1' go <* A.endOfInput where
  go = do
    -- 1. sequence
    rnaFoldSequence       ← BS.copy <$> AC.takeWhile AC.isAlpha_ascii <* AC.skipSpace A.<?> "RNAfold sequence"
    -- 2. mfe
    rnaFoldMFEStruc       ← BS.copy <$> AC.takeTill AC.isSpace <* AC.skipSpace A.<?> "RNAfold MFE structure"
    rnaFoldMFEEner        ← AC.char '(' *> AC.skipSpace *> AC.double <* AC.char ')' <* AC.skipSpace A.<?> "RNAfold MFE energy"
    -- 3. ensemble
    rnaFoldEnsembleStruc  ← BS.copy <$> AC.takeTill AC.isSpace <* AC.skipSpace
    rnaFoldEnsembleEner   ← AC.char '[' *> AC.skipSpace *> AC.double <* AC.char ']' <* AC.skipSpace
    -- 4. centroid
    rnaFoldCentroidStruc  ← BS.copy <$> AC.takeTill AC.isSpace <* AC.skipSpace
    rnaFoldCentroidEner   ← AC.char '{' *> AC.skipSpace *> AC.double <* AC.skipSpace
    dequal                ← AC.string "d=" *> AC.double <* AC.char '}' <* AC.skipSpace
    -- 5.mfe frequency and diversity
    AC.string "frequency of mfe structure in ensemble" *> AC.skipSpace A.<?> "frequency"
    rnaFoldMfeFrequency ← AC.double
    AC.string "; ensemble diversity" *> AC.skipSpace
    rnaFoldDiversity ← AC.double
    AC.skipSpace
    return RNAfoldResult{..}
{-# Inline pRNAfold #-}
-}

