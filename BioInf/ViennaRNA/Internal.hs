
-- | Internal mutex system to be used as long as the ViennaRNA package is not
-- thread-safe.

module BioInf.ViennaRNA.Internal
  ( viennaRNAmutex
  , withMutex
  , unsafePerformIO
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar, takeMVar, putMVar)
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)
import Debug.Trace

-- | The mutex guarding single-threaded access to the @ViennaRNA@ bindings as
-- long as the @C@ library is not multi-threaded.

viennaRNAmutex ∷ MVar ()
viennaRNAmutex = unsafePerformIO $! newMVar ()
{-# NoInline viennaRNAmutex #-}

-- | Work with the ViennaRNA mutex.
--
-- This version seems strict enough to not block indefinitely.

withMutex ∷ IO a → IO a
withMutex f = do
  () ← takeMVar viennaRNAmutex
  !x ← f
  putMVar viennaRNAmutex ()
  return $! x
{-# NoInline withMutex #-}

