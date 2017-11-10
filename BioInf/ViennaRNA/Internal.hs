
-- | Internal mutex system to be used as long as the ViennaRNA package is not
-- thread-safe.

module BioInf.ViennaRNA.Internal
  ( viennaRNAmutex
  , withMutex
  , unsafePerformIO
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import System.IO.Unsafe (unsafePerformIO)

-- | The mutex guarding single-threaded access to the @ViennaRNA@ bindings as
-- long as the @C@ library is not multi-threaded.

viennaRNAmutex ∷ MVar ()
viennaRNAmutex = unsafePerformIO $ newMVar ()
{-# NoInline viennaRNAmutex #-}

withMutex f = withMVar viennaRNAmutex $ \() → f
{-# Inline withMutex #-}

