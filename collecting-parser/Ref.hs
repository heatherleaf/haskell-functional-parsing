

--------------------------------------------------
-- Implementation of observable sharing
-- as described in section 8.1


module Ref (Ref, ref, deref) where

import IOExts (unsafePerformIO, IORef, newIORef, readIORef)


newtype Ref a = Ref (IORef a)
		deriving Eq


ref :: a -> Ref a
ref a = Ref (unsafePerformIO (newIORef a))


deref :: Ref a -> a
deref (Ref ref) = unsafePerformIO (readIORef ref)

