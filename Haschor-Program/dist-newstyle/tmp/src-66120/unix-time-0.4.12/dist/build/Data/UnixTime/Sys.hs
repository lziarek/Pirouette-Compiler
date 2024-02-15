{-# LINE 1 "Data/UnixTime/Sys.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.UnixTime.Sys (getUnixTime) where

import Data.UnixTime.Types
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

-- from System.Time




type CTimeVal = ()
type CTimeZone = ()

foreign import ccall unsafe "gettimeofday"
    c_gettimeofday :: Ptr CTimeVal -> Ptr CTimeZone -> IO CInt

-- |
-- Get current 'UnixTime' from OS.

getUnixTime :: IO UnixTime
getUnixTime = allocaBytes (16) $ \ p_timeval -> do
{-# LINE 28 "Data/UnixTime/Sys.hsc" #-}
    throwErrnoIfMinus1_ "getClockTime" $ c_gettimeofday p_timeval nullPtr
    peek (castPtr p_timeval)
