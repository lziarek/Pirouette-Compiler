{-# LINE 1 "Data/UnixTime/Types.hsc" #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.UnixTime.Types where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString
import Data.ByteString.Char8 ()
import Data.Int

{-# LINE 12 "Data/UnixTime/Types.hsc" #-}
import Foreign.C.Types
import Foreign.Storable

{-# LINE 15 "Data/UnixTime/Types.hsc" #-}
import Data.Binary

{-# LINE 17 "Data/UnixTime/Types.hsc" #-}
import GHC.Generics



-- |
-- Data structure for Unix time.
--
-- Please note that this uses GHC-derived 'Eq' and 'Ord' instances.
-- Notably
--
-- >>> UnixTime 1 0 > UnixTime 0 999999999
-- True
--
-- You should instead use 'UnixDiffTime' along with its helpers such
-- as 'Data.UnixTime.microSecondsToUnixDiffTime' which will ensure
-- that such unusual values are never created.
data UnixTime = UnixTime {
    -- | Seconds from 1st Jan 1970
    utSeconds :: {-# UNPACK #-} !CTime
    -- | Micro seconds (i.e. 10^(-6))
  , utMicroSeconds :: {-# UNPACK #-} !Int32
  } deriving (Eq,Ord,Show,Generic)

instance Storable UnixTime where
    sizeOf _    = ((16))
{-# LINE 42 "Data/UnixTime/Types.hsc" #-}
    alignment _ = (8)
{-# LINE 43 "Data/UnixTime/Types.hsc" #-}

{-# LINE 67 "Data/UnixTime/Types.hsc" #-}
    peek ptr    = UnixTime
            <$> ((\hsc_ptr -> peekByteOff hsc_ptr 0))  ptr
{-# LINE 69 "Data/UnixTime/Types.hsc" #-}
            <*> ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 70 "Data/UnixTime/Types.hsc" #-}
    poke ptr ut = do
            ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  ptr (utSeconds ut)
{-# LINE 72 "Data/UnixTime/Types.hsc" #-}
            ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr (utMicroSeconds ut)
{-# LINE 73 "Data/UnixTime/Types.hsc" #-}

{-# LINE 74 "Data/UnixTime/Types.hsc" #-}


{-# LINE 76 "Data/UnixTime/Types.hsc" #-}
instance Binary UnixTime where
        put (UnixTime (CTime sec) msec) = do
            put sec
            put msec
        get = UnixTime <$> (CTime `fmap` get) <*> get

{-# LINE 82 "Data/UnixTime/Types.hsc" #-}

-- |
-- Format of the strptime()/strftime() style.
type Format = ByteString

-- |
-- Data structure for UnixTime diff.
--
-- It is up to the user to ensure that @'udtMicroSeconds' < 1000000@.
-- Helpers such as 'Data.UnixTime.microSecondsToUnixDiffTime' can help
-- you to create valid values. For example, it's a mistake to use
-- 'Data.Text.addUnixDiffTime' with a value @UnixDiffTime 0 9999999@
-- as it will produce an incorrect value back. You should instead use
-- functions such as 'Data.UnixTime.microSecondsToUnixDiffTime' to
-- create values that are in-range. This avoids any gotchas when then
-- doing comparisons.
data UnixDiffTime = UnixDiffTime {
    -- | Seconds
    udtSeconds :: {-# UNPACK #-} !CTime
    -- | Micro seconds (i.e. 10^(-6))
  , udtMicroSeconds :: {-# UNPACK #-} !Int32
  } deriving (Eq,Ord,Show)
