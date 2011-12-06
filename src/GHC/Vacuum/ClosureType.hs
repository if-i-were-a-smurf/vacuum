{-# LANGUAGE CPP #-}
-- |
-- Module      : GHC.Vacuum.ClosureType
-- Copyright   : (c) Matt Morrow 2009, Austin Seipp 2011
-- License     : LGPLv3
-- 
-- Maintainer  : as@hacks.yi.org
-- Stability   : experimental
-- Portability : non-portable (GHC only)
-- 
-- This module exports the different kind of closure types there
-- are per GHC version.
-- 
-- NOTE: the 'ClosureType' datatype is automatically generated, based
-- on the version of GHC, and the proper one is included for you.
-- 
module GHC.Vacuum.ClosureType
       ( ClosureType(..)
       , isFun      -- :: ClosureType -> Bool
       , isThunk    -- :: ClosureType -> Bool
       , isCon      -- :: ClosureType -> Bool
       ) where

-- Import the correct ClosureType datatype based on
-- the version

#if __GLASGOW_HASKELL__ == 702
import GHC.Vacuum.ClosureType.V702 (ClosureType(..))
#elif __GLASGOW_HASKELL__ == 700
import GHC.Vacuum.ClosureType.V700 (ClosureType(..))
#else
#error Unsupported GHC version in ClosureTypes.hs!
#endif 

------------------------------------------------

isFun :: ClosureType -> Bool
isFun FUN = True
isFun FUN_1_0 = True
isFun FUN_0_1 = True
isFun FUN_2_0 = True
isFun FUN_1_1 = True
isFun FUN_0_2 = True
isFun FUN_STATIC = True
isFun _ = False

isThunk :: ClosureType -> Bool
isThunk AP = True
isThunk THUNK = True
isThunk THUNK_1_0 = True
isThunk THUNK_0_1 = True
isThunk THUNK_2_0 = True
isThunk THUNK_1_1 = True
isThunk THUNK_0_2 = True
isThunk THUNK_STATIC = True
isThunk THUNK_SELECTOR = True
isThunk _ = False

isCon :: ClosureType -> Bool
isCon CONSTR = True
isCon CONSTR_1_0 = True
isCon CONSTR_0_1 = True
isCon CONSTR_2_0 = True
isCon CONSTR_1_1 = True
isCon CONSTR_0_2 = True
isCon CONSTR_STATIC = True
isCon CONSTR_NOCAF_STATIC = True
isCon _ = False
