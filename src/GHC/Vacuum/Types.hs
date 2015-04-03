{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : GHC.Vacuum.Types
-- Copyright   : (c) Matt Morrow 2009, Austin Seipp 2011-2012
-- License     : LGPLv3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : non-portable (GHC only)
-- 
-- 
-- 
module GHC.Vacuum.Types (
   module GHC.Vacuum.Types
) where

import GHC.Vacuum.ClosureType
import GHC.Vacuum.Internal(HValue)

#if !(MIN_VERSION_base(4,8,0))
import Data.Word
import Data.Monoid(Monoid(..))
#endif
import Data.IntMap(IntMap)
import System.Mem.StableName

------------------------------------------------

type HNodeId = Int

data HNode = HNode
  {nodePtrs  :: [HNodeId]
  ,nodeLits  :: [Word]
  ,nodeInfo  :: InfoTab}
  deriving(Eq,Ord,Read,Show)

emptyHNode :: ClosureType -> HNode
emptyHNode ct = HNode
  {nodePtrs   = []
  ,nodeLits   = []
  ,nodeInfo   = if isCon ct
                  then ConInfo [] [] [] 0 0 ct 0 []
                  else OtherInfo 0 0 ct 0 []}

nodePkg   :: HNode -> String
nodeMod   :: HNode -> String
nodeName  :: HNode -> String
nodePkg   = fst3 . itabName . nodeInfo
nodeMod   = snd3 . itabName . nodeInfo
nodeName  = trd3 . itabName . nodeInfo

fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x
snd3 :: (a, b, c) -> b
snd3 (_,x,_) = x
trd3 :: (a, b, c) -> c
trd3 (_,_,x) = x

itabName :: InfoTab -> (String, String, String)
itabName i@(ConInfo{}) = (itabPkg i, itabMod i, itabCon i)
itabName  _            = ([], [], [])

summary :: HNode -> ([String],[HNodeId],[Word])
summary (HNode ps ls info) = case itabName info of
                              (a,b,c) -> ([a,b,c],ps,ls)

data InfoTab
  = ConInfo   {itabPkg    :: String
              ,itabMod    :: String
              ,itabCon    :: String
              ,itabPtrs   ::  Word
              ,itabLits   ::  Word
              ,itabType   ::  ClosureType
              ,itabSrtLen ::  Word
              ,itabCode   :: [Word]}
  | OtherInfo {itabPtrs   ::  Word
              ,itabLits   ::  Word
              ,itabType   ::  ClosureType
              ,itabSrtLen ::  Word
              ,itabCode   :: [Word]}
  deriving(Eq,Ord,Read,Show)

data Closure = Closure
  {closPtrs :: [HValue]
  ,closLits :: [Word]
  ,closITab :: InfoTab}

instance Show Closure where
    showsPrec d (Closure p l i) =
        showParen (d > 10) ("Closure"++)
                                . showsPrec d (map (const "(HValue)") p)
                                . showsPrec d l
                                . showsPrec d i

-- A box for safe deposit of HValues
data Box a = Box a

------------------------------------------------

data Env = Env
  {uniq  :: HNodeId
    -- the keys are hashes of StableNames
  ,seen  :: IntMap [(StableName HValue,HNodeId)]
  ,hvals :: IntMap (Box HValue)
  ,graph :: IntMap HNode}

emptyEnv :: Env
emptyEnv = Env
  {uniq = 0
  ,seen = mempty
  ,hvals = mempty
  ,graph = mempty}

------------------------------------------------
