-- |
-- Module      : GHC.Vacuum.Pretty
-- Copyright   : (c) Matt Morrow 2009, Austin Seipp 2011-2012
-- License     : LGPLv3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : non-portable (GHC only)
-- 
-- 
-- 
{-# LANGUAGE RecordWildCards, CPP #-}
module GHC.Vacuum.Pretty (
   module GHC.Vacuum.Pretty
) where
import Control.Arrow (second)

import Data.List
import Data.IntMap (IntMap)
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif
import qualified Data.IntMap as IM

import Control.Monad
import GHC.Vacuum.Types

-----------------------------------------------------------------------------

toAdjPair :: (HNodeId, HNode) -> (Int, [Int])
toAdjPair = second nodePtrs

toAdjList :: IntMap HNode -> [(Int, [Int])]
toAdjList = fmap toAdjPair . IM.toList

nameGraph :: IntMap HNode -> [(String, [String])]
nameGraph m = let g = toAdjList m
                  pp i = maybe "..."
                          (\n -> nodeName n ++ "|" ++ show i)
                          (IM.lookup i m)
              in fmap (\(x,xs) -> (pp x, fmap pp xs)) g

data ShowHNode = ShowHNode
  {showHNode   :: Int -> HNode -> String
  ,externHNode :: Int -> String}

showHNodes :: ShowHNode -> IntMap HNode -> [(String, [String])]
showHNodes (ShowHNode showN externN) m
  = let g = toAdjList m
        pp i = maybe (externN i) (showN i) (IM.lookup i m)
    in fmap (\(x,xs) -> (pp x, fmap pp xs)) g

-----------------------------------------------------------------------------

-- | To assist in \"rendering\"
--  the graph to some source.
data Draw e v m a = Draw
  {mkV   :: Int -> a -> m v
  ,mkE   :: v -> v -> m e
  ,succs :: a -> [Int]}

newtype G e v = G {unG :: IntMap (v, IntMap e)}
  deriving(Eq,Ord,Read,Show)

draw :: (Monad m) => Draw e v m a -> IntMap a -> m (G e v)
draw Draw{..} g = do
  vs <- IM.fromList `liftM` forM (IM.toList g)
          (\(i,a) -> do v <- mkV i a
                        return (i,(v,succs a)))
  (G . IM.fromList) `liftM` forM (IM.toList vs)
    (\(i,(v,ps)) -> do --let us = fmap (vs IM.!) ps
                       es <- IM.fromList `liftM` forM ps
                               (\p -> do e <- mkE v (fst (vs IM.! p))
                                         return (p,e))
                       return (i,(v,es)))

-- | An example @Draw@
printDraw :: Draw (Int,Int) Int IO HNode
printDraw = Draw
  {mkV   = \i _ -> print i >> return i
  ,mkE   = \u v -> print (u,v) >> return (u,v)
  ,succs = nodePtrs}

-- | Build a map to @(preds,succs)@
split :: (a -> [Int]) -> IntMap a -> IntMap ([Int],[Int])
split f = flip IM.foldWithKey mempty (\i a m ->
            let ps = f a
            in foldl' (\m' p -> IM.insertWith mappend p ([i],[]) m')
                      (IM.insertWith mappend i ([],ps) m)
                      ps)

-----------------------------------------------------------------------------
