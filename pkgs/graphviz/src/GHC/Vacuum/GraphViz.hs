{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : GHC.Vacuum.GraphViz
-- Copyright   : (c) Austin Seipp 2012, Ivan Lazar Miljenovic 2011
-- License     : LGPLv3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : non-portable (GHC only)
-- 
-- This module exports a simple, high level interface for exporting
-- @vacuum@ graphs to GraphViz @dot@ output, and rendering them to
-- PNG/SVG files. It also contains various helpers if you wish to
-- customize the output yourself in some manner.
-- 
-- For this module to work, you will need to have graphviz installed,
-- and the 'dot' utility should be available somewhere in your
-- @$PATH@.
-- 
-- The simplest possible usage of this module is like so:
-- 
-- > vacuumToPng "list" [1..10]
-- 
-- This will output a \'list.png\' file, which contains a pretty graph
-- visualization of the expression @[1..10]@. You may alternatively
-- use 'vacuumToSvg' in the same manner, to export a graph to an SVG
-- file. This is more than sufficient for many use cases.
-- 
module GHC.Vacuum.GraphViz
       ( -- * Simple API
         vacuumToPng           -- :: FilePath -> a -> IO FilePath
       , vacuumToSvg           -- :: FilePath -> a -> IO FilePath
         
         -- * Lower level API allowing more output control
       , graphToDotFile
       , graphToDot
       , graphToDotParams
         
         -- * GraphViz attributes
       , vacuumParams
       ) where

import Data.GraphViz hiding (graphToDot)
import Data.GraphViz.Attributes.Complete( Attribute(RankDir, Splines, FontName)
                                        , RankDir(FromLeft), EdgeType(SplineEdges))
import Control.Arrow(second)

import GHC.Vacuum

------------------------------------------------

-- | @vacuumToPng \"foo\" e@ renders a graph representation of the
-- expression @e@ (which can be any expression what-so-ever) to
-- the file \"foo.png\" for later viewing.
vacuumToPng :: FilePath -> a -> IO FilePath
vacuumToPng fp a = graphToDotFile fp Png $ nameGraph (vacuum a)

-- | @vacuumToSvg \"foo\" e@ renders a graph representation of the
-- expression @e@ (which can be any expression what-so-ever) to
-- the file \"foo.svg\" for later viewing.
vacuumToSvg :: FilePath -> a -> IO FilePath
vacuumToSvg fp a = graphToDotFile fp Svg $ nameGraph (vacuum a)


------------------------------------------------

graphToDotFile :: (Ord a, PrintDot a) => FilePath -> GraphvizOutput -> [(a, [a])] -> IO FilePath
graphToDotFile fpre outTy g = Data.GraphViz.addExtension (runGraphviz (graphToDot g)) outTy fpre

graphToDot :: (Ord a) => [(a, [a])] -> DotGraph a
graphToDot = graphToDotParams vacuumParams

graphToDotParams :: (Ord a, Ord cl) => GraphvizParams a () () cl l -> [(a, [a])] -> DotGraph a
graphToDotParams params nes = graphElemsToDot params ns es
  where
    ns = map (second $ const ()) nes

    es = concatMap mkEs nes
    mkEs (f,ts) = map (\t -> (f,t,())) ts

------------------------------------------------

vacuumParams :: GraphvizParams a () () () ()
vacuumParams = defaultParams { globalAttributes = gStyle }

gStyle :: [GlobalAttributes]
gStyle = [ GraphAttrs [RankDir FromLeft, Splines SplineEdges, FontName "courier"]
         , NodeAttrs  [textLabel "\\N", shape PlainText, fontColor Blue]
         , EdgeAttrs  [color Black, style dotted]
         ]

------------------------------------------------
