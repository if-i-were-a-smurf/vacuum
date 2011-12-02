{-# LANGUAGE OverloadedStrings #-}

module GHC.Vacuum.GraphViz
       ( -- * Simple API
         vacuumToPng
       , vacuumToSvg
         -- * Lower level API allowing more output control
       , graphToDotFile
       , graphToDot
         -- * GraphViz attributes
       , graphToDotParams
       , vacuumParams
       ) where
import System.FilePath

import Data.GraphViz hiding (graphToDot)
import Data.GraphViz.Attributes.Complete( Attribute(RankDir, Splines, FontName)
                                        , RankDir(FromLeft), EdgeType(SplineEdges))
import Data.GraphViz.Printing
import Control.Arrow(second)

import GHC.Vacuum

------------------------------------------------

vacuumToPng :: FilePath -> a -> IO FilePath
vacuumToPng fp a = graphToDotFile fp Png $ nameGraph (vacuum a)

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
