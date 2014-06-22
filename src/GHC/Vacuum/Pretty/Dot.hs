{-# LANGUAGE OverloadedStrings #-}

{- |
> import GHC.Vacuum.Pretty.Dot
> import Data.GraphViz.Commands
> graphToDotPng :: FilePath -> [(String,[String])] -> IO FilePath
> graphToDotPng fpre g = addExtension (runGraphviz (graphToDot g)) Png fpre
-}
module GHC.Vacuum.Pretty.Dot (
   graphToDot
  ,graphToDotParams
  ,vacuumParams
) where

import Data.GraphViz hiding (graphToDot)
import Data.GraphViz.Attributes.Complete( Attribute(RankDir, Splines, FontName)
                                        , RankDir(FromLeft), EdgeType(SplineEdges))

import Control.Arrow(second)

------------------------------------------------

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
