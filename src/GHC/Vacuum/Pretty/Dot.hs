
module GHC.Vacuum.Pretty.Dot (
   graphToDot
  ,ppGraph,ppEdge,gStyle
--   ,Doc,text,render
) where

import Text.PrettyPrint

------------------------------------------------

-- | .
graphToDot :: (a -> String) -> [(a, [a])] -> Doc
graphToDot f = ppGraph . fmap (f *** fmap f)
  where f *** g = \(a, b)->(f a, g b)

------------------------------------------------

gStyle :: String
gStyle = unlines
  ["  graph [rankdir=LR, splines=true];"
  ,"  node [label=\"\\N\", shape=none, fontcolor=blue, fontname=courier];"
  ,"  edge [color=black, style=dotted, fontname=courier, arrowname=onormal];"]

ppGraph :: [(String, [String])] -> Doc
ppGraph xs = (text "digraph g" <+> text "{")
              $+$ text gStyle
                $+$ nest indent (vcat . fmap ppEdge $ xs)
                    $+$ text "}"
                        where indent = 2

ppEdge :: (String, [String]) -> Doc
ppEdge (x,xs) = (dQText x) <+> (text "->")
                    <+> (braces . hcat . punctuate semi
                        . fmap dQText $ xs)

dQText :: String -> Doc
dQText = doubleQuotes . text

{-
import System.Cmd
import System.Exit
graphToDotPng :: FilePath -> [(String,[String])] -> IO Bool
graphToDotPng fpre g = do
  let [dot,png] = fmap (fpre++) [".dot",".png"]
  writeFile dot
    . render . ppGraph
      -- . fmap (show***fmap show)
        $ g
  ((==ExitSuccess) `fmap`) .system . intercalate " " $
    -- ["cat",dot,"|","dot -Tpng",">",png,"2>/dev/null;","gliv",png,"&"]
    ["cat",dot,"|","dot -Tpng",">",png,"2>/dev/null;","display",png,"&"]

graphToDotPdf :: FilePath -> [(String,[String])] -> IO Bool
graphToDotPdf fpre g = do
  let [dot,png] = fmap (fpre++) [".dot",".pdf"]
  writeFile dot
    . render . ppGraph
      -- . fmap (show***fmap show)
        $ g
  ((==ExitSuccess) `fmap`) .system . intercalate " " $
    -- ["cat",dot,"|","dot -Tpng",">",png,"2>/dev/null;","gliv",png,"&"]
    ["cat",dot,"tred","|","dot -Tpdf",">",png,"2>/dev/null;","evince",png,"&"]
-}

------------------------------------------------
