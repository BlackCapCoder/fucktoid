module Main where

import BF
import BFComp2
import Optimize
import System.Process
import Control.Monad
import CompileOpt

main :: IO ()
main = do
  let program = "pi"
  str <- compile ({- reverse . dropWhile isPure . reverse . -} runOpt optimize').parse <$> readFile (program ++ ".bf")
  putStrLn $ (show . length $ lines str) ++ " lines"
  writeFile (program ++ ".c") str
  void $ readProcess "gcc" [program ++ ".c", "-O2", "-o", program] ""
  void $ readProcess "strip" [program] ""

tstPrg = "[->+++>--<<]"
  --"[+++++ ++++]>[----- ----]+++++ ++++<"


{-
factor - 23540256049: 104743 224743
  O2: 1.83s
  O0: 5.78s

  84242
-}
