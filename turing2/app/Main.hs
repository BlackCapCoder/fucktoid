module Main where

import Collatz
import Control.Monad

main = do
  forM_ ps $ \(xs,n) -> do
    putStrLn $ show n ++ ": " ++ unwords (map show xs)
