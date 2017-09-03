module Optimize where

import Control.Monad
import Data.List

type Opt a = [a] -> Maybe [a]


untilFail :: Opt a -> [a] -> [a]
untilFail o p | Just p' <- o p = untilFail o p'
              | otherwise   = p

untilFail' :: Opt a -> [a] -> Maybe [a]
untilFail' o p | Just p' <- o p = Just $ untilFail o p'
               | otherwise = Nothing

atEvery :: Opt a -> [a] -> [a]
atEvery o p | Just (x:xs) <- o p = x : atEvery o xs
            | (x:xs)   <- p   = x : atEvery o xs
            | otherwise       = []

atEvery' :: Opt a -> [a] -> Maybe [a]
atEvery' _ [] = Nothing
atEvery' o p
  | Just (x:xs) <- o p
  = case atEvery' o xs of
      Nothing -> Just $ x:xs
      q -> (x:) <$> q
  | otherwise
  = (head p:) <$> atEvery' o (tail p)

tryAll :: Eq a => [Opt a] -> Opt a
tryAll os p = do
  Just x : _ <- pure . dropWhile (==Nothing) $ ($ p) <$> os
  return x

runOpt :: Opt a -> [a] -> [a]
runOpt o p | Just p' <- o p = p'
           | otherwise   = p


break' f p | (x, y) <- break f p
           , not $ null y = Just (x, y)
           | otherwise = Nothing

