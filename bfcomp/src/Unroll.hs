module Unroll where

import Control.Monad
import Control.Monad.State hiding (put)
import Control.Monad.Cont
import Control.Lens
import Optimize
import qualified Data.Map.Strict as M
import Debug.Trace
import BF
import Data.Word (Word8)
import Data.Maybe (isNothing)

maxItr = 1*100000

data PState = PState
  { _mem :: !(M.Map Loc Word8)
  , _ptr :: !Loc
  , _itr :: !Int
  , _suc :: !Bool
  }; makeLenses ''PState

initial = PState M.empty 0 0 False


unroll' (x@(Out _):xs) = (x:) <$> unroll' xs
unroll' (x@(Put _):xs) = (x:) <$> unroll' xs
unroll' x = trace "unrolling.." $ flip evalState initial $ unroll x

unroll (x:xs) = do
  let isLoop = (\case (Loop _ _) -> True; _ -> False) x
  old <- get

  let f = if isLoop && not (isPure x)
             then const $ return Nothing
             else step

  runContT (f x) $ \case
    Just () -> do
      when isLoop $ suc .= True >> itr .= 0
      unroll xs
    Nothing -> do
      modify $ const old
      fmap (++x:xs) <$> unroll []

unroll [] = trace "done." $ do
    s <- use suc
    if s then do
      m <- use mem
      p <- use ptr
      return $ Just [Set m, Move p]
    else return Nothing


-----

success = return $ Just ()

step (Set a) = do
  p <- use ptr
  mem %= \m -> M.union (M.mapKeys (+p) a) m
  success

step (Add a) = do
  p <- use ptr
  mem %= \m -> M.unionWith (+) m $ M.mapKeys (+p) a
  success

-- mem[key] += mem[l] * val
step (Mult l a) = do
  p <- use ptr
  x <- (\case Just x -> x; Nothing -> 0) . M.lookup (p+l) <$> use mem
  step . Add $ M.map (*x) a

step (Move a) = ptr += a >> success

step (Loop l o) = callCC $ \k -> do
  p <- use ptr
  x <- M.lookup (p+l) <$> use mem
  when (x == Just 0 || isNothing x) . k $ Just ()
  i <- use itr
  when (i >= maxItr) $ k Nothing
  itr += 1
  y <- foldl (>>) (Just ()) <$> mapM step o
  when (isNothing y) $ k Nothing
  step $ Loop l o

step NOP = success
step x   = return Nothing
