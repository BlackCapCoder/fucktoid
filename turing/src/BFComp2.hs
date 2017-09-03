{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances
           , NamedFieldPuns, MultiWayIf
#-}
module BFComp2 where

import Language
import qualified Brainfuck as B
import qualified Data.Map as M


type Cell    = Int
type Program = [Op]

data Op
  = Inp Cell
  | Add Cell Int
  | Out Cell
  | Loop
    { balance :: Int
    , domain  :: (Int, Int)
    , isPure  :: Bool
    , logic   :: [Op] }
  | Copy
    { from     :: Cell
    , step     :: Int
    , subjects :: [Cell]
    }
  deriving (Eq, Show)

isOpPure x = case x of
  Inp _ -> False
  Out _ -> False
  Loop{isPure} -> isPure
  _     -> True

instance Lang Program where
instance Trans B.Brainfuck Program where
  trans bf | (_, _, _, _, ps) <- foldl parse (True, 0, 0, 0, []) bf
           = id
           . reverse . dropWhile isOpPure
           $ ps
    where parse (p, s, e, ptr, ps) x = case x of
            B.L     -> (p, min s (ptr-1), e, ptr-1, ps)
            B.R     -> (p, s, max e (ptr+1), ptr+1, ps)
            B.Inc    | (Add c v:b) <- ps, c == ptr -> (p, s, e, ptr, Add ptr (v+1) : b)
                     | otherwise                  -> (p, s, e, ptr, Add ptr (  1) : ps)
            B.Dec    | (Add c v:b) <- ps, c == ptr -> (p, s, e, ptr, Add ptr (v-1) : b)
                     | otherwise                  -> (p, s, e, ptr, Add ptr ( -1) : ps)
            B.Read  -> (False, s, e, ptr, Inp ptr :ps)
            B.Write -> (False, s, e, ptr, Out ptr :ps)
            B.Loop o | (Loop{}:_) <- ps -> (p, s, e, ptr, ps)
                     | (p', s', e', ptr', ps') <- foldl parse (True, ptr, ptr, ptr, []) o
                    -> if | p' && (ptr' - ptr) == 0
                          -> (p, min s s', max e e', ptr', parseCopy ptr (reverse ps') : ps )
                          | otherwise
                          -> (p && p', min s s', max e e', ptr'
                             , Loop { balance = ptr' - ptr
                                    , domain  = (s', e')
                                    , isPure  = p'
                                    , logic   = reverse ps'
                                    } : ps )

parseCopy = undefined
-- parseCopy p xs | m <- foldl f M.empty xs
--                , Just s <- M.lookup p m
--                = Copy p s . map fst . M.toList $ M.delete p m
--   where f m (Add c v) = M.insertWith (+) c v m
--         f m x = error $ show x


test2 = do
  bf <- B.parse <$> readFile "doublecpy.bf"
  let ops = trans bf :: Program
  return ops

