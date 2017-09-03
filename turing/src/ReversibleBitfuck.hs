{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module ReversibleBitfuck (Op (..), ReversibleBitfuck) where

import Language
import qualified ReversibleBrainfuck as B

data Op = L | R | Flip | Read | Write | Loop [Op]
type ReversibleBitfuck = [Op]

instance Lang ReversibleBitfuck where

instance Trans B.ReversibleBrainfuck ReversibleBitfuck where
  trans = (=<<) $ \x -> case x of
            B.L -> replicate 9 L
            B.R -> replicate 9 R
            B.Read  -> (concat $ replicate 8 [R, Read]) ++ replicate 8 L
            B.Write -> (concat $ replicate 8 [R, Write]) ++ replicate 8 L
            B.Inc -> [R, Loop [R], Flip, L, Loop [Flip, L]] ++ replicate 9 R ++ Loop [Flip] : replicate 9 L
            B.Dec -> replicate 9 R ++ Flip : replicate 9 L ++ Flip : Loop [R, Flip] : L : Loop [L] : replicate 9 R ++ Loop [Flip] : replicate 9 L
            B.Loop x -> replicate 9 R ++ Flip : replicate 9 L ++ Flip : Loop [R, Flip] : L : Loop [L] : replicate 9 R
                     ++ [ Loop $ Flip : replicate 9 L ++ Loop [R] : Flip : L : Loop [Flip, L] : trans x
                       ++ replicate 9 R ++ Flip : replicate 9 L ++ Flip : Loop [R, Flip] : L : Loop [L] : replicate 9 R
                       , L, Loop [Flip, L]]

{-
-- Boolean logic
false  = []
true   = [L, Flip, R]                                -- <+>
left   = [Loop true]                                 -- (<+>)
right  = [R, Loop [L, L, Flip, R, R], L]             -- >(<<+>>)<
nleft  = [Flip, left, Flip]                          -- +(<+>)+
nright = [R, Flip, Loop [L, L, Flip, R, R], Flip, L] -- >+(<<+>>)+<
and    = [Loop right]                                -- (>(<<+>>)<)
nand   = [true, and]                                 -- <+>(>(<<+>>)<)
nor    = [Flip, R, Flip, L, and, Flip, R, Flip, L]   -- +>+<(>(<<+>>)<)+>+<
or     = [true, nor]                                 -- <+>+>+<(>(<<+>>)<)+>+<
implL  = [R, Flip, L, and, R, Flip, L]               -- >+<(>(<<+>>)<)>+<
implR  = [Flip, and, Flip]                           -- +(>(<<+>>)<)+
eq     = [and, nor]                                  -- (>(<<+>>)<)+>+<(>(<<+>>)<)+>+<
xor    = [implR, implL]                              -- +(>(<<+>>)<)+>+<(>(<<+>>)<)>+<
nimplL = [true, implL]                               -- <+>>+<(>(<<+>>)<)>+<
nimplR = [true, implR]                               -- <+>+(>(<<+>>)<)+


-- Mics
swap = [Loop [R, Flip, L], R, left, L, Loop [R, Flip, L]] -- (>+<)>(<+>)<(>+<)
-}
