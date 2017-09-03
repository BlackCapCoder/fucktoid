{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module NanoFuck (Op (..), NanoFuck) where

import Language
import qualified ReversibleBitfuck as B


data Op = Flip | Loop [Op]
type NanoFuck = [Op]

instance Lang NanoFuck where

instance Trans B.ReversibleBitfuck NanoFuck where
  trans = (=<<) $ \x -> case x of
    B.Flip     -> [Flip, q]
    B.R        -> [Flip, q, Flip]
    B.L        -> [q]
    (B.Loop x) -> [Flip, q, Flip, Loop $ trans x]

   where q = Loop []

instance Trans NanoFuck B.ReversibleBitfuck where
  trans = (=<<) $ \x -> case x of
    Flip     -> [B.Flip, B.R]
    (Loop x) -> [B.L, B.Loop $ trans x]
