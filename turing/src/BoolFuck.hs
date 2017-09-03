{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module BoolFuck (Op (..), BoolFuck) where

import Language
import qualified MuFuck as M
import qualified Brainfuck as B
import qualified SmallFuck as S


data Op       = L | R | Flip | Loop [Op] | Read | Write
type BoolFuck = [Op]

instance Lang BoolFuck where

instance Trans M.MuFuck BoolFuck where
  trans = (=<<) $ \x -> case x of
            M.R         -> [R, Flip]
            M.L         -> [L]
            (M.Loop xs) -> [Loop $ trans xs]

instance Trans S.SmallFuck BoolFuck where
  trans = map $ \x -> case x of
            S.L -> L
            S.R -> R
            S.Flip -> Flip
            (S.Loop x) -> Loop $ trans x

instance Trans BoolFuck S.SmallFuck where
  trans = (=<<) $ \x -> case x of
            L -> return S.L
            R -> return S.R
            Flip -> return S.Flip
            (Loop x) -> return . S.Loop $ trans x
            _ -> error "Lossy transpilation: BoolFuck -> SmallFuck"

instance Trans B.Brainfuck BoolFuck where
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

