{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module ReversibleBrainfuck (Op (..), ReversibleBrainfuck) where

import Language
import qualified Brainfuck as B

data Op = L | R | Inc | Dec | Read | Write | Loop [Op]
type ReversibleBrainfuck = [Op]

instance Lang ReversibleBrainfuck where

instance Trans B.Brainfuck ReversibleBrainfuck where
  trans = (=<<) $ \x -> case x of
    B.Inc      -> return Inc
    B.Dec      -> return Dec
    B.Read     -> return Read -- TODO: Current cell must be zero
    B.Write    -> return Write
    B.R        -> [R, R, Inc, R, R]
    B.L        -> [L, L, Dec, L, L]
    (B.Loop x) -> [ Loop [ R, R, r L, R, r R, L, L, Inc, R, R, r L, L, r R, L, L]
                  , R, R, r L, R, r R, L, L
                  , Loop $ [ R, R, Inc, R, R, R, R, r L, L, r R, L, L ]
                         ++ trans x
                         ++ [ L, L, r L, R, r R, L, L, Inc, R, R, r L, L, r R, L, L
                           , Loop [ R, R, r L, R, r R, L, L, Dec, R, R, r L, L, r R, L, L ]
                           , R, R, r L, R, r R, L, L ]
                  , R, R, Inc, R, R, R, R, r L, L, r R, L, L ]

    where r = Loop . replicate 4

