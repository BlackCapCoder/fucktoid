module MuFuck where

data MuOp   = L | R | Loop [MuOp]
type MuFuck = [MuOp]
