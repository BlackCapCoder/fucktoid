-- Formally proven turing complete
module Brainfuck where

import Control.Monad.State
import Control.Monad.Loops
import Control.Lens
import Data.Word (Word8)
import Data.List

data Op      = Inc | Dec | Next | Prev | In | Out | Loop Program
type Program = [Op]


parse = fst . parse'
  where parse' []       = ([], [])
        parse' (']':xs) = ([], xs)
        parse' xs | Just (o, rst) <- parseOp xs = parse' rst & _1 %~ (o:)
                  | otherwise = parse' $ tail xs

        parseOp (x:xs)
          = case x of
              '+' -> Just (Inc , xs)
              '-' -> Just (Dec , xs)
              '>' -> Just (Next, xs)
              '<' -> Just (Prev, xs)
              ',' -> Just (In  , xs)
              '.' -> Just (Out , xs)
              '[' | (x,xs) <- parse' xs -> Just (Loop x, xs)
              _   -> Nothing

instance Show Op where
  show Inc      = "+"
  show Dec      = "-"
  show Next     = ">"
  show Prev     = "<"
  show In       = ","
  show Out      = "."
  show (Loop x) = '[' : pretty x ++ "]"

pretty = concatMap show

----

tape = Tape [] 0 $ repeat 0

data Tape a = Tape
  { _left    :: [a]
  , _current ::  a
  , _right   :: [a]
  }; makeLenses ''Tape

data Env = Env
  { _mem :: Tape Word8
  , _inp :: [Word8]
  , _out :: [Word8]
  }; makeLenses ''Env


interpret code input
  = view (_2.out) . runState (f code) $ Env tape input []
 where f = mapM_ $ \case
             Inc    -> mem.current += 1
             Dec    -> mem.current -= 1
             Prev   -> mem %= \(Tape (l:ls) c r) -> Tape ls l (c:r)
             Next   -> mem %= \(Tape l c (r:rs)) -> Tape (c:l) r rs
             Loop l -> whileM_ ((/=0) <$> use (mem.current)) $ f l
             Out    -> do
               x <- use $ mem.current
               out %= (++[x])
             In     -> do
               x <- use inp
               inp %= tail
               mem.current .= head x
