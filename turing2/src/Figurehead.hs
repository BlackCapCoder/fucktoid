module Figurehead where

data Op = Push Int | Loop Int
        deriving Eq

data Lex = LPush Int | LLoop [Lex]
         deriving (Eq, Show)

parseOp :: String -> (Maybe Op, String)
parseOp (x:xs)
  | (a,b) <- break (/=x) xs
  , not $ null a
  = if | x == '|' -> (Just . Push $ length a + 1, b)
       | x == ' ' -> (Just . Loop $ length a + 1, b)
  | otherwise = parseOp xs
parseOp _ = (Nothing, [])

parse (parseOp -> (Just o, rst)) = o : parse rst
parse _ = []

lexF :: [Op] -> [Lex]
lexF = \case
  Push n:xs -> LPush n : lexF xs
  x:(break (==x) -> (a,b))
     -> LLoop (lexF a) : lexF (tail b)
  [] -> []

parse' = lexF . parse
tst    = parse' "|| || ||   |||   "


interpret m = \case
  LPush n:xs -> interpret (m++[n]) xs
  LLoop q:xs -> flip interpret xs $
    until (\x -> null x || head m /= head x)
          (flip interpret q . tail)
          $ tail m
  [] -> m

interpret' = interpret []
