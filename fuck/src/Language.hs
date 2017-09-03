{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Language where

class Lang a where
class (Lang a, Lang b) => Trans a b where
  trans :: a -> b

