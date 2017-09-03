import Fucktoid

data Compressed = Ptr Int
                | Add Int
                | FOp BF

instance Fucktoid Compressed where
  to (Ptr 0) = [NOP]
  to (Add 0) = [NOP]
  to (Ptr n) | n > 0 = replicate n Next
             | otherwise = replicate (abs n) Prev
  to (Add n) | n > 0 = replicate n Inc
             | otherwise = replicate (abs n) Dec

  from (x:xs) | isAdd x, (a,b) <- span isAdd (x:xs) = Add (sum $ map getVal a)
              | isPtr x, (a,b) <- span isPtr (x:xs) = Ptr (sum $ map getVal a)
              | otherwise = FOp $ x:xs


isAdd = flip elem [Inc,  Dec]
isPtr = flip elem [Next, Prev]
getVal x | Inc  <- x = 1
         | Next <- x = 1
         | Dec  <- x = -1
         | Prev <- x = -1
