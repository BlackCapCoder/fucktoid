module Main where
import Lib

main :: IO ()
-- main = print $ stateCheck' [Next,Skip,Next,Skip,Prev,Skip,Next,Skip,Halt,Next]
-- main = print =<< stateCheck' . head <$> nonterms'
-- main = retry'
-- main = print =<< stateCheck' . (!!20) . reverse <$> nonterms
main = run
