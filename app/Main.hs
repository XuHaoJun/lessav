module Main where

import Lib

main :: IO ()
main = do 
  -- putStrLn "Start scaning current directory ......"
  -- avs <- createAvTree
  -- _ <- dumpDb avs
  -- return ()
  _ <- createAvTree4
  return ()
