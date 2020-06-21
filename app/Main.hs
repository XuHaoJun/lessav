module Main where

import Lib
import qualified Av as Av

main :: IO ()
main = do 
  maybe'avs <- getAvsFromDb
  let dbAvs = (case maybe'avs of
                Nothing -> []
                Just dbAvs ->  dbAvs)
  avs <- createAvTree' dbAvs
  dumpDb avs
  createCsv avs
  return ()
