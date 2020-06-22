module Main where

import Lib
import qualified Av as Av

import Numeric.Natural
import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))

data LessavOpts = LessavOpts
  { workerThreads      :: Natural
  , taskDelay          :: Int }

sample :: Parser LessavOpts
sample = LessavOpts 
      <$> option auto
          ( long "workerThreads"
         <> short 'j'
         <> showDefault
         <> value 2
         <> metavar "INT"
         <> help "num threads for worker process task." )
      <*> option auto
          ( long "pushTaskDelay"
         <> short 'd'
         <> showDefault
         <> value 500000
         <> metavar "INT"
         <> help "delay(milliseond) for push task to workers, no check previous pushed task status. one task has 2 http request and create shortcuts" )

main :: IO ()
main = do 
  opts <- execParser parserOpts
  maybe'avs <- getAvsFromDb
  let dbAvs = (case maybe'avs of
                Nothing -> []
                Just dbAvs ->  dbAvs)
  avs <- createAvTree' dbAvs (workerThreads opts) (taskDelay opts)
  dumpDb avs
  createCsv avs
  return ()
  where
    parserOpts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Manager ?v videos by shortcut tree"
     <> header "lessav - a simple ?v video manager based on shortcut tree." )