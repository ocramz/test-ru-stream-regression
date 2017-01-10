module Main where

import System.Environment

import Lib 


main :: IO ()
main = do
  -- argv@(n : _) <- getArgs
  -- let n' = case length argv of 0 -> topN
  --                              1 -> read n :: Int
  --                              _ -> topN
  processDataset topN -- n'
