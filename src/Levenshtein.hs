#!/usr/bin/env stack
-- stack script --resolver lts-16.0
{-# LANGUAGE BangPatterns #-}

module Levenshtein where

import Data.Array

-- from https://www.reddit.com/r/programming/comments/w4gs6/levenshtein_distance_in_haskell/c5a6jjz/

lev' :: (Eq a) => [a] -> [a] -> Int
lev' [] [] = 0
lev' [] ys = length ys
lev' xs [] = length xs
lev' (x:xs) (y:ys)
  | x == y    = lev xs ys
  | otherwise = 1 + minimum [lev' xs (y:ys),
                             lev' (x:xs) ys,
                             lev' xs ys]

lev :: (Eq a) => [a] -> [a] -> Int
lev xs ys = levMemo ! (n, m)
  where levMemo = array ((0,0),(n,m)) [((i,j),lev' i j) | i <- [0..n], j <- [0..m]]
        n = length xs
        m = length ys
        xa = listArray (1, n) xs
        ya = listArray (1, m) ys
        lev' 0 v = v
        lev' u 0 = u
        lev' u v
          | xa ! u == ya ! v = levMemo ! (u-1, v-1)
          | otherwise        = 1 + minimum [levMemo ! (u, v-1),
                                            levMemo ! (u-1, v),
                                            levMemo ! (u-1, v-1)] 

levp :: (Eq a) => [a] -> [a] -> Float
levp xs ys = 
  1 - (editDistance / maxLen)
  where
    maxLen :: Float
    maxLen = fromIntegral (max (length xs) (length ys))
    editDistance :: Float
    editDistance= fromIntegral $ lev xs ys