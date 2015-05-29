module Main where

import CG.CG3 (convertInput, countIntersects)

main = do
  input <- getContents
  print $ countIntersects $ convertInput input
