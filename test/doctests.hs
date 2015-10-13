module Main where

import Test.DocTest

main :: IO ()
main = doctest [
  "src/Sec01.hs",
  "src/Sec22.hs"
  ]
