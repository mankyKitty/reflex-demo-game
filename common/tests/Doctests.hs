module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/Types.hs"
  , "src/RayWut.hs"
  , "src/RayCaster.hs"
  ]
