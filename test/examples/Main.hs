module Main
  ( main
  )
  where

-- doctest
import Test.DocTest (doctest)


-- |
--
--

main :: IO ()
main =
  doctest
    [ "-isrc"
    , "src/Money.hs"
    ]
