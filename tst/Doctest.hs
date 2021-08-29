module Main ( main ) where
--
import Test.DocTest ( doctest )
--

main :: IO ()
main = doctest
  [ "lib/Data/Valor.hs"
  , "lib/Data/Valor/Tutorial.hs"
  , "int/Data/Valor/Internal.hs"
  ]
