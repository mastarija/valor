module Main ( main ) where
--
import Test.DocTest ( doctest )
--

main :: IO ()
main = doctest
  [ "lib/Data/Valor.hs"
  , "int/Data/Valor/Internal.hs"
  ]
