module Test.Gen.Wrong where

import Test.QuickCheck.Gen ( Gen , oneof )

import Data.Valor.Internal ( Wrong (..) )


genWrong :: Gen a -> Gen ( Wrong a )
genWrong gen = oneof
  [ Inert <$> gen
  , Wrong <$> gen
  ]
